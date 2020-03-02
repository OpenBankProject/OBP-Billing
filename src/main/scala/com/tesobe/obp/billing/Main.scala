package com.tesobe.obp.billing

import java.util.Date
import java.util.concurrent.atomic.AtomicLong

import com.tesobe.obp.billing.auth._
import com.tesobe.obp.billing.utils.DateUtils._
import com.tesobe.obp.billing.utils.HttpUtils.{buildNinjaRequest, collectNinjaJson, countNinjaElements, findOneNinjaJson, getObpJson, product_key, reduceLeftNinjaJson}
import com.tesobe.obp.billing.utils.StringUtils.{decorateJsonValue, isNotEmptyStr}
import net.liftweb.json
import net.liftweb.json.JInt

object Main {
  implicit val formats = net.liftweb.json.DefaultFormats

  def getNewerInvoice(v1: Invoice, v2: Invoice): Invoice = {
    val first = v1.custom_text_value1
    val second = v2.custom_text_value1
    val isFirstIsTime = isTimeStr(first)
    val isSecondIsTime = isTimeStr(second)
    if (isFirstIsTime && isSecondIsTime) {
      List(v1, v2).maxBy(_.custom_text_value1)
    } else if (isFirstIsTime) {
      v1
    } else {
      v2
    }
  }


  def buildInvoiceItems(dateTimeRange: Seq[Date], consumerId: String, ninjaProduct: NinjaProduct): List[InvoiceItem] = {
    def getMetric(from: Date, to: Date): AggregateMetric = {
      val fromDate = toTimeStr(from)
      val toDate = toTimeStr(to)
      println(s"Fetching metrics: consumer_id=$consumerId&from_date=$fromDate&to_date=$toDate")
      val metrics = getObpJson[List[AggregateMetric]]("management/aggregate-metrics", "",
        List(
          ("consumer_id", consumerId),
          ("from_date", fromDate),
          ("to_date", toDate)
        )
      )
      println(s"Fetched metrics: $metrics")
      metrics.head
    }
    println(s"Fetching range day of metrics......")
    val totalMetric = getMetric(dateTimeRange.head, dateTimeRange.last)
    if (totalMetric.count == 0) {
      Nil
    } else {
      val days: List[(Date, Date)] = dateTimeRange.zip(dateTimeRange.tail).toList

      for {
        (from, to) <- days
        metric = getMetric(from, to)
        if metric.count > 0
        itemCost = BigDecimal(ninjaProduct.cost) * BigDecimal(metric.count)
        invoiceItem = InvoiceItem(product_key, toDateStr(from), itemCost.toDouble, metric.count)
      } yield {
        Thread.sleep(100) // if request obp-api too fast, will block until request timeout
        invoiceItem
      }
    }
  }


  def main(args: Array[String]): Unit = {
    println("Fetching all obp consumers.")
    val consumers = getObpJson[List[Consumer]]("management/consumers", "consumers")
        .filter(it => isNotEmptyStr(it.consumer_id))
    println(s"Got all obp consumers, count: ${consumers.size}")

    println("Fetching all exists not deleted Ninja Clients.")
    val allConsumerId = consumers.map(_.consumer_id).toSet
    // exists client corresponding consumerId, consumerId -> client_id
    val exitsClients = collectNinjaJson[Client, (String, Int), ClientsResponse]("clients") {
      case client if !client.is_deleted && allConsumerId.contains(client.custom_value1) => client.custom_value1 -> client.id
    }.toMap

    println(s"Got all exists not deleted Ninja Clients, count: ${exitsClients.size}")

    def isConsumerNoClient(consumerId: String): Boolean = !exitsClients.contains(consumerId)

    // do create client
    val noClientConsumers = consumers.filter(it => isConsumerNoClient(it.consumer_id))

    println(s"Not created Ninja Client corresponding OBP Consumers: ${noClientConsumers.mkString("\n")}")

    val newClients = noClientConsumers
      .map(consumer => {
        val appName = Option(consumer.app_name).filter(isNotEmptyStr).getOrElse(s"Empty name created by ${consumer.created_by_user.username}")
        val jsonForPost =
          s"""
             |{
             |"name": "${decorateJsonValue(appName)}",
             |"public_notes": "${decorateJsonValue(consumer.description)}",
             |"custom_value1": "${consumer.consumer_id}",
             |"custom_value2": "${decorateJsonValue(consumer.description)}",
             |"website": "${decorateJsonValue(consumer.redirect_url)}",
             |"contact":{
             |      "email": "${decorateJsonValue(consumer.created_by_user.email)}"
             |   }
             |}
             |""".stripMargin

        println(s"create client, current consumerId: ${consumer.consumer_id}")

        val response = buildNinjaRequest("clients").method("POST").postData(jsonForPost).asString
        val code = response.code
        assume(code == 200, s"create client fail, ${response.body}")
        val clientId = (json.parse(response.body) \ "data" \ "id").asInstanceOf[JInt].values.intValue()

        println(s"create client success, current consumerId: ${consumer.consumer_id}, client_id: $clientId")

        consumer.consumer_id -> clientId
      }).toMap

    if (noClientConsumers.nonEmpty) {
      println(s"Created new Cilents count: ${noClientConsumers.size}")
    }

    //consumerId -> client_id
    val consumerIdToClientId: Map[String, Int] = exitsClients ++ newClients
    // consumerId -> created , So we can get any metric most early date_c by consumerId
    val consumerIdToCreated = consumers.map(it => it.consumer_id -> it.created).toMap

    val ninjaProduct: NinjaProduct = findOneNinjaJson[NinjaProduct, NinjaProductsResponse]("products")(_.product_key == product_key)
      .getOrElse(throw new IllegalStateException(s"ninja should have a produce with product_key 'API-CALLS-PLAN-A', But not found that product."))

    println(s"Got the Product '$product_key': $ninjaProduct")


    {
      println(s"start process of create invoice.")
      val invoiceNumberPrefix = "OBP-"
      val totalInvoices: Int = countNinjaElements[Invoice, InvoicesResponse]("invoices", _.invoice_number.startsWith(invoiceNumberPrefix))
      val invoiceNumberBuilder = new AtomicLong(totalInvoices)
      for ((consumerId, clientId)<- consumerIdToClientId) {
        val newestInvoice = reduceLeftNinjaJson[Invoice, InvoicesResponse]("invoices", List("client_id" -> clientId))(getNewerInvoice)
        val fromDate: Date = newestInvoice match {
          case Some(invoice) if isTimeStr(invoice.custom_text_value1) => parseTime(invoice.custom_text_value1)
          case _ => consumerIdToCreated(consumerId) // the consumer created time as start time of invoice
        }

        val dateTimeRange: Seq[Date] = buildRangeTime(fromDate)

        val invoiceItems: List[InvoiceItem] = buildInvoiceItems(dateTimeRange, consumerId, ninjaProduct)
        if (invoiceItems.nonEmpty) {
          val invoiceNumber = s"$invoiceNumberPrefix${invoiceNumberBuilder.incrementAndGet()}"
          val invoiceDate = toDateStr(dateTimeRange.last)
          // store in field custom_text_value1, to record the created time of invoice
          val invoiceTime = toTimeStr(dateTimeRange.last)
          val duDate = toDateStr(addDate(dateTimeRange.last, 30)) // plus one month
          val note = s"${toDateTime(dateTimeRange.head)} TO ${toDateTime(dateTimeRange.last)}"
          val invoice = SimpleInvoice(clientId, invoiceNumber, invoiceDate, duDate, note, invoiceTime, invoiceItems)

          val zson = json.compactRender(json.Extraction.decompose(invoice))
          println(s"create invoice: $zson")
          val response = buildNinjaRequest("invoices").method("POST").postData(zson).asString
          val code = response.code
          if (code != 200) println(s"create invoice fail, $zson, ${response.body}")
          println(s"create invoice success, invoice_number: $invoiceNumber")
        }
      }

    }

    println("all invoice created successfully.")
  }

}
