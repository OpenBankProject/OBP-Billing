package com.tesobe.obp.billing.utils

import com.tesobe.obp.billing.auth.PagingResponse
import com.tesobe.obp.billing.utils.Props.getProperty
import net.liftweb.json
import net.liftweb.json.DefaultFormats
import scalaj.http._

import scala.collection.GenTraversable

object HttpUtils {
  implicit val formats: DefaultFormats.type = net.liftweb.json.DefaultFormats

  private val token = {
    println("Fetching obp token.")

    val username = getProperty("obp.username")
    val password = getProperty("obp.password")
    val consumerKey = getProperty("obp.consumerKey")
    val directloginUrl = getProperty("obp.api.directloginUrl")
    val authorizationHeader = s"""DirectLogin username="$username",password="$password",consumer_key="$consumerKey""""
    val body = Http(directloginUrl)
      .header("Content-Type", "application/json")
      .header("Authorization", authorizationHeader).postData("")
      .timeout(connTimeoutMs = 10000, readTimeoutMs = 10000)
      .asString.body
    val obpToken = json.parse(body).extract[Map[String, String]]
      .getOrElse("token", throw new IllegalArgumentException(s"login fail, please check login parameters.Authorization: $authorizationHeader"))

    println(s"Got obp token: $obpToken")

    obpToken
  }

  val product_key: String = getProperty("ninja.api.invoice.item.product_key")

  def buildObpRequest(relativeUrl: String): HttpRequest = {
    Http(getProperty("obp.api.versionedUrl") + "/" + relativeUrl)
      .timeout(connTimeoutMs = 30000, readTimeoutMs = 60000)
      .header("Content-Type", "application/json")
      .header("Authorization", s"""DirectLogin token="$token"""")
  }

  def buildNinjaRequest(relativeUrl: String): HttpRequest = {
    val ninjaToken = getProperty("ninja.api.token")
    Http(getProperty("ninja.api.rootUrl") + "/" + relativeUrl)
      .timeout(connTimeoutMs = 30000, readTimeoutMs = 60000)
      .header("content-type", "application/json")
      .header("X-Ninja-Token", ninjaToken)
  }


  private def requestJson[T: Manifest](urlBuilder: String => HttpRequest)(relativeUrl: String, dataPath: String = "")(fun: HttpRequest => HttpRequest) = {
    val response = fun(urlBuilder(relativeUrl)).asString.throwError
    val jValue = json.parse(response.body)
    val zson = dataPath.split('.') match {
      case Array("") => jValue
      case arr => (jValue /: arr) ((j, path) => j \ path)
    }
    zson.extract[T]
  }

  def requestNinjaJson[T: Manifest](relativeUrl: String, dataPath: String = "")(fun: HttpRequest => HttpRequest): T =
    requestJson[T](buildNinjaRequest)(relativeUrl, dataPath)(fun)

  def getNinjaJson[T <: PagingResponse[_] : Manifest](relativeUrl: String, params: Seq[(String, Any)] = Seq.empty): T =
    requestNinjaJson[T](relativeUrl, "")(_.method("GET").params(params.map(it => it._1 -> it._2.toString)))

  def getAllNinjaJson[D, T <: PagingResponse[D] : Manifest](relativeUrl: String, params: Seq[(String, Any)] = Seq.empty, pageSize: Int = 20): List[D] = {
    val pagedParam = params :+ ("per_page" -> pageSize)
    val firstPage: T = getNinjaJson[T](relativeUrl, pagedParam)
    val firstData = firstPage.data
    foldLeftFromSecond(firstPage.pageRange(pageSize), firstData) { (data, page) =>
      val nextPage = "page" -> page
      val nextData: List[D] = getNinjaJson[T](relativeUrl, pagedParam :+ nextPage).data

      data ::: nextData
    }
  }

  def collectNinjaJson[D, E, T <: PagingResponse[D] : Manifest](relativeUrl: String,
                                                                params: Seq[(String, Any)] = Seq.empty,
                                                                pageSize: Int = 20)(pf: PartialFunction[D, E]): List[E] = {
    val pagedParam = params :+ ("per_page" -> pageSize)
    val firstPage: T = getNinjaJson[T](relativeUrl, pagedParam)
    val firstData = firstPage.data.collect(pf)
    foldLeftFromSecond(firstPage.pageRange(pageSize), firstData) { (data, page) =>
      val nextPage = "page" -> page
      val nextData: List[E] = getNinjaJson[T](relativeUrl, pagedParam :+ nextPage).data.collect(pf)

      data ::: nextData
    }
  }

  def findOneNinjaJson[D <: {def is_deleted: Boolean}, T <: PagingResponse[D] : Manifest](relativeUrl: String,
                                                                                          params: Seq[(String, Any)] = Seq.empty,
                                                                                          pageSize: Int = 20)
                                                                                         (predicate: D => Boolean): Option[D] = {
    val pagedParam = params :+ ("per_page" -> pageSize)
    val firstPage: T = getNinjaJson[T](relativeUrl, pagedParam)
    val isMatched: D => Boolean = it => !it.is_deleted && predicate(it)
    val firstMatch: Option[D] = firstPage.data.find(isMatched)
    foldLeftFromSecond(firstPage.pageRange(pageSize), firstMatch) { (data, page) =>
      data match {
        case Some(_) => data
        case _ =>
          val nextPage = "page" -> page
          val nextData: List[D] = getNinjaJson[T](relativeUrl, pagedParam :+ nextPage).data

          nextData.find(isMatched)
      }
    }
  }

  def reduceLeftNinjaJson[D <: {def is_deleted: Boolean}, T <: PagingResponse[D] : Manifest](relativeUrl: String,
                                                                                             params: Seq[(String, Any)] = Seq.empty,
                                                                                             pageSize: Int = 20)
                                                                                            (op: (D, D) => D): Option[D] = {
    val pagedParam = params :+ ("per_page" -> pageSize)
    val firstPage: T = getNinjaJson[T](relativeUrl, pagedParam)

    if (firstPage.data.isEmpty) {
      None
    } else {
      val firstPageData = firstPage.data.filterNot(_.is_deleted)
      val first: D = if (firstPageData.isEmpty) {
        null.asInstanceOf[D]
      } else {
        firstPageData.reduceLeft(op)
      }
      val result = firstPage.pageRange(pageSize).tail
        .foldLeft(first) { (data, page) => {
          val nextPage = "page" -> page
          val nextData: List[D] = getNinjaJson[T](relativeUrl, pagedParam :+ nextPage).data.filterNot(_.is_deleted)
          nextData.foldLeft(data)((pre, cur) => if (pre == null) cur else op(pre, cur))
        }
        }

      Option(result)
    }
  }

  def foldLeftNinjaElements[D, T <: PagingResponse[D] : Manifest](relativeUrl: String,
                                                                  predicate: D => Boolean = (_: D) => true,
                                                                  params: Seq[(String, Any)] = Seq.empty,
                                                                  pageSize: Int = 20
                                                                 )(f: (D, D) => D): Option[D] = {
    val pagedParam = params :+ ("per_page" -> pageSize)
    val firstPage: T = getNinjaJson[T](relativeUrl, pagedParam)

    if (firstPage.data.isEmpty) {
      None
    } else {
      def foldFunction(pre: Option[D], cur: D): Option[D] = pre match {
        case None => Option(cur)
        case Some(v) => Option(f(v, cur))
      }

      def initValue: Option[D] = None

      val firstPageFoundValue = firstPage.data.filter(predicate).foldLeft(initValue)(foldFunction)

      foldLeftFromSecond(firstPage.pageRange(pageSize), firstPageFoundValue) { (preValue, page) => {
          val nextPage = "page" -> page
          getNinjaJson[T](relativeUrl, pagedParam :+ nextPage).data.filter(predicate).foldLeft(preValue)(foldFunction)
        }
      }
    }
  }

  def requestObpJson[T: Manifest](relativeUrl: String, dataPath: String = "")(fun: HttpRequest => HttpRequest): T =
    requestJson[T](buildObpRequest)(relativeUrl, dataPath)(fun)

  def getObpJson[T: Manifest](relativeUrl: String, datePath: String, params: Seq[(String, String)] = Seq.empty): T =
    requestObpJson[T](relativeUrl, datePath)(_.method("GET").params(params))


  private def foldLeftFromSecond[A, B](coll: GenTraversable[A], z: B)(op: (B, A) => B): B = {
    if(coll == null || coll.isEmpty) {
      z
    } else {
      coll.tail.foldLeft(z)(op)
    }
  }
}
