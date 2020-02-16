package com.tesobe.obp.billing.utils

import java.net.URL

import scala.io.{BufferedSource, Source}

object Props {
  private val props: Map[String, String] = {
    // read application.properties key value, and parse EL express: ${obp.some.xxx}
    val propertiesFile: URL = this.getClass.getResource("/application.properties")
    val source: BufferedSource = Source.fromFile(propertiesFile.toURI, "utf-8")

    var rawProps: Map[String, String] = source.getLines()
      .filterNot(_.startsWith("#"))
      .filterNot(_.trim.isEmpty)
      .map(line => {
        val Array(key, value) = line.split("""\s*=\s*""", 2)
        (key, value)
      }).toMap

    val EL = """(?m)(.*?)\$\{(.*?)\}(.*)""".r

    def hasEl(str: String) = str match {
      case EL(_, _, _) => true
      case _ => false
    }

    while (rawProps.values.exists(hasEl)) {
      rawProps = rawProps.map(pair => pair._2 match {
        case EL(pre, key, suf) => (pair._1, s"$pre${rawProps(key.trim)}$suf")
        case _ => pair
      })
    }

    rawProps
  }

  def getProperty(key: String) = props(key)
}
