package com.tesobe.obp.billing.utils

import java.io.{BufferedReader, InputStream, InputStreamReader}

object Props {
  private val props: Map[String, String] = {
    // read application.properties key value, and parse EL express: ${obp.some.xxx}
    val propertiesFileInput: InputStream = this.getClass.getResourceAsStream("/application.properties")

    var rawProps: Map[String, String] = try {
      val reader = new BufferedReader(new InputStreamReader(propertiesFileInput, "utf-8"))
      val lines: Array[String] = reader.lines()
        .filter(!_.startsWith("#"))
        .filter(!_.trim.isEmpty)
        .toArray(size => new Array[String](size))

        reader.close()

        lines.map(line => {
          val Array(key, value) = line.split("""\s*=\s*""", 2)
          (key, value)
        }).toMap
    } finally {
      propertiesFileInput.close()
    }

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
