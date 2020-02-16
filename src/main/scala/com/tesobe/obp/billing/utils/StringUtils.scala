package com.tesobe.obp.billing.utils

object StringUtils {

  def isEmptyStr(str: String): Boolean = str == null || str.trim.isEmpty

  def isNotEmptyStr(str: String) = !isEmptyStr(str)

  def decorateJsonValue(jsonValue: String) = jsonValue.trim.replaceAll("\\r|\\n", "\\\\n")
}
