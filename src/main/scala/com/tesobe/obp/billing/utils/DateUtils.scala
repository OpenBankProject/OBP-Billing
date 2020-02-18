package com.tesobe.obp.billing.utils

import java.text.{ParseException, SimpleDateFormat}
import java.util.Date

object DateUtils {
  private val oneDayMs: Long = 24 * 60 * 60 * 1000
  private val dateWithMsFormat = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSS'Z'")
  private val dateTimeFormat = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss")
  private val dateFormat = new SimpleDateFormat("yyyy-MM-dd")

  /**
   * add some days to create new Date, and the result HH:mm:ss 00:00:00
   * @param date
   * @return
   */
  def addDate(date: Date, addDays: Int): Date = {
    val addMs = oneDayMs * addDays
    val newDate = new Date(date.getTime + addMs)
    toDate(newDate)
  }

  /**
   * create new date the time is 00:00:00 accroding given date.
   * @param date
   * @return
   */
  def toDate(date: Date) = dateFormat.parse(dateFormat.format(date))

  def buildRangeTime(fromDate: Date, toDate: Date = new Date()): Seq[Date] = {
    val secondDate: Date = addDate(fromDate, 1) // next day begin time: 00:00:00
    val range = secondDate.getTime to (toDate.getTime, oneDayMs)
    fromDate +: range.map(new Date(_)) :+ toDate
  }


  def isTimeStr(str: String): Boolean =
    try {
      dateWithMsFormat.parse(str)
      true
    } catch {
      case _: ParseException => false
    }

  def parseTime(str: String): Date = dateWithMsFormat.parse(str)
  def toTimeStr(date: Date): String = dateWithMsFormat.format(date)
  def parseDate(str: String): Date = dateFormat.parse(str)
  def toDateStr(date: Date): String = dateFormat.format(date)
  def toDateTime(date: Date): String = dateTimeFormat.format(date)
}
