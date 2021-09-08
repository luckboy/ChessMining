/*
 * Chess Mining - Library to data mining for chess games.
 * Copyright (C) 2021 Łukasz Szpakowski
 *
 * This library is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as
 * published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received copies of the GNU Lesser General Public
 * License and the GNU General Public License along with this library.
 * If not, see <http://www.gnu.org/licenses/>.
 */
package pl.luckboy.chessmining.value
import java.io._

/** A number reader.
  *
  * @constructor Creates a new number reader.
  *
  * @param r the reader.
  */
class NumberReader(r: Reader) extends Closeable
{
  private val reader = r
  private var pushedChar = -1

  private def readChar() =
    if(pushedChar != -1) {
      val i = pushedChar
      pushedChar = -1
      i
    } else
      reader.read()
    
  private def unreadChar(c: Int)
  {
    if(c != -1) pushedChar = c
  }
  
  /** Reads a string.
    *
    * @return a string.
    */
  def readString() = {
    var isStop = false
    while(!isStop) {
      val i = readChar()
      if(i == -1) {
        isStop = true
      } else {
        val c = i.toChar
        if(c != ' ' && c != '\t' && c != '\u000B' && c != '\n' && c != '\r') {
          unreadChar(i)
          isStop = true
        }
      }
    }
    isStop = false
    val sb = new StringBuilder()
    while(!isStop) {
      val i = readChar()
      if(i == -1) {
        isStop = true
      } else {
        val c = i.toChar
        if(c != ' ' && c != '\t' && c != '\u000B' && c != '\n' && c != '\r') {
          sb += c
        } else {
          unreadChar(i)
          isStop = true
        } 
      }
    }
    sb.toString()
  }
  
  /** Reads an `Int`.
    *
    * @return an optional `Int`.
    */
  def readInt() = {
    try {
      Some(Integer.parseInt(readString()))
    } catch {
      case e: NumberFormatException => None
    }
  }

  /** Reads a `Long`.
    *
    * @return an optional `Long`.
    */
  def readLong() = {
    try {
      Some(java.lang.Long.parseLong(readString()))
    } catch {
      case e: NumberFormatException => None
    }
  }
  
  override def close()
  {
    reader.close()
  }
}
