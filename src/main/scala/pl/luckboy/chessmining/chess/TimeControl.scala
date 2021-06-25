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
package pl.luckboy.chessmining.chess

/** Represents a kind of time control. */
sealed abstract class TimeControl
{
  override def toString() =
    this match {
      case UnknownTimeControl                            => "?"
      case ClassicalTimeControl(moveCount, seconds)      => moveCount + "/" + seconds
      case SuddenDeathTimeControl(seconds)               => seconds.toString()
      case IncrementalTimeControl(seconds, extraSeconds) => seconds + "+" + extraSeconds
      case HourglassTimeControl(seconds)                 => "*" + seconds
    }
}

object TimeControl
{
  /** Creates a new kind of time control from the string.
    *
    * @param s the string.
    * @return a new kind of time control.
    */
  def apply(s: String) =
    parseTimeControl(s) match {
      case Some(timeControl) => timeControl
      case None              => throw new ChessException("Invalid time control")
    }
  
  /** Parses the string and creates an optional kind of time control.
    *
    * @param s the string.
    * @return an optional kind of time control.
    */
  def parseTimeControl(s: String) =
    if(s == "?") {
      Some(UnknownTimeControl)
    } else if(s.matches("[0-9]+/[0-9]+")) {
      val ss = s.split("/")
      try {
        val moveCount = Integer.parseInt(ss(0))
        val seconds = Integer.parseInt(ss(1))
        Some(ClassicalTimeControl(moveCount, seconds))
      } catch {
        case e: NumberFormatException => None
      }
    } else if(s.matches("[0-9]+")) {
      try {
        Some(SuddenDeathTimeControl(Integer.parseInt(s)))
      } catch {
        case e: NumberFormatException => None
      }
    } else if(s.matches("[0-9]+\\+[0-9]+")) {
      val ss = s.split("\\+")
      try {
        val seconds = Integer.parseInt(ss(0))
        val extraSeconds = Integer.parseInt(ss(1))
        Some(IncrementalTimeControl(seconds, extraSeconds))
      } catch {
        case e: NumberFormatException => None
      }
    } else if(s.matches("\\*[0-9]+")) {
      try {
        Some(HourglassTimeControl(Integer.parseInt(s.substring(1))))
      } catch {
        case e: NumberFormatException => None
      }
    } else
      None
}

/** A kind of unknown time control. */
case object UnknownTimeControl extends TimeControl
/** A kind of classical time control.
  * 
  * @param moveCount the number of moves.
  * @param seconds the seconds.
  */
case class ClassicalTimeControl(moveCount: Int, seconds: Int) extends TimeControl
/** A kind of sudden death time control.
  * 
  * @param seconds the seconds.
  */
case class SuddenDeathTimeControl(seconds: Int) extends TimeControl
/** A kind of incremental time control.
  * 
  * @param seconds the seconds.
  * @param extraSeconds the extra seconds.
  */
case class IncrementalTimeControl(seconds: Int, extraSeconds: Int) extends TimeControl
/** A kind of hourglass time control.
  * 
  * @param seconds the seconds.
  */
case class HourglassTimeControl(seconds: Int) extends TimeControl
