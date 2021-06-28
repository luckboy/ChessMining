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
package pl.luckboy.chessmining.ui

/** A console file progress bar that shows the progress of file reading on the console.
  *
  * @constructor Creates a new progress bar of file of console.
  *
  * @param name the file name.
  * @param len the file length.
  */
class ConsoleFileProgressBar(name: String, len: Long) extends FileProgressBar
{
  /** The file name. */
  val fileName = name
  /** The file length. */
  val fileLength = len
  private var progress = 0L
  private var isClosed = true
  private var hasError = false

  private def showProgress()
  {
    val count = 80 - 2 - 4
    Console.print("[")
    val x = if(fileLength != 0L) ((progress.min(fileLength) * count.toLong) / fileLength).toInt else count
    Console.print("#" * x)
    Console.print("-" * (count - x))
    Console.print("]")
    val y = if(fileLength != 0L) ((progress.min(fileLength) * 100L) / fileLength).toInt else 100
    val s = y.toString()
    Console.print(" " * (3 - s.length))
    Console.print(s)
    Console.print("%")
    Console.print("\r")
    Console.flush()
  }

  def show()
  {
    isClosed = false
    Console.println(fileName + ":")
    showProgress()
  }

  def updateProgress(count: Long)
  {
    progress = count
    if(!isClosed) showProgress()
  }

  def close()
  {
    if(!isClosed && !hasError) Console.println()
    isClosed = true
    hasError = false
  }

  def showError(msg: String)
  {
    if(!isClosed) Console.println()
    Console.println("Error: " + msg)
    hasError = true
  }
}

/** A factory of console file progress bar. */
object ConsoleFileProgressBar extends FileProgressBarFactory
{
  override def apply(name: String, len: Long) = new ConsoleFileProgressBar(name, len)
}
