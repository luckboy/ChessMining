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

class ConsoleFileProgressBar(name: String, len: Long) extends FileProgressBar
{
  val fileName = name
  val fileLength = len
  private var progress = 0L
  private var isClosed = false
  private var hasError = false

  private def showProgress()
  {
    val n = 80 - 2 - 4
    Console.print("[")
    val x = if(fileLength != 0L) ((progress.min(fileLength) * n.toLong) / fileLength).toInt else n
    Console.print("#" * x)
    Console.print("-" * (n - x))
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
    Console.println(fileName + ":")
    showProgress()
  }

  def updateProgress(n: Long)
  {
    progress = n
    showProgress()
  }

  def close()
  {
    if(!hasError) Console.println()
    isClosed = true
    hasError = false
  }

  def showError(message: String)
  {
    if(!isClosed) Console.println()
    Console.println("Error: " + message)
    hasError = true
  }
}

object ConsoleFileProgressBar extends FileProgressBarFactory
{
  override def apply(name: String, len: Long) = new ConsoleFileProgressBar(name, len)
}