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

class DummyFileProgressBar(name: String, len: Long) extends FileProgressBar
{
  val fileName = name
  val fileLength = len

  def show()
  {
  }

  def updateProgress(n: Long)
  {
  }

  def close()
  {
  }

  def showError(message: String)
  {
  }
}

object DummyFileProgressBar extends FileProgressBarFactory
{
  override def apply(name: String, len: Long) = new DummyFileProgressBar(name, len)
}