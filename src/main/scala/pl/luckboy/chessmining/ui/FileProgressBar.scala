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

/** A file progress bar that shows the progress of file reading. */
abstract class FileProgressBar
{
  /** Returns the file name.
    *
    * @return the file name.
    */
  def fileName: String

  /** Returns the file length.
    *
    * @return the file length.
    */
  def fileLength: Long

  /** Shows the progress bar. */
  def show(): Unit
  
  /** Updates the progress.
    *
    * @param count the number of bytes.
    */
  def updateProgress(count: Long): Unit

  /** Closes the progress bar. */
  def close(): Unit

  /** Shows an error. */
  def showError(msg: String): Unit
}
