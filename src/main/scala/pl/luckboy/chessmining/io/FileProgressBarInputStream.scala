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
package pl.luckboy.chessmining.io
import java.io._
import pl.luckboy.chessmining.ui._

/** An input stream that updates the progress for the file progress bar during reading.
  *
  * @constructor Creates a new input stream.
  *
  * @param is the input stream.
  * @param fpb the file progress bar.
  */
class FileProgressBarInputStream(is: InputStream, fpb: FileProgressBar) extends InputStream
{
  private val inputStream = is
  private val fileProgressBar = fpb
  private var byteCount = 0L
  private var partByteCount = 0L
  private var markedByteCount = 0L
  private var markedPartByteCount = 0L
  private var hasFirstByte = true

  override def available() = inputStream.available()

  override def close()
  {
    fileProgressBar.close()
    inputStream.close()
  }

  override def mark(readlimit: Int)
  {
    inputStream.mark(readlimit)
    markedByteCount = byteCount
    markedPartByteCount = partByteCount
  }

  override def markSupported() = inputStream.markSupported()

  private def addToProgress(count: Long)
  {
    byteCount += count
    partByteCount += count
    if(partByteCount >= FileProgressBarInputStream.MaxProgressUpdatingPartByteCount.toLong || byteCount >= fileProgressBar.fileLength) {
      fileProgressBar.updateProgress(byteCount)
      partByteCount %= FileProgressBarInputStream.MaxProgressUpdatingPartByteCount.toLong
    }
  }
  
  override def read() = {
    val b = inputStream.read()
    if(b != -1) addToProgress(1L)
    b
  }

  override def read(b: Array[Byte], off: Int, len: Int) = {
    val r = inputStream.read(b, off, len)
    if(r != -1) addToProgress(r.toLong)
    r
  }
  
  override def reset() 
  {
    inputStream.reset()
    byteCount = markedByteCount
    partByteCount = markedPartByteCount
    fileProgressBar.updateProgress(byteCount)
  }
  
  override def skip(n: Long) = {
    val r = inputStream.skip(n)
    addToProgress(r)
    r
  }
}

object FileProgressBarInputStream
{
  /** A maximal number of bytes for a file part after which this input stream updates the progress. */
  val MaxProgressUpdatingPartByteCount = 64 * 1024
}
