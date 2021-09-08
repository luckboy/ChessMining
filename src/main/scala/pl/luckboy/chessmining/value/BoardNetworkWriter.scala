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

/** A writer of board network.
  *
  * @constructor Creates a new writer of board network.
  *
  * @param w writer.
  */
class BoardNetworkWriter(w: Writer) extends Closeable
{
  private val writer = w

  private def writeChar(c: Char)
  {
    writer.write(c.toInt)
  }

  private def writeString(s: String)
  {
    writer.write(s)
  }

  /** Writes the board network. 
    *
    * @param boardNetwork the board network.
    */
  def writeBoardNetwork(boardNetwork: BoardNetwork)
  {
    for(sideEdgeCounts <- boardNetwork.edgeCounts) {
      for(rowEdgeCounts <- sideEdgeCounts) {
        var isFirst = true
        for(edgeCount <- rowEdgeCounts) {
          if(!isFirst) writeChar(' ')
          writeString(edgeCount.toString())
          isFirst = false
        }
        writeChar('\n')
      }
      writeChar('\n')
    }
  }

  override def close()
  {
    writer.close()
  }
}
