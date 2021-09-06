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

class BoardNetworkReader(r: Reader) extends NumberReader(r)
{
  def readBoardNetwork() = {
    val boardNetwork = BoardNetwork()
    var sideId = 0
    var isStop = false
    while(sideId < 2 && !isStop) {
      var i = 0
      while(i < 13 * 64 && !isStop) {
        var j = 0
        while(j < 13 * 64 && !isStop) {
          readLong() match {
            case Some(value) => boardNetwork.edgeCounts(sideId)(i)(j) = value
            case None        => isStop = true
          }
          j += 1
        }
        i += 1
      }
      sideId += 1
    }
    if(!isStop)
      Some(boardNetwork)
    else
      None
  }
}
