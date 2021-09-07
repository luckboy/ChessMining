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
package pl.luckboy.chessmining.eval
import pl.luckboy.chessmining._
import pl.luckboy.chessmining.chess._

object BoardNetwork
{
  def boardNetwork(boardNetwork: value.BoardNetwork, board: Board, side: Side.Value)(f: (Long) => Boolean) = {
    val sum = (0 until 64).foldLeft(0) {
      (sum: Int, squ1: Int) =>
        val coloredPiece1 = board.coloredPiece(squ1) 
        (0 until 64).foldLeft(sum) {
          (sum2: Int, squ2) =>
            if(squ1 != squ2) {
              val coloredPiece2 = board.coloredPiece(squ2)
              val count = boardNetwork.edgeCount(side, coloredPiece1, squ1, coloredPiece2, squ2)
              if(f(count)) sum2 + 1 else sum2
            } else
              sum2
        }
    }
    math.sqrt(sum.toDouble).toInt
  }
}
