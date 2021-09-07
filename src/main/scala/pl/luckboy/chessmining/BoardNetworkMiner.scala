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
package pl.luckboy.chessmining
import pl.luckboy.chessmining.chess._
import pl.luckboy.chessmining.value._

abstract class BoardNetworkMiner extends Miner[(Game, Board), BoardNetwork]
{
  protected def addToEdgeCounts(boardNetwork: BoardNetwork, side: Side.Value, coloredPiece1: ColoredPiece.Value, squ1: Int, coloredPiece2: ColoredPiece.Value, squ2: Int, value: Long): Unit

  override def startValue = BoardNetwork()

  override def function(x: BoardNetwork, y: (Game, Board)) = {
    for(side <- y._1.winSideOption) {
      val oppSide = ~side
      for(squ1 <- 0 until 64) {
        for(squ2 <- 0 until 64) {
          addToEdgeCounts(x, side, y._2.coloredPiece(squ1), squ1, y._2.coloredPiece(squ2), squ2, 1L)
          addToEdgeCounts(x, oppSide, y._2.coloredPiece(squ1), squ1, y._2.coloredPiece(squ2), squ2, -1L)
        }
      }
    }
    x
  }
}
