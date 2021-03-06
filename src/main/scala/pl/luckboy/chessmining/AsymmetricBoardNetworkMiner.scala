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

/** A miner of asymmetric board network that counts the edges.
  *
  * The example usage is:
  * {{{
  * val miner = AsymmetricBoardNetworkMiner
  * val network = miner(iter)
  * }}}
  */
case object AsymmetricBoardNetworkMiner extends BoardNetworkMiner
{
  override protected def addToEdgeCounts(boardNetwork: BoardNetwork, side: Side.Value, coloredPiece1: ColoredPiece.Value, squ1: Int, coloredPiece2: ColoredPiece.Value, squ2: Int, value: Long)
  {
    boardNetwork.addToEdgeCount(side, coloredPiece1, squ1, coloredPiece2, squ2, value)
  }
}
