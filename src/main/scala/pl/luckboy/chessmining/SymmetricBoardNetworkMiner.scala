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

case object SymmetricBoardNetworkMiner extends BoardNetworkMiner
{
  private def reverseSide(side: Side.Value) = ~side

  private def reverseColoredPiece(coloredPiece: ColoredPiece.Value) = {
    coloredPieceToSideOption(coloredPiece) match {
      case Some(side) =>
        coloredPieceToPieceOption(coloredPiece) match {
          case Some(piece) => sideAndPieceToColoredPiece(~side, piece)
          case None        => ColoredPiece.Empty
        }
      case None       =>
        ColoredPiece.Empty
    }
  }

  private def reverseSquare(squ: Int) = ((7 - (squ >> 3)) << 3) | (squ & 7)

  override protected def updatePairCounts(boardNetwork: BoardNetwork, side: Side.Value, coloredPiece1: ColoredPiece.Value, squ1: Int, coloredPiece2: ColoredPiece.Value, squ2: Int, value: Long)
  {
    boardNetwork.addToPairCount(side, coloredPiece1, squ1, coloredPiece2, squ2, value)
    val invSide = reverseSide(side)
    val invColoredPiece1 = reverseColoredPiece(coloredPiece1)
    val invSqu1 = reverseSquare(squ1)
    val invColoredPiece2 = reverseColoredPiece(coloredPiece2)
    val invSqu2 = reverseSquare(squ2)
    boardNetwork.addToPairCount(invSide, invColoredPiece1, invSqu1, invColoredPiece2, invSqu2, value)
  }
}
