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
import pl.luckboy.chessmining.chess._

object KingTropism
{
  def kingTropism(board: Board, side: Side.Value) =
    (0 until 64).find { board.coloredPiece(_) == sideAndPieceToColoredPiece(side, Piece.King) }.map {
      (to: Int) =>
        val oppSide = ~side
        foldBishopSlides(to, (0, 0)) { (tuple: (Int, Int)) => (tuple._1, 0) } {
          (tuple: (Int, Int), from: Int) =>
            if(board.coloredPiece(from) == sideAndPieceToColoredPiece(oppSide, Piece.Bishop) ||
              board.coloredPiece(from) == sideAndPieceToColoredPiece(oppSide, Piece.Queen))
              ((tuple._1 + (7 - (tuple._2 + 1)), 0), false)
            else
              ((tuple._1, tuple._2 + 1), true)
        }._1 + foldRookSlides(to, (0, 0)) { (tuple: (Int, Int)) => (tuple._1, 0) } {
          (tuple: (Int, Int), from: Int) =>
            if(board.coloredPiece(from) == sideAndPieceToColoredPiece(oppSide, Piece.Rook) ||
              board.coloredPiece(from) == sideAndPieceToColoredPiece(oppSide, Piece.Queen))
              ((tuple._1 + (7 - (tuple._2 + 1)), 0), false)
            else
              ((tuple._1, tuple._2 + 1), true)
        }._1
    }.getOrElse(0)
}
