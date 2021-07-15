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

object OpenLine
{
  def isOpenLine(board: Board, side: Side.Value, squ: Int) =
    foldBishopSlides(squ, false) { (b: Boolean) => b } {
      (b: Boolean, from: Int) =>
        if(board.coloredPiece(from) == sideAndPieceToColoredPiece(side, Piece.Bishop) ||
          board.coloredPiece(from) == sideAndPieceToColoredPiece(side, Piece.Queen))
          (true, false)
        else
          (b, board.pieceOption(from).map { _ != Piece.Pawn }.getOrElse(true))
    } || foldBishopSlides(squ, false) { (b: Boolean) => b } {
      (b: Boolean, from: Int) =>
        if(board.coloredPiece(from) == sideAndPieceToColoredPiece(side, Piece.Rook) ||
          board.coloredPiece(from) == sideAndPieceToColoredPiece(side, Piece.Queen))
          (true, false)
        else
          (b, board.pieceOption(from).map { _ != Piece.Pawn }.getOrElse(true))
    }

  def isSemiOpenLine(board: Board, side: Side.Value, squ: Int) =
    foldBishopSlides(squ, false) { (b: Boolean) => b } {
      (b: Boolean, from: Int) =>
        if(board.coloredPiece(from) == sideAndPieceToColoredPiece(side, Piece.Bishop) ||
          board.coloredPiece(from) == sideAndPieceToColoredPiece(side, Piece.Queen))
          (true, false)
        else
          (b, board.coloredPiece(from) != sideAndPieceToColoredPiece(side, Piece.Pawn))
    } || foldBishopSlides(squ, false) { (b: Boolean) => b } {
      (b: Boolean, from: Int) =>
        if(board.coloredPiece(from) == sideAndPieceToColoredPiece(side, Piece.Rook) ||
          board.coloredPiece(from) == sideAndPieceToColoredPiece(side, Piece.Queen))
          (true, false)
        else
          (b, board.coloredPiece(from) != sideAndPieceToColoredPiece(side, Piece.Pawn))
    }
}
