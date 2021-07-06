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

/** This object provides method that evaluates space. */
object Space
{
  /** Evaluates a space.
    *
    * @param board the board.
    * @param side the side.
    * @return a space.
    */
  def space(board: Board, side: Side.Value) = {
    val oppSide = ~side
    val attackArray = (0 until 64).foldLeft(Array.fill(64)(false)) {
      (array: Array[Boolean], from: Int) =>
        if(board.color(from) == sideToColor(oppSide)) {
          board.pieceOption(from) match {
            case Some(Piece.Pawn)   =>
              foldPawnCaptureSquares(oppSide, from, array) {
                (array2: Array[Boolean], to: Int) =>
                  array2(to) = true
                  array2
              }
            case Some(Piece.Knight) => 
              foldKnightSquares(from, array) {
                (array2: Array[Boolean], to: Int) =>
                  array2(to) = true
                  array2
              }
            case Some(Piece.Bishop) =>
              foldBishopSlides(from, array) { (array2: Array[Boolean]) => array2 } {
                (array2: Array[Boolean], to: Int) =>
                  array2(to) = true
                  (array2, board.color(to) == Color.Empty)
              }
            case Some(Piece.Rook)   =>
              foldRookSlides(from, array) { (array2: Array[Boolean]) => array2 } {
                (array2: Array[Boolean], to: Int) =>
                  array2(to) = true
                  (array2, board.color(to) == Color.Empty)
              }
            case Some(Piece.Queen)  =>
              foldQueenSlides(from, array) { (array2: Array[Boolean]) => array2 } {
                (array2: Array[Boolean], to: Int) =>
                  array2(to) = true
                  (array2, board.color(to) == Color.Empty)
              }
            case Some(Piece.King)   => 
              foldKingSquares(from, array) {
                (array2: Array[Boolean], to: Int) =>
                  array2(to) = true
                  array2
              }
            case _                  =>
              array
          }
        } else
          array
    }
    val booleanPieceArray = (0 until 64).foldLeft(Array.fill(64)(false)) {
      (array: Array[Boolean], squ: Int) => 
        if(board.color(squ) == sideToColor(side)) array(squ) = true
        array
    }
    attackArray.zip(booleanPieceArray).map {
      case ((b1: Boolean, b2: Boolean)) => !b1 && !b2
    }.count { (b: Boolean) => b }
  }
}
