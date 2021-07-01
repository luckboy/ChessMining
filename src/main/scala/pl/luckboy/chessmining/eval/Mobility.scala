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
import Squares._

/** This object provides methods which evaluate mobilities. */
object Mobility
{
  private def pawnMobilityFrom(board: Board, side: Side.Value, from: Int) = {
    val oppSide = ~side
    val enPassantSquOpt = if(board.side == side)
      board.enPassantColumnOption.map {
        _ + (if(board.side == Side.White) A6 else A3)
      }
    else
      None
    foldPawnCaptureSquares(side, from, 0) {
      (sum: Int, to: Int) =>
        if(board.color(to) == sideToColor(oppSide) || enPassantSquOpt.map { _ == to }.getOrElse(false))
          sum + 1
        else
          sum
    } + foldPawnSquares(side, from, 0) {
      (sum: Int, to: Int) =>
        if(board.color(to) == Color.Empty)
          (sum + 1, true)
        else
          (sum, false)
    }
  }

  private def knightMobilityFrom(board: Board, side: Side.Value, from: Int) = {
    val oppSide = ~side
    foldKnightSquares(from, 0) {
      (sum: Int, to: Int) =>
        if(board.color(to) == sideToColor(oppSide) || board.color(to) == Color.Empty) 
          sum + 1
        else
          sum
    }
  }

  private def bishopMobilityFrom(board: Board, side: Side.Value, from: Int) = {
    val oppSide = ~side
    foldBishopSlides(from, 0) { (sum: Int) => sum } {
      (sum: Int, to: Int) =>
        if(board.color(to) == sideToColor(oppSide) || board.color(to) == Color.Empty)
          (sum + 1, board.color(to) == Color.Empty)
        else
          (sum, false)
    }
  }

  private def rookMobilityFrom(board: Board, side: Side.Value, from: Int) = {
    val oppSide = ~side
    foldRookSlides(from, 0) { (sum: Int) => sum } {
      (sum: Int, to: Int) =>
        if(board.color(to) == sideToColor(oppSide) || board.color(to) == Color.Empty)
          (sum + 1, board.color(to) == Color.Empty)
        else
          (sum, false)
    }
  }

  private def queenMobilityFrom(board: Board, side: Side.Value, from: Int) = {
    val oppSide = ~side
    foldQueenSlides(from, 0) { (sum: Int) => sum } {
      (sum: Int, to: Int) =>
        if(board.color(to) == sideToColor(oppSide) || board.color(to) == Color.Empty)
          (sum + 1, board.color(to) == Color.Empty)
        else
          (sum, false)
    }
  }

  private def kingMobilityFrom(board: Board, side: Side.Value, from: Int) = {
    val oppSide = ~side
    foldKingSquares(from, 0) {
      (sum: Int, to: Int) =>
        if(board.color(to) == sideToColor(oppSide) || board.color(to) == Color.Empty) 
          sum + 1
        else
          sum
    }
  }
  
  /** Evaluates a mobility.
    *
    * @param board the board.
    * @param side the side.
    * @return a mobility.
    */
  def mobility(board: Board, side: Side.Value) =
    (0 until 64).foldLeft(0) {
      (sum: Int, from: Int) =>
        if(board.color(from) == sideToColor(side)) {
          board.pieceOption(from) match {
            case Some(Piece.Pawn)   => sum + pawnMobilityFrom(board, side, from)
            case Some(Piece.Knight) => sum + knightMobilityFrom(board, side, from)
            case Some(Piece.Bishop) => sum + bishopMobilityFrom(board, side, from)
            case Some(Piece.Rook)   => sum + rookMobilityFrom(board, side, from)
            case Some(Piece.Queen)  => sum + queenMobilityFrom(board, side, from)
            case Some(Piece.King)   => sum + kingMobilityFrom(board, side, from)
            case _                  => sum
          }
        } else
          sum
    }

  /** Evaluates a pawn mobility.
    *
    * @param board the board.
    * @param side the side.
    * @return a pawn mobility.
    */
  def pawnMobility(board: Board, side: Side.Value) =
    (0 until 64).foldLeft(0) {
      (sum: Int, from: Int) =>
        if(board.coloredPiece(from) == sideAndPieceToColoredPiece(side, Piece.Pawn))
          sum + pawnMobilityFrom(board, side, from)
        else
          sum
    }

  /** Evaluates a knight mobility.
    *
    * @param board the board.
    * @param side the side.
    * @return a knight mobility.
    */
  def knightMobility(board: Board, side: Side.Value) =
    (0 until 64).foldLeft(0) {
      (sum: Int, from: Int) =>
        if(board.coloredPiece(from) == sideAndPieceToColoredPiece(side, Piece.Knight))
          sum + knightMobilityFrom(board, side, from)
        else
          sum
    }

  /** Evaluates a bishop mobility.
    *
    * @param board the board.
    * @param side the side.
    * @return a bishop mobility.
    */
  def bishopMobility(board: Board, side: Side.Value) =
    (0 until 64).foldLeft(0) {
      (sum: Int, from: Int) =>
        if(board.coloredPiece(from) == sideAndPieceToColoredPiece(side, Piece.Bishop))
          sum + bishopMobilityFrom(board, side, from)
        else
          sum
    }

  /** Evaluates a rook mobility.
    *
    * @param board the board.
    * @param side the side.
    * @return a rook mobility.
    */
  def rookMobility(board: Board, side: Side.Value) =
    (0 until 64).foldLeft(0) {
      (sum: Int, from: Int) =>
        if(board.coloredPiece(from) == sideAndPieceToColoredPiece(side, Piece.Rook))
          sum + rookMobilityFrom(board, side, from)
        else
          sum
    }

  /** Evaluates a queen mobility.
    *
    * @param board the board.
    * @param side the side.
    * @return a queen mobility.
    */
  def queenMobility(board: Board, side: Side.Value) =
    (0 until 64).foldLeft(0) {
      (sum: Int, from: Int) =>
        if(board.coloredPiece(from) == sideAndPieceToColoredPiece(side, Piece.Queen))
          sum + queenMobilityFrom(board, side, from)
        else
          sum
    }

  /** Evaluates a king mobility.
    *
    * @param board the board.
    * @param side the side.
    * @return a king mobility.
    */
  def kingMobility(board: Board, side: Side.Value) =
    (0 until 64).foldLeft(0) {
      (sum: Int, from: Int) =>
        if(board.coloredPiece(from) == sideAndPieceToColoredPiece(side, Piece.King))
          sum + kingMobilityFrom(board, side, from)
        else
          sum
    }
}
