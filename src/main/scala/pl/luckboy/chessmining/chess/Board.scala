/*
 * Chess Mining - Library to data mining for chess games.
 * Copyright (C) 2021 Łukasz Szpakowski
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as
 * published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
package pl.luckboy.chessmining.chess
import ColoredPieces._

case class Board(
  pieces: Array[ColoredPiece.Value],
  side: Side.Value,
  castlings: Array[SideCastlings.Value],
  enPassantColumnOption: Option[Int],
  halfmoveClock: Int,
  fullmoveNumber: Int)
{
  override def equals(that: Any) =
    that match {
      case board: Board =>
        !(0 until 64).exists { (squ: Int) => pieces(squ) != board.pieces(squ) } &&
        side == board.side &&
        !(0 until 2).exists { (sideId: Int) => castlings(sideId) != board.castlings(sideId) } &&
        enPassantColumnOption == board.enPassantColumnOption &&
        halfmoveClock == board.halfmoveClock &&
        fullmoveNumber == board.fullmoveNumber
      case _            => false
    }
  
  def coloredPiece(squ: Int) = pieces(squ)

  def color(squ: Int) = coloredPieceToColor(pieces(squ))

  def pieceOption(squ: Int) = coloredPieceToPieceOption(pieces(squ))

  def sideCastlings(side: Side.Value) = castlings(side.id)
}

object Board
{
  val Initial = Board(
      Array(
        WR, WN, WB, WQ, WK, WB, WN, WR,
        WP, WP, WP, WP, WP, WP, WP, WP,
        Em, Em, Em, Em, Em, Em, Em, Em,
        Em, Em, Em, Em, Em, Em, Em, Em,
        Em, Em, Em, Em, Em, Em, Em, Em,
        Em, Em, Em, Em, Em, Em, Em, Em,
        BP, BP, BP, BP, BP, BP, BP, BP,
        BR, BN, BB, BQ, BK, BB, BN, BR),
      Side.White,
      Array(SideCastlings.All, SideCastlings.All),
      None,
      0, 1)
}
