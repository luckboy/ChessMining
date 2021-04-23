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

sealed abstract class Move
{
  override def toString() =
    this match {
      case NormalMove(piece, from, to, promotionPieceOpt, isCapture) =>
        val sb = new StringBuilder()
        sb += pieceToChar(piece)
        sb ++= squareToString(from)
        if(isCapture) sb += 'x'
        sb ++= squareToString(to)
        for(promotionPiece <- promotionPieceOpt) {
          sb += '='
          sb += promotionPieceToChar(promotionPiece)
        }
        sb.toString()
      case ShortCastling =>
        "O-O"
      case LongCastling =>
        "O-O-O"
    }
}

case class NormalMove(piece: Piece.Value, from: Int, to: Int, promotionPieceOption: Option[PromotionPiece.Value], isCapture: Boolean) extends Move
case object ShortCastling extends Move
case object LongCastling extends Move
