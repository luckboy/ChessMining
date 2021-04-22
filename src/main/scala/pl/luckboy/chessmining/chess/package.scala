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
package pl.luckboy.chessmining

package object chess
{
  implicit class RichSide(side: Side.Value)
  {
    def unary_~ = Side(side.id ^ 1)
  }

  implicit class RichSideCastlings(sideCastlings: SideCastlings.Value)
  {
    def unary_~ = SideCastlings(sideCastlings.id ^ 3)

    def &(sideCastlings2: SideCastlings.Value) = SideCastlings(sideCastlings.id & sideCastlings2.id)

    def |(sideCastlings2: SideCastlings.Value) = SideCastlings(sideCastlings.id | sideCastlings2.id)

    def ^(sideCastlings2: SideCastlings.Value) = SideCastlings(sideCastlings.id ^ sideCastlings2.id)
  }

  def coloredPieceToColor(coloredPiece: ColoredPiece.Value) = Color(coloredPiece.id >> 3)

  def coloredPieceToPieceOption(coloredPiece: ColoredPiece.Value) =
    coloredPiece match {
      case ColoredPiece.Empty => None
      case _                  => Some(Piece(coloredPiece.id & 7))
    }

  def coloredPieceToPromotionPieceOption(coloredPiece: ColoredPiece.Value) =
    coloredPiece match {
      case ColoredPiece.Empty => None
      case _                  => pieceToPromotionPieceOption(Piece(coloredPiece.id & 7))
    }

  def coloredPieceToSideOption(coloredPiece: ColoredPiece.Value) =
    coloredPiece match {
      case ColoredPiece.Empty => None
      case _                  => Some(Side((coloredPiece.id >> 3) - 1))
    }

  def colorAndPieceToColoredPiece(color: Color.Value, piece: Piece.Value) =
    color match {
      case Color.Empty => ColoredPiece.Empty
      case _           => ColoredPiece((color.id << 3) | piece.id)
    }

  def colorAndPromotionPieceToColoredPiece(color: Color.Value, promotionPiece: PromotionPiece.Value) =
    color match {
      case Color.Empty => ColoredPiece.Empty
      case _           => ColoredPiece((color.id << 3) | promotionPiece.id)
    }

  def sideAndPieceToColoredPiece(side: Side.Value, piece: Piece.Value) = ColoredPiece(((side.id + 1) << 3) | piece.id)

  def sideAndPromotionPieceToColoredPiece(side: Side.Value, promotionPiece: PromotionPiece.Value) = ColoredPiece(((side.id + 1) << 3) | promotionPiece.id)

  def colorToSideOption(color: Color.Value) =
    color match {
      case Color.Empty => None
      case _           => Some(Side(color.id - 1))
    }

  def pieceToPromotionPieceOption(piece: Piece.Value) =
    piece match {
      case Piece.Pawn | Piece.King => None
      case _                       => Some(PromotionPiece(piece.id))
    }
  
  def promotionPieceToPiece(promotionPiece: PromotionPiece.Value) = Piece(promotionPiece.id)

  def sideToColor(side: Side.Value) = Color(side.id + 1)
}
