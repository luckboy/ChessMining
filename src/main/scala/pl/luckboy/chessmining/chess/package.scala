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

  def charToColoredPieceOption(c: Char) =
    c match {
      case ' ' => Some(ColoredPiece.Empty)
      case 'P' => Some(ColoredPiece.WhitePawn)
      case 'N' => Some(ColoredPiece.WhiteKnight)
      case 'B' => Some(ColoredPiece.WhiteBishop)
      case 'R' => Some(ColoredPiece.WhiteRook)
      case 'Q' => Some(ColoredPiece.WhiteQueen)
      case 'K' => Some(ColoredPiece.WhiteKing)
      case 'p' => Some(ColoredPiece.BlackPawn)
      case 'n' => Some(ColoredPiece.BlackKnight)
      case 'b' => Some(ColoredPiece.BlackBishop)
      case 'r' => Some(ColoredPiece.BlackRook)
      case 'q' => Some(ColoredPiece.BlackQueen)
      case 'k' => Some(ColoredPiece.BlackKing)
      case _   => None
    }

  def charToPieceOption(c: Char) =
    c match {
      case 'P' => Some(Piece.Pawn)
      case 'N' => Some(Piece.Knight)
      case 'B' => Some(Piece.Bishop)
      case 'R' => Some(Piece.Rook)
      case 'Q' => Some(Piece.Queen)
      case 'K' => Some(Piece.King)
      case _   => None
    }

  def charToPromotionPieceOption(c: Char) =
    c match {
      case 'N' => Some(PromotionPiece.Knight)
      case 'B' => Some(PromotionPiece.Bishop)
      case 'R' => Some(PromotionPiece.Rook)
      case 'Q' => Some(PromotionPiece.Queen)
      case _   => None
    }

  def charToSideOption(c: Char) =
    c match {
      case 'w' => Some(Side.White)
      case 'b' => Some(Side.Black)
      case _   => None
    }
    
  def charToColumnOption(c: Char) =
    if(c >= 'a' && c <= 'h')
      Some(c.toInt - 'a'.toInt)
    else
      None

  def charToRowOption(c: Char) =
    if(c >= '1' && c <= '8')
      Some(c.toInt - '1'.toInt)
    else
      None

  def stringToSquareOption(s: String) =
    if(s.length == 2) {
      for {
        col <- charToColumnOption(s(0))
        row <- charToRowOption(s(1))
      } yield col + (row << 3)
    } else
      None

  def coloredPieceToChar(coloredPiece: ColoredPiece.Value) =
    coloredPiece match {
      case ColoredPiece.Empty       => ' '
      case ColoredPiece.WhitePawn   => 'P'
      case ColoredPiece.WhiteKnight => 'N'
      case ColoredPiece.WhiteBishop => 'B'
      case ColoredPiece.WhiteRook   => 'R'
      case ColoredPiece.WhiteQueen  => 'Q'
      case ColoredPiece.WhiteKing   => 'K'
      case ColoredPiece.BlackPawn   => 'p'
      case ColoredPiece.BlackKnight => 'n'
      case ColoredPiece.BlackBishop => 'b'
      case ColoredPiece.BlackRook   => 'r'
      case ColoredPiece.BlackQueen  => 'q'
      case ColoredPiece.BlackKing   => 'k'
    }
    
  def pieceToChar(piece: Piece.Value) =
    piece match {
      case Piece.Pawn   => 'P'
      case Piece.Knight => 'N'
      case Piece.Bishop => 'B'
      case Piece.Rook   => 'R'
      case Piece.Queen  => 'Q'
      case Piece.King   => 'K'
    }

  def promotionPieceToChar(promotionPiece: PromotionPiece.Value) =
    promotionPiece match {
      case PromotionPiece.Knight => 'N'
      case PromotionPiece.Bishop => 'B'
      case PromotionPiece.Rook   => 'R'
      case PromotionPiece.Queen  => 'Q'
    }

  def sideToChar(side: Side.Value) =
    side match {
      case Side.White => 'w'
      case Side.Black => 'b'
    }

  def columnToChar(col: Int) = (col + 'a'.toInt).toChar
  
  def rowToChar(row: Int) = (row + '1'.toInt).toChar
  
  def squareToString(squ: Int) = {
    val sb = new StringBuilder()
    sb += columnToChar(squ & 7)
    sb += rowToChar(squ >> 3)
    sb.toString()
  }
}
