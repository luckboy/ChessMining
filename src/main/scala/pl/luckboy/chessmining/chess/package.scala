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

package object chess
{
  import Tables._

  /** A rich class of side. */ 
  implicit class RichSide(side: Side.Value)
  {
    /** Returns an opposite side.
      *
      * @return an opposite side.
      */
    def unary_~ = Side(side.id ^ 1)
  }

  /** A rich class of side castlings. */ 
  implicit class RichSideCastlings(sideCastlings: SideCastlings.Value)
  {
    /** Returns side castlings of bitwise negation.
      * 
      * @return side castlings of bitwise negation.
      */
    def unary_~ = SideCastlings(sideCastlings.id ^ 3)

    /** Returns side castlings of bitwise AND.
      *
      * @param sideCastlings2 the side castlings.
      * @return side castlings of bitwise AND.
      */
    def &(sideCastlings2: SideCastlings.Value) = SideCastlings(sideCastlings.id & sideCastlings2.id)

    /** Returns side castlings of bitwise OR.
      *
      * @param sideCastlings2 the side castlings.
      * @return side castlings of bitwise OR.
      */
    def |(sideCastlings2: SideCastlings.Value) = SideCastlings(sideCastlings.id | sideCastlings2.id)

    /** Returns side castlings of bitwise XOR.
      *
      * @param sideCastlings2 the side castlings.
      * @return side castlings of bitwise XOR.
      */
    def ^(sideCastlings2: SideCastlings.Value) = SideCastlings(sideCastlings.id ^ sideCastlings2.id)
  }

  /** Converts the colored piece to a color.
    *
    * @param coloredPiece the colored piece.
    * @return a color.
    */
  def coloredPieceToColor(coloredPiece: ColoredPiece.Value) = Color(coloredPiece.id >> 3)

  /** Converts the colored piece to an optional piece.
    *
    * @param coloredPiece the colored piece.
    * @return an optional piece.
    */
  def coloredPieceToPieceOption(coloredPiece: ColoredPiece.Value) =
    coloredPiece match {
      case ColoredPiece.Empty => None
      case _                  => Some(Piece(coloredPiece.id & 7))
    }

  /** Converts the colored piece to an optional promotion piece.
    *
    * @param coloredPiece the colored piece.
    * @return an optional promotion piece.
    */
  def coloredPieceToPromotionPieceOption(coloredPiece: ColoredPiece.Value) =
    coloredPiece match {
      case ColoredPiece.Empty => None
      case _                  => pieceToPromotionPieceOption(Piece(coloredPiece.id & 7))
    }

  /** Converts the colored piece to an optional side.
    *
    * @param coloredPiece the colored piece.
    * @return an optional side.
    */
  def coloredPieceToSideOption(coloredPiece: ColoredPiece.Value) =
    coloredPiece match {
      case ColoredPiece.Empty => None
      case _                  => Some(Side((coloredPiece.id >> 3) - 1))
    }

  /** Converts the color and the piece to a colored piece.
    *
    * @param color the color.
    * @param piece the piece.
    * @return a colored piece.
    */
  def colorAndPieceToColoredPiece(color: Color.Value, piece: Piece.Value) =
    color match {
      case Color.Empty => ColoredPiece.Empty
      case _           => ColoredPiece((color.id << 3) | piece.id)
    }

  /** Converts the color and the promotion piece to a colored piece.
    *
    * @param color the color.
    * @param piece the promotion piece.
    * @return a colored piece.
    */
  def colorAndPromotionPieceToColoredPiece(color: Color.Value, promotionPiece: PromotionPiece.Value) =
    color match {
      case Color.Empty => ColoredPiece.Empty
      case _           => ColoredPiece((color.id << 3) | promotionPiece.id)
    }

  /** Converts the side and the piece to a colored piece.
    *
    * @param side the side.
    * @param piece the piece.
    * @return a colored piece.
    */
  def sideAndPieceToColoredPiece(side: Side.Value, piece: Piece.Value) = ColoredPiece(((side.id + 1) << 3) | piece.id)

  /** Converts the side and the promotion piece to a colored piece.
    *
    * @param side the side.
    * @param piece the promotion piece.
    * @return a colored piece.
    */
  def sideAndPromotionPieceToColoredPiece(side: Side.Value, promotionPiece: PromotionPiece.Value) = ColoredPiece(((side.id + 1) << 3) | promotionPiece.id)

  /** Converts the color to a optional side.
    *
    * @param color the color.
    * @return an optional side.
    */
  def colorToSideOption(color: Color.Value) =
    color match {
      case Color.Empty => None
      case _           => Some(Side(color.id - 1))
    }

  /** Converts the piece to an optional promotion piece.
    *
    * @param piece the piece.
    * @return an optional promotion piece.
    */
  def pieceToPromotionPieceOption(piece: Piece.Value) =
    piece match {
      case Piece.Pawn | Piece.King => None
      case _                       => Some(PromotionPiece(piece.id))
    }
  
  /** Converts the promotion piece to a piece.
    *
    * @param promotionPiece the promotion piece.
    * @return a piece.
    */
  def promotionPieceToPiece(promotionPiece: PromotionPiece.Value) = Piece(promotionPiece.id)

  /** Converts the side to a color. 
    *
    * @param side the side.
    * @return a color.
    */
  def sideToColor(side: Side.Value) = Color(side.id + 1)

  /** Checks whether the game result is side win.
    *
    * @param result the game result.
    * @param side the side.
    * @return `true` if the game result is side win, otherwise `false`.
    */
  def isSideWin(result: Result.Value, side: Side.Value) = 
    side match {
      case Side.White =>
        result match {
          case Result.WhiteWin => true
          case _               => false
        }
      case Side.Black =>
        result match {
          case Result.BlackWin => true
          case _               => false
        }
    }

  /** Checks whether the game result is side loss.
    *
    * @param result the game result.
    * @param side the side.
    * @return `true` if the game result is side loss, otherwise `false`.
    */
  def isSideLoss(result: Result.Value, side: Side.Value) = 
    side match {
      case Side.White =>
        result match {
          case Result.BlackWin => true
          case _               => false
        }
      case Side.Black =>
        result match {
          case Result.WhiteWin => true
          case _               => false
        }
    }

  /** Checks whether the game result is draw.
    *
    * @param result the game result.
    * @return `true` if the game result is draw, otherwise `false`.
    */
  def isDraw(result: Result.Value) = result == Result.Draw

  /** Converts the game result to an optional win side.
    *
    * @param result the game result.
    * @return an optional win side.
    */
  def resultToWinSideOption(result: Result.Value) = 
    result match {
      case Result.WhiteWin => Some(Side.White)
      case Result.BlackWin => Some(Side.Black)
      case _               => None
    }

  /** Converts the game result to an optional loss side.
    *
    * @param result the game result.
    * @return an optional loss side.
    */
  def resultToLossSideOption(result: Result.Value) = 
    result match {
      case Result.WhiteWin => Some(Side.Black)
      case Result.BlackWin => Some(Side.White)
      case _               => None
    }
    
  /** Converts the character to an optional colored piece.
    *
    * @param c the character.
    * @return an optional colored piece.
    */
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

  /** Converts the character to an optional piece.
    *
    * @param c the character.
    * @return an optional piece.
    */
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

  /** Converts the character to an optional promotion piece.
    *
    * @param c the character.
    * @return an optional promotion piece.
    */
  def charToPromotionPieceOption(c: Char) =
    c match {
      case 'N' => Some(PromotionPiece.Knight)
      case 'B' => Some(PromotionPiece.Bishop)
      case 'R' => Some(PromotionPiece.Rook)
      case 'Q' => Some(PromotionPiece.Queen)
      case _   => None
    }

  /** Converts the character to an optional side.
    *
    * @param c the character.
    * @return an optional side.
    */
  def charToSideOption(c: Char) =
    c match {
      case 'w' => Some(Side.White)
      case 'b' => Some(Side.Black)
      case _   => None
    }
    
  /** Converts the character to an optional columm.
    *
    * @param c the character.
    * @return an optional column.
    */
  def charToColumnOption(c: Char) =
    if(c >= 'a' && c <= 'h')
      Some(c.toInt - 'a'.toInt)
    else
      None

  /** Converts the character to an optional row.
    *
    * @param c the character.
    * @return an optional row.
    */
  def charToRowOption(c: Char) =
    if(c >= '1' && c <= '8')
      Some(c.toInt - '1'.toInt)
    else
      None

  /** Converts the string to an optional square.
    *
    * @param s the string.
    * @return an optional square.
    */
  def stringToSquareOption(s: String) =
    if(s.length == 2) {
      for {
        col <- charToColumnOption(s(0))
        row <- charToRowOption(s(1))
      } yield col + (row << 3)
    } else
      None

  /** Checks whether the character is a column character.
    *
    * @param c the character.
    * @return `true` if the character is a column character, otherwise `false`.
    */
  def isColumnChar(c: Char) = (c >= 'a' && c <= 'h')
  
  /** Checks whether the character is a row character.
    *
    * @param c the character.
    * @return `true` if the character is a row character, otherwise `false`.
    */
  def isRowChar(c: Char) = (c >= '1' && c <= '8')
      
  /** Converts the colored piece to a character.
    *
    * @param coloredPiece the colored piece.
    * @return a character.
    */
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
    
  /** Converts the piece to a character.
    *
    * @param piece the piece.
    * @return a character.
    */
  def pieceToChar(piece: Piece.Value) =
    piece match {
      case Piece.Pawn   => 'P'
      case Piece.Knight => 'N'
      case Piece.Bishop => 'B'
      case Piece.Rook   => 'R'
      case Piece.Queen  => 'Q'
      case Piece.King   => 'K'
    }

  /** Converts the promotion piece to a character.
    *
    * @param promotionPiece the promotion piece.
    * @return a character.
    */
  def promotionPieceToChar(promotionPiece: PromotionPiece.Value) =
    promotionPiece match {
      case PromotionPiece.Knight => 'N'
      case PromotionPiece.Bishop => 'B'
      case PromotionPiece.Rook   => 'R'
      case PromotionPiece.Queen  => 'Q'
    }

  /** Converts the side to a character.
    *
    * @param side the side.
    * @return a character.
    */
  def sideToChar(side: Side.Value) =
    side match {
      case Side.White => 'w'
      case Side.Black => 'b'
    }

  /** Converts the column to a character.
    *
    * @param col the column.
    * @return a character.
    */
  def columnToChar(col: Int) = (col + 'a'.toInt).toChar
  
  /** Converts the row to a character.
    *
    * @param row the row.
    * @return a character.
    */
  def rowToChar(row: Int) = (row + '1'.toInt).toChar
  
  /** Converts the square to a string.
    *
    * @param squ the square.
    * @return a string.
    */
  def squareToString(squ: Int) = {
    val sb = new StringBuilder()
    sb += columnToChar(squ & 7)
    sb += rowToChar(squ >> 3)
    sb.toString()
  }
  
  /** Converts the string to an optional game result.
    *
    * @param s the string.
    * @return an optional game result.
    */
  def stringToResultOption(s: String) =
    s match {
      case "1-0"     => Some(Result.WhiteWin)
      case "0-1"     => Some(Result.BlackWin)
      case "1/2-1/2" => Some(Result.Draw)
      case "*"       => Some(Result.Unfinished)
      case _         => None
    }

  /** Converts the game result to a string.
    *
    * @param result the game result.
    * @return a string.
    */
  def resultToString(result: Result.Value) =
    result match {
      case Result.WhiteWin   => "1-0"
      case Result.BlackWin   => "0-1"
      case Result.Draw       => "1/2-1/2"
      case Result.Unfinished => "*"
    }

  /** Folds the pawn capture squares.
    *
    * @tparam T the result type.
    * @param side the side.
    * @param squ the square.
    * @param z the start value.
    * @param f the function.
    * @return a result of folded squares.
    */
  def foldPawnCaptureSquares[T](side: Side.Value, squ: Int, z: T)(f: (T, Int) => T) = {
    var i = 0
    var x = z
    while(i < 2) {
      val to120 = Mailbox64(squ) + TabPawnCaptureSteps120(side.id)(i)
      val to = Mailbox(to120)
      if(to != -1) x = f(x, to)
      i += 1
    }
    x
  }

  /** Folds the pawn squares.
    *
    * @tparam T the result type.
    * @param side the side.
    * @param squ the square.
    * @param z the start value.
    * @param f the function.
    * @return a result of folded squares.
    */
  def foldPawnSquares[T](side: Side.Value, squ: Int, z: T)(f: (T, Int) => (T, Boolean)) = {
    var x = z
    var to120 = Mailbox64(squ) + (if(side == Side.White) 10 else -10)
    var to = Mailbox(to120)
    if(to != -1) {
      val (y, isCont) = f(x, to)
      x = y
      if(isCont) {
        if((squ >> 3) == (if(side == Side.White) 1 else 6)) {
          to120 += (if(side == Side.White) 10 else -10)
          to = Mailbox(to120)
          if(to != -1) {
            val (y2, _) = f(x, to)
            x = y2
          }
        }
      }
    }
    x
  }

  /** Folds the knight squares.
    *
    * @tparam T the result type.
    * @param squ the square.
    * @param z the start value.
    * @param f the function.
    * @return a result of folded squares.
    */
  def foldKnightSquares[T](squ: Int, z: T)(f: (T, Int) => T) = {
    var i = 0
    var x = z
    while(i < 8) {
      val to120 = Mailbox64(squ) + TabKnightSteps120(i)
      val to = Mailbox(to120)
      if(to != -1) x = f(x, to)
      i += 1
    }
    x
  }
  
  /** Folds the bishop slides.
    *
    * @tparam T the result type.
    * @param squ the square.
    * @param z the start value.
    * @param f the slide function.
    * @param g the square function.
    * @return a result of folded slides.
    */
  def foldBishopSlides[T](squ: Int, z: T)(f: T => T)(g: (T, Int) => (T, Boolean)) = {
    var i = 0
    var x = z
    while(i < 4) {
      x = f(x)
      var isStop = false
      var to120 = Mailbox64(squ) + TabBishopSteps120(i)
      var to = Mailbox(to120)
      while(to != -1 && !isStop) {
        val (y, isCont) = g(x, to)
        x = y
        isStop = !isCont
        to120 += TabBishopSteps120(i)
        to = Mailbox(to120)
      }
      i += 1
    }
    x
  }

  /** Folds the rook slides.
    *
    * @tparam T the result type.
    * @param squ the square.
    * @param z the start value.
    * @param f the slide function.
    * @param g the square function.
    * @return a result of folded slides.
    */
  def foldRookSlides[T](squ: Int, z: T)(f: T => T)(g: (T, Int) => (T, Boolean)) = {
    var i = 0
    var x = z
    while(i < 4) {
      x = f(x)
      var isStop = false
      var to120 = Mailbox64(squ) + TabRookSteps120(i)
      var to = Mailbox(to120)
      while(to != -1 && !isStop) {
        val (y, isCont) = g(x, to)
        x = y
        isStop = !isCont
        to120 += TabRookSteps120(i)
        to = Mailbox(to120)
      }
      i += 1
    }
    x
  }

  /** Folds the queen slides.
    *
    * @tparam T the result type.
    * @param squ the square.
    * @param z the start value.
    * @param f the slide function.
    * @param g the square function.
    * @return a result of folded slides.
    */
  def foldQueenSlides[T](squ: Int, z: T)(f: T => T)(g: (T, Int) => (T, Boolean)) = {
    var i = 0
    var x = z
    while(i < 8) {
      x = f(x)
      var isStop = false
      var to120 = Mailbox64(squ) + TabQueenSteps120(i)
      var to = Mailbox(to120)
      while(to != -1 && !isStop) {
        val (y, isCont) = g(x, to)
        x = y
        isStop = !isCont
        to120 += TabQueenSteps120(i)
        to = Mailbox(to120)
      }
      i += 1
    }
    x
  }

  /** Folds the king squares.
    *
    * @tparam T the result type.
    * @param squ the square.
    * @param z the start value.
    * @param f the function.
    * @return a result of folded squares.
    */
  def foldKingSquares[T](squ: Int, z: T)(f: (T, Int) => T) = {
    var i = 0
    var x = z
    while(i < 8) {
      val to120 = Mailbox64(squ) + TabKingSteps120(i)
      val to = Mailbox(to120)
      if(to != -1) x = f(x, to)
      i += 1
    }
    x
  }
}
