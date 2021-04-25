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

sealed abstract class SANMove
{
  def checkOption: Option[Check.Value]

  def toMoveOption(board: Board) = Move.sanMoveToMoveOption(this, board)
  
  override def toString() = {
    val sb = new StringBuilder()
    var checkOpt = None: Option[Check.Value]
    this match {
      case SANNormalMove(piece, srcColOpt, srcRowOpt, dstSqu, promotionPieceOpt, isCapture, _) =>
        if(piece != Piece.Pawn) sb += pieceToChar(piece)
        for(col <- srcColOpt) {
          sb += columnToChar(col)
        }
        for(row <- srcRowOpt) {
          sb += rowToChar(row)
        }
        if(isCapture) sb += 'x'
        sb ++= squareToString(dstSqu)
        for(promotionPiece <- promotionPieceOpt) {
          sb += '='
          sb += promotionPieceToChar(promotionPiece)
        }
      case SANShortCastling(_) =>
        sb ++= "O-O"
      case SANLongCastling(_) =>
        sb ++= "O-O-O"
    }
    for(check <- checkOption) {
      check match {
        case Check.Check     => sb += '+'
        case Check.Checkmate => sb += '#'
      }
    }
    sb.toString()
  }
}

object SANMove
{
  def apply(s: String) =
    parseSANMove(s) match {
      case Some(sanMove) => sanMove
      case None          => throw new ChessException("illegal move")
    }
  
  private def parseCheckOptionAndSuffix(s: String) =
    if(s.length >= 1) {
      var i = 0
      var checkOption = None: Option[Check.Value]
      s(i) match {
        case '+' =>
          checkOption = Some(Check.Check)
          i += 1
        case '#' =>
          checkOption = Some(Check.Checkmate)
          i += 1
        case _   =>
          ()
      }
      while(i < s.length && (s(i) == '!' || s(i) == '?')) {
        i += 1
      }
      if(i >= s.length)
        Some(checkOption)
      else
        None
    } else
      Some(None)

  def parseSANMove(s: String) = {
    if(s.length >= 5 && s.substring(0, 5) == "O-O-O") {
      for(checkOption <- parseCheckOptionAndSuffix(s.substring(5)))
        yield SANLongCastling(checkOption)
    } else if(s.length >= 3 && s.substring(0, 3) == "O-O") {
      for(checkOption <- parseCheckOptionAndSuffix(s.substring(3)))
        yield SANShortCastling(checkOption)
    } else {
      var i = 0
      if(i < s.length) {
        var piece = Piece.Pawn
        for(piece2 <- charToPieceOption(s(i))) {
          piece = piece2
          i += 1
        }
        if(i < s.length) {
          var fromColumnOption = None: Option[Int]
          var fromRowOption = None: Option[Int]
          val savedIdx = i
          for(col <- charToColumnOption(s(i))) {
            fromColumnOption = Some(col)
            i += 1
          }
          for(row <- charToRowOption(s(i))) {
            fromRowOption = Some(row)
            i += 1
          }
          if(i == s.length || (!(s(i) >= 'a' && s(i) <= 'h') && s(i) != 'x')) {
            fromColumnOption = None
            fromRowOption = None
            i = savedIdx
          }
          if(i < s.length) {
            var isCapture = false
            if(s(i) == 'x') {
              isCapture = true
              i += 1
            }
            if(i + 1 < s.length) {
              var to = 0
              var isDstSqu = false
              for(squ <- stringToSquareOption(s.substring(i, i + 2))) {
                to = squ
                isDstSqu = true
                i += 2
              }
              if(isDstSqu) {
                var mustPromotion = false
                if(i < s.length && s(i) == '=') {
                  mustPromotion = true
                  i += 1
                }
                var promotionPieceOption = None: Option[PromotionPiece.Value]
                if(i < s.length) {
                  for(promotionPiece <- charToPromotionPieceOption(s(i))) {
                    promotionPieceOption = Some(promotionPiece)
                    i += 1
                  }
                }
                if(!mustPromotion || promotionPieceOption != None) {
                  for(checkOption <- parseCheckOptionAndSuffix(s.substring(i)))
                    yield SANNormalMove(
                      piece,
                      fromColumnOption,
                      fromRowOption,
                      to,
                      promotionPieceOption,
                      isCapture,
                      checkOption)
                } else
                  None
              } else
                None
            } else
              None
          } else
            None
        } else
          None
      } else
        None
    }
  }
}

case class SANNormalMove(
  piece: Piece.Value,
  fromColumnOption: Option[Int],
  fromRowOption: Option[Int],
  to: Int,
  promotionPieceOption: Option[PromotionPiece.Value],
  isCapture: Boolean,
  checkOption: Option[Check.Value]) extends SANMove
case class SANShortCastling(checkOption: Option[Check.Value]) extends SANMove
case class SANLongCastling(checkOption: Option[Check.Value]) extends SANMove
