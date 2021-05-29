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
package pl.luckboy.chessmining.chess

sealed abstract class Move
{
  def isCheck(board: Board) = board.makeMove(this).map { _.inCheck }.getOrElse(false)

  def isCheckmate(board: Board) = board.makeMove(this).map { _.inCheckmate }.getOrElse(false)

  def checkOption(board: Board) =
    if(isCheckmate(board))
      Some(Check.Checkmate)
    else if(isCheck(board))
      Some(Check.Check)
    else
      None
  
  def toSANMove(board: Board) =
    this match {
      case normalMove @ NormalMove(_, _, _, _, _) =>
        var (isFound, isAmbiguous, mustBeSrcCol, mustBeSrcRow) = board.generateLegalMoves.foldLeft((false, false, false, false)) {
          case (tuple @ (isFound: Boolean, isAmbiguous: Boolean, mustBeSrcCol: Boolean, mustBeSrcRow: Boolean), move2: Move) =>
            move2 match {
              case normalMove2 @ NormalMove(_, _, _, _, _) =>
                if(normalMove2 == normalMove) {
                  (true, isAmbiguous, mustBeSrcCol, mustBeSrcRow)
                } else if(normalMove2.piece == normalMove.piece &&
                  normalMove2.to == normalMove.to &&
                  normalMove2.promotionPieceOption == normalMove.promotionPieceOption &&
                  normalMove2.isCapture == normalMove.isCapture) {
                  val mustBeSrcRow2 = (mustBeSrcRow || ((normalMove2.from & 7) == (normalMove.from & 7)))
                  val mustBeSrcCol2 = (mustBeSrcCol || ((normalMove2.from >> 3) == (normalMove.from >> 3)))
                  (isFound, true, mustBeSrcCol2, mustBeSrcRow2) 
                } else
                  tuple
              case _ =>
                tuple
            }
        }
        if(isAmbiguous && !mustBeSrcCol && !mustBeSrcRow) mustBeSrcCol = true
        if(!mustBeSrcCol && !mustBeSrcRow && normalMove.piece == Piece.Pawn && normalMove.isCapture)
          mustBeSrcCol = true
        val fromColumnOption = if(mustBeSrcCol) Some(normalMove.from & 7) else None
        val fromRowOption = if(mustBeSrcRow) Some(normalMove.from >> 3) else None
        val checkOption = if(isFound) this.checkOption(board) else None
        SANNormalMove(
          normalMove.piece,
          fromColumnOption,
          fromRowOption,
          normalMove.to,
          normalMove.promotionPieceOption,
          normalMove.isCapture,
          checkOption)
      case ShortCastling =>
        val isFound = board.generateLegalMoves.contains(this)
        val checkOption = if(isFound) this.checkOption(board) else None
        SANShortCastling(checkOption)
      case LongCastling =>
        val isFound = board.generateLegalMoves.contains(this)
        val checkOption = if(isFound) this.checkOption(board) else None
        SANLongCastling(checkOption)
    }
  
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
  
  def toSANString(board: Board) = toSANMove(board).toString
}

object Move
{
  def apply(s: String, board: Board) =
    parseSANMove(s, board) match {
      case Some(move) => move
      case None       => throw new ChessException("Illegal move")
    }
  
  def sanMoveToMoveOption(sanMove: SANMove, board: Board) =
    board.generateLegalMoves.filter {
      (move: Move) =>
        (sanMove, move) match {
          case (sanNormalMove @ SANNormalMove(_, _, _, _, _, _, _), normalMove @ NormalMove(_, _, _, _, _)) =>
            sanNormalMove.piece == normalMove.piece &&
            sanNormalMove.fromColumnOption.map { _ == (normalMove.from & 7) }.getOrElse(true) &&
            sanNormalMove.fromRowOption.map { _ == (normalMove.from >> 3) }.getOrElse(true) &&
            sanNormalMove.to == normalMove.to &&
            sanNormalMove.promotionPieceOption == normalMove.promotionPieceOption &&
            (sanNormalMove.isCapture == normalMove.isCapture || (!sanNormalMove.isCapture && normalMove.isCapture))
          case (SANShortCastling(_), ShortCastling) =>
            true
          case (SANLongCastling(_), LongCastling) =>
            true
          case _ =>
            false
        }
    } match {
      case Vector(move) =>
        sanMove.checkOption match {
          case Some(Check.Check) =>
            if(move.isCheck(board))
              Some(move)
            else
              None
          case Some(Check.Checkmate) =>
            if(move.isCheckmate(board))
              Some(move)
            else
              None
          case _ =>
            Some(move)
        }
      case _ =>
        None
    }

  def parseSANMove(s: String, board: Board) =
    SANMove.parseSANMove(s).flatMap { sanMoveToMoveOption(_, board) }
}

case class NormalMove(piece: Piece.Value, from: Int, to: Int, promotionPieceOption: Option[PromotionPiece.Value], isCapture: Boolean) extends Move
case object ShortCastling extends Move
case object LongCastling extends Move
