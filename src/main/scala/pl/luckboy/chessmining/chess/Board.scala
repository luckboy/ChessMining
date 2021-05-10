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
import Squares._
import Tables._

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
  
  def hasAttack(side: Side.Value, squ: Int) = {
    val oppSide = ~side;
    {
      var i = 0
      var isAttack = false
      while(i < 2 && !isAttack) {
        val from120 = Mailbox64(squ) + TabPawnCaptureSteps120(side.id)(i)
        val from = Mailbox(from120)
        if(from != -1 && coloredPiece(from) == sideAndPieceToColoredPiece(oppSide, Piece.Pawn))
          isAttack = true
        i += 1
      }
      isAttack
    } || {
      var i = 0
      var isAttack = false
      while(i < 8 && !isAttack) {
        val from120 = Mailbox64(squ) + TabKnightSteps120(i)
        val from = Mailbox(from120)
        if(from != -1 && coloredPiece(from) == sideAndPieceToColoredPiece(oppSide, Piece.Knight))
          isAttack = true
        i += 1
      }
      isAttack
    } || {
      var i = 0
      var isAttack = false
      while(i < 4 && !isAttack) {
        var isStop = false
        var from120 = Mailbox64(squ) + TabBishopSteps120(i)
        var from = Mailbox(from120)
        while(from != -1 && !isStop && !isAttack) {
          if(coloredPiece(from) == sideAndPieceToColoredPiece(oppSide, Piece.Bishop) ||
            coloredPiece(from) == sideAndPieceToColoredPiece(oppSide, Piece.Queen))
            isAttack = true
          isStop = (color(from) != Color.Empty)
          from120 += TabBishopSteps120(i)
          from = Mailbox(from120)
        }
        i += 1
      }
      isAttack
    } || {
      var i = 0
      var isAttack = false
      while(i < 4 && !isAttack) {
        var isStop = false
        var from120 = Mailbox64(squ) + TabRookSteps120(i)
        var from = Mailbox(from120)
        while(from != -1 && !isStop && !isAttack) {
          if(coloredPiece(from) == sideAndPieceToColoredPiece(oppSide, Piece.Rook) ||
            coloredPiece(from) == sideAndPieceToColoredPiece(oppSide, Piece.Queen))
            isAttack = true
          isStop = (color(from) != Color.Empty)
          from120 += TabRookSteps120(i)
          from = Mailbox(from120)
        }
        i += 1
      }
      isAttack
    } || {
      var i = 0
      var isAttack = false
      while(i < 8 && !isAttack) {
        val from120 = Mailbox64(squ) + TabKingSteps120(i)
        val from = Mailbox(from120)
        if(from != -1 && coloredPiece(from) == sideAndPieceToColoredPiece(oppSide, Piece.King))
          isAttack = true
        i += 1
      }
      isAttack
    }
  }

  def inCheckForSide(side: Side.Value) =
    (0 until 64).find {
      coloredPiece(_) == sideAndPieceToColoredPiece(side, Piece.King)
    }.map { hasAttack(side, _) }.getOrElse(false)

  def inCheck = inCheckForSide(side)

  def generatePseudolegalMoves = {
    val oppSide = ~side
    var moves = Vector[Move]()
    for(from <- 0 until 64) {
      if(color(from) == sideToColor(side)) {
        pieceOption(from) match {
          case Some(Piece.Pawn) =>
            var i = 0
            val enPassantSquOpt = enPassantColumnOption.map {
              _ + (if(side == Side.White) A6 else A3)
            }
            while(i < 2) {
              val to120 = Mailbox64(from) + TabPawnCaptureSteps120(side.id)(i)
              val to = Mailbox(to120)
              if(to != -1 && (color(to) == sideToColor(oppSide) ||
                  enPassantSquOpt.map { to == _ }.getOrElse(false))) {
                if((from >> 3) == (if(side == Side.White) 6 else 1)) {
                  for(promotionPiece <- Array(
                      PromotionPiece.Knight,
                      PromotionPiece.Bishop,
                      PromotionPiece.Rook,
                      PromotionPiece.Queen))
                    moves :+= NormalMove(Piece.Pawn, from, to, Some(promotionPiece), true)
                } else
                  moves :+= NormalMove(Piece.Pawn, from, to, None, true)
              }
              i += 1
            }
            var to120 = Mailbox64(from) + (if(side == Side.White) 10 else -10)
            var to = Mailbox(to120)
            if(to != -1 && color(to) == Color.Empty) {
              if((from >> 3) == (if(side == Side.White) 6 else 1)) {
                for(promotionPiece <- Array(
                    PromotionPiece.Knight,
                    PromotionPiece.Bishop,
                    PromotionPiece.Rook,
                    PromotionPiece.Queen))
                  moves :+= NormalMove(Piece.Pawn, from, to, Some(promotionPiece), false)
              } else {
                moves :+= NormalMove(Piece.Pawn, from, to, None, false)
                if((from >> 3) == (if(side == Side.White) 1 else 6)) {
                  to120 += (if(side == Side.White) 10 else -10)
                  to = Mailbox(to120)
                  if(to != -1 && color(to) == Color.Empty)
                    moves :+= NormalMove(Piece.Pawn, from, to, None, false)
                }
              }
            }
          case Some(Piece.Knight) =>
            var i = 0
            while(i < 8) {
              val to120 = Mailbox64(from) + TabKnightSteps120(i)
              val to = Mailbox(to120)
              if(to != -1 && (color(to) == Color.Empty || color(to) == sideToColor(oppSide)))
                moves :+= NormalMove(Piece.Knight, from, to, None, color(to) == sideToColor(oppSide))
              i += 1
            }
          case Some(Piece.Bishop) =>
            var i = 0
            while(i < 4) {
              var isStop = false
              var to120 = Mailbox64(from) + TabBishopSteps120(i)
              var to = Mailbox(to120)
              while(to != -1 && !isStop) {
                if(to != -1 && (color(to) == Color.Empty || color(to) == sideToColor(oppSide)))
                  moves :+= NormalMove(Piece.Bishop, from, to, None, color(to) == sideToColor(oppSide))
                isStop = (color(to) != Color.Empty)
                to120 += TabBishopSteps120(i)
                to = Mailbox(to120)
              }
              i += 1
            }
          case Some(Piece.Rook) =>
            var i = 0
            while(i < 4) {
              var isStop = false
              var to120 = Mailbox64(from) + TabRookSteps120(i)
              var to = Mailbox(to120)
              while(to != -1 && !isStop) {
                if(to != -1 && (color(to) == Color.Empty || color(to) == sideToColor(oppSide)))
                  moves :+= NormalMove(Piece.Rook, from, to, None, color(to) == sideToColor(oppSide))
                isStop = (color(to) != Color.Empty)
                to120 += TabRookSteps120(i)
                to = Mailbox(to120)
              }
              i += 1
            }
          case Some(Piece.Queen) =>
            var i = 0
            while(i < 8) {
              var isStop = false
              var to120 = Mailbox64(from) + TabQueenSteps120(i)
              var to = Mailbox(to120)
              while(to != -1 && !isStop) {
                if(to != -1 && (color(to) == Color.Empty || color(to) == sideToColor(oppSide)))
                  moves :+= NormalMove(Piece.Queen, from, to, None, color(to) == sideToColor(oppSide))
                isStop = (color(to) != Color.Empty)
                to120 += TabQueenSteps120(i)
                to = Mailbox(to120)
              }
              i += 1
            }
          case Some(Piece.King) =>
            var i = 0
            while(i < 8) {
              val to120 = Mailbox64(from) + TabKingSteps120(i)
              val to = Mailbox(to120)
              if(to != -1 && (color(to) == Color.Empty || color(to) == sideToColor(oppSide)))
                moves :+= NormalMove(Piece.King, from, to, None, color(to) == sideToColor(oppSide))
              i += 1
            }
          case _ =>
            ()
        }
      }
    }
    val castlingRow = if(side == Side.White) 0 else 7
    if((sideCastlings(side) & SideCastlings.Short) != SideCastlings.None &&
      color(F1 | (castlingRow << 3)) == Color.Empty &&
      color(G1 | (castlingRow << 3)) == Color.Empty)
      moves :+= ShortCastling
    if((sideCastlings(side) & SideCastlings.Long) != SideCastlings.None &&
      color(B1 | (castlingRow << 3)) == Color.Empty &&
      color(C1 | (castlingRow << 3)) == Color.Empty &&
      color(D1 | (castlingRow << 3)) == Color.Empty)
      moves :+= LongCastling
    moves
  }

  def unsafelyMakeMove(move: Move) = {
    val oppSide = ~side
    val newBoardOpt = move match {
      case NormalMove(piece, from, to, promotionPieceOpt, isCapture) =>
        val newPieces = Array.fill(64)(ColoredPiece.Empty)
        Array.copy(pieces, 0, newPieces, 0, 64)
        newPieces(from) = ColoredPiece.Empty
        newPieces(to) = sideAndPieceToColoredPiece(side, promotionPieceOpt.map(promotionPieceToPiece).getOrElse(piece))
        val enPassantSquOpt = enPassantColumnOption.map {
          _ + (if(side == Side.White) A6 else A3)
        }
        for(enPassantSqu <- enPassantSquOpt) {
          if(piece == Piece.Pawn && to == enPassantSqu)
            newPieces(to + (if(side == Side.White) -8 else 8)) = ColoredPiece.Empty
        }
        val newCastlings = Array.fill(2)(SideCastlings.None)
        Array.copy(castlings, 0, newCastlings, 0, 2)
        piece match {
          case Piece.Rook =>
            if(from == (if(side == Side.White) H1 else H8))
              newCastlings(side.id) = newCastlings(side.id) & ~SideCastlings.Short
            if(from == (if(side == Side.White) A1 else A8))
              newCastlings(side.id) = newCastlings(side.id) & ~SideCastlings.Long
          case Piece.King =>
            newCastlings(side.id) = SideCastlings.None
          case _ =>
            ()
        }
        if(pieceOption(to).map { _ == Piece.Rook }.getOrElse(false) &&
          to == (if(side == Side.White) H8 else H1))
          newCastlings(oppSide.id) = newCastlings(side.id) & ~SideCastlings.Short
        if(pieceOption(to).map { _ == Piece.Rook }.getOrElse(false) &&
          to == (if(side == Side.White) A8 else A1))
          newCastlings(oppSide.id) = newCastlings(side.id) & ~SideCastlings.Long
        var newEnPassantColumnOption = None: Option[Int]
        val pawnSrcRow2 = if(side == Side.White) 1 else 6
        val pawnDstRow2 = if(side == Side.White) 3 else 4
        if(piece == Piece.Pawn && (from >> 3) == pawnSrcRow2 && (to >> 3) == pawnDstRow2) {
          val enPassantSqu = from + (if(side == Side.White) 8 else -8)
          var i = 0
          var isOppPawn = false
          while(i < 2 && !isOppPawn) {
            val oppPawnSrc120 = Mailbox64(enPassantSqu) + TabPawnCaptureSteps120(side.id)(i)
            val oppPawnSrc = Mailbox(oppPawnSrc120)
            if(oppPawnSrc != -1 && coloredPiece(oppPawnSrc) == sideAndPieceToColoredPiece(oppSide, Piece.Pawn))
              isOppPawn = true
            i += 1
          }
          if(isOppPawn) newEnPassantColumnOption = Some(enPassantSqu & 7)
        }
        Some(Board(
            newPieces,
            ~side,
            newCastlings,
            newEnPassantColumnOption,
            if(!isCapture && piece != Piece.Pawn) halfmoveClock + 1 else 0,
            if(side == Side.Black) fullmoveNumber + 1 else fullmoveNumber))
      case ShortCastling =>
        val kingSrc = if(side == Side.White) E1 else E8
        val kingDst = if(side == Side.White) G1 else G8
        val rookSrc = if(side == Side.White) H1 else H8
        val rookDst = if(side == Side.White) F1 else F8
        if(!inCheck && !hasAttack(side, rookDst)) {
          val newPieces = Array.fill(64)(ColoredPiece.Empty)
          Array.copy(pieces, 0, newPieces, 0, 64)
          newPieces(kingSrc) = ColoredPiece.Empty
          newPieces(kingDst) = sideAndPieceToColoredPiece(side, Piece.King)
          newPieces(rookSrc) = ColoredPiece.Empty
          newPieces(rookDst) = sideAndPieceToColoredPiece(side, Piece.Rook)
          val newCastlings = Array.fill(2)(SideCastlings.None)
          Array.copy(castlings, 0, newCastlings, 0, 2)
          newCastlings(side.id) = SideCastlings.None
          Some(Board(
              newPieces,
              ~side,
              newCastlings,
              None,
              halfmoveClock + 1,
              if(side == Side.Black) fullmoveNumber + 1 else fullmoveNumber))
        } else
          None
      case LongCastling =>
        val kingSrc = if(side == Side.White) E1 else E8
        val kingDst = if(side == Side.White) C1 else C8
        val rookSrc = if(side == Side.White) A1 else A8
        val rookDst = if(side == Side.White) D1 else D8
        if(!inCheck && !hasAttack(side, rookDst)) {
          val newPieces = Array.fill(64)(ColoredPiece.Empty)
          Array.copy(pieces, 0, newPieces, 0, 64)
          newPieces(kingSrc) = ColoredPiece.Empty
          newPieces(kingDst) = sideAndPieceToColoredPiece(side, Piece.King)
          newPieces(rookSrc) = ColoredPiece.Empty
          newPieces(rookDst) = sideAndPieceToColoredPiece(side, Piece.Rook)
          val newCastlings = Array.fill(2)(SideCastlings.None)
          Array.copy(castlings, 0, newCastlings, 0, 2)
          newCastlings(side.id) = SideCastlings.None
          Some(Board(
              newPieces,
              ~side,
              newCastlings,
              None,
              halfmoveClock + 1,
              if(side == Side.Black) fullmoveNumber + 1 else fullmoveNumber))
        } else
          None
    }
    newBoardOpt.flatMap {
      (newBoard: Board) => if(!newBoard.inCheckForSide(side)) Some(newBoard) else None
    }
  }

  def makeMove(move: Move) =
    generatePseudolegalMoves.find { _ == move }.flatMap(unsafelyMakeMove)

  def generateLegalMoves =
    generatePseudolegalMoves.filter { unsafelyMakeMove(_) != None }

  def inCheckmate = inCheck && generateLegalMoves.isEmpty

  def inStalemate = !inCheck && generateLegalMoves.isEmpty

  override def toString() = {
    val sb = new StringBuilder()
    var row = 7
    while(row >= 0) {
      var col = 0
      while(col < 8) {
        if(color(col + (row << 3)) == Color.Empty) {
          var count = 0
          while(count < 8 && col < 8 && color(col + (row << 3)) == Color.Empty) {
            col += 1
            count += 1
          }
          sb ++= count.toString()
        } else {
          sb += coloredPieceToChar(coloredPiece(col + (row << 3)))
          col += 1
        }
      }
      if(row - 1 >= 0) sb += '/'
      row -= 1
    }
    sb += ' '
    sb += sideToChar(side)
    sb += ' '
    if(sideCastlings(Side.White) != SideCastlings.None || sideCastlings(Side.Black) != SideCastlings.None) {
      if((sideCastlings(Side.White) & SideCastlings.Short) != SideCastlings.None) sb += 'K'
      if((sideCastlings(Side.White) & SideCastlings.Long) != SideCastlings.None) sb += 'Q'
      if((sideCastlings(Side.Black) & SideCastlings.Short) != SideCastlings.None) sb += 'k'
      if((sideCastlings(Side.Black) & SideCastlings.Long) != SideCastlings.None) sb += 'q'
    } else
      sb += '-'
    sb += ' '
    val enPassantSquOpt = enPassantColumnOption.map { _ + (if(side == Side.White) A6 else A3) }
    enPassantSquOpt match {
      case Some(enPassantSqu) => sb ++= squareToString(enPassantSqu)
      case None               => sb += '-'
    }
    sb += ' '
    sb ++= halfmoveClock.toString()
    sb += ' '
    sb ++= fullmoveNumber.toString()
    sb.toString()
  }
}

object Board
{
  val Initial = Board()
      
  def apply(): Board = Board(
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

  def apply(s: String): Board =
    parseBoard(s) match {
      case Some(board) => board
      case None        => throw new ChessException("Invalid FEN")
    }

  def parseBoard(s: String) = {
    val ss = s.split("[ \t]+")
    var isOk = true
    if(ss.length == 6) {
      val pieceStrs = ss(0).split("/")
      val pieces = Array.fill(64)(ColoredPiece.Empty)
      if(pieceStrs.length == 8) {
        var row = 7
        var i = 0
        isOk = true
        while(row >= 0 && isOk) {
          val pieceStr = pieceStrs(i)
          var col = 0
          var j = 0
          while(col < 8 && isOk) {
            charToColoredPieceOption(pieceStr(j)) match {
              case Some(coloredPiece) =>
                pieces(col + (row << 3)) = coloredPiece
                col += 1
              case None               =>
                if(pieceStr(j) >= '1' && pieceStr(j) <= '8') {
                  val count = pieceStr(j).toInt - '0'.toInt
                  if(count <= 8 - col) {
                    for(k <- 0 until count) {
                      pieces(col + (row << 3)) = ColoredPiece.Empty
                      col += 1
                    }
                  } else
                    isOk = false
                } else
                  isOk = false
            }
            j += 1
          }
          if(j != pieceStr.length) isOk = false
          row -= 1
          i += 1
        }
        if(isOk) {
          if(pieces.filter { _ == ColoredPiece.WhiteKing }.length != 1) isOk = false
          if(pieces.filter { _ == ColoredPiece.BlackKing }.length != 1) isOk = false
        }
        if(isOk) {
          val sideStr = ss(1)
          var side = Side.White
          if(sideStr.length == 1) {
            charToSideOption(sideStr(0)) match {
              case Some(side2) => side = side2
              case _           => isOk = false
            }
          } else
            isOk = false
          if(isOk) {
            val castlingStr = ss(2)
            val castlings = Array.fill(2)(SideCastlings.None)
            if(castlingStr != "-") {
              var i = 0
              while(i < castlingStr.length && isOk) {
                castlingStr(i) match {
                  case 'K' => castlings(Side.White.id) = castlings(Side.White.id) | SideCastlings.Short
                  case 'Q' => castlings(Side.White.id) = castlings(Side.White.id) | SideCastlings.Long
                  case 'k' => castlings(Side.Black.id) = castlings(Side.Black.id) | SideCastlings.Short
                  case 'q' => castlings(Side.Black.id) = castlings(Side.Black.id) | SideCastlings.Long
                  case _   => isOk = false
                }
                i += 1
              }
            }
            if(isOk) {
              if((castlings(Side.White.id) & SideCastlings.Short) != SideCastlings.None) {
                if(pieces(E1) != ColoredPiece.WhiteKing)
                  isOk = false
                else if(pieces(H1) != ColoredPiece.WhiteRook)
                  isOk = false
              }
              if((castlings(Side.White.id) & SideCastlings.Long) != SideCastlings.None) {
                if(pieces(E1) != ColoredPiece.WhiteKing)
                  isOk = false
                else if(pieces(A1) != ColoredPiece.WhiteRook)
                  isOk = false
              }
              if((castlings(Side.Black.id) & SideCastlings.Short) != SideCastlings.None) {
                if(pieces(E8) != ColoredPiece.BlackKing)
                  isOk = false
                else if(pieces(H8) != ColoredPiece.BlackRook)
                  isOk = false
              }
              if((castlings(Side.Black.id) & SideCastlings.Long) != SideCastlings.None) {
                if(pieces(E8) != ColoredPiece.BlackKing)
                  isOk = false
                else if(pieces(A8) != ColoredPiece.BlackRook)
                  isOk = false
              }
            }
            if(isOk) {
              val enPassantSquStr = ss(3)
              var enPassantSquOpt = None: Option[Int]
              var enPassantColumnOption = None: Option[Int]
              if(enPassantSquStr != "-") {
                stringToSquareOption(enPassantSquStr) match {
                  case Some(enPassantSqu) => enPassantSquOpt = Some(enPassantSqu)
                  case None               => isOk = false
                }
              }
              if(isOk) {
                enPassantSquOpt match {
                  case Some(enPassantSqu) =>
                    var isPawn = false
                    if((enPassantSqu >> 3) == (if(side == Side.White) 5 else 2)) {
                      val capSqu = enPassantSqu + (if(side == Side.White) -8 else 8)
                      if(pieces(capSqu) != sideAndPieceToColoredPiece(~side, Piece.Pawn)) {
                        isOk = false
                      } else {
                        var i = 0
                        while(i < 2 && !isPawn) {
                          val from120 = Mailbox64(enPassantSqu) + TabPawnCaptureSteps120((~side).id)(i)
                          val from = Mailbox(from120)
                          if(from != -1 && pieces(from) == sideAndPieceToColoredPiece(side, Piece.Pawn))
                            isPawn = true
                          i += 1
                        }
                      }
                    } else
                      isOk = false
                    enPassantColumnOption = if(isPawn) Some(enPassantSqu & 7) else None
                  case None               =>
                    ()
                }
              }
              if(isOk) {
                val halfmoveClockStr = ss(4)
                var halfmoveClock = 0
                try {
                  halfmoveClock = Integer.parseInt(halfmoveClockStr)
                } catch {
                  case e: NumberFormatException => isOk = false
                }
                if(isOk) {
                  val fullmoveNumberStr = ss(5)
                  var fullmoveNumber = 1
                  try {
                    fullmoveNumber = Integer.parseInt(fullmoveNumberStr)
                  } catch {
                    case e: NumberFormatException => isOk = false
                  }
                  if(isOk) {
                    val board = Board(
                        pieces,
                        side,
                        castlings,
                        enPassantColumnOption,
                        halfmoveClock,
                        fullmoveNumber)
                    if(!board.inCheckForSide(~side))
                      Some(board)
                    else 
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
        } else
          None
      } else
        None
    } else
      None
  }
}
