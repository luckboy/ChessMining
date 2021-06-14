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
package pl.luckboy
import pl.luckboy.chessmining.chess._
import pl.luckboy.chessmining.ui._

package object chessmining
{
  implicit val defaultGameReaderFactory = PGNReader
  implicit val defaultGameWriterFactory = PGNWriter
  implicit val defaultFileProgressBarFactory = ConsoleFileProgressBar

  implicit def defaultWinMinerFactory[T] = new BinaryValueMinerFactory[WinMiner[T], BinaryMiner[(Game, T), _], WinMiner[T]] {
    override def apply(miner: WinMiner[T], firstMinerOpt: Option[BinaryMiner[(Game, T), _]], secondMinerOpt: Option[BinaryMiner[(Game, T), _]]) =
      miner.copy(firstMinerOption = firstMinerOpt, secondMinerOption = secondMinerOpt)
  }

  implicit def defaultWinBoardMinerFactory[T] = new BinaryValueMinerFactory[WinBoardMiner[T], BinaryBoardMiner[(Game, T), _], WinBoardMiner[T]] {
    override def apply(miner: WinBoardMiner[T], firstMinerOpt: Option[BinaryBoardMiner[(Game, T), _]], secondMinerOpt: Option[BinaryBoardMiner[(Game, T), _]]) =
      miner.copy(firstMinerOption = firstMinerOpt, secondMinerOption = secondMinerOpt)
  }

  implicit def defaultLossMinerFactory[T] = new BinaryValueMinerFactory[LossMiner[T], BinaryMiner[(Game, T), _], LossMiner[T]] {
    override def apply(miner: LossMiner[T], firstMinerOpt: Option[BinaryMiner[(Game, T), _]], secondMinerOpt: Option[BinaryMiner[(Game, T), _]]) =
      miner.copy(firstMinerOption = firstMinerOpt, secondMinerOption = secondMinerOpt)
  }

  implicit def defaultLossBoardMinerFactory[T] = new BinaryValueMinerFactory[LossBoardMiner[T], BinaryBoardMiner[(Game, T), _], LossBoardMiner[T]] {
    override def apply(miner: LossBoardMiner[T], firstMinerOpt: Option[BinaryBoardMiner[(Game, T), _]], secondMinerOpt: Option[BinaryBoardMiner[(Game, T), _]]) =
      miner.copy(firstMinerOption = firstMinerOpt, secondMinerOption = secondMinerOpt)
  }

  implicit def defaultDrawMinerFactory[T] = new BinaryValueMinerFactory[DrawMiner[T], BinaryMiner[(Game, T), _], DrawMiner[T]] {
    override def apply(miner: DrawMiner[T], firstMinerOpt: Option[BinaryMiner[(Game, T), _]], secondMinerOpt: Option[BinaryMiner[(Game, T), _]]) =
      miner.copy(firstMinerOption = firstMinerOpt, secondMinerOption = secondMinerOpt)
  }

  implicit def defaultDrawBoardMinerFactory[T] = new BinaryValueMinerFactory[DrawBoardMiner[T], BinaryBoardMiner[(Game, T), _], DrawBoardMiner[T]] {
    override def apply(miner: DrawBoardMiner[T], firstMinerOpt: Option[BinaryBoardMiner[(Game, T), _]], secondMinerOpt: Option[BinaryBoardMiner[(Game, T), _]]) =
      miner.copy(firstMinerOption = firstMinerOpt, secondMinerOption = secondMinerOpt)
  }

  implicit def defaultCountMinerFactory[T] = new BinaryValueMinerFactory[CountMiner[T], BinaryMiner[(Game, T), _], CountMiner[T]] {
    override def apply(miner: CountMiner[T], firstMinerOpt: Option[BinaryMiner[(Game, T), _]], secondMinerOpt: Option[BinaryMiner[(Game, T), _]]) =
      miner.copy(firstMinerOption = firstMinerOpt, secondMinerOption = secondMinerOpt)
  }

  implicit def defaultCountBoardMinerFactory[T] = new BinaryValueMinerFactory[CountBoardMiner[T], BinaryBoardMiner[(Game, T), _], CountBoardMiner[T]] {
    override def apply(miner: CountBoardMiner[T], firstMinerOpt: Option[BinaryBoardMiner[(Game, T), _]], secondMinerOpt: Option[BinaryBoardMiner[(Game, T), _]]) =
      miner.copy(firstMinerOption = firstMinerOpt, secondMinerOption = secondMinerOpt)
  }

  private def sideToName(side: Side.Value) = if(side == Side.White) "white" else "black"

  private def pieceToName(piece: Piece.Value) =
    piece match {
      case Piece.Pawn   => "pawn"
      case Piece.Knight => "knight"
      case Piece.Bishop => "bishop"
      case Piece.Rook   => "rook"
      case Piece.Queen  => "queen"
      case Piece.King   => "king"
    }
  
  def side(side: Side.Value) =
    NamedFunction2(sideToName(side), {
      (tuple: (Game, Any), side2: Side.Value) => side == side2
    })

  def side3(side: Side.Value) =
    NamedFunction3(sideToName(side), {
      (tuple: (Game, Any), side2: Side.Value, squ: Int) => side == side2
    })

  val white = side(Side.White)
  val black = side(Side.Black)
  val white3 = side3(Side.White)
  val black3 = side3(Side.Black)

  val anyPiece = NamedFunction3("piece", {
      (tuple: (Game, Board), side: Side.Value, squ: Int) =>
        tuple match {
          case (_, board) => board.color(squ) == sideToColor(side)
        }
    })
  
  def piece(piece: Piece.Value) =
    NamedFunction3(pieceToName(piece), {
      (tuple: (Game, Board), side: Side.Value, squ: Int) =>
        tuple match {
          case (_, board) => board.coloredPiece(squ) == sideAndPieceToColoredPiece(side, piece)
        }
    })
    
  val empty = NamedFunction3("empty", {
      (tuple: (Game, Board), side: Side.Value, squ: Int) =>
        tuple match {
          case (_, board) => board.color(squ) == Color.Empty
        }
    })

  val anyPiece2 = NamedFunction2("piece", {
      (tuple: (Game, Board), squ: Int) =>
        tuple match {
          case (_, board) => board.color(squ) != Color.Empty
        }
    })

  def piece2(piece: Piece.Value) =
    NamedFunction2(pieceToName(piece), {
      (tuple: (Game, Board), squ: Int) =>
        tuple match {
          case (_, board) => board.pieceOption(squ).map { _ == piece }.getOrElse(false)
        }
    })

  val empty2 = NamedFunction2("empty", {
      (tuple: (Game, Board), squ: Int) =>
        tuple match {
          case (_, board) => board.color(squ) == Color.Empty
        }
    })

  def anySidePiece(side: Side.Value) =
    NamedFunction2(sideToName(side) + " piece", {
      (tuple: (Game, Board), squ: Int) =>
        tuple match {
          case (_, board) => board.color(squ) == sideToColor(side)
        }
    })

  def sidePiece(side: Side.Value, piece: Piece.Value) = 
    NamedFunction2(sideToName(side) + " " + pieceToName(piece), {
      (tuple: (Game, Board), squ: Int) =>
        tuple match {
          case (_, board) => board.coloredPiece(squ) == sideAndPieceToColoredPiece(side, piece)
        }
    })

  val pawn = piece(Piece.Pawn)
  val knight = piece(Piece.Knight)
  val bishop = piece(Piece.Bishop)
  val rook = piece(Piece.Rook)
  val queen = piece(Piece.Queen)
  val king = piece(Piece.King)
  val pawn2 = piece2(Piece.Pawn)
  val knight2 = piece2(Piece.Knight)
  val bishop2 = piece2(Piece.Bishop)
  val rook2 = piece2(Piece.Rook)
  val queen2 = piece2(Piece.Queen)
  val king2 = piece2(Piece.King)
  val anyWhitePiece = anySidePiece(Side.White)
  val anyBlackPiece = anySidePiece(Side.Black)
  val whitePawn = sidePiece(Side.White, Piece.Pawn)
  val whiteKnight = sidePiece(Side.White, Piece.Knight)
  val whiteBishop = sidePiece(Side.White, Piece.Bishop)
  val whiteRook = sidePiece(Side.White, Piece.Rook)
  val whiteQueen = sidePiece(Side.White, Piece.Queen)
  val whiteKing = sidePiece(Side.White, Piece.King)
  val blackPawn = sidePiece(Side.Black, Piece.Pawn)
  val blackKnight = sidePiece(Side.Black, Piece.Knight)
  val blackBishop = sidePiece(Side.Black, Piece.Bishop)
  val blackRook = sidePiece(Side.Black, Piece.Rook)
  val blackQueen = sidePiece(Side.Black, Piece.Queen)
  val blackKing = sidePiece(Side.Black, Piece.King)
}
