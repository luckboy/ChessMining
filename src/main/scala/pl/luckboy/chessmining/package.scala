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
import pl.luckboy.chessmining.eval._
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

  type BoardChart = chart.BoardChart
  
  val BoardChart = chart.BoardChart
  
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

  val greaterMobility = NamedFunction2("> mobility", {
      (tuple: (Game, Board), side: Side.Value) =>
        tuple match {
          case (_, board) => Mobility.mobility(board, side) > Mobility.mobility(board, ~side)
        }
    })

  val equalMobility = NamedFunction2("= mobility", {
      (tuple: (Game, Board), side: Side.Value) =>
        tuple match {
          case (_, board) => Mobility.mobility(board, side) == Mobility.mobility(board, ~side)
        }
    })

  val lessMobility = NamedFunction2("< mobility", {
      (tuple: (Game, Board), side: Side.Value) =>
        tuple match {
          case (_, board) => Mobility.mobility(board, side) < Mobility.mobility(board, ~side)
        }
    })

  def greaterPieceMobility(piece: Piece.Value) = {
    val fun = piece match {
      case Piece.Pawn =>
        {
          (tuple: (Game, Board), side: Side.Value) =>
            tuple match {
              case (_, board) => Mobility.pawnMobility(board, side) > Mobility.pawnMobility(board, ~side)
            }
        }        
      case Piece.Knight =>
        {
          (tuple: (Game, Board), side: Side.Value) =>
            tuple match {
              case (_, board) => Mobility.knightMobility(board, side) > Mobility.knightMobility(board, ~side)
            }
        }
      case Piece.Bishop =>
        {
          (tuple: (Game, Board), side: Side.Value) =>
            tuple match {
              case (_, board) => Mobility.bishopMobility(board, side) > Mobility.bishopMobility(board, ~side)
            }
        }
      case Piece.Rook =>
        {
          (tuple: (Game, Board), side: Side.Value) =>
            tuple match {
              case (_, board) => Mobility.rookMobility(board, side) > Mobility.rookMobility(board, ~side)
            }
        }
      case Piece.Queen =>
        {
          (tuple: (Game, Board), side: Side.Value) =>
            tuple match {
              case (_, board) => Mobility.queenMobility(board, side) > Mobility.queenMobility(board, ~side)
            }
        }
      case Piece.King =>
        {
          (tuple: (Game, Board), side: Side.Value) =>
            tuple match {
              case (_, board) => Mobility.kingMobility(board, side) > Mobility.kingMobility(board, ~side)
            }
        }
    }
    NamedFunction2("> " + pieceToName(piece) + " mobility", fun)
  }

  def equalPieceMobility(piece: Piece.Value) = {
    val fun = piece match {
      case Piece.Pawn =>
        {
          (tuple: (Game, Board), side: Side.Value) =>
            tuple match {
              case (_, board) => Mobility.pawnMobility(board, side) == Mobility.pawnMobility(board, ~side)
            }
        }        
      case Piece.Knight =>
        {
          (tuple: (Game, Board), side: Side.Value) =>
            tuple match {
              case (_, board) => Mobility.knightMobility(board, side) == Mobility.knightMobility(board, ~side)
            }
        }
      case Piece.Bishop =>
        {
          (tuple: (Game, Board), side: Side.Value) =>
            tuple match {
              case (_, board) => Mobility.bishopMobility(board, side) == Mobility.bishopMobility(board, ~side)
            }
        }
      case Piece.Rook =>
        {
          (tuple: (Game, Board), side: Side.Value) =>
            tuple match {
              case (_, board) => Mobility.rookMobility(board, side) == Mobility.rookMobility(board, ~side)
            }
        }
      case Piece.Queen =>
        {
          (tuple: (Game, Board), side: Side.Value) =>
            tuple match {
              case (_, board) => Mobility.queenMobility(board, side) == Mobility.queenMobility(board, ~side)
            }
        }
      case Piece.King =>
        {
          (tuple: (Game, Board), side: Side.Value) =>
            tuple match {
              case (_, board) => Mobility.kingMobility(board, side) == Mobility.kingMobility(board, ~side)
            }
        }
    }
    NamedFunction2("= " + pieceToName(piece) + " mobility", fun)
  }

  def lessPieceMobility(piece: Piece.Value) = {
    val fun = piece match {
      case Piece.Pawn =>
        {
          (tuple: (Game, Board), side: Side.Value) =>
            tuple match {
              case (_, board) => Mobility.pawnMobility(board, side) < Mobility.pawnMobility(board, ~side)
            }
        }        
      case Piece.Knight =>
        {
          (tuple: (Game, Board), side: Side.Value) =>
            tuple match {
              case (_, board) => Mobility.knightMobility(board, side) < Mobility.knightMobility(board, ~side)
            }
        }
      case Piece.Bishop =>
        {
          (tuple: (Game, Board), side: Side.Value) =>
            tuple match {
              case (_, board) => Mobility.bishopMobility(board, side) < Mobility.bishopMobility(board, ~side)
            }
        }
      case Piece.Rook =>
        {
          (tuple: (Game, Board), side: Side.Value) =>
            tuple match {
              case (_, board) => Mobility.rookMobility(board, side) < Mobility.rookMobility(board, ~side)
            }
        }
      case Piece.Queen =>
        {
          (tuple: (Game, Board), side: Side.Value) =>
            tuple match {
              case (_, board) => Mobility.queenMobility(board, side) < Mobility.queenMobility(board, ~side)
            }
        }
      case Piece.King =>
        {
          (tuple: (Game, Board), side: Side.Value) =>
            tuple match {
              case (_, board) => Mobility.kingMobility(board, side) < Mobility.kingMobility(board, ~side)
            }
        }
    }
    NamedFunction2("< " + pieceToName(piece) + " mobility", fun)
  }
  
  def greaterSideMobility(side: Side.Value) =
    NamedFunction1("> " + sideToName(side) + " mobility", {
      (tuple: (Game, Board)) =>
        tuple match {
          case (_, board) => Mobility.mobility(board, side) > Mobility.mobility(board, ~side)
        }
    })

  def equalSideMobility(side: Side.Value) =
    NamedFunction1("= " + sideToName(side) + " mobility", {
      (tuple: (Game, Board)) =>
        tuple match {
          case (_, board) => Mobility.mobility(board, side) == Mobility.mobility(board, ~side)
        }
    })

  def lessSideMobility(side: Side.Value) =
    NamedFunction1("< " + sideToName(side) + " mobility", {
      (tuple: (Game, Board)) =>
        tuple match {
          case (_, board) => Mobility.mobility(board, side) < Mobility.mobility(board, ~side)
        }
    })

  def greaterSidePieceMobility(side: Side.Value, piece: Piece.Value) = {
    val fun = piece match {
      case Piece.Pawn =>
        {
          (tuple: (Game, Board)) =>
            tuple match {
              case (_, board) => Mobility.pawnMobility(board, side) > Mobility.pawnMobility(board, ~side)
            }
        }        
      case Piece.Knight =>
        {
          (tuple: (Game, Board)) =>
            tuple match {
              case (_, board) => Mobility.knightMobility(board, side) > Mobility.knightMobility(board, ~side)
            }
        }
      case Piece.Bishop =>
        {
          (tuple: (Game, Board)) =>
            tuple match {
              case (_, board) => Mobility.bishopMobility(board, side) > Mobility.bishopMobility(board, ~side)
            }
        }
      case Piece.Rook =>
        {
          (tuple: (Game, Board)) =>
            tuple match {
              case (_, board) => Mobility.rookMobility(board, side) > Mobility.rookMobility(board, ~side)
            }
        }
      case Piece.Queen =>
        {
          (tuple: (Game, Board)) =>
            tuple match {
              case (_, board) => Mobility.queenMobility(board, side) > Mobility.queenMobility(board, ~side)
            }
        }
      case Piece.King =>
        {
          (tuple: (Game, Board)) =>
            tuple match {
              case (_, board) => Mobility.kingMobility(board, side) > Mobility.kingMobility(board, ~side)
            }
        }
    }
    NamedFunction1("> " + sideToName(side) + " " + pieceToName(piece) + " mobility", fun)
  }

  def equalSidePieceMobility(side: Side.Value, piece: Piece.Value) = {
    val fun = piece match {
      case Piece.Pawn =>
        {
          (tuple: (Game, Board)) =>
            tuple match {
              case (_, board) => Mobility.pawnMobility(board, side) == Mobility.pawnMobility(board, ~side)
            }
        }        
      case Piece.Knight =>
        {
          (tuple: (Game, Board)) =>
            tuple match {
              case (_, board) => Mobility.knightMobility(board, side) == Mobility.knightMobility(board, ~side)
            }
        }
      case Piece.Bishop =>
        {
          (tuple: (Game, Board)) =>
            tuple match {
              case (_, board) => Mobility.bishopMobility(board, side) == Mobility.bishopMobility(board, ~side)
            }
        }
      case Piece.Rook =>
        {
          (tuple: (Game, Board)) =>
            tuple match {
              case (_, board) => Mobility.rookMobility(board, side) == Mobility.rookMobility(board, ~side)
            }
        }
      case Piece.Queen =>
        {
          (tuple: (Game, Board)) =>
            tuple match {
              case (_, board) => Mobility.queenMobility(board, side) == Mobility.queenMobility(board, ~side)
            }
        }
      case Piece.King =>
        {
          (tuple: (Game, Board)) =>
            tuple match {
              case (_, board) => Mobility.kingMobility(board, side) == Mobility.kingMobility(board, ~side)
            }
        }
    }
    NamedFunction1("= " + sideToName(side) + " " + pieceToName(piece) + " mobility", fun)
  }

  def lessSidePieceMobility(side: Side.Value, piece: Piece.Value) = {
    val fun = piece match {
      case Piece.Pawn =>
        {
          (tuple: (Game, Board)) =>
            tuple match {
              case (_, board) => Mobility.pawnMobility(board, side) < Mobility.pawnMobility(board, ~side)
            }
        }        
      case Piece.Knight =>
        {
          (tuple: (Game, Board)) =>
            tuple match {
              case (_, board) => Mobility.knightMobility(board, side) < Mobility.knightMobility(board, ~side)
            }
        }
      case Piece.Bishop =>
        {
          (tuple: (Game, Board)) =>
            tuple match {
              case (_, board) => Mobility.bishopMobility(board, side) < Mobility.bishopMobility(board, ~side)
            }
        }
      case Piece.Rook =>
        {
          (tuple: (Game, Board)) =>
            tuple match {
              case (_, board) => Mobility.rookMobility(board, side) < Mobility.rookMobility(board, ~side)
            }
        }
      case Piece.Queen =>
        {
          (tuple: (Game, Board)) =>
            tuple match {
              case (_, board) => Mobility.queenMobility(board, side) < Mobility.queenMobility(board, ~side)
            }
        }
      case Piece.King =>
        {
          (tuple: (Game, Board)) =>
            tuple match {
              case (_, board) => Mobility.kingMobility(board, side) < Mobility.kingMobility(board, ~side)
            }
        }
    }
    NamedFunction1("< " + sideToName(side) + " " + pieceToName(piece) + " mobility", fun)
  }
  
  val greaterPawnMobility = greaterPieceMobility(Piece.Pawn)
  val greaterKnightMobility = greaterPieceMobility(Piece.Knight)
  val greaterBishopMobility = greaterPieceMobility(Piece.Bishop)
  val greaterRookMobility = greaterPieceMobility(Piece.Rook)
  val greaterQueenMobility = greaterPieceMobility(Piece.Queen)
  val greaterKingMobility = greaterPieceMobility(Piece.King)
  val equalPawnMobility = equalPieceMobility(Piece.Pawn)
  val equalKnightMobility = equalPieceMobility(Piece.Knight)
  val equalBishopMobility = equalPieceMobility(Piece.Bishop)
  val equalRookMobility = equalPieceMobility(Piece.Rook)
  val equalQueenMobility = equalPieceMobility(Piece.Queen)
  val equalKingMobility = equalPieceMobility(Piece.King)
  val lessPawnMobility = lessPieceMobility(Piece.Pawn)
  val lessKnightMobility = lessPieceMobility(Piece.Knight)
  val lessBishopMobility = lessPieceMobility(Piece.Bishop)
  val lessRookMobility = lessPieceMobility(Piece.Rook)
  val lessQueenMobility = lessPieceMobility(Piece.Queen)
  val lessKingMobility = lessPieceMobility(Piece.King)
  val greaterWhiteMobility = greaterSideMobility(Side.White)
  val greaterBlackMobility = greaterSideMobility(Side.Black)
  val equalWhiteMobility = equalSideMobility(Side.White)
  val equalBlackMobility = equalSideMobility(Side.Black)
  val lessWhiteMobility = lessSideMobility(Side.White)
  val lessBlackMobility = lessSideMobility(Side.Black)
  val greaterWhitePawnMobility = greaterSidePieceMobility(Side.White, Piece.Pawn)
  val greaterWhiteKnightMobility = greaterSidePieceMobility(Side.White, Piece.Knight)
  val greaterWhiteBishopMobility = greaterSidePieceMobility(Side.White, Piece.Bishop)
  val greaterWhiteRookMobility = greaterSidePieceMobility(Side.White, Piece.Rook)
  val greaterWhiteQueenMobility = greaterSidePieceMobility(Side.White, Piece.Queen)
  val greaterWhiteKingMobility = greaterSidePieceMobility(Side.White, Piece.King)
  val greaterBlackPawnMobility = greaterSidePieceMobility(Side.Black, Piece.Pawn)
  val greaterBlackKnightMobility = greaterSidePieceMobility(Side.Black, Piece.Knight)
  val greaterBlackBishopMobility = greaterSidePieceMobility(Side.Black, Piece.Bishop)
  val greaterBlackRookMobility = greaterSidePieceMobility(Side.Black, Piece.Rook)
  val greaterBlackQueenMobility = greaterSidePieceMobility(Side.Black, Piece.Queen)
  val greaterBlackKingMobility = greaterSidePieceMobility(Side.Black, Piece.King)
  val equalWhitePawnMobility = equalSidePieceMobility(Side.White, Piece.Pawn)
  val equalWhiteKnightMobility = equalSidePieceMobility(Side.White, Piece.Knight)
  val equalWhiteBishopMobility = equalSidePieceMobility(Side.White, Piece.Bishop)
  val equalWhiteRookMobility = equalSidePieceMobility(Side.White, Piece.Rook)
  val equalWhiteQueenMobility = equalSidePieceMobility(Side.White, Piece.Queen)
  val equalWhiteKingMobility = equalSidePieceMobility(Side.White, Piece.King)
  val equalBlackPawnMobility = equalSidePieceMobility(Side.Black, Piece.Pawn)
  val equalBlackKnightMobility = equalSidePieceMobility(Side.Black, Piece.Knight)
  val equalBlackBishopMobility = equalSidePieceMobility(Side.Black, Piece.Bishop)
  val equalBlackRookMobility = equalSidePieceMobility(Side.Black, Piece.Rook)
  val equalBlackQueenMobility = equalSidePieceMobility(Side.Black, Piece.Queen)
  val equalBlackKingMobility = equalSidePieceMobility(Side.Black, Piece.King)
  val lessWhitePawnMobility = lessSidePieceMobility(Side.White, Piece.Pawn)
  val lessWhiteKnightMobility = lessSidePieceMobility(Side.White, Piece.Knight)
  val lessWhiteBishopMobility = lessSidePieceMobility(Side.White, Piece.Bishop)
  val lessWhiteRookMobility = lessSidePieceMobility(Side.White, Piece.Rook)
  val lessWhiteQueenMobility = lessSidePieceMobility(Side.White, Piece.Queen)
  val lessWhiteKingMobility = lessSidePieceMobility(Side.White, Piece.King)
  val lessBlackPawnMobility = lessSidePieceMobility(Side.Black, Piece.Pawn)
  val lessBlackKnightMobility = lessSidePieceMobility(Side.Black, Piece.Knight)
  val lessBlackBishopMobility = lessSidePieceMobility(Side.Black, Piece.Bishop)
  val lessBlackRookMobility = lessSidePieceMobility(Side.Black, Piece.Rook)
  val lessBlackQueenMobility = lessSidePieceMobility(Side.Black, Piece.Queen)
  val lessBlackKingMobility = lessSidePieceMobility(Side.Black, Piece.King)
}
