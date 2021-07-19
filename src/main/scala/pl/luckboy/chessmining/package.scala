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

/** This package contains a library to data mining for chess games.
  *
  * The example usage is:
  * {{{
  * import scalax.chart.api._
  * import pl.luckboy.chessmining._
  * val games = Games.fromFile("somefile.pgn")
  * val iter = for(g <- games; b <- Boards.fromGame(g)) yield (g, b) // ->
  * val miner = WinBoardMiner(anyPiece) +\+ white3
  * val data = miner(iter)
  * val chart = BoardChart(data)
  * chart.show()
  * }}}
  */
package object chessmining
{
  /** A default factory of game reader that is the factory of PGN reader. */
  implicit val defaultGameReaderFactory = PGNReader
  /** A default factory of game writer that is the factory of PGN writer. */
  implicit val defaultGameWriterFactory = PGNWriter
  /** A default factory of file progress bar that is the factory of console file progress bar. */
  implicit val defaultFileProgressBarFactory = ConsoleFileProgressBar

  /** A default factory of win miner. */
  implicit def defaultWinMinerFactory[T] = new BinaryValueMinerFactory[WinMiner[T], BinaryMiner[(Game, T), _], WinMiner[T]] {
    override def apply(miner: WinMiner[T], firstMinerOpt: Option[BinaryMiner[(Game, T), _]], secondMinerOpt: Option[BinaryMiner[(Game, T), _]]) =
      miner.copy(firstMinerOption = firstMinerOpt, secondMinerOption = secondMinerOpt)
  }

  /** A default factory of win board miner. */
  implicit def defaultWinBoardMinerFactory[T] = new BinaryValueMinerFactory[WinBoardMiner[T], BinaryBoardMiner[(Game, T), _], WinBoardMiner[T]] {
    override def apply(miner: WinBoardMiner[T], firstMinerOpt: Option[BinaryBoardMiner[(Game, T), _]], secondMinerOpt: Option[BinaryBoardMiner[(Game, T), _]]) =
      miner.copy(firstMinerOption = firstMinerOpt, secondMinerOption = secondMinerOpt)
  }

  /** A default factory of loss miner. */
  implicit def defaultLossMinerFactory[T] = new BinaryValueMinerFactory[LossMiner[T], BinaryMiner[(Game, T), _], LossMiner[T]] {
    override def apply(miner: LossMiner[T], firstMinerOpt: Option[BinaryMiner[(Game, T), _]], secondMinerOpt: Option[BinaryMiner[(Game, T), _]]) =
      miner.copy(firstMinerOption = firstMinerOpt, secondMinerOption = secondMinerOpt)
  }

  /** A default factory of loss board miner. */
  implicit def defaultLossBoardMinerFactory[T] = new BinaryValueMinerFactory[LossBoardMiner[T], BinaryBoardMiner[(Game, T), _], LossBoardMiner[T]] {
    override def apply(miner: LossBoardMiner[T], firstMinerOpt: Option[BinaryBoardMiner[(Game, T), _]], secondMinerOpt: Option[BinaryBoardMiner[(Game, T), _]]) =
      miner.copy(firstMinerOption = firstMinerOpt, secondMinerOption = secondMinerOpt)
  }

  /** A default factory of draw miner. */
  implicit def defaultDrawMinerFactory[T] = new BinaryValueMinerFactory[DrawMiner[T], BinaryMiner[(Game, T), _], DrawMiner[T]] {
    override def apply(miner: DrawMiner[T], firstMinerOpt: Option[BinaryMiner[(Game, T), _]], secondMinerOpt: Option[BinaryMiner[(Game, T), _]]) =
      miner.copy(firstMinerOption = firstMinerOpt, secondMinerOption = secondMinerOpt)
  }

  /** A default factory of draw board miner. */
  implicit def defaultDrawBoardMinerFactory[T] = new BinaryValueMinerFactory[DrawBoardMiner[T], BinaryBoardMiner[(Game, T), _], DrawBoardMiner[T]] {
    override def apply(miner: DrawBoardMiner[T], firstMinerOpt: Option[BinaryBoardMiner[(Game, T), _]], secondMinerOpt: Option[BinaryBoardMiner[(Game, T), _]]) =
      miner.copy(firstMinerOption = firstMinerOpt, secondMinerOption = secondMinerOpt)
  }

  /** A default factory of count miner. */
  implicit def defaultCountMinerFactory[T] = new BinaryValueMinerFactory[CountMiner[T], BinaryMiner[(Game, T), _], CountMiner[T]] {
    override def apply(miner: CountMiner[T], firstMinerOpt: Option[BinaryMiner[(Game, T), _]], secondMinerOpt: Option[BinaryMiner[(Game, T), _]]) =
      miner.copy(firstMinerOption = firstMinerOpt, secondMinerOption = secondMinerOpt)
  }

  /** A default factory of count board miner. */
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

  /** A named function of any data element for the win miner and the loss miner. */
  val any = NamedFunction2("any", {
      (any: Any, side: Side.Value) => true
    })

  /** A named function of any data element for the win board miner and the loss board miner. */
  val any3 = NamedFunction3("any", {
      (any: Any, side: Side.Value, squ: Int) => true
    })

  /** A named function of any data element for the draw miner and the count miner. */
  val any1 = NamedFunction1("any", {
      (any: Any) => true
    })

  /** A named function of any data element for the draw board miner and the count board miner. */
  val any2 = NamedFunction2("any", {
      (any: Any, squ: Int) => true
    })

  /** Returns a named function of side for the win miner and the loss miner.
    *
    * @param side the side.
    * @return a named function.
    */
  def side(side: Side.Value) =
    NamedFunction2(sideToName(side), {
      (any: Any, side2: Side.Value) => side == side2
    })

  /** Returns a named function of side for the win board miner and the loss board miner.
    *
    * @param side the side.
    * @return a named function.
    */
  def side3(side: Side.Value) =
    NamedFunction3(sideToName(side), {
      (any: Any, side2: Side.Value, squ: Int) => side == side2
    })

  /** A named function of white for the win miner and the loss miner. */
  val white = side(Side.White)
  /** A named function of black for the win miner and the loss miner. */
  val black = side(Side.Black)
  /** A named function of white for the win board miner and the loss board miner. */
  val white3 = side3(Side.White)
  /** A named function of black for the win board miner and the loss board miner. */
  val black3 = side3(Side.Black)

  /** A named function of any piece for the win board and the loss board miner. */ 
  val anyPiece = NamedFunction3("piece", {
      (tuple: (Game, Board), side: Side.Value, squ: Int) =>
        tuple match {
          case (_, board) => board.color(squ) == sideToColor(side)
        }
    })
  
  /** Returns a named function of piece for the win board miner and the loss board miner.
    *
    * @param piece the piece.
    * @return a named function.
    */
  def piece(piece: Piece.Value) =
    NamedFunction3(pieceToName(piece), {
      (tuple: (Game, Board), side: Side.Value, squ: Int) =>
        tuple match {
          case (_, board) => board.coloredPiece(squ) == sideAndPieceToColoredPiece(side, piece)
        }
    })
    
  /** A named function of empty for the win board miner and the loss board miner. */
  val empty = NamedFunction3("empty", {
      (tuple: (Game, Board), side: Side.Value, squ: Int) =>
        tuple match {
          case (_, board) => board.color(squ) == Color.Empty
        }
    })

  /** A named function of any piece for the draw board miner and the count board miner. */
  val anyPiece2 = NamedFunction2("piece", {
      (tuple: (Game, Board), squ: Int) =>
        tuple match {
          case (_, board) => board.color(squ) != Color.Empty
        }
    })

  /** Returns a named function of piece for the draw board miner and the count board miner.
    *
    * @param piece the piece.
    * @return a named function.
    */
  def piece2(piece: Piece.Value) =
    NamedFunction2(pieceToName(piece), {
      (tuple: (Game, Board), squ: Int) =>
        tuple match {
          case (_, board) => board.pieceOption(squ).map { _ == piece }.getOrElse(false)
        }
    })

  /** A named function of empty for the draw board miner and the count board miner. */
  val empty2 = NamedFunction2("empty", {
      (tuple: (Game, Board), squ: Int) =>
        tuple match {
          case (_, board) => board.color(squ) == Color.Empty
        }
    })

  /** Returns a named function of any side piece for the draw board miner and the count board miner.
    *
    * @param side the side.
    * @return a named function.
    */
  def anySidePiece(side: Side.Value) =
    NamedFunction2(sideToName(side) + " piece", {
      (tuple: (Game, Board), squ: Int) =>
        tuple match {
          case (_, board) => board.color(squ) == sideToColor(side)
        }
    })

  /** Returns a named function of side piece for the draw board miner and the count board miner.
    *
    * @param side the side.
    * @param piece the piece.
    * @return a named function.
    */
  def sidePiece(side: Side.Value, piece: Piece.Value) = 
    NamedFunction2(sideToName(side) + " " + pieceToName(piece), {
      (tuple: (Game, Board), squ: Int) =>
        tuple match {
          case (_, board) => board.coloredPiece(squ) == sideAndPieceToColoredPiece(side, piece)
        }
    })

  /** A named function of pawn for the win board miner and the loss board miner. */
  val pawn = piece(Piece.Pawn)
  /** A named function of knight for the win board miner and the loss board miner. */
  val knight = piece(Piece.Knight)
  /** A named function of bishop for the win board miner and the loss board miner. */
  val bishop = piece(Piece.Bishop)
  /** A named function of rook for the win board miner and the loss board miner. */
  val rook = piece(Piece.Rook)
  /** A named function of queen for the win board miner and the loss board miner. */
  val queen = piece(Piece.Queen)
  /** A named function of king for the win board miner and the loss board miner. */
  val king = piece(Piece.King)
  /** A named function of pawn for the draw board miner and the count board miner. */
  val pawn2 = piece2(Piece.Pawn)
  /** A named function of knight for the draw board miner and the count board miner. */
  val knight2 = piece2(Piece.Knight)
  /** A named function of bishop for the draw board miner and the count board miner. */
  val bishop2 = piece2(Piece.Bishop)
  /** A named function of rook for the draw board miner and the count board miner. */
  val rook2 = piece2(Piece.Rook)
  /** A named function of queen for the draw board miner and the count board miner. */
  val queen2 = piece2(Piece.Queen)
  /** A named function of king for the draw board miner and the count board miner. */
  val king2 = piece2(Piece.King)
  /** A named function of any white piece for the draw board miner and the count board miner. */
  val anyWhitePiece = anySidePiece(Side.White)
  /** A named function of any black piece for the draw board miner and the count board miner. */
  val anyBlackPiece = anySidePiece(Side.Black)
  /** A named function of white pawn for the draw board miner and the count board miner. */
  val whitePawn = sidePiece(Side.White, Piece.Pawn)
  /** A named function of white knight for the draw board miner and the count board miner. */
  val whiteKnight = sidePiece(Side.White, Piece.Knight)
  /** A named function of white bishop for the draw board miner and the count board miner. */
  val whiteBishop = sidePiece(Side.White, Piece.Bishop)
  /** A named function of white rook for the draw board miner and the count board miner. */
  val whiteRook = sidePiece(Side.White, Piece.Rook)
  /** A named function of white queen for the draw board miner and the count board miner. */
  val whiteQueen = sidePiece(Side.White, Piece.Queen)
  /** A named function of white king for the draw board miner and the count board miner. */
  val whiteKing = sidePiece(Side.White, Piece.King)
  /** A named function of black pawn for the draw board miner and the count board miner. */
  val blackPawn = sidePiece(Side.Black, Piece.Pawn)
  /** A named function of black knight for the draw board miner and the count board miner. */
  val blackKnight = sidePiece(Side.Black, Piece.Knight)
  /** A named function of black bishop for the draw board miner and the count board miner. */
  val blackBishop = sidePiece(Side.Black, Piece.Bishop)
  /** A named function of black rook for the draw board miner and the count board miner. */
  val blackRook = sidePiece(Side.Black, Piece.Rook)
  /** A named function of black queen for the draw board miner and the count board miner. */
  val blackQueen = sidePiece(Side.Black, Piece.Queen)
  /** A named function of black king for the draw board miner and the count board miner. */
  val blackKing = sidePiece(Side.Black, Piece.King)

  /** A named function of move for the win miner and the loss miner. */
  val move = NamedFunction2("move", {
      (tuple: (Game, SideMove), side: Side.Value) =>
        tuple match {
          case (_, SideMove(moveSide, _)) => moveSide == side
        }
    })

  /** Returns a named function of side move for the draw miner and the count miner.
    *
    * @param side the side.
    * @return a named function.
    */
  def sideMove(side: Side.Value) =
    NamedFunction1(sideToName(side) + " move", {
      (tuple: (Game, SideMove)) =>
        tuple match {
          case (_, SideMove(moveSide, _)) => moveSide == side
        }
    })

  /** A named function of white move for the draw miner and the count miner. */
  val whiteMove = sideMove(Side.White)
  /** A named function of black move for the draw miner and the count miner. */
  val blackMove = sideMove(Side.Black)

  /** A named function of move source for the win board miner and the loss board miner. */
  val moveSource = NamedFunction3("move source", {
      (tuple: (Game, SideMove), side: Side.Value, squ: Int) =>
        tuple match {
          case (_, SideMove(moveSide, normalMove @ NormalMove(_, _, _, _, _))) =>
            moveSide == side && normalMove.from == squ
          case _ =>
            false
        }
    })

  /** A named function of move destination for the win board miner and the loss board miner. */
  val moveDestination = NamedFunction3("move destination", {
      (tuple: (Game, SideMove), side: Side.Value, squ: Int) =>
        tuple match {
          case (_, SideMove(moveSide, normalMove @ NormalMove(_, _, _, _, _))) =>
            moveSide == side && normalMove.to == squ
          case _ =>
            false
        }
    })

  /** Returns a named function of side move source for the draw board miner and the count board miner.
    *
    * @param side the side.
    * @return a named function.
    */
  def sideMoveSource(side: Side.Value) =
    NamedFunction2(sideToName(side) + " move source", {
      (tuple: (Game, SideMove), squ: Int) =>
        tuple match {
          case (_, SideMove(moveSide, normalMove @ NormalMove(_, _, _, _, _))) =>
            moveSide == side && normalMove.from == squ
          case _ =>
            false
        }
    })

  /** Returns a named function of side move destination for the draw board miner and the count board miner.
    *
    * @param side the side.
    * @return a named function.
    */
  def sideMoveDestination(side: Side.Value) =
    NamedFunction2(sideToName(side) + " move destination", {
      (tuple: (Game, SideMove), squ: Int) =>
        tuple match {
          case (_, SideMove(moveSide, normalMove @ NormalMove(_, _, _, _, _))) =>
            moveSide == side && normalMove.to == squ
          case _ =>
            false
        }
    })
  
  /** A named function of white move source for the draw board miner and the count board miner. */
  val whiteMoveSource = sideMoveSource(Side.White)
  /** A named function of black move source for the draw board miner and the count board miner. */
  val blackMoveSource = sideMoveSource(Side.Black)
  /** A named function of white move destination for the draw board miner and the count board miner. */
  val whiteMoveDestination = sideMoveDestination(Side.White)
  /** A named function of black move destination for the draw board miner and the count board miner. */
  val blackMoveDestination = sideMoveDestination(Side.Black)

  /** A named function of move for the win miner, the loss miner and the board move. */
  val moveForBoardMove = NamedFunction2("move", {
      (tuple: (Game, BoardMove), side: Side.Value) =>
        tuple match {
          case (_, BoardMove(board, _, _)) => board.side == side
        }
    })

  /** Returns a named function of side move for the draw miner, the count miner and the board move.
    *
    * @param side the side.
    * @return a named function.
    */
  def sideMoveForBoardMove(side: Side.Value) =
    NamedFunction1(sideToName(side) + " move", {
      (tuple: (Game, BoardMove)) =>
        tuple match {
          case (_, BoardMove(board, _, _)) => board.side == side
        }
    })

  /** A named function of white move for the draw miner, the count miner and the board move. */
  val whiteMoveForBoardMove = sideMoveForBoardMove(Side.White)
  /** A named function of black move for the draw miner, the count miner and the board move. */
  val blackMoveForBoardMove = sideMoveForBoardMove(Side.Black)
  
  /** A named function of move source for the win board miner, the loss board miner and the board
    * move.
    */
  val moveSourceForBoardMove = NamedFunction3("move source", {
      (tuple: (Game, BoardMove), side: Side.Value, squ: Int) =>
        tuple match {
          case (_, BoardMove(board, normalMove @ NormalMove(_, _, _, _, _), _)) =>
            board.side == side && normalMove.from == squ
          case _ =>
            false
        }
    })

  /** A named function of move destination for the win board miner, the loss board miner and the board
    * move.
    */
  val moveDestinationForBoardMove = NamedFunction3("move destination", {
      (tuple: (Game, BoardMove), side: Side.Value, squ: Int) =>
        tuple match {
          case (_, BoardMove(board, normalMove @ NormalMove(_, _, _, _, _), _)) =>
            board.side == side && normalMove.to == squ
          case _ =>
            false
        }
    })

  /** Returns a named function of side move source for the draw board miner, the count board miner
    * and the board move.
    *
    * @param side the side.
    * @return a named function.
    */
  def sideMoveSourceForBoardMove(side: Side.Value) =
    NamedFunction2(sideToName(side) + " move source", {
      (tuple: (Game, BoardMove), squ: Int) =>
        tuple match {
          case (_, BoardMove(board, normalMove @ NormalMove(_, _, _, _, _), _)) =>
            board.side == side && normalMove.from == squ
          case _ =>
            false
        }
    })

  /** Returns a named function of side move destination for the draw board miner, the count board
    * miner and the board move.
    *
    * @param side the side.
    * @return a named function.
    */
  def sideMoveDestinationForBoardMove(side: Side.Value) =
    NamedFunction2(sideToName(side) + " move destination", {
      (tuple: (Game, BoardMove), squ: Int) =>
        tuple match {
          case (_, BoardMove(board, normalMove @ NormalMove(_, _, _, _, _), _)) =>
            board.side == side && normalMove.to == squ
          case _ =>
            false
        }
    })

  /** A named function of white move source for the draw board miner, the count board miner and the
    * board move.
    */
  val whiteMoveSourceForBoardMove = sideMoveSourceForBoardMove(Side.White)
  /** A named function of black move source for the draw board miner, the count board miner and the
    * board move.
    */
  val blackMoveSourceForBoardMove = sideMoveSourceForBoardMove(Side.Black)
  /** A named function of white move destination for the draw board miner, the count board miner and
    * the board move.
    */
  val whiteMoveDestinationForBoardMove = sideMoveDestinationForBoardMove(Side.White)
  /** A named function of black move destination for the draw board miner, the count board miner and
    * the board move.
    */
  val blackMoveDestinationForBoardMove = sideMoveDestinationForBoardMove(Side.Black)
  
  //
  // Named functions of mobility.
  //
  
  /** A named function of greater mobility for the win miner and the loss miner. */
  val greaterMobility = NamedFunction2("> mobility", {
      (tuple: (Game, Board), side: Side.Value) =>
        tuple match {
          case (_, board) => Mobility.mobility(board, side) > Mobility.mobility(board, ~side)
        }
    })

  /** A named function of equal mobility for the win miner and the loss miner. */
  val equalMobility = NamedFunction2("= mobility", {
      (tuple: (Game, Board), side: Side.Value) =>
        tuple match {
          case (_, board) => Mobility.mobility(board, side) == Mobility.mobility(board, ~side)
        }
    })

  /** A named function of less mobility for the win miner and the loss miner. */
  val lessMobility = NamedFunction2("< mobility", {
      (tuple: (Game, Board), side: Side.Value) =>
        tuple match {
          case (_, board) => Mobility.mobility(board, side) < Mobility.mobility(board, ~side)
        }
    })

  /** Returns a named function of greater piece mobility for the win miner and the loss miner.
    *
    * @param piece the piece.
    * @return a named function.
    */
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

  /** Returns a named function of equal piece mobility for the win miner and the loss miner.
    *
    * @param piece the piece.
    * @return a named function.
    */
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

  /** Returns a named function of less piece mobility for the win miner and the loss miner.
    *
    * @param piece the piece.
    * @return a named function.
    */
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
  
  /** Returns a named function of greater side mobility for the draw miner and the count miner.
    *
    * @param side the side.
    * @return a named function.
    */
  def greaterSideMobility(side: Side.Value) =
    NamedFunction1("> " + sideToName(side) + " mobility", {
      (tuple: (Game, Board)) =>
        tuple match {
          case (_, board) => Mobility.mobility(board, side) > Mobility.mobility(board, ~side)
        }
    })

  /** Returns a named function of equal side mobility for the draw miner and the count miner.
    *
    * @param side the side.
    * @return a named function.
    */
  def equalSideMobility(side: Side.Value) =
    NamedFunction1("= " + sideToName(side) + " mobility", {
      (tuple: (Game, Board)) =>
        tuple match {
          case (_, board) => Mobility.mobility(board, side) == Mobility.mobility(board, ~side)
        }
    })

  /** Returns a named function of less side mobility for the draw miner and the count miner.
    *
    * @param side the side.
    * @return a named function.
    */
  def lessSideMobility(side: Side.Value) =
    NamedFunction1("< " + sideToName(side) + " mobility", {
      (tuple: (Game, Board)) =>
        tuple match {
          case (_, board) => Mobility.mobility(board, side) < Mobility.mobility(board, ~side)
        }
    })

  /** Returns a named function of greater side piece mobility for the draw miner and the count miner.
    *
    * @param side the side.
    * @param piece the piece.
    * @return a named function.
    */
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

  /** Returns a named function of equal side piece mobility for the draw miner and the count miner.
    *
    * @param side the side.
    * @param piece the piece.
    * @return a named function.
    */
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

  /** Returns a named function of less side piece mobility for the draw miner and the count miner.
    *
    * @param side the side.
    * @param piece the piece.
    * @return a named function.
    */
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
  
  /** A named function of greater pawn mobility for the win miner and the loss miner. */
  val greaterPawnMobility = greaterPieceMobility(Piece.Pawn)
  /** A named function of greater knight mobility for the win miner and the loss miner. */
  val greaterKnightMobility = greaterPieceMobility(Piece.Knight)
  /** A named function of greater bishop mobility for the win miner and the loss miner. */
  val greaterBishopMobility = greaterPieceMobility(Piece.Bishop)
  /** A named function of greater rook mobility for the win miner and the loss miner. */
  val greaterRookMobility = greaterPieceMobility(Piece.Rook)
  /** A named function of greater queen mobility for the win miner and the loss miner. */
  val greaterQueenMobility = greaterPieceMobility(Piece.Queen)
  /** A named function of greater king mobility for the win miner and the loss miner. */
  val greaterKingMobility = greaterPieceMobility(Piece.King)
  /** A named function of equal pawn mobility for the win miner and the loss miner. */
  val equalPawnMobility = equalPieceMobility(Piece.Pawn)
  /** A named function of equal knight mobility for the win miner and the loss miner. */
  val equalKnightMobility = equalPieceMobility(Piece.Knight)
  /** A named function of equal bishop mobility for the win miner and the loss miner. */
  val equalBishopMobility = equalPieceMobility(Piece.Bishop)
  /** A named function of equal rook mobility for the win miner and the loss miner. */
  val equalRookMobility = equalPieceMobility(Piece.Rook)
  /** A named function of equal queen mobility for the win miner and the loss miner. */
  val equalQueenMobility = equalPieceMobility(Piece.Queen)
  /** A named function of equal king mobility for the win miner and the loss miner. */
  val equalKingMobility = equalPieceMobility(Piece.King)
  /** A named function of less pawn mobility for the win miner and the loss miner. */
  val lessPawnMobility = lessPieceMobility(Piece.Pawn)
  /** A named function of less knight mobility for the win miner and the loss miner. */
  val lessKnightMobility = lessPieceMobility(Piece.Knight)
  /** A named function of less bishop mobility for the win miner and the loss miner. */
  val lessBishopMobility = lessPieceMobility(Piece.Bishop)
  /** A named function of less rook mobility for the win miner and the loss miner. */
  val lessRookMobility = lessPieceMobility(Piece.Rook)
  /** A named function of less queen mobility for the win miner and the loss miner. */
  val lessQueenMobility = lessPieceMobility(Piece.Queen)
  /** A named function of less king mobility for the win miner and the loss miner. */
  val lessKingMobility = lessPieceMobility(Piece.King)
  /** A named function of greater white mobility for the draw miner and the count miner. */
  val greaterWhiteMobility = greaterSideMobility(Side.White)
  /** A named function of greater black mobility for the draw miner and the count miner. */
  val greaterBlackMobility = greaterSideMobility(Side.Black)
  /** A named function of equal white mobility for the draw miner and the count miner. */
  val equalWhiteMobility = equalSideMobility(Side.White)
  /** A named function of equal black mobility for the draw miner and the count miner. */
  val equalBlackMobility = equalSideMobility(Side.Black)
  /** A named function of less white mobility for the draw miner and the count miner. */
  val lessWhiteMobility = lessSideMobility(Side.White)
  /** A named function of less black mobility for the draw miner and the count miner. */
  val lessBlackMobility = lessSideMobility(Side.Black)
  /** A named function of greater white pawn mobility for the draw miner and the count miner. */
  val greaterWhitePawnMobility = greaterSidePieceMobility(Side.White, Piece.Pawn)
  /** A named function of greater white knight mobility for the draw miner and the count miner. */
  val greaterWhiteKnightMobility = greaterSidePieceMobility(Side.White, Piece.Knight)
  /** A named function of greater white bishop mobility for the draw miner and the count miner. */
  val greaterWhiteBishopMobility = greaterSidePieceMobility(Side.White, Piece.Bishop)
  /** A named function of greater white rook mobility for the draw miner and the count miner. */
  val greaterWhiteRookMobility = greaterSidePieceMobility(Side.White, Piece.Rook)
  /** A named function of greater white queen mobility for the draw miner and the count miner. */
  val greaterWhiteQueenMobility = greaterSidePieceMobility(Side.White, Piece.Queen)
  /** A named function of greater white king mobility for the draw miner and the count miner. */
  val greaterWhiteKingMobility = greaterSidePieceMobility(Side.White, Piece.King)
  /** A named function of greater black pawn mobility for the draw miner and the count miner. */
  val greaterBlackPawnMobility = greaterSidePieceMobility(Side.Black, Piece.Pawn)
  /** A named function of greater black knight mobility for the draw miner and the count miner. */
  val greaterBlackKnightMobility = greaterSidePieceMobility(Side.Black, Piece.Knight)
  /** A named function of greater black bishop mobility for the draw miner and the count miner. */
  val greaterBlackBishopMobility = greaterSidePieceMobility(Side.Black, Piece.Bishop)
  /** A named function of greater black rook mobility for the draw miner and the count miner. */
  val greaterBlackRookMobility = greaterSidePieceMobility(Side.Black, Piece.Rook)
  /** A named function of greater black queen mobility for the draw miner and the count miner. */
  val greaterBlackQueenMobility = greaterSidePieceMobility(Side.Black, Piece.Queen)
  /** A named function of greater black king mobility for the draw miner and the count miner. */
  val greaterBlackKingMobility = greaterSidePieceMobility(Side.Black, Piece.King)
  /** A named function of equal white pawn mobility for the draw miner and the count miner. */
  val equalWhitePawnMobility = equalSidePieceMobility(Side.White, Piece.Pawn)
  /** A named function of equal white knight mobility for the draw miner and the count miner. */
  val equalWhiteKnightMobility = equalSidePieceMobility(Side.White, Piece.Knight)
  /** A named function of equal white bishop mobility for the draw miner and the count miner. */
  val equalWhiteBishopMobility = equalSidePieceMobility(Side.White, Piece.Bishop)
  /** A named function of equal white rook mobility for the draw miner and the count miner. */
  val equalWhiteRookMobility = equalSidePieceMobility(Side.White, Piece.Rook)
  /** A named function of equal white queen mobility for the draw miner and the count miner. */
  val equalWhiteQueenMobility = equalSidePieceMobility(Side.White, Piece.Queen)
  /** A named function of equal white king mobility for the draw miner and the count miner. */
  val equalWhiteKingMobility = equalSidePieceMobility(Side.White, Piece.King)
  /** A named function of equal black pawn mobility for the draw miner and the count miner. */
  val equalBlackPawnMobility = equalSidePieceMobility(Side.Black, Piece.Pawn)
  /** A named function of equal black knight mobility for the draw miner and the count miner. */
  val equalBlackKnightMobility = equalSidePieceMobility(Side.Black, Piece.Knight)
  /** A named function of equal black bishop mobility for the draw miner and the count miner. */
  val equalBlackBishopMobility = equalSidePieceMobility(Side.Black, Piece.Bishop)
  /** A named function of equal black rook mobility for the draw miner and the count miner. */
  val equalBlackRookMobility = equalSidePieceMobility(Side.Black, Piece.Rook)
  /** A named function of equal black queen mobility for the draw miner and the count miner. */
  val equalBlackQueenMobility = equalSidePieceMobility(Side.Black, Piece.Queen)
  /** A named function of equal black king mobility for the draw miner and the count miner. */
  val equalBlackKingMobility = equalSidePieceMobility(Side.Black, Piece.King)
  /** A named function of less white pawn mobility for the draw miner and the count miner. */
  val lessWhitePawnMobility = lessSidePieceMobility(Side.White, Piece.Pawn)
  /** A named function of less white knight mobility for the draw miner and the count miner. */
  val lessWhiteKnightMobility = lessSidePieceMobility(Side.White, Piece.Knight)
  /** A named function of less white bishop mobility for the draw miner and the count miner. */
  val lessWhiteBishopMobility = lessSidePieceMobility(Side.White, Piece.Bishop)
  /** A named function of less white rook mobility for the draw miner and the count miner. */
  val lessWhiteRookMobility = lessSidePieceMobility(Side.White, Piece.Rook)
  /** A named function of less white queen mobility for the draw miner and the count miner. */
  val lessWhiteQueenMobility = lessSidePieceMobility(Side.White, Piece.Queen)
  /** A named function of less white king mobility for the draw miner and the count miner. */
  val lessWhiteKingMobility = lessSidePieceMobility(Side.White, Piece.King)
  /** A named function of less black pawn mobility for the draw miner and the count miner. */
  val lessBlackPawnMobility = lessSidePieceMobility(Side.Black, Piece.Pawn)
  /** A named function of less black knight mobility for the draw miner and the count miner. */
  val lessBlackKnightMobility = lessSidePieceMobility(Side.Black, Piece.Knight)
  /** A named function of less black bishop mobility for the draw miner and the count miner. */
  val lessBlackBishopMobility = lessSidePieceMobility(Side.Black, Piece.Bishop)
  /** A named function of less black rook mobility for the draw miner and the count miner. */
  val lessBlackRookMobility = lessSidePieceMobility(Side.Black, Piece.Rook)
  /** A named function of less black queen mobility for the draw miner and the count miner. */
  val lessBlackQueenMobility = lessSidePieceMobility(Side.Black, Piece.Queen)
  /** A named function of less black king mobility for the draw miner and the count miner. */
  val lessBlackKingMobility = lessSidePieceMobility(Side.Black, Piece.King)

  /** A named function of move of greater mobility for the win miner and the loss miner. */
  val greaterMobilityMove = NamedFunction2("> mobility move", {
      (tuple: (Game, BoardMove), side: Side.Value) =>
        tuple match {
          case (_, BoardMove(board, _, nextBoard)) =>
            board.side == side &&
            Mobility.mobility(nextBoard, board.side) > Mobility.mobility(board, board.side)
        }
    })

  /** A named function of move of equal mobility for the win miner and the loss miner. */
  val equalMobilityMove = NamedFunction2("= mobility move", {
      (tuple: (Game, BoardMove), side: Side.Value) =>
        tuple match {
          case (_, BoardMove(board, _, nextBoard)) =>
            board.side == side &&
            Mobility.mobility(nextBoard, board.side) == Mobility.mobility(board, board.side)
        }
    })

  /** A named function of move of less mobility for the win miner and the loss miner. */
  val lessMobilityMove = NamedFunction2("< mobility move", {
      (tuple: (Game, BoardMove), side: Side.Value) =>
        tuple match {
          case (_, BoardMove(board, _, nextBoard)) =>
            board.side == side &&
            Mobility.mobility(nextBoard, board.side) < Mobility.mobility(board, board.side)
        }
    })

  /** Returns a named function of move of greater side mobility for the draw miner and the count
    * miner.
    *
    * @param side the side.
    * @return a named function.
    */
  def greaterSideMobilityMove(side: Side.Value) =
    NamedFunction1("> " + sideToName(side) + " mobility move", {
      (tuple: (Game, BoardMove)) =>
        tuple match {
          case (_, BoardMove(board, _, nextBoard)) =>
            board.side == side &&
            Mobility.mobility(nextBoard, board.side) > Mobility.mobility(board, board.side)
        }
    })

  /** Returns a named function of move of equal side mobility for the draw miner and the count miner.
    *
    * @param side the side.
    * @return a named function.
    */
  def equalSideMobilityMove(side: Side.Value) =
    NamedFunction1("= " + sideToName(side) + " mobility move", {
      (tuple: (Game, BoardMove)) =>
        tuple match {
          case (_, BoardMove(board, _, nextBoard)) =>
            board.side == side &&
            Mobility.mobility(nextBoard, board.side) == Mobility.mobility(board, board.side)
        }
    })

  /** Returns a named function of move of less side mobility for the draw miner and the count miner.
    *
    * @param side the side.
    * @return a named function.
    */
  def lessSideMobilityMove(side: Side.Value) =
    NamedFunction1("< " + sideToName(side) + " mobility move", {
      (tuple: (Game, BoardMove)) =>
        tuple match {
          case (_, BoardMove(board, _, nextBoard)) =>
            board.side == side &&
            Mobility.mobility(nextBoard, board.side) < Mobility.mobility(board, board.side)
        }
    })

  /** A named function of move of greater white mobility for the draw miner and the count miner. */
  val greaterWhiteMobilityMove = greaterSideMobilityMove(Side.White)
  /** A named function of move of greater black mobility for the draw miner and the count miner. */
  val greaterBlackMobilityMove = greaterSideMobilityMove(Side.Black)
  /** A named function of move of equal white mobility for the draw miner and the count miner. */
  val equalWhiteMobilityMove = equalSideMobilityMove(Side.White)
  /** A named function of move of equal black mobility for the draw miner and the count miner. */
  val equalBlackMobilityMove = equalSideMobilityMove(Side.Black)
  /** A named function of move of less white mobility for the draw miner and the count miner. */
  val lessWhiteMobilityMove = lessSideMobilityMove(Side.White)
  /** A named function of move of less black mobility for the draw miner and the count miner. */
  val lessBlackMobilityMove = lessSideMobilityMove(Side.Black)

  /** A named function of move source of greater mobility for the win board miner and the loss board
    * miner.
    */
  val greaterMobilityMoveSource = NamedFunction3("> mobility move source", {
      (tuple: (Game, BoardMove), side: Side.Value, squ: Int) =>
        tuple match {
          case (_, BoardMove(board, normalMove @ NormalMove(_, _, _, _, _), nextBoard)) =>
            board.side == side && normalMove.from == squ &&
            Mobility.mobility(nextBoard, board.side) > Mobility.mobility(board, board.side)
          case _ =>
            false
        }
    })

  /** A named function of move source of equal mobility for the win board miner and the loss board
    * miner.
    */
  val equalMobilityMoveSource = NamedFunction3("= mobility move source", {
      (tuple: (Game, BoardMove), side: Side.Value, squ: Int) =>
        tuple match {
          case (_, BoardMove(board, normalMove @ NormalMove(_, _, _, _, _), nextBoard)) =>
            board.side == side && normalMove.from == squ &&
            Mobility.mobility(nextBoard, board.side) == Mobility.mobility(board, board.side)
          case _ =>
            false
        }
    })

  /** A named function of move source of less mobility for the win board miner and the loss board
    * miner.
    */
  val lessMobilityMoveSource = NamedFunction3("< mobility move source", {
      (tuple: (Game, BoardMove), side: Side.Value, squ: Int) =>
        tuple match {
          case (_, BoardMove(board, normalMove @ NormalMove(_, _, _, _, _), nextBoard)) =>
            board.side == side && normalMove.from == squ &&
            Mobility.mobility(nextBoard, board.side) < Mobility.mobility(board, board.side)
          case _ =>
            false
        }
    })

  /** A named function of move destination of greater mobility for the win board miner and the loss
    * board miner.
    */
  val greaterMobilityMoveDestination = NamedFunction3("> mobility move destination", {
      (tuple: (Game, BoardMove), side: Side.Value, squ: Int) =>
        tuple match {
          case (_, BoardMove(board, normalMove @ NormalMove(_, _, _, _, _), nextBoard)) =>
            board.side == side && normalMove.to == squ &&
            Mobility.mobility(nextBoard, board.side) > Mobility.mobility(board, board.side)
          case _ =>
            false
        }
    })

  /** A named function of move destination of equal mobility for the win board miner and the loss
    * board miner.
    */
  val equalMobilityMoveDestination = NamedFunction3("= mobility move destination", {
      (tuple: (Game, BoardMove), side: Side.Value, squ: Int) =>
        tuple match {
          case (_, BoardMove(board, normalMove @ NormalMove(_, _, _, _, _), nextBoard)) =>
            board.side == side && normalMove.to == squ &&
            Mobility.mobility(nextBoard, board.side) == Mobility.mobility(board, board.side)
          case _ =>
            false
        }
    })

  /** A named function of move destination of less mobility for the win board miner and the loss
    * board miner.
    */
  val lessMobilityMoveDestination = NamedFunction3("< mobility move destination", {
      (tuple: (Game, BoardMove), side: Side.Value, squ: Int) =>
        tuple match {
          case (_, BoardMove(board, normalMove @ NormalMove(_, _, _, _, _), nextBoard)) =>
            board.side == side && normalMove.to == squ &&
            Mobility.mobility(nextBoard, board.side) < Mobility.mobility(board, board.side)
          case _ =>
            false
        }
    })

  /** Returns a named function of move source of greater side mobility for the draw board miner and
    * the count board miner.
    *
    * @param side the side.
    * @return a named function.
    */
  def greaterSideMobilityMoveSource(side: Side.Value) =
    NamedFunction2("> " + sideToName(side) + " mobility move source", {
      (tuple: (Game, BoardMove), squ: Int) =>
        tuple match {
          case (_, BoardMove(board, normalMove @ NormalMove(_, _, _, _, _), nextBoard)) =>
            board.side == side && normalMove.from == squ &&
            Mobility.mobility(nextBoard, board.side) > Mobility.mobility(board, board.side)
          case _ =>
            false
        }
    })

  /** Returns a named function of move source of equal side mobility for the draw board miner and the
    * count board miner.
    *
    * @param side the side.
    * @return a named function.
    */
  def equalSideMobilityMoveSource(side: Side.Value) =
    NamedFunction2("= " + sideToName(side) + " mobility move source", {
      (tuple: (Game, BoardMove), squ: Int) =>
        tuple match {
          case (_, BoardMove(board, normalMove @ NormalMove(_, _, _, _, _), nextBoard)) =>
            board.side == side && normalMove.from == squ &&
            Mobility.mobility(nextBoard, board.side) == Mobility.mobility(board, board.side)
          case _ =>
            false
        }
    })

  /** Returns a named function of move source of less side mobility for the draw board miner and the
    * count board miner.
    *
    * @param side the side.
    * @return a named function.
    */
  def lessSideMobilityMoveSource(side: Side.Value) =
    NamedFunction2("< " + sideToName(side) + " mobility move source", {
      (tuple: (Game, BoardMove), squ: Int) =>
        tuple match {
          case (_, BoardMove(board, normalMove @ NormalMove(_, _, _, _, _), nextBoard)) =>
            board.side == side && normalMove.from == squ &&
            Mobility.mobility(nextBoard, board.side) < Mobility.mobility(board, board.side)
          case _ =>
            false
        }
    })

  /** Returns a named function of move destination of greater side mobility for the draw board miner
    * and the count board miner.
    *
    * @param side the side.
    * @return a named function.
    */
  def greaterSideMobilityMoveDestination(side: Side.Value) =
    NamedFunction2("> " + sideToName(side) + " mobility move destination", {
      (tuple: (Game, BoardMove), squ: Int) =>
        tuple match {
          case (_, BoardMove(board, normalMove @ NormalMove(_, _, _, _, _), nextBoard)) =>
            board.side == side && normalMove.to == squ &&
            Mobility.mobility(nextBoard, board.side) > Mobility.mobility(board, board.side)
          case _ =>
            false
        }
    })

  /** Returns a named function of move destination of equal side mobility for the draw board miner
    * and the count board miner.
    *
    * @param side the side.
    * @return a named function.
    */
  def equalSideMobilityMoveDestination(side: Side.Value) =
    NamedFunction2("= " + sideToName(side) + " mobility move destination", {
      (tuple: (Game, BoardMove), squ: Int) =>
        tuple match {
          case (_, BoardMove(board, normalMove @ NormalMove(_, _, _, _, _), nextBoard)) =>
            board.side == side && normalMove.to == squ &&
            Mobility.mobility(nextBoard, board.side) == Mobility.mobility(board, board.side)
          case _ =>
            false
        }
    })

  /** Returns a named function of move destination of less side mobility for the draw board miner
    * and the count board miner.
    *
    * @param side the side.
    * @return a named function.
    */
  def lessSideMobilityMoveDestination(side: Side.Value) =
    NamedFunction2("< " + sideToName(side) + " mobility move destination", {
      (tuple: (Game, BoardMove), squ: Int) =>
        tuple match {
          case (_, BoardMove(board, normalMove @ NormalMove(_, _, _, _, _), nextBoard)) =>
            board.side == side && normalMove.to == squ &&
            Mobility.mobility(nextBoard, board.side) < Mobility.mobility(board, board.side)
          case _ =>
            false
        }
    })

  /** A named function of move source of greater white mobility for the draw board miner and the
    * count board miner.
    */
  val greaterWhiteMobilityMoveSource = greaterSideMobilityMoveSource(Side.White)
  /** A named function of move source of greater black mobility for the draw board miner and the
    * count board miner.
    */
  val greaterBlackMobilityMoveSource = greaterSideMobilityMoveSource(Side.Black)
  /** A named function of move source of equal white mobility for the draw board miner and the count
    * board miner.
    */
  val equalWhiteMobilityMoveSource = equalSideMobilityMoveSource(Side.White)
  /** A named function of move source of equal black mobility for the draw board miner and the count
    * board miner.
    */
  val equalBlackMobilityMoveSource = equalSideMobilityMoveSource(Side.Black)
  /** A named function of move source of less white mobility for the draw board miner and the count
    * board miner.
    */
  val lessWhiteMobilityMoveSource = lessSideMobilityMoveSource(Side.White)
  /** A named function of move source of less black mobility for the draw board miner and the count
    * board miner.
    */
  val lessBlackMobilityMoveSource = lessSideMobilityMoveSource(Side.Black)
  /** A named function of move destination of greater white mobility for the draw board miner and the
    * count board miner.
    */
  val greaterWhiteMobilityMoveDestination = greaterSideMobilityMoveDestination(Side.White)
  /** A named function of move destination of greater black mobility for the draw board miner and the
    * count board miner.
    */
  val greaterBlackMobilityMoveDestination = greaterSideMobilityMoveDestination(Side.Black)
  /** A named function of move destination of equal white mobility for the draw board miner and the
    * count board miner.
    */
  val equalWhiteMobilityMoveDestination = equalSideMobilityMoveDestination(Side.White)
  /** A named function of move destination of equal black mobility for the draw board miner and the
    * count board miner.
    */
  val equalBlackMobilityMoveDestination = equalSideMobilityMoveDestination(Side.Black)
  /** A named function of move destination of less white mobility for the draw board miner and the
    * count board miner.
    */
  val lessWhiteMobilityMoveDestination = lessSideMobilityMoveDestination(Side.White)
  /** A named function of move destination of less black mobility for the draw board miner and the
    * count board miner.
    */
  val lessBlackMobilityMoveDestination = lessSideMobilityMoveDestination(Side.Black)

  //
  // Named functions of space.
  //

  /** A named function of greater space for the win miner and the loss miner. */
  val greaterSpace = NamedFunction2("> space", {
      (tuple: (Game, Board), side: Side.Value) =>
        tuple match {
          case (_, board) => Space.space(board, side) > Space.space(board, ~side)
        }
    })

  /** A named function of equal space for the win miner and the loss miner. */
  val equalSpace = NamedFunction2("= space", {
      (tuple: (Game, Board), side: Side.Value) =>
        tuple match {
          case (_, board) => Space.space(board, side) == Space.space(board, ~side)
        }
    })

  /** A named function of less space for the win miner and the loss miner. */
  val lessSpace = NamedFunction2("< space", {
      (tuple: (Game, Board), side: Side.Value) =>
        tuple match {
          case (_, board) => Space.space(board, side) < Space.space(board, ~side)
        }
    })

  /** Returns a named function of greater side space for the draw miner and the count miner.
    *
    * @param side the side.
    * @return a named function.
    */
  def greaterSideSpace(side: Side.Value) =
    NamedFunction1("> " + sideToName(side) + " space", {
      (tuple: (Game, Board)) =>
        tuple match {
          case (_, board) => Space.space(board, side) > Space.space(board, ~side)
        }
    })

  /** Returns a named function of equal side space for the draw miner and the count miner.
    *
    * @param side the side.
    * @return a named function.
    */
  def equalSideSpace(side: Side.Value) =
    NamedFunction1("= " + sideToName(side) + " space", {
      (tuple: (Game, Board)) =>
        tuple match {
          case (_, board) => Space.space(board, side) == Space.space(board, ~side)
        }
    })

  /** Returns a named function of less side space for the draw miner and the count miner.
    *
    * @param side the side.
    * @return a named function.
    */
  def lessSideSpace(side: Side.Value) =
    NamedFunction1("< " + sideToName(side) + " space", {
      (tuple: (Game, Board)) =>
        tuple match {
          case (_, board) => Space.space(board, side) < Space.space(board, ~side)
        }
    })

  /** A named function of greater white space for the draw miner and the count miner. */
  val greaterWhiteSpace = greaterSideSpace(Side.White)
  /** A named function of greater black space for the draw miner and the count miner. */
  val greaterBlackSpace = greaterSideSpace(Side.Black)
  /** A named function of equal white space for the draw miner and the count miner. */
  val equalWhiteSpace = equalSideSpace(Side.White)
  /** A named function of equal black space for the draw miner and the count miner. */
  val equalBlackSpace = equalSideSpace(Side.Black)
  /** A named function of less white space for the draw miner and the count miner. */
  val lessWhiteSpace = lessSideSpace(Side.White)
  /** A named function of less black space for the draw miner and the count miner. */
  val lessBlackSpace = lessSideSpace(Side.Black)

  /** A named function of move of greater space for the win miner and the loss miner. */
  val greaterSpaceMove = NamedFunction2("> space move", {
      (tuple: (Game, BoardMove), side: Side.Value) =>
        tuple match {
          case (_, BoardMove(board, _, nextBoard)) =>
            board.side == side &&
            Space.space(nextBoard, board.side) > Space.space(board, board.side)
        }
    })

  /** A named function of move of equal space for the win miner and the loss miner. */
  val equalSpaceMove = NamedFunction2("= space move", {
      (tuple: (Game, BoardMove), side: Side.Value) =>
        tuple match {
          case (_, BoardMove(board, _, nextBoard)) =>
            board.side == side &&
            Space.space(nextBoard, board.side) == Space.space(board, board.side)
        }
    })

  /** A named function of move of less space for the win miner and the loss miner. */
  val lessSpaceMove = NamedFunction2("< space move", {
      (tuple: (Game, BoardMove), side: Side.Value) =>
        tuple match {
          case (_, BoardMove(board, _, nextBoard)) =>
            board.side == side &&
            Space.space(nextBoard, board.side) < Space.space(board, board.side)
        }
    })
    
  /** Returns a named function of move of greater side space for the draw miner and the count miner.
    *
    * @param side the side.
    * @return a named function.
    */
  def greaterSideSpaceMove(side: Side.Value) =
    NamedFunction1("> " + sideToName(side) + " space move", {
      (tuple: (Game, BoardMove)) =>
        tuple match {
          case (_, BoardMove(board, _, nextBoard)) =>
            board.side == side &&
            Space.space(nextBoard, board.side) > Space.space(board, board.side)
        }
    })

  /** Returns a named function of move of equal side space for the draw miner and the count miner.
    *
    * @param side the side.
    * @return a named function.
    */
  def equalSideSpaceMove(side: Side.Value) =
    NamedFunction1("= " + sideToName(side) + " space move", {
      (tuple: (Game, BoardMove)) =>
        tuple match {
          case (_, BoardMove(board, _, nextBoard)) =>
            board.side == side &&
            Space.space(nextBoard, board.side) == Space.space(board, board.side)
        }
    })

  /** Returns a named function of move of less side space for the draw miner and the count miner.
    *
    * @param side the side.
    * @return a named function.
    */
  def lessSideSpaceMove(side: Side.Value) =
    NamedFunction1("< " + sideToName(side) + " space move", {
      (tuple: (Game, BoardMove)) =>
        tuple match {
          case (_, BoardMove(board, _, nextBoard)) =>
            board.side == side &&
            Space.space(nextBoard, board.side) < Space.space(board, board.side)
        }
    })
    
  /** A named function of move of greater white space for the draw miner and the count miner. */
  val greaterWhiteSpaceMove = greaterSideSpaceMove(Side.White)
  /** A named function of move of greater black space for the draw miner and the count miner. */
  val greaterBlackSpaceMove = greaterSideSpaceMove(Side.Black)
  /** A named function of move of equal white space for the draw miner and the count miner. */
  val equalWhiteSpaceMove = equalSideSpaceMove(Side.White)
  /** A named function of move of equal black space for the draw miner and the count miner. */
  val equalBlackSpaceMove = equalSideSpaceMove(Side.Black)
  /** A named function of move of less white space for the draw miner and the count miner. */
  val lessWhiteSpaceMove = lessSideSpaceMove(Side.White)
  /** A named function of move of less black space for the draw miner and the count miner. */
  val lessBlackSpaceMove = lessSideSpaceMove(Side.Black)

  /** A named function of move source of greater space for the win board miner and the loss board
    * miner.
    */
  val greaterSpaceMoveSource = NamedFunction3("> space move source", {
      (tuple: (Game, BoardMove), side: Side.Value, squ: Int) =>
        tuple match {
          case (_, BoardMove(board, normalMove @ NormalMove(_, _, _, _, _), nextBoard)) =>
            board.side == side && normalMove.from == squ &&
            Space.space(nextBoard, board.side) > Space.space(board, board.side)
          case _ =>
            false
        }
    })

  /** A named function of move source of equal space for the win board miner and the loss board
    * miner.
    */
  val equalSpaceMoveSource = NamedFunction3("= space move source", {
      (tuple: (Game, BoardMove), side: Side.Value, squ: Int) =>
        tuple match {
          case (_, BoardMove(board, normalMove @ NormalMove(_, _, _, _, _), nextBoard)) =>
            board.side == side && normalMove.from == squ &&
            Space.space(nextBoard, board.side) == Space.space(board, board.side)
          case _ =>
            false
        }
    })

  /** A named function of move source of less space for the win board miner and the loss board miner. */
  val lessSpaceMoveSource = NamedFunction3("< space move source", {
      (tuple: (Game, BoardMove), side: Side.Value, squ: Int) =>
        tuple match {
          case (_, BoardMove(board, normalMove @ NormalMove(_, _, _, _, _), nextBoard)) =>
            board.side == side && normalMove.from == squ &&
            Space.space(nextBoard, board.side) < Space.space(board, board.side)
          case _ =>
            false
        }
    })

  /** A named function of move destination of greater space for the win board miner and the loss
    * board miner.
    */
  val greaterSpaceMoveDestination = NamedFunction3("> space move destination", {
      (tuple: (Game, BoardMove), side: Side.Value, squ: Int) =>
        tuple match {
          case (_, BoardMove(board, normalMove @ NormalMove(_, _, _, _, _), nextBoard)) =>
            board.side == side && normalMove.to == squ &&
            Space.space(nextBoard, board.side) > Space.space(board, board.side)
          case _ =>
            false
        }
    })

  /** A named function of move destination of equal space for the win board miner and the loss board
    * miner.
    */
  val equalSpaceMoveDestination = NamedFunction3("= space move destination", {
      (tuple: (Game, BoardMove), side: Side.Value, squ: Int) =>
        tuple match {
          case (_, BoardMove(board, normalMove @ NormalMove(_, _, _, _, _), nextBoard)) =>
            board.side == side && normalMove.to == squ &&
            Space.space(nextBoard, board.side) == Space.space(board, board.side)
          case _ =>
            false
        }
    })

  /** A named function of move destination of less space for the win board miner and the loss board
    * miner.
    */
  val lessSpaceMoveDestination = NamedFunction3("< space move destination", {
      (tuple: (Game, BoardMove), side: Side.Value, squ: Int) =>
        tuple match {
          case (_, BoardMove(board, normalMove @ NormalMove(_, _, _, _, _), nextBoard)) =>
            board.side == side && normalMove.to == squ &&
            Space.space(nextBoard, board.side) < Space.space(board, board.side)
          case _ =>
            false
        }
    })

  /** Returns a named function of move source of greater side mobility for the draw board miner and
    * the count board miner.
    *
    * @param side the side.
    * @return a named function.
    */
  def greaterSideSpaceMoveSource(side: Side.Value) =
    NamedFunction2("> " + sideToName(side) + " space move source", {
      (tuple: (Game, BoardMove), squ: Int) =>
        tuple match {
          case (_, BoardMove(board, normalMove @ NormalMove(_, _, _, _, _), nextBoard)) =>
            board.side == side && normalMove.from == squ &&
            Space.space(nextBoard, board.side) > Space.space(board, board.side)
          case _ =>
            false
        }
    })

  /** Returns a named function of move source of equal side mobility for the draw board miner and the
    * count board miner.
    *
    * @param side the side.
    * @return a named function.
    */
  def equalSideSpaceMoveSource(side: Side.Value) =
    NamedFunction2("= " + sideToName(side) + " space move source", {
      (tuple: (Game, BoardMove), squ: Int) =>
        tuple match {
          case (_, BoardMove(board, normalMove @ NormalMove(_, _, _, _, _), nextBoard)) =>
            board.side == side && normalMove.from == squ &&
            Space.space(nextBoard, board.side) == Space.space(board, board.side)
          case _ =>
            false
        }
    })

  /** Returns a named function of move source of less side mobility for the draw board miner and the
    * count board miner.
    *
    * @param side the side.
    * @return a named function.
    */
  def lessSideSpaceMoveSource(side: Side.Value) =
    NamedFunction2("< " + sideToName(side) + " space move source", {
      (tuple: (Game, BoardMove), squ: Int) =>
        tuple match {
          case (_, BoardMove(board, normalMove @ NormalMove(_, _, _, _, _), nextBoard)) =>
            board.side == side && normalMove.from == squ &&
            Space.space(nextBoard, board.side) < Space.space(board, board.side)
          case _ =>
            false
        }
    })

  /** Returns a named function of move destination of greater side mobility for the draw board miner
    * and the count board miner.
    *
    * @param side the side.
    * @return a named function.
    */
  def greaterSideSpaceMoveDestination(side: Side.Value) =
    NamedFunction2("> " + sideToName(side) + " space move destination", {
      (tuple: (Game, BoardMove), squ: Int) =>
        tuple match {
          case (_, BoardMove(board, normalMove @ NormalMove(_, _, _, _, _), nextBoard)) =>
            board.side == side && normalMove.to == squ &&
            Space.space(nextBoard, board.side) > Space.space(board, board.side)
          case _ =>
            false
        }
    })

  /** Returns a named function of move destination of equal side mobility for the draw board miner
    * and the count board miner.
    *
    * @param side the side.
    * @return a named function.
    */
  def equalSideSpaceMoveDestination(side: Side.Value) =
    NamedFunction2("= " + sideToName(side) + " space move destination", {
      (tuple: (Game, BoardMove), squ: Int) =>
        tuple match {
          case (_, BoardMove(board, normalMove @ NormalMove(_, _, _, _, _), nextBoard)) =>
            board.side == side && normalMove.to == squ &&
            Space.space(nextBoard, board.side) == Space.space(board, board.side)
          case _ =>
            false
        }
    })

  /** Returns a named function of move destination of less side mobility for the draw board miner and
    * the count board miner.
    *
    * @param side the side.
    * @return a named function.
    */
  def lessSideSpaceMoveDestination(side: Side.Value) =
    NamedFunction2("< " + sideToName(side) + " space move destination", {
      (tuple: (Game, BoardMove), squ: Int) =>
        tuple match {
          case (_, BoardMove(board, normalMove @ NormalMove(_, _, _, _, _), nextBoard)) =>
            board.side == side && normalMove.to == squ &&
            Space.space(nextBoard, board.side) < Space.space(board, board.side)
          case _ =>
            false
        }
    })

  /** A named function of move source of greater white space for the draw board miner and the count
    * board miner.
    */
  val greaterWhiteSpaceMoveSource = greaterSideSpaceMoveSource(Side.White)
  /** A named function of move source of greater black space for the draw board miner and the count
    * board miner.
    */
  val greaterBlackSpaceMoveSource = greaterSideSpaceMoveSource(Side.Black)
  /** A named function of move source of equal white space for the draw board miner and the count
    * board miner.
    */
  val equalWhiteSpaceMoveSource = equalSideSpaceMoveSource(Side.White)
  /** A named function of move source of equal black space for the draw board miner and the count
    * board miner.
    */
  val equalBlackSpaceMoveSource = equalSideSpaceMoveSource(Side.Black)
  /** A named function of move source of less white space for the draw board miner and the count
    * board miner.
    */
  val lessWhiteSpaceMoveSource = lessSideSpaceMoveSource(Side.White)
  /** A named function of move source of less black space for the draw board miner and the count
    * board miner.
    */
  val lessBlackSpaceMoveSource = lessSideSpaceMoveSource(Side.Black)
  /** A named function of move destination of greater white space for the draw board miner and the 
    * count board miner.
    */
  val greaterWhiteSpaceMoveDestination = greaterSideSpaceMoveDestination(Side.White)
  /** A named function of move destination of greater black space for the draw board miner and the 
    * count board miner.
    */
  val greaterBlackSpaceMoveDestination = greaterSideSpaceMoveDestination(Side.Black)
  /** A named function of move destination of equal white space for the draw board miner and the 
    * count board miner.
    */
  val equalWhiteSpaceMoveDestination = equalSideSpaceMoveDestination(Side.White)
  /** A named function of move destination of equal black space for the draw board miner and the 
    * count board miner.
    */
  val equalBlackSpaceMoveDestination = equalSideSpaceMoveDestination(Side.Black)
  /** A named function of move destination of less white space for the draw board miner and the
    * count board miner.
    */
  val lessWhiteSpaceMoveDestination = lessSideSpaceMoveDestination(Side.White)
  /** A named function of move destination of less black space for the draw board miner and the 
    * count board miner.
    */
  val lessBlackSpaceMoveDestination = lessSideSpaceMoveDestination(Side.Black)
  
  //
  // Named functions of attack.
  //
  
  /** A named function of attack for the win board miner and the loss board miner. */
  val attack = NamedFunction3("attack", {
      (tuple: (Game, Board), side: Side.Value, squ: Int) =>
        tuple match {
          case (_, board) => board.hasAttack(side, squ) 
        }
    })

  /** Returns a named function of side attack for the draw board miner and the count board miner.
    *
    * @param side the side.
    * @return a named function.
    */
  def sideAttack(side: Side.Value) =
    NamedFunction2(sideToName(side) + " attack", {
      (tuple: (Game, Board), squ: Int) =>
        tuple match {
          case (_, board) => board.hasAttack(side, squ)
        }
    })

  /** A named function of white attack for the draw board miner and the count board miner. */
  val whiteAttack = sideAttack(Side.White)
  /** A named function of black attack for the draw board miner and the count board miner. */
  val blackAttack = sideAttack(Side.Black)

  /** A named function of opposite attack for the win board miner and the loss board miner. */
  val oppositeAttack = NamedFunction3("opposite attack", {
      (tuple: (Game, Board), side: Side.Value, squ: Int) =>
        tuple match {
          case (_, board) => board.hasAttack(~side, squ) 
        }
    })

  /** Returns a named function of side opposite attack for the draw board miner and the count board
    * miner.
    *
    * @param side the side.
    * @return a named function.
    */
  def sideOppositeAttack(side: Side.Value) =
    NamedFunction2(sideToName(side) + " opposite attack", {
      (tuple: (Game, Board), squ: Int) =>
        tuple match {
          case (_, board) => board.hasAttack(~side, squ)
        }
    })

  /** A named function of white opposite attack for the draw board miner and the count board miner. */
  val whiteOppositeAttack = sideOppositeAttack(Side.White)
  /** A named function of black opposite attack for the draw board miner and the count board miner. */
  val blackOppositeAttack = sideOppositeAttack(Side.Black)

  //
  // Named functions of open line.
  //
  
  /** A named function of open line for the win board miner and the loss board miner. */
  val openLine = NamedFunction3("open line", {
      (tuple: (Game, Board), side: Side.Value, squ: Int) =>
        tuple match {
          case (_, board) => OpenLine.isOpenLine(board, side, squ) 
        }
    })

  /** A named function of semi-open line for the win board miner and the loss board miner. */
  val semiOpenLine = NamedFunction3("semi-open line", {
      (tuple: (Game, Board), side: Side.Value, squ: Int) =>
        tuple match {
          case (_, board) => OpenLine.isSemiOpenLine(board, side, squ)
        }
    })

  /** Returns a named function of side open line for the draw board miner and the count board miner.
    *
    * @param side the side.
    * @return a named function.
    */
  def sideOpenLine(side: Side.Value) =
    NamedFunction2(sideToName(side) + " open line", {
      (tuple: (Game, Board), squ: Int) =>
        tuple match {
          case (_, board) => OpenLine.isOpenLine(board, side, squ)
        }
    })

  /** Returns a named function of side semi-open line for the draw board miner and the count board
    * miner.
    *
    * @param side the side.
    * @return a named function.
    */
  def sideSemiOpenLine(side: Side.Value) =
    NamedFunction2(sideToName(side) + " semi-open line", {
      (tuple: (Game, Board), squ: Int) =>
        tuple match {
          case (_, board) => OpenLine.isSemiOpenLine(board, side, squ)
        }
    })
    
  /** A named function of white open line for the draw board miner and the count board miner. */
  val whiteOpenLine = sideOpenLine(Side.White)
  /** A named function of black open line for the draw board miner and the count board miner. */
  val blackOpenLine = sideOpenLine(Side.Black)
  /** A named function of white semi-open line for the draw board miner and the count board miner. */
  val whiteSemiOpenLine = sideSemiOpenLine(Side.White)
  /** A named function of black semi-open line for the draw board miner and the count board miner. */
  val blackSemiOpenLine = sideSemiOpenLine(Side.Black)
  
  //
  // Named functions of king zone.
  //

  val greaterKingZone = NamedFunction2("> king zone", {
      (tuple: (Game, Board), side: Side.Value) =>
        tuple match {
          case (_, board) => KingZone.kingZone(board, side) > KingZone.kingZone(board, ~side)
        }
    })

  val equalKingZone = NamedFunction2("= king zone", {
      (tuple: (Game, Board), side: Side.Value) =>
        tuple match {
          case (_, board) => KingZone.kingZone(board, side) == KingZone.kingZone(board, ~side)
        }
    })

  val lessKingZone = NamedFunction2("< king zone", {
      (tuple: (Game, Board), side: Side.Value) =>
        tuple match {
          case (_, board) => KingZone.kingZone(board, side) < KingZone.kingZone(board, ~side)
        }
    })

  def greaterSideKingZone(side: Side.Value) =
    NamedFunction1("> " + sideToName(side) + " king zone", {
      (tuple: (Game, Board)) =>
        tuple match {
          case (_, board) => KingZone.kingZone(board, side) > KingZone.kingZone(board, ~side)
        }
    })

  def equalSideKingZone(side: Side.Value) =
    NamedFunction1("= " + sideToName(side) + " king zone", {
      (tuple: (Game, Board)) =>
        tuple match {
          case (_, board) => KingZone.kingZone(board, side) == KingZone.kingZone(board, ~side)
        }
    })

  def lessSideKingZone(side: Side.Value) =
    NamedFunction1("< " + sideToName(side) + " king zone", {
      (tuple: (Game, Board)) =>
        tuple match {
          case (_, board) => KingZone.kingZone(board, side) < KingZone.kingZone(board, ~side)
        }
    })

  val greaterWhiteKingZone = greaterSideKingZone(Side.White)
  val greaterBlackKingZone = greaterSideKingZone(Side.Black)
  val equalWhiteKingZone = equalSideKingZone(Side.White)
  val equalBlackKingZone = equalSideKingZone(Side.Black)
  val lessWhiteKingZone = lessSideKingZone(Side.White)
  val lessBlackKingZone = lessSideKingZone(Side.Black)
}
