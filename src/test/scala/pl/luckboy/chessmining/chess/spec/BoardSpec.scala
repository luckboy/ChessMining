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
package pl.luckboy.chessmining.chess.spec
import org.scalatest._
import org.scalatest.flatspec._
import org.scalatest.matchers._
import pl.luckboy.chessmining.chess._
import Squares._

class BoardSpec extends AnyFlatSpec with should.Matchers with Inside
{
  "A Board.Initial" should "be the initial board" in {
    val board = Board.Initial
    board.coloredPiece(A8) should be (ColoredPiece.BlackRook)
    board.coloredPiece(B8) should be (ColoredPiece.BlackKnight)
    board.coloredPiece(C8) should be (ColoredPiece.BlackBishop)
    board.coloredPiece(D8) should be (ColoredPiece.BlackQueen)
    board.coloredPiece(E8) should be (ColoredPiece.BlackKing)
    board.coloredPiece(F8) should be (ColoredPiece.BlackBishop)
    board.coloredPiece(G8) should be (ColoredPiece.BlackKnight)
    board.coloredPiece(H8) should be (ColoredPiece.BlackRook)
    board.coloredPiece(A7) should be (ColoredPiece.BlackPawn)
    board.coloredPiece(B7) should be (ColoredPiece.BlackPawn)
    board.coloredPiece(C7) should be (ColoredPiece.BlackPawn)
    board.coloredPiece(D7) should be (ColoredPiece.BlackPawn)
    board.coloredPiece(E7) should be (ColoredPiece.BlackPawn)
    board.coloredPiece(F7) should be (ColoredPiece.BlackPawn)
    board.coloredPiece(G7) should be (ColoredPiece.BlackPawn)
    board.coloredPiece(H7) should be (ColoredPiece.BlackPawn)
    board.coloredPiece(A6) should be (ColoredPiece.Empty)
    board.coloredPiece(B6) should be (ColoredPiece.Empty)
    board.coloredPiece(C6) should be (ColoredPiece.Empty)
    board.coloredPiece(D6) should be (ColoredPiece.Empty)
    board.coloredPiece(E6) should be (ColoredPiece.Empty)
    board.coloredPiece(F6) should be (ColoredPiece.Empty)
    board.coloredPiece(G6) should be (ColoredPiece.Empty)
    board.coloredPiece(H6) should be (ColoredPiece.Empty)
    board.coloredPiece(A5) should be (ColoredPiece.Empty)
    board.coloredPiece(B5) should be (ColoredPiece.Empty)
    board.coloredPiece(C5) should be (ColoredPiece.Empty)
    board.coloredPiece(D5) should be (ColoredPiece.Empty)
    board.coloredPiece(E5) should be (ColoredPiece.Empty)
    board.coloredPiece(F5) should be (ColoredPiece.Empty)
    board.coloredPiece(G5) should be (ColoredPiece.Empty)
    board.coloredPiece(H5) should be (ColoredPiece.Empty)
    board.coloredPiece(A4) should be (ColoredPiece.Empty)
    board.coloredPiece(B4) should be (ColoredPiece.Empty)
    board.coloredPiece(C4) should be (ColoredPiece.Empty)
    board.coloredPiece(D4) should be (ColoredPiece.Empty)
    board.coloredPiece(E4) should be (ColoredPiece.Empty)
    board.coloredPiece(F4) should be (ColoredPiece.Empty)
    board.coloredPiece(G4) should be (ColoredPiece.Empty)
    board.coloredPiece(H4) should be (ColoredPiece.Empty)
    board.coloredPiece(A3) should be (ColoredPiece.Empty)
    board.coloredPiece(B3) should be (ColoredPiece.Empty)
    board.coloredPiece(C3) should be (ColoredPiece.Empty)
    board.coloredPiece(D3) should be (ColoredPiece.Empty)
    board.coloredPiece(E3) should be (ColoredPiece.Empty)
    board.coloredPiece(F3) should be (ColoredPiece.Empty)
    board.coloredPiece(G3) should be (ColoredPiece.Empty)
    board.coloredPiece(H3) should be (ColoredPiece.Empty)
    board.coloredPiece(A2) should be (ColoredPiece.WhitePawn)
    board.coloredPiece(B2) should be (ColoredPiece.WhitePawn)
    board.coloredPiece(C2) should be (ColoredPiece.WhitePawn)
    board.coloredPiece(D2) should be (ColoredPiece.WhitePawn)
    board.coloredPiece(E2) should be (ColoredPiece.WhitePawn)
    board.coloredPiece(F2) should be (ColoredPiece.WhitePawn)
    board.coloredPiece(G2) should be (ColoredPiece.WhitePawn)
    board.coloredPiece(H2) should be (ColoredPiece.WhitePawn)
    board.coloredPiece(A1) should be (ColoredPiece.WhiteRook)
    board.coloredPiece(B1) should be (ColoredPiece.WhiteKnight)
    board.coloredPiece(C1) should be (ColoredPiece.WhiteBishop)
    board.coloredPiece(D1) should be (ColoredPiece.WhiteQueen)
    board.coloredPiece(E1) should be (ColoredPiece.WhiteKing)
    board.coloredPiece(F1) should be (ColoredPiece.WhiteBishop)
    board.coloredPiece(G1) should be (ColoredPiece.WhiteKnight)
    board.coloredPiece(H1) should be (ColoredPiece.WhiteRook)
    board.side should be (Side.White)
    board.sideCastlings(Side.White) should be (SideCastlings.All)
    board.sideCastlings(Side.Black) should be (SideCastlings.All)
    board.enPassantColumnOption should be (None)
    board.halfmoveClock should be (0)
    board.fullmoveNumber should be (1)
  }
  
  "A Board.parseBoard" should "set the pieces" in {
    val boardOpt = Board.parseBoard("rnbqk2r/pppp1ppp/3b1n2/4p3/2B1P3/5N2/PPPP1PPP/RNBQK2R w KQkq - 0 4")
    inside(boardOpt) {
      case Some(board) =>
        board.coloredPiece(A8) should be (ColoredPiece.BlackRook)
        board.coloredPiece(B8) should be (ColoredPiece.BlackKnight)
        board.coloredPiece(C8) should be (ColoredPiece.BlackBishop)
        board.coloredPiece(D8) should be (ColoredPiece.BlackQueen)
        board.coloredPiece(E8) should be (ColoredPiece.BlackKing)
        board.coloredPiece(F8) should be (ColoredPiece.Empty)
        board.coloredPiece(G8) should be (ColoredPiece.Empty)
        board.coloredPiece(H8) should be (ColoredPiece.BlackRook)
        board.coloredPiece(A7) should be (ColoredPiece.BlackPawn)
        board.coloredPiece(B7) should be (ColoredPiece.BlackPawn)
        board.coloredPiece(C7) should be (ColoredPiece.BlackPawn)
        board.coloredPiece(D7) should be (ColoredPiece.BlackPawn)
        board.coloredPiece(E7) should be (ColoredPiece.Empty)
        board.coloredPiece(F7) should be (ColoredPiece.BlackPawn)
        board.coloredPiece(G7) should be (ColoredPiece.BlackPawn)
        board.coloredPiece(H7) should be (ColoredPiece.BlackPawn)
        board.coloredPiece(A6) should be (ColoredPiece.Empty)
        board.coloredPiece(B6) should be (ColoredPiece.Empty)
        board.coloredPiece(C6) should be (ColoredPiece.Empty)
        board.coloredPiece(D6) should be (ColoredPiece.BlackBishop)
        board.coloredPiece(E6) should be (ColoredPiece.Empty)
        board.coloredPiece(F6) should be (ColoredPiece.BlackKnight)
        board.coloredPiece(G6) should be (ColoredPiece.Empty)
        board.coloredPiece(H6) should be (ColoredPiece.Empty)
        board.coloredPiece(A5) should be (ColoredPiece.Empty)
        board.coloredPiece(B5) should be (ColoredPiece.Empty)
        board.coloredPiece(C5) should be (ColoredPiece.Empty)
        board.coloredPiece(D5) should be (ColoredPiece.Empty)
        board.coloredPiece(E5) should be (ColoredPiece.BlackPawn)
        board.coloredPiece(F5) should be (ColoredPiece.Empty)
        board.coloredPiece(G5) should be (ColoredPiece.Empty)
        board.coloredPiece(H5) should be (ColoredPiece.Empty)
        board.coloredPiece(A4) should be (ColoredPiece.Empty)
        board.coloredPiece(B4) should be (ColoredPiece.Empty)
        board.coloredPiece(C4) should be (ColoredPiece.WhiteBishop)
        board.coloredPiece(D4) should be (ColoredPiece.Empty)
        board.coloredPiece(E4) should be (ColoredPiece.WhitePawn)
        board.coloredPiece(F4) should be (ColoredPiece.Empty)
        board.coloredPiece(G4) should be (ColoredPiece.Empty)
        board.coloredPiece(H4) should be (ColoredPiece.Empty)
        board.coloredPiece(A3) should be (ColoredPiece.Empty)
        board.coloredPiece(B3) should be (ColoredPiece.Empty)
        board.coloredPiece(C3) should be (ColoredPiece.Empty)
        board.coloredPiece(D3) should be (ColoredPiece.Empty)
        board.coloredPiece(E3) should be (ColoredPiece.Empty)
        board.coloredPiece(F3) should be (ColoredPiece.WhiteKnight)
        board.coloredPiece(G3) should be (ColoredPiece.Empty)
        board.coloredPiece(H3) should be (ColoredPiece.Empty)
        board.coloredPiece(A2) should be (ColoredPiece.WhitePawn)
        board.coloredPiece(B2) should be (ColoredPiece.WhitePawn)
        board.coloredPiece(C2) should be (ColoredPiece.WhitePawn)
        board.coloredPiece(D2) should be (ColoredPiece.WhitePawn)
        board.coloredPiece(E2) should be (ColoredPiece.Empty)
        board.coloredPiece(F2) should be (ColoredPiece.WhitePawn)
        board.coloredPiece(G2) should be (ColoredPiece.WhitePawn)
        board.coloredPiece(H2) should be (ColoredPiece.WhitePawn)
        board.coloredPiece(A1) should be (ColoredPiece.WhiteRook)
        board.coloredPiece(B1) should be (ColoredPiece.WhiteKnight)
        board.coloredPiece(C1) should be (ColoredPiece.WhiteBishop)
        board.coloredPiece(D1) should be (ColoredPiece.WhiteQueen)
        board.coloredPiece(E1) should be (ColoredPiece.WhiteKing)
        board.coloredPiece(F1) should be (ColoredPiece.Empty)
        board.coloredPiece(G1) should be (ColoredPiece.Empty)
        board.coloredPiece(H1) should be (ColoredPiece.WhiteRook)
    }
  }

  it should "set the side for the white side" in {
    val boardOpt = Board.parseBoard("rnbqkb1r/ppp1pppp/5n2/3p4/3P4/5N2/PPP1PPPP/RNBQKB1R w KQkq - 0 3")
    inside(boardOpt) {
      case Some(board) =>
        board.side should be (Side.White)
    }
  }

  it should "set the side for the black side" in {
    val boardOpt = Board.parseBoard("rnbqkb1r/ppp1pppp/5n2/3p2B1/3P4/5N2/PPP1PPPP/RN1QKB1R b KQkq - 0 3")
    inside(boardOpt) {
      case Some(board) =>
        board.side should be (Side.Black)
    }
  }

  it should "set the castlings for all castlings" in {
    val boardOpt = Board.parseBoard("rnbqkb1r/pppp1ppp/5n2/4p3/3P4/4PN2/PPP2PPP/RNBQKB1R b KQkq - 0 3")
    inside(boardOpt) {
      case Some(board) =>
        board.sideCastlings(Side.White) should be (SideCastlings.All)
        board.sideCastlings(Side.Black) should be (SideCastlings.All)
    }
  }

  it should "set the castlings for some castlings" in {
    val boardOpt = Board.parseBoard("1nbqkbnr/1ppppppp/r7/p7/7P/7R/PPPPPPP1/RNBQKBN1 w Qk - 0 3")
    inside(boardOpt) {
      case Some(board) =>
        board.sideCastlings(Side.White) should be (SideCastlings.Long)
        board.sideCastlings(Side.Black) should be (SideCastlings.Short)
    }
  }

  it should "set the castlings for no castlings" in {
    val boardOpt = Board.parseBoard("6r1/4k1p1/1n6/6N1/8/6P1/4K3/6R1 b - - 0 1")
    inside(boardOpt) {
      case Some(board) =>
        board.sideCastlings(Side.White) should be (SideCastlings.None)
        board.sideCastlings(Side.Black) should be (SideCastlings.None)
    }
  }

  it should "set the enPassantColumnOption for the white side" in {
    val boardOpt = Board.parseBoard("rnbqkbnr/ppp2ppp/4p3/3pP3/8/8/PPPP1PPP/RNBQKBNR w KQkq d6 0 3")
    inside(boardOpt) {
      case Some(board) =>
        board.enPassantColumnOption should be (Some(D1))
    }
  }

  it should "set the enPassantColumnOption for the black side" in {
    val boardOpt = Board.parseBoard("rnbqkbnr/ppppp1pp/8/8/3P1pP1/2N5/PPP1PP1P/R1BQKBNR b KQkq g3 0 3")
    inside(boardOpt) {
      case Some(board) =>
        board.enPassantColumnOption should be (Some(G1))
    }
  }

  it should "set the enPassantColumnOption for the white side and no pawns" in {
    val boardOpt = Board.parseBoard("rnbqkbnr/ppp1pppp/8/3p4/3P4/8/PPP1PPPP/RNBQKBNR w KQkq d6 0 2")
    inside(boardOpt) {
      case Some(board) =>
        board.enPassantColumnOption should be (None)
    }
  }

  it should "set the enPassantColumnOption for the black side and no pawns" in {
    val boardOpt = Board.parseBoard("rnbqkbnr/ppp1pppp/8/3p4/3P1P2/8/PPP1P1PP/RNBQKBNR b KQkq f3 0 2")
    inside(boardOpt) {
      case Some(board) =>
        board.enPassantColumnOption should be (None)
    }
  }

  it should "set the enPassantColumnOption for the white side and no en passant square" in {
    val boardOpt = Board.parseBoard("rnbqkbnr/pppp1pp1/8/4p3/4P1Pp/3P4/PPP2P1P/RNBQKBNR w KQkq - 0 4")
    inside(boardOpt) {
      case Some(board) =>
        board.enPassantColumnOption should be (None)
    }
  }

  it should "set the enPassantColumnOption for the black side and no en passant square" in {
    val boardOpt = Board.parseBoard("rnbqkbnr/pppp1ppp/8/8/3Pp3/2P2P2/PP2P1PP/RNBQKBNR b KQkq - 0 3")
    inside(boardOpt) {
      case Some(board) =>
        board.enPassantColumnOption should be (None)
    }
  }
  
  it should "set the halfmoveClock" in {
    val boardOpt = Board.parseBoard("rnbqkb1r/ppp2ppp/3p1n2/4p3/4P3/3P1N2/PPP2PPP/RNBQKB1R w KQkq - 2 4")
    inside(boardOpt) {
      case Some(board) =>
        board.halfmoveClock should be (2)
    }
  }

  it should "set the fullmoveNumber" in {
    val boardOpt = Board.parseBoard("rnbqkb1r/ppp2ppp/3p1n2/4p3/4P3/2PP1N2/PP3PPP/RNBQKB1R b KQkq - 0 4")
    inside(boardOpt) {
      case Some(board) =>
        board.fullmoveNumber should be (4)
    }
  }
  
  it should "complain on the incorrect pieces" in {
    val boardOpt = Board.parseBoard("rnbqkbnr/pppp1ppp/8/4p3/4P3/8/PPPP1PPP/RXBQKBNR w KQkq - 0 2")
    boardOpt should be (None)
  }

  it should "complain on the incorrect side" in {
    val boardOpt = Board.parseBoard("rnbqkbnr/pppp1ppp/8/4p3/4P3/8/PPPP1PPP/RNBQKBNR x KQkq - 0 2")
    boardOpt should be (None)
  }

  it should "complain on the incorrect castlings" in {
    val boardOpt = Board.parseBoard("rnbqkbnr/pppp1ppp/8/4p3/4P3/8/PPPP1PPP/RNBQKBNR w XX - 0 2")
    boardOpt should be (None)
  }

  it should "complain on the incorrect en passant square" in {
    val boardOpt = Board.parseBoard("rnbqkb1r/pp1p1ppp/5n2/2pPp3/4P3/8/PPP2PPP/RNBQKBNR w KQkq x6 0 4")
    boardOpt should be (None)
  }

  it should "complain on the incorrect halfmove clock" in {
    val boardOpt = Board.parseBoard("rnbqkbnr/pppp1ppp/8/4p3/4P3/8/PPPP1PPP/RNBQKBNR w KQkq - x 2")
    boardOpt should be (None)
  }

  it should "complain on the incorrect fullmove number" in {
    val boardOpt = Board.parseBoard("rnbqkbnr/pppp1ppp/8/4p3/4P3/8/PPPP1PPP/RNBQKBNR w KQkq - 0 X")
    boardOpt should be (None)
  }
  
  it should "complain on the black king that can be captured" in {
    val boardOpt = Board.parseBoard("1k6/8/2N2P2/8/8/8/1p3K2/8 w - - 0 1")
    boardOpt should be (None)
  }

  it should "complain on the white king that can be captured" in {
    val boardOpt = Board.parseBoard("1k6/8/5P2/8/7b/8/1p3K2/8 b - - 0 1")
    boardOpt should be (None)
  }
  
  "A Board.toString" should "convert the board to the string" in {
    val board = Board("r1bqkb1r/pppp1ppp/2n2n2/4p3/4P3/2N2N2/PPPP1PPP/R1BQKB1R w KQkq - 0 4")
    board.toString() should be ("r1bqkb1r/pppp1ppp/2n2n2/4p3/4P3/2N2N2/PPPP1PPP/R1BQKB1R w KQkq - 0 4")
  }
  
  it should "convert the board to the string for no castlings" in {
    val board = Board("4k3/8/1p3n2/2p5/6P1/7P/5B2/4K3 b - - 0 1")
    board.toString() should be ("4k3/8/1p3n2/2p5/6P1/7P/5B2/4K3 b - - 0 1")
  }

  it should "convert the board to the string for the white side and the en passant" in {
    val board = Board("rnbqk1nr/pp1p1ppp/3bp3/1Pp5/4P3/8/P1PP1PPP/RNBQKBNR w KQkq c6 0 4")
    board.toString() should be ("rnbqk1nr/pp1p1ppp/3bp3/1Pp5/4P3/8/P1PP1PPP/RNBQKBNR w KQkq c6 0 4")
  }

  it should "convert the board to the string for the black side and the en passant" in {
    val board = Board("rnbqkbnr/pppp1p1p/8/4p3/4PPp1/3P3N/PPP3PP/RNBQKB1R b KQkq f3 0 4")
    board.toString() should be ("rnbqkbnr/pppp1p1p/8/4p3/4PPp1/3P3N/PPP3PP/RNBQKB1R b KQkq f3 0 4")
  }
  
  "A Board.hasAttack" should "return true for the white side pawn attack" in {
    val board = Board("4k3/8/8/8/4p3/8/8/4K3 b - - 0 1")
    board.hasAttack(Side.White, D3) should be (true)
  }

  it should "return true for the black side pawn attack" in {
    val board = Board("4k3/8/8/1P1p4/8/8/8/4K3 w - - 0 1")
    board.hasAttack(Side.Black, C6) should be (true)
  }

  it should "return true for the knight attack" in {
    val board = Board("4k3/3N4/8/3n4/2n5/4K3/8/8 w - - 0 1")
    board.hasAttack(Side.White, B6) should be (true)
  }

  it should "return true for the bishop attack" in {
    val board = Board("4k3/8/8/1b6/8/8/4K3/8 w - - 0 1")
    board.hasAttack(Side.White, E2) should be (true)
  }

  it should "return true for the rook attack" in {
    val board = Board("8/4k2R/8/8/8/8/8/4K3 b - - 0 1")
    board.hasAttack(Side.Black, E7) should be (true)
  }

  it should "return true for the queen attack as the bishop attack" in {
    val board = Board("8/4k3/8/2q5/8/8/8/4K3 b - - 0 1")
    board.hasAttack(Side.White, E3) should be (true)
  }

  it should "return true for the queen attack as the rook attack" in {
    val board = Board("4k3/8/8/5Q2/8/8/8/3K4 w - - 0 1")
    board.hasAttack(Side.Black, F7) should be (true)
  }
  
  it should "return true for the king attack" in {
    val board = Board("4k3/8/8/8/3K4/8/8/8 w - - 0 1")
    board.hasAttack(Side.Black, E5) should be (true)
  }
  
  it should "return false for the barriers" in {
    val board = Board("4k3/8/8/4r3/1q6/2P1p3/8/4K3 w - - 0 1")
    board.hasAttack(Side.White, E1) should be (false)
  }

  it should "return false for no barriers" in {
    val board = Board("4k3/8/8/5N2/8/8/8/4K3 b - - 0 1")
    board.hasAttack(Side.Black, E8) should be (false)
  }

  it should "return false for no attack piece" in {
    val board = Board("4k3/8/8/8/8/3N4/8/4K3 w - - 0 1")
    board.hasAttack(Side.White, E1) should be (false)
  }
  
  "A Board.generetePseudolegalMoves" should "generate moves for the initial board" in {
    val moves = Board.Initial.generatePseudolegalMoves
    moves should have length (20)
    moves should contain (NormalMove(Piece.Knight, B1, A3, None, false))
    moves should contain (NormalMove(Piece.Knight, B1, C3, None, false))
    moves should contain (NormalMove(Piece.Knight, G1, F3, None, false))
    moves should contain (NormalMove(Piece.Knight, G1, H3, None, false))
    moves should contain (NormalMove(Piece.Pawn, A2, A3, None, false))
    moves should contain (NormalMove(Piece.Pawn, A2, A4, None, false))
    moves should contain (NormalMove(Piece.Pawn, B2, B3, None, false))
    moves should contain (NormalMove(Piece.Pawn, B2, B4, None, false))
    moves should contain (NormalMove(Piece.Pawn, C2, C3, None, false))
    moves should contain (NormalMove(Piece.Pawn, C2, C4, None, false))
    moves should contain (NormalMove(Piece.Pawn, D2, D3, None, false))
    moves should contain (NormalMove(Piece.Pawn, D2, D4, None, false))
    moves should contain (NormalMove(Piece.Pawn, E2, E3, None, false))
    moves should contain (NormalMove(Piece.Pawn, E2, E4, None, false))
    moves should contain (NormalMove(Piece.Pawn, F2, F3, None, false))
    moves should contain (NormalMove(Piece.Pawn, F2, F4, None, false))
    moves should contain (NormalMove(Piece.Pawn, G2, G3, None, false))
    moves should contain (NormalMove(Piece.Pawn, G2, G4, None, false))
    moves should contain (NormalMove(Piece.Pawn, H2, H3, None, false))
    moves should contain (NormalMove(Piece.Pawn, H2, H4, None, false))
  }
  
  it should "generate moves for the white side and the pawns" in {
    val moves = Board("4k3/6p1/1p6/8/8/5P2/2P5/4K3 w - - 0 1").generatePseudolegalMoves
    moves.length should be >= (3)
    moves should contain (NormalMove(Piece.Pawn, C2, C3, None, false))
    moves should contain (NormalMove(Piece.Pawn, C2, C4, None, false))
    moves should contain (NormalMove(Piece.Pawn, F3, F4, None, false))
  }

  it should "generate moves for the black side and the pawns" in {
    val moves = Board("4k3/7p/8/3p4/8/1P2P3/8/4K3 b - - 0 1").generatePseudolegalMoves
    moves.length should be >= (3)
    moves should contain (NormalMove(Piece.Pawn, D5, D4, None, false))
    moves should contain (NormalMove(Piece.Pawn, H7, H6, None, false))
    moves should contain (NormalMove(Piece.Pawn, H7, H5, None, false))
  }
  
  it should "not generate moves for the white side, the pawns and the barriers" in {
    val moves = Board("4k3/8/2p1p3/8/4N3/2n5/2P1P3/4K3 w - - 0 1").generatePseudolegalMoves
    moves should not contain (NormalMove(Piece.Pawn, C2, C3, None, false))
    moves should not contain (NormalMove(Piece.Pawn, C2, C4, None, false))
    moves should not contain (NormalMove(Piece.Pawn, E2, E4, None, false))
  }

  it should "not generate moves for the black side, the pawns and the barriers" in {
    val moves = Board("4k3/2p3p1/6P1/2n5/8/8/4P3/4K3 b - - 0 1").generatePseudolegalMoves
    moves should not contain (NormalMove(Piece.Pawn, C7, C7, None, false))
    moves should not contain (NormalMove(Piece.Pawn, G7, G6, None, false))
    moves should not contain (NormalMove(Piece.Pawn, G7, G7, None, false))
  }

  it should "generate captures for the white side and the pawns" in {
    val moves = Board("4k3/7n/6P1/8/8/3p4/4P3/4K3 w - - 0 1").generatePseudolegalMoves
    moves.length should be >= (2)
    moves should contain (NormalMove(Piece.Pawn, E2, D3, None, true))
    moves should contain (NormalMove(Piece.Pawn, G6, H7, None, true))
  }

  it should "generate captures for the black side and the pawns" in {
    val moves = Board("4k3/8/5p2/6N1/2p5/1P6/8/4K3 b - - 0 1").generatePseudolegalMoves
    moves.length should be >= (2)
    moves should contain (NormalMove(Piece.Pawn, C4, B3, None, true))
    moves should contain (NormalMove(Piece.Pawn, F6, G5, None, true))
  }
  
  it should "not generate captures for the white side and the pawns" in {
    val moves = Board("4k3/8/8/4N3/1N3P2/2P5/8/4K3 w - - 0 1").generatePseudolegalMoves
    moves should not contain (NormalMove(Piece.Pawn, C3, B4, None, true))
    moves should not contain (NormalMove(Piece.Pawn, C3, D4, None, true))
    moves should not contain (NormalMove(Piece.Pawn, F4, E5, None, true))    
    moves should not contain (NormalMove(Piece.Pawn, F4, G5, None, true))    
  }

  it should "not generate captures for the black side and the pawns" in {
    val moves = Board("4k3/8/1p3p2/2b3b1/8/8/8/4K3 b - - 0 1").generatePseudolegalMoves
    moves should not contain (NormalMove(Piece.Pawn, B6, A5, None, true))
    moves should not contain (NormalMove(Piece.Pawn, B6, C5, None, true))
    moves should not contain (NormalMove(Piece.Pawn, F6, E5, None, true))    
    moves should not contain (NormalMove(Piece.Pawn, F6, G5, None, true))    
  }

  it should "generate captures for the white side and the en passant" in {
    val moves = Board("4k3/8/8/3PpP2/8/8/8/4K3 w - e6 0 1").generatePseudolegalMoves
    moves.length should be >= (2)
    moves should contain (NormalMove(Piece.Pawn, D5, E6, None, true))
    moves should contain (NormalMove(Piece.Pawn, F5, E6, None, true))
  }

  it should "generate captures for the black side and the en passant" in {
    val moves = Board("4k3/8/8/8/1pPp4/8/8/4K3 b - c3 0 1").generatePseudolegalMoves
    moves.length should be >= (2)
    moves should contain (NormalMove(Piece.Pawn, B4, C3, None, true))
    moves should contain (NormalMove(Piece.Pawn, D4, C3, None, true))
  }

  it should "generate moves for the white side and promotions" in {
    val moves = Board("4k2b/1P4P1/8/8/8/8/8/4K3 w - - 0 1").generatePseudolegalMoves
    moves.length should be >= (12)
    moves should contain (NormalMove(Piece.Pawn, B7, B8, Some(PromotionPiece.Knight), false))
    moves should contain (NormalMove(Piece.Pawn, B7, B8, Some(PromotionPiece.Bishop), false))
    moves should contain (NormalMove(Piece.Pawn, B7, B8, Some(PromotionPiece.Rook), false))
    moves should contain (NormalMove(Piece.Pawn, B7, B8, Some(PromotionPiece.Queen), false))
    moves should contain (NormalMove(Piece.Pawn, G7, G8, Some(PromotionPiece.Knight), false))
    moves should contain (NormalMove(Piece.Pawn, G7, G8, Some(PromotionPiece.Bishop), false))
    moves should contain (NormalMove(Piece.Pawn, G7, G8, Some(PromotionPiece.Rook), false))
    moves should contain (NormalMove(Piece.Pawn, G7, G8, Some(PromotionPiece.Queen), false))
    moves should contain (NormalMove(Piece.Pawn, G7, H8, Some(PromotionPiece.Knight), true))
    moves should contain (NormalMove(Piece.Pawn, G7, H8, Some(PromotionPiece.Bishop), true))
    moves should contain (NormalMove(Piece.Pawn, G7, H8, Some(PromotionPiece.Rook), true))
    moves should contain (NormalMove(Piece.Pawn, G7, H8, Some(PromotionPiece.Queen), true))
  }

  it should "generate moves for the black side and promotions" in {
    val moves = Board("4k3/8/8/8/8/8/2p3p1/3NK3 b - - 0 1").generatePseudolegalMoves
    moves.length should be >= (12)
    moves should contain (NormalMove(Piece.Pawn, C2, C1, Some(PromotionPiece.Knight), false))
    moves should contain (NormalMove(Piece.Pawn, C2, C1, Some(PromotionPiece.Bishop), false))
    moves should contain (NormalMove(Piece.Pawn, C2, C1, Some(PromotionPiece.Rook), false))
    moves should contain (NormalMove(Piece.Pawn, C2, C1, Some(PromotionPiece.Queen), false))
    moves should contain (NormalMove(Piece.Pawn, C2, D1, Some(PromotionPiece.Knight), true))
    moves should contain (NormalMove(Piece.Pawn, C2, D1, Some(PromotionPiece.Bishop), true))
    moves should contain (NormalMove(Piece.Pawn, C2, D1, Some(PromotionPiece.Rook), true))
    moves should contain (NormalMove(Piece.Pawn, C2, D1, Some(PromotionPiece.Queen), true))
    moves should contain (NormalMove(Piece.Pawn, G2, G1, Some(PromotionPiece.Knight), false))
    moves should contain (NormalMove(Piece.Pawn, G2, G1, Some(PromotionPiece.Bishop), false))
    moves should contain (NormalMove(Piece.Pawn, G2, G1, Some(PromotionPiece.Rook), false))
    moves should contain (NormalMove(Piece.Pawn, G2, G1, Some(PromotionPiece.Queen), false))
  }

  it should "generate moves for the white side and the knights" in {
    val moves = Board("4k3/1N6/3b1p2/2P1p3/6N1/4P3/8/4K3 w - - 0 1").generatePseudolegalMoves
    moves.length should be >= (8)
    moves should contain (NormalMove(Piece.Knight, G4, F6, None, true))
    moves should contain (NormalMove(Piece.Knight, G4, H6, None, false))
    moves should contain (NormalMove(Piece.Knight, G4, E5, None, true))
    moves should not contain (NormalMove(Piece.Knight, G4, E3, None, false))
    moves should not contain (NormalMove(Piece.Knight, G4, E3, None, true))
    moves should contain (NormalMove(Piece.Knight, G4, F2, None, false))
    moves should contain (NormalMove(Piece.Knight, G4, H2, None, false))
    moves should contain (NormalMove(Piece.Knight, B7, D8, None, false))
    moves should contain (NormalMove(Piece.Knight, B7, D6, None, true))
    moves should contain (NormalMove(Piece.Knight, B7, A5, None, false))
    moves should not contain (NormalMove(Piece.Knight, B7, C5, None, false))
    moves should not contain (NormalMove(Piece.Knight, B7, C5, None, true))
  }

  it should "generate moves for the black side and the knights" in {
    val moves = Board("4k3/6b1/2pp4/5n2/1n6/3PP3/2P5/4K3 b - - 0 1").generatePseudolegalMoves
    moves.length should be >= (11)
    moves should contain (NormalMove(Piece.Knight, B4, A6, None, false))
    moves should not contain (NormalMove(Piece.Knight, B4, C6, None, false))
    moves should not contain (NormalMove(Piece.Knight, B4, C6, None, true))
    moves should contain (NormalMove(Piece.Knight, B4, D5, None, false))
    moves should contain (NormalMove(Piece.Knight, B4, D3, None, true))
    moves should contain (NormalMove(Piece.Knight, B4, A2, None, false))
    moves should contain (NormalMove(Piece.Knight, B4, C2, None, true))
    moves should contain (NormalMove(Piece.Knight, F5, E7, None, false))
    moves should not contain (NormalMove(Piece.Knight, F5, G7, None, false))
    moves should not contain (NormalMove(Piece.Knight, F5, G7, None, true))
    moves should not contain (NormalMove(Piece.Knight, F5, D6, None, false))
    moves should not contain (NormalMove(Piece.Knight, F5, D6, None, true))
    moves should contain (NormalMove(Piece.Knight, F5, H6, None, false))
    moves should contain (NormalMove(Piece.Knight, F5, D4, None, false))
    moves should contain (NormalMove(Piece.Knight, F5, H4, None, false))
    moves should contain (NormalMove(Piece.Knight, F5, E3, None, true))
    moves should contain (NormalMove(Piece.Knight, F5, G3, None, false))
  }

  it should "generate moves for the white side and the bishops" in {
    val moves = Board("4k3/8/P3n3/4p3/2B4P/6B1/4N3/4K3 w - - 0 1").generatePseudolegalMoves
    moves.length should be >= (10)
    moves should contain (NormalMove(Piece.Bishop, G3, F4, None, false))
    moves should contain (NormalMove(Piece.Bishop, G3, E5, None, true))
    moves should not contain (NormalMove(Piece.Bishop, G3, D6, None, false))
    moves should not contain (NormalMove(Piece.Bishop, G3, D6, None, true))
    moves should not contain (NormalMove(Piece.Bishop, G3, H4, None, false))
    moves should not contain (NormalMove(Piece.Bishop, G3, H4, None, true))
    moves should contain (NormalMove(Piece.Bishop, G3, F2, None, false))
    moves should not contain (NormalMove(Piece.Bishop, G3, E1, None, false))
    moves should not contain (NormalMove(Piece.Bishop, G3, E1, None, true))
    moves should contain (NormalMove(Piece.Bishop, G3, H2, None, false))
    moves should contain (NormalMove(Piece.Bishop, C4, B5, None, false))
    moves should not contain (NormalMove(Piece.Bishop, C4, A6, None, false))
    moves should not contain (NormalMove(Piece.Bishop, C4, A6, None, true))
    moves should contain (NormalMove(Piece.Bishop, C4, D5, None, false))
    moves should contain (NormalMove(Piece.Bishop, C4, E6, None, true))
    moves should not contain (NormalMove(Piece.Bishop, C4, F7, None, false))
    moves should not contain (NormalMove(Piece.Bishop, C4, F7, None, true))
    moves should contain (NormalMove(Piece.Bishop, C4, B3, None, false))
    moves should contain (NormalMove(Piece.Bishop, C4, A2, None, false))
    moves should contain (NormalMove(Piece.Bishop, C4, D3, None, false))
    moves should not contain (NormalMove(Piece.Bishop, C4, E2, None, false))
    moves should not contain (NormalMove(Piece.Bishop, C4, E2, None, true))
  }

  it should "generate moves for the black side and the bishops" in {
    val moves = Board("4k3/3p3p/8/4pb2/8/3N3N/1b6/4K3 b - - 0 1").generatePseudolegalMoves
    moves.length should be >= (11)
    moves should contain (NormalMove(Piece.Bishop, B2, A3, None, false))
    moves should contain (NormalMove(Piece.Bishop, B2, C3, None, false))
    moves should contain (NormalMove(Piece.Bishop, B2, D4, None, false))
    moves should not contain (NormalMove(Piece.Bishop, B2, E5, None, false))
    moves should not contain (NormalMove(Piece.Bishop, B2, E5, None, true))
    moves should contain (NormalMove(Piece.Bishop, B2, A1, None, false))
    moves should contain (NormalMove(Piece.Bishop, B2, C1, None, false))
    moves should contain (NormalMove(Piece.Bishop, F5, E6, None, false))
    moves should not contain (NormalMove(Piece.Bishop, F5, D7, None, false))
    moves should not contain (NormalMove(Piece.Bishop, F5, D7, None, true))
    moves should contain (NormalMove(Piece.Bishop, F5, G6, None, false))
    moves should not contain (NormalMove(Piece.Bishop, F5, H7, None, false))
    moves should not contain (NormalMove(Piece.Bishop, F5, H7, None, true))
    moves should contain (NormalMove(Piece.Bishop, F5, E4, None, false))
    moves should contain (NormalMove(Piece.Bishop, F5, D3, None, true))
    moves should not contain (NormalMove(Piece.Bishop, F5, C2, None, false))
    moves should not contain (NormalMove(Piece.Bishop, F5, C2, None, true))    
    moves should contain (NormalMove(Piece.Bishop, F5, G4, None, false))
    moves should contain (NormalMove(Piece.Bishop, F5, H3, None, true))
  }

  it should "generate moves for the white side and the rooks" in {
    val moves = Board("4k3/8/R2P4/6P1/8/p3n1R1/8/4K3 w - - 0 1").generatePseudolegalMoves
    moves.length should be >= (13)
    moves should contain (NormalMove(Piece.Rook, G3, G4, None, false))
    moves should not contain (NormalMove(Piece.Rook, G3, G5, None, false))
    moves should not contain (NormalMove(Piece.Rook, G3, G5, None, true))
    moves should contain (NormalMove(Piece.Rook, G3, F3, None, false))
    moves should contain (NormalMove(Piece.Rook, G3, E3, None, true))
    moves should not contain (NormalMove(Piece.Rook, G3, D3, None, false))
    moves should not contain (NormalMove(Piece.Rook, G3, D3, None, true))
    moves should contain (NormalMove(Piece.Rook, G3, H3, None, false))
    moves should contain (NormalMove(Piece.Rook, G3, G2, None, false))
    moves should contain (NormalMove(Piece.Rook, G3, G1, None, false))
    moves should contain (NormalMove(Piece.Rook, A6, A7, None, false))
    moves should contain (NormalMove(Piece.Rook, A6, A8, None, false))
    moves should contain (NormalMove(Piece.Rook, A6, B6, None, false))
    moves should contain (NormalMove(Piece.Rook, A6, C6, None, false))
    moves should not contain (NormalMove(Piece.Rook, A6, D6, None, false))
    moves should not contain (NormalMove(Piece.Rook, A6, D6, None, true))
    moves should contain (NormalMove(Piece.Rook, A6, A5, None, false))
    moves should contain (NormalMove(Piece.Rook, A6, A4, None, false))
    moves should contain (NormalMove(Piece.Rook, A6, A3, None, true))
    moves should not contain (NormalMove(Piece.Rook, A6, A2, None, false))
    moves should not contain (NormalMove(Piece.Rook, A6, A2, None, true))
  }

  it should "generate moves for the black side and the rooks" in {
    val moves = Board("4k1n1/8/1p6/4P1r1/1r1P4/6N1/1P6/4K3 b - - 0 1").generatePseudolegalMoves
    moves.length should be >= (14)
    moves should contain (NormalMove(Piece.Rook, B4, B5, None, false))
    moves should not contain (NormalMove(Piece.Rook, B4, B6, None, false))
    moves should not contain (NormalMove(Piece.Rook, B4, B6, None, true))
    moves should contain (NormalMove(Piece.Rook, B4, A4, None, false))
    moves should contain (NormalMove(Piece.Rook, B4, C4, None, false))
    moves should contain (NormalMove(Piece.Rook, B4, D4, None, true))
    moves should not contain (NormalMove(Piece.Rook, B4, E4, None, false))
    moves should not contain (NormalMove(Piece.Rook, B4, E4, None, true))    
    moves should contain (NormalMove(Piece.Rook, B4, B3, None, false))
    moves should contain (NormalMove(Piece.Rook, B4, B2, None, true))
    moves should not contain (NormalMove(Piece.Rook, B4, B1, None, false))
    moves should not contain (NormalMove(Piece.Rook, B4, B1, None, true))
    moves should contain (NormalMove(Piece.Rook, G5, G6, None, false))
    moves should contain (NormalMove(Piece.Rook, G5, G7, None, false))
    moves should not contain (NormalMove(Piece.Rook, G5, G8, None, false))
    moves should not contain (NormalMove(Piece.Rook, G5, G8, None, true))
    moves should contain (NormalMove(Piece.Rook, G5, F5, None, false))
    moves should contain (NormalMove(Piece.Rook, G5, E5, None, true))
    moves should not contain (NormalMove(Piece.Rook, G5, D5, None, false))
    moves should not contain (NormalMove(Piece.Rook, G5, D5, None, true))
    moves should contain (NormalMove(Piece.Rook, G5, H5, None, false))
    moves should contain (NormalMove(Piece.Rook, G5, G4, None, false))
    moves should contain (NormalMove(Piece.Rook, G5, G3, None, true))
    moves should not contain (NormalMove(Piece.Rook, G5, G2, None, false))
    moves should not contain (NormalMove(Piece.Rook, G5, G2, None, true))
  }

  it should "generate moves for the white side and the queen" in {
    val moves = Board("4k3/8/3p2P1/8/8/4b1Q1/8/4K3 w - - 0 1").generatePseudolegalMoves
    moves.length should be >= (13)
    moves should contain (NormalMove(Piece.Queen, G3, F4, None, false))
    moves should contain (NormalMove(Piece.Queen, G3, E5, None, false))
    moves should contain (NormalMove(Piece.Queen, G3, D6, None, true))
    moves should not contain (NormalMove(Piece.Queen, G3, C7, None, false))
    moves should not contain (NormalMove(Piece.Queen, G3, C7, None, true))
    moves should contain (NormalMove(Piece.Queen, G3, G4, None, false))
    moves should contain (NormalMove(Piece.Queen, G3, G5, None, false))
    moves should not contain (NormalMove(Piece.Queen, G3, G6, None, false))
    moves should not contain (NormalMove(Piece.Queen, G3, G6, None, true))
    moves should contain (NormalMove(Piece.Queen, G3, H4, None, false))
    moves should contain (NormalMove(Piece.Queen, G3, F3, None, false))
    moves should contain (NormalMove(Piece.Queen, G3, E3, None, true))
    moves should not contain (NormalMove(Piece.Queen, G3, D3, None, false))
    moves should not contain (NormalMove(Piece.Queen, G3, D3, None, true))
    moves should contain (NormalMove(Piece.Queen, G3, H3, None, false))
    moves should contain (NormalMove(Piece.Queen, G3, F2, None, false))
    moves should not contain (NormalMove(Piece.Queen, G3, E1, None, false))
    moves should not contain (NormalMove(Piece.Queen, G3, E1, None, true))
    moves should contain (NormalMove(Piece.Queen, G3, G2, None, false))
    moves should contain (NormalMove(Piece.Queen, G3, G1, None, false))
    moves should contain (NormalMove(Piece.Queen, G3, H2, None, false))
  }

  it should "generate moves for the black side and the queen" in {
    val moves = Board("4k3/1p6/8/1q2P3/8/3N4/1P6/4K3 b - - 0 1").generatePseudolegalMoves
    moves.length should be >= (14)
    moves should contain (NormalMove(Piece.Queen, B5, A6, None, false))
    moves should contain (NormalMove(Piece.Queen, B5, B6, None, false))
    moves should not contain (NormalMove(Piece.Queen, B5, B7, None, false))
    moves should not contain (NormalMove(Piece.Queen, B5, B7, None, true))
    moves should contain (NormalMove(Piece.Queen, B5, C6, None, false))
    moves should contain (NormalMove(Piece.Queen, B5, D7, None, false))
    moves should not contain (NormalMove(Piece.Queen, B5, E8, None, false))
    moves should not contain (NormalMove(Piece.Queen, B5, E8, None, true))
    moves should contain (NormalMove(Piece.Queen, B5, A5, None, false))
    moves should contain (NormalMove(Piece.Queen, B5, C5, None, false))
    moves should contain (NormalMove(Piece.Queen, B5, D5, None, false))
    moves should contain (NormalMove(Piece.Queen, B5, E5, None, true))
    moves should not contain (NormalMove(Piece.Queen, B5, F5, None, false))
    moves should not contain (NormalMove(Piece.Queen, B5, F5, None, true))
    moves should contain (NormalMove(Piece.Queen, B5, A4, None, false))
    moves should contain (NormalMove(Piece.Queen, B5, B4, None, false))
    moves should contain (NormalMove(Piece.Queen, B5, B3, None, false))
    moves should contain (NormalMove(Piece.Queen, B5, B2, None, true))
    moves should not contain (NormalMove(Piece.Queen, B5, B1, None, false))
    moves should not contain (NormalMove(Piece.Queen, B5, B1, None, true))
    moves should contain (NormalMove(Piece.Queen, B5, C4, None, false))
    moves should contain (NormalMove(Piece.Queen, B5, D3, None, true))
    moves should not contain (NormalMove(Piece.Queen, B5, E2, None, false))
    moves should not contain (NormalMove(Piece.Queen, B5, E2, None, true))
  }

  it should "generate moves for the white side and the king" in {
    val moves = Board("4k3/8/8/8/8/8/3Np3/4K3 w - - 0 1").generatePseudolegalMoves
    moves.length should be >= (4)
    moves should not contain (NormalMove(Piece.King, E1, D2, None, false))
    moves should not contain (NormalMove(Piece.King, E1, D2, None, true))
    moves should contain (NormalMove(Piece.King, E1, E2, None, true))
    moves should contain (NormalMove(Piece.King, E1, F2, None, false)) 
    moves should contain (NormalMove(Piece.King, E1, D1, None, false)) 
    moves should contain (NormalMove(Piece.King, E1, F1, None, false)) 
  }

  it should "generate moves for the black side and the king" in {
    val moves = Board("4k3/3P1p2/2B5/8/8/8/8/4K3 b - - 0 1").generatePseudolegalMoves
    moves.length should be >= (4)
    moves should contain (NormalMove(Piece.King, E8, D8, None, false))
    moves should contain (NormalMove(Piece.King, E8, F8, None, false)) 
    moves should contain (NormalMove(Piece.King, E8, D7, None, true)) 
    moves should contain (NormalMove(Piece.King, E8, E7, None, false)) 
    moves should not contain (NormalMove(Piece.King, E8, F7, None, false))
    moves should not contain (NormalMove(Piece.King, E8, F7, None, true))
  }

  it should "generate all castlings for the white side" in {
    val moves = Board("4k3/8/8/8/8/8/8/R3K2R w KQ - 0 1").generatePseudolegalMoves
    moves.length should  be >= (2)
    moves should contain (ShortCastling)
    moves should contain (LongCastling) 
  }

  it should "generate all castlings for the black side" in {
    val moves = Board("r3k2r/8/8/8/8/8/8/4K3 b kq - 0 1").generatePseudolegalMoves
    moves.length should  be >= (2)
    moves should contain (ShortCastling)
    moves should contain (LongCastling) 
  }

  it should "not generate all castlings for the white side" in {
    val moves = Board("4k3/8/8/8/8/8/8/RN2K2R w Q - 0 1").generatePseudolegalMoves
    moves should not contain (ShortCastling)
    moves should not contain (LongCastling) 
  }

  it should "not generate all castlings for the black side" in {
    val moves = Board("r3kb1r/8/8/8/8/8/8/4K3 b k - 0 1").generatePseudolegalMoves
    moves should not contain (ShortCastling)
    moves should not contain (LongCastling) 
  }

  it should "generate moves for the white side and the pieces" in {
    val moves = Board("rnbqkb1r/pppp1ppp/5n2/4p3/4P3/5N2/PPPP1PPP/RNBQKB1R w KQkq - 0 3").generatePseudolegalMoves
    moves.length should  be (27)
    moves should contain (NormalMove(Piece.Knight, B1, A3, None, false))
    moves should contain (NormalMove(Piece.Knight, B1, C3, None, false))
    moves should contain (NormalMove(Piece.Queen, D1, E2, None, false))
    moves should contain (NormalMove(Piece.King, E1, E2, None, false))
    moves should contain (NormalMove(Piece.Bishop, F1, E2, None, false))
    moves should contain (NormalMove(Piece.Bishop, F1, D3, None, false))
    moves should contain (NormalMove(Piece.Bishop, F1, C4, None, false))
    moves should contain (NormalMove(Piece.Bishop, F1, B5, None, false))
    moves should contain (NormalMove(Piece.Bishop, F1, A6, None, false))
    moves should contain (NormalMove(Piece.Rook, H1, G1, None, false))
    moves should contain (NormalMove(Piece.Pawn, A2, A3, None, false))
    moves should contain (NormalMove(Piece.Pawn, A2, A4, None, false))
    moves should contain (NormalMove(Piece.Pawn, B2, B3, None, false))
    moves should contain (NormalMove(Piece.Pawn, B2, B4, None, false))
    moves should contain (NormalMove(Piece.Pawn, C2, C3, None, false))
    moves should contain (NormalMove(Piece.Pawn, C2, C4, None, false))
    moves should contain (NormalMove(Piece.Pawn, D2, D3, None, false))
    moves should contain (NormalMove(Piece.Pawn, D2, D4, None, false))
    moves should contain (NormalMove(Piece.Pawn, G2, G3, None, false))
    moves should contain (NormalMove(Piece.Pawn, G2, G4, None, false))
    moves should contain (NormalMove(Piece.Pawn, H2, H3, None, false))
    moves should contain (NormalMove(Piece.Pawn, H2, H4, None, false))
    moves should contain (NormalMove(Piece.Knight, F3, E5, None, true))
    moves should contain (NormalMove(Piece.Knight, F3, G5, None, false))
    moves should contain (NormalMove(Piece.Knight, F3, D4, None, false))
    moves should contain (NormalMove(Piece.Knight, F3, H4, None, false))
    moves should contain (NormalMove(Piece.Knight, F3, G1, None, false))
  }

  it should "generate moves for the black side and the pieces" in {
    val moves = Board("rnbqkb1r/pppp1ppp/5n2/4p3/4P3/3P1N2/PPP2PPP/RNBQKB1R b KQkq - 0 3").generatePseudolegalMoves
    moves.length should  be (27)
    moves should contain (NormalMove(Piece.Knight, F6, G8, None, false))
    moves should contain (NormalMove(Piece.Knight, F6, D5, None, false))
    moves should contain (NormalMove(Piece.Knight, F6, H5, None, false))
    moves should contain (NormalMove(Piece.Knight, F6, E4, None, true))
    moves should contain (NormalMove(Piece.Knight, F6, G4, None, false))
    moves should contain (NormalMove(Piece.Pawn, A7, A6, None, false))
    moves should contain (NormalMove(Piece.Pawn, A7, A5, None, false))
    moves should contain (NormalMove(Piece.Pawn, B7, B6, None, false))
    moves should contain (NormalMove(Piece.Pawn, B7, B5, None, false))
    moves should contain (NormalMove(Piece.Pawn, C7, C6, None, false))
    moves should contain (NormalMove(Piece.Pawn, C7, C5, None, false))
    moves should contain (NormalMove(Piece.Pawn, D7, D6, None, false))
    moves should contain (NormalMove(Piece.Pawn, D7, D5, None, false))
    moves should contain (NormalMove(Piece.Pawn, G7, G6, None, false))
    moves should contain (NormalMove(Piece.Pawn, G7, G5, None, false))
    moves should contain (NormalMove(Piece.Pawn, H7, H6, None, false))
    moves should contain (NormalMove(Piece.Pawn, H7, H5, None, false))
    moves should contain (NormalMove(Piece.Knight, B8, A6, None, false))
    moves should contain (NormalMove(Piece.Knight, B8, C6, None, false))
    moves should contain (NormalMove(Piece.Queen, D8, E7, None, false))
    moves should contain (NormalMove(Piece.King, E8, E7, None, false))
    moves should contain (NormalMove(Piece.Bishop, F8, E7, None, false))
    moves should contain (NormalMove(Piece.Bishop, F8, D6, None, false))
    moves should contain (NormalMove(Piece.Bishop, F8, C5, None, false))
    moves should contain (NormalMove(Piece.Bishop, F8, B4, None, false))
    moves should contain (NormalMove(Piece.Bishop, F8, A3, None, false))
    moves should contain (NormalMove(Piece.Rook, H8, G8, None, false))
  }
  
  "A Board.unsafelyMakeMove" should "make the move for the piece" in {
    val board = Board("4k3/2p5/5n2/8/8/5N2/2P5/4K3 w - - 0 1")
    inside(board.unsafelyMakeMove(NormalMove(Piece.Knight, F3, E5, None, false))) {
      case Some(newBoard) =>
        newBoard should be (Board("4k3/2p5/5n2/4N3/8/8/2P5/4K3 b - - 1 1"))
    }
  }

  it should "make the capture for the piece" in {
    val board = Board("4k3/8/8/3P2N1/8/8/6b1/4K3 b - - 0 1")
    inside(board.unsafelyMakeMove(NormalMove(Piece.Bishop, G2, D5, None, true))) {
      case Some(newBoard) =>
        newBoard should be (Board("4k3/8/8/3b2N1/8/8/8/4K3 w - - 0 2"))
    }
  }

  it should "not make the king move for check" in {
    val board = Board("4k3/8/8/8/2n5/8/8/4K3 w - - 0 1")
    board.unsafelyMakeMove(NormalMove(Piece.King, E1, D2, None, false)) should be (None)
  }

  it should "not make the pawn move for check" in {
    val board = Board("4k3/3p4/8/1B6/8/8/8/4K3 b - - 0 1")
    board.unsafelyMakeMove(NormalMove(Piece.Pawn, D7, D5, None, false)) should be (None)
  }

  it should "make the move for promotion" in {
    val board = Board("4k3/1P6/3n4/8/8/8/8/4K3 w - - 0 1")
    inside(board.unsafelyMakeMove(NormalMove(Piece.Pawn, B7, B8, Some(PromotionPiece.Queen), false))) {
      case Some(newBoard) =>
        newBoard should be (Board("1Q2k3/8/3n4/8/8/8/8/4K3 b - - 0 1"))
    }
  }

  it should "make the capture for promotion" in {
    val board = Board("4k3/8/8/8/8/8/2p2N2/3NK3 b - - 0 1")
    inside(board.unsafelyMakeMove(NormalMove(Piece.Pawn, C2, D1, Some(PromotionPiece.Knight), true))) {
      case Some(newBoard) =>
        newBoard should be (Board("4k3/8/8/8/8/8/5N2/3nK3 w - - 0 2"))
    }
  }
  
  it should "make the capture for the white side and the en passant" in {
    val board = Board("4k3/8/8/4PpP1/8/8/8/4K3 w - f6 0 1")
    inside(board.unsafelyMakeMove(NormalMove(Piece.Pawn, G5, F6, None, true))) {
      case Some(newBoard) =>
        newBoard should be (Board("4k3/8/5P2/4P3/8/8/8/4K3 b - - 0 1"))
    }
  }

  it should "make the capture for the black side and the en passant" in {
    val board = Board("4k3/8/8/8/1pP5/8/8/4K3 b - c3 0 1")
    inside(board.unsafelyMakeMove(NormalMove(Piece.Pawn, B4, C3, None, true))) {
      case Some(newBoard) =>
        newBoard should be (Board("4k3/8/8/8/8/2p5/8/4K3 w - - 0 2"))
    }
  }

  it should "set the enPassantColumnOption for the white side" in {
    val board = Board("4k3/8/8/8/3p4/8/2P2P2/4K3 w - - 0 1")
    inside(board.unsafelyMakeMove(NormalMove(Piece.Pawn, C2, C4, None, false))) {
      case Some(newBoard) =>
        newBoard should be (Board("4k3/8/8/8/2Pp4/8/5P2/4K3 b - c3 0 1"))
    }
  }

  it should "set the enPassantColumnOption for the black side" in {
    val board = Board("4k3/4p3/8/3P1N2/8/8/8/4K3 b - - 0 1")
    inside(board.unsafelyMakeMove(NormalMove(Piece.Pawn, E7, E5, None, false))) {
      case Some(newBoard) =>
        newBoard should be (Board("4k3/8/8/3PpN2/8/8/8/4K3 w - e6 0 2"))
    }
  }

  it should "not set the enPassantColumnOption for the white side" in {
    val board = Board("4k3/8/8/8/2n5/8/3P4/4K3 w - - 0 1")
    inside(board.unsafelyMakeMove(NormalMove(Piece.Pawn, D2, D4, None, false))) {
      case Some(newBoard) =>
        newBoard should be (Board("4k3/8/8/8/2nP4/8/8/4K3 b - - 0 1"))
    }
  }

  it should "not set the enPassantColumnOption for the black side" in {
    val board = Board("4k3/5p2/8/6B1/8/8/8/4K3 b - - 0 1")
    inside(board.unsafelyMakeMove(NormalMove(Piece.Pawn, F7, F5, None, false))) {
      case Some(newBoard) =>
        newBoard should be (Board("4k3/8/8/5pB1/8/8/8/4K3 w - - 0 2"))
    }
  }

  it should "make the short castling for the white side" in {
    val board = Board("4k3/8/8/8/8/8/8/4K2R w K - 0 1")
    inside(board.unsafelyMakeMove(ShortCastling)) {
      case Some(newBoard) =>
        newBoard should be (Board("4k3/8/8/8/8/8/8/5RK1 b - - 1 1"))
    }
  }

  it should "make the short castling for the black side" in {
    val board = Board("4k2r/8/8/8/8/8/8/4K3 b k - 0 1")
    inside(board.unsafelyMakeMove(ShortCastling)) {
      case Some(newBoard) =>
        newBoard should be (Board("5rk1/8/8/8/8/8/8/4K3 w - - 1 2"))
    }
  }

  it should "make the long castling for the white side" in {
    val board = Board("4k3/8/8/8/8/8/8/R3K3 w Q - 0 1")
    inside(board.unsafelyMakeMove(LongCastling)) {
      case Some(newBoard) =>
        newBoard should be (Board("4k3/8/8/8/8/8/8/2KR4 b - - 1 1"))
    }
  }

  it should "make the long castling for the black side" in {
    val board = Board("r3k3/8/8/8/8/8/8/4K3 b q - 0 1")
    inside(board.unsafelyMakeMove(LongCastling)) {
      case Some(newBoard) =>
        newBoard should be (Board("2kr4/8/8/8/8/8/8/4K3 w - - 1 2"))
    }
  }

  it should "not make the short castling for check before the castling" in {
    val board = Board("4k3/8/8/8/8/6b1/8/R3K2R w KQ - 0 1")
    board.unsafelyMakeMove(ShortCastling) should be (None)
  }

  it should "not make the short castling for attack to the rook destination square" in {
    val board = Board("r3k2r/8/8/8/8/8/5R2/4K3 b kq - 0 1")
    board.unsafelyMakeMove(ShortCastling) should be (None)
  }

  it should "not make the short castling for check after the castling" in {
    val board = Board("4k3/8/8/8/3b4/8/8/R3K2R w KQ - 0 1")
    board.unsafelyMakeMove(ShortCastling) should be (None)
  }

  it should "not make the long castling for check before the castling" in {
    val board = Board("r3k2r/8/8/8/8/4R3/8/4K3 b kq - 0 1")
    board.unsafelyMakeMove(LongCastling) should be (None)
  }

  it should "not make the long castling for attack to the rook destination square" in {
    val board = Board("4k3/8/8/8/6b1/8/8/R3K2R w KQ - 0 1")
    board.unsafelyMakeMove(LongCastling) should be (None)
  }

  it should "not make the long castling for check after the castling" in {
    val board = Board("r3k2r/8/8/5B2/8/8/8/4K3 b kq - 0 1")
    board.unsafelyMakeMove(LongCastling) should be (None)
  }

  it should "remove the white short castling for the white side" in {
    val board = Board("r3k2r/8/8/8/8/8/8/R3K2R w KQkq - 0 1")
    inside(board.unsafelyMakeMove(NormalMove(Piece.Rook, H1, H3, None, false))) {
      case Some(newBoard) =>
        newBoard should be (Board("r3k2r/8/8/8/8/7R/8/R3K3 b Qkq - 1 1"))
    }
  }

  it should "remove the black short castling for the black side" in {
    val board = Board("r3k2r/8/8/8/8/8/8/R3K2R b KQkq - 0 1")
    inside(board.unsafelyMakeMove(NormalMove(Piece.Rook, H8, H6, None, false))) {
      case Some(newBoard) =>
        newBoard should be (Board("r3k3/8/7r/8/8/8/8/R3K2R w KQq - 1 2"))
    }
  }

  it should "remove the white long castling for the white side" in {
    val board = Board("r3k2r/8/8/8/8/8/8/R3K2R w KQkq - 0 1")
    inside(board.unsafelyMakeMove(NormalMove(Piece.Rook, A1, A4, None, false))) {
      case Some(newBoard) =>
        newBoard should be (Board("r3k2r/8/8/8/R7/8/8/4K2R b Kkq - 1 1"))
    }
  }

  it should "remove the black long castling for the black side" in {
    val board = Board("r3k2r/8/8/8/8/8/8/R3K2R b KQkq - 0 1")
    inside(board.unsafelyMakeMove(NormalMove(Piece.Rook, A8, A5, None, false))) {
      case Some(newBoard) =>
        newBoard should be (Board("4k2r/8/8/r7/8/8/8/R3K2R w KQk - 1 2"))
    }
  }

  it should "remove all white castlings for the white side" in {
    val board = Board("r3k2r/8/8/8/8/8/8/R3K2R w KQkq - 0 1")
    inside(board.unsafelyMakeMove(NormalMove(Piece.King, E1, E2, None, false))) {
      case Some(newBoard) =>
        newBoard should be (Board("r3k2r/8/8/8/8/8/4K3/R6R b kq - 1 1"))
    }
  }

  it should "remove all black castlings for the black side" in {
    val board = Board("r3k2r/8/8/8/8/8/8/R3K2R b KQkq - 0 1")
    inside(board.unsafelyMakeMove(NormalMove(Piece.King, E8, E7, None, false))) {
      case Some(newBoard) =>
        newBoard should be (Board("r6r/4k3/8/8/8/8/8/R3K2R w KQ - 1 2"))
    }
  }

  it should "remove the black short castling for the white side" in {
    val board = Board("r3k2r/8/8/4B3/8/8/8/R3K2R w KQkq - 0 1")
    inside(board.unsafelyMakeMove(NormalMove(Piece.Bishop, E5, H8, None, true))) {
      case Some(newBoard) =>
        newBoard should be (Board("r3k2B/8/8/8/8/8/8/R3K2R b KQq - 0 1"))
    }
  }

  it should "remove the white short castling for the black side" in {
    val board = Board("r3k2r/8/8/8/4b3/8/8/R3K2R b KQkq - 0 1")
    inside(board.unsafelyMakeMove(NormalMove(Piece.Bishop, E4, H1, None, true))) {
      case Some(newBoard) =>
        newBoard should be (Board("r3k2r/8/8/8/8/8/8/R3K2b w Qkq - 0 2"))
    }
  }

  it should "remove the black long castling for the white side" in {
    val board = Board("r3k2r/8/8/8/4B3/8/8/R3K2R w KQkq - 0 1")
    inside(board.unsafelyMakeMove(NormalMove(Piece.Bishop, E4, A8, None, true))) {
      case Some(newBoard) =>
        newBoard should be (Board("B3k2r/8/8/8/8/8/8/R3K2R b KQk - 0 1"))
    }
  }

  it should "remove the white long castling for the black side" in {
    val board = Board("r3k2r/8/8/4b3/8/8/8/R3K2R b KQkq - 0 1")
    inside(board.unsafelyMakeMove(NormalMove(Piece.Bishop, E5, A1, None, true))) {
      case Some(newBoard) =>
        newBoard should be (Board("r3k2r/8/8/8/8/8/8/b3K2R w Kkq - 0 2"))
    }
  }
}
