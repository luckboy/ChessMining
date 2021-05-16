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
package pl.luckboy.chessmining.chess.spec
import org.scalatest._
import org.scalatest.flatspec._
import org.scalatest.matchers._
import pl.luckboy.chessmining.chess._
import Squares._

class MoveSpec extends AnyFlatSpec with should.Matchers with Inside
{
  "A Move.sanMoveToMoveOption" should "convert the SAN move to the move" in {
    val board = Board("rnbqkbnr/pppp1ppp/8/4p3/4P3/8/PPPP1PPP/RNBQKBNR w KQkq - 0 2")
    inside(Move.sanMoveToMoveOption(SANMove("Nf3"), board)) {
      case Some(move) =>
        move should be (NormalMove(Piece.Knight, G1, F3, None, false))
    }
  }

  it should "convert the SAN move with the source column to the move" in {
    val board = Board("4k3/8/8/8/8/8/2N1N3/4K3 w - - 0 1")
    inside(Move.sanMoveToMoveOption(SANMove("Ncd4"), board)) {
      case Some(move) =>
        move should be (NormalMove(Piece.Knight, C2, D4, None, false))
    }
  }

  it should "convert the SAN move with the source row to the move" in {
    val board = Board("4k3/7r/8/8/8/7r/8/4K3 b - - 0 1")
    inside(Move.sanMoveToMoveOption(SANMove("R3h5"), board)) {
      case Some(move) =>
        move should be (NormalMove(Piece.Rook, H3, H5, None, false))
    }
  }

  it should "convert the SAN move with the source square to the move" in {
    val board = Board("4k3/2B5/8/8/8/2B3B1/8/4K3 w - - 0 1")
    inside(Move.sanMoveToMoveOption(SANMove("Bc3e5"), board)) {
      case Some(move) =>
        move should be (NormalMove(Piece.Bishop, C3, E5, None, false))
    }
  }

  it should "convert the SAN move with the promotion piece to the move" in {
    val board = Board("4k3/1P6/8/8/8/8/8/4K3 w - - 0 1")
    inside(Move.sanMoveToMoveOption(SANMove("b8=Q"), board)) {
      case Some(move) =>
        move should be (NormalMove(Piece.Pawn, B7, B8, Some(PromotionPiece.Queen), false))
    }
  }

  it should "convert the SAN short castling to the move for the white side" in {
    val board = Board("4k3/8/8/8/8/8/8/4K2R w K - 0 1")
    inside(Move.sanMoveToMoveOption(SANMove("O-O"), board)) {
      case Some(move) =>
        move should be (ShortCastling)
    }
  }

  it should "convert the SAN short castling to the move for the black side" in {
    val board = Board("4k2r/8/8/8/8/8/8/4K3 b k - 0 1")
    inside(Move.sanMoveToMoveOption(SANMove("O-O"), board)) {
      case Some(move) =>
        move should be (ShortCastling)
    }
  }

  it should "convert the SAN long castling to the move for the white side" in {
    val board = Board("4k3/8/8/8/8/8/8/R3K3 w Q - 0 1")
    inside(Move.sanMoveToMoveOption(SANMove("O-O-O"), board)) {
      case Some(move) =>
        move should be (LongCastling)
    }
  }

  it should "convert the SAN long castling to the move for the black side" in {
    val board = Board("r3k3/8/8/8/8/8/8/4K3 b q - 0 1")
    inside(Move.sanMoveToMoveOption(SANMove("O-O-O"), board)) {
      case Some(move) =>
        move should be (LongCastling)
    }
  }

  it should "convert the SAN capture to the move" in {
    val board = Board("4k3/8/8/8/4b3/2N5/8/4K3 w - - 0 1")
    inside(Move.sanMoveToMoveOption(SANMove("Nxe4"), board)) {
      case Some(move) =>
        move should be (NormalMove(Piece.Knight, C3, E4, None, true))
    }
  }

  it should "convert the SAN move to the capture" in {
    val board = Board("4k3/8/2b5/8/8/5P2/8/4K3 b - - 0 1")
    inside(Move.sanMoveToMoveOption(SANMove("Bf3"), board)) {
      case Some(move) =>
        move should be (NormalMove(Piece.Bishop, C6, F3, None, true))
    }
  }

  it should "convert the SAN capture to the move for the white side and the en passant" in {
    val board = Board("4k3/8/8/2Pp4/8/8/8/4K3 w - d6 0 1")
    inside(Move.sanMoveToMoveOption(SANMove("cxd6"), board)) {
      case Some(move) =>
        move should be (NormalMove(Piece.Pawn, C5, D6, None, true))
    }
  }

  it should "convert the SAN capture to the move for the black side and the en passant" in {
    val board = Board("4k3/8/8/8/4Pp2/8/8/4K3 b - e3 0 1")
    inside(Move.sanMoveToMoveOption(SANMove("fxe3"), board)) {
      case Some(move) =>
        move should be (NormalMove(Piece.Pawn, F4, E3, None, true))
    }
  }

  it should "convert the SAN move with check to the move" in {
    val board = Board("4k3/5p2/8/8/4B3/8/8/4K3 w - - 0 1")
    inside(Move.sanMoveToMoveOption(SANMove("Bc6+"), board)) {
      case Some(move) =>
        move should be (NormalMove(Piece.Bishop, E4, C6, None, false))
    }
  }

  it should "convert the SAN move with checkmate to the move" in {
    val board = Board("4k3/8/8/6r1/8/8/r7/4K3 b - - 0 1")
    inside(Move.sanMoveToMoveOption(SANMove("Rg1#"), board)) {
      case Some(move) =>
        move should be (NormalMove(Piece.Rook, G5, G1, None, false))
    }
  }
  
  it should "complain on the illegal move" in {
    val board = Board("4k3/8/8/7b/8/8/8/4K3 w - - 0 1")
    Move.sanMoveToMoveOption(SANMove("Ke2"), board) should be (None)
  }
  
  it should "complain on the ambiguous move" in {
    val board = Board("4k3/8/3n1n2/8/8/8/8/4K3 b - - 0 1")
    Move.sanMoveToMoveOption(SANMove("Ne4"), board) should be (None)
  }

  it should "complain on the non-existent short castling in moves for the white side" in {
    val board = Board("4k3/8/8/8/8/8/8/4K2R w - - 0 1")
    Move.sanMoveToMoveOption(SANMove("O-O"), board) should be (None)
  }

  it should "complain on the non-existent short castling in moves for the black side" in {
    val board = Board("4k2r/8/8/8/8/8/8/4K3 b - - 0 1")
    Move.sanMoveToMoveOption(SANMove("O-O"), board) should be (None)
  }

  it should "complain on the non-existent long castling in moves for the white side" in {
    val board = Board("4k3/8/8/8/8/8/8/R3K3 w - - 0 1")
    Move.sanMoveToMoveOption(SANMove("O-O-O"), board) should be (None)
  }

  it should "complain on the non-existent long castling in moves for the black side" in {
    val board = Board("r3k3/8/8/8/8/8/8/4K3 b - - 0 1")
    Move.sanMoveToMoveOption(SANMove("O-O-O"), board) should be (None)
  }

  it should "complain on no capture" in {
    val board = Board("4k3/8/8/5b2/8/8/8/4K3 b - - 0 1")
    Move.sanMoveToMoveOption(SANMove("Bxc2"), board) should be (None)
  }

  it should "complain on no check" in {
    val board = Board("4k3/8/8/8/8/8/2R5/4K3 w - - 0 1")
    Move.sanMoveToMoveOption(SANMove("Rc7+"), board) should be (None)
  }

  it should "complain on no checkmate" in {
    val board = Board("4k3/8/8/1q6/8/8/8/4K3 b - - 0 1")
    Move.sanMoveToMoveOption(SANMove("Qb1#"), board) should be (None)
  }

  "A Move.toSANMove" should "convert the move to the SAN move" in {
    val board = Board("rnbqkbnr/pppp1ppp/8/4p3/4P3/8/PPPP1PPP/RNBQKBNR w KQkq - 0 2")
    val move = NormalMove(Piece.Knight, B1, C3, None, false)
    move.toSANMove(board) should be (SANMove("Nc3"))
  }

  it should "convert the move to the SAN move with the source column" in {
    val board = Board("4k3/8/8/8/8/1b3b2/8/4K3 b - - 0 1")
    val move = NormalMove(Piece.Bishop, B3, D5, None, false)
    move.toSANMove(board) should be (SANMove("Bbd5"))
  }

  it should "convert the move to the SAN move with the source row" in {
    val board = Board("4k3/6R1/8/8/8/8/8/4K1R1 w - - 0 1")
    val move = NormalMove(Piece.Rook, G7, G4, None, false)
    move.toSANMove(board) should be (SANMove("R7g4"))
  }

  it should "convert the move to the SAN move with the source square" in {
    val board = Board("4k3/8/8/1N6/8/1N3N2/8/4K3 w - - 0 1")
    val move = NormalMove(Piece.Knight, B3, D4, None, false)
    move.toSANMove(board) should be (SANMove("Nb3d4"))
  }

  it should "convert the move to the SAN move with the source row for three pieces" in {
    val board = Board("4k3/1b3b2/8/8/8/1b6/8/4K3 b - - 0 1")
    val move = NormalMove(Piece.Bishop, B3, D5, None, false)
    move.toSANMove(board) should be (SANMove("B3d5"))
  }

  it should "convert the move to the SAN move with the source square for four pieces" in {
    val board = Board("4k3/8/2N1N3/8/8/8/2N1N3/4K3 w - - 0 1")
    val move = NormalMove(Piece.Knight, E6, D4, None, false)
    move.toSANMove(board) should be (SANMove("Ne6d4"))
  }

  it should "convert the move to the SAN move with the source column for the different columns and different rows" in {
    val board = Board("4k3/8/8/8/8/5b2/2b5/4K3 b - - 0 1")
    val move = NormalMove(Piece.Bishop, C2, E4, None, false)
    move.toSANMove(board) should be (SANMove("Bce4"))
  }

  it should "convert the pawn capture to the SAN move with the source column" in {
    val board = Board("4k3/8/8/4p3/3P4/6P1/8/4K3 w - - 0 1")
    val move = NormalMove(Piece.Pawn, D4, E5, None, true)
    move.toSANMove(board) should be (SANMove("dxe5"))
  }

  it should "convert the move to the SAN move with the promotion piece" in {
    val board = Board("8/1P2k3/8/8/8/8/8/4K3 w - - 0 1")
    val move = NormalMove(Piece.Pawn, B7, B8, Some(PromotionPiece.Queen), false)
    move.toSANMove(board) should be (SANMove("b8=Q"))
  }

  it should "convert the short castling to the SAN move for the white side" in {
    val board = Board("4k3/8/8/8/8/8/8/4K2R w K - 0 1")
    val move = ShortCastling
    move.toSANMove(board) should be (SANMove("O-O"))
  }

  it should "convert the short castling to the SAN move for the black side" in {
    val board = Board("4k2r/8/8/8/8/8/8/4K3 b k - 0 1")
    val move = ShortCastling
    move.toSANMove(board) should be (SANMove("O-O"))
  }

  it should "convert the long castling to the SAN move for the white side" in {
    val board = Board("4k3/8/8/8/8/8/8/R3K3 w Q - 0 1")
    val move = LongCastling
    move.toSANMove(board) should be (SANMove("O-O-O"))
  }

  it should "convert the long castling to the SAN move for the black side" in {
    val board = Board("r3k3/8/8/8/8/8/8/4K3 b q - 0 1")
    val move = LongCastling
    move.toSANMove(board) should be (SANMove("O-O-O"))
  }

  it should "convert the capture to the SAN move" in {
    val board = Board("4k3/8/8/8/4b3/2N5/8/4K3 w - - 0 1")
    val move = NormalMove(Piece.Knight, C3, E4, None, true)
    move.toSANMove(board) should be (SANMove("Nxe4"))
  }

  it should "convert the capture to the SAN move for the white side and the en passant" in {
    val board = Board("4k3/8/8/2Pp4/8/8/8/4K3 w - d6 0 1")
    val move = NormalMove(Piece.Pawn, C5, D6, None, true)
    move.toSANMove(board) should be (SANMove("cxd6"))
  }

  it should "convert the capture to the SAN move for the black side and the en passant" in {
    val board = Board("4k3/8/8/8/5pP1/8/8/4K3 b - g3 0 1")
    val move = NormalMove(Piece.Pawn, F4, G3, None, true)
    move.toSANMove(board) should be (SANMove("fxg3"))
  }

  it should "convert the move to the SAN move with check" in {
    val board = Board("4k3/8/8/4b3/8/8/8/4K3 b - - 0 1")
    val move = NormalMove(Piece.Bishop, E5, C3, None, false)
    move.toSANMove(board) should be (SANMove("Bc3+"))
  }

  it should "convert the move to the SAN move with checkmate" in {
    val board = Board("4k3/7R/8/8/8/8/7Q/4K3 w - - 0 1")
    val move = NormalMove(Piece.Queen, H2, B8, None, false)
    move.toSANMove(board) should be (SANMove("Qb8#"))
  }
  
  it should "not set the checkOption for the illegal move" in {
    val board = Board("4k3/7R/8/8/7Q/8/8/4K3 w - - 0 1")
    val move = NormalMove(Piece.Queen, H4, B8, None, false)
    move.toSANMove(board) should be (SANMove("Qb8"))
  }
}
