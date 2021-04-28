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

class SANMoveSpec extends AnyFlatSpec with should.Matchers with Inside
{
  "A SANMove.parseSANMove" should "parse the move" in {
    val sanMoveOpt = SANMove.parseSANMove("e4")
    inside(sanMoveOpt) {
      case Some(sanMove) =>
        sanMove should be (SANNormalMove(Piece.Pawn, None, None, E4, None, false, None))
    }
  }

  it should "parse the move with the piece" in {
    val sanMoveOpt = SANMove.parseSANMove("Nc3")
    inside(sanMoveOpt) {
      case Some(sanMove) =>
        sanMove should be (SANNormalMove(Piece.Knight, None, None, C3, None, false, None))
    }
  }

  it should "parse the move with the source column" in {
    val sanMoveOpt = SANMove.parseSANMove("Bbd4")
    inside(sanMoveOpt) {
      case Some(sanMove) =>
        sanMove should be (SANNormalMove(Piece.Bishop, Some(B2 % 8), None, D4, None, false, None))
    }
  }

  it should "parse the move with the source row" in {
    val sanMoveOpt = SANMove.parseSANMove("R1e1")
    inside(sanMoveOpt) {
      case Some(sanMove) =>
        sanMove should be (SANNormalMove(Piece.Rook, None, Some(A1 / 8), E1, None, false, None))
    }
  }

  it should "parse the move with the source square" in {
    val sanMoveOpt = SANMove.parseSANMove("Qd2b4")
    inside(sanMoveOpt) {
      case Some(sanMove) =>
        sanMove should be (SANNormalMove(Piece.Queen, Some(D2 % 8), Some(D2 / 8), B4, None, false, None))
    }
  }

  it should "parse the capture with the piece" in {
    val sanMoveOpt = SANMove.parseSANMove("Bxf4")
    inside(sanMoveOpt) {
      case Some(sanMove) =>
        sanMove should be (SANNormalMove(Piece.Bishop, None, None, F4, None, true, None))
    }
  }

  it should "parse the capture with the source column" in {
    val sanMoveOpt = SANMove.parseSANMove("dxe5")
    inside(sanMoveOpt) {
      case Some(sanMove) =>
        sanMove should be (SANNormalMove(Piece.Pawn, Some(D4 % 8), None, E5, None, true, None))
    }
  }

  it should "parse the capture with the source row" in {
    val sanMoveOpt = SANMove.parseSANMove("B2xe4")
    inside(sanMoveOpt) {
      case Some(sanMove) =>
        sanMove should be (SANNormalMove(Piece.Bishop, None, Some(C2 / 8), E4, None, true, None))
    }
  }

  it should "parse the capture with the source square" in {
    val sanMoveOpt = SANMove.parseSANMove("Nf3xd4")
    inside(sanMoveOpt) {
      case Some(sanMove) =>
        sanMove should be (SANNormalMove(Piece.Knight, Some(F3 % 8), Some(F3 / 8), D4, None, true, None))
    }
  }

  it should "parse the move with the promotion piece" in {
    val sanMoveOpt = SANMove.parseSANMove("e8Q")
    inside(sanMoveOpt) {
      case Some(sanMove) =>
        sanMove should be (SANNormalMove(Piece.Pawn, None, None, E8, Some(PromotionPiece.Queen), false, None))
    }
  }

  it should "parse the capture with the promotion piece" in {
    val sanMoveOpt = SANMove.parseSANMove("dxc1N")
    inside(sanMoveOpt) {
      case Some(sanMove) =>
        sanMove should be (SANNormalMove(Piece.Pawn, Some(D2 % 8), None, C1, Some(PromotionPiece.Knight), true, None))
    }
  }

  it should "parse the move with the promotion piece and the equal character" in {
    val sanMoveOpt = SANMove.parseSANMove("a8=R")
    inside(sanMoveOpt) {
      case Some(sanMove) =>
        sanMove should be (SANNormalMove(Piece.Pawn, None, None, A8, Some(PromotionPiece.Rook), false, None))
    }
  }

  it should "parse the short castling" in {
    val sanMoveOpt = SANMove.parseSANMove("O-O")
    inside(sanMoveOpt) {
      case Some(sanMove) =>
        sanMove should be (SANShortCastling(None))
    }
  }

  it should "parse the long castling" in {
    val sanMoveOpt = SANMove.parseSANMove("O-O-O")
    inside(sanMoveOpt) {
      case Some(sanMove) =>
        sanMove should be (SANLongCastling(None))
    }
  }
  
  it should "parse the move with check" in {
    val sanMoveOpt = SANMove.parseSANMove("Qa4+")
    inside(sanMoveOpt) {
      case Some(sanMove) =>
        sanMove should be (SANNormalMove(Piece.Queen, None, None, A4, None, false, Some(Check.Check)))
    }
  }

  it should "parse the move with checkmate" in {
    val sanMoveOpt = SANMove.parseSANMove("Rc8#")
    inside(sanMoveOpt) {
      case Some(sanMove) =>
        sanMove should be (SANNormalMove(Piece.Rook, None, None, C8, None, false, Some(Check.Checkmate)))
    }
  }

  it should "parse the move with the promotion piece and check" in {
    val sanMoveOpt = SANMove.parseSANMove("e1Q+")
    inside(sanMoveOpt) {
      case Some(sanMove) =>
        sanMove should be (SANNormalMove(Piece.Pawn, None, None, E1, Some(PromotionPiece.Queen), false, Some(Check.Check)))
    }
  }

  it should "parse the move with the promotion piece and checkmate" in {
    val sanMoveOpt = SANMove.parseSANMove("a8R#")
    inside(sanMoveOpt) {
      case Some(sanMove) =>
        sanMove should be (SANNormalMove(Piece.Pawn, None, None, A8, Some(PromotionPiece.Rook), false, Some(Check.Checkmate)))
    }
  }

  it should "parse the castling with check" in {
    val sanMoveOpt = SANMove.parseSANMove("O-O+")
    inside(sanMoveOpt) {
      case Some(sanMove) =>
        sanMove should be (SANShortCastling(Some(Check.Check)))
    }
  }

  it should "parse the castling with checkmate" in {
    val sanMoveOpt = SANMove.parseSANMove("O-O-O#")
    inside(sanMoveOpt) {
      case Some(sanMove) =>
        sanMove should be (SANLongCastling(Some(Check.Checkmate)))
    }
  }

  it should "parse the move with suffix" in {
    val sanMoveOpt = SANMove.parseSANMove("e3!?")
    inside(sanMoveOpt) {
      case Some(sanMove) =>
        sanMove should be (SANNormalMove(Piece.Pawn, None, None, E3, None, false, None))
    }
  }

  it should "parse the move with check and suffix" in {
    val sanMoveOpt = SANMove.parseSANMove("Nf6+!?")
    inside(sanMoveOpt) {
      case Some(sanMove) =>
        sanMove should be (SANNormalMove(Piece.Knight, None, None, F6, None, false, Some(Check.Check)))
    }
  }
  
  it should "complain on the incorrect piece" in {
    val sanMoveOpt = SANMove.parseSANMove("Xc3")
    sanMoveOpt should be (None)
  }

  it should "complain on the incorrect source column or the incorrect source row" in {
    val sanMoveOpt = SANMove.parseSANMove("Nyc3")
    sanMoveOpt should be (None)
  }

  it should "complain on the incorrect destination square" in {
    val sanMoveOpt = SANMove.parseSANMove("B1h0")
    sanMoveOpt should be (None)
  }

  it should "complain on the move without the destination square" in {
    val sanMoveOpt = SANMove.parseSANMove("??")
    sanMoveOpt should be (None)
  }

  it should "complain on the move without the promotion piece with the equal character" in {
    val sanMoveOpt = SANMove.parseSANMove("h8=")
    sanMoveOpt should be (None)
  }

  it should "complain on the incorrect promotion piece" in {
    val sanMoveOpt = SANMove.parseSANMove("e8=P")
    sanMoveOpt should be (None)
  }

  it should "complain on incorrect check" in {
    val sanMoveOpt = SANMove.parseSANMove("a8=Q-")
    sanMoveOpt should be (None)
  }

  "A SANMove.toString" should "convert the move to the string" in {
    val sanMove = SANMove("d4")
    sanMove.toString() should be ("d4")
  }

  it should "convert the move with the piece to the string" in {
    val sanMove = SANMove("Nf3")
    sanMove.toString() should be ("Nf3")
  }

  it should "convert the move with the source column to the string" in {
    val sanMove = SANMove("Bcd3")
    sanMove.toString() should be ("Bcd3")
  }

  it should "convert the move with the source row to the string" in {
    val sanMove = SANMove("R1h2")
    sanMove.toString() should be ("R1h2")
  }

  it should "convert the move with the source square to the string" in {
    val sanMove = SANMove("Nc3e4")
    sanMove.toString() should be ("Nc3e4")
  }

  it should "convert the capture with the piece to the string" in {
    val sanMove = SANMove("Bxe4")
    sanMove.toString() should be ("Bxe4")
  }

  it should "convert the capture with the source column to the string" in {
    val sanMove = SANMove("Raxh1")
    sanMove.toString() should be ("Raxh1")
  }

  it should "convert the capture with the source row to the string" in {
    val sanMove = SANMove("B1xd4")
    sanMove.toString() should be ("B1xd4")
  }
  
  it should "convert the capture with the source square to the string" in {
    val sanMove = SANMove("Nf3xe5")
    sanMove.toString() should be ("Nf3xe5")
  }

  it should "convert the move with the promotion piece to the string" in {
    val sanMove = SANMove("e8=Q")
    sanMove.toString() should be ("e8=Q")
  }

  it should "convert the capture with the promotion piece to the string" in {
    val sanMove = SANMove("dxc1=N")
    sanMove.toString() should be ("dxc1=N")
  }

  it should "convert the short castling to the string" in {
    val sanMove = SANMove("O-O")
    sanMove.toString() should be ("O-O")
  }

  it should "convert the long castling to the string" in {
    val sanMove = SANMove("O-O-O")
    sanMove.toString() should be ("O-O-O")
  }

  it should "convert the move with check to the string" in {
    val sanMove = SANMove("Nd6+")
    sanMove.toString() should be ("Nd6+")
  }

  it should "convert the move with checkmate to the string" in {
    val sanMove = SANMove("Rh8#")
    sanMove.toString() should be ("Rh8#")
  }

  it should "convert the move with the promotion piece and check to the string" in {
    val sanMove = SANMove("e8=Q+")
    sanMove.toString() should be ("e8=Q+")
  }

  it should "convert the move with the promotion piece and checkmate to the string" in {
    val sanMove = SANMove("a1=R#")
    sanMove.toString() should be ("a1=R#")
  }

  it should "convert the short castling with check to the string" in {
    val sanMove = SANMove("O-O+")
    sanMove.toString() should be ("O-O+")
  }

  it should "convert the long castling with checkmate to the string" in {
    val sanMove = SANMove("O-O-O#")
    sanMove.toString() should be ("O-O-O#")
  }
}
