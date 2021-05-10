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
import java.io._
import org.scalatest._
import org.scalatest.flatspec._
import org.scalatest.matchers._
import pl.luckboy.chessmining.chess._
import Squares._

class PGNReaderSpec extends AnyFlatSpec with should.Matchers with Inside
{
  "A PGNReader" should "read the game" in {
    val s =
      "[Event \"Jakis turniej\"]\n" +
      "[Site \"Kowalno\"]\n" +
      "[Date \"2021.??.??\"]\n" +
      "[Round \"1\"]\n" +
      "[White \"Kowalski, Jan\"]\n" +
      "[Black \"Nowak, Piotr\"]\n" +
      "[Result \"*\"]\n" +
      "\n" +
      "1. e4 e5 2. Nc3 Nf6 *\n" +
      "\n"
    val r = new PGNReader(new StringReader(s))
    val gameOptEither = r.readGame()
    inside(gameOptEither) {
      case Right(Some(game)) =>
        game.event should be ("Jakis turniej")
        game.site should be ("Kowalno")
        game.date should be ("2021.??.??")
        game.round should be ("1")
        game.white should have length (1)
        game.white(0) should be ("Kowalski, Jan")
        game.black should have length (1)
        game.black(0) should be ("Nowak, Piotr")
        game.result should be (Result.Unfinished)
        game.eventDateOption should be (None)
        game.whiteEloOption should be (None)
        game.blackEloOption should be (None)
        game.whiteUSCFOption should be (None)
        game.blackUSCFOption should be (None)
        game.ecoOption should be (None)
        game.timeControlOption should be (None)
        game.boardOption should be (None)
        game.movesWithVariations should have length (4)
        game.movesWithVariations(0).move should be (NormalMove(Piece.Pawn, E2, E4, None, false))
        game.movesWithVariations(0).variations shouldBe empty
        game.movesWithVariations(1).move should be (NormalMove(Piece.Pawn, E7, E5, None, false))
        game.movesWithVariations(1).variations shouldBe empty
        game.movesWithVariations(2).move should be (NormalMove(Piece.Knight, B1, C3, None, false))
        game.movesWithVariations(2).variations shouldBe empty
        game.movesWithVariations(3).move should be (NormalMove(Piece.Knight, G8, F6, None, false))
        game.movesWithVariations(3).variations shouldBe empty
    }
  }

  it should "read the game with the comments" in {
    val s =
      "; Some comment\n" +
      "[Event \"Jakis turniej\"]\n" +
      "[Site \"Kowalno\"]\n" +
      "[Date \"2021.??.??\"]\n" +
      "[Round \"2\"]\n" +
      "[White \"Nowak, Pawel\"]\n" +
      "[Black \"Jankowski, Piotr\"]\n" +
      "{\n" +
      "  Second commment\n" +
      "}\n" +
      "[Result \"*\"]\n" +
      "\n" +
      "1. d4 d5 2. Nf3 {Some move} *\n" +
      "\n"
    val r = new PGNReader(new StringReader(s))
    val gameOptEither = r.readGame()
    inside(gameOptEither) {
      case Right(Some(game)) =>
        game.event should be ("Jakis turniej")
        game.site should be ("Kowalno")
        game.date should be ("2021.??.??")
        game.round should be ("2")
        game.white should have length (1)
        game.white(0) should be ("Nowak, Pawel")
        game.black should have length (1)
        game.black(0) should be ("Jankowski, Piotr")
        game.result should be (Result.Unfinished)
        game.eventDateOption should be (None)
        game.whiteEloOption should be (None)
        game.blackEloOption should be (None)
        game.whiteUSCFOption should be (None)
        game.blackUSCFOption should be (None)
        game.ecoOption should be (None)
        game.timeControlOption should be (None)
        game.boardOption should be (None)
        game.movesWithVariations should have length (3)
        game.movesWithVariations(0).move should be (NormalMove(Piece.Pawn, D2, D4, None, false))
        game.movesWithVariations(0).variations shouldBe empty
        game.movesWithVariations(1).move should be (NormalMove(Piece.Pawn, D7, D5, None, false))
        game.movesWithVariations(1).variations shouldBe empty
        game.movesWithVariations(2).move should be (NormalMove(Piece.Knight, G1, F3, None, false))
        game.movesWithVariations(2).variations shouldBe empty
    }
  }

  it should "read the game with the espaces" in {
    val s =
      "%espace1\n" +
      "[Event \"Jakis turniej\"]\n" +
      "[Site \"Kowalno\"]\n" +
      "[Date \"2021.??.??\"]\n" +
      "[Round \"3\"]\n" +
      "[White \"Kowalski, Jan\"]\n" +
      "[Black \"Jankowski, Piotr\"]\n" +
      "%espace2\n" +
      "[Result \"*\"]\n" +
      "\n" +
      "1. d4 d5 2. Nc3 *\n" +
      "\n"
    val r = new PGNReader(new StringReader(s))
    val gameOptEither = r.readGame()
    inside(gameOptEither) {
      case Right(Some(game)) =>
        game.event should be ("Jakis turniej")
        game.site should be ("Kowalno")
        game.date should be ("2021.??.??")
        game.round should be ("3")
        game.white should have length (1)
        game.white(0) should be ("Kowalski, Jan")
        game.black should have length (1)
        game.black(0) should be ("Jankowski, Piotr")
        game.result should be (Result.Unfinished)
        game.eventDateOption should be (None)
        game.whiteEloOption should be (None)
        game.blackEloOption should be (None)
        game.whiteUSCFOption should be (None)
        game.blackUSCFOption should be (None)
        game.ecoOption should be (None)
        game.timeControlOption should be (None)
        game.boardOption should be (None)
        game.movesWithVariations should have length (3)
        game.movesWithVariations(0).move should be (NormalMove(Piece.Pawn, D2, D4, None, false))
        game.movesWithVariations(0).variations shouldBe empty
        game.movesWithVariations(1).move should be (NormalMove(Piece.Pawn, D7, D5, None, false))
        game.movesWithVariations(1).variations shouldBe empty
        game.movesWithVariations(2).move should be (NormalMove(Piece.Knight, B1, C3, None, false))
        game.movesWithVariations(2).variations shouldBe empty
    }
  }

  it should "read the game for the white win" in {
    val s =
      "[Event \"Jakis turniej\"]\n" +
      "[Site \"Kowalno\"]\n" +
      "[Date \"2021.??.??\"]\n" +
      "[Round \"4\"]\n" +
      "[White \"Piotrowski, Pawel\"]\n" +
      "[Black \"Kowalski, Jan\"]\n" +
      "[Result \"1-0\"]\n" +
      "\n" +
      "1. e4 e5 2. Nc3 1-0\n" +
      "\n"
    val r = new PGNReader(new StringReader(s))
    val gameOptEither = r.readGame()
    inside(gameOptEither) {
      case Right(Some(game)) =>
        game.event should be ("Jakis turniej")
        game.site should be ("Kowalno")
        game.date should be ("2021.??.??")
        game.round should be ("4")
        game.white should have length (1)
        game.white(0) should be ("Piotrowski, Pawel")
        game.black should have length (1)
        game.black(0) should be ("Kowalski, Jan")
        game.result should be (Result.WhiteWin)
        game.eventDateOption should be (None)
        game.whiteEloOption should be (None)
        game.blackEloOption should be (None)
        game.whiteUSCFOption should be (None)
        game.blackUSCFOption should be (None)
        game.ecoOption should be (None)
        game.timeControlOption should be (None)
        game.boardOption should be (None)
        game.movesWithVariations should have length (3)
        game.movesWithVariations(0).move should be (NormalMove(Piece.Pawn, E2, E4, None, false))
        game.movesWithVariations(0).variations shouldBe empty
        game.movesWithVariations(1).move should be (NormalMove(Piece.Pawn, E7, E5, None, false))
        game.movesWithVariations(1).variations shouldBe empty
        game.movesWithVariations(2).move should be (NormalMove(Piece.Knight, B1, C3, None, false))
        game.movesWithVariations(2).variations shouldBe empty
    }
  }

  it should "read the game for the black win" in {
    val s =
      "[Event \"Jakis turniej\"]\n" +
      "[Site \"Kowalno\"]\n" +
      "[Date \"2021.??.??\"]\n" +
      "[Round \"5\"]\n" +
      "[White \"Nowak, Piotr\"]\n" +
      "[Black \"Kowalski, Jan\"]\n" +
      "[Result \"0-1\"]\n" +
      "\n" +
      "1. e4 e5 2. Nc3 Bc5 0-1\n" +
      "\n"
    val r = new PGNReader(new StringReader(s))
    val gameOptEither = r.readGame()
    inside(gameOptEither) {
      case Right(Some(game)) =>
        game.event should be ("Jakis turniej")
        game.site should be ("Kowalno")
        game.date should be ("2021.??.??")
        game.round should be ("5")
        game.white should have length (1)
        game.white(0) should be ("Nowak, Piotr")
        game.black should have length (1)
        game.black(0) should be ("Kowalski, Jan")
        game.result should be (Result.BlackWin)
        game.eventDateOption should be (None)
        game.whiteEloOption should be (None)
        game.blackEloOption should be (None)
        game.whiteUSCFOption should be (None)
        game.blackUSCFOption should be (None)
        game.ecoOption should be (None)
        game.timeControlOption should be (None)
        game.boardOption should be (None)
        game.movesWithVariations should have length (4)
        game.movesWithVariations(0).move should be (NormalMove(Piece.Pawn, E2, E4, None, false))
        game.movesWithVariations(0).variations shouldBe empty
        game.movesWithVariations(1).move should be (NormalMove(Piece.Pawn, E7, E5, None, false))
        game.movesWithVariations(1).variations shouldBe empty
        game.movesWithVariations(2).move should be (NormalMove(Piece.Knight, B1, C3, None, false))
        game.movesWithVariations(2).variations shouldBe empty
        game.movesWithVariations(3).move should be (NormalMove(Piece.Bishop, F8, C5, None, false))
        game.movesWithVariations(3).variations shouldBe empty
    }
  }

  it should "read the game for the draw" in {
    val s =
      "[Event \"Jakis turniej\"]\n" +
      "[Site \"Kowalno\"]\n" +
      "[Date \"2021.??.??\"]\n" +
      "[Round \"6\"]\n" +
      "[White \"Piotrowski, Pawel\"]\n" +
      "[Black \"Nowak, Piotr\"]\n" +
      "[Result \"1/2-1/2\"]\n" +
      "\n" +
      "1. d4 e5 2. Nc3 Bb4 1/2-1/2\n" +
      "\n"
    val r = new PGNReader(new StringReader(s))
    val gameOptEither = r.readGame()
    inside(gameOptEither) {
      case Right(Some(game)) =>
        game.event should be ("Jakis turniej")
        game.site should be ("Kowalno")
        game.date should be ("2021.??.??")
        game.round should be ("6")
        game.white should have length (1)
        game.white(0) should be ("Piotrowski, Pawel")
        game.black should have length (1)
        game.black(0) should be ("Nowak, Piotr")
        game.result should be (Result.Draw)
        game.eventDateOption should be (None)
        game.whiteEloOption should be (None)
        game.blackEloOption should be (None)
        game.whiteUSCFOption should be (None)
        game.blackUSCFOption should be (None)
        game.ecoOption should be (None)
        game.timeControlOption should be (None)
        game.boardOption should be (None)
        game.movesWithVariations should have length (4)
        game.movesWithVariations(0).move should be (NormalMove(Piece.Pawn, D2, D4, None, false))
        game.movesWithVariations(0).variations shouldBe empty
        game.movesWithVariations(1).move should be (NormalMove(Piece.Pawn, E7, E5, None, false))
        game.movesWithVariations(1).variations shouldBe empty
        game.movesWithVariations(2).move should be (NormalMove(Piece.Knight, B1, C3, None, false))
        game.movesWithVariations(2).variations shouldBe empty
        game.movesWithVariations(3).move should be (NormalMove(Piece.Bishop, F8, B4, None, false))
        game.movesWithVariations(3).variations shouldBe empty
    }
  }

  it should "read the game for the unfinished game" in {
    val s =
      "[Event \"Jakis turniej\"]\n" +
      "[Site \"Kowalno\"]\n" +
      "[Date \"2021.??.??\"]\n" +
      "[Round \"7\"]\n" +
      "[White \"Piotrowski, Pawel\"]\n" +
      "[Black \"Jankowski, Piotr\"]\n" +
      "[Result \"*\"]\n" +
      "\n" +
      "1. e4 e5 2. Nc3 Nc6 3. Qf3*\n" +
      "\n"
    val r = new PGNReader(new StringReader(s))
    val gameOptEither = r.readGame()
    inside(gameOptEither) {
      case Right(Some(game)) =>
        game.event should be ("Jakis turniej")
        game.site should be ("Kowalno")
        game.date should be ("2021.??.??")
        game.round should be ("7")
        game.white should have length (1)
        game.white(0) should be ("Piotrowski, Pawel")
        game.black should have length (1)
        game.black(0) should be ("Jankowski, Piotr")
        game.result should be (Result.Unfinished)
        game.eventDateOption should be (None)
        game.whiteEloOption should be (None)
        game.blackEloOption should be (None)
        game.whiteUSCFOption should be (None)
        game.blackUSCFOption should be (None)
        game.ecoOption should be (None)
        game.timeControlOption should be (None)
        game.boardOption should be (None)
        game.movesWithVariations should have length (5)
        game.movesWithVariations(0).move should be (NormalMove(Piece.Pawn, E2, E4, None, false))
        game.movesWithVariations(0).variations shouldBe empty
        game.movesWithVariations(1).move should be (NormalMove(Piece.Pawn, E7, E5, None, false))
        game.movesWithVariations(1).variations shouldBe empty
        game.movesWithVariations(2).move should be (NormalMove(Piece.Knight, B1, C3, None, false))
        game.movesWithVariations(2).variations shouldBe empty
        game.movesWithVariations(3).move should be (NormalMove(Piece.Knight, B8, C6, None, false))
        game.movesWithVariations(3).variations shouldBe empty
        game.movesWithVariations(4).move should be (NormalMove(Piece.Queen, D1, F3, None, false))
        game.movesWithVariations(4).variations shouldBe empty
    }
  }

  it should "read the game with the optional fields" in {
    val s =
      "[Event \"Jakies zdarzenie\"]\n" +
      "[Site \"Nowakowo\"]\n" +
      "[Date \"2021.04.02\"]\n" +
      "[Round \"1\"]\n" +
      "[White \"Kowalski, Jan\"]\n" +
      "[Black \"Nowak, Piotr\"]\n" +
      "[Result \"*\"]\n" +
      "[EventDate \"2021.04.01\"]\n" +
      "[WhiteElo \"1200\"]\n" +
      "[BlackElo \"1300\"]\n" +
      "[WhiteUSCF \"1400\"]\n" +
      "[BlackUSCF \"1500\"]\n" +
      "[ECO \"C25g\"]\n" +
      "[TimeControl \"600\"]\n" +
      "\n" +
      "1. e4 e5 2. Nc3 Nf6 *\n" +
      "\n"
    val r = new PGNReader(new StringReader(s))
    val gameOptEither = r.readGame()
    inside(gameOptEither) {
      case Right(Some(game)) =>
        game.event should be ("Jakies zdarzenie")
        game.site should be ("Nowakowo")
        game.date should be ("2021.04.02")
        game.round should be ("1")
        game.white should have length (1)
        game.white(0) should be ("Kowalski, Jan")
        game.black should have length (1)
        game.black(0) should be ("Nowak, Piotr")
        game.result should be (Result.Unfinished)
        inside(game.eventDateOption) {
          case Some(eventDate) =>
            eventDate should be ("2021.04.01")
        }
        inside(game.whiteEloOption) {
          case Some(whiteElo) =>
            whiteElo should be (Vector(Some(1200)))
        }
        inside(game.blackEloOption) {
          case Some(blackElo) =>
            blackElo should be (Vector(Some(1300)))
        }
        inside(game.whiteUSCFOption) {
          case Some(whiteUSCF) =>
            whiteUSCF should be (Vector(Some(1400)))
        }
        inside(game.blackUSCFOption) {
          case Some(blackUSCF) =>
            blackUSCF should be (Vector(Some(1500)))
        }
        inside(game.ecoOption) {
          case Some(eco) =>
            eco should be ("C25g")
        }
        inside(game.timeControlOption) {
          case Some(timeControl) =>
            timeControl should be (Vector(Some(SuddenDeathTimeControl(600))))
        }
        game.boardOption should be (None)
        game.movesWithVariations should have length (4)
        game.movesWithVariations(0).move should be (NormalMove(Piece.Pawn, E2, E4, None, false))
        game.movesWithVariations(0).variations shouldBe empty
        game.movesWithVariations(1).move should be (NormalMove(Piece.Pawn, E7, E5, None, false))
        game.movesWithVariations(1).variations shouldBe empty
        game.movesWithVariations(2).move should be (NormalMove(Piece.Knight, B1, C3, None, false))
        game.movesWithVariations(2).variations shouldBe empty
        game.movesWithVariations(3).move should be (NormalMove(Piece.Knight, G8, F6, None, false))
        game.movesWithVariations(3).variations shouldBe empty
    }
  }

  it should "read the game with four players for two sides" in {
    val s =
      "[Event \"Jakies zdarzenie\"]\n" +
      "[Site \"Nowakowo\"]\n" +
      "[Date \"2021.04.03\"]\n" +
      "[Round \"2\"]\n" +
      "[White \"Nowak, Piotr:Pomagier, Jan\"]\n" +
      "[Black \"Jankowski, Piotr:Pomagacz, Pawel\"]\n" +
      "[Result \"*\"]\n" +
      "[WhiteElo \"1200:-\"]\n" +
      "[BlackElo \"-:1300\"]\n" +
      "[WhiteUSCF \"1400:-\"]\n" +
      "[BlackUSCF \"-:1500\"]\n" +
      "\n" +
      "1. e4 e5 2. Nc3 Nf6 *\n" +
      "\n"
    val r = new PGNReader(new StringReader(s))
    val gameOptEither = r.readGame()
    inside(gameOptEither) {
      case Right(Some(game)) =>
        game.event should be ("Jakies zdarzenie")
        game.site should be ("Nowakowo")
        game.date should be ("2021.04.03")
        game.round should be ("2")
        game.white should have length (2)
        game.white(0) should be ("Nowak, Piotr")
        game.white(1) should be ("Pomagier, Jan")
        game.black should have length (2)
        game.black(0) should be ("Jankowski, Piotr")
        game.black(1) should be ("Pomagacz, Pawel")
        game.result should be (Result.Unfinished)
        game.eventDateOption should be (None)
        inside(game.whiteEloOption) {
          case Some(whiteElo) =>
            whiteElo should be (Vector(Some(1200), None))
        }
        inside(game.blackEloOption) {
          case Some(blackElo) =>
            blackElo should be (Vector(None, Some(1300)))
        }
        inside(game.whiteUSCFOption) {
          case Some(whiteUSCF) =>
            whiteUSCF should be (Vector(Some(1400), None))
        }
        inside(game.blackUSCFOption) {
          case Some(blackUSCF) =>
            blackUSCF should be (Vector(None, Some(1500)))
        }
        game.ecoOption should be (None)
        game.timeControlOption should be (None)
        game.boardOption should be (None)
        game.movesWithVariations should have length (4)
        game.movesWithVariations(0).move should be (NormalMove(Piece.Pawn, E2, E4, None, false))
        game.movesWithVariations(0).variations shouldBe empty
        game.movesWithVariations(1).move should be (NormalMove(Piece.Pawn, E7, E5, None, false))
        game.movesWithVariations(1).variations shouldBe empty
        game.movesWithVariations(2).move should be (NormalMove(Piece.Knight, B1, C3, None, false))
        game.movesWithVariations(2).variations shouldBe empty
        game.movesWithVariations(3).move should be (NormalMove(Piece.Knight, G8, F6, None, false))
        game.movesWithVariations(3).variations shouldBe empty
    }
  }
  
  it should "read the game with all time control kinds" in {
    val s =
      "[Event \"Jakies zdarzenie\"]\n" +
      "[Site \"Nowakowo\"]\n" +
      "[Date \"2021.04.02\"]\n" +
      "[Round \"3\"]\n" +
      "[White \"Piotrowski, Pawel\"]\n" +
      "[Black \"Jankoswski, Piotr\"]\n" +
      "[Result \"*\"]\n" +
      "[TimeControl \"?:-:40/9000:300:4500+60:*180\"]\n" +
      "\n" +
      "1. e4 e5 2. Nc3 Nc6 *\n" +
      "\n"
    val r = new PGNReader(new StringReader(s))
    val gameOptEither = r.readGame()
    inside(gameOptEither) {
      case Right(Some(game)) =>
        game.event should be ("Jakies zdarzenie")
        game.site should be ("Nowakowo")
        game.date should be ("2021.04.02")
        game.round should be ("3")
        game.white should have length (1)
        game.white(0) should be ("Piotrowski, Pawel")
        game.black should have length (1)
        game.black(0) should be ("Jankoswski, Piotr")
        game.result should be (Result.Unfinished)
        game.eventDateOption should be (None)
        game.whiteEloOption should be (None)
        game.blackEloOption should be (None)
        game.whiteUSCFOption should be (None)
        game.blackUSCFOption should be (None)
        game.ecoOption should be (None)
        inside(game.timeControlOption) {
          case Some(timeControl) =>
            timeControl should have length (6)
            timeControl(0) should be (Some(UnknownTimeControl))
            timeControl(1) should be (None)
            timeControl(2) should be (Some(ClassicalTimeControl(40, 9000)))
            timeControl(3) should be (Some(SuddenDeathTimeControl(300)))
            timeControl(4) should be (Some(IncrementalTimeControl(4500, 60)))
            timeControl(5) should be (Some(HourglassTimeControl(180)))
        }
        game.boardOption should be (None)
        game.movesWithVariations should have length (4)
        game.movesWithVariations(0).move should be (NormalMove(Piece.Pawn, E2, E4, None, false))
        game.movesWithVariations(0).variations shouldBe empty
        game.movesWithVariations(1).move should be (NormalMove(Piece.Pawn, E7, E5, None, false))
        game.movesWithVariations(1).variations shouldBe empty
        game.movesWithVariations(2).move should be (NormalMove(Piece.Knight, B1, C3, None, false))
        game.movesWithVariations(2).variations shouldBe empty
        game.movesWithVariations(3).move should be (NormalMove(Piece.Knight, B8, C6, None, false))
        game.movesWithVariations(3).variations shouldBe empty
    }
  }
  
  it should "read the game for the FEN and the white side" in {
    val s =
      "[Event \"Drugi turniej\"]\n" +
      "[Site \"Jankowo\"]\n" +
      "[Date \"2021.??.??\"]\n" +
      "[Round \"1\"]\n" +
      "[White \"Kowalski, Jan\"]\n" +
      "[Black \"Nowak, Piotr\"]\n" +
      "[Result \"*\"]\n" +
      "[FEN \"rnbqkbnr/pppp1ppp/8/4p3/4P3/8/PPPP1PPP/RNBQKBNR w KQkq - 0 2\"]\n" +
      "\n" +
      "2. Nc3 Nf6 3. Bc4 Bc5 *\n" +
      "\n"
    val r = new PGNReader(new StringReader(s))
    val gameOptEither = r.readGame()
    inside(gameOptEither) {
      case Right(Some(game)) =>
        game.event should be ("Drugi turniej")
        game.site should be ("Jankowo")
        game.date should be ("2021.??.??")
        game.round should be ("1")
        game.white should have length (1)
        game.white(0) should be ("Kowalski, Jan")
        game.black should have length (1)
        game.black(0) should be ("Nowak, Piotr")
        game.result should be (Result.Unfinished)
        game.eventDateOption should be (None)
        game.whiteEloOption should be (None)
        game.blackEloOption should be (None)
        game.whiteUSCFOption should be (None)
        game.blackUSCFOption should be (None)
        game.ecoOption should be (None)
        game.timeControlOption should be (None)
        inside(game.boardOption) {
          case Some(board) =>
            board should be (Board("rnbqkbnr/pppp1ppp/8/4p3/4P3/8/PPPP1PPP/RNBQKBNR w KQkq - 0 2"))
        }
        game.movesWithVariations should have length (4)
        game.movesWithVariations(0).move should be (NormalMove(Piece.Knight, B1, C3, None, false))
        game.movesWithVariations(0).variations shouldBe empty
        game.movesWithVariations(1).move should be (NormalMove(Piece.Knight, G8, F6, None, false))
        game.movesWithVariations(1).variations shouldBe empty
        game.movesWithVariations(2).move should be (NormalMove(Piece.Bishop, F1, C4, None, false))
        game.movesWithVariations(2).variations shouldBe empty
        game.movesWithVariations(3).move should be (NormalMove(Piece.Bishop, F8, C5, None, false))
        game.movesWithVariations(3).variations shouldBe empty
    }
  }

  it should "read the game for the FEN and the black side" in {
    val s =
      "[Event \"Drugi turniej\"]\n" +
      "[Site \"Jankowo\"]\n" +
      "[Date \"2021.??.??\"]\n" +
      "[Round \"2\"]\n" +
      "[White \"Nowak, Piotr\"]\n" +
      "[Black \"Jankowski, Piotr\"]\n" +
      "[Result \"*\"]\n" +
      "[FEN \"rnbqkbnr/pppp1ppp/8/4p3/4P3/5N2/PPPP1PPP/RNBQKB1R b KQkq - 0 2\"]\n" +
      "\n" +
      "2... Nc6 3. Nc3 Nf6 *\n" +
      "\n"
    val r = new PGNReader(new StringReader(s))
    val gameOptEither = r.readGame()
    inside(gameOptEither) {
      case Right(Some(game)) =>
        game.event should be ("Drugi turniej")
        game.site should be ("Jankowo")
        game.date should be ("2021.??.??")
        game.round should be ("2")
        game.white should have length (1)
        game.white(0) should be ("Nowak, Piotr")
        game.black should have length (1)
        game.black(0) should be ("Jankowski, Piotr")
        game.result should be (Result.Unfinished)
        game.eventDateOption should be (None)
        game.whiteEloOption should be (None)
        game.blackEloOption should be (None)
        game.whiteUSCFOption should be (None)
        game.blackUSCFOption should be (None)
        game.ecoOption should be (None)
        game.timeControlOption should be (None)
        inside(game.boardOption) {
          case Some(board) =>
            board should be (Board("rnbqkbnr/pppp1ppp/8/4p3/4P3/5N2/PPPP1PPP/RNBQKB1R b KQkq - 0 2"))
        }
        game.movesWithVariations should have length (3)
        game.movesWithVariations(0).move should be (NormalMove(Piece.Knight, B8, C6, None, false))
        game.movesWithVariations(0).variations shouldBe empty
        game.movesWithVariations(1).move should be (NormalMove(Piece.Knight, B1, C3, None, false))
        game.movesWithVariations(1).variations shouldBe empty
        game.movesWithVariations(2).move should be (NormalMove(Piece.Knight, G8, F6, None, false))
        game.movesWithVariations(2).variations shouldBe empty
    }
  }

  it should "read the game with the variation for the white side" in {
    val s =
      "[Event \"Drugie zdarzenie\"]\n" +
      "[Site \"Piotrkowo\"]\n" +
      "[Date \"2021.??.??\"]\n" +
      "[Round \"1\"]\n" +
      "[White \"Kowalski, Jan\"]\n" +
      "[Black \"Nowak, Piotr\"]\n" +
      "[Result \"*\"]\n" +
      "\n" +
      "1. e4 e5 2. Nf3 ( 2. Nc3 Nf6 3. Nf3 ) *\n" +
      "\n"
    val r = new PGNReader(new StringReader(s))
    val gameOptEither = r.readGame()
    inside(gameOptEither) {
      case Right(Some(game)) =>
        game.event should be ("Drugie zdarzenie")
        game.site should be ("Piotrkowo")
        game.date should be ("2021.??.??")
        game.round should be ("1")
        game.white should have length (1)
        game.white(0) should be ("Kowalski, Jan")
        game.black should have length (1)
        game.black(0) should be ("Nowak, Piotr")
        game.result should be (Result.Unfinished)
        game.eventDateOption should be (None)
        game.whiteEloOption should be (None)
        game.blackEloOption should be (None)
        game.whiteUSCFOption should be (None)
        game.blackUSCFOption should be (None)
        game.ecoOption should be (None)
        game.timeControlOption should be (None)
        game.boardOption should be (None)
        game.movesWithVariations should have length (3)
        game.movesWithVariations(0).move should be (NormalMove(Piece.Pawn, E2, E4, None, false))
        game.movesWithVariations(0).variations shouldBe empty
        game.movesWithVariations(1).move should be (NormalMove(Piece.Pawn, E7, E5, None, false))
        game.movesWithVariations(1).variations shouldBe empty
        game.movesWithVariations(2).move should be (NormalMove(Piece.Knight, G1, F3, None, false))
        game.movesWithVariations(2).variations should have length (1)
        game.movesWithVariations(2).variations(0) should have length (3)
        game.movesWithVariations(2).variations(0)(0).move should be (NormalMove(Piece.Knight, B1, C3, None, false))
        game.movesWithVariations(2).variations(0)(0).variations shouldBe empty
        game.movesWithVariations(2).variations(0)(1).move should be (NormalMove(Piece.Knight, G8, F6, None, false))
        game.movesWithVariations(2).variations(0)(1).variations shouldBe empty
        game.movesWithVariations(2).variations(0)(2).move should be (NormalMove(Piece.Knight, G1, F3, None, false))
        game.movesWithVariations(2).variations(0)(2).variations shouldBe empty
    }
  }

  it should "read the game with the variation for the black side" in {
    val s =
      "[Event \"Drugie zdarzenie\"]\n" +
      "[Site \"Piotrkowo\"]\n" +
      "[Date \"2021.??.??\"]\n" +
      "[Round \"2\"]\n" +
      "[White \"Nowak, Piotr\"]\n" +
      "[Black \"Jankowski, Piotr\"]\n" +
      "[Result \"*\"]\n" +
      "\n" +
      "1. e4 e5 2. Nf3 Nc6 ( 2... Nf6 3. Nc3 Nc6 ) *\n" +
      "\n"
    val r = new PGNReader(new StringReader(s))
    val gameOptEither = r.readGame()
    inside(gameOptEither) {
      case Right(Some(game)) =>
        game.event should be ("Drugie zdarzenie")
        game.site should be ("Piotrkowo")
        game.date should be ("2021.??.??")
        game.round should be ("2")
        game.white should have length (1)
        game.white(0) should be ("Nowak, Piotr")
        game.black should have length (1)
        game.black(0) should be ("Jankowski, Piotr")
        game.result should be (Result.Unfinished)
        game.eventDateOption should be (None)
        game.whiteEloOption should be (None)
        game.blackEloOption should be (None)
        game.whiteUSCFOption should be (None)
        game.blackUSCFOption should be (None)
        game.ecoOption should be (None)
        game.timeControlOption should be (None)
        game.boardOption should be (None)
        game.movesWithVariations should have length (4)
        game.movesWithVariations(0).move should be (NormalMove(Piece.Pawn, E2, E4, None, false))
        game.movesWithVariations(0).variations shouldBe empty
        game.movesWithVariations(1).move should be (NormalMove(Piece.Pawn, E7, E5, None, false))
        game.movesWithVariations(1).variations shouldBe empty
        game.movesWithVariations(2).move should be (NormalMove(Piece.Knight, G1, F3, None, false))
        game.movesWithVariations(2).variations shouldBe empty
        game.movesWithVariations(3).move should be (NormalMove(Piece.Knight, B8, C6, None, false))
        game.movesWithVariations(3).variations should have length (1)
        game.movesWithVariations(3).variations(0) should have length (3)
        game.movesWithVariations(3).variations(0)(0).move should be (NormalMove(Piece.Knight, G8, F6, None, false))
        game.movesWithVariations(3).variations(0)(0).variations shouldBe empty
        game.movesWithVariations(3).variations(0)(1).move should be (NormalMove(Piece.Knight, B1, C3, None, false))
        game.movesWithVariations(3).variations(0)(1).variations shouldBe empty
        game.movesWithVariations(3).variations(0)(2).move should be (NormalMove(Piece.Knight, B8, C6, None, false))
        game.movesWithVariations(3).variations(0)(2).variations shouldBe empty
    }
  }

  it should "read the game with two variations for same move" in {
    val s =
      "[Event \"Drugie zdarzenie\"]\n" +
      "[Site \"Piotrkowo\"]\n" +
      "[Date \"2021.??.??\"]\n" +
      "[Round \"3\"]\n" +
      "[White \"Nowak, Piotr\"]\n" +
      "[Black \"Pawelski, Jan\"]\n" +
      "[Result \"*\"]\n" +
      "\n" +
      "1. e4 e5 2. Bc4 ( 2. Nf3 Nf6 ) ( 2. Nc3 Nc6 ) 2... Bc5 *\n" +
      "\n"
    val r = new PGNReader(new StringReader(s))
    val gameOptEither = r.readGame()
    inside(gameOptEither) {
      case Right(Some(game)) =>
        game.event should be ("Drugie zdarzenie")
        game.site should be ("Piotrkowo")
        game.date should be ("2021.??.??")
        game.round should be ("3")
        game.white should have length (1)
        game.white(0) should be ("Nowak, Piotr")
        game.black should have length (1)
        game.black(0) should be ("Pawelski, Jan")
        game.result should be (Result.Unfinished)
        game.eventDateOption should be (None)
        game.whiteEloOption should be (None)
        game.blackEloOption should be (None)
        game.whiteUSCFOption should be (None)
        game.blackUSCFOption should be (None)
        game.ecoOption should be (None)
        game.timeControlOption should be (None)
        game.boardOption should be (None)
        game.movesWithVariations should have length (4)
        game.movesWithVariations(0).move should be (NormalMove(Piece.Pawn, E2, E4, None, false))
        game.movesWithVariations(0).variations shouldBe empty
        game.movesWithVariations(1).move should be (NormalMove(Piece.Pawn, E7, E5, None, false))
        game.movesWithVariations(1).variations shouldBe empty
        game.movesWithVariations(2).move should be (NormalMove(Piece.Bishop, F1, C4, None, false))
        game.movesWithVariations(2).variations should have length (2)
        game.movesWithVariations(2).variations(0) should have length (2)
        game.movesWithVariations(2).variations(0)(0).move should be (NormalMove(Piece.Knight, G1, F3, None, false))
        game.movesWithVariations(2).variations(0)(0).variations shouldBe empty
        game.movesWithVariations(2).variations(0)(1).move should be (NormalMove(Piece.Knight, G8, F6, None, false))
        game.movesWithVariations(2).variations(0)(1).variations shouldBe empty
        game.movesWithVariations(2).variations(1) should have length (2)
        game.movesWithVariations(2).variations(1)(0).move should be (NormalMove(Piece.Knight, B1, C3, None, false))
        game.movesWithVariations(2).variations(1)(0).variations shouldBe empty
        game.movesWithVariations(2).variations(1)(1).move should be (NormalMove(Piece.Knight, B8, C6, None, false))
        game.movesWithVariations(2).variations(1)(1).variations shouldBe empty
        game.movesWithVariations(3).move should be (NormalMove(Piece.Bishop, F8, C5, None, false))
        game.movesWithVariations(3).variations shouldBe empty
    }
  }

  it should "read the game with the subvariation" in {
    val s =
      "[Event \"Drugie zdarzenie\"]\n" +
      "[Site \"Piotrkowo\"]\n" +
      "[Date \"2021.??.??\"]\n" +
      "[Round \"4\"]\n" +
      "[White \"Jankowski, Piotr\"]\n" +
      "[Black \"Kowalski, Jan\"]\n" +
      "[Result \"*\"]\n" +
      "\n" +
      "1. Nc3 Nf6 2. e4 e5 ( 2... Nc6 3. Bb5 ( 3. Nf3 ) 3... e5 ) 3. Nf3 *\n" +
      "\n"
    val r = new PGNReader(new StringReader(s))
    val gameOptEither = r.readGame()
    inside(gameOptEither) {
      case Right(Some(game)) =>
        game.event should be ("Drugie zdarzenie")
        game.site should be ("Piotrkowo")
        game.date should be ("2021.??.??")
        game.round should be ("4")
        game.white should have length (1)
        game.white(0) should be ("Jankowski, Piotr")
        game.black should have length (1)
        game.black(0) should be ("Kowalski, Jan")
        game.result should be (Result.Unfinished)
        game.eventDateOption should be (None)
        game.whiteEloOption should be (None)
        game.blackEloOption should be (None)
        game.whiteUSCFOption should be (None)
        game.blackUSCFOption should be (None)
        game.ecoOption should be (None)
        game.timeControlOption should be (None)
        game.boardOption should be (None)
        game.movesWithVariations should have length (5)
        game.movesWithVariations(0).move should be (NormalMove(Piece.Knight, B1, C3, None, false))
        game.movesWithVariations(0).variations shouldBe empty
        game.movesWithVariations(1).move should be (NormalMove(Piece.Knight, G8, F6, None, false))
        game.movesWithVariations(1).variations shouldBe empty
        game.movesWithVariations(2).move should be (NormalMove(Piece.Pawn, E2, E4, None, false))
        game.movesWithVariations(2).variations shouldBe empty
        game.movesWithVariations(3).move should be (NormalMove(Piece.Pawn, E7, E5, None, false))
        game.movesWithVariations(3).variations should have length (1)
        game.movesWithVariations(3).variations(0) should have length (3)
        game.movesWithVariations(3).variations(0)(0).move should be (NormalMove(Piece.Knight, B8, C6, None, false))
        game.movesWithVariations(3).variations(0)(0).variations shouldBe empty
        game.movesWithVariations(3).variations(0)(1).move should be (NormalMove(Piece.Bishop, F1, B5, None, false))
        game.movesWithVariations(3).variations(0)(1).variations should have length (1)
        game.movesWithVariations(3).variations(0)(1).variations(0) should have length (1)
        game.movesWithVariations(3).variations(0)(1).variations(0)(0).move should be (NormalMove(Piece.Knight, G1, F3, None, false))
        game.movesWithVariations(3).variations(0)(1).variations(0)(0).variations shouldBe empty
        game.movesWithVariations(3).variations(0)(2).move should be (NormalMove(Piece.Pawn, E7, E5, None, false))
        game.movesWithVariations(3).variations(0)(2).variations shouldBe empty
        game.movesWithVariations(4).move should be (NormalMove(Piece.Knight, G1, F3, None, false))
        game.movesWithVariations(4).variations shouldBe empty
    }
  }

  it should "read the game with the numeric annotation glyph" in {
    val s =
      "[Event \"Drugie zdarzenie\"]\n" +
      "[Site \"Piotrkowo\"]\n" +
      "[Date \"2021.??.??\"]\n" +
      "[Round \"5\"]\n" +
      "[White \"Kowalski, Jan\"]\n" +
      "[Black \"Jankowski, Piotr\"]\n" +
      "[Result \"*\"]\n" +
      "\n" +
      "1. e4 e5 2. Nf3 $14 Nc6 *\n" +
      "\n"
    val r = new PGNReader(new StringReader(s))
    val gameOptEither = r.readGame()
    inside(gameOptEither) {
      case Right(Some(game)) =>
        game.event should be ("Drugie zdarzenie")
        game.site should be ("Piotrkowo")
        game.date should be ("2021.??.??")
        game.round should be ("5")
        game.white should have length (1)
        game.white(0) should be ("Kowalski, Jan")
        game.black should have length (1)
        game.black(0) should be ("Jankowski, Piotr")
        game.result should be (Result.Unfinished)
        game.eventDateOption should be (None)
        game.whiteEloOption should be (None)
        game.blackEloOption should be (None)
        game.whiteUSCFOption should be (None)
        game.blackUSCFOption should be (None)
        game.ecoOption should be (None)
        game.timeControlOption should be (None)
        game.boardOption should be (None)
        game.movesWithVariations should have length (4)
        game.movesWithVariations(0).move should be (NormalMove(Piece.Pawn, E2, E4, None, false))
        game.movesWithVariations(0).variations shouldBe empty
        game.movesWithVariations(1).move should be (NormalMove(Piece.Pawn, E7, E5, None, false))
        game.movesWithVariations(1).variations shouldBe empty
        game.movesWithVariations(2).move should be (NormalMove(Piece.Knight, G1, F3, None, false))
        game.movesWithVariations(2).variations shouldBe empty
        game.movesWithVariations(3).move should be (NormalMove(Piece.Knight, B8, C6, None, false))
        game.movesWithVariations(3).variations shouldBe empty
    }
  }

  it should "read the game with the numeric annotation glyph and the variation" in {
    val s =
      "[Event \"Drugie zdarzenie\"]\n" +
      "[Site \"Piotrkowo\"]\n" +
      "[Date \"2021.??.??\"]\n" +
      "[Round \"6\"]\n" +
      "[White \"Kowalski, Jan\"]\n" +
      "[Black \"Pawelski, Jan\"]\n" +
      "[Result \"*\"]\n" +
      "\n" +
      "1. e4 e5 2. Nc3 $1 ( 2.  Nf3 ) 2... Nf6 *\n" +
      "\n"
    val r = new PGNReader(new StringReader(s))
    val gameOptEither = r.readGame()
    inside(gameOptEither) {
      case Right(Some(game)) =>
        game.event should be ("Drugie zdarzenie")
        game.site should be ("Piotrkowo")
        game.date should be ("2021.??.??")
        game.round should be ("6")
        game.white should have length (1)
        game.white(0) should be ("Kowalski, Jan")
        game.black should have length (1)
        game.black(0) should be ("Pawelski, Jan")
        game.result should be (Result.Unfinished)
        game.eventDateOption should be (None)
        game.whiteEloOption should be (None)
        game.blackEloOption should be (None)
        game.whiteUSCFOption should be (None)
        game.blackUSCFOption should be (None)
        game.ecoOption should be (None)
        game.timeControlOption should be (None)
        game.boardOption should be (None)
        game.movesWithVariations should have length (4)
        game.movesWithVariations(0).move should be (NormalMove(Piece.Pawn, E2, E4, None, false))
        game.movesWithVariations(0).variations shouldBe empty
        game.movesWithVariations(1).move should be (NormalMove(Piece.Pawn, E7, E5, None, false))
        game.movesWithVariations(1).variations shouldBe empty
        game.movesWithVariations(2).move should be (NormalMove(Piece.Knight, B1, C3, None, false))
        game.movesWithVariations(2).variations should have length (1)
        game.movesWithVariations(2).variations(0) should have length (1)
        game.movesWithVariations(2).variations(0)(0).move should be (NormalMove(Piece.Knight, G1, F3, None, false))
        game.movesWithVariations(2).variations(0)(0).variations shouldBe empty
        game.movesWithVariations(3).move should be (NormalMove(Piece.Knight, G8, F6, None, false))
        game.movesWithVariations(3).variations shouldBe empty
    }
  }

    it should "read the game for the move characters greater than 80" in {
    val s =
      "[Event \"Drugie zdarzenie\"]\n" +
      "[Site \"Piotrkowo\"]\n" +
      "[Date \"2021.??.??\"]\n" +
      "[Round \"7\"]\n" +
      "[White \"Nowak, Piotr\"]\n" +
      "[Black \"Pawelski, Jan\"]\n" +
      "[Result \"*\"]\n" +
      "\n" +
      "1. e4 e5 2. Nf3 Nf6 3. Nc3 Nc6 4. Bb5 Bb4 5. O-O O-O 6. Qe2 Qe7 7. d3\n" +
      "d6 *\n" +
      "\n"
    val r = new PGNReader(new StringReader(s))
    val gameOptEither = r.readGame()
    inside(gameOptEither) {
      case Right(Some(game)) =>
        game.event should be ("Drugie zdarzenie")
        game.site should be ("Piotrkowo")
        game.date should be ("2021.??.??")
        game.round should be ("7")
        game.white should have length (1)
        game.white(0) should be ("Nowak, Piotr")
        game.black should have length (1)
        game.black(0) should be ("Pawelski, Jan")
        game.result should be (Result.Unfinished)
        game.eventDateOption should be (None)
        game.whiteEloOption should be (None)
        game.blackEloOption should be (None)
        game.whiteUSCFOption should be (None)
        game.blackUSCFOption should be (None)
        game.ecoOption should be (None)
        game.timeControlOption should be (None)
        game.boardOption should be (None)
        game.movesWithVariations should have length (14)
        game.movesWithVariations(0).move should be (NormalMove(Piece.Pawn, E2, E4, None, false))
        game.movesWithVariations(0).variations shouldBe empty
        game.movesWithVariations(1).move should be (NormalMove(Piece.Pawn, E7, E5, None, false))
        game.movesWithVariations(1).variations shouldBe empty
        game.movesWithVariations(2).move should be (NormalMove(Piece.Knight, G1, F3, None, false))
        game.movesWithVariations(2).variations shouldBe empty
        game.movesWithVariations(3).move should be (NormalMove(Piece.Knight, G8, F6, None, false))
        game.movesWithVariations(3).variations shouldBe empty
        game.movesWithVariations(4).move should be (NormalMove(Piece.Knight, B1, C3, None, false))
        game.movesWithVariations(4).variations shouldBe empty
        game.movesWithVariations(5).move should be (NormalMove(Piece.Knight, B8, C6, None, false))
        game.movesWithVariations(5).variations shouldBe empty
        game.movesWithVariations(6).move should be (NormalMove(Piece.Bishop, F1, B5, None, false))
        game.movesWithVariations(6).variations shouldBe empty
        game.movesWithVariations(7).move should be (NormalMove(Piece.Bishop, F8, B4, None, false))
        game.movesWithVariations(7).variations shouldBe empty
        game.movesWithVariations(8).move should be (ShortCastling)
        game.movesWithVariations(8).variations shouldBe empty
        game.movesWithVariations(9).move should be (ShortCastling)
        game.movesWithVariations(9).variations shouldBe empty
        game.movesWithVariations(10).move should be (NormalMove(Piece.Queen, D1, E2, None, false))
        game.movesWithVariations(10).variations shouldBe empty
        game.movesWithVariations(11).move should be (NormalMove(Piece.Queen, D8, E7, None, false))
        game.movesWithVariations(11).variations shouldBe empty
        game.movesWithVariations(12).move should be (NormalMove(Piece.Pawn, D2, D3, None, false))
        game.movesWithVariations(12).variations shouldBe empty
        game.movesWithVariations(13).move should be (NormalMove(Piece.Pawn, D7, D6, None, false))
        game.movesWithVariations(13).variations shouldBe empty
    }
  }
  
  it should "read two games" in {
    val s =
      "[Event \"Trzeci turniej\"]\n" +
      "[Site \"Pawelsko\"]\n" +
      "[Date \"2021.??.??\"]\n" +
      "[Round \"1\"]\n" +
      "[White \"Kowalski, Jan\"]\n" +
      "[Black \"Nowak, Piotr\"]\n" +
      "[Result \"*\"]\n" +
      "\n" +
      "1. e4 e5 2. Nc3 Nc6 *\n" +
      "\n" +
      "[Event \"Trzeci turniej\"]\n" +
      "[Site \"Pawelsko\"]\n" +
      "[Date \"2021.??.??\"]\n" +
      "[Round \"2\"]\n" +
      "[White \"Nowak, Piotr\"]\n" +
      "[Black \"Jankowski, Piotr\"]\n" +
      "[Result \"*\"]\n" +
      "\n" +
      "1. e4 e5 2. Nf3 Nf6 *\n" +
      "\n"
    val r = new PGNReader(new StringReader(s))
    val gameOptEither = r.readGame()
    val gameOptEither2 = r.readGame()
    inside(gameOptEither) {
      case Right(Some(game)) =>
        game.event should be ("Trzeci turniej")
        game.site should be ("Pawelsko")
        game.date should be ("2021.??.??")
        game.round should be ("1")
        game.white should have length (1)
        game.white(0) should be ("Kowalski, Jan")
        game.black should have length (1)
        game.black(0) should be ("Nowak, Piotr")
        game.result should be (Result.Unfinished)
        game.eventDateOption should be (None)
        game.whiteEloOption should be (None)
        game.blackEloOption should be (None)
        game.whiteUSCFOption should be (None)
        game.blackUSCFOption should be (None)
        game.ecoOption should be (None)
        game.timeControlOption should be (None)
        game.boardOption should be (None)
        game.movesWithVariations should have length (4)
        game.movesWithVariations(0).move should be (NormalMove(Piece.Pawn, E2, E4, None, false))
        game.movesWithVariations(0).variations shouldBe empty
        game.movesWithVariations(1).move should be (NormalMove(Piece.Pawn, E7, E5, None, false))
        game.movesWithVariations(1).variations shouldBe empty
        game.movesWithVariations(2).move should be (NormalMove(Piece.Knight, B1, C3, None, false))
        game.movesWithVariations(2).variations shouldBe empty
        game.movesWithVariations(3).move should be (NormalMove(Piece.Knight, B8, C6, None, false))
        game.movesWithVariations(3).variations shouldBe empty
    }
    inside(gameOptEither2) {
      case Right(Some(game)) =>
        game.event should be ("Trzeci turniej")
        game.site should be ("Pawelsko")
        game.date should be ("2021.??.??")
        game.round should be ("2")
        game.white should have length (1)
        game.white(0) should be ("Nowak, Piotr")
        game.black should have length (1)
        game.black(0) should be ("Jankowski, Piotr")
        game.result should be (Result.Unfinished)
        game.eventDateOption should be (None)
        game.whiteEloOption should be (None)
        game.blackEloOption should be (None)
        game.whiteUSCFOption should be (None)
        game.blackUSCFOption should be (None)
        game.ecoOption should be (None)
        game.timeControlOption should be (None)
        game.boardOption should be (None)
        game.movesWithVariations should have length (4)
        game.movesWithVariations(0).move should be (NormalMove(Piece.Pawn, E2, E4, None, false))
        game.movesWithVariations(0).variations shouldBe empty
        game.movesWithVariations(1).move should be (NormalMove(Piece.Pawn, E7, E5, None, false))
        game.movesWithVariations(1).variations shouldBe empty
        game.movesWithVariations(2).move should be (NormalMove(Piece.Knight, G1, F3, None, false))
        game.movesWithVariations(2).variations shouldBe empty
        game.movesWithVariations(3).move should be (NormalMove(Piece.Knight, G8, F6, None, false))
        game.movesWithVariations(3).variations shouldBe empty
    }
  }

  it should "read the tags" in {
    val s =
      "[Event \"Trzeci turniej\"]\n" +
      "[Site \"Pawelsko\"]\n" +
      "[Date \"2021.??.??\"]\n" +
      "[Round \"3\"]\n" +
      "[White \"Jankowski, Piotr\"]\n" +
      "[Black \"Nowak, Piotr\"]\n" +
      "[Result \"*\"]\n" +
      "[SomeTag \"some value\"]\n" +
      "[SomeTag2 \"some value 2\"]\n" +
      "\n" +
      "1. e4 e5 2. Nc3 Nc6 *\n"
    val r = new PGNReader(new StringReader(s))
    val gameOptEither = r.readGame()
    inside(gameOptEither) {
      case Right(Some(game)) =>
        game.tags("Event") should be ("Trzeci turniej")
        game.tags("Site") should be ("Pawelsko")
        game.tags("Date") should be ("2021.??.??")
        game.tags("Round") should be ("3")
        game.tags("White") should be ("Jankowski, Piotr")
        game.tags("Black") should be ("Nowak, Piotr")
        game.tags("Result") should be ("*")
        game.tags("SomeTag") should be ("some value")
        game.tags("SomeTag2") should be ("some value 2")
    }
  }

  it should "not read the games for end of file" in {
    val s = ""
    val r = new PGNReader(new StringReader(s))
    val gameOptEither = r.readGame()
    gameOptEither should be (Right(None))
  }

  it should "complain on unrecognized character" in {
    val s = "&"
    val r = new PGNReader(new StringReader(s))
    val gameOptEither = r.readGame()
    gameOptEither should be (Left(PGNReaderError(1L, "Unrecognized character")))
  }

  it should "complain on unclosed comment" in {
    val s =
      "[Event \"Jakis turniej\"]\n" +
      "{\n" +
      "  some comment"
    val r = new PGNReader(new StringReader(s))
    val gameOptEither = r.readGame()
    gameOptEither should be (Left(PGNReaderError(3L, "Unclosed comment")))
  }

  it should "complain on unexpected token for second token in the tag" in {
    val s =
      "[Event \"Jakis turniej\"]\n" +
      "[\"value\" \"value2\"]\n"
    val r = new PGNReader(new StringReader(s))
    val gameOptEither = r.readGame()
    gameOptEither should be (Left(PGNReaderError(2L, "Unexpected token")))
  }

  it should "complain on unexpected token for third token in the tag" in {
    val s =
      "[Event Turniej]\n"
    val r = new PGNReader(new StringReader(s))
    val gameOptEither = r.readGame()
    gameOptEither should be (Left(PGNReaderError(1L, "Unexpected token")))
  }

  it should "complain on unexpected token for fourth token in the tag" in {
    val s =
      "[Event \"Jakis torniej\">\n"
    val r = new PGNReader(new StringReader(s))
    val gameOptEither = r.readGame()
    gameOptEither should be (Left(PGNReaderError(1L, "Unexpected token")))
  }

  it should "complain on unexpected token for the move number" in {
    val s =
      "[Event \"Jakis turniej\"]\n" +
      "[Site \"Kowalno\"]\n" +
      "[Date \"2021.??.??\"]\n" +
      "[Round \"1\"]\n" +
      "[White \"Kowalski, Jan\"]\n" +
      "[Black \"Nowak, Piotr\"]\n" +
      "[Result \"*\"]\n" +
      "\n" +
      "<\n" +
      "\n"
    val r = new PGNReader(new StringReader(s))
    val gameOptEither = r.readGame()
    gameOptEither should be (Left(PGNReaderError(9L, "Unexpected token")))
  }

  it should "complain on unexpected token for the first move" in {
    val s =
      "[Event \"Jakis turniej\"]\n" +
      "[Site \"Kowalno\"]\n" +
      "[Date \"2021.??.??\"]\n" +
      "[Round \"1\"]\n" +
      "[White \"Kowalski, Jan\"]\n" +
      "[Black \"Nowak, Piotr\"]\n" +
      "[Result \"*\"]\n" +
      "\n" +
      "1. \"e4\"\n" +
      "\n"
    val r = new PGNReader(new StringReader(s))
    val gameOptEither = r.readGame()
    gameOptEither should be (Left(PGNReaderError(9L, "Unexpected token")))
  }

  it should "complain on unexpected token for the second move" in {
    val s =
      "[Event \"Jakis turniej\"]\n" +
      "[Site \"Kowalno\"]\n" +
      "[Date \"2021.??.??\"]\n" +
      "[Round \"1\"]\n" +
      "[White \"Kowalski, Jan\"]\n" +
      "[Black \"Nowak, Piotr\"]\n" +
      "[Result \"*\"]\n" +
      "\n" +
      "1. e4 \"e5\"\n" +
      "\n"
    val r = new PGNReader(new StringReader(s))
    val gameOptEither = r.readGame()
    gameOptEither should be (Left(PGNReaderError(9L, "Unexpected token")))
  }

  it should "complain on incorrect Elo for the WhiteElo tag" in {
    val s =
      "[Event \"Jakis turniej\"]\n" +
      "[Site \"Kowalno\"]\n" +
      "[Date \"2021.??.??\"]\n" +
      "[Round \"1\"]\n" +
      "[White \"Kowalski, Jan\"]\n" +
      "[Black \"Nowak, Piotr\"]\n" +
      "[Result \"*\"]\n" +
      "[WhiteElo \"xxx\"]\n" +
      "\n" +
      "1. e4 e5 2. Nc3 Nf6 *\n" +
      "\n"
    val r = new PGNReader(new StringReader(s))
    val gameOptEither = r.readGame()
    gameOptEither should be (Left(PGNReaderError(8L, "Incorrect Elo")))
  }

  it should "complain on incorrect Elo for the BlackElo tag" in {
    val s =
      "[Event \"Jakis turniej\"]\n" +
      "[Site \"Kowalno\"]\n" +
      "[Date \"2021.??.??\"]\n" +
      "[Round \"1\"]\n" +
      "[White \"Kowalski, Jan\"]\n" +
      "[Black \"Nowak, Piotr\"]\n" +
      "[Result \"*\"]\n" +
      "[BlackElo \"xxx\"]\n" +
      "\n" +
      "1. e4 e5 2. Nc3 Nf6 *\n" +
      "\n"
    val r = new PGNReader(new StringReader(s))
    val gameOptEither = r.readGame()
    gameOptEither should be (Left(PGNReaderError(8L, "Incorrect Elo")))
  }

  it should "complain on incorrect USCF for the WhiteUSCF tag" in {
    val s =
      "[Event \"Jakis turniej\"]\n" +
      "[Site \"Kowalno\"]\n" +
      "[Date \"2021.??.??\"]\n" +
      "[Round \"1\"]\n" +
      "[White \"Kowalski, Jan\"]\n" +
      "[Black \"Nowak, Piotr\"]\n" +
      "[Result \"*\"]\n" +
      "[WhiteUSCF \"xxx\"]\n" +
      "\n" +
      "1. e4 e5 2. Nc3 Nf6 *\n" +
      "\n"
    val r = new PGNReader(new StringReader(s))
    val gameOptEither = r.readGame()
    gameOptEither should be (Left(PGNReaderError(8L, "Incorrect USCF")))
  }

  it should "complain on incorrect USCF for the BlackUSCF tag" in {
    val s =
      "[Event \"Jakis turniej\"]\n" +
      "[Site \"Kowalno\"]\n" +
      "[Date \"2021.??.??\"]\n" +
      "[Round \"1\"]\n" +
      "[White \"Kowalski, Jan\"]\n" +
      "[Black \"Nowak, Piotr\"]\n" +
      "[Result \"*\"]\n" +
      "[BlackUSCF \"xxx\"]\n" +
      "\n" +
      "1. e4 e5 2. Nc3 Nf6 *\n" +
      "\n"
    val r = new PGNReader(new StringReader(s))
    val gameOptEither = r.readGame()
    gameOptEither should be (Left(PGNReaderError(8L, "Incorrect USCF")))
  }

  it should "complain on incorrect time control" in {
    val s =
      "[Event \"Jakis turniej\"]\n" +
      "[Site \"Kowalno\"]\n" +
      "[Date \"2021.??.??\"]\n" +
      "[Round \"1\"]\n" +
      "[White \"Kowalski, Jan\"]\n" +
      "[Black \"Nowak, Piotr\"]\n" +
      "[Result \"*\"]\n" +
      "[TimeControl \"xxx\"]\n" +
      "\n" +
      "1. e4 e5 2. Nc3 Nf6 *\n" +
      "\n"
    val r = new PGNReader(new StringReader(s))
    val gameOptEither = r.readGame()
    gameOptEither should be (Left(PGNReaderError(8L, "Incorrect time control")))
  }

  it should "complain on invalid FEN" in {
    val s =
      "[Event \"Jakis turniej\"]\n" +
      "[Site \"Kowalno\"]\n" +
      "[Date \"2021.??.??\"]\n" +
      "[Round \"1\"]\n" +
      "[White \"Kowalski, Jan\"]\n" +
      "[Black \"Nowak, Piotr\"]\n" +
      "[Result \"*\"]\n" +
      "[FEN \"rnbqkbnr/pppp1ppp/8/4x3/4P3/8/PPPP1PPP/RNBQKBNR w KQkq - 0 2\"]\n" +
      "\n" +
      "2. Nc3 Nf6 *\n" +
      "\n"
    val r = new PGNReader(new StringReader(s))
    val gameOptEither = r.readGame()
    gameOptEither should be (Left(PGNReaderError(8L, "Invalid FEN")))
  }

  it should "complain on incorrect move number" in {
    val s =
      "[Event \"Jakis turniej\"]\n" +
      "[Site \"Kowalno\"]\n" +
      "[Date \"2021.??.??\"]\n" +
      "[Round \"1\"]\n" +
      "[White \"Kowalski, Jan\"]\n" +
      "[Black \"Nowak, Piotr\"]\n" +
      "[Result \"*\"]\n" +
      "\n" +
      "xx. e4 e5 2. Nc3 Nf6 *\n" +
      "\n"
    val r = new PGNReader(new StringReader(s))
    val gameOptEither = r.readGame()
    gameOptEither should be (Left(PGNReaderError(9L, "Incorrect move number")))
  }

  it should "complain on illegal move" in {
    val s =
      "[Event \"Jakis turniej\"]\n" +
      "[Site \"Kowalno\"]\n" +
      "[Date \"2021.??.??\"]\n" +
      "[Round \"1\"]\n" +
      "[White \"Kowalski, Jan\"]\n" +
      "[Black \"Nowak, Piotr\"]\n" +
      "[Result \"*\"]\n" +
      "\n" +
      "1. e5 e5 *\n" +
      "\n"
    val r = new PGNReader(new StringReader(s))
    val gameOptEither = r.readGame()
    gameOptEither should be (Left(PGNReaderError(9L, "Illegal move")))
  }

  it should "complain on move number isn't equal to board fullmove number" in {
    val s =
      "[Event \"Jakis turniej\"]\n" +
      "[Site \"Kowalno\"]\n" +
      "[Date \"2021.??.??\"]\n" +
      "[Round \"1\"]\n" +
      "[White \"Kowalski, Jan\"]\n" +
      "[Black \"Nowak, Piotr\"]\n" +
      "[Result \"*\"]\n" +
      "\n" +
      "2. e4 e5 *\n" +
      "\n"
    val r = new PGNReader(new StringReader(s))
    val gameOptEither = r.readGame()
    gameOptEither should be (Left(PGNReaderError(9L, "Move number isn't equal to board fullmove number")))
  }

  it should "complain on game termination isn't equal to result for the draw game termination" in {
    val s =
      "[Event \"Jakis turniej\"]\n" +
      "[Site \"Kowalno\"]\n" +
      "[Date \"2021.??.??\"]\n" +
      "[Round \"1\"]\n" +
      "[White \"Kowalski, Jan\"]\n" +
      "[Black \"Nowak, Piotr\"]\n" +
      "[Result \"*\"]\n" +
      "\n" +
      "1. e4 e5 2. Nc3 Nf6 1/2-1/2\n" +
      "\n"
    val r = new PGNReader(new StringReader(s))
    val gameOptEither = r.readGame()
    gameOptEither should be (Left(PGNReaderError(9L, "Game termination isn't equal to result")))
  }

  it should "complain on game termination isn't equal to result for the unfinished game termination" in {
    val s =
      "[Event \"Jakis turniej\"]\n" +
      "[Site \"Kowalno\"]\n" +
      "[Date \"2021.??.??\"]\n" +
      "[Round \"1\"]\n" +
      "[White \"Kowalski, Jan\"]\n" +
      "[Black \"Nowak, Piotr\"]\n" +
      "[Result \"1/2-1/2\"]\n" +
      "\n" +
      "1. e4 e5 2. Nc3 Nf6 *\n" +
      "\n"
    val r = new PGNReader(new StringReader(s))
    val gameOptEither = r.readGame()
    gameOptEither should be (Left(PGNReaderError(9L, "Game termination isn't equal to result")))
  }

  it should "complain on unexpected right parenthesis" in {
    val s =
      "[Event \"Jakis turniej\"]\n" +
      "[Site \"Kowalno\"]\n" +
      "[Date \"2021.??.??\"]\n" +
      "[Round \"1\"]\n" +
      "[White \"Kowalski, Jan\"]\n" +
      "[Black \"Nowak, Piotr\"]\n" +
      "[Result \"*\"]\n" +
      "\n" +
      "1. e4 e5 2. Nc3 Nf6 )\n" +
      "\n"
    val r = new PGNReader(new StringReader(s))
    val gameOptEither = r.readGame()
    gameOptEither should be (Left(PGNReaderError(9L, "Unexpected right parenthesis")))
  }
}
