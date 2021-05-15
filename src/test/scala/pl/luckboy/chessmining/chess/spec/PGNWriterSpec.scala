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
 * You should have received a copy of the GNU Lesser General Public
 * License and the GNU General Public License along with this program.
 * If not, see <http://www.gnu.org/licenses/>.
 */
package pl.luckboy.chessmining.chess.spec
import java.io._
import org.scalatest.flatspec._
import org.scalatest.matchers._
import pl.luckboy.chessmining.chess._
import Squares._

class PGNWriterSpec extends AnyFlatSpec with should.Matchers
{
  "A PGNWriter" should "write the game" in {
    val game = Game(
        event = "Jakis turniej",
        site = "Kowalno",
        date = "2021.??.??",
        round = "1",
        white = Vector("Kowalski, Jan"),
        black = Vector("Nowak, Piotr"),
        result = Result.Unfinished,
        eventDateOption = None,
        whiteEloOption = None,
        blackEloOption = None,
        whiteUSCFOption = None,
        blackUSCFOption = None,
        ecoOption = None,
        timeControlOption = None,
        tags = Map(),
        boardOption = None,
        movesWithVariations = Vector(
          MoveWithVariations(NormalMove(Piece.Pawn, E2, E4, None, false),
            Vector()),
          MoveWithVariations(NormalMove(Piece.Pawn, E7, E5, None, false),
            Vector()),
          MoveWithVariations(NormalMove(Piece.Knight, B1, C3, None, false),
            Vector()),
          MoveWithVariations(NormalMove(Piece.Knight, G8, F6, None, false),
            Vector())))
    val sw = new StringWriter()
    val w = new PGNWriter(sw)
    w.writeGame(game) should be (true)
    sw.toString() should be (
        "[Event \"Jakis turniej\"]\n" +
        "[Site \"Kowalno\"]\n" +
        "[Date \"2021.??.??\"]\n" +
        "[Round \"1\"]\n" +
        "[White \"Kowalski, Jan\"]\n" +
        "[Black \"Nowak, Piotr\"]\n" +
        "[Result \"*\"]\n" +
        "\n" +
        "1. e4 e5 2. Nc3 Nf6 *\n" +
        "\n")
  }

  it should "write the game for the white win" in {
    val game = Game(
        event = "Jakis turniej",
        site = "Kowalno",
        date = "2021.??.??",
        round = "2",
        white = Vector("Nowak, Piotr"),
        black = Vector("Jankowski, Piotr"),
        result = Result.WhiteWin,
        eventDateOption = None,
        whiteEloOption = None,
        blackEloOption = None,
        whiteUSCFOption = None,
        blackUSCFOption = None,
        ecoOption = None,
        timeControlOption = None,
        tags = Map(),
        boardOption = None,
        movesWithVariations = Vector(
          MoveWithVariations(NormalMove(Piece.Pawn, D2, D4, None, false),
            Vector()),
          MoveWithVariations(NormalMove(Piece.Pawn, D7, D5, None, false),
            Vector()),
          MoveWithVariations(NormalMove(Piece.Knight, B1, C3, None, false),
            Vector())))
    val sw = new StringWriter()
    val w = new PGNWriter(sw)
    w.writeGame(game) should be (true)
    sw.toString() should be (
        "[Event \"Jakis turniej\"]\n" +
        "[Site \"Kowalno\"]\n" +
        "[Date \"2021.??.??\"]\n" +
        "[Round \"2\"]\n" +
        "[White \"Nowak, Piotr\"]\n" +
        "[Black \"Jankowski, Piotr\"]\n" +
        "[Result \"1-0\"]\n" +
        "\n" +
        "1. d4 d5 2. Nc3 1-0\n" +
        "\n")
  }

  it should "write the game for the black win" in {
    val game = Game(
        event = "Jakis turniej",
        site = "Kowalno",
        date = "2021.??.??",
        round = "3",
        white = Vector("Nowak, Piotr"),
        black = Vector("Piotrowski, Pawel"),
        result = Result.BlackWin,
        eventDateOption = None,
        whiteEloOption = None,
        blackEloOption = None,
        whiteUSCFOption = None,
        blackUSCFOption = None,
        ecoOption = None,
        timeControlOption = None,
        tags = Map(),
        boardOption = None,
        movesWithVariations = Vector(
          MoveWithVariations(NormalMove(Piece.Pawn, E2, E4, None, false),
            Vector()),
          MoveWithVariations(NormalMove(Piece.Pawn, E7, E5, None, false),
            Vector()),
          MoveWithVariations(NormalMove(Piece.Knight, G1, F3, None, false),
            Vector()),
          MoveWithVariations(NormalMove(Piece.Knight, B8, C6, None, false),
            Vector())))
    val sw = new StringWriter()
    val w = new PGNWriter(sw)
    w.writeGame(game) should be (true)
    sw.toString() should be (
        "[Event \"Jakis turniej\"]\n" +
        "[Site \"Kowalno\"]\n" +
        "[Date \"2021.??.??\"]\n" +
        "[Round \"3\"]\n" +
        "[White \"Nowak, Piotr\"]\n" +
        "[Black \"Piotrowski, Pawel\"]\n" +
        "[Result \"0-1\"]\n" +
        "\n" +
        "1. e4 e5 2. Nf3 Nc6 0-1\n" +
        "\n")
  }

  it should "write the game for the draw" in {
    val game = Game(
        event = "Jakis turniej",
        site = "Kowalno",
        date = "2021.??.??",
        round = "4",
        white = Vector("Jankowski, Piotr"),
        black = Vector("Kowalski, Jan"),
        result = Result.Draw,
        eventDateOption = None,
        whiteEloOption = None,
        blackEloOption = None,
        whiteUSCFOption = None,
        blackUSCFOption = None,
        ecoOption = None,
        timeControlOption = None,
        tags = Map(),
        boardOption = None,
        movesWithVariations = Vector(
          MoveWithVariations(NormalMove(Piece.Pawn, E2, E4, None, false),
            Vector()),
          MoveWithVariations(NormalMove(Piece.Pawn, E7, E5, None, false),
            Vector()),
          MoveWithVariations(NormalMove(Piece.Knight, G1, F3, None, false),
            Vector()),
          MoveWithVariations(NormalMove(Piece.Knight, G8, F6, None, false),
            Vector())))
    val sw = new StringWriter()
    val w = new PGNWriter(sw)
    w.writeGame(game) should be (true)
    sw.toString() should be (
        "[Event \"Jakis turniej\"]\n" +
        "[Site \"Kowalno\"]\n" +
        "[Date \"2021.??.??\"]\n" +
        "[Round \"4\"]\n" +
        "[White \"Jankowski, Piotr\"]\n" +
        "[Black \"Kowalski, Jan\"]\n" +
        "[Result \"1/2-1/2\"]\n" +
        "\n" +
        "1. e4 e5 2. Nf3 Nf6 1/2-1/2\n" +
        "\n")
  }

  it should "write the game for the unfinished game" in {
    val game = Game(
        event = "Jakis turniej",
        site = "Kowalno",
        date = "2021.??.??",
        round = "5",
        white = Vector("Pawelski, Piotr"),
        black = Vector("Jankowski, Piotr"),
        result = Result.Unfinished,
        eventDateOption = None,
        whiteEloOption = None,
        blackEloOption = None,
        whiteUSCFOption = None,
        blackUSCFOption = None,
        ecoOption = None,
        timeControlOption = None,
        tags = Map(),
        boardOption = None,
        movesWithVariations = Vector(
          MoveWithVariations(NormalMove(Piece.Pawn, D2, D4, None, false),
            Vector()),
          MoveWithVariations(NormalMove(Piece.Pawn, D7, D5, None, false),
            Vector()),
          MoveWithVariations(NormalMove(Piece.Knight, G1, F3, None, false),
            Vector())))
    val sw = new StringWriter()
    val w = new PGNWriter(sw)
    w.writeGame(game) should be (true)
    sw.toString() should be (
        "[Event \"Jakis turniej\"]\n" +
        "[Site \"Kowalno\"]\n" +
        "[Date \"2021.??.??\"]\n" +
        "[Round \"5\"]\n" +
        "[White \"Pawelski, Piotr\"]\n" +
        "[Black \"Jankowski, Piotr\"]\n" +
        "[Result \"*\"]\n" +
        "\n" +
        "1. d4 d5 2. Nf3 *\n" +
        "\n")
  }

  it should "write the game with the optional fields" in {
    val game = Game(
        event = "Jakies zdarzenie",
        site = "Nowakowo",
        date = "2021.04.02",
        round = "1",
        white = Vector("Kowalski, Jan"),
        black = Vector("Nowak, Piotr"),
        result = Result.Unfinished,
        eventDateOption = Some("2021.04.01"),
        whiteEloOption = Some(Vector(Some(1200))),
        blackEloOption = Some(Vector(Some(1300))),
        whiteUSCFOption = Some(Vector(Some(1400))),
        blackUSCFOption = Some(Vector(Some(1500))),
        ecoOption = Some("C44a"),
        timeControlOption = Some(Vector(Some(SuddenDeathTimeControl(600)))),
        tags = Map(),
        boardOption = None,
        movesWithVariations = Vector(
          MoveWithVariations(NormalMove(Piece.Pawn, E2, E4, None, false),
            Vector()),
          MoveWithVariations(NormalMove(Piece.Pawn, E7, E5, None, false),
            Vector()),
          MoveWithVariations(NormalMove(Piece.Knight, G1, F3, None, false),
            Vector()),
          MoveWithVariations(NormalMove(Piece.Knight, B8, C6, None, false),
            Vector())))
    val sw = new StringWriter()
    val w = new PGNWriter(sw)
    w.writeGame(game) should be (true)
    sw.toString() should be (
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
        "[ECO \"C44a\"]\n" +
        "[TimeControl \"600\"]\n" +
        "\n" +
        "1. e4 e5 2. Nf3 Nc6 *\n" +
        "\n")
  }

  it should "write the game with four players for two sides" in {
    val game = Game(
        event = "Jakies zdarzenie",
        site = "Nowakowo",
        date = "2021.04.03",
        round = "2",
        white = Vector("Pawelski, Piotr", "Pomagier, Jan"),
        black = Vector("Kowalski, Jan", "Pomagacz, Pawel"),
        result = Result.Unfinished,
        eventDateOption = None,
        whiteEloOption = Some(Vector(Some(1200), None)),
        blackEloOption = Some(Vector(None, Some(1300))),
        whiteUSCFOption = Some(Vector(Some(1400), None)),
        blackUSCFOption = Some(Vector(None, Some(1500))),
        ecoOption = None,
        timeControlOption = None,
        tags = Map(),
        boardOption = None,
        movesWithVariations = Vector(
          MoveWithVariations(NormalMove(Piece.Pawn, E2, E4, None, false),
            Vector()),
          MoveWithVariations(NormalMove(Piece.Pawn, E7, E5, None, false),
            Vector()),
          MoveWithVariations(NormalMove(Piece.Knight, B1, C3, None, false),
            Vector()),
          MoveWithVariations(NormalMove(Piece.Knight, B8, C6, None, false),
            Vector())))
    val sw = new StringWriter()
    val w = new PGNWriter(sw)
    w.writeGame(game) should be (true)
    sw.toString() should be (
        "[Event \"Jakies zdarzenie\"]\n" +
        "[Site \"Nowakowo\"]\n" +
        "[Date \"2021.04.03\"]\n" +
        "[Round \"2\"]\n" +
        "[White \"Pawelski, Piotr:Pomagier, Jan\"]\n" +
        "[Black \"Kowalski, Jan:Pomagacz, Pawel\"]\n" +
        "[Result \"*\"]\n" +
        "[WhiteElo \"1200:-\"]\n" +
        "[BlackElo \"-:1300\"]\n" +
        "[WhiteUSCF \"1400:-\"]\n" +
        "[BlackUSCF \"-:1500\"]\n" +
        "\n" +
        "1. e4 e5 2. Nc3 Nc6 *\n" +
        "\n")
  }
  
    it should "write the game with all time control kinds" in {
    val game = Game(
        event = "Jakies zdarzenie",
        site = "Nowakowo",
        date = "2021.04.02",
        round = "3",
        white = Vector("Piotrowski, Pawel"),
        black = Vector("Nowak, Piotr"),
        result = Result.Unfinished,
        eventDateOption = None,
        whiteEloOption = None,
        blackEloOption = None,
        whiteUSCFOption = None,
        blackUSCFOption = None,
        ecoOption = None,
        timeControlOption = Some(Vector(
            Some(UnknownTimeControl),
            None,
            Some(ClassicalTimeControl(40, 9000)),
            Some(SuddenDeathTimeControl(300)),
            Some(IncrementalTimeControl(4500, 60)),
            Some(HourglassTimeControl(180)))),
        tags = Map(),
        boardOption = None,
        movesWithVariations = Vector(
          MoveWithVariations(NormalMove(Piece.Pawn, E2, E4, None, false),
            Vector()),
          MoveWithVariations(NormalMove(Piece.Pawn, E7, E5, None, false),
            Vector()),
          MoveWithVariations(NormalMove(Piece.Knight, B1, C3, None, false),
            Vector()),
          MoveWithVariations(NormalMove(Piece.Knight, B8, C6, None, false),
            Vector())))
    val sw = new StringWriter()
    val w = new PGNWriter(sw)
    w.writeGame(game) should be (true)
    sw.toString() should be (
        "[Event \"Jakies zdarzenie\"]\n" +
        "[Site \"Nowakowo\"]\n" +
        "[Date \"2021.04.02\"]\n" +
        "[Round \"3\"]\n" +
        "[White \"Piotrowski, Pawel\"]\n" +
        "[Black \"Nowak, Piotr\"]\n" +
        "[Result \"*\"]\n" +
        "[TimeControl \"?:-:40/9000:300:4500+60:*180\"]\n" +
        "\n" +
        "1. e4 e5 2. Nc3 Nc6 *\n" +
        "\n")
  }

  it should "write the game for the FEN and the white side" in {
    val game = Game(
        event = "Drugi turniej",
        site = "Jankowo",
        date = "2021.??.??",
        round = "1",
        white = Vector("Kowalski, Jan"),
        black = Vector("Nowak, Piotr"),
        result = Result.Unfinished,
        eventDateOption = None,
        whiteEloOption = None,
        blackEloOption = None,
        whiteUSCFOption = None,
        blackUSCFOption = None,
        ecoOption = None,
        timeControlOption = None,
        tags = Map(),
        boardOption = Some(Board("rnbqkbnr/pppp1ppp/8/4p3/4P3/8/PPPP1PPP/RNBQKBNR w KQkq - 0 2")),
        movesWithVariations = Vector(
          MoveWithVariations(NormalMove(Piece.Knight, G1, F3, None, false),
            Vector()),
          MoveWithVariations(NormalMove(Piece.Knight, B8, C6, None, false),
            Vector()),
          MoveWithVariations(NormalMove(Piece.Bishop, F1, C4, None, false),
            Vector()),
          MoveWithVariations(NormalMove(Piece.Bishop, F8, C5, None, false),
            Vector())))
    val sw = new StringWriter()
    val w = new PGNWriter(sw)
    w.writeGame(game) should be (true)
    sw.toString() should be (
        "[Event \"Drugi turniej\"]\n" +
        "[Site \"Jankowo\"]\n" +
        "[Date \"2021.??.??\"]\n" +
        "[Round \"1\"]\n" +
        "[White \"Kowalski, Jan\"]\n" +
        "[Black \"Nowak, Piotr\"]\n" +
        "[Result \"*\"]\n" +
        "[SetUp \"1\"]\n" +
        "[FEN \"rnbqkbnr/pppp1ppp/8/4p3/4P3/8/PPPP1PPP/RNBQKBNR w KQkq - 0 2\"]\n" +
        "\n" +
        "2. Nf3 Nc6 3. Bc4 Bc5 *\n" +
        "\n")
  }

  it should "write the game for the FEN and the black side" in {
    val game = Game(
        event = "Drugi turniej",
        site = "Jankowo",
        date = "2021.??.??",
        round = "2",
        white = Vector("Nowak, Piotr"),
        black = Vector("Jankwoski, Piotr"),
        result = Result.Unfinished,
        eventDateOption = None,
        whiteEloOption = None,
        blackEloOption = None,
        whiteUSCFOption = None,
        blackUSCFOption = None,
        ecoOption = None,
        timeControlOption = None,
        tags = Map(),
        boardOption = Some(Board("rnbqkbnr/pppp1ppp/8/4p3/4P3/5N2/PPPP1PPP/RNBQKB1R b KQkq - 0 2")),
        movesWithVariations = Vector(
          MoveWithVariations(NormalMove(Piece.Knight, B8, C6, None, false),
            Vector()),
          MoveWithVariations(NormalMove(Piece.Knight, B1, C3, None, false),
            Vector()),
          MoveWithVariations(NormalMove(Piece.Knight, G8, F6, None, false),
            Vector())))
    val sw = new StringWriter()
    val w = new PGNWriter(sw)
    w.writeGame(game) should be (true)
    sw.toString() should be (
        "[Event \"Drugi turniej\"]\n" +
        "[Site \"Jankowo\"]\n" +
        "[Date \"2021.??.??\"]\n" +
        "[Round \"2\"]\n" +
        "[White \"Nowak, Piotr\"]\n" +
        "[Black \"Jankwoski, Piotr\"]\n" +
        "[Result \"*\"]\n" +
        "[SetUp \"1\"]\n" +
        "[FEN \"rnbqkbnr/pppp1ppp/8/4p3/4P3/5N2/PPPP1PPP/RNBQKB1R b KQkq - 0 2\"]\n" +
        "\n" +
        "2... Nc6 3. Nc3 Nf6 *\n" +
        "\n")
  }
  
  it should "write the game with the variation for the white side" in {
    val game = Game(
        event = "Drugie zdarzenie",
        site = "Piotrkowo",
        date = "2021.??.??",
        round = "1",
        white = Vector("Kowalski, Jan"),
        black = Vector("Nowak, Piotr"),
        result = Result.Unfinished,
        eventDateOption = None,
        whiteEloOption = None,
        blackEloOption = None,
        whiteUSCFOption = None,
        blackUSCFOption = None,
        ecoOption = None,
        timeControlOption = None,
        tags = Map(),
        boardOption = None,
        movesWithVariations = Vector(
          MoveWithVariations(NormalMove(Piece.Pawn, E2, E4, None, false),
            Vector()),
          MoveWithVariations(NormalMove(Piece.Pawn, E7, E5, None, false),
            Vector()),
          MoveWithVariations(NormalMove(Piece.Knight, G1, F3, None, false),
            Vector(
              Vector(
                MoveWithVariations(NormalMove(Piece.Knight, B1, C3, None, false),
                  Vector()),
                MoveWithVariations(NormalMove(Piece.Knight, G8, F6, None, false),
                  Vector()),
                MoveWithVariations(NormalMove(Piece.Knight, G1, F3, None, false),
                  Vector()))))))
    val sw = new StringWriter()
    val w = new PGNWriter(sw)
    w.writeGame(game) should be (true)
    sw.toString() should be (
        "[Event \"Drugie zdarzenie\"]\n" +
        "[Site \"Piotrkowo\"]\n" +
        "[Date \"2021.??.??\"]\n" +
        "[Round \"1\"]\n" +
        "[White \"Kowalski, Jan\"]\n" +
        "[Black \"Nowak, Piotr\"]\n" +
        "[Result \"*\"]\n" +
        "\n" +
        "1. e4 e5 2. Nf3 ( 2. Nc3 Nf6 3. Nf3 ) *\n" +
        "\n")
  }

  it should "write the game with the variation for the black side" in {
    val game = Game(
        event = "Drugie zdarzenie",
        site = "Piotrkowo",
        date = "2021.??.??",
        round = "2",
        white = Vector("Nowak, Piotr"),
        black = Vector("Pawelski, Piotr"),
        result = Result.Unfinished,
        eventDateOption = None,
        whiteEloOption = None,
        blackEloOption = None,
        whiteUSCFOption = None,
        blackUSCFOption = None,
        ecoOption = None,
        timeControlOption = None,
        tags = Map(),
        boardOption = None,
        movesWithVariations = Vector(
          MoveWithVariations(NormalMove(Piece.Pawn, E2, E4, None, false),
            Vector()),
          MoveWithVariations(NormalMove(Piece.Pawn, E7, E5, None, false),
            Vector()),
          MoveWithVariations(NormalMove(Piece.Knight, G1, F3, None, false),
            Vector()),
          MoveWithVariations(NormalMove(Piece.Knight, B8, C6, None, false),
            Vector(
              Vector(
                MoveWithVariations(NormalMove(Piece.Knight, G8, F6, None, false),
                  Vector()),
                MoveWithVariations(NormalMove(Piece.Knight, B1, C3, None, false),
                  Vector()),
                MoveWithVariations(NormalMove(Piece.Knight, B8, C6, None, false),
                  Vector()))))))
    val sw = new StringWriter()
    val w = new PGNWriter(sw)
    w.writeGame(game) should be (true)
    sw.toString() should be (
        "[Event \"Drugie zdarzenie\"]\n" +
        "[Site \"Piotrkowo\"]\n" +
        "[Date \"2021.??.??\"]\n" +
        "[Round \"2\"]\n" +
        "[White \"Nowak, Piotr\"]\n" +
        "[Black \"Pawelski, Piotr\"]\n" +
        "[Result \"*\"]\n" +
        "\n" +
        "1. e4 e5 2. Nf3 Nc6 ( 2... Nf6 3. Nc3 Nc6 ) *\n" +
        "\n")
  }
  
  it should "write the game with two variations for same move" in {
    val game = Game(
        event = "Drugie zdarzenie",
        site = "Piotrkowo",
        date = "2021.??.??",
        round = "3",
        white = Vector("Kowalski, Jan"),
        black = Vector("Jankowski, Piotr"),
        result = Result.Unfinished,
        eventDateOption = None,
        whiteEloOption = None,
        blackEloOption = None,
        whiteUSCFOption = None,
        blackUSCFOption = None,
        ecoOption = None,
        timeControlOption = None,
        tags = Map(),
        boardOption = None,
        movesWithVariations = Vector(
          MoveWithVariations(NormalMove(Piece.Pawn, E2, E4, None, false),
            Vector()),
          MoveWithVariations(NormalMove(Piece.Pawn, E7, E5, None, false),
            Vector()),
          MoveWithVariations(NormalMove(Piece.Knight, G1, F3, None, false),
            Vector(
              Vector(
                MoveWithVariations(NormalMove(Piece.Bishop, F1, C4, None, false),
                  Vector()),
                MoveWithVariations(NormalMove(Piece.Bishop, F8, C5, None, false),
                  Vector())),
              Vector(
                MoveWithVariations(NormalMove(Piece.Knight, B1, C3, None, false),
                  Vector()),
                MoveWithVariations(NormalMove(Piece.Knight, G8, F6, None, false),
                  Vector())))),
          MoveWithVariations(NormalMove(Piece.Knight, B8, C6, None, false),
            Vector())))
    val sw = new StringWriter()
    val w = new PGNWriter(sw)
    w.writeGame(game) should be (true)
    sw.toString() should be (
        "[Event \"Drugie zdarzenie\"]\n" +
        "[Site \"Piotrkowo\"]\n" +
        "[Date \"2021.??.??\"]\n" +
        "[Round \"3\"]\n" +
        "[White \"Kowalski, Jan\"]\n" +
        "[Black \"Jankowski, Piotr\"]\n" +
        "[Result \"*\"]\n" +
        "\n" +
        "1. e4 e5 2. Nf3 ( 2. Bc4 Bc5 ) ( 2. Nc3 Nf6 ) 2... Nc6 *\n" +
        "\n")
  }

  it should "write the game with the subvariation" in {
    val game = Game(
        event = "Drugie zdarzenie",
        site = "Piotrkowo",
        date = "2021.??.??",
        round = "4",
        white = Vector("Pawelski, Piotr"),
        black = Vector("Kowalski, Jan"),
        result = Result.Unfinished,
        eventDateOption = None,
        whiteEloOption = None,
        blackEloOption = None,
        whiteUSCFOption = None,
        blackUSCFOption = None,
        ecoOption = None,
        timeControlOption = None,
        tags = Map(),
        boardOption = None,
        movesWithVariations = Vector(
          MoveWithVariations(NormalMove(Piece.Knight, G1, F3, None, false),
            Vector()),
          MoveWithVariations(NormalMove(Piece.Knight, B8, C6, None, false),
            Vector()),
          MoveWithVariations(NormalMove(Piece.Pawn, E2, E4, None, false),
            Vector()),
          MoveWithVariations(NormalMove(Piece.Pawn, E7, E5, None, false),
            Vector(
              Vector(
                MoveWithVariations(NormalMove(Piece.Knight, G8, F6, None, false),
                  Vector()),
                MoveWithVariations(NormalMove(Piece.Bishop, F1, C4, None, false),
                  Vector(
                    Vector(
                      MoveWithVariations(NormalMove(Piece.Knight, B1, C3, None, false),
                        Vector())))),
                MoveWithVariations(NormalMove(Piece.Pawn, E7, E5, None, false),
                  Vector())))),
          MoveWithVariations(NormalMove(Piece.Knight, B1, C3, None, false),
            Vector())))
    val sw = new StringWriter()
    val w = new PGNWriter(sw)
    w.writeGame(game) should be (true)
    sw.toString() should be (
        "[Event \"Drugie zdarzenie\"]\n" +
        "[Site \"Piotrkowo\"]\n" +
        "[Date \"2021.??.??\"]\n" +
        "[Round \"4\"]\n" +
        "[White \"Pawelski, Piotr\"]\n" +
        "[Black \"Kowalski, Jan\"]\n" +
        "[Result \"*\"]\n" +
        "\n" +
        "1. Nf3 Nc6 2. e4 e5 ( 2... Nf6 3. Bc4 ( 3. Nc3 ) 3... e5 ) 3. Nc3 *\n" +
        "\n")
  }
  
  it should "write the game for the move characters greater than 80" in {
    val game = Game(
        event = "Drugie zdarzenie",
        site = "Piotrkowo",
        date = "2021.??.??",
        round = "5",
        white = Vector("Jankowski, Piotr"),
        black = Vector("Nowak, Piotr"),
        result = Result.Unfinished,
        eventDateOption = None,
        whiteEloOption = None,
        blackEloOption = None,
        whiteUSCFOption = None,
        blackUSCFOption = None,
        ecoOption = None,
        timeControlOption = None,
        tags = Map(),
        boardOption = None,
        movesWithVariations = Vector(
          MoveWithVariations(NormalMove(Piece.Pawn, E2, E4, None, false),
            Vector()),
          MoveWithVariations(NormalMove(Piece.Pawn, E7, E5, None, false),
            Vector()),
          MoveWithVariations(NormalMove(Piece.Knight, G1, F3, None, false),
            Vector()),
          MoveWithVariations(NormalMove(Piece.Knight, G8, F6, None, false),
            Vector()),
          MoveWithVariations(NormalMove(Piece.Knight, B1, C3, None, false),
            Vector()),
          MoveWithVariations(NormalMove(Piece.Knight, B8, C6, None, false),
            Vector()),
          MoveWithVariations(NormalMove(Piece.Bishop, F1, C4, None, false),
            Vector()),
          MoveWithVariations(NormalMove(Piece.Bishop, F8, C5, None, false),
            Vector()),
          MoveWithVariations(ShortCastling,
            Vector()),
          MoveWithVariations(ShortCastling,
            Vector()),
          MoveWithVariations(NormalMove(Piece.Queen, D1, E1, None, false),
            Vector()),
          MoveWithVariations(NormalMove(Piece.Queen, D8, E8, None, false),
            Vector()),
          MoveWithVariations(NormalMove(Piece.Pawn, D2, D3, None, false),
            Vector()),
          MoveWithVariations(NormalMove(Piece.Pawn, D7, D6, None, false),
            Vector()),
          MoveWithVariations(NormalMove(Piece.Bishop, C1, G5, None, false),
            Vector()),
          MoveWithVariations(NormalMove(Piece.Bishop, C8, G4, None, false),
            Vector())))
    val sw = new StringWriter()
    val w = new PGNWriter(sw)
    w.writeGame(game) should be (true)
    sw.toString() should be (
        "[Event \"Drugie zdarzenie\"]\n" +
        "[Site \"Piotrkowo\"]\n" +
        "[Date \"2021.??.??\"]\n" +
        "[Round \"5\"]\n" +
        "[White \"Jankowski, Piotr\"]\n" +
        "[Black \"Nowak, Piotr\"]\n" +
        "[Result \"*\"]\n" +
        "\n" +
        "1. e4 e5 2. Nf3 Nf6 3. Nc3 Nc6 4. Bc4 Bc5 5. O-O O-O 6. Qe1 Qe8 7. d3 d6 8. Bg5\n" +
        "Bg4 *\n" +
        "\n")
  }

  it should "write the game with one tag" in {
    val game = Game(
        event = "Trzeci turniej",
        site = "Pawelsko",
        date = "2021.??.??",
        round = "1",
        white = Vector("Kowalski, Jan"),
        black = Vector("Nowak, Piotr"),
        result = Result.Unfinished,
        eventDateOption = Some("2021.??.??"),
        whiteEloOption = Some(Vector(Some(1200))),
        blackEloOption = Some(Vector(Some(1300))),
        whiteUSCFOption = Some(Vector(Some(1400))),
        blackUSCFOption = Some(Vector(Some(1500))),
        ecoOption = Some("C25g"),
        timeControlOption = Some(Vector(Some(SuddenDeathTimeControl(900)))),
        tags = Map(
          "Event" -> "Trzeci turniej",
          "Site" -> "Pawelsko",
          "Date" -> "2021.??.??",
          "Round" -> "1",
          "White" -> "Kowalski, Jan",
          "Black" -> "Nowak, Piotr",
          "Result" -> "*",
          "EventDate" -> "2021.??.??",
          "WhiteElo" -> "1200",
          "BlackElo" -> "1300",
          "WhiteUSCF" -> "1400",
          "BlackUSCF" -> "1500",
          "ECO" -> "C25g",
          "TimeControl" -> "900",
          "SomeTag" -> "some value"),
        boardOption = None,
        movesWithVariations = Vector(
          MoveWithVariations(NormalMove(Piece.Pawn, E2, E4, None, false),
            Vector()),
          MoveWithVariations(NormalMove(Piece.Pawn, E7, E5, None, false),
            Vector()),
          MoveWithVariations(NormalMove(Piece.Knight, B1, C3, None, false),
            Vector()),
          MoveWithVariations(NormalMove(Piece.Knight, B8, C6, None, false),
            Vector())))
    val sw = new StringWriter()
    val w = new PGNWriter(sw)
    w.writeGame(game) should be (true)
    sw.toString() should be (
        "[Event \"Trzeci turniej\"]\n" +
        "[Site \"Pawelsko\"]\n" +
        "[Date \"2021.??.??\"]\n" +
        "[Round \"1\"]\n" +
        "[White \"Kowalski, Jan\"]\n" +
        "[Black \"Nowak, Piotr\"]\n" +
        "[Result \"*\"]\n" +
        "[EventDate \"2021.??.??\"]\n" +
        "[WhiteElo \"1200\"]\n" +
        "[BlackElo \"1300\"]\n" +
        "[WhiteUSCF \"1400\"]\n" +
        "[BlackUSCF \"1500\"]\n" +
        "[ECO \"C25g\"]\n" +
        "[TimeControl \"900\"]\n" +
        "[SomeTag \"some value\"]\n" +
        "\n" +
        "1. e4 e5 2. Nc3 Nc6 *\n" +
        "\n")
  }

  it should "complain on the illegal move" in {
    val game = Game(
        event = "Jakis turniej",
        site = "Kowalno",
        date = "2021.??.??",
        round = "1",
        white = Vector("Kowalski, Jan"),
        black = Vector("Nowak, Piotr"),
        result = Result.Unfinished,
        eventDateOption = None,
        whiteEloOption = None,
        blackEloOption = None,
        whiteUSCFOption = None,
        blackUSCFOption = None,
        ecoOption = None,
        timeControlOption = None,
        tags = Map(),
        boardOption = None,
        movesWithVariations = Vector(
          MoveWithVariations(NormalMove(Piece.Pawn, E2, E4, None, false),
            Vector()),
          MoveWithVariations(NormalMove(Piece.Pawn, E7, E4, None, false),
            Vector())))
    val sw = new StringWriter()
    val w = new PGNWriter(sw)
    w.writeGame(game) should be (false)
  }
}
