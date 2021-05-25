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
package pl.luckboy.chessmining.iterator.spec
import org.scalatest._
import org.scalatest.flatspec._
import org.scalatest.matchers._
import pl.luckboy.chessmining.chess._
import pl.luckboy.chessmining.iterator._
import Squares._

trait MoveMakingIteratorWithoutVariationsBehaviors[T, U] extends should.Matchers with Inside
{
  this: AnyFlatSpec =>

  def createIterator(game: Game): MoveMakingIteratorWithoutVariations[T, U]

  def createElementOption(board: Board): Option[T]

  def createElement(board: Board, move: Move, nextBoard: Board): T

  def moveMakingIteratorWithoutVariations
  {
    it should "iterate by the elements for the game without the variations" in {
      val game = Game(
          event = "Jakies zdarzenie",
          site = "Nowakowo",
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
            MoveWithVariations(NormalMove(Piece.Knight, B8, C6, None, false),
              Vector())))
      val iter = createIterator(game)
      val elems = iter.toVector
      var i = 0
      inside(createElementOption(Board.Initial)) {
        case Some(expectedElem) =>
          elems should have length (5)
          elems(0) should be (expectedElem)
          i += 1
        case None =>
          elems should have length (4)
      }
      elems(i + 0) should be (createElement(
          Board.Initial,
          NormalMove(Piece.Pawn, E2, E4, None, false),
          Board("rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq - 0 1")))
      elems(i + 1) should be (createElement(
          Board("rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq - 0 1"),
          NormalMove(Piece.Pawn, E7, E5, None, false),
          Board("rnbqkbnr/pppp1ppp/8/4p3/4P3/8/PPPP1PPP/RNBQKBNR w KQkq - 0 2")))
      elems(i + 2) should be (createElement(
          Board("rnbqkbnr/pppp1ppp/8/4p3/4P3/8/PPPP1PPP/RNBQKBNR w KQkq - 0 2"),
          NormalMove(Piece.Knight, B1, C3, None, false),
          Board("rnbqkbnr/pppp1ppp/8/4p3/4P3/2N5/PPPP1PPP/R1BQKBNR b KQkq - 1 2")))
      elems(i + 3) should be (createElement(
          Board("rnbqkbnr/pppp1ppp/8/4p3/4P3/2N5/PPPP1PPP/R1BQKBNR b KQkq - 1 2"),
          NormalMove(Piece.Knight, B8, C6, None, false),
          Board("r1bqkbnr/pppp1ppp/2n5/4p3/4P3/2N5/PPPP1PPP/R1BQKBNR w KQkq - 2 3")))
    }

    it should "iterate by the elements for the game with one variation" in {
      val game = Game(
          event = "Jakies zdarzenie",
          site = "Nowakowo",
          date = "2021.??.??",
          round = "2",
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
      val iter = createIterator(game)
      val elems = iter.toVector
      var i = 0
      inside(createElementOption(Board.Initial)) {
        case Some(expectedElem) =>
          elems should have length (4)
          elems(0) should be (expectedElem)
          i += 1
        case None =>
          elems should have length (3)
      }
      elems(i + 0) should be (createElement(
          Board.Initial,
          NormalMove(Piece.Pawn, E2, E4, None, false),
          Board("rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq - 0 1")))
      elems(i + 1) should be (createElement(
          Board("rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq - 0 1"),
          NormalMove(Piece.Pawn, E7, E5, None, false),
          Board("rnbqkbnr/pppp1ppp/8/4p3/4P3/8/PPPP1PPP/RNBQKBNR w KQkq - 0 2")))
      elems(i + 2) should be (createElement(
          Board("rnbqkbnr/pppp1ppp/8/4p3/4P3/8/PPPP1PPP/RNBQKBNR w KQkq - 0 2"),
          NormalMove(Piece.Knight, G1, F3, None, false),
          Board("rnbqkbnr/pppp1ppp/8/4p3/4P3/5N2/PPPP1PPP/RNBQKB1R b KQkq - 1 2")))
    }

    it should "iterate by the elements for the game with two variations" in {
      val game = Game(
          event = "Jakies zdarzenie",
          site = "Nowakowo",
          date = "2021.??.??",
          round = "3",
          white = Vector("Nowak, Piotr"),
          black = Vector("Piotrowski, Pawel"),
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
              Vector(
                Vector(
                  MoveWithVariations(NormalMove(Piece.Bishop, F1, C4, None, false),
                    Vector()),
                  MoveWithVariations(NormalMove(Piece.Bishop, F8, C5, None, false),
                    Vector())),
                Vector(
                  MoveWithVariations(NormalMove(Piece.Knight, G1, F3, None, false),
                    Vector()),
                  MoveWithVariations(NormalMove(Piece.Knight, B8, C6, None, false),
                    Vector())))),
            MoveWithVariations(NormalMove(Piece.Knight, G8, F6, None, false),
              Vector())))
      val iter = createIterator(game)
      val elems = iter.toVector
      var i = 0
      inside(createElementOption(Board.Initial)) {
        case Some(expectedElem) =>
          elems should have length (5)
          elems(0) should be (expectedElem)
          i += 1
        case None =>
          elems should have length (4)
      }
      elems(i + 0) should be (createElement(
          Board.Initial,
          NormalMove(Piece.Pawn, E2, E4, None, false),
          Board("rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq - 0 1")))
      elems(i + 1) should be (createElement(
          Board("rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq - 0 1"),
          NormalMove(Piece.Pawn, E7, E5, None, false),
          Board("rnbqkbnr/pppp1ppp/8/4p3/4P3/8/PPPP1PPP/RNBQKBNR w KQkq - 0 2")))
      elems(i + 2) should be (createElement(
          Board("rnbqkbnr/pppp1ppp/8/4p3/4P3/8/PPPP1PPP/RNBQKBNR w KQkq - 0 2"),
          NormalMove(Piece.Knight, B1, C3, None, false),
          Board("rnbqkbnr/pppp1ppp/8/4p3/4P3/2N5/PPPP1PPP/R1BQKBNR b KQkq - 1 2")))
      elems(i + 3) should be (createElement(
          Board("rnbqkbnr/pppp1ppp/8/4p3/4P3/2N5/PPPP1PPP/R1BQKBNR b KQkq - 1 2"),
          NormalMove(Piece.Knight, G8, F6, None, false),
          Board("rnbqkb1r/pppp1ppp/5n2/4p3/4P3/2N5/PPPP1PPP/R1BQKBNR w KQkq - 2 3")))
    }

    it should "iterate by the elements for the game with the subvariation" in {
      val game = Game(
          event = "Jakies zdarzenie",
          site = "Nowakowo",
          date = "2021.??.??",
          round = "4",
          white = Vector("Kowalski, Jan"),
          black = Vector("Piotrowski, Pawel"),
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
            MoveWithVariations(NormalMove(Piece.Knight, B1, C3, None, false),
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
                  MoveWithVariations(NormalMove(Piece.Bishop, F1, D3, None, false),
                    Vector(
                      Vector(
                        MoveWithVariations(NormalMove(Piece.Knight, G1, F3, None, false),
                          Vector())))),
                  MoveWithVariations(NormalMove(Piece.Pawn, E7, E5, None, false),
                    Vector())))),
            MoveWithVariations(NormalMove(Piece.Knight, G1, F3, None, false),
              Vector())))
      val iter = createIterator(game)
      val elems = iter.toVector
      var i = 0
      inside(createElementOption(Board.Initial)) {
        case Some(expectedElem) =>
          elems should have length (6)
          elems(0) should be (expectedElem)
          i += 1
        case None =>
          elems should have length (5)
      }
      elems(i + 0) should be (createElement(
          Board.Initial,
          NormalMove(Piece.Knight, B1, C3, None, false),
          Board("rnbqkbnr/pppppppp/8/8/8/2N5/PPPPPPPP/R1BQKBNR b KQkq - 1 1")))
      elems(i + 1) should be (createElement(
          Board("rnbqkbnr/pppppppp/8/8/8/2N5/PPPPPPPP/R1BQKBNR b KQkq - 1 1"),
          NormalMove(Piece.Knight, B8, C6, None, false),
          Board("r1bqkbnr/pppppppp/2n5/8/8/2N5/PPPPPPPP/R1BQKBNR w KQkq - 2 2")))
      elems(i + 2) should be (createElement(
          Board("r1bqkbnr/pppppppp/2n5/8/8/2N5/PPPPPPPP/R1BQKBNR w KQkq - 2 2"),
          NormalMove(Piece.Pawn, E2, E4, None, false),
          Board("r1bqkbnr/pppppppp/2n5/8/4P3/2N5/PPPP1PPP/R1BQKBNR b KQkq - 0 2")))
      elems(i + 3) should be (createElement(
          Board("r1bqkbnr/pppppppp/2n5/8/4P3/2N5/PPPP1PPP/R1BQKBNR b KQkq - 0 2"),
          NormalMove(Piece.Pawn, E7, E5, None, false),
          Board("r1bqkbnr/pppp1ppp/2n5/4p3/4P3/2N5/PPPP1PPP/R1BQKBNR w KQkq - 0 3")))
      elems(i + 4) should be (createElement(
          Board("r1bqkbnr/pppp1ppp/2n5/4p3/4P3/2N5/PPPP1PPP/R1BQKBNR w KQkq - 0 3"),
          NormalMove(Piece.Knight, G1, F3, None, false),
          Board("r1bqkbnr/pppp1ppp/2n5/4p3/4P3/2N2N2/PPPP1PPP/R1BQKB1R b KQkq - 1 3")))
    }

    it should "iterate by the elements for the game with the empty variation" in {
      val game = Game(
          event = "Jakies zdarzenie",
          site = "Nowakowo",
          date = "2021.??.??",
          round = "5",
          white = Vector("Nowak, Piotr"),
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
            MoveWithVariations(NormalMove(Piece.Pawn, E2, E4, None, false),
              Vector()),
            MoveWithVariations(NormalMove(Piece.Pawn, E7, E5, None, false),
              Vector()),
            MoveWithVariations(NormalMove(Piece.Knight, G1, F3, None, false),
              Vector(
                Vector())),
            MoveWithVariations(NormalMove(Piece.Knight, G8, F6, None, false),
              Vector())))
      val iter = createIterator(game)
      val elems = iter.toVector
      var i = 0
      inside(createElementOption(Board.Initial)) {
        case Some(expectedElem) =>
          elems should have length (5)
          elems(0) should be (expectedElem)
          i += 1
        case None =>
          elems should have length (4)
      }
      elems(i + 0) should be (createElement(
          Board.Initial,
          NormalMove(Piece.Pawn, E2, E4, None, false),
          Board("rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq - 0 1")))
      elems(i + 1) should be (createElement(
          Board("rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq - 0 1"),
          NormalMove(Piece.Pawn, E7, E5, None, false),
          Board("rnbqkbnr/pppp1ppp/8/4p3/4P3/8/PPPP1PPP/RNBQKBNR w KQkq - 0 2")))
      elems(i + 2) should be (createElement(
          Board("rnbqkbnr/pppp1ppp/8/4p3/4P3/8/PPPP1PPP/RNBQKBNR w KQkq - 0 2"),
          NormalMove(Piece.Knight, G1, F3, None, false),
          Board("rnbqkbnr/pppp1ppp/8/4p3/4P3/5N2/PPPP1PPP/RNBQKB1R b KQkq - 1 2")))
      elems(i + 3) should be (createElement(
          Board("rnbqkbnr/pppp1ppp/8/4p3/4P3/5N2/PPPP1PPP/RNBQKB1R b KQkq - 1 2"),
          NormalMove(Piece.Knight, G8, F6, None, false),
          Board("rnbqkb1r/pppp1ppp/5n2/4p3/4P3/5N2/PPPP1PPP/RNBQKB1R w KQkq - 2 3")))
    }
  }

  def moveMakingIteratorWithoutVariationsForIllegalMoves
  {
    it should "iterate by the elements for the game without the variations with the illegal move" in {
      val game = Game(
          event = "Jakies zdarzenie",
          site = "Nowakowo",
          date = "2021.??.??",
          round = "6",
          white = Vector("Piotrowski, Pawel"),
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
            MoveWithVariations(NormalMove(Piece.Pawn, E2, E4, None, false),
              Vector()),
            MoveWithVariations(NormalMove(Piece.Pawn, E7, E5, None, false),
              Vector()),
            MoveWithVariations(NormalMove(Piece.Bishop, C1, G5, None, false),
              Vector()),
            MoveWithVariations(NormalMove(Piece.Knight, B8, C6, None, false),
              Vector())))
      val iter = createIterator(game)
      val elems = iter.toVector
      var i = 0
      inside(createElementOption(Board.Initial)) {
        case Some(expectedElem) =>
          elems should have length (3)
          elems(0) should be (expectedElem)
          i += 1
        case None =>
          elems should have length (2)
      }
      elems(i + 0) should be (createElement(
          Board.Initial,
          NormalMove(Piece.Pawn, E2, E4, None, false),
          Board("rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq - 0 1")))
      elems(i + 1) should be (createElement(
          Board("rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq - 0 1"),
          NormalMove(Piece.Pawn, E7, E5, None, false),
          Board("rnbqkbnr/pppp1ppp/8/4p3/4P3/8/PPPP1PPP/RNBQKBNR w KQkq - 0 2")))
    }

    it should "iterate by the elements for the game with one variation and the illegal move" in {
      val game = Game(
          event = "Jakies zdarzenie",
          site = "Nowakowo",
          date = "2021.??.??",
          round = "7",
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
            MoveWithVariations(NormalMove(Piece.Pawn, E2, E4, None, false),
              Vector()),
            MoveWithVariations(NormalMove(Piece.Pawn, E7, E5, None, false),
              Vector()),
            MoveWithVariations(NormalMove(Piece.Knight, B1, C3, None, false),
              Vector(
                Vector(
                  MoveWithVariations(NormalMove(Piece.Knight, G1, F3, None, false),
                    Vector()),
                  MoveWithVariations(NormalMove(Piece.Bishop, C8, G4, None, false),
                    Vector())))),
            MoveWithVariations(NormalMove(Piece.Knight, G8, F6, None, false),
              Vector())))
      val iter = createIterator(game)
      val elems = iter.toVector
      var i = 0
      inside(createElementOption(Board.Initial)) {
        case Some(expectedElem) =>
          elems should have length (5)
          elems(0) should be (expectedElem)
          i += 1
        case None =>
          elems should have length (4)
      }
      elems(i + 0) should be (createElement(
          Board.Initial,
          NormalMove(Piece.Pawn, E2, E4, None, false),
          Board("rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq - 0 1")))
      elems(i + 1) should be (createElement(
          Board("rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq - 0 1"),
          NormalMove(Piece.Pawn, E7, E5, None, false),
          Board("rnbqkbnr/pppp1ppp/8/4p3/4P3/8/PPPP1PPP/RNBQKBNR w KQkq - 0 2")))
      elems(i + 2) should be (createElement(
          Board("rnbqkbnr/pppp1ppp/8/4p3/4P3/8/PPPP1PPP/RNBQKBNR w KQkq - 0 2"),
          NormalMove(Piece.Knight, B1, C3, None, false),
          Board("rnbqkbnr/pppp1ppp/8/4p3/4P3/2N5/PPPP1PPP/R1BQKBNR b KQkq - 1 2")))
      elems(i + 3) should be (createElement(
          Board("rnbqkbnr/pppp1ppp/8/4p3/4P3/2N5/PPPP1PPP/R1BQKBNR b KQkq - 1 2"),
          NormalMove(Piece.Knight, G8, F6, None, false),
          Board("rnbqkb1r/pppp1ppp/5n2/4p3/4P3/2N5/PPPP1PPP/R1BQKBNR w KQkq - 2 3")))
    }
  }
}
