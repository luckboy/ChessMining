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
import java.io._
import org.scalatest.flatspec._
import org.scalatest.matchers._
import pl.luckboy.chessmining.chess._
import pl.luckboy.chessmining.iterator._
import pl.luckboy.chessmining.ui._
import Squares._

class GameReaderIteratorSpec extends AnyFlatSpec with should.Matchers
{
  "A GameReaderIterator" should "iterate by one game" in {
    val s =
      "[Event \"Jakies zdarzenie\"]\n" +
      "[Site \"Nowakowo\"]\n" +
      "[Date \"2021.??.??\"]\n" +
      "[Round \"1\"]\n" +
      "[White \"Kowalski, Jan\"]\n" +
      "[Black \"Nowak, Piotr\"]\n" +
      "[Result \"*\"]\n" +
      "\n" +
      "1. e4 e5 2. Nc3 Nc6 *\n" +
      "\n"
    val fileProgressBar = new DummyFileProgressBar("some file", s.length)
    val iter = new GameReaderIterator(new PGNReader(new StringReader(s)), fileProgressBar)
    val games = iter.toVector
    games should have length (1)
    games(0).event should be ("Jakies zdarzenie")
    games(0).site should be ("Nowakowo")
    games(0).date should be ("2021.??.??")
    games(0).round should be ("1")
    games(0).white should have length (1)
    games(0).white(0) should be ("Kowalski, Jan")
    games(0).black should have length (1)
    games(0).black(0) should be ("Nowak, Piotr")
    games(0).result should be (Result.Unfinished)
    games(0).eventDateOption should be (None)
    games(0).whiteEloOption should be (None)
    games(0).blackEloOption should be (None)
    games(0).whiteUSCFOption should be (None)
    games(0).blackUSCFOption should be (None)
    games(0).ecoOption should be (None)
    games(0).timeControlOption should be (None)
    games(0).boardOption should be (None)
    games(0).movesWithVariations should have length (4)
    games(0).movesWithVariations(0).move should be (NormalMove(Piece.Pawn, E2, E4, None, false))
    games(0).movesWithVariations(0).variations shouldBe empty
    games(0).movesWithVariations(1).move should be (NormalMove(Piece.Pawn, E7, E5, None, false))
    games(0).movesWithVariations(1).variations shouldBe empty
    games(0).movesWithVariations(2).move should be (NormalMove(Piece.Knight, B1, C3, None, false))
    games(0).movesWithVariations(2).variations shouldBe empty
    games(0).movesWithVariations(3).move should be (NormalMove(Piece.Knight, B8, C6, None, false))
    games(0).movesWithVariations(3).variations shouldBe empty
  }

  it should "iterate by two games" in {
    val s =
      "[Event \"Jakies zdarzenie\"]\n" +
      "[Site \"Nowakowo\"]\n" +
      "[Date \"2021.??.??\"]\n" +
      "[Round \"2\"]\n" +
      "[White \"Jankowski, Piotr\"]\n" +
      "[Black \"Kowalski, Jan\"]\n" +
      "[Result \"*\"]\n" +
      "\n" +
      "1. e4 e5 2. Nf3 Nf6 *\n" +
      "\n" +
      "[Event \"Jakies zdarzenie\"]\n" +
      "[Site \"Nowakowo\"]\n" +
      "[Date \"2021.??.??\"]\n" +
      "[Round \"3\"]\n" +
      "[White \"Pawelski, Piotr\"]\n" +
      "[Black \"Nowak, Piotr\"]\n" +
      "[Result \"*\"]\n" +
      "\n" +
      "1. e4 e5 2. Nc3 Nf6 *\n" +
      "\n"
    val fileProgressBar = new DummyFileProgressBar("some file", s.length)
    val iter = new GameReaderIterator(new PGNReader(new StringReader(s)), fileProgressBar)
    val games = iter.toVector
    games should have length (2)
    games(0).event should be ("Jakies zdarzenie")
    games(0).site should be ("Nowakowo")
    games(0).date should be ("2021.??.??")
    games(0).round should be ("2")
    games(0).white should have length (1)
    games(0).white(0) should be ("Jankowski, Piotr")
    games(0).black should have length (1)
    games(0).black(0) should be ("Kowalski, Jan")
    games(0).result should be (Result.Unfinished)
    games(0).eventDateOption should be (None)
    games(0).whiteEloOption should be (None)
    games(0).blackEloOption should be (None)
    games(0).whiteUSCFOption should be (None)
    games(0).blackUSCFOption should be (None)
    games(0).ecoOption should be (None)
    games(0).timeControlOption should be (None)
    games(0).boardOption should be (None)
    games(0).movesWithVariations should have length (4)
    games(0).movesWithVariations(0).move should be (NormalMove(Piece.Pawn, E2, E4, None, false))
    games(0).movesWithVariations(0).variations shouldBe empty
    games(0).movesWithVariations(1).move should be (NormalMove(Piece.Pawn, E7, E5, None, false))
    games(0).movesWithVariations(1).variations shouldBe empty
    games(0).movesWithVariations(2).move should be (NormalMove(Piece.Knight, G1, F3, None, false))
    games(0).movesWithVariations(2).variations shouldBe empty
    games(0).movesWithVariations(3).move should be (NormalMove(Piece.Knight, G8, F6, None, false))
    games(0).movesWithVariations(3).variations shouldBe empty
    games(1).event should be ("Jakies zdarzenie")
    games(1).site should be ("Nowakowo")
    games(1).date should be ("2021.??.??")
    games(1).round should be ("3")
    games(1).white should have length (1)
    games(1).white(0) should be ("Pawelski, Piotr")
    games(1).black should have length (1)
    games(1).black(0) should be ("Nowak, Piotr")
    games(1).result should be (Result.Unfinished)
    games(1).eventDateOption should be (None)
    games(1).whiteEloOption should be (None)
    games(1).blackEloOption should be (None)
    games(1).whiteUSCFOption should be (None)
    games(1).blackUSCFOption should be (None)
    games(1).ecoOption should be (None)
    games(1).timeControlOption should be (None)
    games(1).boardOption should be (None)
    games(1).movesWithVariations should have length (4)
    games(1).movesWithVariations(0).move should be (NormalMove(Piece.Pawn, E2, E4, None, false))
    games(1).movesWithVariations(0).variations shouldBe empty
    games(1).movesWithVariations(1).move should be (NormalMove(Piece.Pawn, E7, E5, None, false))
    games(1).movesWithVariations(1).variations shouldBe empty
    games(1).movesWithVariations(2).move should be (NormalMove(Piece.Knight, B1, C3, None, false))
    games(1).movesWithVariations(2).variations shouldBe empty
    games(1).movesWithVariations(3).move should be (NormalMove(Piece.Knight, G8, F6, None, false))
    games(1).movesWithVariations(3).variations shouldBe empty
  }
  
  it should "iterate for the empty string" in {
    val s = ""
    val fileProgressBar = new DummyFileProgressBar("some file", s.length)
    val iter = new GameReaderIterator(new PGNReader(new StringReader(s)), fileProgressBar)
    val games = iter.toVector
    games shouldBe empty
  }
}
