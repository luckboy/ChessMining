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
import org.scalatest.flatspec._
import pl.luckboy.chessmining.chess._
import pl.luckboy.chessmining.iterator._

class BoardIteratorWithoutVariationsSpec extends AnyFlatSpec with MoveMakingIteratorWithoutVariationsBehaviors[Board, Board]
{
  def createIterator(game: Game) = new BoardIteratorWithoutVariations(game)

  def createElementOption(board: Board) = Some(board)

  def createElement(board: Board, move: Move, nextBoard: Board) = nextBoard

  "A BoardIteratorWithoutVariations" should behave like moveMakingIteratorWithoutVariations

  it should behave like moveMakingIteratorWithoutVariationsForIllegalMoves
}
