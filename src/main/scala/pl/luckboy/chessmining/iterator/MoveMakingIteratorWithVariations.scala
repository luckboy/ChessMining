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
package pl.luckboy.chessmining.iterator
import scala.collection.mutable.Stack
import pl.luckboy.chessmining.chess._

/** An interator of move making with the variations.
  *
  * @tparam T the element type.
  * @tparam U the state type.
  */
trait MoveMakingIteratorWithVariations[T, U] extends MoveMakingIterator[T, U]
{
  private case class StackElement(
    variations: Vector[Vector[MoveWithVariations]],
    startState: U,
    variationIndex: Int,
    moveIndex: Int,
    currentState: U)

  private var stack = Stack[StackElement]()
  
  override protected def initialize(game: Game)
  {
    val state = boardToState(game.boardOption.getOrElse(Board.Initial))
    stack = Stack(StackElement(
        Vector(game.movesWithVariations),
        state,
        0,
        0,
        state))
  }

  override protected def nextOption() = {
    var r = None: Option[T]
    var isStop = false
    while(!stack.isEmpty && !isStop) {
      var elem = stack.top
      while(elem.variationIndex < elem.variations.length && !isStop) {
        if(elem.moveIndex < elem.variations(elem.variationIndex).length) {
          val move = elem.variations(elem.variationIndex)(elem.moveIndex).move
          makeMove(elem.currentState, move) match {
            case Some((x, newState)) =>
              r = Some(x)
              stack.pop()
              stack.push(elem.copy(moveIndex = elem.moveIndex + 1, currentState = newState))
              val variations = elem.variations(elem.variationIndex)(elem.moveIndex).variations
              if(variations.length > 0)
                stack.push(StackElement(
                    variations,
                    elem.currentState,
                    0,
                    0,
                    elem.currentState))
              isStop = true
            case None =>
              ()
          }
        }
        if(!isStop) {
          stack.pop()
          stack.push(elem.copy(variationIndex = elem.variationIndex + 1, moveIndex = 0, currentState = elem.startState))
          elem = stack.top
        }
      }
      if(!isStop) stack.pop()
    }
    r
  }
}
