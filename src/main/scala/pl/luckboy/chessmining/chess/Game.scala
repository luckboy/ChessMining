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
package pl.luckboy.chessmining.chess

/** Represents a chess game.
  *
  * @param event the event.
  * @param site the site.
  * @param date the date.
  * @param round the round.
  * @param white the white players.
  * @param black the black players.
  * @param result the result of the game.
  * @param eventDateOption the optional event date.
  * @param whiteEloOption the optional FIDE Elo ratings of the white players.
  * @param blackEloOption the optional FIDE Elo ratings of the black players.
  * @param whiteUSCFOption the optional USCF ratings of the white players.
  * @param blackUSCFOption the optional USCF ratings of the black players.
  * @param ecoOption the optional ECO.
  * @param timeControl the optional time control.
  * @param tags the tags.
  * @param boardOption the optional start board.
  * @param moveWithVariations the moves with variations.
  */
case class Game(
  event: String,
  site: String,
  date: String,
  round: String,
  white: Vector[String],
  black: Vector[String],
  result: Result.Value,
  eventDateOption: Option[String],
  whiteEloOption: Option[Vector[Option[Int]]],
  blackEloOption: Option[Vector[Option[Int]]],
  whiteUSCFOption: Option[Vector[Option[Int]]],
  blackUSCFOption: Option[Vector[Option[Int]]],
  ecoOption: Option[String],
  timeControlOption: Option[Vector[Option[TimeControl]]],
  tags: Map[String, String],
  boardOption: Option[Board],
  movesWithVariations: Vector[MoveWithVariations])
{
  /** Returns the side players.
    *
    * @param side the side.
    * @return the side players.
    */
  def side(side: Side.Value) =
    side match {
      case Side.White => white
      case Side.Black => black
    }

  /** Returns the optional FIDE Elo ratings of the side players.
    *
    * @param side the side.
    * @return the optional FIDE Elo ratings of the side players.
    */
  def sideEloOption(side: Side.Value) =
    side match {
      case Side.White => whiteEloOption
      case Side.Black => blackEloOption
    }

  /** Returns the optional USCF ratings of the side players.
    *
    * @param side the side.
    * @return the optional USCF ratings of the side players.
    */
  def sideUSCFOption(side: Side.Value) =
    side match {
      case Side.White => whiteUSCFOption
      case Side.Black => blackUSCFOption
    }

  /** Checks whether this game has side win.
    *
    * @param side the side.
    * @return `true` if this game has side win, otherwise `false`.
    */
  def hasSideWin(side: Side.Value) = isSideWin(result, side)

  /** Checks whether this game has side loss.
    *
    * @param side the side.
    * @return `true` if this game has side loss, otherwise `false`.
    */
  def hasSideLoss(side: Side.Value) = isSideLoss(result, side)

  /** Check whether this game has draw.
    *
    * @return `true` if this game has draw, otherwise `false`.
    */    
  def hasDraw = isDraw(result)

  /** Return the optional win side.
    *
    * @return the optional win side.
    */
  def winSideOption = resultToWinSideOption(result)

  /** Return the optional loss side.
    *
    * @return the optional loss side.
    */
  def lossSideOption = resultToLossSideOption(result)
}
