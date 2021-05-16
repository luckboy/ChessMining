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
  def side(side: Side.Value) =
    side match {
      case Side.White => white
      case Side.Black => black
    }

  def sideEloOption(side: Side.Value) =
    side match {
      case Side.White => whiteEloOption
      case Side.Black => blackEloOption
    }

  def sideUSCFOption(side: Side.Value) =
    side match {
      case Side.White => whiteUSCFOption
      case Side.Black => blackUSCFOption
    }
}
