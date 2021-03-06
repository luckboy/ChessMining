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
package pl.luckboy.chessmining

/** A show variable that controls showing of file progress bar.
  *
  * The example usage is:
  * {{{
  * val showVar = ShowVariable()
  * val games = Games.fromFile("somefile.pgn", showVar)
  * val iter = for(g <- games; b <- Boards.fromGame(g)) yield (g, b) // ->
  * showVar.flag = ShowFlag.Set
  * }}}
  *
  * @param flag the show flag.
  */
case class ShowVariable(var flag: ShowFlag.Value = ShowFlag.Clear)
