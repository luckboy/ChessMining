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

/** A named function of 1 parameter that has the name.
  *
  * @tparam T1 the 1st parameter type.
  * @tparam R the result.
  * @param name the name.
  * @param function the function.
  */
case class NamedFunction1[-T1, +R](name: String, function: (T1) => R) extends Function1[T1, R]
{
  override def apply(v1: T1) = function(v1)

  override def toString() = name
}
