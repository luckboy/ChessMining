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

/** A named function of 7 parameters that has the name.
  *
  * @tparam T1 the 1st parameter type.
  * @tparam T2 the 2nd parameter type.
  * @tparam T3 the 3rd parameter type.
  * @tparam T4 the 4th parameter type.
  * @tparam T5 the 5th parameter type.
  * @tparam T6 the 6th parameter type.
  * @tparam T7 the 7th parameter type.
  * @tparam R the result type.
  * @param name the name.
  * @param function the function.
  */
case class NamedFunction7[-T1, -T2, -T3, -T4, -T5, -T6, -T7, +R](name: String, function: (T1, T2, T3, T4, T5, T6, T7) => R) extends Function7[T1, T2, T3, T4, T5, T6, T7, R]
{
  override def apply(v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7) = function(v1, v2, v3, v4, v5, v6, v7)

  override def toString() = name
}
