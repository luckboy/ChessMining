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

case class NamedFunction6[-T1, -T2, -T3, -T4, -T5, -T6, +R](name: String, function: (T1, T2, T3, T4, T5, T6) => R) extends Function6[T1, T2, T3, T4, T5, T6, R]
{
  override def apply(v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6) = function(v1, v2, v3, v4, v5, v6)

  override def toString() = name
}
