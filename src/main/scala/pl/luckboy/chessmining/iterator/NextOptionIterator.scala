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

trait NextOptionIterator[T] extends Iterator[T]
{
  private var nextElementOption = None: Option[T]

  protected def nextOption(): Option[T]

  override def hasNext = {
    if(nextElementOption != None) {
      true
    } else {
      nextElementOption = nextOption()
      nextElementOption != None
    }
  }

  override def next() = {
    val r = if(nextElementOption == None) nextOption() else nextElementOption
    nextElementOption = None
    r match {
      case Some(elem) => elem
      case None       => Iterator.empty.next()
    }
  }
}
