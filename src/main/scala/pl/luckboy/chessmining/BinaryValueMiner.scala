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

abstract class BinaryValueMiner[-T, U, +V <: BinaryValueMiner[T, U, V]] extends Miner[T, Vector[(String, U)]]
{
  def firstMinerOption: Option[V]

  def secondMinerOption: Option[V]

  def firstAdjactive: String

  def secondAdjactive: String

  def firstNounOption: Option[String] 

  def secondNounOption: Option[String] 

  def adjactives: Vector[String] = {
    val firstAdjactives = firstMinerOption.map { _.adjactives.map { _ + " " + firstAdjactive } }.getOrElse(Vector(firstAdjactive))
    val secondAdjactives = secondMinerOption.map { _.adjactives }.getOrElse(Vector(secondAdjactive))
    firstAdjactives ++ secondAdjactives
  }

  def nounOptions: Vector[Option[String]] = {
    val firstNounOptions = firstMinerOption.map { _.nounOptions.map { (sOpt: Option[String]) => firstNounOption } }.getOrElse(Vector(firstNounOption))
    val secondNounOptions = secondMinerOption.map { _.nounOptions }.getOrElse(Vector(secondNounOption))
    firstNounOptions ++ secondNounOptions
  }

  def names =
    adjactives.zip(nounOptions).map {
      case ((s: String, sOpt: Option[String])) => s + sOpt.map { " " + _ }.getOrElse("")
    }

  def count: Int =
    firstMinerOption.map { _.count } .getOrElse(1) + secondMinerOption.map { _.count }.getOrElse(1)

  def +\[W >: V](firstMiner: W): V
  
  def +/[W >: V](secondMiner: W): V
}
