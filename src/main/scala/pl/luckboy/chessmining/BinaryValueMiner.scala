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

abstract class BinaryValueMiner[-T, U, +V <: BinaryValueMiner[T, U, V, W], +W <: BinaryValueMiner[T, U, _, W]] extends Miner[T, Vector[(String, U)]]
{
  def firstMinerOption: Option[W]

  def secondMinerOption: Option[W]

  def firstAdjective: String

  def secondAdjective: String

  def firstNounOption: Option[String] 

  def secondNounOption: Option[String] 

  def adjectives: Vector[String] = {
    val firstAdjectives = firstMinerOption.map { _.adjectives.map { _ + " " + firstAdjective } }.getOrElse(Vector(firstAdjective))
    val secondAdjectives = secondMinerOption.map { _.adjectives }.getOrElse(Vector(secondAdjective))
    firstAdjectives ++ secondAdjectives
  }

  def nounOptions: Vector[Option[String]] = {
    val firstNounOptions = firstMinerOption.map { _.nounOptions.map { (sOpt: Option[String]) => firstNounOption } }.getOrElse(Vector(firstNounOption))
    val secondNounOptions = secondMinerOption.map { _.nounOptions }.getOrElse(Vector(secondNounOption))
    firstNounOptions ++ secondNounOptions
  }

  def names =
    adjectives.zip(nounOptions).map {
      case ((s: String, sOpt: Option[String])) => s + sOpt.map { " " + _ }.getOrElse("")
    }

  def count: Int =
    firstMinerOption.map { _.count } .getOrElse(1) + secondMinerOption.map { _.count }.getOrElse(1)

  def +\[X >: W, Y >: V](firstMiner: X)(implicit binaryValueMinerFactory: BinaryValueMinerFactory[V, X, Y]) =
    binaryValueMinerFactory(this.asInstanceOf[V], Some(firstMiner), secondMinerOption)
  
  def +/[X >: W, Y >: V](secondMiner: X)(implicit binaryValueMinerFactory: BinaryValueMinerFactory[V, X, Y]) =
    binaryValueMinerFactory(this.asInstanceOf[V], firstMinerOption, Some(secondMiner))

  def -\[X >: V]()(implicit binaryValueMinerFactory: BinaryValueMinerFactory[V, W, X]) =
    binaryValueMinerFactory(this.asInstanceOf[V], None, secondMinerOption)

  def -/[X >: V]()(implicit binaryValueMinerFactory: BinaryValueMinerFactory[V, W, X]) =
    binaryValueMinerFactory(this.asInstanceOf[V], firstMinerOption, None)
}
