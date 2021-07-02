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

/** A binary value miner that divides value on two or more value elements. The binary value miner has
  * two optional miners.
  *
  * @tparam T the type of data element.
  * @tparam U the value type.
  * @tparam V the type of actual miner.
  * @tparam W the type of two optional miners. 
  */
abstract class BinaryValueMiner[-T, U, +V <: BinaryValueMiner[T, U, V, W], +W <: BinaryValueMiner[T, U, _, W]] extends Miner[T, Vector[(String, U)]]
{
  /** Returns the optional first miner.
    *
    * @return the optional first miner.
    */
  def firstMinerOption: Option[W]

  /** Returns the optional second miner.
    *
    * @return the optional second miner.
    */
  def secondMinerOption: Option[W]

  /** Returns the first adjective of name.
    *
    * @return the first adjective of name.
    */
  def firstAdjective: String

  /** Returns the second adjective of name.
    *
    * @return the second adjective of name.
    */
  def secondAdjective: String

  /** Returns the optional first noun of name.
    *
    * @return the optional first noun of name.
    */
  def firstNounOption: Option[String] 

  /** Returns the optional second noun of name.
    *
    * @return the optional second noun of name.
    */
  def secondNounOption: Option[String] 

  /** Returns the name adjectives.
    *
    * @return the name adjectives.
    */
  def adjectives: Vector[String] = {
    val firstAdjectives = firstMinerOption.map { _.adjectives.map { _ + " " + firstAdjective } }.getOrElse(Vector(firstAdjective))
    val secondAdjectives = secondMinerOption.map { _.adjectives }.getOrElse(Vector(secondAdjective))
    firstAdjectives ++ secondAdjectives
  }

  /** Returns the optional name nouns.
    *
    * @return the optional name nouns.
    */
  def nounOptions: Vector[Option[String]] = {
    val firstNounOptions = firstMinerOption.map { _.nounOptions.map { (sOpt: Option[String]) => firstNounOption } }.getOrElse(Vector(firstNounOption))
    val secondNounOptions = secondMinerOption.map { _.nounOptions }.getOrElse(Vector(secondNounOption))
    firstNounOptions ++ secondNounOptions
  }

  /** Returns the names of value elements.
    *
    * @return the names of value elements.
    */
  def names =
    adjectives.zip(nounOptions).map {
      case ((s: String, sOpt: Option[String])) => s + sOpt.map { " " + _ }.getOrElse("")
    }

  /** Returns the number of value elements.
    *
    * @return the number of value elements.
    */
  def count: Int =
    firstMinerOption.map { _.count }.getOrElse(1) + secondMinerOption.map { _.count }.getOrElse(1)

  /** Creates a new miner with the first miner from this miner.
    *
    * @tparam X the type of first miner.
    * @tparam Y the miner type.
    * @param firstMiner the first miner.
    * @param binaryValueMinerFactory the factory of binary value miner.
    * @return a new miner.
    */
  def +\[X >: W, Y >: V](firstMiner: X)(implicit binaryValueMinerFactory: BinaryValueMinerFactory[V, X, Y]) =
    binaryValueMinerFactory(this.asInstanceOf[V], Some(firstMiner), secondMinerOption)
  
  /** Creates a new miner with the second miner from this miner.
    *
    * @tparam X the type of second miner.
    * @tparam Y the miner type.
    * @param secondMiner the second miner.
    * @param binaryValueMinerFactory the factory of binary value miner.
    * @return a new miner.
    */
  def +/[X >: W, Y >: V](secondMiner: X)(implicit binaryValueMinerFactory: BinaryValueMinerFactory[V, X, Y]) =
    binaryValueMinerFactory(this.asInstanceOf[V], firstMinerOption, Some(secondMiner))

  /** Creates a new miner without the first miner from this miner.
    *
    * @tparam X the miner type.
    * @param binaryValueMinerFactory the factory of binary value miner.
    * @return a new miner.
    */
  def -\[X >: V]()(implicit binaryValueMinerFactory: BinaryValueMinerFactory[V, W, X]) =
    binaryValueMinerFactory(this.asInstanceOf[V], None, secondMinerOption)

  /** Creates a new miner without the second miner from this miner.
    *
    * @tparam X the miner type.
    * @param binaryValueMinerFactory the factory of binary value miner.
    * @return a new miner.
    */
  def -/[X >: V]()(implicit binaryValueMinerFactory: BinaryValueMinerFactory[V, W, X]) =
    binaryValueMinerFactory(this.asInstanceOf[V], firstMinerOption, None)
}
