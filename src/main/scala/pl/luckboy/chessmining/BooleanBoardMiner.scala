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

abstract class BooleanBoardMiner[-T, +U <: BooleanBoardMiner[T, U]] extends BinaryBoardMiner[T, U]
{
  def booleanSquareFunction(x: T, squ: Int): Boolean

  def squareFunction(x: Vector[(String, Long)], y: T, squ: Int) = {
    val firstCount = firstMinerOption.map { _.count }.getOrElse(1)
    val secondCount = secondMinerOption.map { _.count }.getOrElse(1)
    if(booleanSquareFunction(y, squ)) {
      val firstValue = firstMinerOption.map {
        _.squareFunction(x.slice(0, firstCount), y, squ)
      }.getOrElse(Vector(x(0)._1 -> (x(0)._2 + 1L)))
      (0 until firstCount).foldLeft(x) {
        (x2: Vector[(String, Long)], i: Int) => x2.updated(i, x2(i)._1 -> firstValue(i)._2)
      }
    } else {
      val secondValue = secondMinerOption.map {
        _.squareFunction(x.slice(firstCount, firstCount + secondCount), y, squ)
      }.getOrElse(Vector(x(firstCount)._1 -> (x(firstCount)._2 + 1L)))
      (firstCount until (firstCount + secondCount)).foldLeft(x) {
        (x2: Vector[(String, Long)], i: Int) => x2.updated(i, x2(i)._1 -> secondValue(i - firstCount)._2)
      }
    }
  }

  def function(x: Vector[(String, Array[Long])], y: T) = {
    val firstCount = firstMinerOption.map { _.count }.getOrElse(1)
    val secondCount = secondMinerOption.map { _.count }.getOrElse(1)
    (0 until count).foldLeft(x) {
      (x2: Vector[(String, Array[Long])], i: Int) =>
        val array = Array.fill(64)(0L)
        Array.copy(x2(i)._2, 0, array, 0, 64)
        x2.updated(i, x2(i)._1 -> array)
    }
    (0 until 64).foldLeft(x) {
      (x2: Vector[(String, Array[Long])], squ: Int) =>
        if(booleanSquareFunction(y, squ)) {
          val firstSquValue = firstMinerOption.map {
            _.squareFunction(x2.slice(0, firstCount).map {
              case ((s: String, array: Array[Long])) => s -> array(squ)
             }, y, squ)
          }.getOrElse(Vector(x2(0)._1 -> (x2(0)._2(squ) + 1L)))
          for(i <- 0 until firstCount) {
            x2(i)._2(squ) = firstSquValue(i)._2
          }
          x2
        } else {
          val secondSquValue = secondMinerOption.map {
            _.squareFunction(x2.slice(firstCount, firstCount + secondCount).map {
              case ((s: String, array: Array[Long])) => s -> array(squ)
             }, y, squ)
          }.getOrElse(Vector(x2(firstCount)._1 -> (x2(firstCount)._2(squ) + 1L)))
          for(i <- firstCount until (firstCount + secondCount)) {
            x2(i)._2(squ) = secondSquValue(i - firstCount)._2
          }
          x2
        }
    }
  }
}
