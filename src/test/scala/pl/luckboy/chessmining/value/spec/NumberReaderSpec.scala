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
package pl.luckboy.chessmining.value.spec
import java.io._
import org.scalatest._
import org.scalatest.flatspec._
import org.scalatest.matchers._
import pl.luckboy.chessmining.value._

class NumberReaderSpec extends AnyFlatSpec with should.Matchers with Inside
{
  "A NumberReader.readString" should "reads one string" in {
    val s = "abcdef"
    val r = new NumberReader(new StringReader(s))
    r.readString() should be ("abcdef")
  }

  it should "reads two strings" in {
    val s = "abcdef ABCDEF"
    val r = new NumberReader(new StringReader(s))
    r.readString() should be ("abcdef")
    r.readString() should be ("ABCDEF")
  }

  "A NumberReader.readInt" should "reads one number" in {
    val s = "1234"
    val r = new NumberReader(new StringReader(s))
    inside(r.readInt()) {
      case Some(x) => x should be (1234) 
    }
  }

  it should "reads two numbers" in {
    val s = "-1234 5678"
    val r = new NumberReader(new StringReader(s))
    inside(r.readInt()) {
      case Some(x) => x should be (-1234) 
    }
    inside(r.readInt()) {
      case Some(x) => x should be (5678) 
    }
  }
  
  it should "complain on the incorrect number" in {
    val s = "abcdef"
    val r = new NumberReader(new StringReader(s))
    r.readInt() should be (None)
  }

  "A NumberReader.readLong" should "reads one number" in {
    val s = "12345678901"
    val r = new NumberReader(new StringReader(s))
    inside(r.readLong()) {
      case Some(x) => x should be (12345678901L) 
    }
  }

  it should "reads two numbers" in {
    val s = "-12345678901 56789012345"
    val r = new NumberReader(new StringReader(s))
    inside(r.readLong()) {
      case Some(x) => x should be (-12345678901L) 
    }
    inside(r.readLong()) {
      case Some(x) => x should be (56789012345L) 
    }
  }

  it should "complain on the incorrect number" in {
    val s = "abcdef"
    val r = new NumberReader(new StringReader(s))
    r.readLong() should be (None)
  }
}
