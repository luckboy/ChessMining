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
 * You should have received a copy of the GNU Lesser General Public
 * License and the GNU General Public License along with this library.
 * If not, see <http://www.gnu.org/licenses/>.
 */
package pl.luckboy.chessmining.chess.spec
import org.scalatest.flatspec._
import org.scalatest.matchers._
import pl.luckboy.chessmining.chess._
import Squares._

class PackageSpec extends AnyFlatSpec with should.Matchers
{
  "A foldPawnCaptureSquares" should "fold the squares for the white side" in {
    val (sum, squares) = foldPawnCaptureSquares(Side.White, F4, (0, Vector[Int]())) {
      case ((sum, squares), squ) => (sum + 1, squares :+ squ)
    }
    sum should be (2)
    squares should have length (2)
    squares should contain (E5)
    squares should contain (G5)
  }

  it should "fold the squares for the black side" in {
    val (sum, squares) = foldPawnCaptureSquares(Side.Black, C4, (0, Vector[Int]())) {
      case ((sum, squares), squ) => (sum + 1, squares :+ squ)
    }
    sum should be (2)
    squares should have length (2)
    squares should contain (B3)
    squares should contain (D3)
  }

  "A foldPawnSquares" should "fold the squares for the white side and start square" in {
    val (sum, squares) = foldPawnSquares(Side.White, E2, (0, Vector[Int]())) {
      case ((sum, squares), squ) => ((sum + 1, squares :+ squ), true)
    }
    sum should be (2)
    squares should have length (2)
    squares should contain (E3)
    squares should contain (E4)
  }

  it should "fold the squares for the black side and start square" in {
    val (sum, squares) = foldPawnSquares(Side.Black, D7, (0, Vector[Int]())) {
      case ((sum, squares), squ) => ((sum + 1, squares :+ squ), true)
    }
    sum should be (2)
    squares should have length (2)
    squares should contain (D6)
    squares should contain (D5)
  }

  it should "fold the squares for the white side and non-start square" in {
    val (sum, squares) = foldPawnSquares(Side.White, F3, (0, Vector[Int]())) {
      case ((sum, squares), squ) => ((sum + 1, squares :+ squ), true)
    }
    sum should be (1)
    squares should have length (1)
    squares should contain (F4)
  }

  it should "fold the squares for the black side and non-start square" in {
    val (sum, squares) = foldPawnSquares(Side.Black, C5, (0, Vector[Int]())) {
      case ((sum, squares), squ) => ((sum + 1, squares :+ squ), true)
    }
    sum should be (1)
    squares should have length (1)
    squares should contain (C4)
  }

  "A foldKnightSquares" should "fold the squares" in {
    val (sum, squares) = foldKnightSquares(G4, (0, Vector[Int]())) {
      case ((sum, squares), squ) => (sum + 1, squares :+ squ)
    }
    sum should be (6)
    squares should have length (6)
    squares should contain (F6)
    squares should contain (H6)
    squares should contain (E5)
    squares should contain (E3)
    squares should contain (F2)
    squares should contain (H2)
  }

  "A foldBishopSlides" should "fold the slides" in {
    val (sum, squares) = foldBishopSlides(B3, (0, Vector[Int]())) {
      case ((sum, squares)) => (sum + 100, squares)
    } {
      case ((sum, squares), squ) => ((sum + 1, squares :+ squ), squ != E6)
    }
    sum should be (407)
    squares should have length (7)
    squares should contain (A4)
    squares should contain (C4)
    squares should contain (D5)
    squares should contain (E6)
    squares should contain (A2)
    squares should contain (C2)
    squares should contain (D1)
  }

  "A foldRookSlides" should "fold the slides" in {
    val (sum, squares) = foldRookSlides(C3, (0, Vector[Int]())) {
      case ((sum, squares)) => (sum + 100, squares)
    } {
      case ((sum, squares), squ) => ((sum + 1, squares :+ squ), squ != C6 && squ != F3)
    }
    sum should be (410)
    squares should have length (10)
    squares should contain (C4)
    squares should contain (C5)
    squares should contain (C6)
    squares should contain (B3)
    squares should contain (A3)
    squares should contain (D3)
    squares should contain (E3)
    squares should contain (F3)
    squares should contain (C2)
    squares should contain (C1)    
  }

  "A foldQueenSlides" should "fold the slides" in {
    val (sum, squares) = foldQueenSlides(G3, (0, Vector[Int]())) {
      case ((sum, squares)) => (sum + 100, squares)
    } {
      case ((sum, squares), squ) => ((sum + 1, squares :+ squ), squ != G6 && squ != E5 && squ != D3)
    }
    sum should be (815)
    squares should have length (15)
    squares should contain (F4)
    squares should contain (E5)
    squares should contain (G4)
    squares should contain (G5)
    squares should contain (G6)
    squares should contain (H4)
    squares should contain (F3)
    squares should contain (E3)
    squares should contain (D3)
    squares should contain (H3)
    squares should contain (F2)
    squares should contain (E1)
    squares should contain (G2)
    squares should contain (G1)
    squares should contain (H2)
  }

  "A foldKingSquares" should "fold the squares" in {
    val (sum, squares) = foldKingSquares(E1, (0, Vector[Int]())) {
      case ((sum, squares), squ) => (sum + 1, squares :+ squ)
    }
    sum should be (5)
    squares should have length (5)
    squares should contain (D2)
    squares should contain (E2)
    squares should contain (F2)
    squares should contain (D1)
    squares should contain (F1)
  }
}
