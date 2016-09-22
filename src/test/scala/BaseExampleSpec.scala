import BaseExample._
import State._

import scala.util.Random

class BaseExampleSpec extends BaseSpec {

  "awake" should {
    "make a cell alive" in {
      val dead = Cell(DEAD)

      val result = awake(dead)

      result.state shouldBe ALIVE
    }
  }

  "kill" should {
    "make a cell dead" in {
      val alive = Cell(ALIVE)

      val result = kill(alive)

      result.state shouldBe DEAD
    }
  }

  "next" should {
    "kill cells with fewer than two live neighbours" when {
      "it is the only live cell" in {
        val board = List(List(Cell(ALIVE)))

        val result = next(board)

        result(0)(0).state shouldBe DEAD
      }

      "the live cell is between dead ones" in {
        val board = List(List(Cell(DEAD), Cell(ALIVE), Cell(DEAD)))

        val result = next(board)

        result shouldBe List(List(Cell(DEAD), Cell(DEAD), Cell(DEAD)))
      }
    }

    "keep cells with 2 live neighbours alive" in {
      val board = List(
        List.fill(3)(Cell(ALIVE)) ++ List(Cell(DEAD)) ++ List.fill(3)(Cell(ALIVE)))

      val result = next(board)

      result shouldBe List(List(
        Cell(DEAD), Cell(ALIVE), Cell(DEAD),
        Cell(DEAD),
        Cell(DEAD), Cell(ALIVE), Cell(DEAD)))
    }

    "keep cell with 3 live neighbours alive" in {
      pending
      val board = List(
        List(Cell(DEAD), Cell(DEAD), Cell(DEAD)),
        List(Cell(DEAD), Cell(ALIVE), Cell(DEAD)),
        List(Cell(ALIVE), Cell(ALIVE), Cell(ALIVE)))

      val result = next(board)

      result shouldBe List(
        List(Cell(DEAD), Cell(DEAD), Cell(DEAD)),
        List(Cell(DEAD), Cell(ALIVE), Cell(DEAD)),
        List(Cell(DEAD), Cell(ALIVE), Cell(DEAD)))
    }
  }

}

