import BaseExample._
import State._

import scala.util.Random

class BaseExampleSpec extends BaseSpec {

  "awake" should {
    "make a cell alive" in {
      val dead = Cell(DEAD)

      val result = dead.awake()

      result.state shouldBe ALIVE
    }
  }

  "kill" should {
    "make a cell dead" in {
      val alive = Cell(ALIVE)

      val result = alive.kill()

      result.state shouldBe DEAD
    }
  }

  "get cell from board" should {
    "get None when then indexes are outside the board (no wrap)" in {
      val board = Board(List(List(Cell(ALIVE))))

      val result = board.get(1,0)

      result shouldBe None
    }
  }


  "next" should {
    "kill cells with fewer than two live neighbours" when {
      "it is the only live cell" in {
        val board = Board(List(List(Cell(ALIVE))))

        val result = board.next()

        result.toList shouldBe List(List(Cell(DEAD)))
      }

      "there is only 2 live cells" in {
        val board = Board(List(List(Cell(ALIVE), Cell(ALIVE))))

        val result = board.next()

        result.toList shouldBe List(List(Cell(DEAD), Cell(DEAD)))
      }

      "the live cell is between dead ones" in {
        val board = Board(List(List(Cell(DEAD), Cell(ALIVE), Cell(DEAD))))

        val result = board.next()

        result.toList shouldBe List(List(Cell(DEAD), Cell(DEAD), Cell(DEAD)))
      }
    }

    "keep cells with 2 live neighbours alive" in {
      val board = Board(List(
        List.fill(3)(Cell(ALIVE)) ++ List(Cell(DEAD)) ++ List.fill(3)(Cell(ALIVE))))

      val result = board.next()

      result.toList shouldBe List(List(
        Cell(DEAD), Cell(ALIVE), Cell(DEAD),
        Cell(DEAD),
        Cell(DEAD), Cell(ALIVE), Cell(DEAD)))
    }

    "keep cell with 3 live neighbours alive" in {
      pending
      val board = Board(List(
        List(Cell(DEAD), Cell(DEAD), Cell(DEAD)),
        List(Cell(DEAD), Cell(ALIVE), Cell(DEAD)),
        List(Cell(ALIVE), Cell(ALIVE), Cell(ALIVE))))

      val result = board.next()

      result.toList shouldBe List(
        List(Cell(DEAD), Cell(DEAD), Cell(DEAD)),
        List(Cell(DEAD), Cell(ALIVE), Cell(DEAD)),
        List(Cell(DEAD), Cell(ALIVE), Cell(DEAD)))
    }
  }

}

