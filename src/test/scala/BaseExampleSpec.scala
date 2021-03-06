import BaseExample._
import State._

import scala.util.Random

class BaseExampleSpec extends BaseSpec {

  "awake" should {
    "make a cell alive" in {
      val dead = Cell()

      val result = dead.awake

      result.isAlive shouldBe true
    }
  }

  "kill" should {
    "make a cell dead" in {
      val alive = Cell(ALIVE)

      val result = alive.kill

      result.isAlive shouldBe false
    }
  }

  "cell next" should {
    "kill alive cell" when {
      "there are no live neighbours" in {
        val cell = Cell(ALIVE)

        val result = cell.next(0)

        result.isAlive shouldBe false
      }

      "there is 1 live neighbour" in {
        val cell = Cell(ALIVE)

        val result = cell.next(1)

        result.isAlive shouldBe false
      }

      "there are more than 3 live neighbours" in {
        val cell = Cell(ALIVE)

        val result = cell.next(4)

        result.isAlive shouldBe false
      }
    }

    "keep cell alive" when {
      "there are 2 live neighbours" in {
        val cell = Cell(ALIVE)

        val result = cell.next(2)

        result.isAlive shouldBe true
      }

      "there are 3 live neighbours" in {
        val cell = Cell(ALIVE)

        val result = cell.next(3)

        result.isAlive shouldBe true
      }
    }

    "awake cell" when {
      "there are exactly 3 live neighbours" in {
        val cell = Cell()

        val result = cell.next(3)

        result.isAlive shouldBe true
      }
    }

    "keep cell dead" when {
      "there are 2 live neighbours" in {
        val cell = Cell()

        val result = cell.next(2)

        result.isAlive shouldBe false
      }

      "there are 4 live neighbours" in {
        val cell = Cell()

        val result = cell.next(4)

        result.isAlive shouldBe false
      }
    }


  }

  "get cell from board" should {
    "get None when then indexes are outside the board (no wrap)" in {
      val board = Board(List(List(Cell(ALIVE))))

      val result = board.get(1,0)

      result shouldBe None
    }

    "get a Cell when then indexes are inside the board" in {
      val board = Board(List(
        List(Cell(DEAD), Cell(DEAD), Cell(DEAD)),
        List(Cell(DEAD), Cell(ALIVE), Cell(DEAD)),
        List(Cell(DEAD), Cell(DEAD), Cell(DEAD))))

      val result = board.get(1,1)

      result shouldBe Some(Cell(ALIVE))
    }
  }


  "get the surroundings of the Cell" should {
    "return the complete 8 neighbors when the Cell is fully surrounded" in {
      val board = Board(List(
        List(Cell(DEAD), Cell(DEAD), Cell(DEAD)),
        List(Cell(DEAD), Cell(ALIVE), Cell(DEAD)),
        List(Cell(DEAD), Cell(DEAD), Cell(DEAD))))

      val result = board.surrounding(1,1)

      result shouldBe List.fill(8)(Cell())
    }

    "return the 8 neighbors when the Cell is on a border" in {
      val board = Board(List(
        List(Cell(DEAD), Cell(DEAD), Cell(DEAD)),
        List(Cell(DEAD), Cell(DEAD), Cell(ALIVE)),
        List(Cell(DEAD), Cell(DEAD), Cell(DEAD))))

      val result = board.surrounding(1,2)

      result shouldBe List.fill(8)(Cell())
    }

    "return the 8 neighbors when the Cell is on a corner" in {
      val board = Board(List(
        List(Cell(DEAD), Cell(DEAD), Cell(DEAD)),
        List(Cell(DEAD), Cell(DEAD), Cell(DEAD)),
        List(Cell(DEAD), Cell(DEAD), Cell(ALIVE))))

      val result = board.surrounding(2,2)

      result shouldBe List.fill(8)(Cell())
    }

  }


  "board next" should {
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
        Cell(ALIVE), Cell(ALIVE), Cell(DEAD),
        Cell(DEAD),
        Cell(DEAD), Cell(ALIVE), Cell(ALIVE)))
    }

    "keep cell with 3 live neighbours alive and awake the dead ones with 3 alive neighbours" in {
      val board = Board(List(
        List(Cell(DEAD), Cell(DEAD), Cell(DEAD)),
        List(Cell(DEAD), Cell(ALIVE), Cell(DEAD)),
        List(Cell(ALIVE), Cell(ALIVE), Cell(ALIVE))))

      val result = board.next()

      result.toList shouldBe List(
        List(Cell(DEAD), Cell(DEAD), Cell(DEAD)),
        List(Cell(DEAD), Cell(ALIVE), Cell(DEAD)),
        List(Cell(ALIVE), Cell(ALIVE), Cell(ALIVE)))
    }

    "wipe dead all the board on a T shape" in {
      val board = Board(List(
        List(Cell(ALIVE), Cell(ALIVE), Cell(ALIVE)),
        List(Cell(DEAD), Cell(ALIVE), Cell(DEAD)),
        List(Cell(DEAD), Cell(ALIVE), Cell(DEAD))))

      val result = board.next()

      result.toList shouldBe Board(3,3).toList
    }
  }

}

