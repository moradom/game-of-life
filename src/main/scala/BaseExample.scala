object State extends Enumeration {
  type State = Value
  val ALIVE, DEAD = Value
}
import State._

case class Cell(state: State = DEAD) {
  def this(p: State, c: State, n: State) =
    this(if (p == ALIVE && c == ALIVE && n == ALIVE) ALIVE else DEAD)

  def awake = this.copy(state = ALIVE)

  def kill = Cell(DEAD)

  def next(n: Int) =
    if (state == ALIVE )
      if (n == 2 || n ==3) this else kill
    else
      if (n == 3) awake else this
}

object Cell {
  def apply(p: State, c: State, n: State) = new Cell(p, c, n)

}

case class Board(protected var board: Vector[Vector[Cell]] = Vector.empty) {

  protected var lin: Int = board.size
  protected var col: Int = if (board.isEmpty) 0 else board(0).size

  def this(linIn: Int, colIn: Int) = {
    this(Vector.fill(linIn, colIn)(Cell()))
  }

  def this(list: List[List[Cell]]) = {
    this(list.toVector.map(_.toVector))
  }

  def toList = {
    board.toList.map(_.toList)
  }

  def get(l: Int, c: Int): Option[Cell] = {
    if (l < 0 || l > lin-1 || c < 0 || c > col-1)
      None
    else
      Some(board(l)(c))
  }

  def surrounding(l: Int, c: Int) = {
    (for (j <- l - 1 to l + 1; k <- c - 1 to c + 1 if (l != j || c != k) && get(j, k).isDefined)
      yield get(j, k).get).toList
  }

  def surroundingAlive(l: Int, c: Int) = {
    surrounding(l, c) count(_.state == ALIVE)
  }

  def next() = this.copy(board = nextBoard())

  protected def nextBoard() = {
    (for (j <- 0 until lin)
      yield (for (k <- 0 until col)
        yield board(j)(k).next(surroundingAlive(j, k))).toVector)
              .toVector
  }


}

object Board {
  def apply(linIn: Int, colIn: Int) = new Board(linIn, colIn)
  def apply(list: List[List[Cell]]) = new Board(list)
}

object BaseExample {

}

