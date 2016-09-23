object State extends Enumeration {
  type State = Value
  val ALIVE, DEAD = Value
}
import State._

case class Cell(state: State = DEAD) {
  def this(p: State, c: State, n: State) =
    this(if (p == ALIVE && c == ALIVE && n == ALIVE) ALIVE else DEAD)

  def awake() = this.copy(state = ALIVE)

  def kill() = Cell(DEAD)
}

object Cell {
  def apply(p: State, c: State, n: State) = new Cell(p, c, n)

}

case class Board() {

  protected var lin: Int = 0
  protected var col: Int = 0
  protected var board: Vector[Vector[Cell]] = Vector.empty

  def this(linIn: Int, colIn: Int) = {
    this()
    lin = linIn
    col = colIn
    board = Vector.fill(lin, col)(Cell())
  }

  def this(list: List[List[Cell]]) = {
    this()
    lin = list.length
    col = if (lin > 0) list.head.length else 0
    board = list.toVector.map(_.toVector)
  }

  def toList() = {
    board.toList.map(_.toList)
  }

  def get(l: Int, c: Int): Option[Cell] = {
    if (l < 0 || l > lin-1 || c < 0 || c > col-1)
      None
    else
      Some(board(l)(c))
  }

  def next() = Board(BaseExample.next(this.toList))

}

object Board {
  def apply(linIn: Int, colIn: Int) = new Board(linIn, colIn)
  def apply(list: List[List[Cell]]) = new Board(list)
}

object BaseExample {

  def next(board: List[List[Cell]]): List[List[Cell]] = {

    if (board(0).length == 1)
      List(List(Cell(DEAD)))
    else
      List(
        (Cell(DEAD) +:
        (for (i <- 1 to board(0).length-2)
          yield Cell(board(0)(i-1).state, board(0)(i).state, board(0)(i+1).state)).toList) :+
        Cell(DEAD))
  }
}

