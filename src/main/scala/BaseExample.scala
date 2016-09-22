object State extends Enumeration {
  type State = Value
  val ALIVE, DEAD = Value
}
import State._

case class Cell(state: State) {
  def this(p: State, c: State, n: State) =
    this(if (p == ALIVE && c == ALIVE && n == ALIVE) ALIVE else DEAD)
}

object Cell {
  def apply(p: State, c: State, n: State) = new Cell(p, c, n)
}

object BaseExample {

  def awake(cell: Cell) = cell.copy(state = ALIVE)

  def kill(cell: Cell) = Cell(DEAD)

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

