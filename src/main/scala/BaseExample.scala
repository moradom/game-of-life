object State extends Enumeration {
  type State = Value
  val ALIVE, DEAD = Value
}
import State._

object BaseExample {

  def awake(cell: Cell) = cell.copy(state = ALIVE)

  def kill(cell: Cell) = Cell(DEAD)

  def newState(p: State, c: State, n: State): State =
    if (p == ALIVE && c == ALIVE && n == ALIVE) ALIVE else DEAD

  def next(board: List[Cell]) = {
    def nextRec(previous: State, board: List[Cell]): List[Cell] = {
      board match {
        case h :: ht :: t => Cell(newState(previous, h.state, ht.state)) +: nextRec(h.state, ht :: t)
        case h :: Nil => List(Cell(DEAD))
        case _ => List.empty
      }
    }

    nextRec(DEAD, board)
//    if (board.length == 3 && board(0).state == ALIVE && board(1).state == ALIVE)
//      List(Cell(DEAD), Cell(ALIVE), Cell(DEAD))
//    else
//      for (c <- board) yield Cell(DEAD)
  }


}

case class Cell(state: State)
