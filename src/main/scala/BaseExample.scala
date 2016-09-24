object State extends Enumeration {
  type State = Value
  val ALIVE, DEAD = Value
}
import State._

case class Cell(state: State = DEAD) {

  def awake = this.copy(state = ALIVE)

  def kill = Cell()

  def isAlive = state == ALIVE

  def next(n: Int) =
    if (isAlive)
      if (n == 2 || n ==3) this else kill
    else
      if (n != 3) this else awake
}

case class Board(protected val board: Vector[Vector[Cell]] = Vector.empty) {

  protected val lin: Int = board.size
  protected val col: Int = if (board.isEmpty) 0 else board(0).size
  
  def toList = {
    board.toList.map(_.toList)
  }

  def get(l: Int, c: Int): Option[Cell] = {
    def wrap(i: Int, max: Int) = {
      if ((i < 0 || i >= max) && max < 3)
        None
      else
        if (i < 0) Some(max-1) else if (i == max) Some(0) else Some(i)
    }

    val lWrap = wrap(l, lin)
    val cWrap = wrap(c, col)
    if (lWrap.isEmpty || cWrap.isEmpty)
      None
    else
      Some(board(lWrap.get)(cWrap.get))
  }

  def surrounding(l: Int, c: Int) = {
    (for (j <- l - 1 to l + 1; k <- c - 1 to c + 1 if (l != j || c != k) && get(j, k).isDefined)
      yield get(j, k).get).toList
  }

  def surroundingAlive(l: Int, c: Int) = {
    surrounding(l, c) count(_.isAlive)
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
  def apply(linIn: Int, colIn: Int) = new Board(Vector.fill(linIn, colIn)(Cell()))

  def apply(list: List[List[Cell]]) = new Board(list.toVector.map(_.toVector))
}

object BaseExample {

}

