package miguel.training.cavalier

import cats.Monad
import cats.implicits._
import cats.kernel.Eq
import javax.swing.text.Position
import cats.kernel.Monoid
import cats.effect.IO
import fs2.Stream

// algebra

final case class ChessBoardState[BoardPosition](
    val moves: List[BoardPosition],
    val position: BoardPosition,
    val destination: BoardPosition
)

object ChessBoardState {
  def apply[BoardPosition](
      from: BoardPosition,
      to: BoardPosition
  ): ChessBoardState[BoardPosition] =
    ChessBoardState(
      moves = List(from),
      position = from,
      destination = to
    )
}

trait ChessBoard[BoardPosition] {

  type Board = ChessBoardState[BoardPosition]

  def applyMove(s: Board, move: BoardPosition): Board
  def nextPositions(s: Board): List[BoardPosition]
}

// search
object Path {
  import fs2.Stream

  def bfs[Position](state: ChessBoardState[Position])(implicit
      board: ChessBoard[Position]
  ): Stream[IO, ChessBoardState[Position]] = {

    def discoverChildren(
        parents: Stream[IO, ChessBoardState[Position]],
        visited: Set[Position] = Set.empty
    ): Stream[IO, ChessBoardState[Position]] = {

      def children(parent: ChessBoardState[Position], ps: List[Position]): Stream[IO, ChessBoardState[Position]] = Stream
        .emits(ps)
        .filterNot(visited.contains)
        .map(board.applyMove(parent, _))

      val tail = parents.head.noneTerminate.flatMap {
        case None       => Stream.empty
        case Some(head) => 
            val nextPositions = board.nextPositions(head)  
            discoverChildren(parents.tail ++ children(head, nextPositions), visited ++ nextPositions)
      }
      parents.head ++ tail
    }

    Stream.emit(state) ++ discoverChildren(Stream.emit(state), visited = Set(state.position))

  }
}


case class BoardPosition(i: Int, j: Int)

case class Cavalier(position: BoardPosition) {
  lazy val destinations: List[BoardPosition] = List(
    BoardPosition(i = position.i + 2, j = position.j + 1),
    BoardPosition(i = position.i + 2, j = position.j - 1),
    BoardPosition(i = position.i - 2, j = position.j + 1),
    BoardPosition(i = position.i - 2, j = position.j - 1),
    BoardPosition(i = position.i + 1, j = position.j + 2),
    BoardPosition(i = position.i - 1, j = position.j + 2),
    BoardPosition(i = position.i + 1, j = position.j - 2),
    BoardPosition(i = position.i - 1, j = position.j - 2)
  )
}

object ChessBoard {

  def cavalier(n: Int, m: Int): ChessBoard[BoardPosition] =
    new ChessBoard[BoardPosition] {

      def applyMove(s: Board, move: BoardPosition): Board = {
        ChessBoardState(
          moves = move :: s.moves,
          position = move,
          destination = s.destination
        )
      }

      def nextPositions(s: Board): List[BoardPosition] = Cavalier(s.position).destinations
        .filter(p => p.i > 0 && p.i < n && p.j > 0 && p.j < m)

    }

}
