package miguel.training.cavalier

import cats.effect.IO
import cats.effect.IOApp

object Main extends IOApp {

  implicit val chessBoard = ChessBoard.cavalier(11, 11)

  val state =
    ChessBoardState[BoardPosition](from = BoardPosition(0, 0), to = BoardPosition(10, 10))

  def printState(s: ChessBoardState[BoardPosition]): IO[Unit] = IO {
    println(s.moves.reverse.mkString(" -> "))
  }

  def run(args: List[String]): IO[cats.effect.ExitCode] =
    Path
      .bfs[BoardPosition](state)
      .find( b => b.position == b.destination )
      .evalTap(s => IO(println(s"Hops: ${s.moves.size-1}")))
      .evalTap(printState)
      .evalTap(_ => IO(println()))
      .compile.drain
      .as(cats.effect.ExitCode.Success)

    /*IO(
      Path
        .bfs_lazylist[BoardPosition](state)
        .find(b => b.position == b.destination)
    )
      .flatMap { b => b.fold(IO(println("No solution")))(printState) }
      .as(cats.effect.ExitCode.Success)*/

}
