package annex

// tiny state monad

final case class State[S, A](run: S => (S, A)) {

  def map[B](f: A => B): State[S, B] =
    new State(s => {
      val res = run(s)
      (res._1, f(res._2))
    })

  def flatMap[B](f: A => State[S, B]): State[S, B] =
    new State(s => {
      val res = run(s)
      f(res._2).run(res._1)
    })

}

object State {
  def pure[S, A](a: A): State[S, A] = State(s => (s, a))
}
