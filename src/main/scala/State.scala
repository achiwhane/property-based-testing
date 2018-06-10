// copied from here https://raw.githubusercontent.com/achiwhane/ScalaRNG/master/src/State.scala
// >inb4 DRY

case class State[S, +A](run: S => (A, S)) {
  def map[B >: A](f: A => B): State[S, B] =
    State(s => {
      val (nextAction, nextState) = run(s)
      (f(nextAction), nextState)
    })

  def map2[B, C](fb: State[S, B])(f: (A, B) => C): State[S, C] =
    State(s => {
      val (a, stateB) = run(s)
      val (b, stateC) = fb.run(stateB)

      (f(a, b), stateC)
    })

  def flatMap[B](f: A => State[S, B]): State[S, B] =
    State(s => {
      val (nextVal, nextState) = run(s)
      f(nextVal).run(nextState)
    })

}

object State {
  def unit[S, A](a: A): State[S, A] = State(s => (a, s))

  def get[S]: State[S, S] = State(s => (s, s))

  def set[S](s: S): State[S, Unit] = State(_ => ((), s))

  def modify[S](f: S => S): State[S, Unit] = get.flatMap(x => set(f(x)))

  def modifyWithForComprehensions[S](f: S => S): State[S, Unit] = for {
    s <- get
    _ <- set(f(s))
  } yield ()


  def sequence[S, A](fs: List[State[S, A]]): State[S, List[A]] = {
    def helper(ops: List[State[S, A]], currentState: S, acc: List[A]): (List[A], S) =
      if (ops.isEmpty) (acc.reverse, currentState)
      else ops.head.run(currentState) match {
        case (nextVal, nextState) => helper(ops.tail, nextState, nextVal :: acc)
      }

    State(s => helper(fs, s, List[A]()))
  }
}

