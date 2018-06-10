case class Gen[A](sample: State[RNG, A]) {
  def flatMap[B](f: A => Gen[B]): Gen[B] =
    Gen(State[RNG, B]((rng: RNG) => {
      val (nextVal, nextRNG) = sample.run(rng)

      f(nextVal).sample.run(nextRNG)
    }))

  def listOfN(size: Gen[Int]): Gen[List[A]] = {
    def mapper(acc: List[A]): Gen[List[A]] =
      Gen(State[RNG, List[A]]((rng: RNG) => {
        val (nextVal, nextRNG) = sample.run(rng)
        (nextVal :: acc, nextRNG)
      }))

    def generateList(count: Int, currentGen: Gen[List[A]]): Gen[List[A]] =
      if (count <= 0)
        currentGen
      else
        generateList(count - 1, currentGen.flatMap(mapper))

    size.flatMap(sampleSize => generateList(sampleSize - 1, mapper(List[A]())))
  }
}

object Gen {
  def choose(start: Int, stopExclusive: Int): Gen[Int] = {
    val boundedRNGState = State[RNG, Int]((rng: RNG) => {
      def genNum(currentRNG: RNG): (Int, RNG) = {
        val (nextNum, nextRNG) = currentRNG.nextInt

        if (nextNum >= start && nextNum < stopExclusive)
          (nextNum, nextRNG)
        else
          genNum(nextRNG)

      }

      genNum(rng)
    })

    Gen(boundedRNGState)
  }

  def unit[A](a: => A): Gen[A] = Gen(State[RNG, A](RandCombinator.unit(a)))

  def boolean: Gen[Boolean] = Gen(State[RNG, Boolean]((rng: RNG) => {
    val (nextNum, nextRNG) = rng.nextInt
    (if (nextNum % 2 == 1) true else false, nextRNG)
  }))
}

