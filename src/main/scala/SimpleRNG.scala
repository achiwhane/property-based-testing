import TypeWrapper.Rand

trait RNG {
  def nextInt: (Int, RNG)
}

// really bad but w/e
object TypeWrapper {
  type Rand[+A] = RNG => (A, RNG)
}

case class SimpleRNG(seed: Long) extends RNG {
  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt // >>> is right binary shift with zero-fill

    (n, nextRNG)
  }
}

object RandCombinator {
  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng => {
      val (a, rngA) = ra(rng)
      val (b, rngB) = rb(rngA)

      (f(a, b), rngB)
    }

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] =
    map2(ra, rb)((_, _))


  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = {
    def helper(fs: List[Rand[A]], res: List[A], currRNG: RNG): (List[A], RNG) =
      if (fs.isEmpty) (res.reverse, currRNG)
      else {
        val (output, nextRNG) = fs.head(currRNG)
        helper(fs.tail, output :: res, nextRNG)
      }

    rng => helper(fs, List[A](), rng)
  }

  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
    rng => {
      val (a, nextRNG) = f(rng)
      g(a)(nextRNG)
    }

  def mapInTermsOfFlatMap[A, B](s: Rand[A])(f: A => B): Rand[B] =
    flatMap(s){x => RandCombinator.unit(f(x))}


  def map2InTermsOfFlatMap[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra) { a =>
      flatMap(rb){ b =>
        RandCombinator.unit(f(a, b))
      }
    }



}

// TODO: use State to represent rand instead
object SimpleRNG {
  def int: Rand[Int] = _.nextInt

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    // generates numbers in the range [INT_MIN + 1, INT_MAX]
    def genNum(rng: RNG): (Int, RNG) =  {
      val nextVals = rng.nextInt
      if (nextVals._1 < 0) genNum(nextVals._2)

      nextVals
    }

    genNum(rng)
  }

  def nonNegativeLessThan(n: Int): Rand[Int] =
    RandCombinator.flatMap(nonNegativeInt) { i =>
      val mod = i % n
      if (i + (n - 1) - mod > 0)
        RandCombinator.unit(mod)
      else
        nonNegativeLessThan(n)
    }


  def nonNegativeEven: Rand[Int] =
    RandCombinator.map(nonNegativeInt)(i => i - i % 2)


  def double: Rand[Double] =
    RandCombinator.map(SimpleRNG.int)(i => i % Int.MaxValue / Int.MaxValue)

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i1, rng2) = rng.nextInt
    val (i2, rng3) = double(rng2)

    ((i1, i2), rng3)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val (i1, rng2) = double(rng)
    val (i2, rng3) = rng2.nextInt

    ((i1, i2), rng3)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (i1, rng2) = double(rng)
    val (i2, rng3) = double(rng2)
    val (i3, rng4) = double(rng3)

    ((i1, i2, i3), rng4)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    def helper(count: Int, res: List[Int])(currRNG: RNG): (List[Int], RNG) = {
      if (count == 0) (res, currRNG)
      else {
        val (nextNum, nextRNG) = currRNG.nextInt
        helper(count - 1, nextNum :: res)(nextRNG)
      }
    }

    helper(count, List[Int]())(rng)
  }

  def intsUsingCombinators(count: Int): Rand[List[Int]] =
    RandCombinator.sequence(List.fill(count)(int))

}

