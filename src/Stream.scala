package fpinscala.laziness

trait Stream[+A] {
  import Stream._
  def uncons: Option[(A, Stream[A])]
  def isEmpty: Boolean = uncons.isEmpty

  def toList: List[A] = {
    val lst = scala.collection.mutable.ListBuffer[A]()
    def helper(s: Stream[A]): List[A] = s.uncons match {
      case Some((h, t)) => {
        lst += h
        helper(t)
      }
      case None => lst.toList
    }
    helper(this)
  }

  def toListRecursive: List[A] = uncons match {
    case Some((h, t)) => h :: t.toListRecursive
    case None => List()
  }

  def take(n: Int): Stream[A] = uncons match {
    case Some((h, t)) if n > 0 => cons(h, take(n - 1))
    case _ => empty
  }

  def takeWhile(p: A => Boolean): Stream[A] = uncons match {
    case Some((h, t)) if p(h) => cons(h, t.takeWhile(p))
    case _ => empty
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B =
    uncons match {
      case Some((h, t)) => f(h, t.foldRight(z)(f))
      case None => z
    }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b)

  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a, b) => p(a) && b)

  def takeWhile2(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((el, acc) =>
      if (p(el)) cons(el, acc) else empty)

  def map[B](f: A => B): Stream[B] =
    foldRight(empty[B])((h, t) => cons(f(h), t))

  def filter[B](f: A => Boolean): Stream[A] =
    foldRight(empty[A])((el, acc) =>
      if (f(el)) cons(el, acc) else acc)

  def append[B >: A](s: Stream[B]): Stream[B] =
    foldRight(s)((h, t) => cons(h, t))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty[B])((h, t) => f(h) append t)

}

object Stream {
  val ones: Stream[Int] = constant(1)
  val onetwo: Stream[Int] = cons(1, cons(2, onetwo))
  val oneToFive: Stream[Int] =
    cons(1, cons(2, cons(3, cons(4, cons(5, oneToFive)))))

  def constant[A](a: A): Stream[A] = cons(a, constant(a))

  def from(n: Int): Stream[Int] = cons(n, from(n + 1))

  def fibs: Stream[Int] = {
    def helper(f0: Int, f1: Int): Stream[Int] =
      cons(f0, helper(f1, f0 + f1))
    helper(0, 1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case Some((a, s)) => cons(a, unfold(s)(f))
    case None => empty[A]
  }

  def fibs2: Stream[Int] = {
    cons(0, unfold((0, 1)){case (f0, f1) => Some((f1, (f1, f0 + f1)))})
  }

  def empty[A]: Stream[A] = new Stream[A] {
    def uncons = None
  }

  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] =
    new Stream[A] {
      lazy val uncons = Some((hd, tl))
    }

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))
}
