package fpinscala.errorhandling

object ErrorHandling {
  def lift[A, B](f: A => B): Option[A] => Option[B] = _ map f
}
