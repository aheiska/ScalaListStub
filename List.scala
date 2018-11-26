
import scala.{:: => _, List => _, Nil => _}
import scala.annotation.tailrec

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]


object List {

  // for easy creation: List(1,2,3) produces a new list
  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def head[A](l: List[A]): Option[A] = l match {
    case Nil        => None
    case Cons(a, _) => Some(a)
  }

  def tail[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(_, tail) => tail
  }

  def prepend[A](a: A, l: List[A]): List[A] = Cons(a, l)

  def map[A, B](l: List[A])(f: A => B): List[B] = ???

  def filter[A](l: List[A])(pred: A => Boolean): List[A] = ???

  def fold[A, B](z: B)(l: List[A])(f: (A, B) => B): B = ???

}



