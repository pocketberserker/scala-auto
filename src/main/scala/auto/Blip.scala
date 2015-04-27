package auto

import scalaz._
import shapeless._

sealed abstract class Blip[A] {

  import Blip._

  def apply[B](default: => B)(f: A => B): B = this match {
    case InnerBlip(x) => f(x)
    case _ => default
  }
}

object Blip {

  case object NoBlip extends Blip[Nothing] {
    def apply[A]: Blip[A] = this.asInstanceOf[Blip[A]]
  }
  // TODO: rename
  final case class InnerBlip[A](a: A) extends Blip[A]

  def merge[A](fst: Blip[A], snd: Blip[A])(f: (A, A) => A): Blip[A] =
    mergeF(fst, snd)(identity)(identity)(f)

  def mergeF[A, B, C](fst: Blip[A], snd: Blip[B])(f: A => C)(g: B => C)(h: (A, B) => C): Blip[C] =
    (fst, snd) match {
      case (InnerBlip(x), NoBlip) => InnerBlip(f(x))
      case (NoBlip, InnerBlip(y)) => InnerBlip(g(y))
      case (InnerBlip(x), InnerBlip(y)) => InnerBlip(h(x, y))
      case (NoBlip, NoBlip) => NoBlip[C]
    }

  def mergeL[A](l: Blip[A], r: Blip[A]): Blip[A] = l match {
    case InnerBlip(_) => l
    case _ => r
  }

  def mergeR[A](l: Blip[A], r: Blip[A]): Blip[A] = r match {
    case InnerBlip(_) => r
    case _ => l
  }

  implicit def blipTypeable[A](implicit T: Typeable[A]): Typeable[Blip[A]] =
    Typeable.caseClassTypeable[Blip[A]](classOf[Blip[A]], Array(T))

  implicit def blipMonoid[A](implicit S: Semigroup[A]): Monoid[Blip[A]] =
    Monoid.instance({ case (b1, b2) => merge(b1, b2)({ case (x, y) => S.append(x, y) }) }, NoBlip[A])
}
