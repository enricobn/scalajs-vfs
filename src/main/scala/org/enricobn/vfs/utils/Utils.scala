package org.enricobn.vfs.utils

import scala.collection.GenTraversableOnce

/**
  * Created by enrico on 1/3/17.
  */
object Utils {

  def lift[L,R](xs: GenTraversableOnce[Either[L, R]]) : Either[L, List[R]] =
    xs.foldLeft(Right(List.empty[R]) : Either[L, List[R]]) { (result, value) => {
      result match {
        case Left(_) => result
        case Right(r) =>
          value match {
            case Left(l) => Left(l)
            case Right(r1) => Right(r :+ r1)
          }
      }
    }}

  def lift[T](xs: GenTraversableOnce[Option[T]]) : Option[List[T]] = {
    // I use foldRight since v :: l is faster than l :+ v
    xs.foldRight(Some(List.empty) : Option[List[T]])((value, result) => {
      result match {
        case Some(l) =>
          value match {
            case Some(v) => Some(v :: l)
            case _ => None
          }
        case _ => None
      }
    })
  }

  def optionToLeft[T](o: Option[T]) : Either[T, Null] = o.map(Left(_)).getOrElse(Right(null))

  /**
    * given a traversable collection, applies a function from element to Option[E] "exiting" on first Some.
    * It's used when Option is used as an optional error.
    * @return None if all None otherwise the first Some
    */
  def mapFirstSome[T,E](xs: GenTraversableOnce[T], fun: T => Option[E]) : Option[E] =
    xs.foldLeft(Option.empty[E])((result, wh) => result match {
      case Some(error) => Some(error)
      case _ => fun(wh)
    })

  def liftTuple[T,TL,TR](xs: GenTraversableOnce[(T, Either[TL,TR])]) : Either[TL,List[(T,TR)]] =
    xs.foldRight(Right(List.empty[(T,TR)]) : Either[TL,List[(T,TR)]]) { (value, result) => {
      result match {
        case Left(_) => result
        case Right(r) =>
          value match {
            case (_,Left(l)) => Left(l)
            case (t,Right(r1)) => Right( (t,r1) :: r)
          }
      }
    }}

  def allSome[T,T1](xs: GenTraversableOnce[(T, Option[T1])]) : List[(T,T1)] =
    xs.foldRight(List.empty : List[(T,T1)])((value, result) => {
      value match {
        case (t, Some(v)) => (t,v) :: result
        case _ => result
      }
    })

  implicit class RightBiasedEither[A, B](val e: Either[A, B]) extends AnyVal {

    def foreach[U](f: B => U): Unit = e.right.foreach(f)

    def map[C](f: B => C): Either[A, C] = e.right.map(f)

    def flatMap[C](f: B => Either[A, C]): Either[A, C] = e.right.flatMap(f)

  }


}
