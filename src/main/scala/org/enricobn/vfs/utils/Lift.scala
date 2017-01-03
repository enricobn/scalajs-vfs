package org.enricobn.vfs.utils

/**
  * Created by enrico on 1/3/17.
  */
object Lift {

  def lift[L,R](list: Iterable[Either[L, R]]) : Either[L, List[R]] =
    list.foldRight(Right(List.empty[R]) : Either[L, List[R]]) {(value,result) => {
      result match {
        case Left(l) => result
        case Right(r) =>
          value match {
            case Left(l) => Left(l)
            case Right(r1) => Right(r1 :: r)
          }
      }
    }}

  def lift[T](list: Iterable[Option[T]]) : Option[List[T]] = {
    list.foldRight(Some(List.empty) : Option[List[T]])((value, result) => {
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
}
