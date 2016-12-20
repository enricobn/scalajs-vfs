package org.enricobn.vfs

/**
  * Created by enrico on 12/17/16.
  */
case class IOError(message: String)

object IOError {

  type IOEff[A] = Either[IOError, () => A]

  implicit class SomeIOError(message: String) {
    def ioErrorO = Some(IOError(message))
  }

  implicit class EitherIOError(message: String) {
    def ioErrorE = Left(IOError(message))
  }

//  sealed abstract class VfsIO[A] {
//    def unsafePerformIO(): A
//
//    def flatMap[B](f: A => VfsIO[B]): VfsIO[B] = f(unsafePerformIO())
//
//    def map[B](f: A => B): VfsIO[B] =flatMap(a => VfsIO.pure(f(a)))
//  }
//
//  object VfsIO {
//    def apply[A](a: => A): VfsIO[A] = new VfsIO[A] { def unsafePerformIO(): A = a }
//
//    def pure[A](a: A): VfsIO[A] = new VfsIO[A] { def unsafePerformIO(): A = a }
//  }
}
