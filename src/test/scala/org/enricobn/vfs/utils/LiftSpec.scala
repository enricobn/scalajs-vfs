package org.enricobn.vfs.utils

import org.scalamock.scalatest.MockFactory
import org.scalatest.{FlatSpec, Matchers}

import Lift._

/**
  * Created by enrico on 1/3/17.
  */
class LiftSpec extends FlatSpec with MockFactory with Matchers {

  "lifting Some" should "return Some of list" in {
    val example = List(Some("hello"))

    assert(lift(example).contains(List("hello")))
  }

  "lifting list of some" should "return Some of list" in {
    val example = List(Some("hello"), Some("world"))

    assert(lift(example).contains(List("hello", "world")))
  }

  "lifting None" should "return None" in {
    val example = List(None)

    assert(lift(example).isEmpty)
  }

  "lifting list of None" should "return None" in {
    val example = List(None, None)

    assert(lift(example).isEmpty)
  }

  "lifting an empty list" should "return Some of empty list" in {
    val example = List.empty

    assert(lift[Int](example).contains(List.empty))
  }

  "lifting Some and None" should "return None" in {
    val example = List(Some("hello"), None)

    assert(lift(example).isEmpty)
  }

  "lifting None and Some" should "return None" in {
    val example = List(None, Some("hello"))

    assert(lift(example).isEmpty)
  }

  "lifting left" should "return left" in {
    val example = List(Left("error"))

    assert(lift(example) == Left("error"))
  }

  "lifting right" should "return right" in {
    val example = List(Right(1), Right(2))

    assert(lift(example) == Right(List(1,2)))
  }

  "lifting left and right" should "return left" in {
    val example = List(Left("error"), Right(1))

    assert(lift(example) == Left("error"))
  }

  "lifting right and left" should "return left" in {
    val example = List(Right(1), Left("error"))

    assert(lift(example) == Left("error"))
  }

}
