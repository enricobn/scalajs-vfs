package org.enricobn.vfs.impl

import org.scalamock.scalatest.MockFactory
import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by enrico on 1/3/17.
  */
class VirtualAbsolutePathSpec extends FlatSpec with MockFactory with Matchers {

  "/" should "be root" in {
    val example = VirtualAbsolutePath("/")
    assert(example.isRoot)
  }

  "parent of /foo" should "be root" in {
    val example = VirtualAbsolutePath("/foo")
    assert(example.parent.get.isRoot)
  }

  "foo" should "throw an exception" in {
    val caught =
      intercept[IllegalArgumentException] {
        VirtualAbsolutePath("foo")
      }

    assert(caught.getMessage == "requirement failed: Not an absolute path.")
  }

  "parent of /foo/text.txt" should "be /foo" in {
    val example = VirtualAbsolutePath("/foo/text.txt")
    assert(example.parent.get == VirtualAbsolutePath("/foo"))
  }

  "parent of /" should "be None" in {
    val example = VirtualAbsolutePath("/")
    assert(example.parent.isEmpty)
  }

  "parent of /foo/" should "be /" in {
    val example = VirtualAbsolutePath("/foo/")
    assert(example.parent.get == VirtualAbsolutePath("/"))
  }

  "name of /" should "be /" in {
    val example = VirtualAbsolutePath("/")
    assert(example.name == "/")
  }

  "name of /foo" should "be foo" in {
    val example = VirtualAbsolutePath("/foo")
    assert(example.name == "foo")
  }

  "name of /foo/text.txt" should "be text.txt" in {
    val example = VirtualAbsolutePath("/foo/text.txt")
    assert(example.name == "text.txt")
  }

  "name of /foo/" should "be foo" in {
    val example = VirtualAbsolutePath("/foo/")
    assert(example.name == "foo")
  }

}
