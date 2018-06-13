package org.enricobn.vfs

import org.scalamock.scalatest.MockFactory
import org.scalatest.{FlatSpec, Matchers}

import scala.language.reflectiveCalls

class VirtualPathSpec extends FlatSpec with MockFactory with Matchers {

  "Root path" should "be /" in {
    val sut = VirtualFS.rootPath

    assert(sut.path == "/")
  }

  "Simple absolute path" should "work" in {
    val sut = VirtualPath("/usr")

    assert(sut.path == "/usr")
  }

  "Absolute path" should "work" in {
    val sut = VirtualPath("/usr/bin")

    assert(sut.path == "/usr/bin")
  }

  "andThen with fragment" should "work" in {
    val sut = VirtualPath("/usr")

    val bin = sut.andThen(SimpleFragment("bin"))

    assert(bin.path == "/usr/bin")
  }

  "andThen with path as String" should "work" in {
    val sut = VirtualPath("/usr")

    val bin = sut.andThen("bin")

    assert(bin.path == "/usr/bin")
  }

  "andThen with path as VirtualPath" should "work" in {
    val sut = VirtualPath("/usr")

    val bin = sut.andThen(VirtualPath("bin"))

    assert(bin.path == "/usr/bin")
  }

  "toParentFolder" should "work" in {
    val sut = VirtualPath("/usr/bin")

    val usr = sut.toParentFolder

    assert(usr.path == "/usr/bin/..")
  }

  "absolute path" should "work from fragments" in {
    val sut = VirtualPath(List(RootFragment(), SimpleFragment("usr"), SimpleFragment("bin")))

    assert(sut.path == "/usr/bin")
  }

  "..." should "not throw an exception" in {
    val sut = VirtualPath("../.../")
  }

}
