package org.enricobn.vfs

import org.enricobn.vfs.inmemory.{InMemoryFS}
import org.scalamock.scalatest.MockFactory
import org.scalatest.{FlatSpec, Matchers}

import scala.language.reflectiveCalls

class VirtualPathSpec extends FlatSpec with MockFactory with Matchers {

  def fixture = {
    val vum = stub[VirtualUsersManager]
    (vum.checkWriteAccess _).when(*).returns(true)
    (vum.checkExecuteAccess _).when(*).returns(true)
    (vum.checkReadAccess _).when(*).returns(true)

    val f = new {
      val usersManager: VirtualUsersManager = vum
      val fs = new InMemoryFS(usersManager)
      val usr: VirtualFolder = fs.root.mkdir("usr").right.get
      val bin: VirtualFolder = usr.mkdir("bin").right.get
    }
    (f.usersManager.currentUser _).when().returns("foo")
    f
  }

  "Root path" should "be /" in {
    val sut = VirtualFS.rootPath

    assert(sut.path == "/")
  }

  "Simple absolute path" should "work" in {
    val sut = VirtualPath("/usr")

    assert(sut.path == "/usr")

    assert(sut.parent.get.path == "/")
  }

  "Absolute path" should "work" in {
    val sut = VirtualPath("/usr/bin")

    assert(sut.path == "/usr/bin")

    assert(sut.parent.get.path== "/usr")

    assert(sut.parent.get.parent.get.path == "/")
  }

  "child" should "work" in {
    val sut = VirtualPath("/usr")

    val bin = sut.child("bin")

    assert(bin.path == "/usr/bin")
  }

  "parent" should "work" in {
    val sut = VirtualPath("/usr")

    val bin = sut.child("bin")

    assert(bin.parent.get == sut)
  }

  "parent of root" should "be None" in {
    val sut = VirtualPath("/")

    assert(sut.parent.isEmpty)
  }

  "toFolder of root" should "be root" in {
    val f = fixture

    val sut = VirtualPath("/")

    val folder = sut.toFolder(f.fs, f.fs.root)

    assert(folder.right.get == f.fs.root)
  }

  "toFolder of absolute path" should "work" in {
    val f = fixture

    val sut = VirtualPath("/usr/bin")

    val folder = sut.toFolder(f.fs, f.fs.root)

    assert(folder.right.get.path == "/usr/bin")
  }

  "toFolder of relative path" should "work" in {
    val f = fixture

    val sut = VirtualPath("bin")

    val folder = sut.toFolder(f.fs, f.usr)

    assert(folder.right.get.path == "/usr/bin")
  }

  "toFolder of parent path" should "work" in {
    val f = fixture

    val sut = VirtualPath("../bin")

    val folder = sut.toFolder(f.fs, f.bin)

    assert(folder.right.get.path == "/usr/bin")
  }

  "toFolder of self" should "work" in {
    val f = fixture

    val sut = VirtualPath("./bin")

    val folder = sut.toFolder(f.fs, f.usr)

    assert(folder.right.get.path == "/usr/bin")
  }

  "findFolder of not existent folder" should "return Right(None)" in {
    val f = fixture

    val sut = VirtualPath("home/enrico")

    val folder = sut.findFolder(f.fs, f.fs.root)

    assert(folder.right.get.isEmpty)
  }

  "toFolder of absolute path" should "work from fragments" in {
    val f = fixture

    val sut = VirtualPath(List(RootFragment(), SimpleFragment("usr"), SimpleFragment("bin")))

    val folder = sut.toFolder(f.fs, f.fs.root)

    assert(folder.right.get.path == "/usr/bin")
  }


}
