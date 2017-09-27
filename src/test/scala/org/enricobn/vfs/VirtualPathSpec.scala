package org.enricobn.vfs

import org.enricobn.vfs.inmemory.{InMemoryFS, InMemoryFolder}
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
    val f = fixture

    val sut = f.fs.rootPath

    assert(sut.path(f.fs) == "/")
  }

  "Simple absolute path" should "work" in {
    val f = fixture

    val sut = VirtualPath(f.fs, "/usr")

    assert(sut.path(f.fs) == "/usr")

    assert(sut.parent.get.path(f.fs) == "/")
  }

  "Absolute path" should "work" in {
    val f = fixture

    val sut = VirtualPath(f.fs, "/usr/bin")

    assert(sut.path(f.fs) == "/usr/bin")

    assert(sut.parent.get.path(f.fs)== "/usr")

    assert(sut.parent.get.parent.get.path(f.fs) == "/")
  }

  "child" should "work" in {
    val f = fixture

    val sut = VirtualPath(f.fs, "/usr")

    val bin = sut.child("bin")

    assert(bin.path(f.fs) == "/usr/bin")
  }

  "parent" should "work" in {
    val f = fixture

    val sut = VirtualPath(f.fs, "/usr")

    val bin = sut.child("bin")

    assert(bin.parent.get == sut)
  }

  "parent of root" should "be None" in {
    val f = fixture

    val sut = VirtualPath(f.fs, "/")

    assert(sut.parent.isEmpty)
  }

  "toFolder of root" should "be root" in {
    val f = fixture

    val sut = VirtualPath(f.fs, "/")

    val folder = sut.toFolder(f.fs, f.fs.root)

    assert(folder.right.get.get == f.fs.root)
  }


  "toFolder of absolute path" should "work" in {
    val f = fixture

    val sut = VirtualPath(f.fs, "/usr/bin")

    val folder = sut.toFolder(f.fs, f.fs.root)

    assert(folder.right.get.get.path == "/usr/bin")
  }

  "toFolder of relative path" should "work" in {
    val f = fixture

    val sut = VirtualPath(f.fs, "bin")

    val folder = sut.toFolder(f.fs, f.usr)

    assert(folder.right.get.get.path == "/usr/bin")
  }

  "toFolder of parent path" should "work" in {
    val f = fixture

    val sut = VirtualPath(f.fs, "../bin")

    val folder = sut.toFolder(f.fs, f.bin)

    assert(folder.right.get.get.path == "/usr/bin")
  }

  "toFolder of self" should "work" in {
    val f = fixture

    val sut = VirtualPath(f.fs, "./bin")

    val folder = sut.toFolder(f.fs, f.usr)

    assert(folder.right.get.get.path == "/usr/bin")
  }

}
