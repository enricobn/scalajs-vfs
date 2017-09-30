package org.enricobn.vfs

import org.enricobn.vfs.inmemory.InMemoryFS
import org.scalamock.scalatest.MockFactory
import org.scalatest.{FlatSpec, Matchers}

import scala.language.reflectiveCalls

class VirtualPathSpec extends FlatSpec with MockFactory with Matchers {

  private def fixture = {
    val vum = stub[VirtualUsersManager]
    (vum.checkWriteAccess _).when(*).returns(true)
    (vum.checkExecuteAccess _).when(*).returns(true)
    (vum.checkReadAccess _).when(*).returns(true)

    val f = new {
      val usersManager: VirtualUsersManager = vum
      val fs = new InMemoryFS(usersManager)
      val usr : VirtualFolder = fs.root.mkdir("usr").right.get
      val bin : VirtualFolder = usr.mkdir("bin").right.get
      val rootFile : VirtualFile = fs.root.touch("rootFile").right.get
      val usrFile : VirtualFile = usr.touch("usrFile").right.get
      val binFile : VirtualFile = bin.touch("binFile").right.get
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

    assert(usr.path == "/usr")
  }

  "toFolder of root" should "be root" in {
    val f = fixture

    val sut = VirtualPath("/")

    val folder = sut.toFolder(f.fs.root)

    assert(folder.right.get == f.fs.root)
  }

  "toFolder of absolute path" should "work" in {
    val f = fixture

    val sut = VirtualPath("/usr/bin")

    val folder = sut.toFolder(f.fs.root)

    assert(folder.right.get == f.bin)
  }

  "toFolder of relative path" should "work" in {
    val f = fixture

    val sut = VirtualPath("bin")

    val folder = sut.toFolder(f.usr)

    assert(folder.right.get == f.bin)
  }

  "toFolder of parent path" should "work" in {
    val f = fixture

    val sut = VirtualPath("../bin")

    val folder = sut.toFolder(f.bin)

    assert(folder.right.get == f.bin)
  }

  "toFolder of self" should "work" in {
    val f = fixture

    val sut = VirtualPath("./bin")

    val folder = sut.toFolder(f.usr)

    assert(folder.right.get == f.bin)
  }

  "findFolder of not existent folder" should "return Right(None)" in {
    val f = fixture

    val sut = VirtualPath("home/enrico")

    val folder = sut.findFolder(f.fs.root)

    assert(folder.right.get.isEmpty)
  }

  "toFolder of absolute path" should "work from fragments" in {
    val f = fixture

    val sut = VirtualPath(List(RootFragment(), SimpleFragment("usr"), SimpleFragment("bin")))

    val folder = sut.toFolder(f.fs.root)

    assert(folder.right.get == f.bin)
  }

  "toFile of absolute path" should "work" in {
    val f = fixture

    val sut = VirtualPath("/usr/bin/binFile")

    val file = sut.toFile(f.fs.root)

    assert(f.binFile == file.right.get)
  }

  "toFile of root file" should "work" in {
    val f = fixture

    val sut = VirtualPath("/rootFile")

    val file = sut.toFile(f.fs.root)

    assert(f.rootFile == file.right.get)
  }

  "toFile of relative path" should "work" in {
    val f = fixture

    val sut = VirtualPath("../usrFile")

    val file = sut.toFile(f.bin)

    assert(f.usrFile == file.right.get)
  }

  "findFile of parent of root" should "return Right(None)" in {
    val f = fixture

    val sut = VirtualPath("..")

    val file = sut.findFile(f.fs.root)

    assert(Right(None) == file)
  }


  "findFile of parent of parent of root" should "return Right(None)" in {
    val f = fixture

    val sut = VirtualPath("../..")

    val file = sut.findFile(f.fs.root)

    assert(Right(None) == file)
  }

  "findFolder of ../.. of usr" should "return Right(None)" in {
    val f = fixture

    val sut = VirtualPath("../..")

    val file = sut.findFolder(f.usr)

    assert(Right(None) == file)
  }

  "findFolder of ../.. of bin" should "return root" in {
    val f = fixture

    val sut = VirtualPath("../..")

    val file = sut.findFolder(f.bin)

    assert(Right(Some(f.fs.root)) == file)
  }

  "findFile of ../../usr/bin/binFile from bin" should "return binFile" in {
    val f = fixture

    val sut = VirtualPath("../../usr/bin/binFile")

    val file = sut.findFile(f.bin)

    assert(Right(Some(f.binFile)) == file)
  }

  "..." should "not throw an exception" in {
    val sut = VirtualPath("../.../")
  }

  "find of ../..." should "not work, but don't throw an Exception" in {
    val f = fixture

    val sut = VirtualPath("../.../")
    val file = sut.findFile(f.bin)

    assert(Right(None) == file)
  }

}
