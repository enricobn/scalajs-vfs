package org.enricobn.vfs.inmemory

import org.enricobn.vfs.VirtualUsersManager
import org.scalamock.scalatest.MockFactory
import org.scalatest.{FlatSpec, Matchers}

import scala.language.reflectiveCalls

/**
  * Created by enrico on 12/5/16.
  */
class InMemoryFolderSpec extends FlatSpec with MockFactory with Matchers {

  private def fixture = {
    val vum = stub[VirtualUsersManager]
    (vum.checkWriteAccess _).when(*).returns(true)
    (vum.checkExecuteAccess _).when(*).returns(true)
    (vum.checkReadAccess _).when(*).returns(true)

    val fs = new InMemoryFS(vum)

    new {
      val sut = new InMemoryFolder(vum, Some(fs.root), "foo")
      val root: InMemoryFolder = fs.root
    }
  }

  "resolveFolder" should "returns a sub path" in {
    val f = fixture

    val usr = f.sut.mkdir("usr").right.get

    assert(f.sut.resolveFolder("usr").right.get.get == usr)
  }

  "resolveFolder with a non existent path" should "returns an Right(None)" in {
    val f = fixture

    val folderE = f.sut.resolveFolder("usr")
    assert(folderE.right.get.isEmpty)
  }

  "resolveFolderOrError with a non existent path" should "returns a Left(IOError)" in {
    val f = fixture

    val folderE = f.sut.resolveFolderOrError("usr", "error")
    assert(folderE.left.get.message == "error")
  }

  "ResolveFolder with /" should "return an error" in {
    val f = fixture

    val folderE = f.sut.resolveFolder("/")
    assert(folderE.isLeft)
  }

  "ResolveFolder with .." should "returns the parent folder" in {
    val f = fixture

    val folderE = for {
      sub <- f.sut.mkdir("sub").right
      parent <- sub.resolveFolder("..").right
    } yield parent

    assert(folderE.right.get.get == f.sut)
  }

  "chmod" should "777" in {
    val f = fixture

    assert(!f.sut.permissions.others.write)

    f.sut.chmod(777)

    assert(f.sut.permissions.others.write)
  }

  "chmod" should "377" in {
    val f = fixture

    f.sut.chmod(377)

    assert(!f.sut.permissions.owner.read)
  }

  "chmod" should "477" in {
    val f = fixture

    f.sut.chmod(477)

    assert(f.sut.permissions.owner.read)
    assert(!f.sut.permissions.owner.write)
    assert(!f.sut.permissions.owner.execute)
  }

  "chmod" should "77" in {
    val f = fixture

    f.sut.chmod(77)

    assert(!f.sut.permissions.owner.read)
    assert(!f.sut.permissions.owner.write)
    assert(!f.sut.permissions.owner.execute)
  }

  "by default a folder" should "be executable" in {
    val f = fixture

    assert(f.sut.permissions.owner.execute)
    assert(f.sut.permissions.group.execute)
    assert(f.sut.permissions.others.execute)
  }

  "foo's parent path" should "be /" in {
    val f = fixture
    assert(f.sut.parent.get.path == "/")
  }

  "foo's path" should "be /foo" in {
    val f = fixture
    assert(f.sut.path == "/foo")
  }

  "/foo/usr path" should "be /foo/usr" in {
    val f = fixture
    val usr = f.sut.mkdir("usr").right.get
    assert(usr.path == "/foo/usr")
  }

  "/foo/usr/bin" should "be /foo/usr/bin" in {
    val f = fixture
    val usr = f.sut.mkdir("usr").right.get
    val bin = usr.mkdir("bin").right.get
    assert(bin.path == "/foo/usr/bin")
  }


}
