package org.enricobn.vfs.inmemory

import org.enricobn.vfs._
import org.enricobn.vfs.impl.VirtualFSNotifierImpl
import org.scalamock.scalatest.MockFactory
import org.scalatest.{FlatSpec, Matchers}

import scala.language.reflectiveCalls

/**
  * Created by enrico on 12/5/16.
  */
class InMemoryFolderSpec extends FlatSpec with MockFactory with Matchers {

  private def fixture = {
    val vum = stub[VirtualUsersManager]

    val vsm = stub[VirtualSecurityManager]

    (vsm.checkWriteAccess(_ : VirtualNode)(_ : Authentication)).when(*, *).returns(true)
    (vsm.checkExecuteAccess(_ : VirtualNode)(_: Authentication)).when(*, *).returns(true)
    (vsm.checkReadAccess(_: VirtualNode)(_: Authentication)).when(*, *).returns(true)
    (vum.getUser(_ : Authentication)).when(*).returns(Some(VirtualUsersManager.ROOT))
    (vum.getGroup(_ : Authentication)).when(*).returns(Some(VirtualUsersManager.ROOT))

    val fsINotify = new VirtualFSNotifierImpl()

    val _root = InMemoryFolder.root(vum, vsm, fsINotify)

    new {
      val authentication: Authentication = Authentication("", VirtualUsersManager.ROOT)
      val sut: VirtualFolder = _root.mkdir("foo")(authentication).right.get
      val root: VirtualFolder = _root//fs.root
    }
  }

  "resolveFolder" should "returns a sub path" in {
    val f = fixture

    val usr = f.sut.mkdir("usr")(f.authentication).right.get

    assert(f.sut.resolveFolder("usr")(f.authentication).right.get.get == usr)
  }

  "resolveFolder with a non existent path" should "returns an Right(None)" in {
    val f = fixture

    val folderE = f.sut.resolveFolder("usr")(f.authentication)
    assert(folderE.right.get.isEmpty)
  }

  "resolveFolderOrError with a non existent path" should "returns a Left(IOError)" in {
    val f = fixture

    val folderE = f.sut.resolveFolderOrError("usr")(f.authentication)
    assert(folderE.isLeft)
  }

  "ResolveFolder with /" should "return an error" in {
    val f = fixture

    val usr = f.sut.mkdir("usr")(f.authentication).right.get

    val folderE = f.sut.resolveFolder("/foo/usr")(f.authentication)
    assert(folderE.right.get.get.path == "/foo/usr")
  }

  "ResolveFolder with .." should "returns the parent folder" in {
    val f = fixture

    val folderE = for {
      sub <- f.sut.mkdir("sub")(f.authentication).right
      parent <- sub.resolveFolder("..")(f.authentication).right
    } yield parent

    assert(folderE.right.get.get == f.sut)
  }

  "chmod" should "777" in {
    val f = fixture

    assert(!f.sut.permissions.others.write)

    f.sut.chmod(777)(f.authentication)

    assert(f.sut.permissions.others.write)
  }

  "chmod" should "377" in {
    val f = fixture

    f.sut.chmod(377)(f.authentication)

    assert(!f.sut.permissions.owner.read)
  }

  "chmod" should "477" in {
    val f = fixture

    f.sut.chmod(477)(f.authentication)

    assert(f.sut.permissions.owner.read)
    assert(!f.sut.permissions.owner.write)
    assert(!f.sut.permissions.owner.execute)
  }

  "chmod" should "77" in {
    val f = fixture

    f.sut.chmod(77)(f.authentication)

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
    val usr = f.sut.mkdir("usr")(f.authentication).right.get
    assert(usr.path == "/foo/usr")
  }

  "/foo/usr/bin" should "be /foo/usr/bin" in {
    val f = fixture
    val usr = f.sut.mkdir("usr")(f.authentication).right.get
    val bin = usr.mkdir("bin")(f.authentication).right.get
    assert(bin.path == "/foo/usr/bin")
  }

  "touch" should "create a file" in {
    val f = fixture
    val usr = f.sut.mkdir("usr")(f.authentication).right.get
    val bin = usr.mkdir("bin")(f.authentication).right.get
    bin.touch("file")(f.authentication).right.get
    val file = bin.findFile("file")(f.authentication).right.get.get
    assert(file.path == "/foo/usr/bin/file")
  }

}
