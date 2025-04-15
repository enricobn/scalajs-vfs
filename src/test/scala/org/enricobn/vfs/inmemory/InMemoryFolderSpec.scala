package org.enricobn.vfs.inmemory

import org.enricobn.vfs.*
import org.enricobn.vfs.impl.VirtualFSNotifierImpl
import org.scalamock.matchers.Matchers
import org.scalamock.scalatest.MockFactory
import org.scalatest.flatspec.AnyFlatSpec

import scala.reflect.Selectable.reflectiveSelectable

/**
  * Created by enrico on 12/5/16.
  */
class InMemoryFolderSpec extends AnyFlatSpec with MockFactory with Matchers {

  private def fixture(user: String = VirtualUsersManager.ROOT, group : String = VirtualUsersManager.ROOT): Object {val authentication: Authentication; val sut: VirtualFolder; val root: VirtualFolder; val vum: VirtualUsersManager} = {
    val _vum = stub[VirtualUsersManager]

    val _vsm = stub[VirtualSecurityManager]

    (_vsm.checkWriteAccess(_ : VirtualNode)(_ : Authentication)).when(*, *).returns(true)
    (_vsm.checkExecuteAccess(_ : VirtualNode)(_: Authentication)).when(*, *).returns(true)
    (_vsm.checkReadAccess(_: VirtualNode)(_: Authentication)).when(*, *).returns(true)
    (_vum.getUser(_ : Authentication)).when(*).returns(Some(user))
    (_vum.getGroup(_ : Authentication)).when(*).returns(Some(group))
    (_vum.userExists(_ : String)).when(*).returns(true)

    val fsINotify = new VirtualFSNotifierImpl()

    val _root = InMemoryFolder.root(_vum, _vsm, fsINotify)

    new {
      val authentication: Authentication = Authentication("", VirtualUsersManager.ROOT)
      val sut: VirtualFolder = _root.mkdir("foo")(authentication).toOption.get
      val root: VirtualFolder = _root
      val vum: VirtualUsersManager = _vum
    }
  }
/*
  "resolveFolder" should "returns a sub path" in {
    val f = fixture()

    val usr = f.sut.mkdir("usr")(f.authentication).toOption.get

    assert(f.sut.resolveFolder("usr")(f.authentication).toOption.get.get == usr)
  }

  "resolveFolder with a non existent path" should "returns an Right(None)" in {
    val f = fixture()

    val folderE = f.sut.resolveFolder("usr")(f.authentication)
    assert(folderE.toOption.get.isEmpty)
  }

  "resolveFolderOrError with a non existent path" should "returns a Left(IOError)" in {
    val f = fixture()

    val folderE = f.sut.resolveFolderOrError("usr")(f.authentication)
    assert(folderE.isLeft)
  }

  "ResolveFolder with /" should "return an error" in {
    val f = fixture()

    f.sut.mkdir("usr")(f.authentication).toOption.get

    val folderE = f.sut.resolveFolder("/foo/usr")(f.authentication)
    assert(folderE.toOption.get.get.path == "/foo/usr")
  }

  "ResolveFolder with .." should "returns the parent folder" in {
    val f = fixture()

    val folderE = for {
      sub <- f.sut.mkdir("sub")(f.authentication)
      parent <- sub.resolveFolder("..")(f.authentication)
    } yield parent

    assert(folderE.toOption.get.get == f.sut)
  }

 */

  "chmod" should "777" in {
    val f = fixture()

    assert(!f.sut.permissions.others.write)

    f.sut.chmod(777)(f.authentication)

    assert(f.sut.permissions.others.write)
  }

  "chmod" should "377" in {
    val f = fixture()

    f.sut.chmod(377)(f.authentication)

    assert(!f.sut.permissions.owner.read)
  }

  "chmod" should "477" in {
    val f = fixture()

    f.sut.chmod(477)(f.authentication)

    assert(f.sut.permissions.owner.read)
    assert(!f.sut.permissions.owner.write)
    assert(!f.sut.permissions.owner.execute)
  }

  "chmod" should "77" in {
    val f = fixture()

    f.sut.chmod(77)(f.authentication)

    assert(!f.sut.permissions.owner.read)
    assert(!f.sut.permissions.owner.write)
    assert(!f.sut.permissions.owner.execute)
  }

  "by default a folder" should "be executable" in {
    val f = fixture()

    assert(f.sut.permissions.owner.execute)
    assert(f.sut.permissions.group.execute)
    assert(f.sut.permissions.others.execute)
  }

  "foo's parent path" should "be /" in {
    val f = fixture()

    assert(f.sut.parent.get.path == "/")
  }

  "foo's path" should "be /foo" in {
    val f = fixture()

    assert(f.sut.path == "/foo")
  }

  "/foo/usr path" should "be /foo/usr" in {
    val f = fixture()

    val usr = f.sut.mkdir("usr")(f.authentication).toOption.get

    assert(usr.path == "/foo/usr")
  }

  "/foo/usr/bin" should "be /foo/usr/bin" in {
    val f = fixture()

    val usr = f.sut.mkdir("usr")(f.authentication).toOption.get
    val bin = usr.mkdir("bin")(f.authentication).toOption.get

    assert(bin.path == "/foo/usr/bin")
  }

  "touch" should "create a file" in {
    val f = fixture()

    implicit val authentication: Authentication = f.authentication

    val usr = f.sut.mkdir("usr").toOption.get
    val bin = usr.mkdir("bin").toOption.get
    bin.touch("file").toOption.get
    val file = bin.findFile("file").toOption.get.get
    assert(file.path == "/foo/usr/bin/file")
  }

  "when the owner is not the authenticated user, but the group is the same then the permissions" should "be the group permissions" in {
    val f = fixture("user", "group")

    f.sut.chgrp("group")(f.authentication).toOption.get
    f.sut.chown("anotheruser")(f.authentication).toOption.get
    f.sut.chmod(707)(f.authentication).toOption.get

    val permissions = f.sut.getCurrentUserPermission(f.authentication)

    assert(!permissions.toOption.get.write)
    assert(!permissions.toOption.get.read)
    assert(!permissions.toOption.get.execute)
  }

  "when the owner is the authenticated user then the permissions" should "be the user permissions" in {
    val f = fixture("user", "group")

    f.sut.chgrp("group")(f.authentication).toOption.get
    f.sut.chown("user")(f.authentication).toOption.get
    f.sut.chmod(77)(f.authentication).toOption.get

    val permissions = f.sut.getCurrentUserPermission(f.authentication)

    assert(!permissions.toOption.get.write)
    assert(!permissions.toOption.get.read)
    assert(!permissions.toOption.get.execute)
  }

  "when the owner is not the authenticated user and is not the same group then the permissions" should "be the other permissions" in {
    val f = fixture("user", "group")

    f.sut.chgrp("anothergroup")(f.authentication).toOption.get
    f.sut.chown("anotheruser")(f.authentication).toOption.get
    f.sut.chmod(770)(f.authentication).toOption.get

    val permissions = f.sut.getCurrentUserPermission(f.authentication)

    assert(!permissions.toOption.get.write)
    assert(!permissions.toOption.get.read)
    assert(!permissions.toOption.get.execute)
  }

}
