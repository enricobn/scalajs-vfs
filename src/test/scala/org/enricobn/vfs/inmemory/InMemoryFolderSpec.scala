package org.enricobn.vfs.inmemory

import org.enricobn.vfs.{VirtualFolder, VirtualUsersManager}
import org.scalamock.scalatest.MockFactory
import org.scalatest.{FlatSpec, Matchers}

import scala.language.reflectiveCalls

/**
  * Created by enrico on 12/5/16.
  */
class InMemoryFolderSpec extends FlatSpec with MockFactory with Matchers {
  def fixture = {
    val rootFolder = stub[VirtualFolder]
    (rootFolder.path _).when().returns("/")

    val parent = stub[VirtualFolder]
    (parent.root _).when().returns(rootFolder)

    val vum = stub[VirtualUsersManager]
    (vum.checkWriteAccess _).when(*).returns(true)
    (vum.checkExecuteAccess _).when(*).returns(true)
    (vum.checkReadAccess _).when(*).returns(true)

    new {
      val usersManager = vum
      val folder = new InMemoryFolder(usersManager, parent, "foo")
      val root = rootFolder
    }
  }

  "ResolveFolder" should "return a sub path" in {
    val f = fixture

    val usr = f.folder.mkdir("usr").right.get

    assert(f.folder.resolveFolder("usr").right.get.get == usr)
  }

  "ResolveFolder with a non existent path" should "return an Right(None)" in {
    val f = fixture

    val folderE = f.folder.resolveFolder("usr")
    assert(folderE.right.get.isEmpty)
  }

  "resolveFolderOrError with a non existent path" should "return a Left(IOError)" in {
    val f = fixture

    val folderE = f.folder.resolveFolderOrError("usr", "error")
    assert(folderE.left.get.message == "error")
  }

  "ResolveFolder with /" should "return root" in {
    val f = fixture

    val folderE = f.folder.resolveFolder("/")
    assert(folderE.right.get.get === f.root)
  }

  "ResolveFolder with .." should "return the parent folder" in {
    val f = fixture

    val folderE = for {
      sub <- f.folder.mkdir("sub").right
      parent <- sub.resolveFolder("..").right
    } yield parent

    assert(folderE.right.get.get == f.folder)
  }

  "chmod" should "777" in {
    val f = fixture

    assert(!f.folder.permissions.others.write)

    f.folder.chmod(777)

    assert(f.folder.permissions.others.write)
  }

  "chmod" should "377" in {
    val f = fixture

    f.folder.chmod(377)

    assert(!f.folder.permissions.owner.read)
  }

  "chmod" should "477" in {
    val f = fixture

    f.folder.chmod(477)

    assert(f.folder.permissions.owner.read)
    assert(!f.folder.permissions.owner.write)
    assert(!f.folder.permissions.owner.execute)
  }

  "chmod" should "77" in {
    val f = fixture

    f.folder.chmod(77)

    assert(!f.folder.permissions.owner.read)
    assert(!f.folder.permissions.owner.write)
    assert(!f.folder.permissions.owner.execute)
  }

  "by default a folder" should "be executable" in {
    val f = fixture

    assert(f.folder.permissions.owner.execute)
    assert(f.folder.permissions.group.execute)
    assert(f.folder.permissions.others.execute)
  }
}
