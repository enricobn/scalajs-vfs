package org.enricobn.vfs.inmemory

import org.enricobn.vfs.{VirtualFS, VirtualUsersManager}
import org.scalamock.scalatest.MockFactory
import org.scalatest.{FlatSpec, Matchers}

import scala.language.reflectiveCalls

/**
  * Created by enrico on 12/3/16.
  */
class InMemoryFSSpec extends FlatSpec with MockFactory with Matchers {

  def fixture = {
    val vum = stub[VirtualUsersManager]
    (vum.checkWriteAccess _).when(*).returns(true)
    (vum.checkExecuteAccess _).when(*).returns(true)
    (vum.checkReadAccess _).when(*).returns(true)

    val f = new {
      val usersManager: VirtualUsersManager = vum
      val fs = new InMemoryFS(usersManager)
    }
    (f.usersManager.currentUser _).when().returns("foo")
    f
  }

  "Root name" should "be slash" in {
    val f = fixture

    assert(f.fs.root.name == "/")
  }

  "Mkdir" should "add a folder" in {
    val f = fixture

    val folderName = "foo"

    val pippo = f.fs.root.mkdir(folderName).right.get

    assert(pippo.name == folderName)
    assert(f.fs.root.folders.right.get.size == 1)
    assert(f.fs.root.folders.right.get.head.name == folderName)
  }

  "Mkdir" should "add a folder with current user as owner" in {
    val f = fixture

    val folderName = "foo"

    val pippo = f.fs.root.mkdir(folderName).right.get

    assert(pippo.owner == "foo")
  }

  "RootPath name" should "be slash" in {
    val f = fixture

    assert(VirtualFS.rootPath.path == "/")
  }

}
