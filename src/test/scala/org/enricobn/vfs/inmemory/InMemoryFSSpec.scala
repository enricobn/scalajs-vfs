package org.enricobn.vfs.inmemory

import org.enricobn.vfs._
import org.scalamock.scalatest.MockFactory
import org.scalatest.{FlatSpec, Matchers}

import scala.language.reflectiveCalls

/**
  * Created by enrico on 12/3/16.
  */
class InMemoryFSSpec extends FlatSpec with MockFactory with Matchers {

  private def fixture = {
    val vum = stub[VirtualUsersManager]
    val vsm = stub[VirtualSecurityManager]

    (vsm.checkWriteAccess(_ : VirtualNode)(_ : Authentication)).when(*, *).returns(true)
    (vsm.checkExecuteAccess(_ : VirtualNode)(_: Authentication)).when(*, *).returns(true)
    (vsm.checkReadAccess(_: VirtualNode)(_: Authentication)).when(*, *).returns(true)
    (vum.getUser(_ : Authentication)).when(*).returns(Some("foo"))


    val f = new {
      val usersManager: VirtualUsersManager = vum
      val fs = new InMemoryFS(vum, vsm)
      val authentication = Authentication("", "foo")
    }
    f
  }

  "Root name" should "be slash" in {
    val f = fixture

    assert(f.fs.root.name == "/")
  }

  "Mkdir" should "add a folder" in {
    val f = fixture

    val folderName = "foo"

    val pippo = f.fs.root.mkdir(folderName)(f.authentication).right.get

    assert(pippo.name == folderName)
    assert(f.fs.root.folders(f.authentication).right.get.size == 1)
    assert(f.fs.root.folders(f.authentication).right.get.head.name == folderName)
  }

  "Mkdir" should "add a folder with current user as owner" in {
    val f = fixture

    val folderName = "foo"

    val pippo = f.fs.root.mkdir(folderName)(f.authentication).right.get

    assert(pippo.owner == "foo")
  }

  "RootPath name" should "be slash" in {
    val f = fixture

    assert(VirtualFS.rootPath.path == "/")
  }

}
