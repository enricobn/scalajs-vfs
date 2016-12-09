package org.enricobn.vfs.inmemory

import org.enricobn.vfs.VirtualUsersManager
import org.scalamock.scalatest.MockFactory
import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by enrico on 12/3/16.
  */
class InMemoryFSSpec extends FlatSpec with MockFactory with Matchers {

  def fixture = {
    val f = new {
      val usersManager = stub[VirtualUsersManager]
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

    val pippo = f.fs.root.mkdir(folderName)

    assert(pippo.name == folderName)
    assert(f.fs.root.folders.size == 1)
    assert(f.fs.root.folders.head.name == folderName)
  }

  "Mkdir" should "add a folder with current user as owner" in {
    val f = fixture

    val folderName = "foo"

    val pippo = f.fs.root.mkdir(folderName)

    assert(pippo.owner == "foo")
  }

}
