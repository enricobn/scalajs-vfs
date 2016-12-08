package org.enricobn.vfs.inmemory

import org.enricobn.vfs.{VirtualFolder, VirtualUsersManager}
import org.scalamock.scalatest.MockFactory
import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by enrico on 12/5/16.
  */
class InMemoryFolderSpec extends FlatSpec with MockFactory with Matchers {
  val parent = stub[VirtualFolder]
  val fixture =
    new {
      val usersManager = stub[VirtualUsersManager]
      val folder = new InMemoryFolder(usersManager, parent, "foo")
    }

  "ResolveFolder" should "return a sub path" in {
    val f = fixture

    val usr: VirtualFolder = f.folder.mkdir("usr")

    assert(f.folder.resolveFolder("usr") == usr)
  }
}
