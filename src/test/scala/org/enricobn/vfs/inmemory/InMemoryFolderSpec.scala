package org.enricobn.vfs.inmemory

import org.enricobn.vfs.{VirtualFolder, VirtualUsersManager}
import org.scalamock.scalatest.MockFactory
import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by enrico on 12/5/16.
  */
class InMemoryFolderSpec extends FlatSpec with MockFactory with Matchers {
  def fixture = {
    val parent = stub[VirtualFolder]

    new {
      val usersManager = stub[VirtualUsersManager]
      val folder = new InMemoryFolder(usersManager, parent, "foo")
    }
  }

  "ResolveFolder" should "return a sub path" in {
    val f = fixture

    val usr: VirtualFolder = f.folder.mkdir("usr")

    assert(f.folder.resolveFolder("usr") == usr)
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
