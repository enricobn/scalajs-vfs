package prg.enricobn.vfs.inmemory

import org.enricobn.vfs.VirtualUsersManager
import org.enricobn.vfs.inmemory.InMemoryFS
import org.scalamock.scalatest.MockFactory
import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by enrico on 12/3/16.
  */
class InMemoryFSSpec extends FlatSpec with MockFactory with Matchers {

  def fixture =
    new {
      val usersManager = stub[VirtualUsersManager]
      val fs = new InMemoryFS(usersManager)
    }

  "Root name" should "be slash" in {
    val f = fixture

    assert(f.fs.root.getName == "/")
  }

  "Mkdir" should "add a folder" in {
    val f = fixture

    val folderName = "foo"

    val pippo = f.fs.root.mkdir(folderName)

    assert(pippo.getName == folderName)
    assert(f.fs.root.getFolders.size == 1)
    assert(f.fs.root.getFolders.head.getName == folderName)
  }
}
