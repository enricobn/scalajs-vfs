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
    val rootPassword = "rootPassword"
    val _fs = InMemoryFS(rootPassword).right.get

    val _rootAuthentication: Authentication = _fs.vum.logRoot(rootPassword).right.get

    _fs.vum.addUser("foo", "fooPassword", "foo")(_rootAuthentication)

    val f = new {
      val fs: InMemoryFS = _fs
      val usersManager: VirtualUsersManager = fs.vum
      val fooAuthentication: Authentication = fs.vum.logUser("foo", "fooPassword").right.get
      val rootAuthentication: Authentication = _rootAuthentication
    }
    f
  }

  "Root name" should "be slash" in {
    val f = fixture

    assert(f.fs.root.name == "/")
  }

  "Mkdir as root" should "add a folder in /" in {
    val f = fixture

    val folderName = "foo"

    val result = f.fs.root.mkdir(folderName)(f.rootAuthentication).right.get

    assert(result.name == folderName)

    assert(f.fs.root.folders(f.rootAuthentication).right.get.exists(_.name == folderName))
  }

  "Mkdir as foo" should "not add a folder in /" in {
    val f = fixture

    val folderName = "fooFolder"

    val result = f.fs.root.mkdir(folderName)(f.fooAuthentication)

    assert(result.isLeft)

  }

  "Mkdir as foo" should "add a folder with current user as owner" in {
    val f = fixture

    val folderName = "foo"

    // I must own root to do it
    f.fs.root.chown("foo")(f.rootAuthentication)

    val result = f.fs.root.mkdir(folderName)(f.fooAuthentication).right.get

    assert(result.owner == "foo")
  }

}
