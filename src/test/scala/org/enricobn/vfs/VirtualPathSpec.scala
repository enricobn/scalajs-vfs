package org.enricobn.vfs

import org.enricobn.vfs.inmemory.InMemoryFS
import org.scalamock.scalatest.MockFactory
import org.scalatest.{FlatSpec, Matchers}

import scala.language.reflectiveCalls

class VirtualPathSpec extends FlatSpec with MockFactory with Matchers {

  private def fixture = {
    val vum = stub[VirtualUsersManager]
    val vsm = stub[VirtualSecurityManager]
    implicit val authentication: Authentication = Authentication("", "")
    (vsm.checkWriteAccess(_ : VirtualNode)(_ : Authentication)).when(*, *).returns(true)
    (vsm.checkExecuteAccess(_ : VirtualNode)(_: Authentication)).when(*, *).returns(true)
    (vsm.checkReadAccess(_: VirtualNode)(_: Authentication)).when(*, *).returns(true)
    (vum.getUser(_ : Authentication)).when(*).returns(Some(VirtualUsersManager.ROOT))

    val f = new {
      val usersManager: VirtualUsersManager = vum
      val fs = new InMemoryFS(usersManager, vsm)
      val usr : VirtualFolder = fs.root.mkdir("usr")(authentication).right.get
      val bin : VirtualFolder = usr.mkdir("bin")(authentication).right.get
      val rootFile : VirtualFile = fs.root.touch("rootFile")(authentication).right.get
      val usrFile : VirtualFile = usr.touch("usrFile")(authentication).right.get
      val binFile : VirtualFile = bin.touch("binFile")(authentication).right.get
    }
    f
  }

  "Root path" should "be /" in {
    val sut = VirtualFS.rootPath

    assert(sut.path == "/")
  }

  "Simple absolute path" should "work" in {
    val sut = VirtualPath("/usr")

    assert(sut.path == "/usr")
  }

  "Absolute path" should "work" in {
    val sut = VirtualPath("/usr/bin")

    assert(sut.path == "/usr/bin")
  }

  "andThen with fragment" should "work" in {
    val sut = VirtualPath("/usr")

    val bin = sut.andThen(SimpleFragment("bin"))

    assert(bin.path == "/usr/bin")
  }

  "andThen with path as String" should "work" in {
    val sut = VirtualPath("/usr")

    val bin = sut.andThen("bin")

    assert(bin.path == "/usr/bin")
  }

  "andThen with path as VirtualPath" should "work" in {
    val sut = VirtualPath("/usr")

    val bin = sut.andThen(VirtualPath("bin"))

    assert(bin.path == "/usr/bin")
  }

  "toParentFolder" should "work" in {
    val f = fixture

    val sut = VirtualPath("/usr/bin")

    val usr = sut.toParentFolder

    assert(usr.path == "/usr/bin/..")
  }

  "absolute path" should "work from fragments" in {
    val f = fixture

    val sut = VirtualPath(List(RootFragment(), SimpleFragment("usr"), SimpleFragment("bin")))

    assert(sut.path == "/usr/bin")
  }

  "..." should "not throw an exception" in {
    val sut = VirtualPath("../.../")
  }

}
