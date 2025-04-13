package org.enricobn.vfs

import org.enricobn.vfs.inmemory.InMemoryFS
import org.scalamock.matchers.Matchers
import org.scalamock.scalatest.MockFactory
import org.scalatest.flatspec.AnyFlatSpec

import scala.language.reflectiveCalls

class VirtualPathSpec extends AnyFlatSpec with MockFactory with Matchers {

  private def fixture =
    (for {
      fs <- InMemoryFS("root")
      authentication <- fs.vum.logUser("root", "root")
      home <- fs.root.findFolder("home")(authentication)
      user <- home.get.mkdir("user")(authentication)
      _ <- user.touch("userFile")(authentication)
      _ <- fs.root.touch("rootFile")(authentication)
    } yield (user, authentication)).toOption.get

  "Simple absolute path" should "work" in {
    val sut = VirtualPath.absolute("usr").toOption.get

    assert(sut.toString == "/usr")
  }

  "Absolute path" should "work" in {
    val sut = VirtualPath.absolute("usr", "bin").toOption.get

    assert(sut.toString == "/usr/bin")
  }

  "andThen with fragment" should "work" in {
    val sut = VirtualPath.absolute("usr").toOption.get

    val bin = sut.andThen(SimpleFragment("bin"))

    assert(bin.toString == "/usr/bin")
  }

  "andThen with fragment as String" should "work" in {
    val sut = VirtualPath.absolute("usr").toOption.get

    val bin = sut.andThen("bin").toOption.get

    assert(bin.toString == "/usr/bin")
  }

  "andThen with path as VirtualPath" should "work" in {
    val sut = VirtualPath.absolute("usr").toOption.get

    val bin = sut.andThen(VirtualPath.relative("bin").toOption.get)

    assert(bin.toString == "/usr/bin")
  }

  "toParentFolder" should "work" in {
    val sut = VirtualPath.absolute("usr", "bin").toOption.get

    val usr = sut.parentOrError.toOption.get

    assert(usr.toString == "/usr")
  }

  "absolute path" should "work from path" in {
    val sut = VirtualPath.of("/usr/bin").toOption.get

    assert(sut.isInstanceOf[AbsolutePath])

    assert(sut.toString == "/usr/bin")
  }

  "relative path" should "work from path" in {
    val sut = VirtualPath.of("usr/bin").toOption.get

    assert(sut.isInstanceOf[RelativePath])

    assert(sut.toString == "usr/bin")
  }

  "..." should "not throw an exception" in {
    assert(VirtualPath.relative("..", "...").toOption.get.toString == "../...")
  }

  "toFile of userFile from /home/user" should "work" in {
    val (user, authentication) = fixture

    val result = VirtualPath.of("userFile").flatMap(_.toFile(user)(authentication))
      .fold({ e => fail(e.message) }, { file => file })

    assert(result.name == "userFile")
  }

  "toFile of rootFile from /" should "work" in {
    val (user, authentication) = fixture

    val result = VirtualPath.of("rootFile").flatMap(_.toFile(user.root)(authentication))
      .fold({ e => fail(e.message) }, { file => file })

    assert(result.name == "rootFile")
  }

  "toFolder of user from /home" should "work" in {
    val (user, authentication) = fixture

    val result = VirtualPath.of("user").flatMap(_.toFolder(user.parent.get)(authentication))
      .fold({ e => fail(e.message) }, { file => file })

    assert(result.name == "user")
  }

  "/" should "be /" in {
    assert("/" == VirtualPath.root.toString)
    assert("/" == VirtualPath.root.name)
  }

  "absolute userFile from user" should "be userFile" in {
    val (user, _) = fixture

    val userFilePath = VirtualPath.absolute(user, VirtualPath.relative("userFile").toOption.get).toOption.get

    assert("/home/user/userFile" == userFilePath.toString)

    assert(userFilePath.isInstanceOf[AbsolutePath])
  }

}
