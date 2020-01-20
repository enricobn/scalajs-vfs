package org.enricobn.vfs.impl

import java.util.UUID

import org.enricobn.vfs._
import org.enricobn.vfs.inmemory.InMemoryFS
import org.scalamock.scalatest.MockFactory
import org.scalatest.{FlatSpec, Matchers}

// to access members of structural types (new {}) without warnings
import scala.language.reflectiveCalls

/**
  * Created by enrico on 12/3/16.
  */
class VirtualUsersManagerImplSpec extends FlatSpec with MockFactory with Matchers {

  def fixture = {
    val _rootPassword = "rootPassword"
    val fs = InMemoryFS(_rootPassword).right.get

    // TODO extract a case class
    val f = new {
      val rootPassword: String = _rootPassword //UUID.randomUUID().toString
      val guestPassword: String = UUID.randomUUID().toString
      val usersManager: VirtualUsersManager = fs.vum
      val rootAuthentication: Authentication = usersManager.logRoot(rootPassword).right.get

      fs.root.mkdir("home")(rootAuthentication)

      usersManager.addUser("guest", guestPassword)(rootAuthentication).leftSide.foreach(e => fail(e.message))
    }
    f
  }

  "Login with a valid user" should "be fine" in {
    val f = fixture

    val logUser = f.usersManager.logUser("guest", f.guestPassword)
    assert(logUser.right.get.user == "guest")
  }

  "Login as user then root" should "be fine" in {
    val f = fixture

    var logUser = f.usersManager.logUser("guest", f.guestPassword)
    assert(logUser.right.get.user == "guest")

    logUser = f.usersManager.logRoot(f.rootPassword)
    assert(logUser.right.get.user == VirtualUsersManager.ROOT)
  }

  "Login with invalid password" should "throws an exception" in {
    val f = fixture

    checkIOError(f.usersManager.logUser("guest", "invalid"), "Invalid password.")

//      intercept[VirtualSecurityException] {
    //      f.usersManager.logUser("guest", "invalid")
    //    }
    //    assert(caught.getMessage == "Invalid password.")
  }

  "Login root with invalid password" should "throws an exception" in {
    val f = fixture

    f.usersManager.logUser("guest", f.guestPassword)

    checkIOError(f.usersManager.logRoot("invalid"), "Invalid password.")
  }

  "Login root with null password" should "throws an exception" in {
    val f = fixture

    f.usersManager.logUser("guest", f.guestPassword)

    checkIOError(f.usersManager.logRoot(null), "Invalid password.")
  }

  "Login guest with null password" should "throws an exception" in {
    val f = fixture

    checkIOError(f.usersManager.logUser("guest", null), "Invalid password.")
  }

  "Login with invalid user" should "throws an exception" in {
    val f = fixture

    checkIOError(f.usersManager.logUser("invalid", "pwd"), "Invalid user.")
  }

  "Adding an already added user" should "throws an exception" in {
    val f = fixture

    checkIOError(f.usersManager.addUser("guest", f.guestPassword)(f.rootAuthentication), "User already added.")
  }

  "Adding an already added user with invalid password" should "throws an exception" in {
    val f = fixture

    checkIOError(f.usersManager.addUser("guest", "invalid")(f.rootAuthentication), "User already added.")
  }

  "Adding root" should "throws an exception" in {
    val f = fixture

    checkIOError(f.usersManager.addUser("root", f.rootPassword)(f.rootAuthentication), "User already added.")
  }

  "Adding root with invalid password" should "throws an exception" in {
    val f = fixture

    checkIOError(f.usersManager.addUser("root", "invalid")(f.rootAuthentication), "User already added.")
  }

  "Adding user from another user" should "throws an exception" in {
    val f = fixture

    val authentication = f.usersManager.logUser("guest", f.guestPassword).right.get

    checkIOError(f.usersManager.addUser("brian", "brian")(authentication), "Only root can add users.")
  }

  private def checkIOError(result: Either[IOError, Authentication], message: String) =
    assert(result.left.exists(e => e.message == message))

  private def checkIOError(result: Option[IOError], message: String) =
    assert(result.exists(e => e.message == message))


  private def createNode(owner: String, name: String): VirtualNode = {
    val node: VirtualNode = stub[VirtualNode]
    (node.owner _).when().returns(owner)
    (node.name _).when().returns(name)
    val permissions: VirtualPermissions = stub[VirtualPermissions]
    (node.permissions _).when().returns(permissions)
    node
  }

}
