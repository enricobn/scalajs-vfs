package org.enricobn.vfs.impl

import org.enricobn.vfs.*
import org.enricobn.vfs.inmemory.InMemoryFS
import org.scalamock.matchers.Matchers
import org.scalamock.scalatest.MockFactory
import org.scalatest.flatspec.AnyFlatSpec

// to access members of structural types (new {}) without warnings
import scala.language.reflectiveCalls

/**
  * Created by enrico on 12/3/16.
  */
class VirtualUsersManagerImplSpec extends AnyFlatSpec with MockFactory with Matchers {

  private def fixture = {
    val _rootPassword = "rootPassword"
    val fs = InMemoryFS(_rootPassword).toOption.get

    // TODO extract a case class
    val f = new {
      val rootPassword: String = _rootPassword //UUID.randomUUID().toString
      val guestPassword: String = "guestPassword"
      val usersManager: VirtualUsersManager = fs.vum
      val rootAuthentication: Authentication = usersManager.logRoot(rootPassword).toOption.get

      fs.root.mkdir("home")(rootAuthentication)

      usersManager.addUser("guest", guestPassword, "guest")(rootAuthentication).left.foreach(e => fail(e.message))
    }
    f
  }

  "Login with a valid user" should "be fine" in {
    val f = fixture

    val logUser = f.usersManager.logUser("guest", f.guestPassword)
    assert(logUser.toOption.get.user == "guest")
  }

  "Login as user then root" should "be fine" in {
    val f = fixture

    var logUser = f.usersManager.logUser("guest", f.guestPassword)
    assert(logUser.toOption.get.user == "guest")

    logUser = f.usersManager.logRoot(f.rootPassword)
    assert(logUser.toOption.get.user == VirtualUsersManager.ROOT)
  }

  "Login with invalid password" should "throws an exception" in {
    val f = fixture

    checkIOErrorA(f.usersManager.logUser("guest", "invalid"), "Invalid password.")

//      intercept[VirtualSecurityException] {
    //      f.usersManager.logUser("guest", "invalid")
    //    }
    //    assert(caught.getMessage == "Invalid password.")
  }

  "Login root with invalid password" should "throws an exception" in {
    val f = fixture

    f.usersManager.logUser("guest", f.guestPassword)

    checkIOErrorA(f.usersManager.logRoot("invalid"), "Invalid password.")
  }

  "Login root with null password" should "throws an exception" in {
    val f = fixture

    f.usersManager.logUser("guest", f.guestPassword)

    checkIOErrorA(f.usersManager.logRoot(null), "Invalid password.")
  }

  "Login guest with null password" should "throws an exception" in {
    val f = fixture

    checkIOErrorA(f.usersManager.logUser("guest", null), "Invalid password.")
  }

  "Login with invalid user" should "throws an exception" in {
    val f = fixture

    checkIOErrorA(f.usersManager.logUser("invalid", "pwd"), "Invalid user.")
  }

  "Adding an already added user" should "throws an exception" in {
    val f = fixture

    checkIOError(f.usersManager.addUser("guest", f.guestPassword, "guest")(f.rootAuthentication), "User already added.")
  }

  "Adding an already added user with invalid password" should "throws an exception" in {
    val f = fixture

    checkIOError(f.usersManager.addUser("guest", "invalid", "guest")(f.rootAuthentication), "User already added.")
  }

  "Adding root" should "throws an exception" in {
    val f = fixture

    checkIOError(f.usersManager.addUser("root", f.rootPassword, "guest")(f.rootAuthentication), "User already added.")
  }

  "Adding root with invalid password" should "throws an exception" in {
    val f = fixture

    checkIOError(f.usersManager.addUser("root", "invalid", "guest")(f.rootAuthentication), "User already added.")
  }

  "Adding user from another user" should "throws an exception" in {
    val f = fixture

    val authentication = f.usersManager.logUser("guest", f.guestPassword).toOption.get

    checkIOError(f.usersManager.addUser("brian", "brian", "guest")(authentication), "Only root can add users.")
  }

  private def checkIOErrorA(result: Either[IOError, Authentication], message: String) =
    assert(result.left.exists(e => e.message == message))

  private def checkIOError(result: Either[IOError, Unit], message: String) =
    assert(result.left.exists(e => e.message == message))

  private def createNode(owner: String, name: String): VirtualNode = {
    val node: VirtualNode = stub[VirtualNode]
    (() => node.owner).when().returns(owner)
    (() => node.name).when().returns(name)
    val permissions: VirtualPermissions = stub[VirtualPermissions]
    (() => node.permissions).when().returns(permissions)
    node
  }

}
