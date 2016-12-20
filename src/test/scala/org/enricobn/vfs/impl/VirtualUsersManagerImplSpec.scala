package org.enricobn.vfs.impl

import java.util.UUID

import org.enricobn.vfs._
import org.scalamock.scalatest.MockFactory
import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by enrico on 12/3/16.
  */
class VirtualUsersManagerImplSpec extends FlatSpec with MockFactory with Matchers {

  def fixture = {
    val f = new {
      val rootPassword: String = UUID.randomUUID().toString
      val guestPassword: String = UUID.randomUUID().toString
      val usersManager: VirtualUsersManager = new VirtualUsersManagerImpl(rootPassword)
    }

    f.usersManager.addUser("guest", f.guestPassword)
    f
  }

  private def checkIOError(result: Option[IOError], message: String) =
    assert(result.exists(e => e.message == message))


  "UserManager" should "start as root" in {
    val f = fixture

    assert(VirtualUsersManager.ROOT == f.usersManager.currentUser)
  }

  "Login with a valid user" should "be fine" in {
    val f = fixture

    f.usersManager.logUser("guest", f.guestPassword)
    assert("guest" == f.usersManager.currentUser)
  }

   "Login as user then root" should "be fine" in {
      val f = fixture

      f.usersManager.logUser("guest", f.guestPassword)
      f.usersManager.logRoot(f.rootPassword)
      assert(VirtualUsersManager.ROOT == f.usersManager.currentUser)
    }

  "Login with invalid password" should "throws an exception" in {
    val f = fixture

    checkIOError(f.usersManager.logUser("guest", "invalid"), "Invalid password.")

    //    val caught =
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

    checkIOError(f.usersManager.addUser("guest", f.guestPassword), "User already added.")
  }

  "Adding an already added user with invalid password" should "throws an exception" in {
    val f = fixture

    checkIOError(f.usersManager.addUser("guest", "invalid"), "User already added.")
  }

  "Adding root" should "throws an exception" in {
    val f = fixture

    checkIOError(f.usersManager.addUser("root", f.rootPassword), "User already added.")
  }

  "Adding root with invalid password" should "throws an exception" in {
    val f = fixture

    checkIOError(f.usersManager.addUser("root", "invalid"), "User already added.")
  }

  "Adding user from another user" should "throws an exception" in {
    val f = fixture

    f.usersManager.logUser("guest", f.guestPassword)

    checkIOError(f.usersManager.addUser("brian", "brian"), "Only root can add users.")
  }

  "Check read access for root" should "be fine" in {
    val f = fixture

    val node = mock[VirtualNode]
    assert(f.usersManager.checkReadAccess(node))
  }

  "Check write access for root" should "be fine" in {
    val f = fixture

    val node = mock[VirtualNode]
    assert(f.usersManager.checkWriteAccess(node))
  }

  "Check execute access for root" should "be fine" in {
    val f = fixture

    val node = mock[VirtualNode]
    assert(f.usersManager.checkWriteAccess(node))
  }

  "Read access for owner" should "be fine" in {
    val f = fixture

    f.usersManager.logUser("guest", f.guestPassword)

    val node = createNode("guest", "text.txt")
    val permission = stub[VirtualPermission]

    (node.permissions.owner _).when().returns(permission)
    (permission.read _).when().returns(true)

    assert(f.usersManager.checkReadAccess(node))
  }

  "Read access for other" should "be fine" in {
    val f = fixture

    f.usersManager.logUser("guest", f.guestPassword)

    val node = createNode("john", "text.txt")
    val permission = stub[VirtualPermission]

    (node.permissions.others _).when().returns(permission)
    (permission.read _).when().returns(true)

    assert(f.usersManager.checkReadAccess(node))
  }

  "Read access for owner" should "not be granted" in {
    val f = fixture

    f.usersManager.logUser("guest", f.guestPassword)

    val node = createNode("guest", "text.txt")
    val permission = stub[VirtualPermission]

    (node.permissions.owner _).when().returns(permission)
    (permission.read _).when().returns(false)

    assert(!f.usersManager.checkReadAccess(node))
  }

  "Read access for others" should "not be granted" in {
    val f = fixture

    f.usersManager.logUser("guest", f.guestPassword)

    val node = createNode("john", "text.txt")
    val permission = stub[VirtualPermission]

    (node.permissions.others _).when().returns(permission)
    (permission.read _).when().returns(false)

    assert(!f.usersManager.checkReadAccess(node))
  }

  private def createNode(owner: String, name: String): VirtualNode = {
    val node: VirtualNode = stub[VirtualNode]
    (node.owner _).when().returns(owner)
    (node.name _).when().returns(name)
    val permissions: VirtualPermissions = stub[VirtualPermissions]
    (node.permissions _).when().returns(permissions)
    node
  }

}
