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

    val caught =
      intercept[VirtualSecurityException] {
      f.usersManager.logUser("guest", "invalid")
    }
    assert(caught.getMessage == "Invalid password.")
  }

  "Login root with invalid password" should "throws an exception" in {
    val f = fixture

    f.usersManager.logUser("guest", f.guestPassword)

    val caught =
      intercept[VirtualSecurityException] {
      f.usersManager.logRoot("invalid")
    }
    assert(caught.getMessage == "Invalid password.")
  }

  "Login root with null password" should "throws an exception" in {
    val f = fixture

    f.usersManager.logUser("guest", f.guestPassword)

    val caught =
      intercept[VirtualSecurityException] {
        f.usersManager.logRoot(null)
      }
    assert(caught.getMessage == "Invalid password.")
  }

  "Login guest with null password" should "throws an exception" in {
    val f = fixture

    val caught =
      intercept[VirtualSecurityException] {
        f.usersManager.logUser("guest", null)
      }
    assert(caught.getMessage == "Invalid password.")
  }

  "Login with invalid user" should "throws an exception" in {
    val f = fixture

    val caught =
      intercept[VirtualSecurityException] {
        f.usersManager.logUser("invalid", "pwd")
      }
    assert(caught.getMessage == "Invalid user.")
  }

  "Adding an already added user" should "throws an exception" in {
    val f = fixture

    val caught =
      intercept[VirtualSecurityException] {
        f.usersManager.addUser("guest", f.guestPassword)
      }
    assert(caught.getMessage == "User already added.")
  }

  "Adding an already added user with invalid password" should "throws an exception" in {
    val f = fixture

    val caught =
      intercept[VirtualSecurityException] {
        f.usersManager.addUser("guest", "invalid")
      }
    assert(caught.getMessage == "User already added.")
  }

  "Adding root" should "throws an exception" in {
    val f = fixture

    val caught =
      intercept[VirtualSecurityException] {
        f.usersManager.addUser("root", f.rootPassword)
      }
    assert(caught.getMessage == "User already added.")
  }

  "Adding root with invalid password" should "throws an exception" in {
    val f = fixture

    val caught =
      intercept[VirtualSecurityException] {
        f.usersManager.addUser("root", "invalid")
      }
    assert(caught.getMessage == "User already added.")
  }

  "Adding user from another user" should "throws an exception" in {
    val f = fixture

    f.usersManager.logUser("guest", f.guestPassword)

    val caught =
      intercept[VirtualSecurityException] {
        f.usersManager.addUser("brian", "brian")
      }
    assert(caught.getMessage == "Only root can add users.")
  }

  "Check read access for root" should "be fine" in {
    val f = fixture

    val node = mock[VirtualNode]
    f.usersManager.checkReadAccess(node)
  }

  "Check write access for root" should "be fine" in {
    val f = fixture

    val node = mock[VirtualNode]
    f.usersManager.checkWriteAccess(node)
  }

  "Check execute access for root" should "be fine" in {
    val f = fixture

    val node = mock[VirtualNode]
    f.usersManager.checkWriteAccess(node)
  }

  "Read access for owner" should "be fine" in {
    val f = fixture

    f.usersManager.logUser("guest", f.guestPassword)

    val node = createNode("guest")
    val permission = stub[VirtualPermission]

    (node.permissions.owner _).when().returns(permission)
    (permission.read _).when().returns(true)

    f.usersManager.checkReadAccess(node)
  }

  "Read access for other" should "be fine" in {
    val f = fixture

    f.usersManager.logUser("guest", f.guestPassword)

    val node = createNode("john")
    val permission = stub[VirtualPermission]

    (node.permissions.others _).when().returns(permission)
    (permission.read _).when().returns(true)

    f.usersManager.checkReadAccess(node)
  }

  "Read access for owner" should "not be granted" in {
    val f = fixture

    f.usersManager.logUser("guest", f.guestPassword)

    val node = createNode("guest")
    val permission = stub[VirtualPermission]

    (node.permissions.owner _).when().returns(permission)
    (permission.read _).when().returns(false)

    val caught = intercept[VirtualSecurityException] {
      f.usersManager.checkReadAccess(node)
    }
    assert(caught.getMessage == "Permission denied.")
  }

  "Read access for others" should "not be granted" in {
    val f = fixture

    f.usersManager.logUser("guest", f.guestPassword)

    val node = createNode("john")
    val permission = stub[VirtualPermission]

    (node.permissions.others _).when().returns(permission)
    (permission.read _).when().returns(false)

    val caught = intercept[VirtualSecurityException] {
      f.usersManager.checkReadAccess(node)
    }
    assert(caught.getMessage == "Permission denied.")
  }

  private def createNode(owner: String): VirtualNode = {
    val node: VirtualNode = stub[VirtualNode]
    (node.owner _).when().returns(owner)
    val permissions: VirtualPermissions = stub[VirtualPermissions]
    (node.permissions _).when().returns(permissions)
    node
  }

}
