package org.enricobn.vfs.impl

import org.enricobn.vfs._
import org.scalamock.scalatest.MockFactory
import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by enrico on 12/3/16.
  */
class VirtualSecurityManagerImplSpec extends FlatSpec with MockFactory with Matchers {
  private implicit val authentication = Authentication("", "")

  def fixture(): VirtualSecurityManager = fixture(VirtualUsersManager.ROOT)

  def fixture(user: String): VirtualSecurityManager = {
    val vum = stub[VirtualUsersManager]
    (vum.getUser(_ : Authentication)).when(*).returns(Some(user))

    new VirtualSecurityManagerImpl(vum)

  }

  private def checkIOError(result: Option[IOError], message: String) =
    assert(result.exists(e => e.message == message))

  "Check read access for root" should "be fine" in {
    val f = fixture()

    val node = mock[VirtualNode]
    assert(f.checkReadAccess(node))
  }

  "Check write access for root" should "be fine" in {
    val f = fixture()

    val node = mock[VirtualNode]
    assert(f.checkWriteAccess(node))
  }

  "Check execute access for root" should "be fine" in {
    val f = fixture()

    val node = mock[VirtualNode]
    assert(f.checkWriteAccess(node))
  }

  "Read access for owner" should "be fine" in {
    val f = fixture("guest")

    val node = createNode("guest", "text.txt")
    val permission = stub[VirtualPermission]

    (node.permissions.owner _).when().returns(permission)
    (permission.read _).when().returns(true)

    assert(f.checkReadAccess(node))
  }

  "Read access for other" should "be fine" in {
    val f = fixture("guest")

    val node = createNode("john", "text.txt")
    val permission = stub[VirtualPermission]

    (node.permissions.others _).when().returns(permission)
    (permission.read _).when().returns(true)

    assert(f.checkReadAccess(node))
  }

  "Read access for owner" should "not be granted" in {
    val f = fixture("guest")

    val node = createNode("guest", "text.txt")
    val permission = stub[VirtualPermission]

    (node.permissions.owner _).when().returns(permission)
    (permission.read _).when().returns(false)

    assert(!f.checkReadAccess(node))
  }

  "Read access for others" should "not be granted" in {
    val f = fixture("guest")

    val node = createNode("john", "text.txt")
    val permission = stub[VirtualPermission]

    (node.permissions.others _).when().returns(permission)
    (permission.read _).when().returns(false)

    assert(!f.checkReadAccess(node))
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
