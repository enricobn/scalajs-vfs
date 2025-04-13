package org.enricobn.vfs.impl

import org.enricobn.vfs.*
import org.scalamock.scalatest.MockFactory
import org.scalatest.flatspec.AnyFlatSpec

/**
  * Created by enrico on 12/3/16.
  */
class VirtualSecurityManagerImplSpec extends AnyFlatSpec with MockFactory {
  private implicit val authentication: Authentication = Authentication("", "")

  def fixture(): VirtualSecurityManager = fixture(VirtualUsersManager.ROOT)

  def fixture(user: String): VirtualSecurityManager = {
    val vum = stub[VirtualUsersManager]
    (vum.getUser(_: Authentication)).when(*).returns(Some(user))
    (vum.getGroup(_: Authentication)).when(*).returns(Some(user))

    new VirtualSecurityManagerImpl(vum)

  }

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

    (() => node.permissions.owner).when().returns(permission)
    (() => permission.read).when().returns(true)

    assert(f.checkReadAccess(node))
  }

  "Read access for other" should "be fine" in {
    val f = fixture("guest")

    val node = createNode("john", "text.txt")
    val permission = stub[VirtualPermission]

    (() => node.permissions.others).when().returns(permission)
    (() => permission.read).when().returns(true)

    assert(f.checkReadAccess(node))
  }

  "Read access for owner" should "not be granted" in {
    val f = fixture("guest")

    val node = createNode("guest", "text.txt")
    val permission = stub[VirtualPermission]

    (() => node.permissions.owner).when().returns(permission)
    (() => permission.read).when().returns(false)

    assert(!f.checkReadAccess(node))
  }

  "Read access for others" should "not be granted" in {
    val f = fixture("guest")

    val node = createNode("john", "text.txt")
    val permission = stub[VirtualPermission]

    (() => node.permissions.others).when().returns(permission)
    (() => permission.read).when().returns(false)

    assert(!f.checkReadAccess(node))
  }

  private def createNode(owner: String, name: String): VirtualNode = {
    val node = stub[VirtualNode]
    (() => node.owner).when().returns(owner)
    (() => node.name).when().returns(name)
    val permissions = stub[VirtualPermissions]

    (() => node.permissions).when().returns(permissions)
    node
  }

}
