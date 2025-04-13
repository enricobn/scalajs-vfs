package org.enricobn.vfs.impl

import org.enricobn.vfs.*

import scala.scalajs.js.annotation.{JSExportAll, JSExportTopLevel}

/**
  * Created by enrico on 12/2/16.
  */
@JSExportTopLevel(name = "VirtualSecurityManagerImpl")
@JSExportAll
final class VirtualSecurityManagerImpl(vum: VirtualUsersManager) extends VirtualSecurityManager {

  def checkWriteAccess(node: VirtualNode)(implicit authentication: Authentication): Boolean =
    checkAccess(node, (vp: VirtualPermission) => vp.write)

  def checkReadAccess(node: VirtualNode)(implicit authentication: Authentication): Boolean =
    checkAccess(node, (vp: VirtualPermission) => vp.read)

  def checkExecuteAccess(node: VirtualNode)(implicit authentication: Authentication): Boolean =
    checkAccess(node, (vp: VirtualPermission) => vp.execute)

  private def checkAccess(node: VirtualNode, permission: VirtualPermission => Boolean)(implicit authentication: Authentication) = {
    // TODO handle error better
    if (vum.getUser.isEmpty) {
      throw new Exception("Cannot find user for the given authentication.")
    }

    val user = vum.getUser.get
    val group = vum.getGroup.get
    if (VirtualUsersManager.ROOT == user) {
      true
    } else if (node.owner == user) {
      permission.apply(node.permissions.owner)
    } else if (node.group == group) {
      permission.apply(node.permissions.group)
    } else {
      permission.apply(node.permissions.others)
    }
  }

}
