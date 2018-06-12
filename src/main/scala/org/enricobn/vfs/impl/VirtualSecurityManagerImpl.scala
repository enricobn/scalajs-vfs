package org.enricobn.vfs.impl

import org.enricobn.vfs._

import scala.scalajs.js.annotation.{JSExport, JSExportAll}

/**
  * Created by enrico on 12/2/16.
  */
@JSExport(name = "VirtualSecurityManagerImpl")
@JSExportAll
final class VirtualSecurityManagerImpl(vum: VirtualUsersManager) extends VirtualSecurityManager {

  def checkWriteAccess(node: VirtualNode)(implicit authentication: Authentication): Boolean =
    checkAccess(node, (vp: VirtualPermission) => vp.write)

  def checkReadAccess(node: VirtualNode)(implicit authentication: Authentication): Boolean =
    checkAccess(node, (vp: VirtualPermission) => vp.read)

  def checkExecuteAccess(node: VirtualNode)(implicit authentication: Authentication): Boolean =
    checkAccess(node, (vp: VirtualPermission) => vp.execute)

  private def checkAccess(node: VirtualNode, permission: VirtualPermission => Boolean)(implicit authentication: Authentication) = {
    // TODO handle error
    val user = vum.getUser.get
    if (VirtualUsersManager.ROOT == user) {
      true
    } else if (node.owner == user) {
      permission.apply(node.permissions.owner)
      //TODO group
    } else {
      permission.apply(node.permissions.others)
    }
  }

}
