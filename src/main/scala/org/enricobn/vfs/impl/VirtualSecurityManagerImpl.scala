package org.enricobn.vfs.impl

import org.enricobn.vfs._

import scala.scalajs.js.annotation.{JSExport, JSExportAll}

/**
  * Created by enrico on 12/2/16.
  */
@JSExport(name = "VirtualSecurityManagerImpl")
@JSExportAll
final class VirtualSecurityManagerImpl(vum: VirtualUsersManager) extends VirtualSecurityManager {

  def checkWriteAccess(node: VirtualNode): Boolean =
    checkAccess(node, (vp: VirtualPermission) => vp.write)

  def checkReadAccess(node: VirtualNode): Boolean =
    checkAccess(node, (vp: VirtualPermission) => vp.read)

  def checkExecuteAccess(node: VirtualNode): Boolean =
    checkAccess(node, (vp: VirtualPermission) => vp.execute)

  private def checkAccess(node: VirtualNode, permission: VirtualPermission => Boolean) : Boolean =
    if (VirtualUsersManager.ROOT == vum.currentUser) {
      true
    } else if (node.owner == vum.currentUser) {
      permission.apply(node.permissions.owner)
      //TODO group
    } else {
      permission.apply(node.permissions.others)
    }

}
