package org.enricobn.vfs

trait VirtualSecurityManager {

  def checkReadAccess(node: VirtualNode)(implicit authentication: Authentication) : Boolean

  def checkExecuteAccess(node: VirtualNode)(implicit authentication: Authentication) : Boolean

  def checkWriteAccess(node: VirtualNode)(implicit authentication: Authentication) : Boolean

}
