package org.enricobn.vfs

trait VirtualSecurityManager {

  def checkReadAccess(node: VirtualNode) : Boolean

  def checkExecuteAccess(node: VirtualNode) : Boolean

  def checkWriteAccess(node: VirtualNode) : Boolean

}
