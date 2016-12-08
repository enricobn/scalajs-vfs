package org.enricobn.vfs.inmemory

import org.enricobn.vfs._

/**
  * Created by enrico on 12/2/16.
  */
class InMemoryNode(usersManager: VirtualUsersManager, parent: VirtualFolder, private val name: String)
extends VirtualNode {
  private val owner: String = usersManager.getCurrentUser
  private val permissions: InMemoryPermissions = new InMemoryPermissions
  private var executable: Boolean = false

  def getName: String = name

  def getParent: VirtualFolder = parent

  def getOwner: String = owner

  def getPermissions: VirtualPermissions = permissions

  def isExecutable: Boolean = executable

  @throws[VirtualIOException]
  def setExecutable(executable: Boolean) {
    try {
      usersManager.checkWriteAccess(this)
    }
    catch {
      case e: VirtualSecurityException =>
        throw new VirtualIOException(e.getMessage, e)
    }
    this.executable = executable
  }

  @throws[VirtualSecurityException]
  private[inmemory] def checkWriteAccess(node: VirtualNode) {
    usersManager.checkWriteAccess(node)
  }

  private[inmemory] def getUsersManager: VirtualUsersManager = usersManager

}
