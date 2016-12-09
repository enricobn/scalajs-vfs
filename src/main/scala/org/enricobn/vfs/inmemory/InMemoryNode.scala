package org.enricobn.vfs.inmemory

import org.enricobn.vfs._

/**
  * Created by enrico on 12/2/16.
  */
class InMemoryNode(val usersManager: VirtualUsersManager, val parent: VirtualFolder, val name: String)
extends VirtualNode {
  val owner: String = usersManager.currentUser
  val permissions: VirtualPermissions = new InMemoryPermissions
  var _executable = false

  val executable = _executable

  @throws[VirtualIOException]
  def executable_=(executable: Boolean) {
    try {
      usersManager.checkWriteAccess(this)
    }
    catch {
      case e: VirtualSecurityException =>
        throw new VirtualIOException(e.getMessage, e)
    }
    this._executable = executable
  }

  @throws[VirtualSecurityException]
  private[inmemory] def checkWriteAccess(node: VirtualNode) {
    usersManager.checkWriteAccess(node)
  }

//  private[inmemory] def getUsersManager: VirtualUsersManager = usersManager

}
