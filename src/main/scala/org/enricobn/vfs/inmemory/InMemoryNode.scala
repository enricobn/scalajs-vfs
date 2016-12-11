package org.enricobn.vfs.inmemory

import org.enricobn.vfs._

/**
  * Created by enrico on 12/2/16.
  */
class InMemoryNode private[inmemory] (val usersManager: VirtualUsersManager, val parent: VirtualFolder, val name: String)
extends VirtualNode {
  val owner: String = usersManager.currentUser
  val permissions: VirtualPermissions = new InMemoryPermissions
  var _executable = false

  val executable = _executable

  if (usersManager.currentUser != VirtualUsersManager.ROOT) {
    usersManager.checkWriteAccess(parent)
  }

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

//  private[inmemory] def getUsersManager: VirtualUsersManager = usersManager

}
