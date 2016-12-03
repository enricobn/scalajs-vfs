package org.enricobn.vfs.inmemory

import org.enricobn.vfs.{VirtualFile, VirtualIOException, VirtualSecurityException, VirtualUsersManager}

/**
  * Created by enrico on 12/2/16.
  */
class InMemoryFile(usersManager: VirtualUsersManager, parent: InMemoryFolder, name: String)
extends InMemoryNode(usersManager, parent, name) with VirtualFile {
  private var content: AnyRef = ""

  @throws[VirtualIOException]
  def getContent: AnyRef = content

  @throws[VirtualIOException]
  def setContent (content: AnyRef) {
    try {
      checkWriteAccess (this)
    } catch {
      case e: VirtualSecurityException =>
        throw new VirtualIOException (e.getMessage, e)
    }
    this.content = content
  }
}

