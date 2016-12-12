package org.enricobn.vfs.inmemory

import org.enricobn.vfs.{VirtualFile, VirtualIOException, VirtualSecurityException, VirtualUsersManager}

/**
  * Created by enrico on 12/2/16.
  */
class InMemoryFile private[inmemory] (usersManager: VirtualUsersManager, parent: InMemoryFolder, name: String)
extends InMemoryNode(usersManager, parent, name) with VirtualFile {
  private var _content: AnyRef = ""

  @throws[VirtualIOException]
  final def content = _content

  @throws[VirtualIOException]
  final def content_=(content: AnyRef) {
    try {
      usersManager.checkWriteAccess(this)
    } catch {
      case e: VirtualSecurityException =>
        throw new VirtualIOException (e.getMessage, e)
    }
    this._content = content
  }

  @throws[VirtualIOException]
  final override def run(args: String*) {
    try {
      usersManager.checkExecuteAccess(this)
    } catch {
      case e: VirtualSecurityException =>
        throw new VirtualIOException (e.getMessage, e)
    }
    internalRun(args: _*)
  }

  @throws[VirtualIOException]
  protected def internalRun(args: String*) {
    throw new VirtualIOException(name + ": unsupported executable format")
  }

}
