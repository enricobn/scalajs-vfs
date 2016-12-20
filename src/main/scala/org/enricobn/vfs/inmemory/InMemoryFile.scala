package org.enricobn.vfs.inmemory

import org.enricobn.vfs.{VirtualFile, VirtualUsersManager}

import org.enricobn.vfs.IOError._

/**
  * Created by enrico on 12/2/16.
  */
class InMemoryFile private[inmemory] (usersManager: VirtualUsersManager, parent: InMemoryFolder, name: String)
extends InMemoryNode(usersManager, parent, name) with VirtualFile {
  private var _content: AnyRef = ""

  final def content = Right(_content)

  final def content_=(content: AnyRef) = {
    if (!usersManager.checkWriteAccess(this)) {
      "Access denied.".ioErrorE
    }
    Right(() => this._content = content)
  }


  final override def run(args: String*) = {
    if (!usersManager.checkExecuteAccess(this)) {
      "Access denied.".ioErrorE
    }
    internalRun(args: _*)
  }

  protected def internalRun(args: String*) : IOEff[Unit] =
    (name + ": unsupported executable format").ioErrorE

}
