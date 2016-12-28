package org.enricobn.vfs.inmemory

import org.enricobn.vfs._
import org.enricobn.vfs.IOError._

/**
  * Created by enrico on 12/2/16.
  */

class InMemoryFile private[inmemory] (usersManager: VirtualUsersManager, parent: InMemoryFolder, name: String)
extends InMemoryNode(usersManager, parent, name) with VirtualFile {
  private var _content: AnyRef = ""

  final def content =
    if (!usersManager.checkReadAccess(this)) {
      "Access denied.".ioErrorE
    } else {
        Right(_content)
    }

  final def content_=(content: AnyRef) =
    if (!usersManager.checkWriteAccess(this)) {
      "Access denied.".ioErrorE
    } else {
      Right(this._content = content)
    }

}
