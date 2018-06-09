package org.enricobn.vfs.inmemory

import org.enricobn.vfs._
import org.enricobn.vfs.IOError._

/**
  * Created by enrico on 12/2/16.
  */

class InMemoryFile private[inmemory] (usersManager: VirtualUsersManager, parent: Option[InMemoryFolder], name: String)
extends InMemoryNode(usersManager, parent, name) with VirtualFile {
  private var _content: AnyRef = ""

  final def content: Either[IOError, AnyRef] =
    if (!usersManager.checkReadAccess(this)) {
      Left(accessDenied("content read").get)
    } else {
        Right(_content)
    }

  final def content_=(content: AnyRef): Option[IOError] =
    if (!usersManager.checkWriteAccess(this)) {
      accessDenied("content write")
    } else {
      this._content = content
      None
    }

}
