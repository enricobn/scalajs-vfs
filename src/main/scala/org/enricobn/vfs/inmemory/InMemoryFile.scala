package org.enricobn.vfs.inmemory

import org.enricobn.vfs._

/**
  * Created by enrico on 12/2/16.
  */

class InMemoryFile private[inmemory] (vum: VirtualUsersManager, vsm: VirtualSecurityManager,
                                      parent: Option[InMemoryFolder], name: String)
extends InMemoryNode(vum, vsm, parent, name) with VirtualFile {
  private var _content: AnyRef = ""

  final def content: Either[IOError, AnyRef] =
    if (!vsm.checkReadAccess(this)) {
      Left(accessDenied("content read").get)
    } else {
        Right(_content)
    }

  final def content_=(content: AnyRef): Option[IOError] =
    if (!vsm.checkWriteAccess(this)) {
      accessDenied("content write")
    } else {
      this._content = content
      None
    }

}
