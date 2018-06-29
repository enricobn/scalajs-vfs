package org.enricobn.vfs.inmemory

import org.enricobn.vfs._

/**
  * Created by enrico on 12/2/16.
  */

class InMemoryFile private[inmemory] (vum: VirtualUsersManager, vsm: VirtualSecurityManager,
                                      fsINotify: VirtualFSNotifier, parent: Option[InMemoryFolder], name: String, owner: String)
extends InMemoryNode(vum, vsm, parent, name, owner) with VirtualFile {
  private var _content: AnyRef = ""

  final def getContent(implicit authentication: Authentication) : Either[IOError, AnyRef] =
    if (!vsm.checkReadAccess(this)) {
      Left(accessDenied("content read").get)
    } else {
      Right(_content)
    }

  final def setContent(content: AnyRef)(implicit authentication: Authentication): Option[IOError] =
    if (!vsm.checkWriteAccess(this)) {
      accessDenied("content write")
    } else {
      this._content = content
      fsINotify.notify(this)
      None
    }

}
