package org.enricobn.vfs.inmemory

import org.enricobn.vfs.*

/**
  * Created by enrico on 12/2/16.
  */

class InMemoryFile private[inmemory](vum: VirtualUsersManager, vsm: VirtualSecurityManager,
                                     fsINotify: VirtualFSNotifier, parent: Option[InMemoryFolder], name: String,
                                     owner: String, group: String)
  extends InMemoryNode(vum, vsm, parent, name, owner, group) with VirtualFile {
  private var _content: AnyRef = ""

  final def getContent(implicit authentication: Authentication): Either[IOError, AnyRef] =
    if (!vsm.checkReadAccess(this)) {
      accessDenied("content read")
    } else {
      Right(_content)
    }

  final def setContent(content: AnyRef)(implicit authentication: Authentication): Either[IOError, Unit] =
    if (!vsm.checkWriteAccess(this)) {
      accessDenied("content write")
    } else {
      this._content = content
      fsINotify.notify(this)
      Right(())
    }

  override def compare(that: VirtualNode): Int = super.compareTo(that)
}
