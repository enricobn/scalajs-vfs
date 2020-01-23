package org.enricobn.vfs.inmemory

import org.enricobn.vfs._
import org.enricobn.vfs.impl.{VirtualFSNotifierImpl, VirtualSecurityManagerImpl, VirtualUsersManagerFileImpl}

/**
  * Created by enrico on 12/3/16.
  */

object InMemoryFS {

  def apply(rootPassword: String): Either[IOError, InMemoryFS] = {
    val fs = new InMemoryFS()
    for {
      vum <- VirtualUsersManagerFileImpl(fs, rootPassword).right
    } yield {
      fs.setVum(vum)
      fs.setVsm(new VirtualSecurityManagerImpl(vum))
      fs
    }
  }

  def apply(vumCreator: InMemoryFS => VirtualUsersManager,
            vsmCreator: (InMemoryFS, VirtualUsersManager) => VirtualSecurityManager): InMemoryFS = {
    val fs = new InMemoryFS()
    val vum = vumCreator(fs)
    val vsm = vsmCreator(fs, vum)
    fs.setVum(vum)
    fs.setVsm(vsm)
    fs
  }

}

class InMemoryFS private () extends VirtualFS {
  private var _vum: VirtualUsersManager = _
  private var _vsm: VirtualSecurityManager = _
  val vum = new UsersManagerProxy
  val vsm = new SecurityManagerProxy
  override val notifier: VirtualFSNotifier = new VirtualFSNotifierImpl()

  val root: InMemoryFolder = InMemoryFolder.root(vum, vsm, notifier)

  private def setVum(vum: VirtualUsersManager): Unit = {
    _vum = vum
  }

  private def setVsm(vsm: VirtualSecurityManager): Unit = {
    _vsm = vsm
  }

  class UsersManagerProxy extends VirtualUsersManager {

    override def logUser(user: String, password: String): Either[IOError, Authentication] =
      _vum.logUser(user, password)

    override def addUser(user: String, password: String, group: String)(implicit authentication: Authentication): Option[IOError] =
      _vum.addUser(user, password, group)

    override def userExists(user: String): Boolean = _vum.userExists(user)

    override def getUser(implicit authentication: Authentication): Option[String] =
      if (_vum == null) {
        Some(VirtualUsersManager.ROOT)
      } else {
        _vum.getUser
      }

    override def getGroup(implicit authentication: Authentication): Option[String] =
      if (_vum == null)
        Some(VirtualUsersManager.ROOT)
    else
      _vum.getGroup

  }

  class SecurityManagerProxy extends VirtualSecurityManager {
    override def checkReadAccess(node: VirtualNode)(implicit authentication: Authentication): Boolean =
      if (_vsm == null) {
        true
      } else {
        _vsm.checkReadAccess(node)
      }

    override def checkExecuteAccess(node: VirtualNode)(implicit authentication: Authentication): Boolean =
      if (_vsm == null) {
        true
      } else {
        _vsm.checkExecuteAccess(node)
      }

    override def checkWriteAccess(node: VirtualNode)(implicit authentication: Authentication): Boolean =
      if (_vsm == null) {
        true
      } else {
        _vsm.checkWriteAccess(node)
      }
  }

}
