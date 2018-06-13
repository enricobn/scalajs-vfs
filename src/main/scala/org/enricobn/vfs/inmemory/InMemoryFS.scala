package org.enricobn.vfs.inmemory

import org.enricobn.vfs._
import org.enricobn.vfs.impl.{VirtualSecurityManagerImpl, VirtualUsersManagerFileImpl}

import scala.scalajs.js.annotation.{JSExport, JSExportAll}

/**
  * Created by enrico on 12/3/16.
  */

@JSExport(name = "InMemoryFS")
@JSExportAll
class InMemoryFS(rootPassword: String) extends VirtualFS {
  private var _vum: VirtualUsersManager = _
  private var _vsm: VirtualSecurityManager = _
  val vum = new UsersManagerProxy
  val vsm = new SecurityManagerProxy

  val root = new InMemoryFolder(vum, vsm, None, VirtualFS.rootPath.path, VirtualUsersManager.ROOT)
  _vum = new VirtualUsersManagerFileImpl(this, rootPassword)
  _vsm = new VirtualSecurityManagerImpl(_vum)

  class UsersManagerProxy extends VirtualUsersManager {

    override def logUser(user: String, password: String): Either[IOError, Authentication] =
      _vum.logUser(user, password)

    override def addUser(user: String, password: String)(implicit authentication: Authentication): Option[IOError] =
      _vum.addUser(user, password)

    override def userExists(user: String): Boolean = _vum.userExists(user)

    override def getUser(implicit authentication: Authentication): Option[String] =
      if (_vum == null) {
        Some(VirtualUsersManager.ROOT)
      } else {
        _vum.getUser
      }

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
