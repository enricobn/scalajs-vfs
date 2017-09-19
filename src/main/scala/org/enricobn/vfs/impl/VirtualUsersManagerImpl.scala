package org.enricobn.vfs.impl

import org.enricobn.vfs.{IOError, VirtualNode, VirtualPermission, VirtualUsersManager}

import scala.scalajs.js.annotation.{JSExport, JSExportAll}
import org.enricobn.vfs.IOError._

/**
  * Created by enrico on 12/2/16.
  */
@JSExport(name = "VirtualUsersManagerImpl")
@JSExportAll
final class VirtualUsersManagerImpl(rootPassword: String) extends VirtualUsersManager {
  private val users = new scala.collection.mutable.HashMap[String, String]
  private var _currentUser: String = VirtualUsersManager.ROOT

  users(VirtualUsersManager.ROOT) = rootPassword

  def currentUser: String = _currentUser

  def checkWriteAccess(node: VirtualNode): Boolean =
    checkAccess(node, (vp: VirtualPermission) => vp.write)

  def checkReadAccess(node: VirtualNode): Boolean =
    checkAccess(node, (vp: VirtualPermission) => vp.read)

  def checkExecuteAccess(node: VirtualNode): Boolean =
    checkAccess(node, (vp: VirtualPermission) => vp.execute)

  def logUser(user: String, password: String): Option[IOError] =
    if (!users.contains(user)) {
      "Invalid user.".ioErrorO
    } else if (!users.get(user).contains(password)) {
      "Invalid password.".ioErrorO
    } else {
      _currentUser = user
      None
    }

  def logRoot(password: String): Option[IOError] =
    if (rootPassword != password) {
      "Invalid password.".ioErrorO
    } else {
      _currentUser = VirtualUsersManager.ROOT
      None
    }

  def addUser(user: String, password: String): Option[IOError] =
    if (currentUser != VirtualUsersManager.ROOT) {
      "Only root can add users.".ioErrorO
    } else if (users.contains(user)) {
      "User already added.".ioErrorO
    } else {
      users(user) = password
      None
    }

  override def userExists(user: String): Boolean = users.contains(user)

  private def checkAccess(node: VirtualNode, permission: (VirtualPermission) => Boolean) : Boolean =
    if (VirtualUsersManager.ROOT == currentUser) {
      true
    } else if (node.owner == currentUser) {
      permission.apply(node.permissions.owner)
      //TODO group
    } else {
      permission.apply(node.permissions.others)
    }

}
