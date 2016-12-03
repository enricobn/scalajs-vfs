package org.enricobn.vfs.impl

import org.enricobn.vfs.{VirtualNode, VirtualPermission, VirtualSecurityException, VirtualUsersManager}

/**
  * Created by enrico on 12/2/16.
  */
class VirtualUsersManagerImpl(rootPassword: String) extends VirtualUsersManager {
  private val users = new scala.collection.mutable.HashMap[String, String]
  private var currentUser: String = VirtualUsersManager.ROOT

  users(VirtualUsersManager.ROOT) = rootPassword

  def getCurrentUser: String = currentUser

  @throws[VirtualSecurityException]
  def checkWriteAccess(node: VirtualNode) {
    checkAccess(node, (vp: VirtualPermission) => vp.write)
  }

  @throws[VirtualSecurityException]
  def checkReadAccess(node: VirtualNode) {
    checkAccess(node, (vp: VirtualPermission) => vp.read)
  }

  @throws[VirtualSecurityException]
  def checkExecuteAccess(node: VirtualNode) {
    checkAccess(node, (vp: VirtualPermission) => vp.execute)
  }

  @throws[VirtualSecurityException]
  private def checkAccess(node: VirtualNode, permission: (VirtualPermission) => Boolean) {
    if (VirtualUsersManager.ROOT == currentUser) {
      return
    }
    if (node.getOwner == currentUser) {
      if (!permission.apply(node.getPermissions.owner)) {
        throw new VirtualSecurityException("Permission denied.")
      }
    }
    else {
      if (!permission.apply(node.getPermissions.others)) {
        throw new VirtualSecurityException("Permission denied.")
      }
    }
  }

  @throws[VirtualSecurityException]
  def logUser(user: String, password: String) {
    if (!users.contains(user)) {
      throw new VirtualSecurityException("Invalid user.")
    }
    else if (!users.get(user).contains(password)) {
      throw new VirtualSecurityException("Invalid password.")
    }
    currentUser = user
  }

  @throws[VirtualSecurityException]
  def logRoot(password: String) {
    if (rootPassword != password) {
      throw new VirtualSecurityException("Invalid password.")
    }
    currentUser = VirtualUsersManager.ROOT
  }

  @throws[VirtualSecurityException]
  def addUser(user: String, password: String) {
    if (currentUser != VirtualUsersManager.ROOT) {
      throw new VirtualSecurityException("Only root can add users.")
    }
    if (users.contains(user)) {
      throw new VirtualSecurityException("User already added.")
    }
    users(user) = password
  }
}
