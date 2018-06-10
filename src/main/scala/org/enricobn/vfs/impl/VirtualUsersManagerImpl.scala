package org.enricobn.vfs.impl

import org.enricobn.vfs.IOError._
import org.enricobn.vfs.{IOError, VirtualUsersManager}

import scala.scalajs.js.annotation.{JSExport, JSExportAll}

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

}
