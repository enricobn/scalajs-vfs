package org.enricobn.vfs.impl

import org.enricobn.vfs.IOError._
import org.enricobn.vfs._

import scala.scalajs.js.annotation.{JSExport, JSExportAll}


/**
  * Created by enrico on 6/10/18.
  */
// TODO try to use it
@JSExport(name = "VirtualUsersManagerFileImpl")
@JSExportAll
final class VirtualUsersManagerFileImpl(fs: VirtualFS, rootPassword: String) extends VirtualUsersManager {
  private var _currentUser: String = VirtualUsersManager.ROOT

  private val etcFolderE = fs.root.findFolder("etc") match {
    case Right(Some(etc)) => Right(etc)
    case Right(None) => fs.root.mkdir("etc")
    case _ => throw new RuntimeException("Cannot create etc")
  }

  private val etcFolder = etcFolderE.right.get

  private val passwdFile = etcFolder.findFile("passwd") match {
    case Right(Some(passwd)) =>
      passwd
    case Right(None) =>
      val file = etcFolder.touch("passwd").right.get
      file.content = Map[String, String]()
      file
    case _  => throw new RuntimeException("Cannot create passwd")
  }

  /*VirtualUsersManagerImpl.changePassword(passwdFile, VirtualUsersManager.ROOT, rootPassword)
    .foreach( error => throw new RuntimeException(error.message))
    */

  def currentUser: String = _currentUser

  private def users = passwdFile.content.right.get.asInstanceOf[Map[String, String]]

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
      passwdFile.content = users + (user -> password)
      None
    }

  override def userExists(user: String): Boolean = users.contains(user)

}
