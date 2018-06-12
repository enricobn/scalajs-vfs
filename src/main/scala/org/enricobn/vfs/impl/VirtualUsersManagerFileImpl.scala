package org.enricobn.vfs.impl

import org.enricobn.vfs.IOError._
import org.enricobn.vfs._

import scala.scalajs.js.annotation.{JSExport, JSExportAll}
import scala.util.Random

object VirtualUsersManagerFileImpl {

  private def createId() = Random.nextInt().toString

}

/**
  * Created by enrico on 6/10/18.
  */
// TODO try to use it
@JSExport(name = "VirtualUsersManagerFileImpl")
@JSExportAll
final class VirtualUsersManagerFileImpl(fs: VirtualFS, rootPassword: String) extends VirtualUsersManager {
  import VirtualUsersManagerFileImpl._

  //private var _currentUser: String = VirtualUsersManager.ROOT
  private val rootAuthentication = Authentication(createId(), VirtualUsersManager.ROOT)

  private val etcFolderE = fs.root.findFolder("etc")(rootAuthentication) match {
    case Right(Some(etc)) => Right(etc)
    case Right(None) => fs.root.mkdir("etc")(rootAuthentication)
    case _ => throw new RuntimeException("Cannot create etc")
  }

  private val etcFolder = etcFolderE.right.get

  private val passwdFile = etcFolder.findFile("passwd")(rootAuthentication) match {
    case Right(Some(passwd)) =>
      passwd
    case Right(None) =>
      val file = etcFolder.touch("passwd")(rootAuthentication).right.get
      file.setContent(Map[String, String]())(rootAuthentication)
      file
    case _  => throw new RuntimeException("Cannot create passwd")
  }

  /*VirtualUsersManagerImpl.changePassword(passwdFile, VirtualUsersManager.ROOT, rootPassword)
    .foreach( error => throw new RuntimeException(error.message))
    */

//  def currentUser: String = _currentUser

  private def users = passwdFile.getContent(rootAuthentication).right.get.asInstanceOf[Map[String, String]]

  override def logUser(user: String, password: String): Either[IOError, Authentication] = ???
    /*if (!users.contains(user)) {
      "Invalid user.".ioErrorO
    } else if (!users.get(user).contains(password)) {
      "Invalid password.".ioErrorO
    } else {
      _currentUser = user
      None
    }
    */

  override def logRoot(password: String): Either[IOError, Authentication] = ???
/*    if (rootPassword != password) {
      "Invalid password.".ioErrorO
    } else {
      _currentUser = VirtualUsersManager.ROOT
      None
    }
    */

  override def addUser(user: String, password: String)(implicit authentication: Authentication): Option[IOError] = {
    val currentUser = getUser.get
    if (currentUser != VirtualUsersManager.ROOT) {
      "Only root can add users.".ioErrorO
    } else if (users.contains(user)) {
      "User already added.".ioErrorO
    } else {
      passwdFile.setContent(users + (user -> password))(rootAuthentication)
      None
    }
  }

  override def userExists(user: String): Boolean = users.contains(user)

  override def getUser(implicit authentication: Authentication): Option[String] = ???

}
