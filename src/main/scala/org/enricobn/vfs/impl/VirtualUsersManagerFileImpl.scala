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

  /**
    * key = user
    * value = (authentication, password)
    */
  private val usersAuthentication = new scala.collection.mutable.HashMap[String, (Authentication, String)]

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
      val permissions = VirtualPermissionsImpl(VirtualPermission.WRITE, VirtualPermission.NONE, VirtualPermission.NONE)
      file.setPermissions(permissions)(rootAuthentication)
      file
    case _  => throw new RuntimeException("Cannot create passwd")
  }

  passwdFile.setContent(Map(VirtualUsersManager.ROOT -> rootPassword))(rootAuthentication)
  usersAuthentication(VirtualUsersManager.ROOT) = (rootAuthentication, rootPassword)

  /**
    * key : user name
    * value: user password
    */
  private def users = passwdFile.getContent(rootAuthentication).right.get.asInstanceOf[Map[String, String]]

  override def logUser(user: String, password: String): Either[IOError, Authentication] =
    if (!users.contains(user)) {
      "Invalid user.".ioErrorE
    } else if (!users.get(user).contains(password)) {
      "Invalid password.".ioErrorE
    } else {
      Right(usersAuthentication(user)._1)
    }

  override def addUser(user: String, password: String)(implicit authentication: Authentication): Option[IOError] =
    if (!getUser.contains(VirtualUsersManager.ROOT)) {
      "Only root can add users.".ioErrorO
    } else if (users.contains(user)) {
      "User already added.".ioErrorO
    } else {
      passwdFile.setContent(users + (user -> password))(rootAuthentication)
      usersAuthentication(user) = (Authentication(createId(), user), password)
      val createHomeFolder = for {
        home <- fs.root.resolveFolderOrError("/home")(rootAuthentication).right
        userFolder <- home.mkdir(user)(rootAuthentication).right
        _ <- userFolder.chown(user)(rootAuthentication).toLeft(()).right
      } yield ()

      createHomeFolder.left.toOption
    }

  override def userExists(user: String): Boolean = users.contains(user)

  override def getUser(implicit authentication: Authentication): Option[String] =
    usersAuthentication.find { case (_, (auth, _)) => authentication.id == auth.id } match {
      case Some((user, (_, _))) => Some(user)
      case _ => None
    }

}
