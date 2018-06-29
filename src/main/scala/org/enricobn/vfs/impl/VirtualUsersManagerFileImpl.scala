package org.enricobn.vfs.impl

import org.enricobn.vfs.IOError._
import org.enricobn.vfs._

import scala.util.Random

object VirtualUsersManagerFileImpl {

  private def createId() = Random.nextInt().toString

  def apply(fs: VirtualFS, rootPassword: String): Either[IOError, VirtualUsersManagerFileImpl] = {
    val rootAuthentication = Authentication(createId(), VirtualUsersManager.ROOT)

    for {
      etcFolderO <- fs.root.findFolder("etc")(rootAuthentication).right
      etcFolder <- etcFolderO match {
        case Some(f) => Right(f).right
        case _ => fs.root.mkdir("etc")(rootAuthentication).right
      }
      passwdFileO <- etcFolder.findFile("passwd")(rootAuthentication).right
      passwdFile <- passwdFileO match {
        case Some(f) => Right(f).right
        case _ => createPasswd(etcFolder, rootAuthentication).right
      }
      _ <- passwdFile.setContent(Map(VirtualUsersManager.ROOT -> rootPassword))(rootAuthentication).toLeft(()).right
    } yield new VirtualUsersManagerFileImpl(fs, passwdFile, rootAuthentication, rootPassword)
  }

  private def createPasswd(etcFolder: VirtualFolder, rootAuthentication: Authentication) = {
    import org.enricobn.vfs.utils.Utils.RightBiasedEither
    for {
      file <- etcFolder.touch("passwd")(rootAuthentication)
      _ <- file.setContent(Map[String, String]())(rootAuthentication).toLeft(())
      permissions = VirtualPermissionsImpl(VirtualPermission.WRITE, VirtualPermission.NONE, VirtualPermission.NONE)
      _ <- file.setPermissions(permissions)(rootAuthentication).toLeft(())
    } yield file
  }

}

/**
  * Created by enrico on 6/10/18.
  */
final class VirtualUsersManagerFileImpl private (fs: VirtualFS, passwdFile: VirtualFile, rootAuthentication: Authentication, rootPassword: String) extends VirtualUsersManager {
  import VirtualUsersManagerFileImpl._

  /**
    * key = user
    * value = (authentication, password)
    */
  private val usersAuthentication = new scala.collection.mutable.HashMap[String, (Authentication, String)]

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
