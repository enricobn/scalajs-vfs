package org.enricobn.vfs.impl

import org.enricobn.vfs.*
import org.enricobn.vfs.IOError.*

import scala.util.Random.nextInt

object VirtualUsersManagerFileImpl {

  private def createId() = nextInt().toString

  def apply(fs: VirtualFS, rootPassword: String): Either[IOError, VirtualUsersManagerFileImpl] = {
    val rootAuthentication = Authentication(createId(), VirtualUsersManager.ROOT)

    val passwd = Passwd(Set(AuthenticatedUser(rootAuthentication, VirtualUsersManager.ROOT, rootPassword, VirtualUsersManager.ROOT)))

    for {
      etcFolderO <- fs.root.findFolder("etc")(rootAuthentication)
      etcFolder <- etcFolderO match {
        case Some(f) => Right(f)
        case _ => fs.root.mkdir("etc")(rootAuthentication)
      }
      passwdFileO <- etcFolder.findFile("passwd")(rootAuthentication)
      passwdFile <- passwdFileO match {
        case Some(f) => Right(f)
        case _ => createPasswd(etcFolder, rootAuthentication)
      }
      _ <- getOrCreate(fs, "home")(rootAuthentication)
      _ <- getOrCreate(fs, "home", VirtualUsersManager.ROOT)(rootAuthentication)
    } yield new VirtualUsersManagerFileImpl(fs, passwd, _ => Right(passwdFile), rootAuthentication, rootPassword)
  }

  def apply(fs: VirtualFS, passwd: Passwd): Either[IOError, VirtualUsersManagerFileImpl] = {
    val rootAuthentication = passwd.users.find(_.user == VirtualUsersManager.ROOT).get.auth
    val rootPassword = passwd.users.find(_.user == VirtualUsersManager.ROOT).get.password

    for {
      etcFolderO <- fs.root.findFolder("etc")(rootAuthentication)
      etcFolder <- etcFolderO match {
        case Some(f) => Right(f)
        case _ => fs.root.mkdir("etc")(rootAuthentication)
      }
      passwdFileO <- etcFolder.findFile("passwd")(rootAuthentication)
      passwdFile <- passwdFileO match {
        case Some(f) => Right(f)
        case _ => createPasswd(etcFolder, rootAuthentication)
      }
      _ <- getOrCreate(fs, "home")(rootAuthentication)
      _ <- getOrCreate(fs, "home", VirtualUsersManager.ROOT)(rootAuthentication)
    } yield new VirtualUsersManagerFileImpl(fs, passwd, _ => Right(passwdFile), rootAuthentication, rootPassword)
  }

  private def createPasswd(etcFolder: VirtualFolder, rootAuthentication: Authentication) = {
    for {
      file <- etcFolder.touch("passwd")(rootAuthentication)
      _ <- file.setContent(Passwd(Set()))(rootAuthentication)
      permissions = VirtualPermissionsImpl(VirtualPermission.WRITE, VirtualPermission.NONE, VirtualPermission.NONE)
      _ <- file.setPermissions(permissions)(rootAuthentication)
    } yield file
  }

  private def getOrCreate(fs: VirtualFS, path: String*)(implicit authentication: Authentication) = {
    for {
      vp <- VirtualPath.absolute(path*)
      folder <- vp.toFolderOrCreate(fs.root)
    } yield folder
  }

}

/**
  * Created by enrico on 6/10/18.
  */
final class VirtualUsersManagerFileImpl private(fs: VirtualFS, initialPasswd: Passwd, passwdFileProvider: VirtualFS => Either[IOError, VirtualFile],
                                                rootAuthentication: Authentication, rootPassword: String) extends VirtualUsersManager {

  import VirtualUsersManagerFileImpl.*

  private var passwd = initialPasswd

  override def logUser(user: String, password: String): Either[IOError, Authentication] = {
    if (!passwd.users.exists(_.user == user)) {
      "Invalid user.".ioErrorE
    } else {
      val maybeUser = passwd.users.find(u => u.user == user && u.password == password)
      maybeUser match {
        case Some(AuthenticatedUser(auth, _, _, _)) => Right(auth)
        case _ => "Invalid password.".ioErrorE
      }
    }
  }

  override def addUser(user: String, password: String, group: String)(implicit authentication: Authentication): Either[IOError, Unit] =
    if (!getUser.contains(VirtualUsersManager.ROOT)) {
      "Only root can add users.".ioErrorE
    } else if (passwd.users.exists(_.user == user)) {
      "User already added.".ioErrorE
    } else {
      val newUser = AuthenticatedUser(Authentication(createId(), user), user, password, group)
      val newUsers = Passwd(passwd.users + newUser)
      // TODO I don't like it since if there is an error in createHomeFolder the user is already added
      // but if I don't add it before chown then it fails
      this.passwd = newUsers
      val createHomeFolder = for {
        passwdFile <- passwdFileProvider(fs)
        _ <- passwdFile.setContent(newUsers)
        home <- VirtualPath.absolute("home").flatMap(_.toFolder(fs))
        userFolder <- home.mkdir(user)(rootAuthentication)
        _ <- userFolder.chown(user)(rootAuthentication)
      } yield ()

      createHomeFolder
    }

  override def userExists(user: String): Boolean = passwd.users.exists(_.user == user)

  override def getUser(implicit authentication: Authentication): Option[String] =
    passwd.users.find { case AuthenticatedUser(auth, _, _, _) => authentication.id == auth.id } match {
      case Some(AuthenticatedUser(_, user, _, _)) => Some(user)
      case _ => None
    }

  override def getGroup(implicit authentication: Authentication): Option[String] =
    passwd.users.find { case AuthenticatedUser(auth, _, _, _) => authentication.id == auth.id } match {
      case Some(AuthenticatedUser(_, _, _, group)) => Some(group)
      case _ => None
    }

}
case class AuthenticatedUser(auth: Authentication, user: String, password: String, group: String)

case class Passwd(users: Set[AuthenticatedUser])
