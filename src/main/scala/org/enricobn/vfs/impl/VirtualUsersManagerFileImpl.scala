package org.enricobn.vfs.impl

import java.util.UUID

import org.enricobn.vfs.IOError._
import org.enricobn.vfs._
import org.enricobn.vfs.utils.Utils.RightBiasedEither

object VirtualUsersManagerFileImpl {

  private def createId() = UUID.randomUUID().toString

  def apply(fs: VirtualFS, rootPassword: String): Either[IOError, VirtualUsersManagerFileImpl] = {
    val rootAuthentication = Authentication(createId(), VirtualUsersManager.ROOT)

    val passwd = Passwd(Set(AuthenticatedUser(rootAuthentication, VirtualUsersManager.ROOT, rootPassword)))

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
      _ <- getOrCreate(fs, "home")(rootAuthentication)
      _ <- getOrCreate(fs, "home", VirtualUsersManager.ROOT)(rootAuthentication)
    } yield new VirtualUsersManagerFileImpl(fs, passwd, _ => Right(passwdFile), rootAuthentication, rootPassword)
  }

  def apply(fs: VirtualFS, passwd: Passwd): Either[IOError, VirtualUsersManagerFileImpl] = {
    val rootAuthentication = passwd.users.find(_.user == VirtualUsersManager.ROOT).get.auth
    val rootPassword = passwd.users.find(_.user == VirtualUsersManager.ROOT).get.password

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
      _ <- getOrCreate(fs, "home")(rootAuthentication)
      _ <- getOrCreate(fs, "home", VirtualUsersManager.ROOT)(rootAuthentication)
    } yield new VirtualUsersManagerFileImpl(fs, passwd, _ => Right(passwdFile), rootAuthentication, rootPassword)
  }

  private def createPasswd(etcFolder: VirtualFolder, rootAuthentication: Authentication) = {
    for {
      file <- etcFolder.touch("passwd")(rootAuthentication)
      _ <- file.setContent(Passwd(Set()))(rootAuthentication).toLeft(())
      permissions = VirtualPermissionsImpl(VirtualPermission.WRITE, VirtualPermission.NONE, VirtualPermission.NONE)
      _ <- file.setPermissions(permissions)(rootAuthentication).toLeft(())
    } yield file
  }

  private def getOrCreate(fs: VirtualFS, path: String*)(implicit authentication: Authentication) = {
    for {
      vp <- VirtualPath.of(path:_*)
      folder <- fs.root.resolveFolder(vp)
      result <- folder match {
        case Some(f) => Right(f)
        case _ => {
          val parent = vp.parentFragments match {
            case Some(f) => f
            case _ => VirtualPath(List())
          }
          fs.root.resolveFolder(parent).flatMap(_.get.mkdir(vp.name))
        }
      }
    } yield result
  }

}

/**
  * Created by enrico on 6/10/18.
  */
final class VirtualUsersManagerFileImpl private(fs: VirtualFS, initialPasswd: Passwd, passwdFileProvider: VirtualFS => Either[IOError, VirtualFile],
                                                rootAuthentication: Authentication, rootPassword: String) extends VirtualUsersManager {

  import VirtualUsersManagerFileImpl._

  private var passwd = initialPasswd

  override def logUser(user: String, password: String): Either[IOError, Authentication] = {
    if (!passwd.users.exists(_.user == user)) {
      "Invalid user.".ioErrorE
    } else {
      val maybeUser = passwd.users.find(u => u.user == user && u.password == password)
      maybeUser match {
        case Some(AuthenticatedUser(auth, _, _)) => Right(auth)
        case _ => "Invalid password.".ioErrorE
      }
    }
  }

  override def addUser(user: String, password: String)(implicit authentication: Authentication): Option[IOError] =
    if (!getUser.contains(VirtualUsersManager.ROOT)) {
      "Only root can add users.".ioErrorO
    } else if (passwd.users.exists(_.user == user)) {
      "User already added.".ioErrorO
    } else {
      val newUser = AuthenticatedUser(Authentication(createId(), user), user, password)
      val newUsers = Passwd(passwd.users + newUser)
      // TODO I don't like it since if there is an error in createHomeFolder the user is already added
      // but if I don't add it before chown then it fails
      this.passwd = newUsers
      val createHomeFolder = for {
        passwdFile <- passwdFileProvider(fs)
        _ <- passwdFile.setContent(newUsers).toLeft(())
        home <- fs.root.resolveFolderOrError("/home")(rootAuthentication).right
        userFolder <- home.mkdir(user)(rootAuthentication).right
        _ <- userFolder.chown(user)(rootAuthentication).toLeft(()).right
      } yield ()

      createHomeFolder.left.toOption
    }

  override def userExists(user: String): Boolean = passwd.users.exists(_.user == user)

  override def getUser(implicit authentication: Authentication): Option[String] =
    passwd.users.find { case AuthenticatedUser(auth, _, _) => authentication.id == auth.id } match {
      case Some(AuthenticatedUser(_, user, _)) => Some(user)
      case _ => None
    }

}
case class AuthenticatedUser(auth: Authentication, user: String, password: String)

case class Passwd(users: Set[AuthenticatedUser])
