package org.enricobn.vfs.impl

import org.enricobn.vfs.IOError._
import org.enricobn.vfs.{Authentication, IOError, VirtualUsersManager}

import scala.scalajs.js.annotation.{JSExport, JSExportAll}
import scala.util.Random

object VirtualUsersManagerImplDeprecated {

  private def createId() = Random.nextInt().toString

}

/**
  * Created by enrico on 12/2/16.
  * Deprecated: use InMemoryFS.vum
  */
@JSExport(name = "VirtualUsersManagerImpl")
@JSExportAll
@Deprecated
final class VirtualUsersManagerImplDeprecated(rootPassword: String) extends VirtualUsersManager {
  /**
    * key = user
    * value = (authentication, password)
    */
  private val users = new scala.collection.mutable.HashMap[String, (Authentication, String)]

  import VirtualUsersManagerImplDeprecated._

  users(VirtualUsersManager.ROOT) = (Authentication(createId(), VirtualUsersManager.ROOT), rootPassword)

  def logUser(user: String, password: String): Either[IOError, Authentication] =
    if (users.contains(user)) {
      val (auth, authPassword) = users(user)
      if (authPassword != password) {
        Left(IOError("Invalid password."))
      } else {
        Right(auth)
      }
    } else {
      Left(IOError("Invalid user."))
    }

  def addUser(user: String, password: String)(implicit authentication: Authentication): Option[IOError] =
    if (!getUser.contains(VirtualUsersManager.ROOT)) {
      "Only root can add users.".ioErrorO
    } else if (users.contains(user)) {
      "User already added.".ioErrorO
    } else {
      users(user) = (Authentication(createId(), user), password)
      None
    }

  def getUser(implicit authentication: Authentication) =
    users.find { case (_, (auth, _)) => authentication.id == auth.id } match {
      case Some((user, (_, _))) => Some(user)
      case _ => None
    }

  override def userExists(user: String): Boolean = users.contains(user)

}
