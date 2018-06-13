package org.enricobn.vfs

/**
  * Created by enrico on 12/2/16.
  */
object VirtualUsersManager {
  val ROOT: String = "root"
}

case class Authentication(id: String, user: String)

trait VirtualUsersManager {

  def logUser(user: String, password: String) : Either[IOError, Authentication]

  def logRoot(password: String): Either[IOError, Authentication] = logUser(VirtualUsersManager.ROOT, password)

  def addUser(user: String, password: String)(implicit authentication: Authentication): Option[IOError]

  def userExists(user: String) : Boolean

  def getUser(implicit authentication: Authentication) : Option[String]

}
