package org.enricobn.vfs

/**
  * Created by enrico on 12/2/16.
  */
object VirtualUsersManager {
  val ROOT: String = "root"
}

trait VirtualUsersManager {

  def logUser(user: String, password: String) : Option[IOError]

  def logRoot(password: String) : Option[IOError]

  def addUser(user: String, password: String) : Option[IOError]

  def currentUser: String

  def userExists(user: String) : Boolean

}
