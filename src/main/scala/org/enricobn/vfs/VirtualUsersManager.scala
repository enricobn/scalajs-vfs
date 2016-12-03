package org.enricobn.vfs

/**
  * Created by enrico on 12/2/16.
  */
object VirtualUsersManager {
  val ROOT: String = "root"
}

trait VirtualUsersManager {
  @throws[VirtualSecurityException]
  def checkReadAccess(node: VirtualNode)

  @throws[VirtualSecurityException]
  def checkExecuteAccess(node: VirtualNode)

  @throws[VirtualSecurityException]
  def logUser(user: String, password: String)

  @throws[VirtualSecurityException]
  def logRoot(password: String)

  @throws[VirtualSecurityException]
  def addUser(user: String, password: String)

  def getCurrentUser: String

  @throws[VirtualSecurityException]
  def checkWriteAccess(node: VirtualNode)
}
