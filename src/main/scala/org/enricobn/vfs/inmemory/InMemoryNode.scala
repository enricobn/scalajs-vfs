package org.enricobn.vfs.inmemory

import org.enricobn.vfs._

/**
  * Created by enrico on 12/2/16.
  */
class InMemoryNode(usersManager: VirtualUsersManager, parent: InMemoryFolder, private val name: String)
extends VirtualNode {
  private val owner: String = null
  private val permissions: InMemoryPermissions = new InMemoryPermissions
  private var executable: Boolean = false

  def getName: String = name

  def getParent: VirtualFolder = parent

  def getOwner: String = owner

  def getPermissions: VirtualPermissions = permissions

  def isExecutable: Boolean = executable

  @throws[VirtualIOException]
  def setExecutable(executable: Boolean) {
    try {
      usersManager.checkWriteAccess(this)
    }
    catch {
      case e: VirtualSecurityException =>
        throw new VirtualIOException(e.getMessage, e)
    }
    this.executable = executable
  }

  private[inmemory] def getInMemoryParent: InMemoryFolder = parent

  @throws[VirtualSecurityException]
  private[inmemory] def checkWriteAccess(node: VirtualNode) {
    usersManager.checkWriteAccess(node)
  }

  private[inmemory] def getUsersManager: VirtualUsersManager = usersManager

  def canEqual(other: Any): Boolean = other.isInstanceOf[InMemoryNode]

  override def equals(other: Any): Boolean = other match {
    case that: InMemoryNode =>
      (that canEqual this) &&
        name == that.name
    case _ => false
  }

  override def hashCode(): Int = {
    val state = Seq(name)
    state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
  }
}
