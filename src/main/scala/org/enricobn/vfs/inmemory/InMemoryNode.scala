package org.enricobn.vfs.inmemory

import org.enricobn.vfs._

import scala.collection.immutable.BitSet

/**
  * Created by enrico on 12/2/16.
  */
object InMemoryNode {
  def fromOctal(value: Int) : Int = {
    if (value == 0)
      0
    else {
      val v = value % 10
      v + 8 * fromOctal(value / 10)
    }
  }
}
class InMemoryNode private[inmemory] (val usersManager: VirtualUsersManager, val parent: VirtualFolder, val name: String)
extends VirtualNode {
  import InMemoryNode._
  final val owner: String = usersManager.currentUser
  private val _permissions: InMemoryPermissions = new InMemoryPermissions

  final def permissions: InMemoryPermissions = _permissions

  if (usersManager.currentUser != VirtualUsersManager.ROOT) {
    usersManager.checkWriteAccess(parent)
  }

  @throws[VirtualIOException]
  final def setExecutable() {
    checkWriteAccess()
    _permissions.owner.execute = true
    _permissions.group.execute = true
    _permissions.others.execute = true
  }

  @throws[VirtualIOException]
  final def setPermissions(permissions: VirtualPermissions) {
    checkWriteAccess()
    _permissions.owner.read = permissions.owner.read
    _permissions.owner.write = permissions.owner.write
    _permissions.owner.execute = permissions.owner.execute
    _permissions.group.read = permissions.group.read
    _permissions.group.write = permissions.group.write
    _permissions.group.execute = permissions.group.execute
    _permissions.others.read = permissions.others.read
    _permissions.others.write = permissions.others.write
    _permissions.others.execute = permissions.others.execute
  }

  @throws[VirtualIOException]
  final def chmod(value: Int) {
    checkWriteAccess()
    val mask = BitSet.fromBitMask(Array(fromOctal(value)))
    _permissions.owner.read = mask(8)
    _permissions.owner.write = mask(7)
    _permissions.owner.execute = mask(6)
    _permissions.group.read = mask(5)
    _permissions.group.write = mask(4)
    _permissions.group.execute = mask(3)
    _permissions.others.read = mask(2)
    _permissions.others.write = mask(1)
    _permissions.others.execute = mask(0)
  }

//  private[inmemory] def getUsersManager: VirtualUsersManager = usersManager

  final override def getCurrentUserPermission: VirtualPermission = {
    // TODO group
    if (usersManager.currentUser == owner) {
      _permissions.owner
    } else {
      _permissions.others
    }
  }

  private final def checkWriteAccess() {
    try {
      usersManager.checkWriteAccess(this)
    }
    catch {
      case e: VirtualSecurityException =>
        throw new VirtualIOException(e.getMessage, e)
    }
  }
}
