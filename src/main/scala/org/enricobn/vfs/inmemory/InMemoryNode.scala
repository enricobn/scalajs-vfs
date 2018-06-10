package org.enricobn.vfs.inmemory

import org.enricobn.vfs.IOError._
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
class InMemoryNode private[inmemory] (val vum: VirtualUsersManager, val vsm: VirtualSecurityManager,
                                      val parent: Option[VirtualFolder], val name: String)
extends VirtualNode {
  import InMemoryNode._

  private var _owner: String = vum.currentUser
  private val _permissions: InMemoryPermissions = new InMemoryPermissions

  final def owner: String = _owner

  final def permissions: InMemoryPermissions = _permissions

  if (vum.currentUser != VirtualUsersManager.ROOT && parent.isDefined) {
    // TODO it does nothing: it does not throw an exception, it returns a boolean!!!
    vsm.checkWriteAccess(parent.get)
  }

  final def setExecutable(): Option[IOError] = {
    if (!vsm.checkWriteAccess(this)) {
      accessDenied("set executable")
    } else {
      _permissions.owner.execute = true
      _permissions.group.execute = true
      _permissions.others.execute = true
      None
    }
  }

  final def setPermissions(permissions: VirtualPermissions): Option[IOError] = {
    if (!vsm.checkWriteAccess(this)) {
      accessDenied("set permissions")
    } else {
      _permissions.owner.read = permissions.owner.read
      _permissions.owner.write = permissions.owner.write
      _permissions.owner.execute = permissions.owner.execute
      _permissions.group.read = permissions.group.read
      _permissions.group.write = permissions.group.write
      _permissions.group.execute = permissions.group.execute
      _permissions.others.read = permissions.others.read
      _permissions.others.write = permissions.others.write
      _permissions.others.execute = permissions.others.execute
      None
    }
  }

  final def chmod(value: Int): Option[IOError] = {
    if (!vsm.checkWriteAccess(this)) {
      accessDenied("chmod")
    } else {
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
      None
    }
  }

  override def chown(user: String): Option[IOError] =
    if (!vsm.checkWriteAccess(this)) {
      accessDenied("chown")
    } else if (!vum.userExists(user)) {
      s"chown of $this : user '$user' not defined.".ioErrorO
    } else {
      _owner = user
      None
    }

  final override def getCurrentUserPermission: VirtualPermission = {
    // TODO group
    if (vum.currentUser == owner) {
      _permissions.owner
    } else {
      _permissions.others
    }
  }

  protected def accessDenied(prefix: String): Some[IOError] = {
    Some(IOError(s"$prefix of $this for user ${vum.currentUser}: access denied."))
  }

}
