package org.enricobn.vfs.inmemory

import org.enricobn.vfs.IOError._
import org.enricobn.vfs._

import scala.collection.immutable.BitSet

/**
  * Created by enrico on 12/2/16.
  */
object InMemoryNode {
  def fromOctal(value: Int): Int = {
    if (value == 0)
      0
    else {
      val v = value % 10
      v + 8 * fromOctal(value / 10)
    }
  }
}

abstract class InMemoryNode private[inmemory](val vum: VirtualUsersManager, val vsm: VirtualSecurityManager,
                                              val parent: Option[VirtualFolder], val name: String, val initialOwner: String,
                                              val initialGroup: String)
  extends VirtualNode {

  import InMemoryNode._

  private var _owner: String = initialOwner
  private var _group: String = initialGroup
  private val _permissions: InMemoryPermissions = initialPermissions

  final def owner: String = _owner

  override def group: String = _group

  final def permissions: VirtualPermissions = _permissions

  def initialPermissions: InMemoryPermissions = new InMemoryPermissions

  final def setExecutable(implicit authentication: Authentication): Either[IOError, Unit] = {
    if (!vsm.checkWriteAccess(this)) {
      accessDenied("set executable")
    } else {
      _permissions.owner.execute = true
      _permissions.group.execute = true
      _permissions.others.execute = true
      Right(())
    }
  }

  final def setPermissions(permissions: VirtualPermissions)(implicit authentication: Authentication): Either[IOError, Unit] = {
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
      Right(())
    }
  }

  final def chmod(value: Int)(implicit authentication: Authentication): Either[IOError, Unit] = {
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
      Right(())
    }
  }

  override def chown(user: String)(implicit authentication: Authentication): Either[IOError, Unit] =
    if (!vsm.checkWriteAccess(this)) {
      accessDenied("chown")
    } else if (!vum.userExists(user)) {
      s"chown of $this : user '$user' not defined.".ioErrorE
    } else {
      _owner = user
      Right(())
    }
  override def chgrp(group: String)(implicit authentication: Authentication): Either[IOError, Unit] =
    if (!vsm.checkWriteAccess(this)) {
      accessDenied("chgrp")
      // TODO
    //} else if (!vum.userExists(user)) {
    //  s"chown of $this : user '$user' not defined.".ioErrorO
    } else {
      _group = group
      Right(())
    }

  final override def getCurrentUserPermission(implicit authentication: Authentication): Either[IOError, VirtualPermission] = {
    // TODO group
    vum.getUser(authentication) match {
      case Some(user) =>
        if (user == owner) {
          Right(_permissions.owner)
        } else {
          Right(_permissions.others)
        }
      case _ => "Authentication error.".ioErrorE
    }
  }

  protected def accessDenied[T](prefix: String): Left[IOError, T] = {
    s"$prefix of $this : access denied.".ioErrorE
  }

}
