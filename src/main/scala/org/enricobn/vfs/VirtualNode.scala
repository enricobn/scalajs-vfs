package org.enricobn.vfs

import scala.scalajs.js.annotation.JSExportAll

/**
  * Created by enrico on 12/2/16.
  */
@JSExportAll
trait VirtualNode extends Ordered[VirtualNode] {

  def name: String

  def group: String

  def parent: Option[VirtualFolder]

  def owner: String

  def permissions: VirtualPermissions

  def setExecutable(implicit authentication: Authentication) : Either[IOError, Unit]

  def setPermissions(permissions: VirtualPermissions)(implicit authentication: Authentication): Either[IOError, Unit]

  def chmod(value: Int)(implicit authentication: Authentication): Either[IOError, Unit]

  def chown(owner: String)(implicit authentication: Authentication): Either[IOError, Unit]

  def chgrp(group: String)(implicit authentication: Authentication): Either[IOError, Unit]

  def getCurrentUserPermission(implicit authentication: Authentication) : Either[IOError, VirtualPermission]

  lazy val path: String =
    if (parent.isDefined)
        if (parent.get.parent.isDefined)
          parent.get.path + VirtualFS.pathSeparator + name
        else
          VirtualFS.root + name
    else
      VirtualFS.root


  def canEqual(other: Any): Boolean = other.isInstanceOf[VirtualNode]

  override def equals(other: Any): Boolean = other match {
    case that: VirtualNode =>
      (that canEqual this) &&
        path == that.path
    case _ => false
  }

  override def hashCode(): Int = {
    val state = Seq(path)
    state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
  }

  override def toString: String = path

  //override def compare(x: VirtualNode, y: VirtualNode): Int = x.name.compareTo(y.name)

  override def compareTo(that: VirtualNode): Int = name.compareTo(that.name)
}
