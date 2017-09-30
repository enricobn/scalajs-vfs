package org.enricobn.vfs

import scala.scalajs.js.annotation.JSExportAll

import IOError._

/**
  * Created by enrico on 12/2/16.
  */
@JSExportAll
trait VirtualNode {
  def name: String

  def parent: Option[VirtualFolder]

  def owner: String

  def permissions: VirtualPermissions

  def setExecutable() : Option[IOError]

  def setPermissions(permissions: VirtualPermissions) : Option[IOError]

  def chmod(value: Int) : Option[IOError]

  def chown(owner: String) : Option[IOError]

  lazy val path: String =
    if (parent.isDefined)
        if (parent.get.parent.isDefined)
          parent.get.path + VirtualFS.pathSeparator + name
        else
          VirtualFS.root + name
    else
      VirtualFS.root


  def getCurrentUserPermission : VirtualPermission

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

}
