package org.enricobn.vfs

import scala.scalajs.js.annotation.JSExportAll

import IOError._

/**
  * Created by enrico on 12/2/16.
  */
@JSExportAll
trait VirtualNode {
  def name: String

  def parent: VirtualFolder

  def owner: String

  def permissions: VirtualPermissions

  def setExecutable() : Either[IOError, Unit]

  def setPermissions(permissions: VirtualPermissions) : Either[IOError, Unit]

  def chmod(value: Int) : Either[IOError, Unit]

  def path: String = {
    if (parent != null) {
      if (parent.parent != null) {
        return parent.path + "/" + name
      }
      else {
        return "/" + name
      }
    }
    "/"
  }

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
}
