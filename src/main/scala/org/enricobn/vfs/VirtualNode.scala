package org.enricobn.vfs

import scala.scalajs.js.annotation.JSExportAll

/**
  * Created by enrico on 12/2/16.
  */
@JSExportAll
trait VirtualNode {
  def name: String

  def parent: VirtualFolder

  def owner: String

  def permissions: VirtualPermissions

  def setExecutable() : Unit

  @throws[VirtualIOException]
  def setPermissions(permissions: VirtualPermissions) : Unit

  @throws[VirtualIOException]
  def chmod(value: Int) : Unit

//  @throws[VirtualIOException]
//  def executable_=(executable: Boolean) : Unit

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

  def canEqual(other: Any): Boolean = other.isInstanceOf[VirtualNode]

  def getCurrentUserPermission : VirtualPermission

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
