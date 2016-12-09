package org.enricobn.vfs

/**
  * Created by enrico on 12/2/16.
  */
trait VirtualNode {
  def name: String

  def parent: VirtualFolder

  def owner: String

  def permissions: VirtualPermissions

  def executable: Boolean

  @throws[VirtualIOException]
  def executable_=(executable: Boolean) : Unit

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
