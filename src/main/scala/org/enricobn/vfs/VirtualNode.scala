package org.enricobn.vfs

/**
  * Created by enrico on 12/2/16.
  */
trait VirtualNode {
  def getName: String

  def getParent: VirtualFolder

  def getOwner: String

  def getPermissions: VirtualPermissions

  def isExecutable: Boolean

  @throws[VirtualIOException]
  def setExecutable(b: Boolean)

  def getPath: String = {
    if (getParent != null) {
      if (getParent.getParent != null) {
        return getParent.getPath + "/" + getName
      }
      else {
        return "/" + getName
      }
    }
    "/"
  }

  def canEqual(other: Any): Boolean = other.isInstanceOf[VirtualNode]

  override def equals(other: Any): Boolean = other match {
    case that: VirtualNode =>
      (that canEqual this) &&
        getPath == that.getPath
    case _ => false
  }

  override def hashCode(): Int = {
    val state = Seq(getPath)
    state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
  }
}
