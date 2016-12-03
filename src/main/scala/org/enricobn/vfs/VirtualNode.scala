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
}
