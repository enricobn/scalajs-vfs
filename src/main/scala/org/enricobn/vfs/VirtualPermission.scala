package org.enricobn.vfs

/**
  * Created by enrico on 12/2/16.
  */
object VirtualPermission {
  val EXEC_PERMISSION: VirtualPermission = new VirtualPermission() {
    def read: Boolean = true

    def write: Boolean = false

    def execute: Boolean = true
  }

  val READ_PERMISSION: VirtualPermission = new VirtualPermission() {
    def read: Boolean = true

    def write: Boolean = false

    def execute: Boolean = false
  }
}

trait VirtualPermission {

  def read: Boolean

  def write: Boolean

  def execute: Boolean
}
