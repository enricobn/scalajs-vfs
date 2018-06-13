package org.enricobn.vfs

/**
  * Created by enrico on 12/2/16.
  */
object VirtualPermission {
  val EXEC: VirtualPermission = new VirtualPermission() {
    def read: Boolean = true

    def write: Boolean = false

    def execute: Boolean = true
  }

  val READ: VirtualPermission = new VirtualPermission() {
    def read: Boolean = true

    def write: Boolean = false

    def execute: Boolean = false
  }

  val NONE: VirtualPermission = new VirtualPermission() {
    def read: Boolean = false

    def write: Boolean = false

    def execute: Boolean = false
  }

  val WRITE: VirtualPermission = new VirtualPermission() {
    def read: Boolean = true

    def write: Boolean = true

    def execute: Boolean = false
  }

}

trait VirtualPermission {

  def read: Boolean

  def write: Boolean

  def execute: Boolean

  def octal: Int = {
    def toInt(b: Boolean): Int = if (b) 1 else 0

    toInt(execute) + 2 * toInt(write) + 4 * toInt(read)
  }

}
