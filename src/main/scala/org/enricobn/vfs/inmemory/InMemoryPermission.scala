package org.enricobn.vfs.inmemory

import org.enricobn.vfs.VirtualPermission

/**
  * Created by enrico on 12/2/16.
  */
class InMemoryPermission extends VirtualPermission {
  private var read_ : Boolean = true
  private var write_ : Boolean = true
  private var execute_ : Boolean = true

  def read: Boolean = read_

  def write: Boolean = write_

  def execute: Boolean = execute_

  private[inmemory] def setRead(read_ : Boolean) {
    this.read_ = read_
  }

  private[inmemory] def setWrite(write_ : Boolean) {
    this.write_ = write_
  }

  private[inmemory] def setExecute(execute_ : Boolean) {
    this.execute_ = execute_
  }
}
