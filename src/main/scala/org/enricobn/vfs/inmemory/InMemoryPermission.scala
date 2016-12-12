package org.enricobn.vfs.inmemory

import org.enricobn.vfs.VirtualPermission

/**
  * Created by enrico on 12/2/16.
  */
final class InMemoryPermission extends VirtualPermission {
  private var _read : Boolean = true
  private var _write : Boolean = true
  private var _execute : Boolean = false

  def read: Boolean = _read

  def write: Boolean = _write

  def execute: Boolean = _execute

  // TODO check permissions?
  private[inmemory] def read_=(_read : Boolean) {
    this._read = _read
  }

  // TODO check permissions?
  private[inmemory] def write_=(_write : Boolean) {
    this._write = _write
  }

  // TODO check permissions?
  private[inmemory] def execute_=(_execute : Boolean) {
    this._execute = _execute
  }
}
