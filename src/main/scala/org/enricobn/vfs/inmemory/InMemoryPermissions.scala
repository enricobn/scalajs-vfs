package org.enricobn.vfs.inmemory

import org.enricobn.vfs.VirtualPermissions

/**
  * Created by enrico on 12/2/16.
  */
final class InMemoryPermissions extends VirtualPermissions {
  val owner : InMemoryPermission = new InMemoryPermission
  val group : InMemoryPermission = new InMemoryPermission
  val others : InMemoryPermission = new InMemoryPermission

  others.write = false
}
