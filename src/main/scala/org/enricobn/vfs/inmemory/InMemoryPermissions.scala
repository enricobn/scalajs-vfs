package org.enricobn.vfs.inmemory

import org.enricobn.vfs.{VirtualPermission, VirtualPermissions}

/**
  * Created by enrico on 12/2/16.
  */
class InMemoryPermissions extends VirtualPermissions {
  private var owner_ : InMemoryPermission = new InMemoryPermission
  private var group_ : InMemoryPermission = new InMemoryPermission
  private var others_ : InMemoryPermission = new InMemoryPermission

  others_.setExecute(true)
  others_.setRead(true)
  others_.setWrite(false)

  def owner: VirtualPermission = owner_

  def group: VirtualPermission = group_

  def others: VirtualPermission = others_

  private[inmemory] def setOwner(owner_ : InMemoryPermission) {
    this.owner_ = owner_
  }

  private[inmemory] def setGroup(group_ : InMemoryPermission) {
    this.group_ = group_
  }

  private[inmemory] def setOthers(others_ : InMemoryPermission) {
    this.others_ = others_
  }
}
