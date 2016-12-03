package org.enricobn.vfs.inmemory

import java.util.Objects

import org.enricobn.vfs.{VirtualFS, VirtualFolder, VirtualUsersManager}

/**
  * Created by enrico on 12/3/16.
  */
class InMemoryFS(usersManager: VirtualUsersManager) extends VirtualFS {
  val root = new InMemoryFolder(usersManager, null, "/")

  def create(usersManager: VirtualUsersManager): VirtualFS = {
    Objects.nonNull(usersManager)
    new InMemoryFS(usersManager)
  }

  def getRoot: VirtualFolder = root
}
