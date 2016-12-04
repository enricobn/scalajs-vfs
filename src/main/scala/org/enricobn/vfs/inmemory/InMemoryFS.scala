package org.enricobn.vfs.inmemory

import org.enricobn.vfs.{VirtualFS, VirtualFolder, VirtualUsersManager}

/**
  * Created by enrico on 12/3/16.
  */
class InMemoryFS(usersManager: VirtualUsersManager) extends VirtualFS {
  val root = new InMemoryFolder(usersManager, null, "/")

  def create(usersManager: VirtualUsersManager): VirtualFS = {
    require(usersManager != null)
    new InMemoryFS(usersManager)
  }

  def getRoot: VirtualFolder = root
}
