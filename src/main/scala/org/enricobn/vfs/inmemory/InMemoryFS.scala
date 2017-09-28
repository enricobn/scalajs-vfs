package org.enricobn.vfs.inmemory

import org.enricobn.vfs.{RootFragment, VirtualFS, VirtualPath, VirtualUsersManager}

import scala.scalajs.js.annotation.{JSExport, JSExportAll}

/**
  * Created by enrico on 12/3/16.
  */

@JSExport(name = "InMemoryFS")
@JSExportAll
class InMemoryFS(usersManager: VirtualUsersManager) extends VirtualFS {

  val root = new InMemoryFolder(usersManager, null, VirtualFS.rootPath.path)

}
