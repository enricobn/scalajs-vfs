package org.enricobn.vfs.inmemory

import org.enricobn.vfs._

import scala.scalajs.js.annotation.{JSExport, JSExportAll}

/**
  * Created by enrico on 12/3/16.
  */

@JSExport(name = "InMemoryFS")
@JSExportAll
class InMemoryFS(vum: VirtualUsersManager, vsm: VirtualSecurityManager) extends VirtualFS {

  val root = new InMemoryFolder(vum, vsm, None, VirtualFS.rootPath.path)

}
