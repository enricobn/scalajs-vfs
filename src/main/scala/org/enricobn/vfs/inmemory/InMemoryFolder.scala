package org.enricobn.vfs.inmemory

import org.enricobn.vfs._

/**
  * Created by enrico on 12/2/16.
  */
class InMemoryFolder private[inmemory] (usersManager: VirtualUsersManager, parent: VirtualFolder, name: String)
extends InMemoryNode(usersManager, parent, name)
with VirtualFolder {
  private val _files = new scala.collection.mutable.HashSet[VirtualFile]
  private val _folders = new scala.collection.mutable.HashSet[InMemoryFolder]

  setExecutable()

  @throws[VirtualIOException]
  def folders: Set[VirtualFolder] = {
    try {
      usersManager.checkExecuteAccess(this)
    } catch {
      case e: VirtualSecurityException =>
        throw new VirtualIOException(e.getMessage, e)
    }
    _folders.toSet
  }

  @throws[VirtualIOException]
  def files: Set[VirtualFile] = {
    try {
      usersManager.checkExecuteAccess(this)
    }
    catch {
      case e: VirtualSecurityException =>
        throw new VirtualIOException(e.getMessage, e)
    }
    _files.toSet
  }

  @throws[VirtualIOException]
  def mkdir(name: String): VirtualFolder = {
    if (folders.exists(folder => folder.name == name) || files.exists(file => file.name == name)) {
      throw new VirtualIOException("mkdir: cannot create directory ‘" + name + "’: File exists")
    }
    val folder: InMemoryFolder = new InMemoryFolder(usersManager, this, name)
    _folders.add(folder)
    folder
  }

  @throws[VirtualIOException]
  def deleteFile(name: String) {
    try {
      usersManager.checkWriteAccess(this)
    }
    catch {
      case e: VirtualSecurityException =>
        throw new VirtualIOException("deleteFile: " + e.getMessage, e)
    }
    val file = files.find(_.name == name)
    if (file.isDefined)
      _files.remove(file.get)
    else
      throw new VirtualIOException("No such file")
  }

  def deleteFolder(name: String) {
    try {
      usersManager.checkWriteAccess(this)
    }
    catch {
      case e: VirtualSecurityException =>
        throw new VirtualIOException("deleteFolder: " + e.getMessage, e)
    }
    val folder = _folders.find(_.name == name)
    if (folder.isDefined)
      _folders.remove(folder.get)
    else
      throw new VirtualIOException("No such file")
  }

  @throws[VirtualIOException]
  def touch(name: String): VirtualFile = {
    checkCreate(name)
    val file: InMemoryFile = new InMemoryFile(usersManager, this, name)
    _files.add(file)
    file
  }

  @throws[VirtualIOException]
  def createExecutableFile(name: String, run_ : VirtualFileRun): VirtualFile = {
    checkCreate(name)
    val file: InMemoryFile = new InMemoryFile(usersManager, this, name) {
      @throws[VirtualIOException]
      override def internalRun(args: String*) {
        run_.run(args: _*)
      }
    }

    file.setExecutable()
    file.content = "[byte]"

    _files += file
    file
  }

  @throws[VirtualIOException]
  def createDynamicFile(name: String, contentSupplier: () => AnyRef): VirtualFile = {
    checkCreate(name)
    val file: InMemoryFile = new InMemoryFile(usersManager, this, name)

    file.content = contentSupplier.apply()

    _files.add(file)
    file
  }

  @throws[VirtualIOException]
  private def checkCreate(name: String) {
    try {
      usersManager.checkWriteAccess(this)
    }
    catch {
      case e: VirtualSecurityException =>
        throw new VirtualIOException(e.getMessage, e)
    }
    if (folders.exists(folder => folder.name == name) || files.exists(file => file.name == name)) {
      throw new VirtualIOException("touch: cannot create file ‘" + name + "’: File exists")
    }
  }

  @throws[VirtualIOException]
  def rename(name: String) {
    throw new UnsupportedOperationException
  }
}
