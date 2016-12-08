package org.enricobn.vfs.inmemory

import org.enricobn.vfs._

/**
  * Created by enrico on 12/2/16.
  */
class InMemoryFolder(usersManager: VirtualUsersManager, parent: VirtualFolder, name: String)
extends InMemoryNode(usersManager, parent, name)
with VirtualFolder {
  private val files = new scala.collection.mutable.HashSet[VirtualFile]
  private val folders = new scala.collection.mutable.HashSet[InMemoryFolder]

  @throws[VirtualIOException]
  def getFolders: Set[VirtualFolder] = {
    try {
      getUsersManager.checkExecuteAccess(this)
    } catch {
      case e: VirtualSecurityException =>
        throw new VirtualIOException(e.getMessage, e)
    }
    folders.toSet
  }

  @throws[VirtualIOException]
  def getFiles: Set[VirtualFile] = {
    try {
      getUsersManager.checkExecuteAccess(this)
    }
    catch {
      case e: VirtualSecurityException =>
        throw new VirtualIOException(e.getMessage, e)
    }
    files.toSet
  }

  @throws[VirtualIOException]
  def mkdir(name: String): VirtualFolder = {
    if (getFolders.exists(folder => folder.getName == name) || getFiles.exists(file => file.getName == name)) {
      throw new VirtualIOException("mkdir: cannot create directory ‘" + name + "’: File exists")
    }
    val folder: InMemoryFolder = new InMemoryFolder(getUsersManager, this, name)
    folders.add(folder)
    folder
  }

  @throws[VirtualIOException]
  def deleteFile(name: String) {
    try {
      checkWriteAccess(this)
    }
    catch {
      case e: VirtualSecurityException =>
        throw new VirtualIOException("deleteFile: " + e.getMessage, e)
    }
    val file = files.find(_.getName == name)
    if (file.isDefined)
      files.remove(file.get)
    else
      throw new VirtualIOException("No such file")
  }

  def deleteFolder(name: String) {
    try {
      checkWriteAccess(this)
    }
    catch {
      case e: VirtualSecurityException =>
        throw new VirtualIOException("deleteFolder: " + e.getMessage, e)
    }
    val folder = folders.find(_.getName == name)
    if (folder.isDefined)
      folders.remove(folder.get)
    else
      throw new VirtualIOException("No such file")
  }

  @throws[VirtualIOException]
  def touch(name: String): VirtualFile = {
    checkCreate(name)
    val file: InMemoryFile = new InMemoryFile(getUsersManager, this, name)
    files.add(file)
    file
  }

  @throws[VirtualIOException]
  def createExecutableFile(name: String, run_ : VirtualFileRun): VirtualFile = {
    checkCreate(name)
    val file: InMemoryFile = new InMemoryFile(getUsersManager, this, name) {
      @throws[VirtualIOException]
      override def getContent: AnyRef = "[byte]"

      @throws[VirtualIOException]
      override def setContent(content: AnyRef) {
        throw new VirtualIOException("access denied")
      }

      @throws[VirtualIOException]
      override def run(args: String*) {
        run_.run(args: _*)
      }

      override
      def getPermissions: VirtualPermissions = VirtualPermissions.EXEC_PERMISSIONS

      override
      def isExecutable: Boolean = true

      @throws[VirtualIOException]
      override def setExecutable(b: Boolean) {
        throw new VirtualIOException("access denied")
      }
    }
    files += file
    file
  }

  @throws[VirtualIOException]
  def createDynamicFile(name: String, content: () => AnyRef): VirtualFile = {
    checkCreate(name)
    val file: InMemoryFile = new InMemoryFile(getUsersManager, this, name) {

      @throws[VirtualIOException]
      override def getContent: AnyRef = content.apply()

      @throws[VirtualIOException]
      override def setContent(content: AnyRef) {
        throw new VirtualIOException("access denied")
      }

      override
      def getPermissions: VirtualPermissions = {
        VirtualPermissions.READ_PERMISSIONS
      }

      override
      def isExecutable: Boolean = false

      @throws[VirtualIOException]
      override def setExecutable(b: Boolean) {
        throw new VirtualIOException("access denied")
      }
    }
    files.add(file)
    file
  }

  @throws[VirtualIOException]
  private def checkCreate(name: String) {
    try {
      checkWriteAccess(this)
    }
    catch {
      case e: VirtualSecurityException =>
        throw new VirtualIOException(e.getMessage, e)
    }
    if (getFolders.exists(folder => folder.getName == name) || getFiles.exists(file => file.getName == name)) {
      throw new VirtualIOException("touch: cannot create file ‘" + name + "’: File exists")
    }
  }

  @throws[VirtualIOException]
  def rename(name: String) {
    throw new UnsupportedOperationException
  }
}
