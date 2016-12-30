package org.enricobn.vfs.inmemory

import org.enricobn.vfs._
import IOError._

/**
  * Created by enrico on 12/2/16.
  */
class InMemoryFolder private[inmemory] (usersManager: VirtualUsersManager, parent: VirtualFolder, name: String)
extends InMemoryNode(usersManager, parent, name)
with VirtualFolder {
  private val _files = new scala.collection.mutable.HashSet[VirtualFile]
  private val _folders = new scala.collection.mutable.HashSet[InMemoryFolder]

  setExecutable() match {
    case Some(error) => throw new IllegalStateException(error.message)
    case _ =>
  }

  private def checkExecuteAccess() : Option[IOError] =
    if (!usersManager.checkExecuteAccess(this)) {
      "Access denied.".ioErrorO
    } else {
      None
    }

  private def checkWriteAccess() : Option[IOError] =
    if (!usersManager.checkWriteAccess(this)) {
      "Access denied.".ioErrorO
    } else {
      None
    }

  def folders = checkExecuteAccess().toLeft(_folders.toSet)

  def files = checkExecuteAccess().toLeft(_files.toSet)

  def mkdir(name: String) = {
    checkWriteAccess()
      .orElse(checkExecuteAccess())
      .orElse({
        if (_folders.exists(folder => folder.name == name) || _files.exists(file => file.name == name)) {
          ("mkdir: cannot create directory ‘" + name + "’: File exists").ioErrorO
        } else {
          None
        }
      })
      .toLeft({
        val folder = new InMemoryFolder(usersManager, this, name)
        _folders.add(folder)
        folder
      })
  }

  def deleteFile(name: String) = {
    checkWriteAccess()
      .orElse(checkExecuteAccess())
      .toLeft(
        _files
          .find(_.name == name)
          .map(file => _files.remove(file))
      )
      .right
      .flatMap(_.toRight(IOError("No such file.")))
  }

  def deleteFolder(name: String) = {
    checkWriteAccess()
      .orElse(checkExecuteAccess())
      .toLeft(
        _folders
          .find(_.name == name)
          .map(file => _folders.remove(file))
      )
      .right
      .flatMap(_.toRight(IOError("No such file.")))
  }

  def touch(name: String) = {
    checkCreate(name) match {
      case Some(error) => error.message.ioErrorE
      case _ =>
        val file: InMemoryFile = new InMemoryFile (usersManager, this, name)
        _files.add (file)
        Right(file)
    }
  }

  def rename(name: String) = {
    "Unsupported operation".ioErrorO
  }

  private def checkCreate(name: String) : Option[IOError] = {
    if (!usersManager.checkWriteAccess(this)) {
      "Access denied.".ioErrorO
    } else if (!usersManager.checkExecuteAccess(this)) {
      "Access denied.".ioErrorO
    } else if (_folders.exists(folder => folder.name == name) || _files.exists(file => file.name == name)) {
      ("touch: cannot create file ‘" + name + "’: File exists").ioErrorO
    } else {
      None
    }
  }
}
