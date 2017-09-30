package org.enricobn.vfs.inmemory

import org.enricobn.vfs._
import IOError._

/**
  * Created by enrico on 12/2/16.
  */
class InMemoryFolder private[inmemory] (usersManager: VirtualUsersManager, parent: Option[VirtualFolder], name: String)
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

  def folders: Either[IOError, Set[VirtualFolder]] = checkExecuteAccess().toLeft(_folders.toSet)

  def files: Either[IOError, Set[VirtualFile]] = checkExecuteAccess().toLeft(_files.toSet)

  def mkdir(name: String): Either[IOError, InMemoryFolder] = {
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
        val folder = new InMemoryFolder(usersManager, Some(this), name)
        _folders.add(folder)
        folder
      })
  }

  def deleteFile(name: String): Option[IOError] = {
    checkWriteAccess()
      .orElse(findFile(name).right.map {
          case Some(file) => _files.remove(file.asInstanceOf[InMemoryFile])
          case _ => IOError("No such file.")
        }.left.toOption
      )
  }

  def deleteFolder(name: String): Option[IOError] = {
    checkWriteAccess()
        .orElse(findFolder(name).right.map {
          case Some(folder) => _folders.remove(folder.asInstanceOf[InMemoryFolder])
          case _ => IOError("No such directory.")
        }.left.toOption
    )
  }

  def touch(name: String): Either[IOError, InMemoryFile] = {
    checkCreate(name) match {
      case Some(error) => error.message.ioErrorE
      case _ =>
        val file: InMemoryFile = new InMemoryFile (usersManager, Some(this), name)
        _files.add (file)
        Right(file)
    }
  }

  def rename(name: String): Some[IOError] = {
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
