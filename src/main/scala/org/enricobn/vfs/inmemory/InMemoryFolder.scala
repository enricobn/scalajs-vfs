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
    case Left(error) => throw new IllegalStateException(error.message)
    case _ =>
  }

  private def checkExecuteAccessE() : Either[IOError, Unit] =
    if (!usersManager.checkExecuteAccess(this)) {
      "Access denied.".ioErrorE
    } else {
      Right(())
    }

  private def checkWriteAccessE() : Either[IOError, Unit] =
    if (!usersManager.checkWriteAccess(this)) {
      "Access denied.".ioErrorE
    } else {
      Right(())
    }

  def folders = checkExecuteAccessE().right.map(_ =>_folders.toSet)

  def files = checkExecuteAccessE().right.map(_ => _files.toSet)

  def mkdir(name: String) = {
    for {
      _ <- checkExecuteAccessE().right
      _ <- checkWriteAccessE().right
      folder <- if (_folders.exists(folder => folder.name == name) || _files.exists(file => file.name == name)) {
                  ("mkdir: cannot create directory ‘" + name + "’: File exists").ioErrorE.right
                } else {
                  Right(new InMemoryFolder(usersManager, this, name)).right
                }
    } yield {
      _folders.add(folder)
      folder
    }
  }

  def deleteFile(name: String) = {
    for {
      _ <- checkExecuteAccessE().right
      _ <- checkWriteAccessE().right
      deleted <- _files.find(_.name == name)
              .map(file => _files.remove(file))
              .toRight(IOError("No such file"))
              .right
    } yield deleted
  }

  def deleteFolder(name: String) = {
    for {
      _ <- checkExecuteAccessE().right
      _ <- checkWriteAccessE().right
      deleted <- _folders.find(_.name == name)
        .map(file => _folders.remove(file))
        .toRight(IOError("No such file"))
        .right
    } yield deleted
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
    "Unsupported operation".ioErrorE
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
