package org.enricobn.vfs.inmemory

import org.enricobn.vfs.IOError._
import org.enricobn.vfs._

object InMemoryFolder {

  def root(vum: VirtualUsersManager, vsm: VirtualSecurityManager, fsINotify: VirtualFSNotifier) =
    new InMemoryFolder(vum, vsm, fsINotify, None, VirtualFS.root, VirtualUsersManager.ROOT)

}

/**
  * Created by enrico on 12/2/16.
  *
  * The only way to create a folder is to use InMemoryFolder.root or using mkdir on an already created folder.
  */
class InMemoryFolder private (vum: VirtualUsersManager, vsm: VirtualSecurityManager,
                              fsINotify: VirtualFSNotifier,
                              parent: Option[VirtualFolder], name: String, owner: String)
extends InMemoryNode(vum, vsm, parent, name, owner)
with VirtualFolder {
  private val _files = new scala.collection.mutable.HashSet[VirtualFile]
  private val _folders = new scala.collection.mutable.HashSet[InMemoryFolder]

  override def initialPermissions: InMemoryPermissions = {
    val permissions = new InMemoryPermissions
    permissions.owner.execute = true
    permissions.group.execute = true
    permissions.others.execute = true
    permissions
  }

  private def checkExecuteAccess(implicit authentication: Authentication) : Option[IOError] =
    if (!vsm.checkExecuteAccess(this)) {
      accessDenied("check execute access")
    } else {
      None
    }

  private def checkWriteAccess(implicit authentication: Authentication) : Option[IOError] =
    if (!vsm.checkWriteAccess(this)) {
     accessDenied("check write access")
    } else {
      None
    }

  def folders(implicit authentication: Authentication) : Either[IOError, Set[VirtualFolder]] =
    checkExecuteAccess.toLeft(_folders.toSet)

  def files(implicit authentication: Authentication) : Either[IOError, Set[VirtualFile]] =
    checkExecuteAccess.toLeft(_files.toSet)

  def mkdir(name: String)(implicit authentication: Authentication): Either[IOError, InMemoryFolder] = {
    checkWriteAccess
      .orElse(checkExecuteAccess)
      .orElse({
        if (_folders.exists(folder => folder.name == name) || _files.exists(file => file.name == name)) {
          ("mkdir: cannot create directory ‘" + name + "’: File exists").ioErrorO
        } else {
          None
        }
      })
      .toLeft({
        // TODO handle error
        val user = vum.getUser.get
        val folder = new InMemoryFolder(vum, vsm, fsINotify, Some(this), name, user)
        _folders.add(folder)
        fsINotify.notify(this)
        folder
      })
  }

  def deleteFile(name: String)(implicit authentication: Authentication): Option[IOError] = {
    checkWriteAccess
      .orElse(findFile(name).right.map {
          case Some(file) => _files.remove(file.asInstanceOf[InMemoryFile])
          case _ => IOError("No such file.")
        }.left.toOption
      )
  }

  def deleteFolder(name: String)(implicit authentication: Authentication): Option[IOError] = {
    checkWriteAccess(authentication)
        .orElse(findFolder(name).right.map {
          case Some(folder) => _folders.remove(folder.asInstanceOf[InMemoryFolder])
          case _ => IOError("No such directory.")
        }.left.toOption
    )
  }

  def touch(name: String)(implicit authentication: Authentication): Either[IOError, InMemoryFile] = {
    checkCreate(name)(authentication) match {
      case Some(error) => error.message.ioErrorE
      case _ =>
        // TODO handle error
        val user = vum.getUser(authentication).get
        val file: InMemoryFile = new InMemoryFile (vum, vsm, fsINotify, Some(this), name, user)
        _files.add (file)
        fsINotify.notify(this)
        Right(file)
    }
  }

  def rename(name: String): Some[IOError] = {
    "Unsupported operation".ioErrorO
  }

  def createFile(name: String, content: AnyRef)(implicit authentication: Authentication): Either[IOError, VirtualFile] =
    for {
      createdFile <- touch(name).right
      file <- createdFile.setContent(content).toLeft(createdFile).right
    } yield file

  private def checkCreate(name: String)(implicit authentication: Authentication) = {
    if (!vsm.checkWriteAccess(this)) {
      accessDenied(s"touch $name for write access")
    } else if (!vsm.checkExecuteAccess(this)) {
      accessDenied(s"touch $name for execute access")
    } else if (_folders.exists(folder => folder.name == name) || _files.exists(file => file.name == name)) {
      ("touch: cannot create file ‘" + name + "’: File exists").ioErrorO
    } else {
      None
    }
  }
}
