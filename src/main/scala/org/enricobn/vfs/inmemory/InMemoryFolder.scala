package org.enricobn.vfs.inmemory

import org.enricobn.vfs.IOError._
import org.enricobn.vfs._
import org.enricobn.vfs.utils.Utils.RightBiasedEither

object InMemoryFolder {

  def root(vum: VirtualUsersManager, vsm: VirtualSecurityManager, fsINotify: VirtualFSNotifier) =
    new InMemoryFolder(vum, vsm, fsINotify, None, VirtualFS.root, VirtualUsersManager.ROOT, VirtualUsersManager.ROOT)

}

/**
  * Created by enrico on 12/2/16.
  *
  * The only way to create a folder is to use InMemoryFolder.root or using mkdir on an already created folder.
  */
class InMemoryFolder private(vum: VirtualUsersManager, vsm: VirtualSecurityManager,
                             fsINotify: VirtualFSNotifier, parent: Option[VirtualFolder], name: String,
                             owner: String, group: String)
  extends InMemoryNode(vum, vsm, parent, name, owner, group)
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

  private def checkExecuteAccess(implicit authentication: Authentication): Either[IOError, Unit] =
    if (!vsm.checkExecuteAccess(this)) {
      accessDenied("check execute access")
    } else {
      Right(())
    }

  private def checkWriteAccess(implicit authentication: Authentication): Either[IOError, Unit] =
    if (!vsm.checkWriteAccess(this)) {
      accessDenied("check write access")
    } else {
      Right(())
    }

  def folders(implicit authentication: Authentication): Either[IOError, Set[VirtualFolder]] =
    checkExecuteAccess.map(_ => _folders.toSet)

  def files(implicit authentication: Authentication): Either[IOError, Set[VirtualFile]] =
    checkExecuteAccess.map(_ => _files.toSet)

  def mkdir(name: String)(implicit authentication: Authentication): Either[IOError, InMemoryFolder] =
    for {
      _ <- checkWriteAccess
      _ <- checkExecuteAccess
      _ <- if (_folders.exists(folder => folder.name == name) || _files.exists(file => file.name == name)) {
        Left[IOError, Unit](IOError("mkdir: cannot create directory ‘" + name + "’: File exists"))
      } else {
        Right(())
      }
    } yield {
      // TODO handle error
      val user = vum.getUser.get
      val group = vum.getGroup.get
      val folder = new InMemoryFolder(vum, vsm, fsINotify, Some(this), name, user, group)
      _folders.add(folder)
      fsINotify.notify(this)
      folder
    }

  def deleteFile(name: String)(implicit authentication: Authentication): Either[IOError, Unit] =
    for {
      _ <- checkWriteAccess
      _ <- findFile(name).map {
        case Some(file) => Right(_files.remove(file.asInstanceOf[InMemoryFile]))
        case _ => IOError("No such file.")
      }
    } yield ()

  def deleteFolder(name: String)(implicit authentication: Authentication): Either[IOError, Unit] =
    for {
      _ <- checkWriteAccess
      _ <- findFolder(name).map {
        case Some(folder) => _folders.remove(folder.asInstanceOf[InMemoryFolder])
        case _ => IOError("No such directory.")
      }
    } yield ()

  def touch(name: String)(implicit authentication: Authentication): Either[IOError, InMemoryFile] =
    for {
      _ <- checkCreate(name)
    } yield {
      // TODO handle error
      val user = vum.getUser.get
      val group = vum.getGroup.get
      val file: InMemoryFile = new InMemoryFile(vum, vsm, fsINotify, Some(this), name, user, group)
      _files.add(file)
      fsINotify.notify(this)
      file
    }

  def rename(name: String): Either[IOError, Unit] = {
    "Unsupported operation".ioErrorE
  }

  def createFile(name: String, content: AnyRef)(implicit authentication: Authentication): Either[IOError, VirtualFile] =
    for {
      createdFile <- touch(name).right
      _ <- createdFile.setContent(content)
    } yield createdFile

  private def checkCreate(name: String)(implicit authentication: Authentication): Either[IOError, Unit] = {
    if (!vsm.checkWriteAccess(this)) {
      accessDenied(s"touch $name for write access for user ${authentication.user}")
    } else if (!vsm.checkExecuteAccess(this)) {
      accessDenied(s"touch $name for execute access for user ${authentication.user}")
    } else if (_folders.exists(folder => folder.name == name) || _files.exists(file => file.name == name)) {
      ("touch: cannot create file ‘" + name + "’: file exists").ioErrorE
    } else {
      Right(())
    }
  }
}
