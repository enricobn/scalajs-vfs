package org.enricobn.vfs

import org.enricobn.vfs.IOError._

import scala.scalajs.js.annotation.JSExportAll

/**
  * Created by enrico on 12/2/16.
  */
@JSExportAll
trait VirtualFolder extends VirtualNode {

  lazy val root: VirtualFolder =
    if (parent.isEmpty)
      this
    else
      parent.get.root

  def folders(implicit authentication: Authentication) : Either[IOError, Set[VirtualFolder]]

  def files(implicit authentication: Authentication) : Either[IOError, Set[VirtualFile]]

  def mkdir(name: String)(implicit authentication: Authentication): Either[IOError, VirtualFolder]

  def deleteFile(name: String)(implicit authentication: Authentication): Either[IOError, Unit]

  def deleteFolder(name: String)(implicit authentication: Authentication): Either[IOError, Unit]

  def touch(name: String)(implicit authentication: Authentication): Either[IOError, VirtualFile]

  def rename(name: String) : Either[IOError, Unit]

  def createFile(name: String, content: AnyRef)(implicit authentication: Authentication): Either[IOError, VirtualFile]

  def findFile(fileName: String)(implicit authentication: Authentication): Either[IOError, Option[VirtualFile]] = {
    files.right.map(_.find(_.name == fileName))
  }

  def findFileOrError(fileName: String)(implicit authentication: Authentication): Either[IOError, VirtualFile] =
    findFile(fileName) match {
      case Left(error) => Left(error)
      case Right(Some(f)) => Right(f)
      case Right(None) => s"Cannot find file $fileName in $this".ioErrorE
    }

  def findFiles(predicate: Function[VirtualFile, Boolean])(implicit authentication: Authentication): Either[IOError, Set[VirtualFile]] =
    files.right.map(_.filter(predicate))

  def findFolder(folderName: String)(implicit authentication: Authentication): Either[IOError, Option[VirtualFolder]] =
    folders(authentication).right.map(_.find(_.name == folderName))

  def findFolderOrError(folderName: String)(implicit authentication: Authentication): Either[IOError, VirtualFolder] =
    findFolder(folderName) match {
      case Left(error) => Left(error)
      case Right(Some(f)) => Right(f)
      case Right(None) => s"Cannot find folder $folderName in $this".ioErrorE
    }

  def findFolders(predicate: Function[VirtualFolder, Boolean])(implicit authentication: Authentication): Either[IOError, Set[VirtualFolder]] =
    folders(authentication).right.map(_.filter(predicate))

}
