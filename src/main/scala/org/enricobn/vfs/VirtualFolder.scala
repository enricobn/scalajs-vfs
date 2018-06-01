package org.enricobn.vfs

import scala.scalajs.js.annotation.JSExportAll

/**
  * Created by enrico on 12/2/16.
  */
@JSExportAll
trait VirtualFolder extends VirtualNode {

  def folders: Either[IOError, Set[VirtualFolder]]

  def files: Either[IOError, Set[VirtualFile]]

  def mkdir(name: String): Either[IOError, VirtualFolder]

  def deleteFile(name: String) : Option[IOError]

  def deleteFolder(name: String) : Option[IOError]

  def touch(name: String): Either[IOError, VirtualFile]

  def rename(name: String) : Option[IOError]

  def createFile(name: String, content: AnyRef) : Either[IOError, VirtualFile]

  def findFile(fileName: String): Either[IOError, Option[VirtualFile]] = {
    files.right.map(_.find(_.name == fileName))
  }

  def findFileOrError(fileName: String, errorMessage: String): Either[IOError, VirtualFile] =
    findFile(fileName) match {
      case Left(error) => Left(error)
      case Right(Some(f)) => Right(f)
      case Right(None) => Left(IOError(errorMessage))
    }

  def findFiles(predicate: Function[VirtualFile, Boolean]): Either[IOError, Set[VirtualFile]] =
    files.right.map(_.filter(predicate))

  def findFolder(folderName: String): Either[IOError, Option[VirtualFolder]] =
    folders.right.map(_.find(_.name == folderName))

  def findFolderOrError(fileName: String, errorMessage: String): Either[IOError, VirtualFolder] =
    findFolder(fileName) match {
      case Left(error) => Left(error)
      case Right(Some(f)) => Right(f)
      case Right(None) => Left(IOError(errorMessage))
    }

  def findFolders(predicate: Function[VirtualFolder, Boolean]): Either[IOError, Set[VirtualFolder]] =
    folders.right.map(_.filter(predicate))

  /**
    * Resolves the given path relative to this folder. It MUST be a relative path.
    * @return Left(IOError) if an error arises, Right(Some(folder)) if folder found or Right(None) if no folder is found.
    */
  def resolveFolder(path: String): Either[IOError, Option[VirtualFolder]] = {
    val virtualPath = VirtualPath(path)

    val folderE : Either[IOError, Option[VirtualFolder]] = Right(Some(this))

    virtualPath.fragments.foldLeft(folderE)((actualFolderE, fragment) =>
      actualFolderE match {
        case Right(Some(actualFolder)) =>
          fragment match {
            case SelfFragment() => Right(Some(actualFolder))
            case ParentFragment() => Right(actualFolder.parent)
            case simple: SimpleFragment => actualFolder.findFolder(simple.name)
            case _ => Left(IOError(s"Invalid path: '$path'"))
          }
        case n@Right(None) => n
        case error => error
      })
  }

  /**
    * resolves the given path relative to this folder.
    * @return Left(IOError) if an error arises, Right(folder) if folder found or Right(IOError(errorMessage)) if no folder found.
    */
  def resolveFolderOrError(path: String, errorMessage: String): Either[IOError, VirtualFolder] = {
    resolveFolder(path) match {
      case Left(error) => Left(error)
      case Right(Some(f)) => Right(f)
      case Right(None) => Left(IOError(errorMessage))
    }
  }

  lazy val root: VirtualFolder =
    if (parent.isEmpty)
      this
    else
      parent.get.root

}
