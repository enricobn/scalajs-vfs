package org.enricobn.vfs

import org.enricobn.vfs.utils.Utils.RightBiasedEither

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

  def deleteFile(name: String)(implicit authentication: Authentication): Option[IOError]

  def deleteFolder(name: String)(implicit authentication: Authentication): Option[IOError]

  def touch(name: String)(implicit authentication: Authentication): Either[IOError, VirtualFile]

  def rename(name: String) : Option[IOError]

  def createFile(name: String, content: AnyRef)(implicit authentication: Authentication): Either[IOError, VirtualFile]

  def findFile(fileName: String)(implicit authentication: Authentication): Either[IOError, Option[VirtualFile]] = {
    files.right.map(_.find(_.name == fileName))
  }

  def findFileOrError(fileName: String, errorMessage: String)(implicit authentication: Authentication): Either[IOError, VirtualFile] =
    findFile(fileName) match {
      case Left(error) => Left(error)
      case Right(Some(f)) => Right(f)
      case Right(None) => Left(IOError(errorMessage))
    }

  def findFiles(predicate: Function[VirtualFile, Boolean])(implicit authentication: Authentication): Either[IOError, Set[VirtualFile]] =
    files.right.map(_.filter(predicate))

  def findFolder(folderName: String)(implicit authentication: Authentication): Either[IOError, Option[VirtualFolder]] =
    folders(authentication).right.map(_.find(_.name == folderName))

  def findFolderOrError(fileName: String, errorMessage: String)(implicit authentication: Authentication): Either[IOError, VirtualFolder] =
    findFolder(fileName) match {
      case Left(error) => Left(error)
      case Right(Some(f)) => Right(f)
      case Right(None) => Left(IOError(errorMessage))
    }

  def findFolders(predicate: Function[VirtualFolder, Boolean])(implicit authentication: Authentication): Either[IOError, Set[VirtualFolder]] =
    folders(authentication).right.map(_.filter(predicate))

  /**
    * Resolves the given path relative to this folder.
    * @return Left(IOError) if an error arises, Right(Some(folder)) if folder found or Right(None) if no folder is found.
    */
  def resolveFolder(path: String)(implicit authentication: Authentication): Either[IOError, Option[VirtualFolder]] =
    resolveFolder(VirtualPath(path))

  /**
    * Resolves the given path relative to this folder.
    * @return Left(IOError) if an error arises, Right(Some(folder)) if folder found or Right(None) if no folder is found.
    */
  def resolveFolder(path: VirtualPath)(implicit authentication: Authentication): Either[IOError, Option[VirtualFolder]] = {

    val folderE : Either[IOError, Option[VirtualFolder]] = Right(Some(this))

    path.fragments.foldLeft(folderE)((actualFolderE, fragment) =>
      actualFolderE match {
        case Right(Some(actualFolder)) =>
          fragment match {
            case SelfFragment() => Right(Some(actualFolder))
            case ParentFragment() => Right(actualFolder.parent)
            case simple: SimpleFragment => actualFolder.findFolder(simple.name)
            case _: RootFragment => Right(Some(root))
          }
        case n@Right(None) => n
        case error => error
      })
  }

  /**
    * resolves the given path relative to this folder.
    * @return Left(IOError) if an error arises or no folder found else Right(folder)
    */
  def resolveFolderOrError(path: String)(implicit authentication: Authentication): Either[IOError, VirtualFolder] = {
    noneToIOError(resolveFolder(path), s"Cannot resolve path '$path' from '${this.path}'.")
  }

  def resolveFile(path: VirtualPath)(implicit authentication: Authentication): Either[IOError, Option[VirtualFile]] = {
    path.parentFragments match {
      case Some(parent) => for {
        parentFolder <- resolveFolderOrError(parent.path)
        file <- parentFolder.findFile(path.name)
      } yield file
      case _ => findFile(path.name)
    }
  }

  def resolveFile(path: String)(implicit authentication: Authentication): Either[IOError, Option[VirtualFile]] =
    resolveFile(VirtualPath(path))

  def resolveFile(fragments: List[String])(implicit authentication: Authentication): Either[IOError, Option[VirtualFile]] =
    for {
      path <- VirtualPath.of(fragments:_*)
      file <- resolveFile(path)
    } yield file

  def resolveFileOrError(path: VirtualPath)(implicit authentication: Authentication): Either[IOError, VirtualFile] =
    noneToIOError(resolveFile(path), s"Cannot resolve path '${path.path}' from '${this.path}'.")

  def resolveFileOrError(path: String)(implicit authentication: Authentication): Either[IOError, VirtualFile] =
    noneToIOError(resolveFile(path), s"Cannot resolve path '$path' from '${this.path}'.")

  def resolveFileOrError(fragments: List[String])(implicit authentication: Authentication): Either[IOError, VirtualFile] =
    noneToIOError(resolveFile(fragments), s"Cannot resolve path '$fragments' from '${this.path}'.")

  private def noneToIOError[T](e: Either[IOError, Option[T]], message: String) =
    e match {
      case Left(error) => Left(error)
      case Right(Some(f)) => Right(f)
      case Right(None) => Left(IOError(message))
    }

}
