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

  def findFile(fileName: String, predicate: Function[VirtualFile, Boolean] = _ => true): Either[IOError, Option[VirtualFile]] = {
    files.right.map(_.find(file => file.name == fileName && predicate.apply(file)))
  }

  def findFileOrError(fileName: String, errorMessage: String, predicate: Function[VirtualFile, Boolean] = _ => true): Either[IOError, VirtualFile] = {
    findFile(fileName, predicate) match {
      case Left(error) => Left(error)
      case Right(Some(f)) => Right(f)
      case Right(None) => Left(IOError(errorMessage))
    }
  }

  def findFolder(name: String, predicate: Function[VirtualFolder, Boolean] = _ => true): Either[IOError, Option[VirtualFolder]] = {
    folders.right.map(_.find(folder => folder.name == name && predicate.apply(folder)))
  }

  /**
    * resolves the given path relative to this folder
    * @return Left(IOError) if an error arises, Right(Some(folder)) if folder found or Right(None) if no folder found
    */
  def resolveFolder(path: String): Either[IOError, Option[VirtualFolder]] = {
    if (path == "/") {
      return Right(Some(root))
    } else if (path.startsWith("/")) {
      return root.resolveFolder(path.substring(1))
    } else if (path.isEmpty) {
      return Right(Some(this))
    }
    val elements: Array[String] = path.split("/")
    var result = this
    for (element <- elements) {
      if (element == "..") {
        if (result.parent != null) {
          result = result.parent
        }
      } else if (element != ".") {
        result.findFolder(element) match {
          case Left(error) => return Left(error)
          case Right(None) => return Right(None)
          case Right(Some(f)) => result = f
        }
      }
    }
    Right(Some(result))
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

  def root: VirtualFolder = {
    if (parent == null) {
      this
    } else {
      parent.root
    }
  }
}
