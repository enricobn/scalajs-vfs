package org.enricobn.vfs

/**
  * Created by enrico on 5/9/17.
  */

object VirtualPath {

  def apply(path: String): VirtualPath = {
    if (path == VirtualFS.rootPath.path)
      return VirtualFS.rootPath

    val absolute = path.startsWith(VirtualFS.pathSeparator)

    val relativePath =
      if (absolute)
        path.substring(1)
      else
        path

    var fragments: List[PathFragment] =
      if (absolute)
        List(RootFragment())
      else
        List.empty

    relativePath.split(VirtualFS.pathSeparator).foreach(name => {
      val fragment =
        if (name == VirtualFS.selfFragment)
          SelfFragment()
        else if (name == VirtualFS.parentFragment)
          ParentFragment()
        else
          SimpleFragment(name)

      fragments = fragments :+ fragment
    })

    VirtualPath(fragments)
  }
}

case class VirtualPath(fragments: List[PathFragment]) {

  def andThen(name: String): VirtualPath = andThen(SimpleFragment(name))

  def andThen(fragment: PathFragment): VirtualPath = VirtualPath(fragments :+ fragment)

  private def parentFragments : Option[VirtualPath] =
    if (fragments.length == 1)
      None
    else
      Some(VirtualPath(fragments.slice(0, fragments.length -1)))

  def name : String =
    fragments.reverse.head.toString

  def path : String = {
    fragments.foldLeft("")((oldValue, fragment) => {
      val prefix =
        if (oldValue.isEmpty || oldValue == VirtualFS.rootPath.path)
          ""
        else
          VirtualFS.pathSeparator

      oldValue + prefix + fragment.toString
    })
  }

  def isRelative: Boolean =
    fragments.head match {
      case RootFragment() => false
      case _ => true
    }

  /**
    * @param currentFolder since the path could be relative you must specify the current folder.
    * @return Right(Some(folder)) if the folder exists, Right(None) if the folder or the path do not exist,
    *         Left(error) if an error occurred.
    */
  def findFolder(currentFolder: VirtualFolder): Either[IOError,Option[VirtualFolder]] = {
    val first: Either[IOError, Option[VirtualFolder]] =
      fragments.head match {
        case RootFragment() => Right(Some(currentFolder.root))
        case _ => Right(Some(currentFolder))
      }

    val fragmentsToProcess =
      fragments.head match {
        case RootFragment() => fragments.tail
        case _ => fragments
      }

    fragmentsToProcess.foldLeft(first)((oldValue, fragment) =>
      oldValue match {
        case Right(Some(oldFolder)) =>
          fragment match {
            case SelfFragment() => Right(Some(oldFolder))
            case ParentFragment() => Right(oldFolder.parent)
            case simple: SimpleFragment => oldFolder.findFolder(simple.name)
            case _ => Left(IOError(s"Invalid path: '$path'"))
          }
        case n@Right(None) => n
        case error => error
      })
  }

  /**
    * @param currentFolder since the path could be relative you must specify the current folder.
    * @return a Left(error) if the folder or the path does not exist; if you want to check that, use
    * [[VirtualPath.findFolder]] instead.
    */
  def toFolder(currentFolder: VirtualFolder): Either[IOError,VirtualFolder] =
    findFolder(currentFolder) match {
      case Right(Some(folder)) => Right(folder)
      case Right(None) => Left(IOError(s"Cannot resolve path '$path' from '$currentFolder'."))
      case Left(error) => Left(error)
    }

  /**
    * @param currentFolder since the path could be relative you must specify the current folder.
    * @return Right(Some(file)) if the file exists, Right(None) if the file or the path do not exist,
    *         Left(error) if an error occurred.
    */
  def findFile(currentFolder: VirtualFolder): Either[IOError,Option[VirtualFile]] =
    if (parentFragments.isEmpty)
      currentFolder.findFile(this.name)
    else
      parentFragments.get.findFolder(currentFolder) match {
        case Right(Some(folder)) => folder.findFile(name)
        case Right(None) => Right(None)
        case Left(error) => Left(error)
      }

  /**
    * @param currentFolder since the path could be relative you must specify the current folder.
    * @return a Left(error) if the file or the path does not exist; if you want to check that, use
    * [[VirtualPath.findFile]] instead.
    */
  def toFile(currentFolder: VirtualFolder): Either[IOError,VirtualFile] =
    findFile(currentFolder) match {
      case Right(Some(file)) => Right(file)
      case Right(None) => Left(IOError(s"Cannot resolve file '$path' from '$currentFolder'"))
      case Left(error) => Left(error)
    }

}

sealed trait PathFragment

case class RootFragment() extends PathFragment {
  override def toString: String = VirtualFS.pathSeparator
}

case class ParentFragment() extends PathFragment {
  override def toString: String = VirtualFS.parentFragment
}

case class SelfFragment() extends PathFragment {
  override def toString: String = VirtualFS.selfFragment
}

case class SimpleFragment private (name: String) extends PathFragment {

  if (name.contains(VirtualFS.pathSeparator)) {
    //I like more Either, but this constructor will not be used from clients.
    throw new IllegalArgumentException("Illegal characters in name.")
  }

  override def toString: String = name
}