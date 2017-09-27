package org.enricobn.vfs

/**
  * Created by enrico on 5/9/17.
  */

object VirtualPath {
  def apply(fs: VirtualFS, path: String): VirtualPath = {
    if (path == fs.rootPath.path(fs))
      return fs.rootPath

    val absolute = path.startsWith(fs.pathSeparator)

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

    relativePath.split(fs.pathSeparator).foreach(name => {
      val fragment =
        if (name == fs.selfFragment)
          SelfFragment()
        else if (name == fs.parentFragment)
          ParentFragment()
        else
          SimpleFragment(name)

      fragments = fragments :+ fragment
    })

    VirtualPath(fragments)
  }
}

case class VirtualPath(fragments: List[PathFragment]) {

  def child(name: String): VirtualPath = VirtualPath(fragments ++ List(SimpleFragment(name)))

  def parent : Option[VirtualPath] =
    if (fragments.length == 1)
      None
    else
      Some(VirtualPath(fragments.slice(0, fragments.length -1)))

  def path(fs: VirtualFS) : String = {
    fragments.foldLeft("")((oldValue, fragment) => {
      val prefix =
        if (oldValue.isEmpty || oldValue == fs.root.path)
          ""
        else
          fs.pathSeparator

      oldValue + prefix + fragment.toString(fs)
    })
  }

  def isRelative: Boolean =
    fragments.head match {
      case RootFragment() => false
      case _ => true
    }

  /**
    * It will result in a Left(error) if the folder or the path does not exists, if you want to check that use
    * [[findFolder()]] instead.
    */
  def toFolder(fs: VirtualFS, currentFolder: VirtualFolder): Either[IOError,VirtualFolder] =
    findFolder(fs, currentFolder) match {
      case Right(Some(folder)) => Right(folder)
      case Right(None) => Left(IOError(s"Cannot resolve path '${path(fs)}' from '$currentFolder'"))
      case Left(error) => Left(error)
    }

  /**
    *
    * @return Right(Some(folder)) if the folder exists, Right(None) if the folder or the path do not exist,
    *         Left(error) if an error occurred.
    */
  def findFolder(fs: VirtualFS, currentFolder: VirtualFolder): Either[IOError,Option[VirtualFolder]] = {
    val first: Either[IOError, Option[VirtualFolder]] =
      fragments.head match {
        case RootFragment() => Right(Some(fs.root))
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
            case ParentFragment() => Right(Some(oldFolder.parent))
            case simple: SimpleFragment => oldFolder.findFolder(simple.name)
            case _ => Left(IOError(s"Invalid path: '${path(fs)}'"))
          }
        case n@Right(None) => n
        case error => error
      })
  }
}

sealed trait PathFragment {
  def toString(fs: VirtualFS) : String
}

case class RootFragment() extends PathFragment {
  override def toString(fs: VirtualFS): String = fs.root.path
}

case class ParentFragment() extends PathFragment {
  override def toString(fs: VirtualFS): String = fs.parentFragment
}

case class SelfFragment() extends PathFragment {
  override def toString(fs: VirtualFS): String = fs.selfFragment
}

case class SimpleFragment(name: String) extends PathFragment {
  override def toString(fs: VirtualFS): String = name
}