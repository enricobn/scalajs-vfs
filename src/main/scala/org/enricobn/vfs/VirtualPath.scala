package org.enricobn.vfs

import org.enricobn.vfs.utils.Utils

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

  def of(fragments: String*): Either[IOError, VirtualPath] = {
    import org.enricobn.vfs.utils.Utils.RightBiasedEither

    for {
      fr <- Utils.lift(fragments.map(PathFragment(_)))
      path <- Right(VirtualPath(fr))
    } yield path
  }

}

case class VirtualPath(fragments: List[PathFragment]) {

  def toParentFolder: VirtualPath = andThen(ParentFragment())

  def andThen(path: String): VirtualPath = andThen(VirtualPath(path))

  def andThen(fragment: PathFragment): VirtualPath = VirtualPath(fragments :+ fragment)

  def andThen(path: VirtualPath): VirtualPath = VirtualPath(fragments ++ path.fragments)

  def parentFragments: Option[VirtualPath] =
    if (fragments.length == 1)
      None
    else
      Some(VirtualPath(fragments.slice(0, fragments.length - 1)))

  def name: String =
    fragments.reverse.head.toString

  def path: String = {
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

}

object PathFragment {

  def apply(fragment: String): Either[IOError, PathFragment] =
    if (fragment == VirtualFS.selfFragment)
      Right(SelfFragment())
    else if (fragment == VirtualFS.parentFragment)
      Right(ParentFragment())
    else
      try {
        Right(SimpleFragment(fragment))
      } catch {
        case it: Exception => Left(IOError(it.getMessage))
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

case class SimpleFragment private(name: String) extends PathFragment {

  if (name.contains(VirtualFS.pathSeparator)) {
    // TODO I don't like to throw an exception, can I ignore this?
    throw new IllegalArgumentException("Illegal characters in name.")
  }

  override def toString: String = name
}