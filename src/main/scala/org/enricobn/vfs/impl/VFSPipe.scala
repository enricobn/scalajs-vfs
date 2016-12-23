package org.enricobn.vfs.impl

import org.enricobn.vfs.{VFSInput, VFSOutput}

import scala.collection.mutable

/**
  * Created by enrico on 12/13/16.
  */
object VFSPipe {
  private[VFSPipe$] class StringPublisher extends mutable.Publisher[String] {
    override type Pub = mutable.Publisher[String]

    override def publish(event: String) {
      super.publish(event)
    }
  }
}

class VFSPipe extends VFSInput with VFSOutput {
  import VFSPipe._
  val stringPublisher = new StringPublisher

  def subscribe(fun: Function[String,Unit]) {
    stringPublisher.subscribe(new StringPublisher#Sub {
      override def notify(pub: mutable.Publisher[String], event: String) {
        fun(event)
      }
    })
  }

  def write(s: String) {
    stringPublisher.publish(s)
  }

  override def flush() {

  }
}
