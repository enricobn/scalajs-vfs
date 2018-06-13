package org.enricobn.vfs


/**
  * Created by enrico on 12/2/16.
  */
object VirtualPermissions {

  val EXEC_PERMISSIONS = VirtualPermissionsImpl(VirtualPermission.EXEC, VirtualPermission.EXEC, VirtualPermission.EXEC)

  val READ_PERMISSIONS = VirtualPermissionsImpl(VirtualPermission.READ, VirtualPermission.READ, VirtualPermission.READ)

}

case class VirtualPermissionsImpl(override val owner: VirtualPermission, override val group: VirtualPermission,
                                  override val others: VirtualPermission) extends VirtualPermissions

trait VirtualPermissions {

  def owner: VirtualPermission

  def group: VirtualPermission

  def others: VirtualPermission

  def octal : Int = others.octal + 10 * group.octal + 100 * owner.octal
}
