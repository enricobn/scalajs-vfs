package org.enricobn.vfs


/**
  * Created by enrico on 12/2/16.
  */
object VirtualPermissions {
  val EXEC_PERMISSIONS: VirtualPermissions = new VirtualPermissions() {
    def owner: VirtualPermission = VirtualPermission.EXEC_PERMISSION

    def group: VirtualPermission =VirtualPermission.EXEC_PERMISSION

    def others: VirtualPermission = VirtualPermission.EXEC_PERMISSION
  }

  val READ_PERMISSIONS: VirtualPermissions = new VirtualPermissions() {
    def owner: VirtualPermission = VirtualPermission.READ_PERMISSION

    def group: VirtualPermission = VirtualPermission.READ_PERMISSION


    def others: VirtualPermission = VirtualPermission.READ_PERMISSION

  }

}

trait VirtualPermissions {

  def owner: VirtualPermission

  def group: VirtualPermission

  def others: VirtualPermission
}
