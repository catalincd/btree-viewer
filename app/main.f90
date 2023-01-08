program main
  use, intrinsic :: iso_c_binding, only: c_ptr, c_funloc, c_null_char, c_null_ptr
  use M_strings
  use stripping
  use gtk, only: gtk_application_new, G_APPLICATION_FLAGS_NONE
  use g, only: g_application_run, g_object_unref
  use handlers
  use btreeclass

  implicit none
  integer(c_int)     :: status
  type(c_ptr)        :: app
  character(200)     :: output
  type(BTree) tree

  call Insert(tree, 5)
  call Insert(tree, 10)
  call Insert(tree, 3)
  call Insert(tree, 7)
  call Insert(tree, 4)
  !call Insert(tree, 12)
  !call Insert(tree, 26)
  call Print(tree, output)
  print *, output

  print *, getDigitsNum(50)
  print *, getDigitsNum(5)
  print *, getDigitsNum(5231)

  app = gtk_application_new("btree-viewer.catalincd"//c_null_char, G_APPLICATION_FLAGS_NONE)
 
  call g_signal_connect(app, "activate"//c_null_char, c_funloc(activate), c_null_ptr)

  status = g_application_run(app, 0_c_int, [c_null_ptr])

  print *, "Exit..."

  call g_object_unref(app)
end program main
