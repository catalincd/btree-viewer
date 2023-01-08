module handlers

  use, intrinsic :: iso_c_binding, only: c_ptr, c_int, c_funloc, c_null_char
  use gtk
  use cairo
  use btreeclass
  use stripping
  use M_strings
  use gtk_hl_container
  use gtk_hl_button
  use gtk_hl_dialog
  use gtk_hl_chooser
  use gtk_hl_misc
  use gtk_hl_entry
  use gtk_hl_combobox

  implicit none
  type(c_ptr) :: insertBox
  type(BTree) :: mainTree
  integer :: width, height
  type(c_ptr) :: window
  type(c_ptr) :: drawingArea

contains

  subroutine activate(app, gdata) bind(c)
    type(c_ptr), value, intent(in)  :: app, gdata
    type(c_ptr) :: base
    type(c_ptr) :: toprow
    type(c_ptr) :: insertButton
    type(c_ptr) :: searchButton
    type(c_ptr) :: deleteButton
    type(c_ptr) :: clearButton
    type(c_ptr) :: setButton
    type(c_ptr) :: screenButton

    width = 1200
    height = 800
    window = gtk_application_window_new(app)
    call gtk_window_set_title(window, "B-Tree Viewer"//c_null_char)
    call gtk_window_set_default_size(window, width, height)

    base = hl_gtk_box_new()
    call gtk_widget_set_margin_start (base, 10_c_int)
    call gtk_widget_set_margin_end (base, 10_c_int)
    call gtk_widget_set_margin_top (base, 10_c_int)
    call gtk_widget_set_margin_bottom (base, 10_c_int)
    call gtk_window_set_child(window, base)

    toprow = hl_gtk_box_new(horizontal=TRUE, homogeneous=FALSE)
    call hl_gtk_box_pack(base, toprow)

    insertBox = gtk_entry_new()
    call g_signal_connect(insertBox, "changed"//c_null_char, c_funloc(filter_insert))
    call hl_gtk_box_pack(toprow, insertBox)

    insertButton = hl_gtk_button_new("Insert"//c_null_char, clicked=c_funloc(insertNumber))
    call hl_gtk_box_pack(toprow, insertButton)
    
    searchButton = hl_gtk_button_new("Search"//c_null_char, clicked=c_funloc(searchNumber))
    call hl_gtk_box_pack(toprow, searchButton)

    deleteButton = hl_gtk_button_new("Delete"//c_null_char, clicked=c_funloc(deleteNumber))
    call hl_gtk_box_pack(toprow, deleteButton)

    clearButton = hl_gtk_button_new("Clear"//c_null_char, clicked=c_funloc(clearTree))
    call hl_gtk_box_pack(toprow, clearButton)

    setButton = hl_gtk_button_new("Set T"//c_null_char, clicked=c_funloc(print_hello))
    call hl_gtk_box_pack(toprow, setButton)

    screenButton = hl_gtk_button_new("Screenshot"//c_null_char, clicked=c_funloc(print_hello))
    call hl_gtk_box_pack(toprow, screenButton)

    drawingArea = gtk_drawing_area_new()
    call gtk_drawing_area_set_content_width(drawingArea, 700)
    call gtk_drawing_area_set_content_height(drawingArea, 700)
    call gtk_drawing_area_set_draw_func(drawingArea, c_funloc(draw_function), c_null_ptr, c_null_funptr)
    !call gtk_window_set_child(window, drawingArea)
    call hl_gtk_box_pack(base, drawingArea)

    call gtk_widget_show(window)

  end subroutine activate

  subroutine draw_function(widget, cairo_context, width, height, gdata) bind(c)
    use, intrinsic :: iso_fortran_env, only: wp=>real64

    type(c_ptr), value, intent(in)    :: widget, cairo_context, gdata
    integer(c_int), value, intent(in) :: width, height
    integer                           :: t
    real(wp), parameter               :: pi = acos(-1.0_wp)

    call cairo_set_antialias(cairo_context, CAIRO_ANTIALIAS_BEST)

    call cairo_new_sub_path(cairo_context)


    call cairo_set_source_rgb(cairo_context, 0d0, 0d0, 0.1d0)
    call cairo_paint(cairo_context)
    call cairo_set_source_rgb(cairo_context, 1d0, 0d0, 0d0)
    call cairo_set_line_width(cairo_context, 2d0)
    
    !call cairo_rectangle(cairo_context, 10d0, 10d0, 100d0, 100d0)
    !call cairo_stroke(cairo_context)

    if(.not. mainTree%root%IsNull) then
      print *,"DRAWING:",LOC(mainTree%root%addr)
      call draw_node(cairo_context, mainTree%root%addr, 0d0, width- 0d0, 1)
    end if
  end subroutine draw_function

  recursive subroutine draw_node(cairo_context, node, start, width, level)
    type(c_ptr), value, intent(in)    :: cairo_context
    type(BNode), intent(inout) :: node
    character(50) :: temp
    integer, intent(in) :: level 
    real(8), intent(in) :: start, width
    integer :: i
    integer :: digitsNum

    real(8) :: children_width
    real(8) :: current_width
    real(8) :: current_top
    real(8) :: current_left
    real(8) :: text_left
    real(8) :: text_top

    real(8) :: LEVEL_HEIGHT = 100d0
    real(8) :: CELL_WIDTH = 40d0
    real(8) :: CELL_HEIGHT = 25d0
    real(8) :: DIGIT_WIDTH = 5.75d0

    logical :: ACCEPT_EMPTY = .false.

    current_width = node%KeysNum * CELL_WIDTH
    current_left = start + (width / 2) - (current_width / 2)
    current_top = level * LEVEL_HEIGHT
    text_top = current_top + CELL_HEIGHT - 6

    call cairo_rectangle(cairo_context, current_left, current_top, current_width, CELL_HEIGHT)

    call cairo_set_font_size (cairo_context, 20d0)

    do i=0,node%KeysNum-1

      digitsNum = getDigitsNum(node%Keys(i))
      text_left = current_left + (i * CELL_WIDTH) + (CELL_WIDTH / 2) - (digitsNum * DIGIT_WIDTH)
      temp = ""
      call value_to_string(node%Keys(i), temp)
      call cairo_move_to(cairo_context, text_left, text_top)

      if(node%Found .and. node%FoundIdx==i) then
        node%Found = .false.
        call cairo_set_source_rgb(cairo_context, 0d0, 1d0, 0d0)
        call cairo_show_text (cairo_context, temp//c_null_char)
        call cairo_set_source_rgb(cairo_context, 1d0, 0d0, 0d0)
      else
        call cairo_show_text (cairo_context, temp//c_null_char)
      end if

      call cairo_move_to(cairo_context, current_left + (i * CELL_WIDTH), current_top)
      call cairo_line_to(cairo_context, current_left + (i * CELL_WIDTH), current_top + CELL_HEIGHT)
    end do 

    if(.not. node%Leaf) then
      children_width = width / node%ChildrenNum
      do i=0,node%ChildrenNum-1
        if(node%Children(i)%addr%KeysNum > 0 .or. ACCEPT_EMPTY) then
          call cairo_move_to(cairo_context, current_left + (i * CELL_WIDTH), current_top + CELL_HEIGHT)
          call cairo_line_to(cairo_context, start + (children_width * i) + children_width / 2, current_top + LEVEL_HEIGHT)
          call draw_node(cairo_context, node%Children(i)%addr, start + (children_width * i), children_width, level+1)
        end if
      end do
    end if


    call cairo_stroke(cairo_context)

  end subroutine draw_node

  subroutine insertNumber(widget, gdata) bind(c)
    type(c_ptr), value, intent(in)  :: widget, gdata
    type(c_ptr) :: buffer
    character(:), allocatable :: ftext
    character(200) :: inorder
    integer :: num
    integer :: ierr

    buffer = gtk_entry_get_buffer(insertBox)
    call c_f_string_copy_alloc(gtk_entry_buffer_get_text(buffer), ftext)
    call string_to_value(ftext,num,ierr)
    call Insert(mainTree, num)
    print *, "Insert: ",num
    call Print(mainTree, inorder)
    print *, "Inorder: ", inorder
    call gtk_widget_queue_draw(drawingArea);

  end subroutine insertNumber

  subroutine deleteNumber(widget, gdata) bind(c)
    type(c_ptr), value, intent(in)  :: widget, gdata
    type(c_ptr) :: buffer
    character(:), allocatable :: ftext
    character(200) :: inorder
    integer :: num
    integer :: ierr

    buffer = gtk_entry_get_buffer(insertBox)
    call c_f_string_copy_alloc(gtk_entry_buffer_get_text(buffer), ftext)
    call string_to_value(ftext,num,ierr)
    call Delete(mainTree, num)
    print *, "Delete: ",num
    call Print(mainTree, inorder)
    print *, "Inorder: ", inorder
    call gtk_widget_queue_draw(drawingArea);

  end subroutine deleteNumber


  subroutine searchNumber(widget, gdata) bind(c)
    type(c_ptr), value, intent(in)  :: widget, gdata
    type(c_ptr) :: buffer
    character(:), allocatable :: ftext
    character(200) :: inorder
    integer :: num
    integer :: ierr

    buffer = gtk_entry_get_buffer(insertBox)
    call c_f_string_copy_alloc(gtk_entry_buffer_get_text(buffer), ftext)
    call string_to_value(ftext,num,ierr)
    call Search(mainTree, num)
    print *, "Search: ",num
    call gtk_widget_queue_draw(drawingArea);

  end subroutine searchNumber

  subroutine clearTree
    mainTree%root%IsNull = .true.
    call gtk_widget_queue_draw(drawingArea);
  end subroutine clearTree

  subroutine print_hello(widget, gdata) bind(c)
    type(c_ptr), value, intent(in)  :: widget, gdata

    print *, "HELL-O!"
  end subroutine print_hello

  subroutine filter_insert(widget, gdata) bind(c)
    type(c_ptr), value, intent(in) :: widget, gdata
    call filter_box(insertBox)
  end subroutine filter_insert

  subroutine filter_box(box)
    type(c_ptr) :: box
    type(c_ptr) :: buffer
    integer(c_int16_t) :: ntext
    character(:), allocatable :: ftext
    character(20) outtext
    
    ntext = gtk_entry_get_text_length(box)
    buffer = gtk_entry_get_buffer(box)
    call c_f_string_copy_alloc(gtk_entry_buffer_get_text(buffer), ftext)
    call stripchars(ftext, outtext)
    !print *, len(ftext), ntext, ftext, " -", outtext
    if (outtext /= ftext) then
      buffer = gtk_entry_get_buffer(box)
      call gtk_entry_buffer_set_text (buffer, trim(outtext)//c_null_char, -1)
    end if
  end subroutine filter_box


end module handlers
