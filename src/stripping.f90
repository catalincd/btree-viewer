module stripping
  use M_strings, only : isdigit
  implicit none
  private s
contains

  function getDigitsNum(n) result(num)
    integer, intent(in) :: n
    integer :: n_copy
    integer :: num
    num = 0
    n_copy = n
    do while(n_copy > 0)
      num = num + 1
      n_copy = n_copy / 10
    end do
  end function getDigitsNum
  
  subroutine stripchars(instr, outstr)
    character(*), intent(in) :: instr
    character(*), intent(out) :: outstr
    character(200) :: temp
    integer i

    temp = ""

    do i=1, len_trim(instr)
      if(isdigit(instr(i:i))) then
        temp = trim(temp) // instr(i:i)
      end if
    end do

    !print *,instr, "=>", temp, outstr

    outstr = trim(temp)

  end subroutine stripchars 

  subroutine strip(in, out, keep)
    character(*), intent(in) :: in, keep
    character(*), intent(out) :: out

    call s(in, out, keep, len(in))
  end subroutine strip

  subroutine s(in, out, keep, len)
    integer, intent(in) :: len
    character, intent(in) :: in(len), keep*(*)
    character, intent(out) :: out(len)
    integer :: i

    out = PACK(in, SCAN(in,keep).eq.1, [(' ',i=1,len)])
  end subroutine s
  
  
end module stripping
