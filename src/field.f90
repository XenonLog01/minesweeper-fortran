module field
implicit none
  type, public :: game_field
    integer, allocatable :: mines(:,:)
    integer, allocatable :: vis_mines(:,:)
    integer :: w, h ! dimensions of the field
    integer :: n_mines
    integer :: n_mines_discovered
    logical :: game_over
  contains
    procedure :: set_field
    procedure :: print_field
    procedure :: cell_number
    procedure :: flag_mine
    procedure :: reveal_cell
    procedure :: delete
  end type game_field
contains
  subroutine cell_number(this, x, y, count)
    class(game_field), intent(inout) :: this
    integer, intent(in)  :: x, y
    integer, intent(out) :: count
    integer :: i, j
    integer :: t_x, t_y, t_is_mine ! Temporary variables

    count = 0

    do i = -1, 1
      t_x = x + i

      do j = -1, 1
        t_y = y + j

        if (i .eq. 0 .and. j .eq. 0) then 
          cycle
        end if

        if (t_x .gt. this%w) then
          cycle
        else if (t_x .lt. 1) then
          cycle
        end if

        if (t_y .gt. this%h) then
          cycle
        else if (t_y .lt. 1) then
          cycle
        end if

        t_is_mine = this%mines(t_x, t_y)

        if (t_is_mine .eq. -3) then
          count = count + 1 
        end if
      end do
    end do
  end subroutine cell_number

  subroutine set_field(this, w, h, n_mines)
    class(game_field), intent(inout) :: this

    integer, intent(in) :: w
    integer, intent(in) :: h
    integer, intent(in) :: n_mines

    integer :: placed_mines = 0
    integer :: x,y, i,j

    this%w = w
    this%h = h
    if (n_mines .gt. w*h) then
      this%n_mines = w*h
    else
      this%n_mines = n_mines
    end if
    this%n_mines_discovered = 0
    this%game_over = .false.

    allocate(this%mines(w, h))
    allocate(this%vis_mines(w,h))

    this%mines(:,:)     = 0
    this%vis_mines(:,:) = -1

    ! Place the mines on the field.
    do while (placed_mines .ne. n_mines)
      x = rand_int(1,w)
      y = rand_int(1,h)

      if (this%mines(x,y) .eq. -3) then
        cycle
      end if

      this%mines(x,y) = -3
      placed_mines = placed_mines + 1
    end do

    do i=1, w
      do j=1, h
        if (this%mines(i, j) .eq. -3) then
          cycle
        end if

        call this%cell_number(i, j, this%mines(i, j))
      end do
    end do
  end subroutine set_field

  subroutine print_field(this)
    class(game_field), intent(inout) :: this

    character(32) :: fmt 
    character(1), dimension(this%w, this%h) :: chr_arr
    integer, dimension(8) :: valid_nums
    integer :: grid_curr
    integer :: i, j

    parameter (valid_nums = (/1,2,3,4,5,6,7,8/))
    write(fmt, *) '( ', this%w, '(1x,a1) )'

    do i = 1, this%w
      do j = 1, this%h
        grid_curr = this%vis_mines(i, j)
        ! grid_curr = this%mines(i, j)

        if (grid_curr .eq. -1) then
          chr_arr(i, j) = "#"
        else if (grid_curr .eq. -2) then 
          chr_arr(i, j) = "F"
        else if (grid_curr .eq. -3) then
          chr_arr(i, j) = "M"
        else if (grid_curr .eq. 0) then
          chr_arr(i, j) = " " 
        else if (any(valid_nums .eq. grid_curr )) then
          chr_arr(i, j) = char(grid_curr + 48)
        end if
      end do
    end do

    ! print *, achar(27)//"[1G"
    print *, "---------------------------------------------------------"
    write(*, fmt) chr_arr
  
  end subroutine print_field


  subroutine flag_mine(this, x, y)
    class(game_field), intent(inout) :: this
    integer, intent(in) :: x, y
    
    if (this%vis_mines(x,y) .eq. -1) then
      this%vis_mines(x,y) = -2
    else if (this%vis_mines(x,y) .eq. -2) then
      if (this%mines(x,y) .eq. -3) then
        this%n_mines_discovered = this%n_mines_discovered - 1
      end if
      this%vis_mines(x,y) = -1
    end if

    if (this%mines(x,y) .eq. -3) then
      this%n_mines_discovered = this%n_mines_discovered + 1
    end if
  end subroutine flag_mine

  subroutine reveal_cell(this, x, y)
    class(game_field), intent(inout) :: this
    integer, intent(in) :: x, y
    integer :: selected_cell
    integer :: i, j, t_x, t_y
  
    selected_cell = this%mines(x,y)
    this%vis_mines(x,y) = selected_cell

    if (selected_cell .eq. -3) then
      this%game_over = .true.
    else if (selected_cell .eq. -2) then
      return
    else if (selected_cell .eq. 0) then
      do i = -1, 1
        t_x = x + i

        do j = -1, 1
          t_y = y + j

          if (i .eq. 0 .and. j .eq. 0) then 
            cycle
          end if

          if (t_x .gt. this%w) then
            cycle
          else if (t_x .lt. 1) then
            cycle
          end if
          if (t_y .gt. this%h) then
            cycle
          else if (t_y .lt. 1) then
            cycle
          end if

          if (this%vis_mines(t_x,t_y) .eq. -1) then
            call this%reveal_cell(t_x, t_y)
          end if
        end do
      end do
    end if
  end subroutine reveal_cell

  subroutine delete(this)
    class(game_field), intent(inout) :: this

    deallocate(this%mines)
    deallocate(this%vis_mines)
  end subroutine delete

  subroutine rand_seed()
    integer :: k, i, m, val(8)
    integer, allocatable :: seed(:)

    call random_seed(size = k)
    allocate(seed(k))
    call date_and_time (values=val)
    seed = [(173*i**2+4567,i=1,k)]
    m = min(8,k)
    seed(1:m) = seed(1:m) + val(m:1:-1)
    call random_seed (put=seed)
  end subroutine rand_seed

  integer function rand_int(lower, upper)
    integer :: lower, upper
    real :: n

    call random_number(n)
    rand_int = lower + floor((upper+1-lower)*n)
  end function rand_int
end module field
