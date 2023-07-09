program minesweeper
  use field
implicit none
  type(game_field) :: l_field

  integer :: running = 1
  integer :: mode        ! The selected input mode
  integer :: p_x, p_y    ! Player x, y

  call l_field%set_field(5, 10, 10)

  do while (running .ne. 0)
    ! TODO ::
    !  - Otherwise ::
    !   - Reveal the selected tile
    !   - If it's a mine, exit the application
    !  - Clear the previous frame
    !  - Print the field
    call l_field%print_field

    print *, "Placement mode: (discover :: 1, flag :: 2, quit :: 3)"
    read(*,*) mode
    if (mode .eq. 3) then
      exit
    end if

    ! Get the x, y coordinates.
    print *, "x, y:"
    read(*,*) p_x
    read(*,*) p_y

    ! Make sure that the x, y coordinates are in the bounds of the field
    if (p_x .gt. l_field%w) then
      print *, "That x value is too large!"
      cycle
    else if (p_x .lt. 1) then
      print *, "That x value is too small!"
      cycle
    end if
    if (p_y .gt. l_field%h) then
      print *, "That y value is too large!"
      cycle
    else if (p_y .lt. 1) then
      print *, "That y value is too small!"
      cycle
    end if

    if (mode .eq. 1) then
      call l_field%reveal_cell(p_x, p_y)
      if (l_field%game_over .eqv. .true.) then
        call l_field%print_field
        print *, "You lose!"
        exit
      end if
    else if (mode .eq. 2) then
      call l_field%flag_mine(p_x, p_y)
    end if

    if (l_field%n_mines_discovered .eq. l_field%n_mines) then
      call l_field%print_field
      print *, "You win!"
      exit
    end if
  end do 

  call l_field%delete

contains

end program minesweeper
