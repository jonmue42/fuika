program main
  use, intrinsic :: iso_c_binding, only: c_null_char
  use :: raylib
  !use fuika_game, only: say_hello
  use class_Ball
  
  implicit none (type, external)
  ! loop variables
  integer :: i, j

  ! Array of all spheres in the game
  type(Ball), allocatable :: balls(:)

  ! Shock number for collisions
  real :: shock_num = 0.5

  real :: BALL_RADII(10) = [10, 20, 30, 40, 50, 60, 70, 80, 90, 100]
  real :: BALL_MASSES(10) = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]

  ! raylib winodw settings
  integer, parameter :: SCREEN_WIDTH = 600
  integer, parameter :: SCREEN_HEIGHT = 1000
  integer, parameter :: BOUNDS(2) = [SCREEN_WIDTH, SCREEN_HEIGHT]
  real, parameter :: START_COORDS(2) = [SCREEN_WIDTH/2, 600]  



  balls = [Ball(color=RED, radius=BALL_RADII(5), mass=BALL_MASSES(1), coords=START_COORDS, velocity=[0.0, 0.0],gravity=900)]


  ! Initialize window
  call init_window(SCREEN_WIDTH, SCREEN_HEIGHT, "FUIKA GAME")

  do while (.not. window_should_close())
  ! Controls
    ! Add a new ball
    if ( is_key_pressed(KEY_UP) ) then
      balls(size(balls))%gravity = 900  
      balls(size(balls))%coords(2) = balls(size(balls))%coords(2) + balls(size(balls))%radius * 2
      balls = [balls, Ball(color=BLUE,radius=50.0, mass=1.0, coords=[SCREEN_WIDTH/2, 50], velocity=[0.0, 0.0], gravity=0)]
    elseif ( is_key_pressed(KEY_DOWN) ) then
      balls = [balls(:size(balls)-2), balls(size(balls))]  ! remove last ball
    end if
    if ( is_key_down(KEY_RIGHT) ) then
      balls(size(balls))%coords = balls(size(balls))%coords + [500, 0] * get_frame_time()
    end if
    if ( is_key_down(KEY_LEFT) ) then
      balls(size(balls))%coords = balls(size(balls))%coords - [500, 0] * get_frame_time()
    end if
    ! Update all balls
    do i = 1, size(balls)
      call balls(i)%collision(BOUNDS, shock_num, balls(i+1:))
    end do
    do i = 1, size(balls)
      call balls(i)%update
    end do
    call begin_drawing()
        call clear_background(RAYWHITE)
        ! Draw all balls
        do i = 1, size(balls)
         call balls(i)%draw
        end do
    call draw_fps(10, 10)
    call end_drawing()
  end do
  call close_window()

end program main
