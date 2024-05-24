module fuika_game
  implicit none
  private

  public :: say_hello
contains
  subroutine say_hello
    print *, "Hello, fuika_game!"
  end subroutine say_hello
end module fuika_game
