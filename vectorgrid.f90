! FUN 
module grid
    use pointmass
    implicit none
    integer, parameter :: WIDTH = 64, HEIGHT = 64
    type vectorgrid
        integer, dimension(HEIGHT, WIDTH) :: varray !for outputting~!
        pointer, allocatable, :: masses
    contains
        procedure :: gridinit
        procedure :: gridupdate
    end type
    contains
    function gridinit(this)
        class(grid), intent(inout) :: this
        integer :: i, j
        do i = 1, HEIGHT
            do j = 1, WIDTH

end module grid

