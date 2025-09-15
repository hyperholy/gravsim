module scene
    use pointmass_mod
    use v2dmaths
    implicit none
    public
    integer, parameter :: WIDTH = 64, HEIGHT = 64
    real, parameter :: DT = 10000 !silly gravity mult
    type(pointmass), allocatable :: masses (:)
    contains
    subroutine sceneinit()
        if (.not. allocated(masses)) allocate(masses(0))
    end subroutine sceneinit
    subroutine sceneadd(p1)
        type(pointmass), intent(in) :: p1
        type(pointmass), allocatable :: tmp(:)
        integer :: n
        n = size(masses)
        !temp array allocated
        allocate(tmp(n+1))
        !copy over if any
        if (n > 0) tmp(1:n) = masses
        !add new pointmass at the end
        tmp(n+1) = p1
        !move tmp into old masses and deallocating
        call move_alloc(tmp, masses)
    end subroutine sceneadd
    !processes the entire scene for us!
    subroutine sceneprocess()
        integer :: i
        real, dimension(2) :: tempvector !i hate race conditions
        !process and update the velocities
        do i = 1, size(masses)
            tempvector = [0.0,0.0]
            tempvector = masses(i)%gravity_calc(masses) * DT
            masses(i)%velocity = v2d_add(masses(i)%velocity, tempvector)
            
        end do
        !update the positions
        do i = 1, size(masses)
            tempvector = masses(i)%coord
            masses(i)%coord = v2d_add(tempvector, masses(i)%velocity)
            
        end do
    end subroutine sceneprocess
    !outputs the scene onto a 64x64 char array
    function scenereturn() result(parray)
        character, dimension(HEIGHT, WIDTH) :: parray !for outputting~!
        integer :: i
        character :: c
        parray = ' '
        do i = 1, size(masses)
            if (abs(masses(i)%coord(1)) >= WIDTH .or. abs(masses(i)%coord(2)) >= HEIGHT) then
                cycle
            endif
            c = '?'
            if (masses(i)%mass < 5) then
                c = '.'
            else if (masses(i)%mass < 10) then
                c = '*'
            else if (masses(i)%mass < 20) then
                c = '@'
            else
                c = '#'
            end if
            parray(int(masses(i)%coord(2)), int(masses(i)%coord(1))) = c
        end do
    end function scenereturn
    !print onto the console the 
    subroutine printscreen()
        character, dimension(HEIGHT, WIDTH) :: screen
        integer :: i, j
        screen = scenereturn()
        print *, char(27)//"[2J"! clear screen
        do i = 1, HEIGHT
            do j = 1, WIDTH
                write(*,'(A)', advance="no") screen(i,j)
            end do
            write(*,*)
        end do
        
    end subroutine printscreen
end module scene