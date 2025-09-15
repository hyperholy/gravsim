module pointmass
    implicit none
    real, parameter :: gravconst = 6.6743e-11

    type pointmass
        real :: mass
        integer :: id
        real, dimension(2) :: coord
        real, dimension(2) :: velocity
    contains
        procedure :: gravity_calc => pointmass_gravity_calc
        procedure :: point_isequal
        procedure :: point_distance
    end type pointmass

    contains
    !compares two pointmass structs' IDs
    logical function point_isequal(p1, p2)
        type(pointmass), intent(in) :: p2
        type(pointmass), intent(in) :: p1
        if (p2%id == p1%id)
            point_isequal = .true.
        else
            point_isequal = .false.
        end if
    end function point_isequal
    !finds the signed distance between two pointmasses
    real function point_distance(p1, p2)
        type(pointmass), intent(in) :: p2
        type(pointmass), intent(in) :: p1
        real :: diffx, diffy
        diffx = p1%coord(1) - p2%coord(1)
        diffy = p1%coord(2) - p2%coord(2)
        point_distance = sqrt(diffx**2 + diffy**2)        
    end function distance
    !calculates the force experienced on a pointmass based on a list of pointmasses
    subroutine pointmass_gravity_calc(this, masslist, vforce)
        class(pointmass), intent(in) :: this
        type(pointmass), intent(in) :: masslist(:)
        real, dimension(2), intent(out) :: vforce
        real :: dx, dy, r
        integer :: i
        vforce = [0.0, 0.0]
        do i = 1, size(masslist)
            if (.not. point_isequal(this, masslist(i))) then
                dx = masslist(i)%coord(1) - this%coord(1)
                dy = masslist(i)%coord(2) - this%coord(2)
                r = point_distance(masslist(i), this)
                vforce(1) = vforce(1) + gravconst * this%mass * masslist(i)%mass * dx / r ** 3
                vforce(2) = vforce(2) + gravconst * this%mass * masslist(i)%mass * dy / r ** 3
            end if
        end do
    end subroutine pointmass_gravity_calc
end module pointmass 