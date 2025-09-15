module pointmass_mod
    use v2dmaths
    implicit none
    real, parameter :: gravconst = 6.6743e-11

    type pointmass
        real :: mass
        integer :: id
        real, dimension(2) :: coord
        real, dimension(2) :: velocity
    contains
        procedure :: gravity_calc => pointmass_gravity_calc
    end type pointmass

    contains
    !compares two pointmass structs' IDs
    function point_isequal(p1, p2) result(is_eq)
        type(pointmass), intent(in) :: p2
        type(pointmass), intent(in) :: p1
        logical :: is_eq
        is_eq = (p2%id == p1%id)
    end function point_isequal
    !calculates the force experienced on a pointmass based on a list of pointmasses
    function pointmass_gravity_calc(this, masslist) result(vforce)
        class(pointmass), intent(in) :: this
        type(pointmass), intent(in) :: masslist(:)
        real, dimension(2) :: vforce
        real :: dx, dy, r
        integer :: i
        vforce = [0.0, 0.0]
        do i = 1, size(masslist)
            if (.not. point_isequal(this, masslist(i))) then
                dx = masslist(i)%coord(1) - this%coord(1)
                dy = masslist(i)%coord(2) - this%coord(2)
                r = v2d_distance(masslist(i)%coord, this%coord)
                vforce(1) = vforce(1) + gravconst * this%mass * masslist(i)%mass * dx / r ** 3
                vforce(2) = vforce(2) + gravconst * this%mass * masslist(i)%mass * dy / r ** 3
            end if
        end do
    end function pointmass_gravity_calc
end module pointmass_mod