module v2dmaths
    implicit none
    contains
    !distance between two 2d vectors
    real function v2d_distance(p1, p2)
        real, dimension(2), intent(in) :: p2
        real, dimension(2), intent(in) :: p1
        real :: diffx, diffy
        diffx = p1(1) - p2(1)
        diffy = p1(2) - p2(2)
        v2d_distance = sqrt(diffx**2 + diffy**2)
    end function v2d_distance
    !magnitude of a 2d vector
    real function v2d_magnitude(p1)
        real, dimension(2), intent(in) :: p1
        v2d_magnitude = sqrt(p1(1)**2 + p1(2)**2)
    end function v2d_magnitude
    !dot product of two 2d vectors
    real function v2d_dot(p1, p2)
        real, dimension(2), intent(in) :: p1, p2
        v2d_dot = p1(1) * p2(1) + p1(2) * p2(2)
    end function v2d_dot
    !adds two 2d vectors
    function v2d_add(p1, p2) result(pout)
        real, dimension(2), intent(in) :: p1, p2
        real, dimension(2) :: pout
        pout(1) = p1(1) + p2(1)
        pout(2) = p1(2) + p2(2)
    end function v2d_add
end module v2dmaths 