!cd "C:\Users\Angle Grinder\Documents\GitHub\gravsim"
!gfortran gravity.f90 -0 gravity
program gravity
    use vector
    implicit none
    type(vector2d) :: v1
    v1%v = [1,2]
    print *, "X: ", v1%v(1), "Y: ", v1%v(2), "MAGNITUDE: ", v1%magnitude()
    !print *, char(27)//"[2J"   ! clear screen
end program gravity
