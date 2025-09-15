program main
    use v2dmaths
    use pointmass_mod
    use scene
    implicit none
    type(pointmass) :: rock, rock2
    integer :: i
    call sceneinit
    !rock!!!!
    rock%id = 1
    rock%coord = [32.0,32.0]
    rock%mass = 100000000
    rock%velocity = [0.0,0.0]
    !rock2!!!
    rock2%id = 2
    rock2%coord = [8.0,8.0]
    rock2%mass = 3
    rock2%velocity = [0.0,0.0]
    call sceneadd(rock)
    call sceneadd(rock2)
    !do the physic
    do i = 1, 30
        call printscreen()
        call sceneprocess()
        !print *, "rock2 pos", masses(2)%coord
    end do
end program main

!cd "C:\Users\Angle Grinder\Documents\GitHub\gravsim"
!gfortran v2dmaths.f90 pointmass_mod.f90 scene.f90 gravity.f90 -o gravity