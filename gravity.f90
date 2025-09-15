program main
    use v2dmaths
    use pointmass_mod
    use scene
    implicit none
    type(pointmass) :: rock, rock2, rock3
    integer :: i
    call sceneinit
    !moon!!!!
    rock%id = 1
    rock%coord = [30.0,30.0]
    rock%mass = 100
    rock%velocity = [0.3,-0.05]
    !earth!!!
    rock2%id = 2
    rock2%coord = [30.0,35.0]
    rock2%mass = 1
    rock2%velocity = [0.4,-0.05]
    !sun!!!
    rock3%id = 3
    rock3%coord = [70, 32]
    rock3%mass = 10000
    rock3%velocity = [0.0,0.0]
    call sceneadd(rock)
    call sceneadd(rock2)
    call sceneadd(rock3)
    !do the physic
    do i = 1, 300
        call printscreen()
        call sceneprocess()
        call system("sleep .1")
    end do
end program main

!cd "C:\Users\Angle Grinder\Documents\GitHub\gravsim"
!gfortran v2dmaths.f90 pointmass_mod.f90 scene.f90 gravity.f90 -o gravity