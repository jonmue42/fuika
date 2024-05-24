module class_Ball
        use, intrinsic :: iso_c_binding
        use :: raylib
        implicit none
        private
        type, public :: Ball
                type(color_type) :: color
                real :: radius
                real :: mass
                real :: coords(2)
                real :: velocity(2)
                real :: gravity
        contains
                procedure :: update => update_ball
                procedure :: draw => draw_ball
                procedure :: collision => collision_ball
        end type Ball
contains
        subroutine update_ball(this)
                class(Ball), intent(inout) :: this
                this%coords = this%coords + this%velocity * get_frame_time()
                this%velocity(2) = this%velocity(2) + this%gravity * 0.5 * get_frame_time()
        end subroutine update_ball

        subroutine draw_ball(this)
                class(Ball), intent(inout) :: this
                call draw_circle_v(vector2_type(this%coords(1), this%coords(2)), this%radius, this%color)
        end subroutine draw_ball

        subroutine collision_ball(this, bounds, shock_num, balls)
                class(Ball), intent(inout) :: this
                integer, intent(in) :: bounds(2)
                real, intent(in) :: shock_num
                type(Ball), intent(inout) :: balls(:)
                integer :: i
                real :: velo1(2)
                real :: velo2(2)
                real :: distance_vec(2)
                real :: distance
                real :: distance_vec_normed(2)
                real :: perp(2)
                ! Collision with ground
                if (this%coords(2) + this%radius >= bounds(2)) then
                        this%velocity(2) = -this%velocity(2)*shock_num
                        this%coords(2) = -this%radius + bounds(2)
                end if
                ! Collision with walls
                if (this%coords(1) - this%radius <= 0) then
                         this%velocity(1) = -this%velocity(1)*shock_num
                         this%coords(1) = this%radius
                 elseif (this%coords(1) + this%radius >= bounds(1)) then
                         this%velocity(1) = -this%velocity(1)*shock_num
                         this%coords(1) = bounds(1) - this%radius
                end if
               ! Collision with other balls
               ! TODO here do concurrent could lead to problems but im not sure
               !do concurrent (i = 1:size(balls))
               !do i = 1, size(balls)
               do concurrent (i = 1:size(balls))
                distance_vec = this%coords - balls(i)%coords
                distance = sqrt(distance_vec(1)**2 + distance_vec(2)**2)
                distance_vec_normed = distance_vec / distance
                perp = -[distance_vec_normed(2), -distance_vec_normed(1)]
                if (distance <= this%radius + balls(i)%radius) then
                        velo2 = balls(i)%velocity
                        velo1 = this%velocity
                         this%velocity = dot_product(velo1, perp)*perp -collions_vel(this, balls(i), velo1, velo2, &
                                 distance_vec_normed, shock_num) * distance_vec_normed
                         balls(i)%velocity = dot_product(velo2, perp)*perp + collions_vel(balls(i), this, velo2, velo1, &
                                 distance_vec_normed, shock_num) * distance_vec_normed
                         this%coords = balls(i)%coords + distance_vec_normed * (this%radius + balls(i)%radius)
                end if
               end do
        end subroutine collision_ball

        pure function collions_vel(ball1, ball2, velo1, velo2, distance_vec_normed, shock_num) result(velocity)
                type(Ball), intent(in) :: ball1, ball2
                real, intent(in) :: velo1(2)
                real, intent(in) :: velo2(2)
                real, intent(in) :: distance_vec_normed(2)
                real, intent(in) :: shock_num
                real :: velocity
                real :: mass1
                real :: mass2
                real :: v1
                real :: v2
                mass1 = ball1%mass
                mass2 = ball2%mass
                v1 = dot_product(velo1, distance_vec_normed)
                v2 = dot_product(velo2, distance_vec_normed)

                velocity = (mass1*v1 + mass2*v2 - shock_num*mass2*(v1 - v2)) / (mass1 + mass2)
        end function collions_vel
        
end module class_Ball
