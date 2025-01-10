module time_scheme_mod

    use flux_mod
    implicit none

    public time_step

    
contains

    subroutine advance(df, Un, Wn, Unp1)

        !In
        real(pr), dimension(:,:), intent(in) :: Un, Wn

        !Inout
        type(DataType), intent(inout) :: df

        !Out
        real(pr), dimension(df%Nx,2), intent(out) :: Unp1

        !Local
        integer  :: e, i
        real(pr) :: lambda, He, Xe
        real(pr), dimension(2) :: Src, Fe
        real(pr), dimension(df%Nx-1)    :: H, X
        real(pr), dimension(df%Nx-1, 2) :: F


        ! Unp1 = Un

        SELECT CASE(df%ordre)

            CASE(1) ! -- Ordre 1

                ! -- Edge loop
                do e=1,df%Nx-1

                    ! Fe = flux_num(df, Un(e,:), Un(e+1,:))
                    Fe = flux_num(df, Wn(e,:), Wn(e+1,:))
                    He = H_bar(Wn(e,:), Wn(e+1,:), Fe)
                    Xe = X_bar(Wn(e,:), Wn(e+1,:), Un(e,:), Un(e+1,:), Fe)

                    F(e,:) = Fe(:)
                    H(e)   = He
                    X(e)   = Xe

                enddo

                df%dt  = time_step(df, Wn, F, H)
                lambda = df%dt/df%dx

                ! -- Celle loop
                do i=2,df%Nx-1

                    Src(1) = 0.0_pr
                    Src(2) = H(i-1)*H(i)*(X(i) - X(i-1))


                    ! Unp1(i,:) = Unp1(i,:) - lambda*(F(i+1,:) - F(i,:))
                    Unp1(i,:) = Un(i,:) - lambda*(X(i)*F(i,:) - X(i-1)*F(i-1,:)) &
                                + 0.5_pr*grav*lambda*Src(:)

                enddo

                Unp1(1,:) = Unp1(2,:)
                Unp1(df%Nx,:) = Unp1(df%Nx-1,:) 

            CASE DEFAULT
                print*, "No such order case yet coded"
                stop

        END SELECT


    end subroutine advance


    function time_step(df, Un, Fn, H) result(dt)
        
        !In
        type(DataType), intent(in)           :: df
        real(pr), dimension(:,:), intent(in) :: Un, Fn
        real(pr), dimension(:), intent(in)   :: H
        

        !Out
        real(pr) :: dt

        !Local
        integer  :: i
        real(pr) :: vp, lambda, vp_max

        lambda = df%cfl*df%dx
        vp_max = 0._pr

        SELECT CASE(df%ordre)

            CASE(1) ! -- Ordre 1

                do i=1,df%Nx-1

                    vp = eigen_value(Un(i,:), Un(i+1,:))
                    if (vp_max < vp) vp_max = vp

                enddo

                dt = lambda/(2._pr*vp_max)

                do i=2,df%Nx-1

                    vp = max(0.0_pr, Fn(i,1)) - min(0.0_pr, Fn(i-1,1))
                    if (dt*df%dx*vp > H(i)) then
                        print*, "Error: Second CFL condition unrespected!"
                        stop
                    endif

                enddo


            CASE DEFAULT
                print*, "No such order case yet coded"
                stop

        END SELECT

    end function time_step
    
end module time_scheme_mod