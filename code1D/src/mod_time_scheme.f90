module time_scheme_mod

    use flux_mod
    use topography_mod
    use musl_mod
    use polynom_mod
    implicit none

    public time_step

    interface time_step
        module procedure dt_ordre1
        module procedure dt_ordre2
    end interface

    
contains

    subroutine advance(df, tn, Xab, Un, Wn, Topo)

        !In
        
        real(pr), dimension(:), intent(in)   :: Xab
        real(pr), dimension(:,:), intent(inout) :: Un

        !Inout
        type(DataType), intent(inout) :: df
        real(pr), intent(inout)       :: tn
        real(pr), dimension(df%Nx,2), intent(inout) :: Wn


        !Out
        !real(pr), dimension(df%Nx,2), intent(out) :: Unp1
        real(pr), dimension(df%Nx), intent(out)   :: Topo

        !Local
        integer  :: e, i
        real(pr) :: lambda, He, Xe
        real(pr), dimension(2) :: Src, Fe, phi, Fl, Fr 
        real(pr), dimension(df%Nx-1)    :: H, X
        real(pr), dimension(df%Nx-1, 2) :: F
        real(pr), dimension(1:df%Nx, 2) :: Upl, Upr, Wpg, Wpd



        !Unp1 = Un

        SELECT CASE(df%ordre)

            CASE(1) ! -- Ordre 1

                df%dt  = time_step(df, Wn)
                lambda = df%dt/df%dx

                ! -- Edge loop
                do e=1,df%Nx-1

                    !Fe = flux_num(df, Un(e,:), Un(e+1,:))
                    Fe = flux_num(df, Wn(e,:), Wn(e+1,:))
                    He = H_bar(Wn(e,:), Wn(e+1,:), Fe)
                    Xe = X_bar(Wn(e,:), Wn(e+1,:), Un(e,:), Un(e+1,:), Fe)

                    F(e,:) = Fe(:)
                    H(e)   = He
                    X(e)   = Xe

                enddo

                call cfl2(df, F, H)

                ! -- Celle loop
                do i=2,df%Nx-1

                    Src(1) = 0.0_pr
                    Src(2) = H(i-1)*H(i)*(X(i) - X(i-1))


                    !Un(i,:) = Un(i,:) - lambda*(F(i,:) - F(i-1,:))
                    Un(i,:) = Un(i,:) - lambda*(X(i)*F(i,:) - X(i-1)*F(i-1,:)) &
                                + 0.5_pr*grav*lambda*Src(:)

                enddo

                Un(1,:) = Un(2,:)
                Un(df%Nx,:) = Un(df%Nx-1,:) 

            CASE(2) ! -- Ordre 2 (MUSCL-Hancock scheme)

                !call apply_poly(df, Un, Upg, Upd)
                !print*, "Upg: ", Upg

                df%dt  = time_step(df, Un)
                lambda = df%dt/df%dx

                do i=2,df%Nx-1
                    
                    phi = MINMOD(Un(i-1,:), Un(i,:), Un(i+1,:))
                    
                    Upl(i,:) = Un(i,:) - 0.5_pr * phi(:)
                    Upr(i,:) = Un(i,:) + 0.5_pr * phi(:)

                    Fl = flux_exact(Upl(i,:))
                    Fr = flux_exact(Upr(i,:))

                    Upl(i,:) = Upl(i,:) + 0.5_pr*lambda*(Fl - Fr)
                    Upr(i,:) = Upr(i,:) + 0.5_pr*lambda*(Fl - Fr)

                enddo

                Upr(1,:) = Upr(2,:)
                Upl(df%Nx,:) = Upl(df%Nx-1,:)

                do e=1,df%Nx-1

                    Fe = flux_num(df, Upr(e,:), Upl(e+1,:))

                    F(e,:) = Fe(:)
                enddo

                ! Wpg = sol_rewrite(Upg, Topo)
                ! Wpd = sol_rewrite(Upd, Topo)

                ! -- Celle loop
                do i=2,df%Nx-1

                    ! Src(1) = 0.0_pr
                    ! Src(2) = H(i-1)*H(i)*(X(i) - X(i-1))


                    Un(i,:) = Un(i,:) - lambda*(F(i,:) - F(i-1,:))
                    ! Un(i,:) = Un(i,:) - lambda*(X(i)*F(i,:) - X(i-1)*F(i-1,:)) &
                    !             + 0.5_pr*grav*lambda*Src(:)

                enddo

                Un(1,:) = Un(2,:)
                Un(df%Nx,:) = Un(df%Nx-1,:) 

            CASE DEFAULT
                print*, "No such order case yet coded"
                stop

        END SELECT

        tn = tn + df%dt
        Topo = topo_fct(df, Xab, tn)
        Wn = sol_rewrite(Un, Topo)


    end subroutine advance


    function dt_ordre1(df, Un) result(dt)
        
        !In
        type(DataType), intent(in)           :: df
        real(pr), dimension(:,:), intent(in) :: Un        

        !Out
        real(pr) :: dt

        !Local
        integer  :: i
        real(pr) :: vp, lambda, vp_max

        lambda = df%cfl*df%dx
        vp_max = 0._pr

        do i=1,df%Nx-1

            vp = eigen_value(Un(i,:), Un(i+1,:))
            if (vp_max < vp) vp_max = vp

        enddo

        dt = lambda/(vp_max)

    end function dt_ordre1


    function dt_ordre2(df, Upg, Upd) result(dt)
        
        !In
        type(DataType), intent(in)           :: df
        real(pr), dimension(:,:), intent(in) :: Upg, Upd
        

        !Out
        real(pr) :: dt

        !Local
        integer  :: i
        real(pr) :: vp, lambda, vp_max

        lambda = df%cfl*df%dx
        vp_max = 0._pr

        do i=1,df%Nx-2

            vp = eigen_value(Upg(i,:), Upd(i,:))
            if (vp_max < vp) vp_max = vp

        enddo

        dt = lambda/(vp_max)

    end function dt_ordre2


    subroutine cfl2(df, Fn, H)
        
        !In
        type(DataType), intent(in)           :: df
        real(pr), dimension(:,:), intent(in) :: Fn
        real(pr), dimension(:), intent(in)   :: H

        !Local
        integer  :: i
        real(pr) :: vp


        do i=2,df%Nx-1

            vp = max(0.0_pr, Fn(i,1)) - min(0.0_pr, Fn(i-1,1))
            if (df%dt*df%dx*vp > H(i)) then
                print*, "Error: Second CFL condition unrespected!"
                stop
            endif

        enddo

    end subroutine cfl2
    
end module time_scheme_mod