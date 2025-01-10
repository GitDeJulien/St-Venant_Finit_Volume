module time_scheme_mod

    use flux_mod
    implicit none

    public time_step

    
contains

    function advance(df, Un, Wn) result(Unp1)

        !In
        type(DataType), intent(in)           :: df
        real(pr), dimension(:,:), intent(in) :: Un, Wn

        !Out
        real(pr), dimension(df%n_celle,2) :: Unp1

        !Local
        integer  :: k !,e
        ! real(pr)               :: Xe, Hd, Hg, lambda, He
        ! real(pr), dimension(2) :: Fg, Fd, Fe, Sg, Sd, S
        real(pr)               :: Xg, Xd, Hd, Hg, lambda
        real(pr), dimension(2) :: Fg, Fd, S
        real(pr), dimension(df%n_celle-1) :: bi

        bi = eigen_values(df, Un)

        !Unp1 = Un
        Unp1 = 0.0_pr

        lambda = df%dt/df%dx
        

        SELECT CASE(df%ordre)

            CASE(1) ! -- Ordre 1

                ! Fg = flux_num(Wn(1,:), Wn(1,:), bi(1))
                ! Hg = h_bar(Wn(1,:), Wn(1,:), Fg)
                ! He = h_bar(Wn(1,:), Wn(2,:), Fd)
                
                ! ! Internal edge loop
                ! !do k=2,df%n_celle-1
                ! do e=2,df%n_edge-1

                !     ! -- Edge flux
                !     Fe = flux_num(Wn(e-1,:), Wn(e,:), bi(e-1))

                !     Xe = x_bar(Wn(e-1,:), Wn(e,:), Un(e-1,:), Un(e,:), Fe)

                !     Hd = h_bar(Wn(e,:), Wn(e+1,:), Fe)

                !     Sg(1) = 0._pr
                !     Sg(2) = Hg*He*Xe

                !     Sd(1) = 0._pr
                !     Sd(2) = He*Hd*Xe

                !     Unp1(e-1,:) = Unp1(e-1,:) - lambda*Xe*Fe(:) + 0.5_pr*grav*lambda*Sg(:)
                !     Unp1(e,:) = Unp1(e,:) + lambda*Xe*Fe(:) - 0.5_pr*grav*lambda*Sd(:)

                !     Hg = He
                !     He = Hd

                ! enddo

                ! ! -- Boundary conditions: Homogenus Neumann
                ! Unp1(1,:) = Unp1(2,:)
                ! Unp1(df%n_celle,:) = Unp1(df%n_celle-1,:)

                Fg = flux_num(df, Wn(1,:), Wn(1,:), bi(1))
                Fd = flux_num(df, Wn(1,:), Wn(2,:), bi(1))
                Hg = h_bar(Wn(1,:), Wn(1,:), bi(1))
                Hd = h_bar(Wn(1,:), Wn(2,:), bi(1))
                Xg = x_bar(Wn(1,:), Wn(1,:), Un(1,:), Un(1,:), Fg)
                Xd = x_bar(Wn(1,:), Wn(2,:), Un(1,:), Un(2,:), Fd)
                S(1) = 0._pr
                S(2) = Hg*Hd*(Xd - Xg)

                Unp1(1,:) = Un(1,:) - lambda*(Xd*Fd(:) - Xg*Fg(:)) + 0.5_pr*grav*lambda*S(:)
                ! Unp1(1,:) = Unp1(1,:) + 0.5_pr*grav*lambda*S(:)
                ! Unp1(1,:) = Unp1(1,:) - lambda*(Fd(:) - Fg(:))
                

                do k=2,df%n_celle-1

                    Fg = flux_num(df, Wn(k-1,:), Wn(k,:), bi(k-1))
                    Fd = flux_num(df, Wn(k,:), Wn(k+1,:), bi(k))
                    Hg = h_bar(Wn(k-1,:), Wn(k,:), bi(k-1))
                    Hd = h_bar(Wn(k,:), Wn(k+1,:), bi(k))
                    Xg = x_bar(Wn(k-1,:), Wn(k,:), Un(k-1,:), Un(k,:), Fg)
                    Xd = x_bar(Wn(k,:), Wn(k+1,:), Un(k,:), Un(k+1,:), Fd)
                    S(1) = 0._pr
                    S(2) = Hg*Hd*(Xd - Xg)

                    Unp1(k,:) = Un(k,:) - lambda*(Xd*Fd(:) - Xg*Fg(:)) + 0.5_pr*grav*lambda*S(:)

                    ! Fg = flux_num(df, Un(k-1,:), Un(k,:), bi(k-1))
                    ! Fd = flux_num(df, Un(k,:), Un(k+1,:), bi(k))
                    ! Unp1(k,:) = Unp1(k,:) - lambda*(Fd(:) - Fg(:))


                enddo

                Fg = flux_num(df, Wn(df%n_celle-1,:), Wn(df%n_celle,:), bi(df%n_celle-1))
                Fd = flux_num(df, Wn(df%n_celle,:), Wn(df%n_celle,:), bi(df%n_celle-1))
                Hg = h_bar(Wn(df%n_celle-1,:), Wn(df%n_celle,:), bi(df%n_celle-1))
                Hd = h_bar(Wn(df%n_celle,:), Wn(df%n_celle,:), bi(df%n_celle-1))
                Xg = x_bar(Wn(df%n_celle-1,:), Wn(df%n_celle,:), Un(df%n_celle-1,:), Un(df%n_celle,:), Fg)
                Xd = x_bar(Wn(df%n_celle,:), Wn(df%n_celle,:), Un(df%n_celle,:), Un(df%n_celle,:), Fd)
                S(1) = 0._pr
                S(2) = Hg*Hd*(Xd - Xg)

                ! Unp1(df%n_celle,:) = Unp1(df%n_celle,:) + 0.5_pr*grav*lambda*S(:)

                Unp1(df%n_celle,:) = Un(df%n_celle,:) - lambda*(Xd*Fd(:) - Xg*Fg(:)) + 0.5_pr*grav*lambda*S(:)
                !Unp1(df%n_celle,:) = Unp1(df%n_celle,:) - lambda*(Fd(:) - Fg(:))
                ! Unp1(1,:) = Un(2,:)
                ! Unp1(df%n_celle,:) = Un(df%n_celle-1,:)


            CASE DEFAULT
                print*, "No such order case yet coded"
                stop

        END SELECT


    end function advance


    function time_step(df, Un) result(dt)
        
        !In
        type(DataType), intent(in)           :: df
        real(pr), dimension(:,:), intent(in) :: Un
        

        !Out
        real(pr) :: dt

        !Local
        real(pr), dimension(df%n_celle-1) :: vp

        SELECT CASE(df%ordre)

            CASE(1) ! -- Ordre 1

                vp = eigen_values(df, Un)

                dt = df%cfl*df%dx/(2._pr*MAXVAL(vp))

            CASE DEFAULT
                print*, "No such order case yet coded"
                stop

        END SELECT

    end function time_step
    
end module time_scheme_mod