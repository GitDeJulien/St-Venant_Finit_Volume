module flux_mod

    use data_reader
    implicit none

    public flux_num, eigen_value, sol_rewrite
    
contains

    function flux_num(df, Ug, Ud) result(Fnum)

        !In
        type(DataType), intent(in)         :: df
        real(pr), dimension(2), intent(in) :: Ug, Ud

        !Out
        real(pr), dimension(2) :: Fnum

        !Local
        real(pr)               :: Se, Sl, Sr
        real(pr) :: Hroe, Uroe
        real(pr), dimension(2) :: vp_ip12, d_alpha, phi_ip12
        real(pr), dimension(2) :: Fg, Fd
        real(pr), dimension(2,2) :: L_ip12, R_ip12

        !! U(1) = h     F(U)(1) = q
        !! U(2) = q     F(U)(2) = q**2/h + g(h**2)/2

        SELECT CASE(df%Riemann_solv)
        CASE(1)

            Se = max(abs(Ug(2)/Ug(1))+SQRT(grav*Ug(1)), abs(Ud(2)/Ud(1))+SQRT(grav*Ud(1)))

            Fnum(1) = 0.5_pr*(Ud(2) + Ug(2)) - 0.5_pr*Se*(Ud(1) - Ug(1))
            Fnum(2) = 0.5_pr*(Ud(2)*Ud(2)/Ud(1) + 0.5_pr*grav*Ud(1)*Ud(1) &
                            + Ug(2)*Ug(2)/Ug(1) + 0.5_pr*grav*Ug(1)*Ug(1)) - 0.5_pr*Se &
                            * (Ud(2) - Ug(2))
        CASE(2)

            Sl = min(Ug(2)/Ug(1)-SQRT(grav*Ug(1)), Ud(2)/Ud(1)-SQRT(grav*Ud(1)))
            Sr = max(Ud(2)/Ud(1)+SQRT(grav*Ud(1)), Ug(2)/Ug(1)+SQRT(grav*Ug(1)))

            if (Sl >= 0.0_pr) then
                Fnum(1) = Ug(2)
                Fnum(2) = Ug(2)*Ug(2)/Ug(1) + 0.5_pr*grav*Ug(1)*Ug(1)

            elseif (Sr <= 0.0_pr) then
                Fnum(1) = Ud(2)
                Fnum(2) = Ud(2)*Ud(2)/Ud(1) + 0.5_pr*grav*Ud(1)*Ud(1)

            else

                Fnum(1) = (Sr*Ug(2) - Sl*Ud(2) + (Sl*Sr)*(Ud(1) - Ug(1)))/(Sr-Sl)
                Fnum(2) = (Sr*(Ug(2)*Ug(2)/Ug(1) + 0.5_pr*grav*Ug(1)*Ug(1)) &
                                - Sl*(Ud(2)*Ud(2)/Ud(1) + 0.5_pr*grav*Ud(1)*Ud(1)) &
                                + (Sl*Sr) * (Ud(2) - Ug(2)))/(Sr-Sl)

            endif

        CASE(3) ! 1st order Roe Scheme

            Fg(:) = flux_exact(Ug(:))
            Fd(:) = flux_exact(Ud(:))
                    
            Hroe = 0.5*(sqrt(Ug(1))+sqrt(Ud(1)))
            Uroe = (sqrt(Ug(1))*Ug(2)/Ug(1) + sqrt(Ud(1))*Ud(2)/Ud(1)) / &
                    (sqrt(Ug(1))+sqrt(Ug(1)))
                    
            ! eigenvalues
            vp_ip12(1) = Uroe - sqrt(grav*Hroe)
            vp_ip12(2) = Uroe + sqrt(grav*Hroe)

            ! Left eigenvectors
            L_ip12(1,1) = - 0.5_pr * sqrt(grav/Hroe) 
            L_ip12(1,2) =   0.5_pr
            L_ip12(2,1) =   0.5_pr * sqrt(grav/Hroe) 
            L_ip12(2,2) =   0.5_pr
                    
            ! Riemann invariant
            d_alpha(1) = L_ip12(1,1)*(Ud(1)-Ug(1)) + &
                            L_ip12(1,2)*(Ud(2)-Ug(2))
            d_alpha(2) = L_ip12(2,1)*(Ud(1)-Ug(1)) + &
                            L_ip12(2,2)*(Ud(2)-Ug(2))
                    
            ! Right eigenvectors
            R_ip12(1,1) = - sqrt(Hroe/grav) 
            R_ip12(1,2) =   sqrt(Hroe/grav)
            R_ip12(2,1) =   1.0_pr 
            R_ip12(2,2) =   1.0_pr
                    
            phi_ip12(1) = - 0.5_pr * abs(vp_ip12(1)) * d_alpha(1)
            phi_ip12(2) = - 0.5_pr * abs(vp_ip12(2)) * d_alpha(2)

            Fnum(1) = 0.5_pr*(Fd(1)+Fg(1)) + R_ip12(1,1)*phi_ip12(1) + R_ip12(1,2)*phi_ip12(2)
            Fnum(2) = 0.5_pr*(Fd(2)+Fg(2)) + R_ip12(2,1)*phi_ip12(1) + R_ip12(2,2)*phi_ip12(2)


        CASE DEFAULT
            print*, "Riemann solver key unknown"
            stop
        END SELECT

    end function flux_num

    function H_bar(Wg, Wd, flux) result(H)

        !In
        real(pr), dimension(2), intent(in) :: Wg, Wd
        real(pr), dimension(2), intent(in) :: flux

        !Out
        real(pr) :: H

        if (flux(1) > 0) then
            H = Wg(1)
        else
            H = Wd(1)
        endif

    end function H_bar

    function X_bar(Wg, Wd, Ug, Ud, flux) result(X)

        !In
        real(pr), dimension(2), intent(in) :: Wg, Wd, Ug, Ud
        real(pr), dimension(2), intent(in) :: flux

        !Out
        real(pr) :: X

        if (flux(1) > 0) then
            X = Ug(1)/Wg(1)
        else
            X = Ud(1)/Wd(1)
        endif

    end function X_bar


    function eigen_value(Ug, Ud) result(vp)

        !In
        real(pr), dimension(2), intent(in) :: Ug, Ud

        !Out
        real(pr) :: vp

        vp = max(abs(Ug(2)/Ug(1)+SQRT(grav*Ug(1))), abs(Ud(2)/Ud(1)+SQRT(grav*Ud(1))), &
                abs(Ug(2)/Ug(1)-SQRT(grav*Ug(1))), abs(Ud(2)/Ud(1)-SQRT(grav*Ud(1))))


    end function eigen_value

    function flux_exact(U) result(F)

        !In
        real(pr), dimension(:), intent(in) :: U

        !Out
        real(pr), dimension(2) :: F

        F(1) = U(2)
        F(2) = U(2)*U(2)/U(1) + 0.5_pr*grav*U(1)*U(1)

    end function

    function sol_rewrite(Un, Topo) result(Wn)

        !In
        real(pr), dimension(:,:), intent(in) :: Un
        real(pr), dimension(:), intent(in)   :: Topo


        !Out
        real(pr), dimension(size(Un,1), 2) :: Wn


        Wn(:,1) = Un(:,1) + Topo(:)
        Wn(:,2) = Wn(:,1) * Un(:,2)/Un(:,1)

    end function sol_rewrite
    
end module flux_mod