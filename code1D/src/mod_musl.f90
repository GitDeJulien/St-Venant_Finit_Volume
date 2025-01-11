module musl_mod

    use precision
    implicit none
    
contains

    function MINMOD(Uim1, Ui, Uip1) result(L_i)

        !In
        real(pr), dimension(:), intent(in) :: Uim1, Ui, Uip1
        
        !Out
        real(pr), dimension(2) :: L_i

        !Local
        real(pr) :: beta
        real(pr), dimension(2) :: L_im1, L_ip1

        L_im1 = Ui - Uim1
        L_ip1 = Uip1 - Ui

        ! -- MINMOD
        beta = 1._pr 

        if (L_ip1(1) > 0) then
            L_i(1) = max(0._pr, min(beta*L_im1(1), L_ip1(1)), min(L_im1(1), beta*L_ip1(1)))
        elseif (L_ip1(1) < 0) then
            L_i(1) = min(0._pr, max(beta*L_im1(1), L_ip1(1)), max(L_im1(1), beta*L_ip1(1)))
        endif

        if (L_ip1(2) > 0) then
            L_i(2) = max(0._pr, min(beta*L_im1(2), L_ip1(2)), min(L_im1(2), beta*L_ip1(2)))
        elseif (L_ip1(2) < 0) then
            L_i(2) = min(0._pr, max(beta*L_im1(2), L_ip1(2)), max(L_im1(2), beta*L_ip1(2)))
        endif
        
        
    end function MINMOD

    ! function von_leer(Un) result(phi)

    !     !In   
    !     real(pr), dimension(:), intent(in) :: theta
    !     real(pr), dimension(:), intent(in) :: theta

    !     !Out
    !     real(pr), dimension(size(Un,1),2) :: phi

    !     phi = (theta + ABS(theta))/(1+ABS(theta))


    ! end function von_leer
    
end module musl_mod