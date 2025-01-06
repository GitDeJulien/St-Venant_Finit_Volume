module flux_mod

    use data_reader
    implicit none

    public flux_num, eigen_values
    
contains

    function flux_num(df, U, bi, li)result(Fnum)

        !In
        type(DataType)                                 :: df
        real(pr), dimension(df%n_celle, 2), intent(in) :: U
        real(pr), dimension(df%n_celle-1), intent(in)  :: bi
        integer, intent(in)                            :: li

        !Out
        real(pr), dimension(2)             :: Fnum

!!!     U(1) = h          F(U)(1) = q
!!!     U(2) = q          F(U)(2) = q**2/h + g(h**2)/2


        if (df%Riemann_solv == 1) then
            Fnum(1) = 0.5_pr*(U(li+1,2) + U(li,2)) - 0.5_pr*bi(li)*(U(li+1,1) - U(li,1))
            Fnum(2) = 0.5_pr*(U(li+1,2)*U(li+1,2)/U(li+1,1) + 0.5_pr*grav*U(li+1,1)*U(li+1,1) &
                        + U(li,2)*U(li,2)/U(li,1) + 0.5_pr*grav*U(li,1)*U(li,1)) - 0.5_pr*bi(li) &
                        * (U(li+1,2) - U(li,2))
        else
            print*, "Riemann solver key unknown"
            stop
        endif

    end function flux_num


    function eigen_values(df, Un) result(vp)

        !In
        type(DataType), intent(in)                     :: df
        real(pr), dimension(df%n_celle, 2), intent(in) :: Un

        !Out
        real(pr), dimension(df%n_celle-1) :: vp

        !Local
        integer  :: li
        real(pr) :: ug, hg, ud, hd

        do li=1,df%n_celle-1
            ug = Un(li,2)/Un(li,1)
            ud = Un(li+1,2)/Un(li+1,1)
            hg = Un(li,1)
            hd = Un(li+1,1)
            vp(li) = max(abs(ug + sqrt(grav*hg)), abs(ud + sqrt(grav*hd)), abs(ug - sqrt(grav*hg)), abs(ud - sqrt(grav*hd)))
        enddo

    end function eigen_values

    function sol_rewrite(df, Un, Topo) result(Wn)

        !In
        type(DataType), intent(in)           :: df
        real(pr), dimension(:,:), intent(in) :: Un
        real(pr), dimension(:), intent(in)   :: Topo


        !Out
        real(pr), dimension(df%n_celle, 2) :: Wn

        Wn(:,1) = Un(:,1) + Topo(:)
        Wn(:,2) = Wn(:,1) * Un(:,2)/Un(:,1)

    end function sol_rewrite
    
end module flux_mod