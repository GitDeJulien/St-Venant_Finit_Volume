module save_out_mod

    use data_reader
    use topography_mod
    implicit none

    public save_approx_sol, save_exact_sol, save_topography
    
contains

    subroutine save_approx_sol(df, X, iter, Un, Wn)

        !In
        type(DataType), intent(in)           :: df
        real(pr), dimension(:), intent(in)   :: X
        integer, intent(in)                  :: iter
        real(pr), dimension(:,:), intent(in) :: Un, Wn

        !Local
        integer :: i, io, ios
        character(len=125) :: ch_iter

        write(ch_iter, '(I5)') iter

        open(newunit=io, file="output/sol1D/sol."//trim(adjustl(ch_iter))//".dat",&
                status="replace", action="write", iostat=ios)

            if (ios /= 0) then
                print *, 'Error opening file: ', " output/sol1D/sol."
                stop
            end if

            write(io, *) "## xi  ", "  hi  ", "  Hi ", "  ui"
            do i=1,df%Nx
                write(io,*) X(i), Un(i,1), Wn(i,1), Un(i,2)/Un(i,1)
            enddo

        close(io)

    end subroutine save_approx_sol


    subroutine save_exact_sol(df, X, iter, Uexact, Topo)

        !In
        type(DataType), intent(in)           :: df
        real(pr), dimension(:), intent(in)   :: X
        integer, intent(in)                  :: iter
        real(pr), dimension(:,:), intent(in) :: Uexact
        real(pr), dimension(:), intent(in)   :: Topo

        !Local
        integer :: i, io, ios
        character(len=125) :: ch_iter

        write(ch_iter, '(I5)') iter

        open(newunit=io, file="output/exact1D/sol."//trim(adjustl(ch_iter))//".dat",&
                status="replace", action="write", iostat=ios)

            if (ios /= 0) then
                print *, 'Error opening file: ', " output/exact1D/sol."
                stop
            end if

            write(io, *) "## xk  ", "  h_exact  ", "  H_exact  ", "  u_exact"
            do i=1,df%Nx
                write(io,*) X(i), Uexact(i,1), Uexact(i,1) + Topo(i), Uexact(i,2)
            enddo

        close(io)

    end subroutine save_exact_sol

    subroutine save_topography(data, X, iter, Topo)
        !In
        type(DataType), intent(in)          :: data
        real(pr), dimension(:), intent(in)  :: X
        integer, intent(in)                 :: iter
        ! real(pr), intent(in)                :: tn
        real(pr), dimension(:), intent(in)  :: Topo

        !Local
        integer :: k, ios
        character(len=256) :: topo_filename
        character(len=10)   :: ch

        WRITE(ch, '(I5)') iter
        topo_filename = 'output/topo1D/topo.' // trim(adjustl(ch)) // '.dat'

        open(unit=10, file=topo_filename, status='REPLACE', action='WRITE', iostat=ios)

            if (ios /= 0) then
                print *, 'Error opening file: ', ' output/topo1D/topo.'
                stop
            end if

            write(10,*) "## xk ", " topo"
            do k=1,data%Nx
                ! write(10,*) X(k), topography1D(data, X(K), tn)
                write(10,*) X(k), Topo(k)
            end do

        close(10)

    end subroutine save_topography

    subroutine save_error(df, Uexact, Un, iter, io)

        !In
        type(DataType), intent(in) :: df
        real(pr), dimension(:,:), intent(in) :: Uexact, Un
        integer, intent(in) :: iter, io

        !Local
        integer  :: i
        real(pr) :: errorL1, errorL2
        real(pr) :: s1!, s2

        s1 = 0.d0
        ! s2 = 0.d0
        do i=1,df%Nx
            s1 = s1 + df%dx*(Uexact(i,1)-Un(i,1))*(Uexact(i,1)-Un(i,1))
            ! s2 = s2 + Uexact(i,1)*Uexact(i,2)
        enddo
        errorL2 = SQRT(s1)

        do i=1,df%Nx
            s1 = s1 + abs(Uexact(i,1)-Un(i,1))
            !s2 = s2 + abs(Uexact(i,1))
        enddo
        errorL1 = df%dx*(s1)

        ! errorL2 = SUM((Uexact(1:196,1)-Un(1:196,1))**2)/SUM(Uexact(1:196,1)**2)
        ! errorL2 = SQRT(errorL2)

        ! errorL1 = SUM(ABS(Uexact(1:196,1)-Un(1:196,1))/ABS(Uexact(1:196,1)))
        ! errorL1 = 1._pr/df%Nx*(errorL1)


        !print*, "L2 error: ", errorL2

        write(io, *) errorL1, errorL2

        if (iter == df%niter) close(io)

    end subroutine save_error
    
end module save_out_mod