module ioUtilityMod
contains


  ! ====================================================== !
  ! === load parameters from file                      === !
  ! ====================================================== !
  subroutine load__parameters
    use variablesMod
    implicit none
    character(cLen)             :: cmt
    character(cLen)             :: phiUnit
    double precision, parameter :: twopi = 8.d0*atan(1.d0)

    ! ------------------------------------------------------ !
    ! --- [1] load parameter file                        --- !
    ! ------------------------------------------------------ !
    open(lun,file=trim(prmFile),form='formatted')
    read(lun,*) cmt, cmt, LI
    read(lun,*) cmt, cmt, LJ
    read(lun,*) cmt, cmt, LK
    read(lun,*) cmt, cmt, r1
    read(lun,*) cmt, cmt, r2
    read(lun,*) cmt, cmt, p1
    read(lun,*) cmt, cmt, p2
    read(lun,*) cmt, cmt, z1
    read(lun,*) cmt, cmt, z2
    read(lun,*) cmt, cmt, mvec(1)
    read(lun,*) cmt, cmt, mvec(2)
    read(lun,*) cmt, cmt, mvec(3)
    read(lun,*) cmt, cmt, phiUnit
    close(lun)
    ! ------------------------------------------------------ !
    ! --- [2] check parameters                           --- !
    ! ------------------------------------------------------ !
    write(6,'(a)'                 ) '[load__parameters@ioUtilityMod] check parameters '
    write(6,'(6x,a12,a4,1x,i8)'   ) 'LI'     , ' :: ', LI
    write(6,'(6x,a12,a4,1x,i8)'   ) 'LJ'     , ' :: ', LJ
    write(6,'(6x,a12,a4,1x,i8)'   ) 'LK'     , ' :: ', LK
    write(6,'(6x,a12,a4,1x,f10.5)') 'r1'     , ' :: ', r1
    write(6,'(6x,a12,a4,1x,f10.5)') 'r2'     , ' :: ', r2
    write(6,'(6x,a12,a4,1x,f10.5)') 'p1'     , ' :: ', p1
    write(6,'(6x,a12,a4,1x,f10.5)') 'p2'     , ' :: ', p2
    write(6,'(6x,a12,a4,1x,f10.5)') 'z1'     , ' :: ', z1
    write(6,'(6x,a12,a4,1x,f10.5)') 'z2'     , ' :: ', z2
    write(6,'(6x,a12,a4,1x,f10.5)') 'mx'     , ' :: ', mvec(1)
    write(6,'(6x,a12,a4,1x,f10.5)') 'my'     , ' :: ', mvec(2)
    write(6,'(6x,a12,a4,1x,f10.5)') 'mz'     , ' :: ', mvec(3)
    write(6,'(6x,a12,a4,1x,a10)'  ) 'phiUnit', ' :: ', phiUnit
    ! ------------------------------------------------------ !
    ! --- [3] unit conversion (phi)                      --- !
    ! ------------------------------------------------------ !
    if ( trim(phiUnit).eq.'degree' ) then
       p1 = p1 / 360.d0 * twopi
       p2 = p2 / 360.d0 * twopi
    endif
    
    return
  end subroutine load__parameters
  

  ! ====================================================== !
  ! === load bfield position from file                 === !
  ! ====================================================== !
  subroutine load__bfieldposition
    use variablesMod
    implicit none
    integer :: iB

    ! ------------------------------------------------------ !
    ! --- [1] preparation                                --- !
    ! ------------------------------------------------------ !
    call countLines( trim(bptFile), nBpt )
    allocate( BField(6,nBpt) )
    BField(:,:) = 0.d0
    ! ------------------------------------------------------ !
    ! --- [2] load bfieldposition                        --- !
    ! ------------------------------------------------------ !
    open(lun,file=trim(bptFile),status='old',form='formatted')
    read(lun,*)
    do iB=1, nBpt
       read(lun,*) BField(1:3,iB)
    enddo
    close(lun)
    return
  end subroutine load__bfieldposition


  ! ====================================================== !
  ! === save calculated bfield results into file       === !
  ! ====================================================== !
  subroutine save__results
    use variablesMod
    implicit none
    integer         :: iB
    character(cLen) :: fmt = '(6(e12.5,1x))'

    open(lun,file=trim(outFile),form='formatted')
    write(lun,*) '# xp yp zp bx by bz'
    do iB=1, nBpt
       write(lun,trim(fmt)) bfield(:,iB)
    enddo
    close(lun)
    write(6,*) '[save__results]  results saved in... ', trim(outFile)
    
    return
  end subroutine save__results


  ! ====================================================== !
  ! === count up Number of Lines in the file           === !
  ! ====================================================== !
  subroutine countLines( FileName, count )
    implicit none
    integer     , intent(out) :: count
    character(*), intent(in)  :: FileName
    integer                   :: ios     = 0
    integer     , parameter   :: lun     = 50
    integer     , parameter   :: cLenMax = 50000
    character(1), parameter   :: comment = '#'
    character(1)              :: topchar
    character(cLenMax)        :: buffer

    count = 0
    open(lun,file=trim(FileName),status='old')
    do
       read(lun,*,iostat=ios) buffer
       topchar = ( adjustL( buffer ) )
       if ( topchar.eq.comment ) cycle
       if ( ios.lt.0 ) exit
       count = count + 1
    enddo
    close(lun)
    return
  end subroutine countLines

  
end module ioUtilityMod
