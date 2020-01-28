module variablesMod
  implicit none
  integer                       :: LI, LJ, LK, nBpt
  double precision              :: r1, r2, p1, p2, z1, z2
  double precision              :: mvec(3)
  double precision, allocatable :: bfield(:,:)
  integer         , parameter   :: cLen    = 300
  integer         , parameter   ::  lun    =  50
  character(cLen)               :: prmFile = 'dat/parameter.conf'
  character(cLen)               :: outFile = 'dat/shimField.dat'
  character(cLen)               :: bptFile = 'dat/bfieldCoordinate.dat'
  
end module variablesMod
