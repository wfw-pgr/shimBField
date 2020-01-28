program main
  use variablesMod
  use ioUtilityMod, only : load__parameters, load__bfieldposition, save__results
  use shimFieldMod, only : calc__shimField
  implicit none
  integer :: k
  call load__parameters
  call load__bfieldposition
  call calc__shimField( bfield, mvec, r1, r2, p1, p2, z1, z2, LI, LJ, LK, nBpt )
  call save__results
  
end program main
