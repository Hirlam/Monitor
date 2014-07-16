MODULE module_odb_extract
  IMPLICIT NONE

  CHARACTER(LEN=12),ALLOCATABLE,DIMENSION(:):: cstaid_odb
  INTEGER,ALLOCATABLE,DIMENSION(:)          :: iobtyp_odb,icodetype_odb,ivarno_odb
  INTEGER,ALLOCATABLE,DIMENSION(:)          :: ivertco_type_odb,isensor_odb,idate_odb,itime_odb
  INTEGER,ALLOCATABLE,DIMENSION(:)          :: istatus_acthdr_odb,istatus_actbod_odb
  INTEGER,ALLOCATABLE,DIMENSION(:)          :: istatus_blkhdr_odb,istatus_pashdr_odb
  INTEGER,ALLOCATABLE,DIMENSION(:)          :: istatus_rejhdr_odb,istatus_rejbod_odb
  INTEGER,ALLOCATABLE,DIMENSION(:)          :: istatus_blkbod_odb,istatus_pasbod_odb
  INTEGER,ALLOCATABLE,DIMENSION(:)          :: ianflag_odb
  REAL,ALLOCATABLE,DIMENSION(:)             :: rlat_odb,rlon_odb,rpress_odb,randep_odb,rfgdep_odb
  REAL,ALLOCATABLE,DIMENSION(:)             :: biascrl_odb,lsm_odb
  REAL,ALLOCATABLE,DIMENSION(:)             :: rfinoe_odb,robs_odb
  REAL                                      :: minlat=-9999.
  REAL                                      :: minlon=-9999.
  REAL                                      :: maxlat=-9999.
  REAL                                      :: maxlon=-9999.
  REAL                                      :: rlon,rlat
  INTEGER,PARAMETER                         :: nstatus=5
  INTEGER,DIMENSION(nstatus)                :: istatus
  INTEGER,PARAMETER                         :: nlim=4
  INTEGER,DIMENSION(nlim)                   :: ilimits
  INTEGER,PARAMETER                         :: norejlims=15
  REAL,DIMENSION(norejlims)                 :: rejlims
  CONTAINS

  ! Possibility to set rejlims different depending on something....
  SUBROUTINE set_rejlims()
    IMPLICIT NONE

!    rejlims=(/-1.5,-0.5,-0.2,0.0,0.2,0.5,1.5/)
!    rejlims=(/-5.0,-3.0,-1.0,0.0,1.0,3.0,5.0/)
!    rejlims=(/-25.0,-15.0,-5.0,0.0,5.0,15.0,25.0/)
    rejlims=(/-25.0,-15.0,-5.0,-3.0,-1.5,-0.5,-0.2,0.0,0.2,0.5,1.5,3.0,5.0,15.0,25.0/)
  END SUBROUTINE set_rejlims

  SUBROUTINE alloc_odb_extracts(icount)
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: icount

    ! Allocate arrays for odb extract
    ALLOCATE(iobtyp_odb(icount))
    ALLOCATE(icodetype_odb(icount))
    ALLOCATE(cstaid_odb(icount))
    ALLOCATE(ivarno_odb(icount))
    ALLOCATE(rlat_odb(icount))
    ALLOCATE(rlon_odb(icount))
    ALLOCATE(ivertco_type_odb(icount))
    ALLOCATE(rpress_odb(icount))
    ALLOCATE(isensor_odb(icount))
    ALLOCATE(idate_odb(icount))
    ALLOCATE(itime_odb(icount))
    ALLOCATE(istatus_acthdr_odb(icount))
    ALLOCATE(istatus_blkhdr_odb(icount))
    ALLOCATE(istatus_pashdr_odb(icount))
    ALLOCATE(istatus_rejhdr_odb(icount))
    ALLOCATE(istatus_actbod_odb(icount))
    ALLOCATE(istatus_blkbod_odb(icount))
    ALLOCATE(istatus_pasbod_odb(icount))
    ALLOCATE(istatus_rejbod_odb(icount))
    ALLOCATE(ianflag_odb(icount))
    ALLOCATE(randep_odb(icount))
    ALLOCATE(rfgdep_odb(icount))
    ALLOCATE(robs_odb(icount))
    ALLOCATE(rfinoe_odb(icount))
    ALLOCATE(biascrl_odb(icount))
    ALLOCATE(lsm_odb(icount))

  END SUBROUTINE alloc_odb_extracts

  SUBROUTINE dealloc_odb_extracts()
    IMPLICIT NONE 

    ! De-allocate temporary arrays used for odb
    DEALLOCATE(iobtyp_odb)
    DEALLOCATE(icodetype_odb)
    DEALLOCATE(cstaid_odb)
    DEALLOCATE(ivarno_odb)
    DEALLOCATE(rlat_odb)
    DEALLOCATE(rlon_odb)
    DEALLOCATE(ivertco_type_odb)
    DEALLOCATE(rpress_odb)
    DEALLOCATE(isensor_odb)
    DEALLOCATE(idate_odb)
    DEALLOCATE(itime_odb)
    DEALLOCATE(randep_odb)
    DEALLOCATE(rfgdep_odb)
    DEALLOCATE(biascrl_odb)
    DEALLOCATE(lsm_odb)
    DEALLOCATE(robs_odb)
    DEALLOCATE(rfinoe_odb)
    DEALLOCATE(istatus_acthdr_odb)
    DEALLOCATE(istatus_actbod_odb)
    DEALLOCATE(istatus_blkhdr_odb)
    DEALLOCATE(istatus_pashdr_odb)
    DEALLOCATE(istatus_rejhdr_odb)
    DEALLOCATE(istatus_blkbod_odb)
    DEALLOCATE(istatus_pasbod_odb)
    DEALLOCATE(istatus_rejbod_odb)
    DEALLOCATE(ianflag_odb)

  END SUBROUTINE dealloc_odb_extracts

END MODULE module_odb_extract
