MODULE scatter

 USE types

 IMPLICIT NONE

 SAVE

 TYPE (scatter_type), ALLOCATABLE ::     scat_data(:,:),    &
                                     all_scat_data(:,:)

 INTEGER, ALLOCATABLE :: all_par_active(:,:,:)

 CONTAINS

 !
 !
 !

 SUBROUTINE allocate_scatter(len_scat,maxper)

 USE data

 IMPLICIT NONE

 INTEGER, INTENT(IN) :: len_scat,maxper

 INTEGER :: i,j

 ! All station scatter plot array 
 ALLOCATE(scat_data(nparver,maxper))

 DO j=1,maxper
    DO i=1,nparver
       ALLOCATE(scat_data(i,j)%dat(nexp+1,len_scat))
    ENDDO
 ENDDO
 scat_data%n = 0

 IF (lallstat) THEN
    ALLOCATE(all_scat_data(nparver,maxper))
    DO j=1,maxper
       DO i=1,nparver
          ALLOCATE(all_scat_data(i,j)%dat(nexp+1,active_stations*len_scat))
       ENDDO
    ENDDO
    all_scat_data%n = 0
 ENDIF

 ALLOCATE(all_par_active(maxper,maxstn,nparver))
 all_par_active = 0

 END SUBROUTINE allocate_scatter
 SUBROUTINE deallocate_scatter(maxper,nparver)

  IMPLICIT NONE

  INTEGER, INTENT(IN) :: maxper,nparver

  INTEGER :: i,j
  
! DO j=1,maxper
!    DO i=1,nparver
     !IF ( ALLOCATED(scat_data(i,j))     &
     !DEALLOCATE(scat_data(i,j)%dat)
     !IF ( ASSOCIATED(all_scat_data(i,j)%dat)) &
     !DEALLOCATE(all_scat_data(i,j)%dat)
     !ENDDO
  !ENDDO

  IF (ALLOCATED(scat_data))      DEALLOCATE(scat_data     )
  IF (ALLOCATED(all_scat_data))  DEALLOCATE(all_scat_data )
  IF (ALLOCATED(all_par_active)) DEALLOCATE(all_par_active)

  RETURN

 END SUBROUTINE deallocate_scatter

END MODULE scatter
