MODULE scatter

 ! 
 ! Storage for scatter array
 !
 ! Ulf Andrae, SMHI, 2007
 ! 

 USE types

 IMPLICIT NONE

 SAVE

 TYPE (scatter_type), ALLOCATABLE ::     scat_data(:,:),    &
                                     all_scat_data(:,:)

 INTEGER, ALLOCATABLE :: all_par_active(:,:,:)

 CONTAINS

 !--------------------------------------------
 !--------------------------------------------
 !--------------------------------------------

 SUBROUTINE allocate_scatter(len_scat,maxper)

 USE data

 IMPLICIT NONE

 INTEGER, INTENT(IN) :: len_scat,maxper

 INTEGER :: i,j,allocate_status

 !-----------------------------------------------

 WRITE(6,*)'ALLOCATE SCATTER ARRAY'

 ! All station scatter plot array 
 ALLOCATE(scat_data(nparver,maxper),STAT=allocate_status)
 IF ( allocate_status /= 0 ) THEN
    WRITE(6,*)'Could not allocate scat_data',allocate_status
    WRITE(6,*)'Reduce use_fclen',use_fclen(1:nuse_fclen)
    CALL abort
 ENDIF

 DO j=1,maxper
    DO i=1,nparver
       ALLOCATE(scat_data(i,j)%dat(nexp+1,len_scat),STAT=allocate_status)
       IF ( allocate_status /= 0 ) THEN
          WRITE(6,*)'Could not allocate scat_data(i,j)',i,j,allocate_status
          WRITE(6,*)'Reduce use_fclen',use_fclen(1:nuse_fclen)
          CALL abort
       ENDIF
    ENDDO
 ENDDO
 scat_data%n = 0

 IF (lallstat) THEN
    ALLOCATE(all_scat_data(nparver,maxper))
    DO j=1,maxper
       DO i=1,nparver
          ALLOCATE(all_scat_data(i,j)%dat(nexp+1,active_stations*len_scat),STAT=allocate_status)
          IF ( allocate_status /= 0 ) THEN
             WRITE(6,*)'Could not allocate all_scat_data(i,j)',i,j,allocate_status
             WRITE(6,*)'Reduce use_fclen',use_fclen(1:nuse_fclen)
             CALL abort
          ENDIF
       ENDDO
    ENDDO
    all_scat_data%n = 0
 ENDIF

 ALLOCATE(all_par_active(maxper,maxstn,nparver))
 all_par_active = 0

 END SUBROUTINE allocate_scatter

 !--------------------------------------------
 !--------------------------------------------
 !--------------------------------------------

 SUBROUTINE deallocate_scatter(maxper,nparver)

  IMPLICIT NONE

  INTEGER, INTENT(IN) :: maxper,nparver

  INTEGER :: i,j
  
  IF (ALLOCATED(scat_data)) THEN

     DO j=1,maxper
        DO i=1,nparver
           DEALLOCATE(scat_data(i,j)%dat)
        ENDDO
     ENDDO
     DEALLOCATE(scat_data)

  ENDIF

  IF (ALLOCATED(all_scat_data)) THEN
    DO j=1,maxper
       DO i=1,nparver
          DEALLOCATE(all_scat_data(i,j)%dat)
       ENDDO
    ENDDO
    DEALLOCATE(all_scat_data )
  ENDIF

  IF (ALLOCATED(all_par_active)) DEALLOCATE(all_par_active)

  RETURN

 END SUBROUTINE deallocate_scatter

END MODULE scatter
