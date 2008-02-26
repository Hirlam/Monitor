SUBROUTINE selection(flag)

!
!  Station selection
!

 USE DATA

 IMPLICIT NONE

 INTEGER :: i,ii,flag,			&
            stnr(0:10000000)

!---------------------------------

 ! Fill stationlist if empty

 ii = 0
 IF ( ALL(stnlist == 0) ) THEN
    DO i=1,maxstn
      IF (hir(i)%stnr/=0)  THEN
         ii = ii + 1
         stnlist(ii) = hir(i)%stnr
      ENDIF
    ENDDO
 ENDIF

 !
 ! Make index array of station numbers
 !

 stnr = 0

 IF (flag.EQ.1) THEN
    ii = TRANSFER(MINLOC(stnlist),ii) - 1
    DO i=1,ii
       stnr(stnlist(i)) = i
    ENDDO
 ELSEIF (flag.EQ.2) THEN
    ii = TRANSFER(MINLOC(stnlist_plot),ii) - 1
    DO i=1,ii
       stnr(stnlist_plot(i)) = i
    ENDDO
 ELSE
    WRITE(6,*)'STUPID PROGRAMMER ERROR FLAG IN SELECTION ',flag
 ENDIF

 !
 ! Reset active stations
 !

 hir%active=.FALSE.

 !
 ! Station list selection 
 !

 IF (sum(stnr(1:)) > 0) THEN 
    DO i=1,maxstn
      IF (stnr(hir(i)%stnr)/=0) hir(i)%active = .TRUE.
    ENDDO
 ENDIF

 !
 ! Box selection
 !

 IF(cbox%active) THEN
   DO i=1,maxstn
   hir(i)%active = 				&
      (hir(i)%lat .GE. cbox%slat .AND.		&
       hir(i)%lat .LE. cbox%nlat .AND.		&
       hir(i)%lon .GE. cbox%wlon .AND.		&
       hir(i)%lon .LE. cbox%elon      )
   ENDDO
 ENDIF

 !
 ! Polygon selection
 !

 IF (lpoly) CALL polygon_selection


 !
 ! Selection info
 !

 IF (lprint_selection) THEN

    WRITE(6,*)
    WRITE(6,*)'Stations after selection :',COUNT(hir(1:maxstn)%active)
    WRITE(6,*)

   DO i=1,maxstn
      IF(hir(i)%active) WRITE(6,*)hir(i)%stnr,hir(i)%lat,hir(i)%lon,obs(i)%active
   ENDDO

 ENDIF

 !
 ! Black listed stations
 !

 IF ( SUM(stnlist_bl) > 0 ) THEN

    stnr = 0
    ii = TRANSFER(MINLOC(stnlist_bl),ii) - 1
    WRITE(6,*)'Number of stations to blacklist',ii
    DO i=1,ii
       stnr(stnlist_bl(i)) = i
    ENDDO
    DO i=1,maxstn
      IF (stnr(hir(i)%stnr) /= 0) THEN
         hir(i)%active = .FALSE.
         WRITE(6,*)'Blacklisted ',hir(i)%stnr
      ENDIF 
    ENDDO

 ENDIF

 CALL station_summary

 RETURN

END
