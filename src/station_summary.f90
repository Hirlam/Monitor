SUBROUTINE station_summary

 !
 ! Give summary of the stations used in verobs
 ! Set lprint_summary = T in namelist to get more
 ! comprehensive summary
 !
 ! Ulf Andrae, SMHI, 2004
 !

 USE DATA

 IMPLICIT NONE

 INTEGER, PARAMETER :: maxstn_print = 10

 INTEGER :: k,l,m,n,k_max

 !----------------------------------------------

 m = 0
 n = 0
 k_max = 0

 WRITE(6,*)
 WRITE(6,*)'--- STATION SUMMARY ---'
 WRITE(6,*)

 DO k=1,maxstn
    IF (obs(k)%active.AND.hir(k)%active) THEN

       IF (n < obs(k)%ntim ) THEN
         k_max = k
         n = obs(k)%ntim
       ENDIF

       ! Count number of active stations
       m = m + 1

       IF (maxstn < maxstn_print .OR. lprint_summary ) THEN

          WRITE(6,*)'STATION :',hir(k)%stnr,hir(k)%lat,hir(k)%lon
          WRITE(6,*)'MODEL PERIOD',hir(k)%o(1)%date,hir(k)%o(hir(k)%ntim)%date,hir(k)%ntim
   
          IF(lprint_summary) THEN
             DO l=1,hir(k)%ntim
                WRITE(6,*)' MODEL :',hir(k)%o(l)%date,hir(k)%o(l)%time
             ENDDO
          ENDIF
   
          WRITE(6,*)'OBS   PERIOD',obs(k)%o(1)%date,obs(k)%o(obs(k)%ntim)%date,obs(k)%ntim
   
          IF(lprint_summary) THEN
             DO l=1,obs(k)%ntim
                WRITE(6,*)' OBS   :',obs(k)%o(l)%date,obs(k)%o(l)%time
             ENDDO
          ENDIF

          WRITE(6,*)

       ENDIF

    ELSE
       IF(lprint_summary) THEN
          WRITE(6,*)k,'STATION NOT ACTIVE ',obs(k)%stnr,hir(k)%stnr
          WRITE(6,*)k,'STATION NOT ACTIVE ',obs(k)%active,hir(k)%active
          WRITE(6,*)
       ENDIF
    ENDIF
 ENDDO

 active_stations = m

 WRITE(6,*)'-----------------------'
 WRITE(6,*)'ACTIVE STATIONS: ',active_stations
 WRITE(6,*)'-----------------------'

 IF (active_stations > 0 ) THEN
    WRITE(6,*)'Longest period (station,ntim):',obs(k_max)%stnr,obs(k_max)%ntim
    WRITE(6,*)'-----------------------'
 ENDIF

 WRITE(6,*)

RETURN
END SUBROUTINE station_summary
