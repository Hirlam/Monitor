MODULE types

 IMPLICIT NONE

 ! 
 ! Plot control type
 !

 TYPE plot_type

    LOGICAL :: active
    LOGICAL :: diff
    LOGICAL :: show_rmse
    LOGICAL :: show_bias
    LOGICAL :: show_obs
    LOGICAL :: show_fc_length

    INTEGER :: timeserie_wind

    REAL    :: minx,maxx,miny,maxy

 END TYPE plot_type

 !
 ! Kalman type
 !
 TYPE kalman_type
    INTEGER :: date,time
    REAL :: q(2,2),q0(2,2),d,f1,f2,x(2),xn(2)
 END TYPE kalman_type 

 ! 
 ! Variable
 !

 TYPE variable
    INTEGER :: ind
    REAL    :: glim,ulim,llim
    REAL    :: pmax,pmin
    CHARACTER(LEN=2 ) :: id ='XX'
    CHARACTER(LEN=50) :: name ='XX'
    CHARACTER(LEN=10) :: unit ='XX'
 END TYPE variable

 !
 ! Scatter data
 !

 TYPE scatter_type
    INTEGER       :: n
    ! nexp,ndat
    REAL, POINTER :: dat(:,:)
 END TYPE scatter_type

 !
 ! Observation
 !

 TYPE observation
   INTEGER, POINTER :: date,time
   REAL,    POINTER :: val(:)
   REAL,    POINTER :: fal(:,:)
   REAL,    POINTER :: nal(:,:,:)
 END TYPE observation

 !
 ! Station
 !

 TYPE station
    INTEGER :: stnr,ntim,		&
               nexp,nfclengths,nparver
    REAL    :: lat,lon,hgt
    LOGICAL :: active,obs_is_allocated

    INTEGER,            POINTER :: pos(:)
    TYPE (observation), POINTER :: o(:)
 END TYPE station

 !
 ! Statistics
 !

 TYPE statistics
    REAL    :: obs
    REAL    :: rmse 
    REAL    :: bias
    INTEGER :: n,r
    REAL    :: mabe
 END TYPE statistics

 ! 
 ! Station statistics
 !

 TYPE statpar
    INTEGER :: stnr
    REAL    :: lat,lon
    LOGICAL :: active

    ! Active parameters
    INTEGER, POINTER :: par_active(:)

    ! (nexp,nparver,ntimver)
    TYPE (statistics), POINTER :: s(:,:,:) 

    ! (nparver,ntimver)
    REAL, POINTER :: o(:,:) 
 END TYPE statpar

 ! 
 ! Lat/lon box
 !

 TYPE box
    REAL    :: slat,nlat,wlon,elon
    LOGICAL :: active
 END TYPE box

 !
 ! Daily variation
 !

 TYPE dayvar
    REAL    :: v
    INTEGER :: n
 END TYPE dayvar

 ! 
 ! Statistics (again)
 !

 TYPE stat_obs
    INTEGER, POINTER :: date,time
    ! (nparver)
    REAL,    POINTER :: obs(:)
    ! (nexp,nparver)
    REAL,    POINTER :: bias(:,:)
    REAL,    POINTER :: rmse(:,:)
    ! nparver
    INTEGER, POINTER :: n(:)
!   INTEGER, POINTER :: r(:,:)
 END TYPE stat_obs

END MODULE types
