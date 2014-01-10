MODULE types

 IMPLICIT NONE

 ! 
 ! Conditional type
 !

 TYPE conditional_type
    INTEGER :: ind
    REAL    :: llim,ulim
    LOGICAL :: lobs
    LOGICAL :: all_mod
 END TYPE conditional_type


 ! 
 ! Contingeny type
 !

 TYPE contingency_type
    INTEGER              :: nclass = 0
    INTEGER              :: ind    = 0
    INTEGER              :: nval   = 0
    INTEGER, POINTER     :: table(:,:,:)
    REAL,    POINTER     :: limit(:)
 END TYPE contingency_type

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
 ! Variable
 !

 TYPE variable
    INTEGER :: lev,acc,acctype
    REAL    :: lim,ulim,llim
    CHARACTER(LEN=10) :: id ='XX'
    CHARACTER(LEN=50) :: text ='XX'
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

 TYPE scatter_bin
    REAL, POINTER :: binx(:),biny(:)
    REAL, POINTER :: array(:,:)
 END TYPE scatter_bin

 !
 ! Significance stat
 !

 TYPE sign_type
   INTEGER          :: date,time
   INTEGER, POINTER :: n(:,:,:)
   REAL,    POINTER :: r(:,:,:)
 END TYPE sign_type

 !
 ! Observation
 !

 TYPE observation
   INTEGER, POINTER :: date,time
   REAL,    POINTER :: val(:)
!  REAL,    POINTER :: fal(:,:)
   REAL,    POINTER :: nal(:,:,:)
 END TYPE observation

 !
 ! Station
 !

 TYPE station
    INTEGER :: stnr,ntim,                    &
               nexp,nfclengths,nparver
    REAL    :: lat,lon,hgt
    REAL, ALLOCATABLE :: hgtmod(:)
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
!  needed for standard deviation calculation : s2 = sum of p**2
!  p is forecast value. d:o for observations
    REAL    :: s2,obs2
!  needed for skewness calculation : s3 = sum of p**3,d:o for observations
    REAL    :: s3,obs3
 END TYPE statistics

 ! 
 ! Station statistics
 !

 TYPE statpar
    INTEGER :: stnr
    REAL    :: lat,lon
    LOGICAL :: active

    ! Active parameters
    ! nparver,ntimver
    INTEGER, POINTER :: par_active(:,:)

    ! (nexp,nparver,ntimver)
    TYPE (statistics), POINTER :: s(:,:,:) 

    ! (nparver,ntimver)
!   REAL, POINTER :: o(:,:) 
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
    REAL,    POINTER :: obs(:),obs2(:),obs3(:)
    ! (nexp,nparver)
    REAL,    POINTER :: bias(:,:),s2(:,:),s3(:,:)
    REAL,    POINTER :: rmse(:,:)
    ! nparver
    INTEGER, POINTER :: n(:)
!   INTEGER, POINTER :: r(:,:)
 END TYPE stat_obs

END MODULE types
