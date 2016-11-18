MODULE module_obstypes
  IMPLICIT NONE
 
  SAVE 

  INTEGER                            :: instrument_nsatelites
  INTEGER,ALLOCATABLE,DIMENSION(:)   :: instrument_satelites
  INTEGER                            :: instrument_used_channels
  INTEGER,ALLOCATABLE,DIMENSION(:)   :: instrument_channels
  INTEGER                            :: obtype_nvars
  INTEGER,ALLOCATABLE,DIMENSION(:)   :: obtype_vars
  INTEGER,PARAMETER                  :: nrefpres=15
  INTEGER,PARAMETER                  :: nrefheights=14
  REAL,DIMENSION(nrefpres)           :: refpres
  REAL,DIMENSION(nrefheights)        :: refheights
  INTEGER                            :: verbose
 
  TYPE variable_type
    INTEGER                :: nr
    CHARACTER(LEN=10)      :: name
    CHARACTER(LEN=10)      :: fname
    INTEGER                :: subtypestart
    INTEGER                :: subtypeend
    INTEGER                :: vertco
    INTEGER                :: level
    INTEGER                :: level1
    INTEGER                :: level2
    INTEGER,DIMENSION(3)   :: count
    REAL,DIMENSION(3)      :: fg_bias=0.
    REAL,DIMENSION(3)      :: fg_abs_bias
    REAL,DIMENSION(3)      :: fg_rms=0.
    REAL,DIMENSION(3)      :: fg_dep=0.
    REAL,DIMENSION(3)      :: fg_uncorr=0.
    REAL,DIMENSION(3)      :: an_bias=0.
    REAL,DIMENSION(3)      :: an_abs_bias
    REAL,DIMENSION(3)      :: an_rms=0.
    REAL,DIMENSION(3)      :: an_dep=0.
    REAL,DIMENSION(3)      :: bc=0.
  END TYPE variable_type

  TYPE sensor_type
    CHARACTER(LEN=10)      :: name
    CHARACTER(LEN=10)      :: satelite
    INTEGER                :: satid
    INTEGER                :: id
    INTEGER                :: channel
    INTEGER                :: used_channels
    INTEGER                :: channels
  END TYPE sensor_type

  TYPE obtype_type
    INTEGER                :: obnumber
    CHARACTER(LEN=10)      :: name
    LOGICAL                :: lsat 
    TYPE(sensor_type)      :: sensor
    TYPE(variable_type)    :: var
  END TYPE obtype_type

  TYPE(obtype_type),ALLOCATABLE,DIMENSION(:) :: all_obs
  INTEGER                                    :: nused

  CONTAINS

  SUBROUTINE alloc_obs(verb)
    IMPLICIT NONE
    INTEGER,INTENT(IN) :: verb

    verbose=verb
    IF ( verbose > 2 ) WRITE(*,*) 'Allocating all_obs with nused=',nused    
    ALLOCATE(all_obs(nused))
    refpres=(/1500,2500,4000,6500,8500,12500,17500,22500,27500,35000,45000,60000,80000,92500,100000/)
    refheights=(/250,500,1000,1500,2000,3000,4000,5000,6000,7000,8000,9000,10000,20000/)
  END SUBROUTINE alloc_obs

  SUBROUTINE dealloc_obs()
    IMPLICIT NONE
    
    IF ( verbose > 2 ) WRITE(*,*) 'De-allocating all_obs'
    DEALLOCATE(all_obs)
  END SUBROUTINE dealloc_obs

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !
  !  Conventional surface observations 
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  SUBROUTINE init_conv_surf(dry,obtype)
    IMPLICIT NONE
    LOGICAL,INTENT(IN)            :: dry
    CHARACTER(LEN=*),INTENT(IN)   :: obtype

    IF ( .NOT. dry ) THEN
      all_obs(nused)%lsat=.FALSE.
      all_obs(nused)%var%level=0
      all_obs(nused)%var%level1=0
      all_obs(nused)%var%level2=0
      all_obs(nused)%var%nr=1
      all_obs(nused)%var%vertco=1
      all_obs(nused)%var%subtypestart=0
      all_obs(nused)%var%subtypeend=200000
      SELECT CASE (TRIM(obtype))
        CASE ("synop_z")
          all_obs(nused)%obnumber=1
          all_obs(nused)%name="synop"
          all_obs(nused)%var%name="z"
          all_obs(nused)%var%subtypestart=11
          all_obs(nused)%var%subtypeend=14
        CASE ("synop_t2m") ! 39
          all_obs(nused)%obnumber=1
          all_obs(nused)%name="synop"
          all_obs(nused)%var%name="t2m"
          all_obs(nused)%var%nr=39
        CASE ("synop_td2m") ! 40
          all_obs(nused)%obnumber=1
          all_obs(nused)%name="synop"
          all_obs(nused)%var%name="td2m"
          all_obs(nused)%var%nr=40
        CASE ("synop_u10m") ! 41
          all_obs(nused)%obnumber=1
          all_obs(nused)%name="synop"
          all_obs(nused)%var%name="u10m"
          all_obs(nused)%var%nr=41
        CASE ("synop_v10m") ! 42
          all_obs(nused)%obnumber=1
          all_obs(nused)%name="synop"
          all_obs(nused)%var%name="v10m"
          all_obs(nused)%var%nr=42
       CASE ("synop_rh2m") ! 58
          all_obs(nused)%obnumber=1
          all_obs(nused)%name="synop"
          all_obs(nused)%var%name="rh2m"
          all_obs(nused)%var%nr=58
        CASE("synop_snow") ! 92
          all_obs(nused)%obnumber=1
          all_obs(nused)%name="synop"
          all_obs(nused)%var%name="snow"
          all_obs(nused)%var%nr=92
        CASE("synop_apd") ! 128
          all_obs(nused)%obnumber=1
          all_obs(nused)%name="synop"
          all_obs(nused)%var%name="apd"
          all_obs(nused)%var%nr=128
        CASE ("ship_z")
          all_obs(nused)%obnumber=1
          all_obs(nused)%name="ship"
          all_obs(nused)%var%name="z"
          all_obs(nused)%var%subtypestart=21
          all_obs(nused)%var%subtypeend=24
        CASE ("ship_t2m") ! 39
          all_obs(nused)%obnumber=1
          all_obs(nused)%name="ship"
          all_obs(nused)%var%name="t2m"
          all_obs(nused)%var%nr=39
        CASE ("ship_td2m") ! 40
          all_obs(nused)%obnumber=1
          all_obs(nused)%name="ship"
          all_obs(nused)%var%name="td2m"
          all_obs(nused)%var%nr=40
        CASE ("ship_u10m") ! 41
          all_obs(nused)%obnumber=1
          all_obs(nused)%name="ship"
          all_obs(nused)%var%name="u10m"
          all_obs(nused)%var%nr=41
        CASE ("ship_v10m") ! 42
          all_obs(nused)%obnumber=1
          all_obs(nused)%name="ship"
          all_obs(nused)%var%name="v10m"
          all_obs(nused)%var%nr=42
       CASE ("ship_rh2m") ! 58
          all_obs(nused)%obnumber=1
          all_obs(nused)%name="ship"
          all_obs(nused)%var%name="rh2m"
          all_obs(nused)%var%nr=58
        CASE ("dribu_z")
          all_obs(nused)%obnumber=4
          all_obs(nused)%name="dribu"
          all_obs(nused)%var%name="z"
          all_obs(nused)%var%subtypestart=160
          all_obs(nused)%var%subtypeend=165
        CASE ("metar_z")
          all_obs(nused)%obnumber=1
          all_obs(nused)%name="metar"
          all_obs(nused)%var%name="z"
          all_obs(nused)%var%subtypestart=139
          all_obs(nused)%var%subtypeend=141
        CASE DEFAULT
          WRITE(*,*) "Variable obtype not defined: ",obtype
          CALL ABORT
      END SELECT

      all_obs(nused)%var%fname=all_obs(nused)%var%name
      ! Not used sensor properties
      all_obs(nused)%sensor%id=0
      all_obs(nused)%sensor%name="undefined"
      all_obs(nused)%sensor%satid=-1
      all_obs(nused)%sensor%channel=-1
      all_obs(nused)%sensor%channel=-1
      all_obs(nused)%sensor%channels=-1
      IF ( verbose > 1 ) write(*,*) nused,'-> Init ',TRIM(all_obs(nused)%name)
    ENDIF
    nused=nused+1
  END SUBROUTINE init_conv_surf

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !
  !  Scatterometer 
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  SUBROUTINE init_scatt(dry,obtype)
    IMPLICIT NONE
    LOGICAL,INTENT(IN)            :: dry
    CHARACTER(LEN=*),INTENT(IN)   :: obtype

    IF ( .NOT. dry ) THEN
      all_obs(nused)%lsat=.FALSE.
      all_obs(nused)%var%level=0
      all_obs(nused)%var%level1=0
      all_obs(nused)%var%level2=0
      all_obs(nused)%var%vertco=1
      all_obs(nused)%var%subtypestart=0
      all_obs(nused)%var%subtypeend=200000
      all_obs(nused)%obnumber=9
      SELECT CASE (TRIM(obtype))
        CASE ("u10m")
          all_obs(nused)%name="scatt"
          all_obs(nused)%var%name="u10m"
          all_obs(nused)%var%nr=124
        CASE ("v10m")
          all_obs(nused)%name="scatt"
          all_obs(nused)%var%name="v10m"
          all_obs(nused)%var%nr=125
        CASE DEFAULT
          WRITE(*,*) "Variable obtype not defined: ",obtype
          CALL ABORT
      END SELECT

      all_obs(nused)%var%fname=all_obs(nused)%var%name
      ! Not used sensor properties at the moment. Don't separate satelites!
      all_obs(nused)%sensor%id=0
      all_obs(nused)%sensor%name="ascat"
      all_obs(nused)%sensor%satid=-1
      all_obs(nused)%sensor%channel=-1
      all_obs(nused)%sensor%channel=-1
      all_obs(nused)%sensor%channels=-1
      IF ( verbose > 1 ) write(*,*) nused,'-> Init ',TRIM(all_obs(nused)%name)
    ENDIF
    nused=nused+1
  END SUBROUTINE init_scatt

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !
  ! Conventional observations with vertical levels 
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  SUBROUTINE init_conv_vert(dry,obt)
    IMPLICIT NONE
    LOGICAL,INTENT(IN)            :: dry
    CHARACTER(LEN=*),INTENT(IN)   :: obt
    CHARACTER(LEN=10)             :: obvar
    INTEGER                       :: obnr
    INTEGER                       :: obv
    INTEGER                       :: subtypestart
    INTEGER                       :: subtypeend
    CHARACTER                     :: var
    CHARACTER(LEN=7)              :: clev
    INTEGER                       :: i,j
    LOGICAL                       :: var_from_namelist

    CALL read_vars_from_namelist(obt,var_from_namelist)
    ! Default values 
    IF ( .NOT. var_from_namelist ) THEN
      SELECT CASE (obt)
        CASE ("aircraft")
          obtype_nvars=3
          ALLOCATE(obtype_vars(obtype_nvars))
          obtype_vars=(/2,3,4/)
        CASE ("temp")
          obtype_nvars=4
          ALLOCATE(obtype_vars(obtype_nvars))
          obtype_vars=(/2,3,4,7/)
        CASE ("limb")
          obtype_nvars=1
          ALLOCATE(obtype_vars(obtype_nvars))
          obtype_vars=(/162/)
        CASE ("pilot")
          obtype_nvars=2
          ALLOCATE(obtype_vars(obtype_nvars))
          obtype_vars=(/3,4/)
        CASE ("amv")
          obtype_nvars=3
          ALLOCATE(obtype_vars(obtype_nvars))
          obtype_vars=(/2,3,4/)
        CASE DEFAULT
          WRITE(*,*) "Variables not defined for obtype: ",obt
          CALL ABORT
      END SELECT
    ENDIF

    DO i=1,nrefpres
      DO j=1,obtype_nvars
        subtypestart=0
        subtypeend=255
        obv=obtype_vars(j)

        SELECT CASE (obt)
          CASE ("aircraft")
            obnr=2
          CASE ("amv")
            obnr=3
          CASE ("temp")
            obnr=5
          CASE ("pilot")
            obnr=6
          CASE ("limb")
            obnr=10
          CASE DEFAULT
            WRITE(*,*) "Obtype not defined: ",obt
            CALL ABORT
        END SELECT
        SELECT CASE (obv)
          CASE (2)
            obvar="t"
          CASE (3)
            obvar="u"
          CASE (4)
            obvar="v"
          CASE (7)
            obvar="q"
          CASE (162)
            obvar="bend_angle"
          CASE DEFAULT
            WRITE(*,*) "Variable not defined: ",obv
            CALL ABORT
        END SELECT

        IF ( .NOT. dry ) THEN
          all_obs(nused)%obnumber=obnr
          all_obs(nused)%name=obt
          all_obs(nused)%lsat=.FALSE.
          IF ( i == 1 ) THEN
            all_obs(nused)%var%level1=0.
          ELSE
            all_obs(nused)%var%level1=refpres(i-1)
          ENDIF
          all_obs(nused)%var%level2=refpres(i)
          all_obs(nused)%var%level=refpres(i)
          all_obs(nused)%var%name=obvar
          all_obs(nused)%var%fname(1:1)=obvar
          all_obs(nused)%var%fname(2:2)="_"
          WRITE(clev,'(I7)') all_obs(nused)%var%level
          clev = adjustl(clev)
          all_obs(nused)%var%fname(3:)=TRIM(clev)
          all_obs(nused)%var%nr=obv
          all_obs(nused)%var%subtypestart=subtypestart
          all_obs(nused)%var%subtypeend=subtypeend
          all_obs(nused)%var%vertco=1

          ! Not used sensor properties
          all_obs(nused)%sensor%id=0
          all_obs(nused)%sensor%name="undefined"
          all_obs(nused)%sensor%satid=-1
          all_obs(nused)%sensor%channel=-1
          all_obs(nused)%sensor%channel=-1
          all_obs(nused)%sensor%channels=-1
          IF ( verbose > 1 ) write(*,*) nused,'-> Init ',TRIM(all_obs(nused)%name),' ',TRIM(all_obs(nused)%var%fname)
        ENDIF
      ENDDO
      nused=nused+1
    ENDDO
    DEALLOCATE(obtype_vars)
  END SUBROUTINE init_conv_vert

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !
  ! Radar observations 
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  SUBROUTINE init_radar(dry,obt)
    IMPLICIT NONE
    LOGICAL,INTENT(IN)            :: dry
    CHARACTER(LEN=*),INTENT(IN)   :: obt
    CHARACTER(LEN=10)             :: obvar
    INTEGER                       :: obnr
    INTEGER                       :: obv
    INTEGER                       :: subtypestart
    INTEGER                       :: subtypeend
    CHARACTER(LEN=7)              :: clev
    INTEGER                       :: i,j
    LOGICAL                       :: var_from_namelist
    INTEGER                       :: nref
    REAL                          :: val1,val2
    LOGICAL                       :: plevel

    obnr=13
    plevel=.FALSE.
    SELECT CASE (obt)
      CASE ("radarv")
        obvar="radv"
        obv=195
      CASE ("radardbz")
        obvar="dbz"
        obv=192
      CASE ("radarrh")
        obvar="rh"
        obv=29
        plevel=.TRUE.
      CASE DEFAULT
        WRITE(*,*) "Variables not defined for obtype: ",obt
        CALL ABORT
    END SELECT

    IF ( plevel ) THEN
      nref=nrefpres
    ELSE
      nref=nrefheights
    ENDIF

    DO i=1,nref
      subtypestart=0
      subtypeend=255

      IF ( .NOT. dry ) THEN
        all_obs(nused)%obnumber=obnr
        all_obs(nused)%name="radar"
        all_obs(nused)%lsat=.FALSE.
        IF ( i == 1 ) THEN
          all_obs(nused)%var%level1=0.
        ELSE
          IF ( plevel ) THEN
            all_obs(nused)%var%level1=refpres(i-1)
          ELSE
            all_obs(nused)%var%level1=refheights(i-1)
          ENDIF
        ENDIF
        IF ( plevel ) THEN
          all_obs(nused)%var%level2=refpres(i)
          all_obs(nused)%var%level=refpres(i)
        ELSE
          all_obs(nused)%var%level2=refheights(i)
          all_obs(nused)%var%level=refheights(i)
        ENDIF
        all_obs(nused)%var%name=obvar
        WRITE(clev,'(I7)') all_obs(nused)%var%level
        clev = adjustl(clev)
        all_obs(nused)%var%fname=TRIM(obvar)//"_"//TRIM(clev)
        all_obs(nused)%var%nr=obv
        all_obs(nused)%var%subtypestart=subtypestart
        all_obs(nused)%var%subtypeend=subtypeend
        all_obs(nused)%var%vertco=1

        ! Not used sensor properties
        all_obs(nused)%sensor%id=0
        all_obs(nused)%sensor%name="undefined"
        all_obs(nused)%sensor%satid=-1
        all_obs(nused)%sensor%channel=-1
        all_obs(nused)%sensor%channel=-1
        all_obs(nused)%sensor%channels=-1
        IF ( verbose > 1 ) write(*,*) nused,'-> Init ',TRIM(all_obs(nused)%name),' ',TRIM(all_obs(nused)%var%fname)
      ENDIF
      nused=nused+1
    ENDDO
  END SUBROUTINE init_radar

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !
  !  AMSU A 
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  SUBROUTINE init_amsua(dry)
    IMPLICIT NONE
    LOGICAL,INTENT(IN)                 :: dry
    LOGICAL                            :: channels_from_namelist
    LOGICAL                            :: satelites_from_namelist
    INTEGER                            :: i,channel
    INTEGER,PARAMETER                  :: amsua_all_channels=15
    CHARACTER(LEN=10)                  :: name
    CHARACTER(LEN=10)                  :: instrument

    instrument="AMSUA"
    CALL read_satelites_from_namelist(instrument,satelites_from_namelist)
    ! Default values
    IF ( .NOT. satelites_from_namelist ) THEN
      instrument_nsatelites=7
      ALLOCATE(instrument_satelites(instrument_nsatelites))
      instrument_satelites=(/3,4,206,207,208,209,223/)
    ENDIF

    DO i=1,instrument_nsatelites
      SELECT CASE (instrument_satelites(i))
        CASE(206)
          name="noaa15"
          CALL read_channels_from_namelist(instrument,name,channels_from_namelist)
          ! Default values
          IF ( .NOT. channels_from_namelist ) THEN
            instrument_used_channels=15
            ALLOCATE(instrument_channels(instrument_used_channels))
            instrument_channels=(/1,2,3,4,5,6,7,8,9,10,11,12,13,14,15/)
          ENDIF
        CASE(207)
          name="noaa16"
          CALL read_channels_from_namelist(instrument,name,channels_from_namelist)
          IF ( .NOT. channels_from_namelist ) THEN
            instrument_used_channels=15
            ALLOCATE(instrument_channels(instrument_used_channels))
            instrument_channels=(/1,2,3,4,5,6,7,8,9,10,11,12,13,14,15/)
          ENDIF
        CASE(208)
          name="noaa17"
          CALL read_channels_from_namelist(instrument,name,channels_from_namelist)
          IF ( .NOT. channels_from_namelist ) THEN
            instrument_used_channels=15
            ALLOCATE(instrument_channels(instrument_used_channels))
            instrument_channels=(/1,2,3,4,5,6,7,8,9,10,11,12,13,14,15/)
          ENDIF
        CASE(209)
          name="noaa18"
          CALL read_channels_from_namelist(instrument,name,channels_from_namelist)
          IF ( .NOT. channels_from_namelist ) THEN
            instrument_used_channels=15
            ALLOCATE(instrument_channels(instrument_used_channels))
            instrument_channels=(/1,2,3,4,5,6,7,8,9,10,11,12,13,14,15/)
          ENDIF
        CASE(223)
          name="noaa19"
          CALL read_channels_from_namelist(instrument,name,channels_from_namelist)
          IF ( .NOT. channels_from_namelist ) THEN
            instrument_used_channels=15
            ALLOCATE(instrument_channels(instrument_used_channels))
            instrument_channels=(/1,2,3,4,5,6,7,8,9,10,11,12,13,14,15/)
          ENDIF
        CASE(3)
          name="metop1"
          CALL read_channels_from_namelist(instrument,name,channels_from_namelist)
          IF ( .NOT. channels_from_namelist ) THEN
            instrument_used_channels=15
            ALLOCATE(instrument_channels(instrument_used_channels))
            instrument_channels=(/1,2,3,4,5,6,7,8,9,10,11,12,13,14,15/)
          ENDIF
        CASE(4)
          name="metop2"
          CALL read_channels_from_namelist(instrument,name,channels_from_namelist)
          IF ( .NOT. channels_from_namelist ) THEN
            instrument_used_channels=15
            ALLOCATE(instrument_channels(instrument_used_channels))
            instrument_channels=(/1,2,3,4,5,6,7,8,9,10,11,12,13,14,15/)
          ENDIF
        CASE DEFAULT
          WRITE(*,*) 'Satelite ',instrument_satelites(i),' is not defined ',TRIM(instrument)
          CALL ABORT
      END SELECT
      DO channel=1,instrument_used_channels
        IF ( .NOT. dry ) THEN
          all_obs(nused)%obnumber=7
          all_obs(nused)%name="satem"
          all_obs(nused)%lsat=.TRUE.
          all_obs(nused)%sensor%satelite=name
          all_obs(nused)%sensor%name="amsua"
          all_obs(nused)%sensor%id=3
          all_obs(nused)%sensor%satid=instrument_satelites(i)
          all_obs(nused)%sensor%channel=instrument_channels(channel)
          all_obs(nused)%sensor%channel=instrument_channels(channel)
          all_obs(nused)%sensor%channels=amsua_all_channels
          all_obs(nused)%var%nr=119
          all_obs(nused)%var%name="rad"
          all_obs(nused)%var%vertco=3

          all_obs(nused)%var%fname=all_obs(nused)%var%name
          ! Not used variable properties. Satelites uses the channels as vertical levels
          all_obs(nused)%var%level1=-1
          all_obs(nused)%var%level2=-1

          IF ( verbose > 1 ) write(*,*) nused,'-> Init AMSU A channel: ',instrument_channels(channel),&
                                        ' on satelite: ',trim(all_obs(nused)%sensor%satelite)
        ENDIF
        nused=nused+1
      ENDDO
      DEALLOCATE(instrument_channels)
    ENDDO
    DEALLOCATE(instrument_satelites)
  END SUBROUTINE init_amsua

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !
  !  AMSU B
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  SUBROUTINE init_amsub(dry)
    IMPLICIT NONE
    LOGICAL,INTENT(IN)   :: dry
    INTEGER              :: i,channel
    LOGICAL              :: channels_from_namelist
    LOGICAL              :: satelites_from_namelist
    CHARACTER(LEN=10)    :: name
    CHARACTER(LEN=10)    :: instrument
    INTEGER,PARAMETER    :: amsub_all_channels=5

    instrument="AMSUB"
    CALL read_satelites_from_namelist(instrument,satelites_from_namelist)
    ! Default values
    IF ( .NOT. satelites_from_namelist ) THEN
      instrument_nsatelites=4
      ALLOCATE(instrument_satelites(instrument_nsatelites))
      instrument_satelites=(/206,207,208,209/)
    ENDIF

    DO i=1,instrument_nsatelites
      SELECT CASE (instrument_satelites(i))
        CASE(206)
          name="noaa15"
          CALL read_channels_from_namelist(instrument,name,channels_from_namelist)
          ! Default values
          IF ( .NOT. channels_from_namelist ) THEN
            instrument_used_channels=5
            ALLOCATE(instrument_channels(instrument_used_channels))
            instrument_channels=(/1,2,3,4,5/)
          ENDIF
        CASE(207)
          name="noaa16"
          CALL read_channels_from_namelist(instrument,name,channels_from_namelist)
          IF ( .NOT. channels_from_namelist ) THEN
            instrument_used_channels=5
            ALLOCATE(instrument_channels(instrument_used_channels))
            instrument_channels=(/1,2,3,4,5/)
          ENDIF
        CASE(208)
          name="noaa17"
          CALL read_channels_from_namelist(instrument,name,channels_from_namelist)
          IF ( .NOT. channels_from_namelist ) THEN
            instrument_used_channels=5
            ALLOCATE(instrument_channels(instrument_used_channels))
            instrument_channels=(/1,2,3,4,5/)
          ENDIF
        CASE(209)
          name="noaa18"
          CALL read_channels_from_namelist(instrument,name,channels_from_namelist)
          IF ( .NOT. channels_from_namelist ) THEN
            instrument_used_channels=5
            ALLOCATE(instrument_channels(instrument_used_channels))
            instrument_channels=(/1,2,3,4,5/) 
          ENDIF
        CASE DEFAULT
          WRITE(*,*) 'Satelite ',instrument_satelites(i),' is not defined ',TRIM(instrument)
          CALL ABORT
      END SELECT

      DO channel=1,instrument_used_channels
        IF ( .NOT. dry ) THEN
          all_obs(nused)%obnumber=7
          all_obs(nused)%name="satem"
          all_obs(nused)%lsat=.TRUE.
          all_obs(nused)%sensor%name="amsub"
          all_obs(nused)%sensor%satelite=name
          all_obs(nused)%sensor%id=4
          all_obs(nused)%sensor%satid=instrument_satelites(i)
          all_obs(nused)%sensor%channel=instrument_channels(channel)
          all_obs(nused)%sensor%used_channels=instrument_used_channels
          all_obs(nused)%sensor%channels=amsub_all_channels
          all_obs(nused)%var%nr=119
          all_obs(nused)%var%name="rad"
          all_obs(nused)%var%vertco=3

          all_obs(nused)%var%fname=all_obs(nused)%var%name
          ! Not used variable properties. Satelites uses the channels as vertical levels
          all_obs(nused)%var%level1=-1
          all_obs(nused)%var%level2=-1

          IF ( verbose > 1 ) write(*,*) nused,'-> Init AMSU B channel: ',instrument_channels(channel),&
                                              ' on satelite: ',trim(all_obs(nused)%sensor%satelite)
        ENDIF
        nused=nused+1
      ENDDO
      DEALLOCATE(instrument_channels)
    ENDDO
    DEALLOCATE(instrument_satelites)
  END SUBROUTINE init_amsub

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !
  !  MHS
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  SUBROUTINE init_mhs(dry)
    IMPLICIT NONE
    LOGICAL,INTENT(IN)   :: dry
    INTEGER              :: i,channel
    LOGICAL              :: channels_from_namelist
    LOGICAL              :: satelites_from_namelist
    CHARACTER(LEN=10)    :: name
    CHARACTER(LEN=10)    :: instrument
    INTEGER,PARAMETER    :: mhs_all_channels=5

    instrument="MHS"
    CALL read_satelites_from_namelist(instrument,satelites_from_namelist)
    ! Default values
    IF ( .NOT. satelites_from_namelist ) THEN
      instrument_nsatelites=3
      ALLOCATE(instrument_satelites(instrument_nsatelites))
      instrument_satelites=(/3,4,223/)
    ENDIF

    DO i=1,instrument_nsatelites
      SELECT CASE (instrument_satelites(i))
        CASE(223)
          name="noaa19"
          CALL read_channels_from_namelist(instrument,name,channels_from_namelist)
          ! Default values
          IF ( .NOT. channels_from_namelist ) THEN
            instrument_used_channels=5
            ALLOCATE(instrument_channels(instrument_used_channels))
            instrument_channels=(/1,2,3,4,5/)
          ENDIF
        CASE(3)
          name="metop1"
          CALL read_channels_from_namelist(instrument,name,channels_from_namelist)
          IF ( .NOT. channels_from_namelist ) THEN
            instrument_used_channels=5
            ALLOCATE(instrument_channels(instrument_used_channels))
            instrument_channels=(/1,2,3,4,5/)
          ENDIF
        CASE(4)
          name="metop2"
          CALL read_channels_from_namelist(instrument,name,channels_from_namelist)
          IF ( .NOT. channels_from_namelist ) THEN
            instrument_used_channels=5
            ALLOCATE(instrument_channels(instrument_used_channels))
            instrument_channels=(/1,2,3,4,5/)
          ENDIF
        CASE DEFAULT
          WRITE(*,*) 'Satelite ',instrument_satelites(i),' is not defined ',TRIM(instrument)
          CALL ABORT
      END SELECT

      DO channel=1,instrument_used_channels
        IF ( .NOT. dry ) THEN
          all_obs(nused)%obnumber=7
          all_obs(nused)%name="satem"
          all_obs(nused)%lsat=.TRUE.
          all_obs(nused)%sensor%name="mhs"
          all_obs(nused)%sensor%satelite=name
          all_obs(nused)%sensor%id=15
          all_obs(nused)%sensor%satid=instrument_satelites(i)
          all_obs(nused)%sensor%channel=instrument_channels(channel)
          all_obs(nused)%sensor%used_channels=instrument_used_channels
          all_obs(nused)%sensor%channels=mhs_all_channels
          all_obs(nused)%var%nr=119
          all_obs(nused)%var%name="rad"
          all_obs(nused)%var%vertco=3

          all_obs(nused)%var%fname=all_obs(nused)%var%name
          ! Not used variable properties. Satelites uses the channels as vertical levels
          all_obs(nused)%var%level1=-1
          all_obs(nused)%var%level2=-1

          IF ( verbose > 1 ) write(*,*) nused,'-> Init MHS    channel: ',instrument_channels(channel),&
                                              ' on satelite: ',trim(all_obs(nused)%sensor%satelite)
        ENDIF
        nused=nused+1
      ENDDO
      DEALLOCATE(instrument_channels)
    ENDDO
    DEALLOCATE(instrument_satelites)
  END SUBROUTINE init_mhs
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !
  !  ATMS 
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  SUBROUTINE init_atms(dry)
    IMPLICIT NONE
    LOGICAL,INTENT(IN)   :: dry
    INTEGER              :: i,channel
    LOGICAL              :: channels_from_namelist
    LOGICAL              :: satelites_from_namelist
    CHARACTER(LEN=10)    :: name
    CHARACTER(LEN=10)    :: instrument
    INTEGER,PARAMETER    :: atms_all_channels=22
  
    instrument="ATMS"
    CALL read_satelites_from_namelist(instrument,satelites_from_namelist)
    ! Default values
    IF ( .NOT. satelites_from_namelist ) THEN
      instrument_nsatelites=1
      ALLOCATE(instrument_satelites(instrument_nsatelites))
      instrument_satelites=(/224/)
    ENDIF
 
    DO i=1,instrument_nsatelites
      SELECT CASE (instrument_satelites(i))
        CASE(224)
          name="jpss0"
          CALL read_channels_from_namelist(instrument,name,channels_from_namelist)
          ! Default values
          IF ( .NOT. channels_from_namelist ) THEN
            instrument_used_channels=22
            ALLOCATE(instrument_channels(instrument_used_channels))
            instrument_channels=(/1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22/)
          ENDIF
        CASE DEFAULT
          WRITE(*,*) 'Satelite ',instrument_satelites(i),' is not defined ',TRIM(instrument)
          CALL ABORT
      END SELECT
 
      DO channel=1,instrument_used_channels
        IF ( .NOT. dry ) THEN
          all_obs(nused)%obnumber=7
          all_obs(nused)%name="satem"
          all_obs(nused)%lsat=.TRUE.
          all_obs(nused)%sensor%name="atms"
          all_obs(nused)%sensor%satelite=name
          all_obs(nused)%sensor%id=19
          all_obs(nused)%sensor%satid=instrument_satelites(i)
          all_obs(nused)%sensor%channel=instrument_channels(channel)
          all_obs(nused)%sensor%used_channels=instrument_used_channels
          all_obs(nused)%sensor%channels=atms_all_channels
          all_obs(nused)%var%nr=119
          all_obs(nused)%var%name="rad"
          all_obs(nused)%var%vertco=3
 
          all_obs(nused)%var%fname=all_obs(nused)%var%name
          ! Not used variable properties. Satelites uses the channels as vertical levels
          all_obs(nused)%var%level1=-1
          all_obs(nused)%var%level2=-1
 
          IF ( verbose > 1 ) write(*,*) nused,'-> Init ATMS    channel: ',instrument_channels(channel),&
                                              ' on satelite: ',trim(all_obs(nused)%sensor%satelite)
        ENDIF
        nused=nused+1
      ENDDO
      DEALLOCATE(instrument_channels)
    ENDDO
    DEALLOCATE(instrument_satelites)
  END SUBROUTINE init_atms

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !
  !  IASI 
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  SUBROUTINE init_iasi(dry)
    IMPLICIT NONE
    LOGICAL,INTENT(IN)   :: dry
    INTEGER              :: i,channel
    LOGICAL              :: channels_from_namelist
    LOGICAL              :: satelites_from_namelist
    CHARACTER(LEN=10)    :: name
    CHARACTER(LEN=10)    :: instrument
    INTEGER,PARAMETER    :: iasi_all_channels=8461

    instrument="IASI"
    CALL read_satelites_from_namelist(instrument,satelites_from_namelist)
    ! Default values
    IF ( .NOT. satelites_from_namelist ) THEN
      instrument_nsatelites=2
      ALLOCATE(instrument_satelites(instrument_nsatelites))
      instrument_satelites=(/3,4/)
    ENDIF

    DO i=1,instrument_nsatelites
      SELECT CASE (instrument_satelites(i))
        CASE(3)
          name="metop1"
          CALL read_channels_from_namelist(instrument,name,channels_from_namelist)
          ! Default values
          IF ( .NOT. channels_from_namelist ) THEN
            instrument_used_channels=366
            ALLOCATE(instrument_channels(instrument_used_channels))
            instrument_channels=(/16,  38,  49,  51,  55,  57,  59,  61,  63,  66,&
                                  70,  72,  74,  79,  81,  83,  85,  87,  89,  92,&
                                  95,  97,  99, 101, 104, 106, 109, 111, 113, 116,&
                                 119, 122, 125, 128, 131, 133, 135, 138, 141, 144,&
                                 146, 148, 151, 154, 157, 159, 161, 163, 165, 167,&
                                 170, 173, 176, 178, 179, 180, 183, 185, 187, 189,&
                                 191, 193, 195, 197, 199, 201, 203, 205, 207, 210,&
                                 212, 214, 217, 219, 222, 224, 226, 228, 230, 232,&
                                 234, 236, 239, 241, 242, 243, 246, 249, 252, 254,&
                                 256, 258, 260, 262, 265, 267, 269, 271, 272, 273,&
                                 275, 278, 280, 282, 284, 286, 288, 290, 292, 294,&
                                 296, 299, 301, 303, 306, 308, 310, 312, 314, 316,&
                                 318, 320, 323, 325, 327, 329, 331, 333, 335, 337,&
                                 339, 341, 343, 345, 347, 350, 352, 354, 356, 358,&
                                 360, 362, 364, 366, 369, 371, 373, 375, 377, 379,&
                                 381, 383, 386, 389, 398, 401, 404, 407, 410, 414,&
                                 416, 426, 428, 432, 434, 439, 445, 457, 515, 546,&
                                 552, 559, 566, 571, 573, 646, 662, 668, 756, 867,&
                                 906, 921,1027,1046,1090,1121,1133,1191,1194,1271,&
                                1479,1509,1513,1521,1536,1574,1578,1579,1585,1587,&
                                1626,1639,1643,1652,1658,1671,1786,1805,1884,1946,&
                                1991,2019,2094,2119,2213,2239,2245,2271,2321,2398,&
                                2701,2741,2745,2819,2889,2907,2910,2919,2939,2944,&
                                2948,2951,2958,2977,2985,2988,2991,2993,3002,3008,&
                                3014,3027,3029,3036,3047,3049,3053,3058,3064,3069,&
                                3087,3093,3098,3105,3107,3110,3127,3136,3151,3160,&
                                3165,3168,3175,3178,3207,3228,3244,3248,3252,3256,&
                                3263,3281,3303,3309,3312,3322,3339,3375,3378,3411,&
                                3438,3440,3442,3444,3446,3448,3450,3452,3454,3458,&
                                3467,3476,3484,3491,3497,3499,3504,3506,3509,3518,&
                                3522,3527,3540,3555,3575,3577,3580,3582,3586,3589,&
                                3599,3645,3653,3658,3661,3943,4032,5130,5368,5371,&
                                5379,5381,5383,5397,5399,5401,5403,5405,5455,5480,&
                                5483,5485,5492,5502,5507,5509,5517,5558,5988,5992,&
                                5994,6003,6350,6458,6463,6601,6962,6978,6980,6982,&
                                6985,6987,6989,6991,6993,6995,6997,7001,7267,7269,&
                                7389,7424,7426,7428,7885,8007/)
          ENDIF
        CASE(4)
          name="metop2"
          CALL read_channels_from_namelist(instrument,name,channels_from_namelist)
          ! Default values
          IF ( .NOT. channels_from_namelist ) THEN
            instrument_used_channels=366
            ALLOCATE(instrument_channels(instrument_used_channels))
            instrument_channels=(/16,  38,  49,  51,  55,  57,  59,  61,  63,  66,&
                                  70,  72,  74,  79,  81,  83,  85,  87,  89,  92,&
                                  95,  97,  99, 101, 104, 106, 109, 111, 113, 116,&
                                 119, 122, 125, 128, 131, 133, 135, 138, 141, 144,&
                                 146, 148, 151, 154, 157, 159, 161, 163, 165, 167,&
                                 170, 173, 176, 178, 179, 180, 183, 185, 187, 189,&
                                 191, 193, 195, 197, 199, 201, 203, 205, 207, 210,&
                                 212, 214, 217, 219, 222, 224, 226, 228, 230, 232,&
                                 234, 236, 239, 241, 242, 243, 246, 249, 252, 254,&
                                 256, 258, 260, 262, 265, 267, 269, 271, 272, 273,&
                                 275, 278, 280, 282, 284, 286, 288, 290, 292, 294,&
                                 296, 299, 301, 303, 306, 308, 310, 312, 314, 316,&
                                 318, 320, 323, 325, 327, 329, 331, 333, 335, 337,&
                                 339, 341, 343, 345, 347, 350, 352, 354, 356, 358,&
                                 360, 362, 364, 366, 369, 371, 373, 375, 377, 379,&
                                 381, 383, 386, 389, 398, 401, 404, 407, 410, 414,&
                                 416, 426, 428, 432, 434, 439, 445, 457, 515, 546,&
                                 552, 559, 566, 571, 573, 646, 662, 668, 756, 867,&
                                 906, 921,1027,1046,1090,1121,1133,1191,1194,1271,&
                                1479,1509,1513,1521,1536,1574,1578,1579,1585,1587,&
                                1626,1639,1643,1652,1658,1671,1786,1805,1884,1946,&
                                1991,2019,2094,2119,2213,2239,2245,2271,2321,2398,&
                                2701,2741,2745,2819,2889,2907,2910,2919,2939,2944,&
                                2948,2951,2958,2977,2985,2988,2991,2993,3002,3008,&
                                3014,3027,3029,3036,3047,3049,3053,3058,3064,3069,&
                                3087,3093,3098,3105,3107,3110,3127,3136,3151,3160,&
                                3165,3168,3175,3178,3207,3228,3244,3248,3252,3256,&
                                3263,3281,3303,3309,3312,3322,3339,3375,3378,3411,&
                                3438,3440,3442,3444,3446,3448,3450,3452,3454,3458,&
                                3467,3476,3484,3491,3497,3499,3504,3506,3509,3518,&
                                3522,3527,3540,3555,3575,3577,3580,3582,3586,3589,&
                                3599,3645,3653,3658,3661,3943,4032,5130,5368,5371,&
                                5379,5381,5383,5397,5399,5401,5403,5405,5455,5480,&
                                5483,5485,5492,5502,5507,5509,5517,5558,5988,5992,&
                                5994,6003,6350,6458,6463,6601,6962,6978,6980,6982,&
                                6985,6987,6989,6991,6993,6995,6997,7001,7267,7269,&
                                7389,7424,7426,7428,7885,8007/)
          ENDIF
        CASE DEFAULT
          WRITE(*,*) 'Satelite ',instrument_satelites(i),' is not defined for ',TRIM(instrument)
          CALL ABORT
      END SELECT
      DO channel=1,instrument_used_channels
        IF ( .NOT. dry ) THEN
          all_obs(nused)%obnumber=7
          all_obs(nused)%name="satem"
          all_obs(nused)%lsat=.TRUE.
          all_obs(nused)%sensor%name="iasi"
          all_obs(nused)%sensor%satelite=name
          all_obs(nused)%sensor%id=16
          all_obs(nused)%sensor%satid=instrument_satelites(i)
          all_obs(nused)%sensor%channel=instrument_channels(channel)
          all_obs(nused)%sensor%used_channels=instrument_used_channels
          all_obs(nused)%sensor%channels=iasi_all_channels
          all_obs(nused)%var%nr=119
          all_obs(nused)%var%name="rad"
          all_obs(nused)%var%vertco=3

          all_obs(nused)%var%fname=all_obs(nused)%var%name
          ! Not used variable properties. Satelites uses the channels as vertical levels
          all_obs(nused)%var%level1=-1
          all_obs(nused)%var%level2=-1

          IF ( verbose > 1 ) write(*,*) nused,'-> Init IASI   channel: ',instrument_channels(channel),&
                                              ' on satelite: ',trim(all_obs(nused)%sensor%satelite)
        ENDIF
        nused=nused+1
      ENDDO
      DEALLOCATE(instrument_channels)
    ENDDO
    DEALLOCATE(instrument_satelites)
  END SUBROUTINE init_iasi

  SUBROUTINE read_vars_from_namelist(obt,vars_from_namelist)
    IMPLICIT NONE
    CHARACTER(LEN=*),INTENT(IN)      :: obt 
    LOGICAL,INTENT(OUT)              :: vars_from_namelist
    INTEGER                          :: ios,count,i
    INTEGER,PARAMETER                :: max_vars=10
    INTEGER,DIMENSION(max_vars)      :: vars 
    NAMELIST / TEMP  / vars
    NAMELIST / AIRCRAFT / vars
    NAMELIST / PILOT / vars
    NAMELIST / AMV / vars
    NAMELIST / LIMB / vars

    vars_from_namelist=.FALSE.
    vars=-99
    OPEN(4,IOSTAT=ios)
    IF ( ios == 0 ) THEN
      REWIND(4)
      ios=-1
      SELECT CASE (TRIM(obt))
        CASE ("temp") 
          READ(4,NML=TEMP,IOSTAT=ios)
        CASE ("amv") 
          READ(4,NML=AMV,IOSTAT=ios)
        CASE ("limb") 
          READ(4,NML=LIMB,IOSTAT=ios)
        CASE ("aircraft")
          READ(4,NML=AIRCRAFT,IOSTAT=ios)
        CASE ("pilot")
          READ(4,NML=PILOT,IOSTAT=ios)
        CASE DEFAULT
          WRITE(*,*) "No namelist defined for ",TRIM(obt)
      END SELECT
      IF ( ios /= 0 ) THEN
         IF (verbose > 2 )  WRITE(*,*) 'Can not read namelist ',trim(obt),&
                                       ' from unit 4. Default values used '
      ELSE
        count=0
        DO i=1,max_vars
          IF ( vars(i) > 0 ) count=count+1
        ENDDO
        IF ( count == 0 ) THEN
          WRITE(*,*) 'No vars selected for ',TRIM(obt)
          CALL abort
        ENDIF
        obtype_nvars=count
        ALLOCATE(obtype_vars(obtype_nvars))

        count=1
        DO i=1,max_vars
          IF ( vars(i) > 0 ) THEN
            obtype_vars(count)=vars(i)
            count=count+1
          ENDIF
        ENDDO
        vars_from_namelist=.TRUE.
        IF ( VERBOSE > 0 ) WRITE(*,*) 'Namelist read from unit 4 for instrument ',trim(obt)
      ENDIF
      CLOSE(4)
    ENDIF
  END SUBROUTINE


  SUBROUTINE read_satelites_from_namelist(instrument,satelites_from_namelist)
    IMPLICIT NONE
    CHARACTER(LEN=*),INTENT(IN)      :: instrument
    LOGICAL,INTENT(OUT)              :: satelites_from_namelist
    INTEGER                          :: ios,count,i
    INTEGER,PARAMETER                :: max_satelites=15
    INTEGER,DIMENSION(max_satelites) :: satelites
    NAMELIST / AMSUA / satelites
    NAMELIST / AMSUB / satelites
    NAMELIST / MHS   / satelites
    NAMELIST / ATMS  / satelites
    NAMELIST / IASI  / satelites
  
    satelites_from_namelist=.FALSE. 
    satelites=-99
    OPEN(4,IOSTAT=ios)
    IF ( ios == 0 ) THEN
      REWIND(4)
      ios=-1
      SELECT CASE (TRIM(instrument))
        CASE ("AMSUA") 
          READ(4,NML=AMSUA,IOSTAT=ios)
        CASE ("AMSUB")
          READ(4,NML=AMSUB,IOSTAT=ios)
        CASE ("MHS")
          READ(4,NML=MHS,IOSTAT=ios)
        CASE ("ATMS")
          READ(4,NML=ATMS,IOSTAT=ios)
        CASE ("IASI")
          READ(4,NML=IASI,IOSTAT=ios)
        CASE DEFAULT
          WRITE(*,*) "No namelist defined for ",TRIM(instrument)
      END SELECT
      IF ( ios /= 0 ) THEN
         IF (verbose > 2 )  WRITE(*,*) 'Can not read namelist ',trim(instrument),&
                                       ' from unit 4. Default values used '
      ELSE
        count=0
        DO i=1,max_satelites
          IF ( satelites(i) > 0 ) count=count+1
        ENDDO
        IF ( count == 0 ) THEN
          WRITE(*,*) 'No satelites selected for ',TRIM(instrument)
          CALL abort
        ENDIF
        instrument_nsatelites=count
        ALLOCATE(instrument_satelites(instrument_nsatelites))

        count=1
        DO i=1,max_satelites
          IF ( satelites(i) > 0 ) THEN
            instrument_satelites(count)=satelites(i)
            count=count+1
          ENDIF
        ENDDO
        satelites_from_namelist=.TRUE.
        IF ( VERBOSE > 0 ) WRITE(*,*) 'Namelist read from unit 4 for instrument ',trim(instrument)
      ENDIF
      CLOSE(4) 
    ENDIF
   END SUBROUTINE

   SUBROUTINE read_channels_from_namelist(instrument,name,channels_from_namelist)
     IMPLICIT NONE
     CHARACTER(LEN=*),INTENT(IN)          :: instrument
     CHARACTER(LEN=*),INTENT(IN)          :: name 
     LOGICAL,INTENT(OUT)                  :: channels_from_namelist
     INTEGER                              :: ios,count,i
     INTEGER,PARAMETER                    :: max_channels=366
     INTEGER,DIMENSION(max_channels)      :: channels
     NAMELIST / AMSUA_NOAA15 / channels
     NAMELIST / AMSUA_NOAA16 / channels
     NAMELIST / AMSUA_NOAA17 / channels
     NAMELIST / AMSUA_NOAA18 / channels
     NAMELIST / AMSUA_NOAA19 / channels
     NAMELIST / AMSUA_METOP1 / channels
     NAMELIST / AMSUA_METOP2 / channels
     NAMELIST / AMSUB_NOAA15 / channels
     NAMELIST / AMSUB_NOAA16 / channels
     NAMELIST / AMSUB_NOAA17 / channels
     NAMELIST / AMSUB_NOAA18 / channels
     NAMELIST / MHS_NOAA19 / channels
     NAMELIST / MHS_METOP1 / channels
     NAMELIST / MHS_METOP2 / channels
     NAMELIST / ATMS_JPSS0 / channels
     NAMELIST / IASI_METOP1 / channels
     NAMELIST / IASI_METOP2 / channels
 
     channels_from_namelist=.FALSE.
     channels=-99
     OPEN(4,IOSTAT=ios)
     IF ( ios == 0 ) THEN
      REWIND(4)
      ios=-1
      SELECT CASE (TRIM(instrument))
        CASE ("AMSUA")
          SELECT CASE (TRIM(name))
            CASE("noaa15")
              READ(4,NML=AMSUA_NOAA15,IOSTAT=ios)
            CASE("noaa16")
              READ(4,NML=AMSUA_NOAA16,IOSTAT=ios)
            CASE("noaa17")
              READ(4,NML=AMSUA_NOAA17,IOSTAT=ios)
            CASE("noaa18")
              READ(4,NML=AMSUA_NOAA18,IOSTAT=ios)
            CASE("noaa19")
              READ(4,NML=AMSUA_NOAA19,IOSTAT=ios)
            CASE("metop1")
              READ(4,NML=AMSUA_METOP1,IOSTAT=ios)
            CASE("metop2")
              READ(4,NML=AMSUA_METOP2,IOSTAT=ios)
          END SELECT
        CASE ("AMSUB")
          SELECT CASE (TRIM(name))
            CASE("noaa15")
              READ(4,NML=AMSUB_NOAA15,IOSTAT=ios)
            CASE("noaa16")
              READ(4,NML=AMSUB_NOAA16,IOSTAT=ios)
            CASE("noaa17")
              READ(4,NML=AMSUB_NOAA17,IOSTAT=ios)
            CASE("noaa18")
              READ(4,NML=AMSUB_NOAA18,IOSTAT=ios)
          END SELECT
        CASE ("MHS")
          SELECT CASE (TRIM(name))
            CASE("noaa19")
              READ(4,NML=MHS_NOAA19,IOSTAT=ios)
            CASE("metop1")
              READ(4,NML=MHS_METOP1,IOSTAT=ios)
            CASE("metop2")
              READ(4,NML=MHS_METOP2,IOSTAT=ios)
          END SELECT
        CASE ("ATMS")
          SELECT CASE (TRIM(name))
            CASE("jpss0")
              READ(4,NML=ATMS_JPSS0,IOSTAT=ios)
          END SELECT
        CASE ("IASI")
          SELECT CASE (TRIM(name))
            CASE("metop1")
              READ(4,NML=IASI_METOP1,IOSTAT=ios)
            CASE("metop2")
              READ(4,NML=IASI_METOP2,IOSTAT=ios)
          END SELECT
        CASE DEFAULT
          WRITE(*,*) "No namelist defined for ",TRIM(instrument)
      END SELECT
      IF ( ios /= 0 ) THEN
        IF (verbose > 2 ) WRITE(*,*) 'Can not read namelist ',trim(instrument),&
                                     ' for satelite ',trim(name),' from unit 4. Default values used'
      ELSE
        count=0
        DO i=1,max_channels
          IF ( channels(i) > 0 ) count=count+1
        ENDDO
        IF ( count == 0 ) THEN
          WRITE(*,*) 'No channels selected for ',TRIM(instrument),' and satelite ',trim(name)
          CALL abort
        ENDIF
        instrument_used_channels=count
        ALLOCATE(instrument_channels(instrument_used_channels))

        count=1
        DO i=1,max_channels
          IF ( channels(i) > 0 ) THEN
            instrument_channels(count)=channels(i)
            count=count+1
          ENDIF
        ENDDO
        channels_from_namelist=.TRUE.
        IF ( VERBOSE > 0 ) WRITE(*,*) 'Namelist read from unit 4 for instrument ',&
                                      trim(instrument),' and satelite ',trim(name)
      ENDIF
    ENDIF
    CLOSE(4)
  END SUBROUTINE read_channels_from_namelist
END MODULE
