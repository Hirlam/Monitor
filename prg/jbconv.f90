!
!     Program to convert background error statistics files for Harmonie
!     from one geometry to another - the only requirement is that wave
!     number 1 in the output geometry should be shorter than wave
!     number 1 in the input geometry
!  
      implicit none
!
!   Namelist definition
!
      integer(kind=4)   :: nlon_in,nlat_in,nezone_in
      real(kind=8)      :: lonc_in,latc_in,lon0_in,lat0_in,gsize_in
      real(kind=8)      :: lon1_in,lat1_in,lon2_in,lat2_in
      integer(kind=4)   :: nlon_out,nlat_out,nezone_out
      real(kind=8)      :: lonc_out,latc_out,lon0_out,lat0_out,gsize_out 
      real(kind=8)      :: lon1_out,lat1_out,lon2_out,lat2_out
      namelist/namjbconv/ nlon_in,nlat_in,nezone_in,&
                    &     lonc_in,latc_in,lon0_in,lat0_in,gsize_in,&
                    &     lon1_in,lat1_in,lon2_in,lat2_in,&
                    &     nlon_out,nlat_out,nezone_out,&
                    &     lonc_out,latc_out,lon0_out,lat0_out,gsize_out,&
                    &     lon1_out,lat1_out,lon2_out,lat2_out
! 
!   Variables read from input file or written to output file
!
      character :: clid*10,clcom*70
      integer(kind=4)   :: iorig,idate,itime,inbset
      integer(kind=4)   :: inbmat,iweight,itypmat,isetdist,ilendef,ios
      integer(kind=4)   :: idim1,idim2,ipar1,ipar2,itypdi1,itypdi2
      real(kind=8)      :: zlon1,zlat1,zlon2,zlat2,zlon0,zlat0
      integer(kind=4)   :: idgl,idlon,idgux,idlux,iismax,imsmax,&
           &               iflevg,ichkwd 
      real(kind=8),allocatable      :: zfcovtps_in(:,:,:),&
                   &       zfcovq_in(:,:,:),&
                   &       zfcovp_in(:,:,:),zfcovd_in(:,:,:)
      real(kind=8),allocatable      :: zpdat_in(:)
      real(kind=8),allocatable      :: zfcovtps_out(:,:,:),&
                   &       zfcovq_out(:,:,:),&
                   &       zfcovp_out(:,:,:),zfcovd_out(:,:,:)
      real(kind=8),allocatable      :: zpdat_out(:)
!
      real(kind=8), allocatable ::  sdiv_in(:,:,:), stps_in(:,:,:),&
                   &                shum_in(:,:,:), bfact_in(:)
!
      real(kind=8), allocatable ::  sdiv_out(:,:,:), stps_out(:,:,:),&
                   &                shum_out(:,:,:), bfact_out(:)
!
!   File unit numbers
!
      integer(kind=4)   :: nuljb_in,nuljb_out,nulout,nulnam
!
!   Other local variables
!
      integer(kind=4)   :: itrunc,nflevg
      integer(kind=4)   :: jn,jk,jj,jn_out
      real(kind=8)      :: zdummy,zeps,zjn,wgt,zscale
      real(kind=8)      :: lx_in,ly_in,lxy_in
      real(kind=8)      :: lx_out,ly_out,lxy_out,l_out
      integer(kind=4)   :: nsmax_jb_in,nmsmax_jb_in
      integer(kind=4),allocatable :: isorel2_in(:),isodor2_in(:)
      integer(kind=4)   ::   ispec2_in,iii 
      integer(kind=4)   :: nsmax_jb_out,nmsmax_jb_out
      integer(kind=4),allocatable :: isorel2_out(:),isodor2_out(:)
      integer(kind=4)   ::   ispec2_out
      real(kind=8),allocatable  ::   pdens_in(:),pdens_out(:)
!
!
!   Initialize some work variables
!
      zeps=0.0000001
      nuljb_in=10
      nuljb_out=11
      nulout=06
      nulnam=05
!
!   Read namelist
!
      read(nulnam,namjbconv)
!
!   Check that wave number 1 in output is no longer than
!   wave number 1 in input
!
      lx_in = float(nlon_in)*gsize_in
      ly_in = float(nlat_in)*gsize_in
      lxy_in = max(lx_in,ly_in)
      lx_out = float(nlon_out)*gsize_out
      ly_out = float(nlat_out)*gsize_out
      lxy_out = max(lx_out,ly_out)
      if( lxy_out.gt.lxy_in ) then
         write(nulout,*)' Length of wavenumber 1 longer in output'
         write(nulout,*)' domain than in input domain!!!!!!!!!!'
         stop
      end if
!
!   Spectral array dimensions
!
      nsmax_jb_in = nlat_in/2  - 1
      nmsmax_jb_in = nlon_in/2 - 1
!
      nsmax_jb_out = nlat_out/2
      nmsmax_jb_out = nlon_out/2
!
!   Open input covariance file
!
      open(nuljb_in,file='stabal96.cv',form='unformatted')
!
!   Read GSA file header
!
      read(nuljb_in,iostat=ios) clid
      write(*,*) IOS
      write(nulout,'(2A)',iostat=ios) 'GSA ID=',trim(clid)
      write(*,*) IOS
      clcom=""
      read(nuljb_in,iostat=ios) clcom
      write(*,*) ios
      write(nulout,*) 'Description : ',clcom
      read(nuljb_in) iorig,idate,itime,inbset
      write(nulout,*) ' Center=',iorig,' Date=',idate,' Time=',itime
!
!   Read  GSA set 0 header : model geometry definition (LAM case !)
!
      read(nuljb_in) inbmat,iweight,itypmat,isetdist,ilendef
      write(nulout,*) ' inbmat=',inbmat,' iweight=',iweight,&
                &     ' itypmat=',itypmat
      write(nulout,*) ' isetdist=',isetdist,' ilendef=',ilendef
      read(nuljb_in) idim1,idim2,ipar1,ipar2,itypdi1,itypdi2
      write(nulout,*) ' idim1=',idim1,' idim2=',idim2,' ipar1=',ipar1
      write(nulout,*) ' ipar2=',ipar2,' itypdi1=',itypdi1,&
                &     ' itypdi2=',itypdi2
      read(nuljb_in)
      read(nuljb_in)
      read(nuljb_in) zlon1,zlat1,zlon2,zlat2,zlon0,zlat0,&
           &       idgl,idlon,idgux,idlux,iismax,imsmax,iflevg,&
           &       ichkwd
      write(nulout,*) ' zlon1=',zlon1,' zlat1=',zlat1,' zlon2=',zlon2
      write(nulout,*) ' zlat2=',zlat2,' zlon0=',zlon0,' zlat0=',zlat0
      write(nulout,*) ' idgl=',idgl,' idlon=',idlon
      write(nulout,*) ' idgux=',idgux,' idlux=',idlux
      write(nulout,*) ' iismax=', iismax,' imsmax=',imsmax
      write(nulout,*) ' iflevg=',iflevg,' ichkwd=',ichkwd
!
!   Check geometry of input covariance file
!
      if( abs(zlon1-lon1_in).gt.zeps .or. abs(zlat1-lat1_in).gt.zeps .or. &
       &  abs(zlon2-lon2_in).gt.zeps .or. abs(zlat2-lat2_in).gt.zeps .or. &
       &  abs(zlon0-lon0_in).gt.zeps .or. abs(zlat0-lat0_in).gt.zeps ) then
         write(nulout,*) 'Input file and namelist geometry disagree!'
         write(nulout,*) 'lon1_in=',lon1_in,' zlon1=',zlon1
         write(nulout,*) 'lat1_in=',lat1_in,' zlat1=',zlat1
         write(nulout,*) 'lon2_in=',lon2_in,' zlon2=',zlon2
         write(nulout,*) 'lat2_in=',lat2_in,' zlat2=',zlat2
         write(nulout,*) 'lon0_in=',lon0_in,' zlon0=',zlon0
         write(nulout,*) 'lat0_in=',lat0_in,' zlat0=',zlat0
         stop
      end if
!
      if( iismax.ne.nsmax_jb_in .or. imsmax.ne.nmsmax_jb_in ) then
         write(nulout,*)'Input file and namelist geometry disagree!'
         write(nulout,*) 'nsmax_jb_in=',nsmax_jb_in,&
                 &       ' iismax=',iismax
         write(nulout,*) 'nmsmax_jb_in=',nmsmax_jb_in,&
                 &       ' imsmax=',imsmax
         stop
      end if
!
!   Set truncation of input covariance spectrum
!   and allocate space for the input covariances
!
      itrunc=iismax
      nflevg=iflevg
      allocate(zfcovtps_in(nflevg+1,nflevg+1,0:itrunc))
      allocate(zfcovq_in(nflevg,nflevg,0:itrunc))
      allocate(zfcovd_in(nflevg,nflevg,0:itrunc))
      allocate(zfcovp_in(nflevg,nflevg,0:itrunc))
      allocate(zpdat_in(nflevg+1))
!
!   Read  GSA set 1: reading vorticity covariance matrices
!
      read(nuljb_in)inbmat,iweight,itypmat,isetdist,ilendef
      read(nuljb_in)idim1,idim2,ipar1,ipar2,itypdi1,itypdi2
      write(nulout,*) ' inbmat=',inbmat,' iweight=',iweight,&
          &                    ' itypmat=',itypmat
      write(nulout,*) ' isetdist=',isetdist,' ilendef=',ilendef
      write(nulout,*) ' idim1=',idim1,' idim2=',idim2,' ipar1=',ipar1
      write(nulout,*) ' ipar2=',ipar2,' itypdi1=',itypdi1,' itypdi2=',&
          &                    itypdi2
      read(nuljb_in)
      read(nuljb_in)
      do jn=0,itrunc
         read(nuljb_in)zdummy
         read(nuljb_in)((zfcovp_in(jj,jk,jn),jj=1,nflevg),jk=1,nflevg),&
              &                 ichkwd
      end do
!
!   Read  GSA set 2: reading unbalanced divergence covariance matrices
!
      read(nuljb_in)inbmat,iweight,itypmat,isetdist,ilendef
      read(nuljb_in)idim1,idim2,ipar1,ipar2,itypdi1,itypdi2
      write(nulout,*) ' inbmat=',inbmat,' iweight=',iweight,&
         &            ' itypmat=',itypmat
      write(nulout,*) ' isetdist=',isetdist,' ilendef=',ilendef
      write(nulout,*) ' idim1=',idim1,' idim2=',idim2,' ipar1=',ipar1
      write(nulout,*) ' ipar2=',ipar2,' itypdi1=',itypdi1,&
         &            ' itypdi2=',itypdi2
      read(nuljb_in)
      read(nuljb_in)
      do jn=0,itrunc
         read(nuljb_in)zdummy
         read(nuljb_in)((zfcovd_in(jj,jk,jn),jj=1,nflevg),jk=1,nflevg),&
            &            ichkwd
      end do
!
!   Read  GSA set 1: reading T,lnps covariance matrices
!
      read(nuljb_in)inbmat,iweight,itypmat,isetdist,ilendef
      read(nuljb_in)idim1,idim2,ipar1,ipar2,itypdi1,itypdi2
      write(nulout,*) ' inbmat=',inbmat,' iweight=',iweight,&
          &               ' itypmat=',itypmat
      write(nulout,*) ' isetdist=',isetdist,' ilendef=',ilendef
      write(nulout,*) ' idim1=',idim1,' idim2=',idim2,' ipar1=',ipar1
      write(nulout,*) ' ipar2=',ipar2,' itypdi1=',itypdi1,' itypdi2=',&
           &                   itypdi2
      read(nuljb_in) (zpdat_in(jj),jj=1,idim1)
      read(nuljb_in)
      do jn=0,itrunc
         read(nuljb_in) zdummy
         read(nuljb_in)&
       &    ((zfcovtps_in(jj,jk,jn),jj=1,nflevg+1),jk=1,nflevg+1),ichkwd
      end do
!
!   Read  GSA set 1: reading unbalanced q covariance matrices
!
      read(nuljb_in)inbmat,iweight,itypmat,isetdist,ilendef
      read(nuljb_in)idim1,idim2,ipar1,ipar2,itypdi1,itypdi2
      write(nulout,*) ' inbmat=',inbmat,' iweight=',iweight,&
            &           ' itypmat=',itypmat
      write(nulout,*) ' isetdist=',isetdist,' ilendef=',ilendef
      write(nulout,*) ' idim1=',idim1,' idim2=',idim2,' ipar1=',ipar1
      write(nulout,*) ' ipar2=',ipar2,' itypdi1=',itypdi1,&
            &           ' itypdi2=',itypdi2
      read(nuljb_in)
      read(nuljb_in)
      do jn=0,itrunc
         read(nuljb_in)zdummy
         read(nuljb_in)((zfcovq_in(jj,jk,jn),jj=1,nflevg),jk=1,nflevg),&
               &          ichkwd
      end do
!
!   Close input covariance file
!
      close(nuljb_in)
!
!   Allocate space for output covariances
!
      allocate(zfcovtps_out(nflevg+1,nflevg+1,0:nsmax_jb_out))
      allocate(zfcovq_out(nflevg,nflevg,0:nsmax_jb_out))
      allocate(zfcovd_out(nflevg,nflevg,0:nsmax_jb_out))
      allocate(zfcovp_out(nflevg,nflevg,0:nsmax_jb_out))
      allocate(zpdat_out(nflevg+1))
!
!   Interpolate output covariances from input covariances
!
      zscale = float(nlon_in)*float(nlat_in)/(float(nlon_out)*float(nlat_out))
      zfcovtps_out(:,:,0) = zscale*zfcovtps_in(:,:,0)
      zfcovp_out(:,:,0) = zscale*zfcovp_in(:,:,0)
      zfcovd_out(:,:,0) = zscale*zfcovd_in(:,:,0)
      zfcovq_out(:,:,0) = zscale*zfcovq_in(:,:,0)
      do jn_out=1,nsmax_jb_out
         l_out = lxy_out/float(jn_out)
         zjn = lxy_in/l_out
         jn  = int(zjn)
         wgt = zjn - float(jn)
         if( jn.lt.nsmax_jb_in ) then
            zfcovtps_out(:,:,jn_out) = zscale*(&
                         &              (1.-wgt)*zfcovtps_in(:,:,jn) + &
                         &                   wgt*zfcovtps_in(:,:,jn+1)     )
            zfcovp_out(:,:,jn_out) = zscale*(&
                         &              (1.-wgt)*zfcovp_in(:,:,jn) + &
                         &                   wgt*zfcovp_in(:,:,jn+1)     )
            zfcovd_out(:,:,jn_out) = zscale*(&
                         &              (1.-wgt)*zfcovd_in(:,:,jn) + &
                         &                   wgt*zfcovd_in(:,:,jn+1)     )
            zfcovq_out(:,:,jn_out) = zscale*(&
                         &              (1.-wgt)*zfcovq_in(:,:,jn) + &
                         &                   wgt*zfcovq_in(:,:,jn+1)     )
         else
            zfcovtps_out(:,:,jn_out) = zscale*zfcovtps_in(:,:,nsmax_jb_in) 
            zfcovp_out(:,:,jn_out) = zscale*zfcovp_in(:,:,nsmax_jb_in)
            zfcovd_out(:,:,jn_out) = zscale*zfcovd_in(:,:,nsmax_jb_in)
            zfcovq_out(:,:,jn_out) = zscale*zfcovq_in(:,:,nsmax_jb_in) 
         end if
      end do
!
      zpdat_out = zpdat_in
!
      allocate(pdens_in(0:nsmax_jb_in))
      allocate(pdens_out(0:nsmax_jb_out))
      do jn=0,nsmax_jb_in
         pdens_in(jn) = zfcovp_in(30,30,jn)
      end do
      do jn=0,nsmax_jb_out
         pdens_out(jn) = zfcovp_out(30,30,jn)
      end do
      do jn=0,nsmax_jb_in
         write(nulout,*)' jn,pdens_in=',jn,pdens_in(jn)
      end do
      do jn=0,nsmax_jb_out
         write(nulout,*)' jn,pdens_out=',jn,pdens_out(jn)
      end do
      deallocate(pdens_in)
      deallocate(pdens_out)
!
!   Open output covariance file
!
      open(nuljb_out,file='stabal96_out.cv',form='unformatted')
!
!   Write GSA file header
!
      write(nuljb_out) clid
      write(nuljb_out) clcom
      write(nuljb_out) iorig,idate,itime,inbset
!
!   Write  GSA set 0 header : model geometry definition (LAM case !)
!
      inbmat   = 1
      iweight  = 168
      itypmat   = 0
      isetdist = 1
      ilendef  = 0
      write(nuljb_out) inbmat,iweight,itypmat,isetdist,ilendef
      idim1    = 1
      idim2    = 13
      ipar1    = 50
      ipar2    = 0
      itypdi1  = 0
      itypdi2  = 0
      write(nuljb_out) idim1,idim2,ipar1,ipar2,itypdi1,itypdi2
      write(nuljb_out)
      write(nuljb_out)
      idgl     = nlat_out
      idlon    = nlon_out
      idgux    = nlat_out - nezone_out
      idlux    = nlon_out - nezone_out
      iismax   = nlat_out/2 
      imsmax   = nlon_out/2 
      iflevg   = nflevg
      ichkwd   = 3141592
      write(nuljb_out) lon1_out,lat1_out,lon2_out,lat2_out,&
           &       lon0_out,lat0_out,&
           &       idgl,idlon,idgux,idlux,iismax,imsmax,iflevg,&
           &       ichkwd
!
!   Write  GSA set 1 header : vertical level definition
!
      inbmat=300
      iweight=45
      itypmat=1
      isetdist=2
      ilendef=1
      write(nuljb_out)inbmat,iweight,itypmat,isetdist,ilendef
      idim1 = nflevg
      idim2 = nflevg
      ipar1 = 4
      ipar2 = 4
      itypdi1 = 1
      itypdi2 = 1
      write(nuljb_out)idim1,idim2,ipar1,ipar2,itypdi1,itypdi2
      write(nuljb_out)
      write(nuljb_out)
!
!   Write  GSA set 1: writing vorticity covariance matrices
!
      do jn=0,iismax
         zdummy = 0.
         write(nuljb_out) zdummy
         ichkwd = 3141592
         write(nuljb_out) &
       &    ((zfcovp_out(jj,jk,jn),jj=1,nflevg),jk=1,nflevg),ichkwd
      end do
!
!   Write  GSA set 2: writing unbalanced divergence covariance matrices
!
      write(nuljb_out)inbmat,iweight,itypmat,isetdist,ilendef
      idim1 = nflevg
      idim2 = nflevg
      ipar1 = 12
      ipar2 = 12
      write(nuljb_out)idim1,idim2,ipar1,ipar2,itypdi1,itypdi2
      write(nuljb_out)
      write(nuljb_out)
      do jn=0,iismax
         zdummy = 0.
         write(nuljb_out) zdummy
         ichkwd = 3141592
         write(nuljb_out) &  
       &   ((zfcovd_out(jj,jk,jn),jj=1,nflevg),jk=1,nflevg),ichkwd
      end do
!
!   Write  GSA set 1: writing T,lnps covariance matrices
!
      write(nuljb_out)inbmat,iweight,itypmat,isetdist,ilendef
      idim1 = nflevg + 1
      idim2 = nflevg + 1
      ipar1 = 14
      ipar2 = 14
      write(nuljb_out)idim1,idim2,ipar1,ipar2,itypdi1,itypdi2
      write(nuljb_out) (zpdat_out(jj),jj=1,nflevg+1)
      write(nuljb_out)
      do jn=0,iismax
         zdummy = 0.
         write(nuljb_out) zdummy
         ichkwd = 3141592
         write(nuljb_out)&
       &   ((zfcovtps_out(jj,jk,jn),jj=1,nflevg+1),jk=1,nflevg+1),ichkwd
      end do
!
!   Write  GSA set 1: writing unbalanced q covariance matrices
!
      write(nuljb_out)inbmat,iweight,itypmat,isetdist,ilendef
      idim1 = nflevg
      idim2 = nflevg
      ipar1 = 17
      ipar2 = 17
      write(nuljb_out)idim1,idim2,ipar1,ipar2,itypdi1,itypdi2
      write(nuljb_out)
      write(nuljb_out)
      do jn=0,iismax
         zdummy = 0.
         write(nuljb_out) zdummy
         ichkwd = 3141592
         write(nuljb_out) &
       &    ((zfcovq_out(jj,jk,jn),jj=1,nflevg),jk=1,nflevg),ichkwd
      end do
!
!   Close output covariance file
!
      close(nuljb_out)
!
!   De-allocate space for input and output covariances
!
      deallocate(zfcovp_in)
      deallocate(zfcovd_in)
      deallocate(zfcovtps_in)
      deallocate(zfcovq_in)
      deallocate(zpdat_in)
      deallocate(zfcovp_out)
      deallocate(zfcovd_out)
      deallocate(zfcovtps_out)
      deallocate(zfcovq_out)
      deallocate(zpdat_out)
!
!   Open input balancing file
!
      open(nuljb_in,file='stabal96.bal',form='unformatted')
!
!   Read GSA file header
!
      read(nuljb_in) clid
      write(nulout,*) 'GSA ID=',clid
      read(nuljb_in) clcom
      write(nulout,*) 'Description : ',clcom
      read(nuljb_in) iorig,idate,itime,inbset
      write(nulout,*) ' Center=',iorig,' Date=',idate,' Time=',itime
!
!   Read  GSA set 0 header : model geometry definition (LAM case !)
!
      read(nuljb_in) inbmat,iweight,itypmat,isetdist,ilendef
      write(nulout,*) ' inbmat=',inbmat,' iweight=',iweight,&
          &           ' itypmat=',itypmat
      write(nulout,*) ' isetdist=',isetdist,' ilendef=',ilendef
      read(nuljb_in) idim1,idim2,ipar1,ipar2,itypdi1,itypdi2
      write(nulout,*) ' idim1=',idim1,' idim2=',idim2,' ipar1=',ipar1
      write(nulout,*) ' ipar2=',ipar2,' itypdi1=',itypdi1,&
          &           ' itypdi2=',itypdi2
      read(nuljb_in)
      read(nuljb_in)
      read(nuljb_in) zlon1,zlat1,zlon2,zlat2,zlon0,zlat0,&
           &       idgl,idlon,idgux,idlux,iismax,imsmax,iflevg,&
           &       ichkwd
      write(nulout,*) ' zlon1=',zlon1,' zlat1=',zlat1,' zlon2=',zlon2
      write(nulout,*) ' zlat2=',zlat2,' zlon0=',zlon0,' zlat0=',zlat0
      write(nulout,*) ' idgl=',idgl,' idlon=',idlon
      write(nulout,*) ' idgux=',idgux,' idlux=',idlux
      write(nulout,*) ' iismax=', iismax,' imsmax=',imsmax
      write(nulout,*) ' iflevg=',iflevg,' ichkwd=',ichkwd
!
!   Check geometry of input balancing file
!
      if( abs(zlon1-lon1_in).gt.zeps .or. abs(zlat1-lat1_in).gt.zeps .or. &
       &  abs(zlon2-lon2_in).gt.zeps .or. abs(zlat2-lat2_in).gt.zeps .or. &
       &  abs(zlon0-lon0_in).gt.zeps .or. abs(zlat0-lat0_in).gt.zeps ) then
         write(nulout,*) 'Input file and namelist geometry disagree!'
         write(nulout,*) 'lon1_in=',lon1_in,' zlon1=',zlon1
         write(nulout,*) 'lat1_in=',lat1_in,' zlat1=',zlat1
         write(nulout,*) 'lon2_in=',lon2_in,' zlon2=',zlon2
         write(nulout,*) 'lat2_in=',lat2_in,' zlat2=',zlat2
         write(nulout,*) 'lon0_in=',lon0_in,' zlon0=',zlon0
         write(nulout,*) 'lat0_in=',lat0_in,' zlat0=',zlat0
         stop
      end if
!
      if( iismax.ne.nsmax_jb_in .or. imsmax.ne.nmsmax_jb_in ) then
         write(nulout,*)'Input file and namelist geometry disagree!'
         write(nulout,*) 'nsmax_jb_in=',nsmax_jb_in,&
                 &       ' iismax=',iismax
         write(nulout,*) 'nmsmax_jb_in=',nmsmax_jb_in,&
                 &       ' imsmax=',imsmax
         stop
      end if
!
!   Set truncation of input balancing operators
!   and allocate space for the input balancing operators
!
      itrunc=iismax
      nflevg=iflevg
      allocate(isorel2_in(0:nsmax_jb_in))
      allocate(isodor2_in(0:nmsmax_jb_in))
      isorel2_in(:)=0
      isodor2_in(:)=0
      call ellips(nsmax_jb_in,nmsmax_jb_in,isodor2_in,isorel2_in)
      ispec2_in=0
      do iii=0,nmsmax_jb_in
         ispec2_in=ispec2_in+4*(isodor2_in(iii)+1)
      end do
      write(nulout,*)' calculated ispec2_in=',ispec2_in
      deallocate(isorel2_in)
      deallocate(isodor2_in)
!      
      allocate(sdiv_in(nflevg,nflevg,0:itrunc))
      allocate(stps_in(nflevg+1,2*nflevg,0:itrunc))
      allocate(shum_in(nflevg,3*nflevg+1,0:itrunc))
!
!   BFACT are allocated and read here
!
      read(nuljb_in)inbmat,iweight,itypmat,isetdist,ilendef
      read(nuljb_in)idim1,idim2,ipar1,ipar2,itypdi1,itypdi2
      write(nulout,*) ' inbmat=',inbmat,' iweight=',iweight,&
          &           ' itypmat=',itypmat
      write(nulout,*) ' isetdist=',isetdist,' ilendef=',ilendef
      write(nulout,*) ' idim1=',idim1,' idim2=',idim2,' ipar1=',ipar1
      write(nulout,*) ' ipar2=',ipar2,' itypdi1=',itypdi1,&
          &           ' itypdi2=',itypdi2
!
!   Check dimension of BFACT
!
      if( ispec2_in.ne.idim2 ) then
         write(nulout,*)' ispec2_in=',ispec2_in,' NE idim2=',idim2
         stop
      end if
!
      read(nuljb_in)
      read(nuljb_in)
      allocate(bfact_in(ispec2_in))
      read(nuljb_in)
      read(nuljb_in) (bfact_in(jj),jj=1,ispec2_in),ichkwd
!
!   Read  GSA set 2 : SDIV matrices
!
      read(nuljb_in)inbmat,iweight,itypmat,isetdist,ilendef
      read(nuljb_in)idim1,idim2,ipar1,ipar2,itypdi1,itypdi2
      write(nulout,*) ' inbmat=',inbmat,' iweight=',iweight,&
           &          ' itypmat=',itypmat
      write(nulout,*) ' isetdist=',isetdist,' ilendef=',ilendef
      write(nulout,*) ' idim1=',idim1,' idim2=',idim2,' ipar1=',ipar1
      write(nulout,*) ' ipar2=',ipar2,' itypdi1=',itypdi1,&
           &          ' itypdi2=',itypdi2
      read(nuljb_in)
      read(nuljb_in)
      do jn=0,itrunc
         read(nuljb_in)
         read(nuljb_in) ((sdiv_in(jj,jk,jn),jk=1,nflevg),jj=1,nflevg),&
              &            ichkwd
      end do
!
!   Read  GSA set 3: STPS/Pb matrices
!
      read(nuljb_in)inbmat,iweight,itypmat,isetdist,ilendef
      read(nuljb_in)idim1,idim2,ipar1,ipar2,itypdi1,itypdi2
      write(nulout,*) ' inbmat=',inbmat,' iweight=',iweight,&
          &            ' itypmat=',itypmat
      write(nulout,*) ' isetdist=',isetdist,' ilendef=',ilendef
      write(nulout,*) ' idim1=',idim1,' idim2=',idim2,' ipar1=',ipar1
      write(nulout,*) ' ipar2=',ipar2,' itypdi1=',itypdi1,&
          &            ' itypdi2=',itypdi2
      read(nuljb_in)
      read(nuljb_in)
      do jn=0,itrunc
         read(nuljb_in) 
         read(nuljb_in)((stps_in(jj,jk,jn),jk=1,nflevg),jj=1,nflevg+1),&
             &              ichkwd
      end do
!
!   Read  GSA set 4: STPS/divu matrices
!
      read(nuljb_in)inbmat,iweight,itypmat,isetdist,ilendef
      read(nuljb_in)idim1,idim2,ipar1,ipar2,itypdi1,itypdi2
      write(nulout,*) ' inbmat=',inbmat,' iweight=',iweight,&
          &            ' itypmat=',itypmat
      write(nulout,*) ' isetdist=',isetdist,' ilendef=',ilendef
      write(nulout,*) ' idim1=',idim1,' idim2=',idim2,' ipar1=',ipar1
      write(nulout,*) ' ipar2=',ipar2,' itypdi1=',itypdi1,&
          &            ' itypdi2=',itypdi2
      read(nuljb_in)
      read(nuljb_in)
      do jn=0,itrunc
         read(nuljb_in) 
         read(nuljb_in)&
       &    ((stps_in(jj,nflevg+jk,jn),jk=1,nflevg),jj=1,nflevg+1),&
       &         ichkwd
      end do
!
!   Read  GSA set 5:   SHUM/Pb matrices
!
      read(nuljb_in)inbmat,iweight,itypmat,isetdist,ilendef
      read(nuljb_in)idim1,idim2,ipar1,ipar2,itypdi1,itypdi2
      write(nulout,*) ' inbmat=',inbmat,' iweight=',iweight,&
          &            ' itypmat=',itypmat
      write(nulout,*) ' isetdist=',isetdist,' ilendef=',ilendef
      write(nulout,*) ' idim1=',idim1,' idim2=',idim2,' ipar1=',ipar1
      write(nulout,*) ' ipar2=',ipar2,' itypdi1=',itypdi1,&
          &            ' itypdi2=',itypdi2
      read(nuljb_in)
      read(nuljb_in)
      do jn=0,itrunc
         read(nuljb_in)
         read(nuljb_in) ((shum_in(jj,jk,jn),jk=1,nflevg),jj=1,nflevg),&
             &               ichkwd
      end do
!
!   Read  GSA set 6:   SHUM/Divu matrices
!
      read(nuljb_in)inbmat,iweight,itypmat,isetdist,ilendef
      read(nuljb_in)idim1,idim2,ipar1,ipar2,itypdi1,itypdi2
      write(nulout,*) ' inbmat=',inbmat,' iweight=',iweight,&
          &            ' itypmat=',itypmat
      write(nulout,*) ' isetdist=',isetdist,' ilendef=',ilendef
      write(nulout,*) ' idim1=',idim1,' idim2=',idim2,' ipar1=',ipar1
      write(nulout,*) ' ipar2=',ipar2,' itypdi1=',itypdi1,&
          &            ' itypdi2=',itypdi2
      read(nuljb_in)
      read(nuljb_in)
      do jn=0,itrunc
         read(nuljb_in) 
         read(nuljb_in) &
           &  ((shum_in(jj,nflevg+jk,jn),jk=1,nflevg),jj=1,nflevg),&
           &            ichkwd
      end do
!
!   Read  GSA set 7:   SHUM/Tpsu matrices
!
      read(nuljb_in)inbmat,iweight,itypmat,isetdist,ilendef
      read(nuljb_in)idim1,idim2,ipar1,ipar2,itypdi1,itypdi2
      write(nulout,*) ' inbmat=',inbmat,' iweight=',iweight,&
          &            ' itypmat=',itypmat
      write(nulout,*) ' isetdist=',isetdist,' ilendef=',ilendef
      write(nulout,*) ' idim1=',idim1,' idim2=',idim2,' ipar1=',ipar1
      write(nulout,*) ' ipar2=',ipar2,' itypdi1=',itypdi1,&
          &            ' itypdi2=',itypdi2
      read(nuljb_in)
      read(nuljb_in)
      do jn=0,itrunc
         read(nuljb_in) 
         read(nuljb_in) &
           &  ((shum_in(jj,2*nflevg+jk,jn),jk=1,nflevg+1),jj=1,nflevg),&
           &                  ichkwd
      end do
!
!   Close input balancing file
!
      close(nuljb_in)
!
!   Allocate space for output balance operators
!
      allocate(isorel2_out(0:nsmax_jb_out))
      allocate(isodor2_out(0:nmsmax_jb_out))
      isorel2_out(:)=0
      isodor2_out(:)=0
      call ellips(nsmax_jb_out,nmsmax_jb_out,isodor2_out,isorel2_out)
      ispec2_out=0
      do iii=0,nmsmax_jb_out
         ispec2_out=ispec2_out+4*(isodor2_out(iii)+1)
      end do
      write(nulout,*)' calculated ispec2_out=',ispec2_out
      deallocate(isorel2_out)
      deallocate(isodor2_out)
!      
      allocate(sdiv_out(nflevg,nflevg,0:nsmax_jb_out))
      allocate(stps_out(nflevg+1,2*nflevg,0:nsmax_jb_out))
      allocate(shum_out(nflevg,3*nflevg+1,0:nsmax_jb_out))
      allocate(bfact_out(ispec2_out))
!
!   Interpolate output balance operators from input balance operators
!
      sdiv_out(:,:,0) = sdiv_in(:,:,0)
      stps_out(:,:,0) = stps_in(:,:,0)
      shum_out(:,:,0) = shum_in(:,:,0)
      do jn_out=1,nsmax_jb_out
         l_out = lxy_out/float(jn_out)
         zjn = lxy_in/l_out
         jn  = int(zjn)
         wgt = zjn - float(jn)
         if( jn.lt.nsmax_jb_in ) then
            sdiv_out(:,:,jn_out) = (1.-wgt)*sdiv_in(:,:,jn) + &
                         &              wgt*sdiv_in(:,:,jn+1)
            stps_out(:,:,jn_out) = (1.-wgt)*stps_in(:,:,jn) + &
                         &              wgt*stps_in(:,:,jn+1)
            shum_out(:,:,jn_out) = (1.-wgt)*shum_in(:,:,jn) + &
                         &              wgt*shum_in(:,:,jn+1)
         else
            sdiv_out(:,:,jn_out) = sdiv_in(:,:,nsmax_jb_in)
            stps_out(:,:,jn_out) = stps_in(:,:,nsmax_jb_in) 
            shum_out(:,:,jn_out) = shum_in(:,:,nsmax_jb_in) 
         end if
      end do
!
      call bfact_conv(nsmax_jb_in,nmsmax_jb_in,ispec2_in,lxy_in,bfact_in,&
         &      nsmax_jb_out,nmsmax_jb_out,ispec2_out,lxy_out,bfact_out,&
         &      1.5)
!
      write(nulout,*)' bfactor_in'
      call bfact_print(nulout,nsmax_jb_in,nmsmax_jb_in,ispec2_in,bfact_in)
!
      write(nulout,*)' bfactor_out'
      call bfact_print(nulout,nsmax_jb_out,nmsmax_jb_out,ispec2_out,bfact_out)
!
!   Open output balance operator file
!
      open(nuljb_out,file='stabal96_out.bal',form='unformatted')
!
!   Write GSA file header
!
      write(nuljb_out) clid
      write(nuljb_out) clcom
      write(nuljb_out) iorig,idate,itime,inbset
!
!   Write  GSA set 0 header : model geometry definition (LAM case !)
!
      inbmat   = 1
      iweight  = 168
      itypmat   = 0
      isetdist = 1
      ilendef  = 0
      write(nuljb_out) inbmat,iweight,itypmat,isetdist,ilendef
      idim1    = 1
      idim2    = 13
      ipar1    = 50
      ipar2    = 0
      itypdi1  = 0
      itypdi2  = 0
      write(nuljb_out) idim1,idim2,ipar1,ipar2,itypdi1,itypdi2
      write(nuljb_out)
      write(nuljb_out)
      idgl     = nlat_out
      idlon    = nlon_out
      idgux    = nlat_out - nezone_out
      idlux    = nlon_out - nezone_out
      iismax   = nsmax_jb_out
      imsmax  =  nmsmax_jb_out
      iflevg   = nflevg
      ichkwd   = 3141592
      write(nuljb_out) lon1_out,lat1_out,lon2_out,lat2_out,&
           &       lon0_out,lat0_out,&
           &       idgl,idlon,idgux,idlux,iismax,imsmax,iflevg,&
           &       ichkwd
      write(nulout,*) lon1_out,lat1_out,lon2_out,lat2_out,&
           &       lon0_out,lat0_out,&
           &       idgl,idlon,idgux,idlux,iismax,imsmax,iflevg,&
           &       ichkwd
!
!     write BFACT
!
      idim1=1
      idim2=ispec2_out
      ipar1=15
      itypmat=4
      itypdi1=0
      itypdi2=3
      write(nuljb_out)inbmat,iweight,itypmat,isetdist,ilendef
      write(nuljb_out)idim1,idim2,ipar1,ipar2,itypdi1,itypdi2
      write(nuljb_out)
      write(nuljb_out)
      write(nuljb_out)
      write(nuljb_out) (bfact_out(jj),jj=1,ispec2_out),ichkwd
!
!   Write  GSA set 2 : SDIV matrices
!
      itypmat=5
      ipar1=11
      ipar2=15
      idim1=nflevg
      idim2=nflevg
      write(nuljb_out)inbmat,iweight,itypmat,isetdist,ilendef
      write(nuljb_out)idim1,idim2,ipar1,ipar2,itypdi1,itypdi2
      write(nuljb_out)
      write(nuljb_out)
      do jn=0,nsmax_jb_out
         write(nuljb_out)
         write(nuljb_out) &
          &    ((sdiv_out(jj,jk,jn),jk=1,nflevg),jj=1,nflevg),ichkwd
      end do
!
!   write  GSA set 3: STPS/Pb matrices
!
      ipar1=13
      ipar2=15
      idim1=nflevg+1
      idim2=nflevg
      write(nuljb_out)inbmat,iweight,itypmat,isetdist,ilendef
      write(nuljb_out)idim1,idim2,ipar1,ipar2,itypdi1,itypdi2
      write(nuljb_out)
      write(nuljb_out)
      do jn=0,nsmax_jb_out
         write(nuljb_out) 
         write(nuljb_out) &
            & ((stps_out(jj,jk,jn),jk=1,nflevg),jj=1,nflevg+1),ichkwd
      end do
!
!   Write  GSA set 4: STPS/divu matrices
!
      ipar1=13
      ipar2=12
      idim1=nflevg+1
      idim2=nflevg
      write(nuljb_out)inbmat,iweight,itypmat,isetdist,ilendef
      write(nuljb_out)idim1,idim2,ipar1,ipar2,itypdi1,itypdi2
      write(nuljb_out)
      write(nuljb_out)
      do jn=0,nsmax_jb_out
         write(nuljb_out) 
         write(nuljb_out)&
       &  ((stps_out(jj,nflevg+jk,jn),jk=1,nflevg),jj=1,nflevg+1),ichkwd
      end do
!
!   write  GSA set 5:   SHUM/Pb matrices
!
      ipar1=16
      ipar2=15
      idim1=nflevg
      idim2=nflevg
      write(nuljb_out)inbmat,iweight,itypmat,isetdist,ilendef
      write(nuljb_out)idim1,idim2,ipar1,ipar2,itypdi1,itypdi2
      write(nuljb_out)
      write(nuljb_out)
      do jn=0,nsmax_jb_out
         write(nuljb_out)
         write(nuljb_out)  &
          &   ((shum_out(jj,jk,jn),jk=1,nflevg),jj=1,nflevg),ichkwd
      end do
!
!   Write  GSA set 6:   SHUM/Divu matrices
!
      ipar1=16
      ipar2=12
      idim1=nflevg
      idim2=nflevg
      write(nuljb_out)inbmat,iweight,itypmat,isetdist,ilendef
      write(nuljb_out)idim1,idim2,ipar1,ipar2,itypdi1,itypdi2
      write(nuljb_out)
      write(nuljb_out)
      do jn=0,nsmax_jb_out
         write(nuljb_out) 
         write(nuljb_out) &
         &  ((shum_out(jj,nflevg+jk,jn),jk=1,nflevg),jj=1,nflevg),ichkwd
      end do
!
!   Write  GSA set 7:   SHUM/Tpsu matrices
!
      ipar1=16
      ipar2=14
      idim1=nflevg
      idim2=nflevg+1
      write(nuljb_out)inbmat,iweight,itypmat,isetdist,ilendef
      write(nuljb_out)idim1,idim2,ipar1,ipar2,itypdi1,itypdi2
      write(nuljb_out)
      write(nuljb_out)
      do jn=0,nsmax_jb_out
         write(nuljb_out) 
         write(nuljb_out) &
         & ((shum_out(jj,2*nflevg+jk,jn),jk=1,nflevg+1),jj=1,nflevg),&
         &           ichkwd
      end do
!
!   Close output balancing file
!
      close(nuljb_out)
!
!   De-allocate space for input and output balance operators
!
      deallocate(sdiv_in)
      deallocate(stps_in)
      deallocate(shum_in)
      deallocate(bfact_in)
      deallocate(sdiv_out)
      deallocate(stps_out)
      deallocate(shum_out)
      deallocate(bfact_out)
!
      stop
      end
      subroutine bfact_conv(nsmax_in,nmsmax_in,ispec2_in,lxy_in,bfact_in,&
         &      nsmax_out,nmsmax_out,ispec2_out,lxy_out,bfact_out,&
         &      ppdjkstar)
!
!  Subroutine to convert the balance operator matrix BFACT
!  (used in the regression of linearized geopotential P on vorticity)
!  from one spectral geometry to another
!
      implicit none
!
      integer(kind=4)      ::  nsmax_in,nmsmax_in,ispec2_in
      real(kind=8)         ::  lxy_in,bfact_in(ispec2_in)
      integer(kind=4)      ::  nsmax_out,nmsmax_out,ispec2_out
      real(kind=8)         ::  lxy_out,bfact_out(ispec2_out)
      real(kind=8)         ::  ppdjkstar
!
      real(kind=8)         ::  zink(0:nsmax_in)
      real(kind=8)         ::  fact1_iso(0:nsmax_in)
!
      integer(kind=4)      ::  kntmp_in(0:nmsmax_in),kmtmp_in(0:nsmax_in)
      integer(kind=4)      ::  ncpl4m_in(0:nmsmax_in),nesm0_in(0:nmsmax_in)
!
      integer(kind=4)      ::  kntmp_out(0:nmsmax_out),kmtmp_out(0:nsmax_out)
      integer(kind=4)      ::  ncpl4m_out(0:nmsmax_out),nesm0_out(0:nmsmax_out)
!
      integer(kind=4)      ::  jm,jn,jkstar,in,inm
      real(kind=8)         ::  zkstar,zmf
!
!  Prepare look-up tables for elliptically truncated spectral fields
!            (input geometry)
!
      call ellips(nsmax_in,nmsmax_in,kntmp_in,kmtmp_in)
!
      do jm=0,nmsmax_in
         ncpl4m_in(jm)=4*(kntmp_in(jm)+1)
      enddo
!
      nesm0_in(0)=1
      do jm=1,nmsmax_in
         nesm0_in(jm)=nesm0_in(jm-1)+ncpl4m_in(jm-1)
      enddo
!
!  Calculate BFACT as a function of 1D wave-number from the 2D input
!
      zink(:)         = 0.
      fact1_iso(:)    = 0.
!
      fact1_iso(0)    = bfact_in(1)
!     
      do jkstar=1,nsmax_in
         do jm=0,nmsmax_in
            do jn=0,ncpl4m_in(jm)-1
               in=int(jn/4)
               inm=nesm0_in(jm)+jn
               if( (jm.ne.0).or.(in.ne.0) )then
                  zkstar=float(nsmax_in)*sqrt( &
            &         (float(in)/float(nsmax_in ))**2+ &
            &         (float(jm)/float(nmsmax_in))**2)
                  if ( (zkstar.ge.(float(jkstar)-ppdjkstar)).and.&
     &                 (zkstar.le.(float(jkstar)+ppdjkstar)) ) then
                     zink(jkstar)=zink(jkstar)+&
     &                float(2**(min(1,in))) * float(2**(min(1,jm)))&
     &                *(0.1)**(abs(float(jkstar)-zkstar))
                     zmf=float(2**(min(1,in))) * float(2**(min(1,jm))) &
     &                *(0.1)**(abs(float(jkstar)-zkstar))
                     fact1_iso(jkstar)=fact1_iso(jkstar)+zmf*bfact_in(inm)
                  endif
               endif
            enddo
         enddo
!
         fact1_iso(jkstar)=fact1_iso(jkstar)/zink(jkstar)
!
      enddo 
!
!  Prepare look-up tables for elliptically truncated spectral fields
!            (input geometry)
!
      call ellips(nsmax_out,nmsmax_out,kntmp_out,kmtmp_out)
!
      do jm=0,nmsmax_out
        ncpl4m_out(jm)=4*(kntmp_out(jm)+1)
      enddo
!
      nesm0_out(0)=1
      do jm=1,nmsmax_out
        nesm0_out(jm)=nesm0_out(jm-1)+ncpl4m_out(jm-1)
      enddo
!
!   Transfer BFACT from the 1D input geometry to the output 2D geometry
!
      bfact_out(1) = fact1_iso(0)
!
      do jm=0,nmsmax_out
         do jn=0,ncpl4m_out(jm)-1
            in=int(jn/4)
            inm=nesm0_out(jm)+jn
!
            if( (jm.ne.0).or.(in.ne.0) )then
               zkstar=float(nsmax_out)*&
                &   sqrt( (float(in)/float(nsmax_out ))**2 +& 
                &         (float(jm)/float(nmsmax_out))**2 )&
                &         *lxy_in/lxy_out
               jkstar=int(zkstar)
               if( jkstar.lt.nsmax_in )then
                  bfact_out(inm)=&
                    & (float(jkstar)+1.-zkstar)*fact1_iso(jkstar) +&
                    & (zkstar-float(jkstar))*fact1_iso(jkstar+1)
               else
                  bfact_out(inm)=fact1_iso(nsmax_in)
               endif
            endif
         enddo
      enddo
!
      return
!
      end
      subroutine bfact_print(nulout,nsmax,nmsmax,ispec2,bfact)
!
      implicit none
!
      integer(kind=4)      ::  nulout
      integer(kind=4)      ::  nsmax,nmsmax,ispec2
      real(kind=8)         ::  bfact(ispec2)
!
      integer(kind=4)      ::  kntmp(0:nmsmax),kmtmp(0:nsmax)
      integer(kind=4)      ::  ncpl4m(0:nmsmax),nesm0(0:nmsmax)
!
      integer(kind=4)      ::  jm,jn,jkstar,in,inm
      real(kind=8)         ::  zkstar,zmf
!
      real(kind=4)         ::   b_out(0:5,0:5)

      call ellips(nsmax,nmsmax,kntmp,kmtmp)
!
      do jm=0,nmsmax
        ncpl4m(jm)=4*(kntmp(jm)+1)
      enddo
!
      nesm0(0)=1
      do jm=1,nmsmax
        nesm0(jm)=nesm0(jm-1)+ncpl4m(jm-1)
      enddo
!
      b_out(:,:) = 0.
!
      do jm=0,nmsmax
         do jn=0,ncpl4m(jm)-1
            in=int(jn/4)
            inm=nesm0(jm)+jn
!
            if( jm.le.5 .and. in.le.5 ) then
               b_out(in,jm) = bfact(inm)
            end if
!
         enddo
      enddo
!
      do jm=0,5
         write(nulout,100)jm,(b_out(jm,jn),jn=0,5)
      end do
!
  100 format('jm=',i4,'bfact=',6f10.0)
      return
!
      end
      SUBROUTINE ELLIPS(KSMAX,KMSMAX,KNTMP,KMTMP)
      INTEGER(KIND=4) :: KSMAX, KMSMAX
      INTEGER(KIND=4) :: KNTMP(0:KMSMAX),KMTMP(0:KSMAX)
      INTEGER(KIND=4) :: JM, JN
      REAL(KIND=8) :: ZEPS, ZKN, ZKM, ZAUXIL
      integer, parameter :: jpdblr = 8
       ZEPS=1.E-10
       ZAUXIL=0.
 100  CONTINUE
      DO 110 JM=1,KMSMAX-1
      ZKN = REAL(KSMAX,JPDBLR)/REAL(KMSMAX,JPDBLR)*&
     & SQRT(MAX(ZAUXIL,REAL(KMSMAX**2-JM**2,JPDBLR)))
        KNTMP(JM)=INT(ZKN+ZEPS)
 110  CONTINUE

      IF( KMSMAX.EQ.0 )THEN
         KNTMP(0)=KSMAX
      ELSE
	 KNTMP(0)=KSMAX
         KNTMP(KMSMAX)=0
      ENDIF
 200  CONTINUE
      DO 210 JN=1,KSMAX-1
       ZKM = REAL(KMSMAX,JPDBLR)/REAL(KSMAX,JPDBLR)*&
     & SQRT(MAX(ZAUXIL,REAL(KSMAX**2-JN**2,JPDBLR)))
        KMTMP(JN)=INT(ZKM+ZEPS)
 210  CONTINUE

      IF( KSMAX.EQ.0 )THEN
	 KMTMP(0)=KMSMAX
      ELSE
	 KMTMP(0)=KMSMAX
         KMTMP(KSMAX)=0
      ENDIF
      RETURN
      END      
