MODULE nec2dpar
    IMPLICIT NONE

    INTEGER, PARAMETER:: NEC2REAL = SELECTED_REAL_KIND(14)

    REAL(NEC2REAL), PARAMETER :: pi = 3.141592654_NEC2REAL
    !REAL(NEC2REAL), PARAMETER :: pi = 3.141592653589793239_NEC2REAL

    INTEGER, PARAMETER      :: maxseg=500       ! Max number of segments     (Windows-95 <= 4950)
    INTEGER, PARAMETER      :: maxmat=500       ! max nr of 'in-core' alloc.    (MAXMAT <= MAXSEG)

    INTEGER, PARAMETER      :: loadmx=maxseg/10 ! Max number of LD cards
    INTEGER, PARAMETER      :: nsmax=64         ! Max number of EX cards
    INTEGER, PARAMETER      :: netmx=64         ! Max number of segs connected to NT/TL 

    ! FIXME fix where this is used right - hwh
    INTEGER, PARAMETER      :: netmxp1=65       ! Max number of segs connected to NT/TL 

    INTEGER, PARAMETER      :: jmax=60          ! Max segments connected to a single segment or junction

    character (LEN=26)      :: g77port = 'GNU Fortran (Ubuntu) 4.8.1'

    logical                 :: debugging = .TRUE.
    !logical                 :: debugging = .FALSE.

END MODULE

!***********************************************************************
!
! formerly BLOCK DATA somset with common /ggrid/
!
MODULE somset
    USE nec2dpar
    IMPLICIT NONE

    REAL(NEC2REAL), DIMENSION(3)          :: dxa(3) = (/.02,.05,.1/)
    REAL(NEC2REAL), DIMENSION(3)          :: dya(3) = (/.1745329252,.0872664626,.1745329252/)
    REAL(NEC2REAL), DIMENSION(3)          :: xsa(3) = (/0.,.2,.2/)
    REAL(NEC2REAL), DIMENSION(3)          :: ysa(3) = (/0.,0.,.3490658504/)
    INTEGER, DIMENSION(3)                 :: nxa(3) = (/11,17,9/)
    INTEGER, DIMENSION(3)                 :: nya(3) = (/10,5,8/)

    COMPLEX*16                            :: ar1(11,10,4)
    COMPLEX*16                            :: ar2(17,5,4)
    COMPLEX*16                            :: ar3(9,8,4)
    COMPLEX*16                            :: epscf

END MODULE

!***********************************************************************
!
! formerly common /plot/
!
MODULE plot
    IMPLICIT NONE

    INTEGER                               :: iplp1
    INTEGER                               :: iplp2
    INTEGER                               :: iplp3
    INTEGER                               :: iplp4

END MODULE

!***********************************************************************
!
! formerly common /matpar/
!
MODULE matpar
    IMPLICIT NONE

    INTEGER                               :: icase
    INTEGER                               :: nbloks
    INTEGER                               :: npblk
    INTEGER                               :: nlast
    INTEGER                               :: nblsym
    INTEGER                               :: npsym
    INTEGER                               :: nlsym
    INTEGER                               :: imat
    INTEGER                               :: icasx
    INTEGER                               :: nbbx
    INTEGER                               :: npbx
    INTEGER                               :: nlbx
    INTEGER                               :: nbbl
    INTEGER                               :: npbl
    INTEGER                               :: nlbl

END MODULE

!***********************************************************************
!
! formerly common /cmb/
!
MODULE cmb
    USE nec2dpar

    IMPLICIT NONE

    INTEGER, PARAMETER                    :: iresrv = maxmat**2

    COMPLEX*16, DIMENSION(iresrv)         :: cm

END MODULE

!***********************************************************************
!
! formerly common /DATA/
!
MODULE data
    USE nec2dpar

    IMPLICIT NONE

    REAL(NEC2REAL), DIMENSION(maxseg)     :: x
    REAL(NEC2REAL), DIMENSION(maxseg)     :: y
    REAL(NEC2REAL), DIMENSION(maxseg)     :: z
    REAL(NEC2REAL), DIMENSION(maxseg)     :: si
    REAL(NEC2REAL), DIMENSION(maxseg)     :: bi
    REAL(NEC2REAL), DIMENSION(maxseg)     :: alp
    REAL(NEC2REAL), DIMENSION(maxseg)     :: bet
    REAL(NEC2REAL)                        :: wlam

    INTEGER, DIMENSION(2*maxseg)          :: icon1
    INTEGER, DIMENSION(2*maxseg)          :: icon2
    INTEGER, DIMENSION(2*maxseg)          :: itag
    INTEGER, DIMENSION(maxseg)            :: iconx
    INTEGER                               :: ld
    INTEGER                               :: n1
    INTEGER                               :: n2
    INTEGER                               :: n
    INTEGER                               :: np
    INTEGER                               :: m1
    INTEGER                               :: m2
    INTEGER                               :: m
    INTEGER                               :: mp
    INTEGER                               :: ipsym

END MODULE

!***********************************************************************
!
! formerly common /SAVE/
!
MODULE save
    USE nec2dpar

    IMPLICIT NONE

    REAL(NEC2REAL)                        :: epsr
    REAL(NEC2REAL)                        :: sig
    REAL(NEC2REAL)                        :: scrwlt
    REAL(NEC2REAL)                        :: scrwrt
    REAL(NEC2REAL)                        :: fmhz
    INTEGER, DIMENSION(2*maxseg)          :: ip
    INTEGER                               :: kcom

END MODULE

!***********************************************************************
!
! formerly common /csave/
!
MODULE csave
    USE nec2dpar

    IMPLICIT NONE

    REAL(NEC2REAL), DIMENSION(19,5)       :: com

END MODULE

!***********************************************************************
!
! formerly common /gnd/
!
MODULE gnd
    USE nec2dpar

    IMPLICIT NONE

    COMPLEX*16                            :: zrati
    COMPLEX*16                            :: zrati2
    COMPLEX*16                            :: frati
    COMPLEX*16                            :: t1
    REAL(NEC2REAL)                        :: t2
    REAL(NEC2REAL)                        :: cl
    REAL(NEC2REAL)                        :: ch
    REAL(NEC2REAL)                        :: scrwl
    REAL(NEC2REAL)                        :: scrwr
    INTEGER                               :: nradl
    INTEGER                               :: ksymp
    INTEGER                               :: ifar
    INTEGER                               :: iperf

END MODULE

!***********************************************************************
!
! formerly common /crnt/
!
MODULE crnt
    USE nec2dpar

    IMPLICIT NONE

    REAL(NEC2REAL), DIMENSION(maxseg)     :: air
    REAL(NEC2REAL), DIMENSION(maxseg)     :: aii
    REAL(NEC2REAL), DIMENSION(maxseg)     :: bir
    REAL(NEC2REAL), DIMENSION(maxseg)     :: bii
    REAL(NEC2REAL), DIMENSION(maxseg)     :: cir
    REAL(NEC2REAL), DIMENSION(maxseg)     :: cii
    COMPLEX*16,     DIMENSION(3*maxseg)   :: cur

END MODULE

!***********************************************************************
!
! formerly common /angl/
!
MODULE angl
    USE nec2dpar

    IMPLICIT NONE

    REAL(NEC2REAL), DIMENSION(maxseg)     :: salp

END MODULE

!***********************************************************************
!
! formerly common /zload/
!
MODULE zload
    USE nec2dpar

    IMPLICIT NONE

    COMPLEX*16, DIMENSION(maxseg)         :: zarray
    INTEGER                               :: nload
    INTEGER                               :: nlodf

END MODULE

!***********************************************************************
!
! formerly common /gwav/
!
MODULE gwav
    USE nec2dpar

    IMPLICIT NONE

    COMPLEX*16                            :: u
    COMPLEX*16                            :: u2
    COMPLEX*16                            :: xx1
    COMPLEX*16                            :: xx2
    REAL(NEC2REAL)                        :: r1
    REAL(NEC2REAL)                        :: r2
    REAL(NEC2REAL)                        :: zmh
    REAL(NEC2REAL)                        :: zph

END MODULE

!***********************************************************************
!
! formerly common /yparm/
!
MODULE yparm
    USE nec2dpar

    IMPLICIT NONE

    COMPLEX*16, DIMENSION(5)              :: y11a
    COMPLEX*16, DIMENSION(20)             :: y12a
    INTEGER                               :: ncoup
    INTEGER                               :: icoup
    INTEGER,    DIMENSION(5)              :: nctag
    INTEGER,    DIMENSION(5)              :: ncseg

END MODULE

!***********************************************************************
!
! formerly common /segj/
!
MODULE segj
    USE nec2dpar

    IMPLICIT NONE

    REAL(NEC2REAL), DIMENSION(jmax)       :: ax
    REAL(NEC2REAL), DIMENSION(jmax)       :: bx
    REAL(NEC2REAL), DIMENSION(jmax)       :: cx
    INTEGER,        DIMENSION(jmax)       :: jco
    INTEGER                               :: jsno
    INTEGER,        DIMENSION(50)         :: iscon
    INTEGER                               :: nscon
    INTEGER,        DIMENSION(10)         :: ipcon
    INTEGER                               :: npcon

END MODULE

!***********************************************************************
!
! formerly common /vsorc/
!
MODULE vsorc
    USE nec2dpar

    IMPLICIT NONE

    COMPLEX*16, DIMENSION(nsmax)        :: vqd
    COMPLEX*16, DIMENSION(nsmax)        :: vsant
    COMPLEX*16, DIMENSION(nsmax)        :: vqds
    INTEGER,    DIMENSION(nsmax)        :: ivqd
    INTEGER,    DIMENSION(nsmax)        :: isant
    INTEGER,    DIMENSION(nsmax)        :: iqds
    INTEGER                             :: nvqd
    INTEGER                             :: nsant
    INTEGER                             :: nqds

END MODULE

!***********************************************************************
!
! formerly common /netcx/
!
MODULE netcx
    USE nec2dpar

    IMPLICIT NONE

    COMPLEX*16                          :: zped
    REAL(NEC2REAL)                      :: pin
    REAL(NEC2REAL)                      :: pnls
    REAL(NEC2REAL), DIMENSION(netmx)    :: x11r(netmx)
    REAL(NEC2REAL), DIMENSION(netmx)    :: x11i(netmx)
    REAL(NEC2REAL), DIMENSION(netmx)    :: x12r(netmx)
    REAL(NEC2REAL), DIMENSION(netmx)    :: x12i(netmx)
    REAL(NEC2REAL), DIMENSION(netmx)    :: x22r(netmx)
    REAL(NEC2REAL), DIMENSION(netmx)    :: x22i(netmx)
    INTEGER,        DIMENSION(netmx)    :: ntyp(netmx)
    INTEGER,        DIMENSION(netmx)    :: iseg1(netmx)
    INTEGER,        DIMENSION(netmx)    :: iseg2(netmx)
    INTEGER                             :: neq
    INTEGER                             :: npeq
    INTEGER                             :: neq2
    INTEGER                             :: nonet
    INTEGER                             :: ntsol
    INTEGER                             :: nprint
    INTEGER                             :: masym

END MODULE

!***********************************************************************
!
! formerly common /fpat/
!
MODULE fpat
    USE nec2dpar

    IMPLICIT NONE

    REAL(NEC2REAL)                      :: thets
    REAL(NEC2REAL)                      :: phis
    REAL(NEC2REAL)                      :: dth
    REAL(NEC2REAL)                      :: dph
    REAL(NEC2REAL)                      :: rfld
    REAL(NEC2REAL)                      :: gnor
    REAL(NEC2REAL)                      :: clt
    REAL(NEC2REAL)                      :: cht
    REAL(NEC2REAL)                      :: epsr2
    REAL(NEC2REAL)                      :: sig2
    REAL(NEC2REAL)                      :: xpr6
    REAL(NEC2REAL)                      :: pinr
    REAL(NEC2REAL)                      :: pnlr
    REAL(NEC2REAL)                      :: ploss
    REAL(NEC2REAL)                      :: xnr
    REAL(NEC2REAL)                      :: ynr
    REAL(NEC2REAL)                      :: znr
    REAL(NEC2REAL)                      :: dxnr
    REAL(NEC2REAL)                      :: dynr
    REAL(NEC2REAL)                      :: dznr
    INTEGER                             :: nth
    INTEGER                             :: nph
    INTEGER                             :: ipd
    INTEGER                             :: iavp
    INTEGER                             :: inor
    INTEGER                             :: iax
    INTEGER                             :: ixtyp
    INTEGER                             :: near
    INTEGER                             :: nfeh
    INTEGER                             :: nrx
    INTEGER                             :: nry
    INTEGER                             :: nrz

END MODULE

!***********************************************************************
PROGRAM nec2dxs
!    av00    01-mar-02    First compile with Gnu77 compiler for windows

! Code converted using "TO_F90 by Alan Miller" 2013-11-02 - hwh

!                         (Thanks to Raymond Anderson for letting me know
!                         about this compiler and doing initial compilations)
!    av01    14-mar-02    Var PI not USEd in routine GWAVE
!    av02    14-mar-02    Sub SECOND already intrinsic function
!    av03    15-mar-02    Multiple changes to include SOMNEC routines in nec2dx.exe
!    av04    16-mar-02    Status='NEW', somehow seems not to replace existing file.
!    av05    21-okt-02    Max number of loads (LOADMX) made equal to max-nr of segments.
!    av06    21-okt-02    Max number of NT cards (NETMX) increased from 30 to 99
!    av07    21-okt-02    Max number of EX cards (NSMAX) increased from 30 to 99
!    av08    22-oct-02    Use of VSRC is uncertain, in some sources equal 10 and some
!                         equal 30 (=nr EX?). What should be new value ???
!    av09    ??           ??
!    av010   30-jan-03    Used DGJJ port of G77 compiler which delivers speed increase
!                         from 30 to 60% for small segment counts
!    av011   04-sep-03    Logging of NetMX, NSMAX changed
!    av012   29-sep-03    Enable USEr-specified NGF file-name.
!    av013   29-sep-03    MinGW port USEd for both 11K segs and virtual memory usage.
!    av014   09-oct-03    Max number of segs at junction/single-seg (JMAX) increased from 30 to 60
!    av015   05-nov-04    BugFix: Use default NGF name when nothing specified.
!    av016   09-nov-06    Official Nec2 bugfix by J.Burke, see nec-list at robomod.net
!    av017   30-jan-08    VSRC (30) var also increase to netmx, see also av08
!    av018   10-oct-08    av015 did not work properly in all cases.
!
!     History:
!        Date      Change
!      -------     ----------------------------------------------
!      5/04/95     Matrix re-transposed in subroutine FACTR.
!                  FACTR and SOLVE changed for non-transposed matrix.
!
!     PROGRAM NEC(INPUT,TAPE5=INPUT,OUTPUT,TAPE11,TAPE12,TAPE13,TAPE14,
!    1TAPE15,TAPE16,TAPE20,TAPE21)
!
!     NUMERICAL ELECTROMAGNETICS CODE (NEC2)  DEVELOPED AT LAWRENCE
!     LIVERMORE LAB., LIVERMORE, CA.  (CONTACT G. BURKE AT 510-422-8414
!     FOR PROBLEMS WITH THE NEC CODE.)
!     FILE CREATED 4/11/80.
!
!                ***********NOTICE**********
!     THIS COMPUTER CODE MATERIAL WAS PREPARED AS AN ACCOUNT OF WORK
!     SPONSORED BY THE UNITED STATES GOVERNMENT.  NEITHER THE UNITED
!     STATES NOR THE UNITED STATES DEPARTMENT OF ENERGY, NOR ANY OF
!     THEIR EMPLOYEES, NOR ANY OF THEIR CONTRACTORS, SUBCONTRACTORS, OR
!     THEIR EMPLOYEES, MAKES ANY WARRANTY, EXPRESS OR IMPLIED, OR
!     ASSUMES ANY LEGAL LIABILITY OR RESPONSIBILITY FOR THE ACCURACY,
!     COMPLETENESS OR USEFULNESS OF ANY INFORMATION, APPARATUS, PRODUCT
!     OR PROCESS DISCLOSED, OR REPRESENTS THAT ITS USE WOULD NOT
!     INFRINGE PRIVATELY-OWNED RIGHTS.
!
!     DOUBLE PRECISION 6/4/85
!
    USE nec2dpar
    USE somset
    USE plot
    USE matpar
    USE cmb
    USE data
    USE save
    USE csave
    USE gnd
    USE crnt
    USE zload
    USE gwav
    USE yparm
    USE segj
    USE vsorc
    USE netcx
    USE fpat

    IMPLICIT REAL(NEC2REAL)(a-h,o-z)

    CHARACTER (LEN=80) :: outfile
    CHARACTER (LEN=80) :: infile
    CHARACTER (LEN=2), DIMENSION(22), PARAMETER :: atst &
        = (/'CE','FR','LD','GN','EX','NT','XQ','NE','GD','RP','CM',  &
        'NX','EN','TL','PT','KH','NH','PQ','EK','WG','CP','PL'/)
    CHARACTER (LEN=2) :: ain

    CHARACTER (LEN=8), DIMENSION(3), PARAMETER :: pnet = (/'        ','STRAIGHT','CROSSED '/)
    CHARACTER (LEN=6), DIMENSION(3), PARAMETER :: hpol = (/'LINEAR','RIGHT ','LEFT  '/)

    REAL :: starttime, endtime, elapsed
    REAL :: tim, tim1, tim2
    REAL(NEC2REAL) tmp1

    REAL(NEC2REAL)                      :: ta = 1.745329252D-02
    REAL(NEC2REAL)                      :: cvel = 299.8
    INTEGER                             :: normf = 20

    INTEGER*2                           :: llneg

    COMPLEX*16 fj,eth,eph,curi
    COMPLEX*16  ex,ey,ez,epsc









    DIMENSION cab(1),sab(1),x2(1),y2(1),z2(1)

    DIMENSION ldtyp(loadmx),ldtag(loadmx),ldtagf(loadmx),  &
        ldtagt(loadmx),zlr(loadmx),zli(loadmx),zlc(loadmx)

    DIMENSION ix(2*maxseg)
    DIMENSION fnorm(200)
    DIMENSION t1x(1),t1y(1),t1z(1),t2x(1),t2y(1),t2z(1)

    DIMENSION xtemp(maxseg),ytemp(maxseg),ztemp(maxseg),sitemp(maxseg),bitemp(maxseg)

    EQUIVALENCE (cab,alp),(sab,bet),(x2,si),(y2,alp),(z2,bet)
    EQUIVALENCE (t1x,si),(t1y,alp),(t1z,bet),(t2x,icon1),(t2y,icon2), (t2z,itag)


        CALL show_program_info()
        CALL get_filenames(infile, outfile)

        CALL second(starttime)

        fj=(0.,1.)
        ld=maxseg

1       kcom=0
        ifrtimw=0
        ifrtimp=0
2       kcom=kcom+1
        IF (kcom > 5) kcom=5
        READ(2,125) ain,(com(i,kcom),i=1,19)

        IF (debugging) THEN
            WRITE (*,7193) ain, ain
7193        FORMAT (' card read: ain=',a2,' com=',a)
        END IF

        CALL upcase(ain,ain,lain)
        IF (kcom <= 1) THEN
            WRITE(3,126)
            WRITE(3,127)
            WRITE(3,128)
        END IF
        WRITE(3,129) (com(i,kcom),i=1,19)
        IF (ain == atst(11)) GO TO 2        ! CM
        IF (ain == atst(1)) GO TO 4         ! CE
        WRITE(3,130)
        STOP

4       CONTINUE
        DO  i=1,ld
          zarray(i)=(0.,0.)
        END DO
        mpcnt=0
        imat=0
        !
        !     SET UP GEOMETRY DATA IN SUBROUTINE DATAGN
        !
        if (debugging) print *, 'calling datagn'  
        CALL datagn
        if (debugging) print *, 'returned from datagn'

        iflow=1
        IF(imat == 0)GO TO 326
        !
        !     CORE ALLOCATION FOR ARRAYS B, C, AND D FOR N.G.F. SOLUTION
        !
        neq=n1+2*m1
        neq2=n-n1+2*(m-m1)+nscon+2*npcon
        CALL fbngf(neq,neq2,iresrv,ib11,ic11,id11,ix11)
        GO TO 6

326     neq=n+2*m
        neq2=0
        ib11=1
        ic11=1
        id11=1
        ix11=1
        icasx=0
6       npeq=np+2*mp
        WRITE(3,135)
        !
        !     DEFAULT VALUES FOR INPUT PARAMETERS AND FLAGS
        !
        iplp1=0
        iplp2=0
        iplp3=0
        iplp4=0
        igo=1
        fmhzs=cvel
        nfrq=1
        rkh=1.
        iexk=0
        ixtyp=0
        nload=0
        nonet=0
        near=-1
        iptflg=-2
        iptflq=-1
        ifar=-1
        zrati=(1.,0.)
        iped=0
        irngf=0
        ncoup=0
        icoup=0
        ! Default = No freq-loop/Neg-sigma
        llneg = 0

        IF(icasx > 0)GO TO 14
        fmhz=cvel
        nlodf=0
        ksymp=1
        nradl=0
        iperf=0
        !
        !     MAIN INPUT SECTION - STANDARD READ STATEMENT - JUMPS TO APPRO-
        !     PRIATE SECTION FOR SPECIFIC PARAMETER SET UP
        !
14      CALL readmn(2,ain,itmp1,itmp2,itmp3,itmp4,tmp1,tmp2,tmp3,tmp4,tmp5,tmp6)

        mpcnt=mpcnt+1

        WRITE(3,137) mpcnt,ain,itmp1,itmp2,itmp3,itmp4,tmp1,tmp2,tmp3, tmp4,tmp5,tmp6

        IF (ain == atst(2)) GO TO 16   ! FR
        IF (ain == atst(3)) GO TO 17   ! LD
        IF (ain == atst(4)) GO TO 21   ! GN
        IF (ain == atst(5)) GO TO 24   ! EX
        IF (ain == atst(6)) GO TO 28   ! NT
        IF (ain == atst(14)) GO TO 28  ! TL
        IF (ain == atst(15)) GO TO 31  ! PT
        IF (ain == atst(18)) GO TO 319 ! PQ
        IF (ain == atst(7)) GO TO 37   ! XQ
        IF (ain == atst(8)) GO TO 32   ! NE
        IF (ain == atst(17)) GO TO 208 ! NH
        IF (ain == atst(9)) GO TO 34   ! GD
        IF (ain == atst(10)) GO TO 36  ! RP
        IF (ain == atst(16)) GO TO 305 ! KH
        IF (ain == atst(19)) GO TO 320 ! EK
        IF (ain == atst(12)) GO TO 1   ! NX
        IF (ain == atst(20)) GO TO 322 ! WG
        IF (ain == atst(21)) GO TO 304 ! CP
        !***
        IF (ain == atst(22)) GO TO 330 ! PL ???
        !***
        IF (ain /= atst(13)) GO TO 15   ! EN

        CALL second(endtime)
        elapsed=endtime-starttime
        WRITE(3,201) elapsed
        STOP

15      WRITE(3,138)
        STOP

        !
        !     FREQUENCY PARAMETERS
        !
16      ifrq=itmp1
        IF(icasx == 0)GO TO 8
        WRITE(3,303) ain
        STOP

8       nfrq=itmp2
        IF (nfrq == 0) nfrq=1
        fmhz=tmp1
        delfrq=tmp2
        IF(iped == 1)zpnorm=0.
        igo=1
        iflow=1
        GO TO 14
        !
        !     MATRIX INTEGRATION LIMIT
        !
305     rkh=tmp1
        IF(igo > 2)igo=2
        iflow=1
        GO TO 14
        !
        !     EXTENDED THIN WIRE KERNEL OPTION
        !
320     iexk=1
        IF(itmp1 == -1)iexk=0
        IF(igo > 2)igo=2
        iflow=1
        GO TO 14
        !
        !     MAXIMUM COUPLING BETWEEN ANTENNAS
        !
304     IF(iflow /= 2)ncoup=0
        icoup=0
        iflow=2
        IF(itmp2 == 0)GO TO 14
        ncoup=ncoup+1
        IF(ncoup > 5)GO TO 312
        nctag(ncoup)=itmp1
        ncseg(ncoup)=itmp2
        IF(itmp4 == 0)GO TO 14
        ncoup=ncoup+1
        IF(ncoup > 5)GO TO 312
        nctag(ncoup)=itmp3
        ncseg(ncoup)=itmp4
        GO TO 14

312     WRITE(3,313)
        STOP
        !
        !     LOADING PARAMETERS
        !
17      IF (iflow == 3) GO TO 18
        nload=0
        iflow=3
        IF (igo > 2) igo=2
        IF (itmp1 == (-1)) GO TO 14
18      nload=nload+1
        IF (nload <= loadmx) GO TO 19
        WRITE(3,139)
        STOP

19      ldtyp(nload)=itmp1
        ldtag(nload)=itmp2
        IF (itmp4 == 0) itmp4=itmp3
        ldtagf(nload)=itmp3
        ldtagt(nload)=itmp4
        IF (itmp4 >= itmp3) GO TO 20
        WRITE(3,140)  nload,itmp3,itmp4
        STOP

20      zlr(nload)=tmp1
        zli(nload)=tmp2
        zlc(nload)=tmp3
        GO TO 14
        !
        !     GROUND PARAMETERS UNDER THE ANTENNA
        !
21      iflow=4
        IF(icasx == 0)GO TO 10
        WRITE(3,303) ain
        STOP

10      IF (igo > 2) igo=2
        IF (itmp1 /= (-1)) GO TO 22
        ksymp=1
        nradl=0
        iperf=0
        GO TO 14

22      iperf=itmp1
        nradl=itmp2
        ksymp=2
        epsr=tmp1
        sig=tmp2
        IF (nradl == 0) GO TO 23
        IF(iperf /= 2)GO TO 314
        WRITE(3,390)
        STOP

314     scrwlt=tmp3
        scrwrt=tmp4
        GO TO 14

23      epsr2=tmp3
        sig2=tmp4
        clt=tmp5
        cht=tmp6
        GO TO 14
        !
        !     EXCITATION PARAMETERS
        !
24      IF (iflow == 5) GO TO 25
        nsant=0
        nvqd=0
        iped=0
        iflow=5
        IF (igo > 3) igo=3
25      masym=itmp4/10
        IF (itmp1 > 0.AND.itmp1 /= 5) GO TO 27
        ixtyp=itmp1
        ntsol=0
        IF(ixtyp == 0)GO TO 205
        nvqd=nvqd+1
        IF(nvqd > nsmax)GO TO 206
        ivqd(nvqd)=isegno(itmp2,itmp3)
        vqd(nvqd)=DCMPLX(tmp1,tmp2)
        IF(ABS(vqd(nvqd)) < 1.d-20)vqd(nvqd)=(1.,0.)
        GO TO 207

205     nsant=nsant+1
        IF (nsant <= nsmax) GO TO 26
206     WRITE(3,141)
        STOP

26      isant(nsant)=isegno(itmp2,itmp3)
        vsant(nsant)=DCMPLX(tmp1,tmp2)
        IF (ABS(vsant(nsant)) < 1.d-20) vsant(nsant)=(1.,0.)
207     iped=itmp4-masym*10
        zpnorm=tmp3
        IF (iped == 1.AND.zpnorm > 0) iped=2
        GO TO 14

27      IF (ixtyp == 0.OR.ixtyp == 5) ntsol=0
        ixtyp=itmp1
        nthi=itmp2
        nphi=itmp3
        xpr1=tmp1
        xpr2=tmp2
        xpr3=tmp3
        xpr4=tmp4
        xpr5=tmp5
        xpr6=tmp6
        nsant=0
        nvqd=0
        thetis=xpr1
        phiss=xpr2
        GO TO 14
        !
        !     NETWORK PARAMETERS
        !
28      IF (iflow == 6) GO TO 29
        nonet=0
        ntsol=0
        iflow=6
        IF (igo > 3) igo=3
        IF (itmp2 == (-1)) GO TO 14

29      nonet=nonet+1
        IF (nonet <= netmx) GO TO 30
        WRITE(3,142)
        STOP

30      ntyp(nonet)=2
        IF (ain == atst(6)) ntyp(nonet)=1
        iseg1(nonet)=isegno(itmp1,itmp2)
        iseg2(nonet)=isegno(itmp3,itmp4)
        x11r(nonet)=tmp1
        x11i(nonet)=tmp2
        x12r(nonet)=tmp3
        x12i(nonet)=tmp4
        x22r(nonet)=tmp5
        x22i(nonet)=tmp6
        IF (ntyp(nonet) == 1.OR.tmp1 > 0.) GO TO 14
        ntyp(nonet)=3
        x11r(nonet)=-tmp1
        GO TO 14
        !
        !     PLOT FLAGS
        !
330     iplp1=itmp1
        iplp2=itmp2
        iplp3=itmp3
        iplp4=itmp4
        OPEN (UNIT=8,FILE='PLTDAT.NEC',STATUS='UNKNOWN',ERR=14)
        GO TO 14
        !
        !     PRINT CONTROL FOR CURRENT
        !
31      iptflg=itmp1
        iptag=itmp2
        iptagf=itmp3
        iptagt=itmp4
        IF(itmp3 == 0.AND.iptflg /= -1)iptflg=-2
        IF (itmp4 == 0) iptagt=iptagf
        GO TO 14
        !
        !     WRITE CONTROL FOR CHARGE
        !
319     iptflq=itmp1
        iptaq=itmp2
        iptaqf=itmp3
        iptaqt=itmp4
        IF(itmp3 == 0.AND.iptflq /= -1)iptflq=-2
        IF(itmp4 == 0)iptaqt=iptaqf
        GO TO 14
        !
        !     NEAR FIELD CALCULATION PARAMETERS
        !
208     nfeh=1
        GO TO 209

32      nfeh=0
209     IF (.NOT.(iflow == 8.AND.nfrq /= 1)) GO TO 33
        WRITE(3,143)
33      near=itmp1
        nrx=itmp2
        nry=itmp3
        nrz=itmp4
        xnr=tmp1
        ynr=tmp2
        znr=tmp3
        dxnr=tmp4
        dynr=tmp5
        dznr=tmp6
        iflow=8
        IF (nfrq /= 1) GO TO 14
        SELECT CASE ( igo )
          CASE (    1)
            GO TO 41
          CASE (    2)
            GO TO 46
          CASE (    3)
            GO TO 53
          CASE (    4)
            GO TO 71
          CASE (    5)
            GO TO 72
        END SELECT
        !
        !     GROUND REPRESENTATION
        !
34      epsr2=tmp1
        sig2=tmp2
        clt=tmp3
        cht=tmp4
        iflow=9
        GO TO 14
        !
        !     STANDARD OBSERVATION ANGLE PARAMETERS
        !
36      ifar=itmp1
        nth=itmp2
        nph=itmp3
        IF (nth == 0) nth=1
        IF (nph == 0) nph=1
        ipd=itmp4/10
        iavp=itmp4-ipd*10
        inor=ipd/10
        ipd=ipd-inor*10
        iax=inor/10
        inor=inor-iax*10
        IF (iax /= 0) iax=1
        IF (ipd /= 0) ipd=1
        IF (nth < 2.OR.nph < 2) iavp=0
        IF (ifar == 1) iavp=0
        thets=tmp1
        phis=tmp2
        dth=tmp3
        dph=tmp4
        rfld=tmp5
        gnor=tmp6
        iflow=10
        SELECT CASE ( igo )
          CASE (    1)
            GO TO 41
          CASE (    2)
            GO TO 46
          CASE (    3)
            GO TO 53
          CASE (    4)
            GO TO 71
          CASE (    5)
            GO TO 78
        END SELECT
        !
        !     WRITE NUMERICAL GREEN'S FUNCTION TAPE
        !
322     iflow=12
        IF(icasx == 0)GO TO 301
        WRITE(3,302)
        STOP
301     irngf=iresrv/2
        SELECT CASE ( igo )
          CASE (    1)
            GO TO 41
          CASE (    2)
            GO TO 46
          CASE (    3)
            GO TO 52
          CASE (    4)
            GO TO 52
          CASE (    5)
            GO TO 52
        END SELECT
        !
        !     EXECUTE CARD  -  CALC. INCLUDING RADIATED FIELDS
        !
37      IF (iflow == 10.AND.itmp1 == 0) GO TO 14
        IF (nfrq == 1.AND.itmp1 == 0.AND.iflow > 7) GO TO 14
        IF (itmp1 /= 0) GO TO 39
        IF (iflow > 7) GO TO 38
        iflow=7
        GO TO 40

38      iflow=11
        GO TO 40

39      ifar=0
        rfld=0.
        ipd=0
        iavp=0
        inor=0
        iax=0
        nth=91
        nph=1
        thets=0.
        phis=0.
        dth=1.0
        dph=0.
        IF (itmp1 == 2) phis=90.
        IF (itmp1 /= 3) GO TO 40
        nph=2
        dph=90.
40      SELECT CASE ( igo )
          CASE (    1)
            GO TO 41
          CASE (    2)
            GO TO 46
          CASE (    3)
            GO TO 53
          CASE (    4)
            GO TO 71
          CASE (    5)
            GO TO 78
        END SELECT
        !
        !     END OF THE MAIN INPUT SECTION
        !
        !     BEGINNING OF THE FREQUENCY DO LOOP
        !
41      mhz=1
        !***
        IF(n == 0 .OR. ifrtimw == 1)GO TO 406
        ifrtimw=1
        DO  i=1,n
          xtemp(i)=x(i)
          ytemp(i)=y(i)
          ztemp(i)=z(i)
          sitemp(i)=si(i)
          bitemp(i)=bi(i)
        END DO

406     IF(m == 0 .OR. ifrtimp == 1)GO TO 407
        ifrtimp=1
        j=ld+1
        DO  i=1,m
          j=j-1
          xtemp(j)=x(j)
          ytemp(j)=y(j)
          ztemp(j)=z(j)
          bitemp(j)=bi(j)
        END DO
407     CONTINUE
        fmhz1=fmhz
        !     CORE ALLOCATION FOR PRIMARY INTERACTON MATRIX.  (A)
        IF(imat == 0)CALL fblock(npeq,neq,iresrv,irngf,ipsym)
42      IF (mhz == 1) GO TO 44
        IF (ifrq == 1) GO TO 43
        !      FMHZ=FMHZ+DELFRQ
        fmhz=fmhz1+(mhz-1)*delfrq
        GO TO 44

43      fmhz=fmhz*delfrq
44      fr=fmhz/cvel
        wlam=cvel/fmhz             ! wavl=299.8/freq
        WRITE(3,145)  fmhz,wlam
        WRITE(3,196) rkh
        IF(iexk == 1)WRITE(3,321)
        !     FREQUENCY SCALING OF GEOMETRIC PARAMETERS
        !***      FMHZS=FMHZ
        IF(n == 0)GO TO 306
        DO  i=1,n
        !***
          x(i)=xtemp(i)*fr
          y(i)=ytemp(i)*fr
          z(i)=ztemp(i)*fr
          si(i)=sitemp(i)*fr
          bi(i)=bitemp(i)*fr
        END DO
        !***
306     IF(m == 0)GO TO 307
        fr2=fr*fr
        j=ld+1
        DO  i=1,m
          j=j-1
        !***
          x(j)=xtemp(j)*fr
          y(j)=ytemp(j)*fr
          z(j)=ztemp(j)*fr
          bi(j)=bitemp(j)*fr2
        END DO
        !***
307     igo=2

        !     STRUCTURE SEGMENT LOADING

46      WRITE(3,146)
        IF(nload /= 0) CALL load(ldtyp,ldtag,ldtagf,ldtagt,zlr,zli,zlc)

        IF(nload == 0.AND.nlodf == 0)WRITE(3,147)
        IF(nload == 0.AND.nlodf /= 0)WRITE(3,327)

        !     GROUND PARAMETER

        WRITE(3,148)            ! Antenna environment
        IF (ksymp == 1) GO TO 49
        frati=(1.,0.)
        IF (iperf == 1) GO TO 48

        IF (sig < 0.) THEN        ! Negative sigma ?
          llneg = 1                 ! Set flag
          sig=-sig/(59.96*wlam)   ! Make positive
        END IF

        epsc=DCMPLX(epsr,-sig*wlam*59.96)
        zrati=1./SQRT(epsc)
        u=zrati
        u2=u*u
        IF (nradl == 0) GO TO 47
        scrwl=scrwlt/wlam
        scrwr=scrwrt/wlam
        t1=fj*2367.067D+0/dfloat(nradl)
        t2=scrwr*dfloat(nradl)
        WRITE(3,170)  nradl,scrwlt,scrwrt
        WRITE(3,149)
47      IF(iperf == 2)GO TO 328        ! Somnec ground ?

        WRITE(3,391)                   ! Finite ground
        GO TO 329

        !******************************************************************************
        !    Include SomNec calculations
        !******************************************************************************

328     IF (llneg <= 1) THEN      ! Single or first step ?
          IF (llneg == 1) llneg=2   ! If negative, only once
          CALL som2d (fmhz,epsr,sig) ! Get SomNec data, av03
        END IF

        frati=(epsc-1.)/(epsc+1.)
        IF(ABS((epscf-epsc)/epsc) < 1.d-3)GO TO 400

        WRITE(3,393) epscf,epsc      ! Error in ground param's
        STOP

400     WRITE(3,392)         ! Sommerfeld ground
329     WRITE(3,150)  epsr,sig,epsc   ! Rel-diel-C, conduct, compl-diel-C
        GO TO 50

48      WRITE(3,151)   ! Perfect ground
        GO TO 50

49      WRITE(3,152)   ! Free space
50      CONTINUE
        ! * * *
        !     FILL AND FACTOR PRIMARY INTERACTION MATRIX
        !
        CALL second (tim1)
        IF(icasx /= 0)GO TO 324
        CALL cmset(neq,cm,rkh,iexk)
        CALL second (tim2)
        tim=tim2-tim1
        CALL factrs(npeq,neq,cm,ip,ix,11,12,13,14)
        GO TO 323
        !
        !     N.G.F. - FILL B, C, AND D AND FACTOR D-C(INV(A)B)
        !
324     IF(neq2 == 0)GO TO 333
        CALL cmngf(cm(ib11),cm(ic11),cm(id11),npbx,neq,neq2,rkh,iexk)
        CALL second (tim2)
        tim=tim2-tim1
        CALL facgf(cm,cm(ib11),cm(ic11),cm(id11),cm(ix11),ip,ix,np,n1,mp, m1,neq,neq2)
323     CALL second (tim1)
        tim2=tim1-tim2
        WRITE(3,153)  tim,tim2
333     igo=3
        ntsol=0
        IF(iflow /= 12)GO TO 53
        !     WRITE N.G.F. FILE
52      CALL gfout
        GO TO 14
        !
        !     EXCITATION SET UP (RIGHT HAND SIDE, -E INC.)
        !
53      nthic=1
        nphic=1
        inc=1
        nprint=0
54      IF (ixtyp == 0.OR.ixtyp == 5) GO TO 56
        IF (iptflg <= 0.OR.ixtyp == 4) WRITE(3,154)
        tmp5=ta*xpr5
        tmp4=ta*xpr4
        IF (ixtyp /= 4) GO TO 55
        tmp1=xpr1/wlam
        tmp2=xpr2/wlam
        tmp3=xpr3/wlam
        tmp6=xpr6/(wlam*wlam)
        WRITE(3,156)  xpr1,xpr2,xpr3,xpr4,xpr5,xpr6
        GO TO 56

55      tmp1=ta*xpr1
        tmp2=ta*xpr2
        tmp3=ta*xpr3
        tmp6=xpr6
        IF (iptflg <= 0) WRITE(3,155)  xpr1,xpr2,xpr3,trim(hpol(ixtyp)),xpr6
56      CALL etmns (tmp1,tmp2,tmp3,tmp4,tmp5,tmp6,ixtyp,cur)
        !
        !     MATRIX SOLVING  (NETWK CALLS SOLVES)
        !
        IF (nonet == 0.OR.inc > 1) GO TO 60
        WRITE(3,158)
        itmp3=0
        itmp1=ntyp(1)
        DO  i=1,2
          IF (itmp1 == 3) itmp1=2
          IF (itmp1 == 2) WRITE(3,159)
          IF (itmp1 == 1) WRITE(3,160)
          DO  j=1,nonet
            itmp2=ntyp(j)
            IF ((itmp2/itmp1) == 1) GO TO 57
            itmp3=itmp2
            CYCLE
            57    itmp4=iseg1(j)
            itmp5=iseg2(j)
            IF (itmp2 >= 2.AND.x11i(j) <= 0.) x11i(j)=wlam*SQRT((x(itmp5)-  &
                x(itmp4))**2+(y(itmp5)-y(itmp4))**2+(z(itmp5)-z(itmp4))**2)
            WRITE(3,157) itag(itmp4),itmp4,itag(itmp5),itmp5,x11r(j),x11i(j), &
                x12r(j),x12i(j),x22r(j),x22i(j),trim(pnet(itmp2))
          END DO
          IF (itmp3 == 0) EXIT
          itmp1=itmp3
        END DO
60      CONTINUE
        IF (inc > 1.AND.iptflg > 0) nprint=1
        CALL netwk(cm,cm(ib11),cm(ic11),cm(id11),ip,cur)
        ntsol=1
        IF (iped == 0) GO TO 61
        itmp1=mhz+4*(mhz-1)
        IF (itmp1 > (normf-3)) GO TO 61
        fnorm(itmp1)=dREAL(zped)
        fnorm(itmp1+1)=DIMAG(zped)
        fnorm(itmp1+2)=ABS(zped)
        fnorm(itmp1+3)=cang(zped)
        IF (iped == 2) GO TO 61
        IF (fnorm(itmp1+2) > zpnorm) zpnorm=fnorm(itmp1+2)
61      CONTINUE
        !
        !     PRINTING STRUCTURE CURRENTS
        !
        IF(n == 0)GO TO 308
        IF (iptflg == (-1)) GO TO 63
        IF (iptflg > 0) GO TO 62
        WRITE(3,161)
        WRITE(3,162)
        GO TO 63

62      IF (iptflg == 3.OR.inc > 1) GO TO 63
            WRITE(3,163)  xpr3,hpol(ixtyp),xpr6
63      ploss=0.
        itmp1=0
        jump=iptflg+1

        DO  i=1,n
          curi=cur(i)*wlam
          cmag=ABS(curi)
          ph=cang(curi)
          IF (nload == 0.AND.nlodf == 0) GO TO 64
          IF (ABS(dREAL(zarray(i))) < 1.d-20) GO TO 64
          ploss=ploss+.5*cmag*cmag*dREAL(zarray(i))*si(i)
64        IF (jump < 0) THEN
            GO TO    68
          ELSE IF (jump == 0) THEN
            CYCLE                 ! HWH was missing target of GO TO 69
          END IF
65        IF (iptag == 0) GO TO 66
          IF (itag(i) /= iptag) CYCLE
66        itmp1=itmp1+1
          IF (itmp1 < iptagf.OR.itmp1 > iptagt) CYCLE
          IF (iptflg == 0) GO TO 68
          IF (iptflg < 2.OR.inc > normf) GO TO 67
          fnorm(inc)=cmag
          isave=i
67        IF (iptflg /= 3) WRITE(3,164)  xpr1,xpr2,cmag,ph,i
          CYCLE

68        WRITE(3,165)  i,itag(i),x(i),y(i),z(i),si(i),curi,cmag,ph
          IF(iplp1 /= 1) CYCLE
          IF(iplp2 == 1) WRITE(8,*) curi
          IF(iplp2 == 2) WRITE(8,*) cmag,ph
        END DO

        IF(iptflq == (-1))GO TO 308
        WRITE(3,315)
        itmp1=0
        fr=1.d-6/fmhz
        DO  i=1,n
          IF(iptflq == (-2))GO TO 318
          IF(iptaq == 0)GO TO 317
          IF(itag(i) /= iptaq)CYCLE
317       itmp1=itmp1+1
          IF(itmp1 < iptaqf.OR.itmp1 > iptaqt)CYCLE
318       curi=fr*DCMPLX(-bii(i),bir(i))
          cmag=ABS(curi)
          ph=cang(curi)
          WRITE(3,165) i,itag(i),x(i),y(i),z(i),si(i),curi,cmag,ph
        END DO
308     IF(m == 0)GO TO 310
        WRITE(3,197)
        j=n-2
        itmp1=ld+1
        DO  i=1,m
          j=j+3
          itmp1=itmp1-1
          ex=cur(j)
          ey=cur(j+1)
          ez=cur(j+2)
          eth=ex*t1x(itmp1)+ey*t1y(itmp1)+ez*t1z(itmp1)
          eph=ex*t2x(itmp1)+ey*t2y(itmp1)+ez*t2z(itmp1)
          ethm=ABS(eth)
          etha=cang(eth)
          ephm=ABS(eph)
          epha=cang(eph)
          !309   WRITE(3,198) I,X(ITMP1),Y(ITMP1),Z(ITMP1),ETHM,ETHA,EPHM,EPHA,E
          !     1X,EY, EZ
          WRITE(3,198) i,x(itmp1),y(itmp1),z(itmp1),ethm,etha,ephm,epha,ex,ey,ez
          IF(iplp1 /= 1) CYCLE
          IF(iplp3 == 1) WRITE(8,*) ex
          IF(iplp3 == 2) WRITE(8,*) ey
          IF(iplp3 == 3) WRITE(8,*) ez
          IF(iplp3 == 4) WRITE(8,*) ex,ey,ez
        END DO
310     IF (ixtyp /= 0.AND.ixtyp /= 5) GO TO 70
        tmp1=pin-pnls-ploss
        tmp2=100.*tmp1/pin
        WRITE(3,166)  pin,tmp1,ploss,pnls,tmp2
70      CONTINUE
        igo=4
        IF(ncoup > 0)CALL couple(cur,wlam)
        IF (iflow /= 7) GO TO 71
        IF (ixtyp > 0.AND.ixtyp < 4) GO TO 113
        IF (nfrq /= 1) GO TO 120
        WRITE(3,135)
        GO TO 14
71      igo=5
        !
        !     NEAR FIELD CALCULATION
        !
72      IF (near == (-1)) GO TO 78
        CALL nfpat
        IF (mhz == nfrq) near=-1
        IF (nfrq /= 1) GO TO 78
        WRITE(3,135)
        GO TO 14
        !
        !     STANDARD FAR FIELD CALCULATION
        !
78      IF(ifar == -1)GO TO 113
        pinr=pin
        pnlr=pnls
        CALL rdpat
113     IF (ixtyp == 0.OR.ixtyp >= 4) GO TO 119
        nthic=nthic+1
        inc=inc+1
        xpr1=xpr1+xpr4
        IF (nthic <= nthi) GO TO 54
        nthic=1
        xpr1=thetis
        xpr2=xpr2+xpr5
        nphic=nphic+1
        IF (nphic <= nphi) GO TO 54
        nphic=1
        xpr2=phiss
        IF (iptflg < 2) GO TO 119
        !     NORMALIZED RECEIVING PATTERN PRINTED
        itmp1=nthi*nphi
        IF (itmp1 <= normf) GO TO 114
        itmp1=normf
        WRITE(3,181)
114     tmp1=fnorm(1)
        DO  j=2,itmp1
          IF (fnorm(j) > tmp1) tmp1=fnorm(j)
        END DO
        WRITE(3,182)  tmp1,xpr3,hpol(ixtyp),xpr6,isave
        DO  j=1,nphi
          itmp2=nthi*(j-1)
          DO  i=1,nthi
            itmp3=i+itmp2
            IF (itmp3 > itmp1) EXIT
            tmp2=fnorm(itmp3)/tmp1
            tmp3=db20(tmp2)
            WRITE(3,183)  xpr1,xpr2,tmp3,tmp2
            xpr1=xpr1+xpr4
          END DO
117       xpr1=thetis
          xpr2=xpr2+xpr5
        END DO
        xpr2=phiss
119     IF (mhz == nfrq) ifar=-1
        IF (nfrq /= 1) GO TO 120
        WRITE(3,135)
        GO TO 14

120     mhz=mhz+1
        IF (mhz <= nfrq) GO TO 42
        IF (iped == 0) GO TO 123
        IF(nvqd < 1)GO TO 199
        WRITE(3,184) ivqd(nvqd),zpnorm
        GO TO 204

199     WRITE(3,184)  isant(nsant),zpnorm
204     itmp1=nfrq
        IF (itmp1 <= (normf/4)) GO TO 121
        itmp1=normf/4
        WRITE(3,185)
121     IF (ifrq == 0) tmp1=fmhz-(nfrq-1)*delfrq
        IF (ifrq == 1) tmp1=fmhz/(delfrq**(nfrq-1))
        DO  i=1,itmp1
          itmp2=i+4*(i-1)
          tmp2=fnorm(itmp2)/zpnorm
          tmp3=fnorm(itmp2+1)/zpnorm
          tmp4=fnorm(itmp2+2)/zpnorm
          tmp5=fnorm(itmp2+3)
          WRITE(3,186)  tmp1,fnorm(itmp2),fnorm(itmp2+1),fnorm(itmp2+2),  &
              fnorm(itmp2+3),tmp2,tmp3,tmp4,tmp5
          IF (ifrq == 0) tmp1=tmp1+delfrq
          IF (ifrq == 1) tmp1=tmp1*delfrq
        END DO
        WRITE(3,135)
123     CONTINUE
        nfrq=1
        mhz=1
        GO TO 14


125   FORMAT (a2,19A4)
126   FORMAT  ('1')
127   FORMAT (///,33X,'*********************************************',  &
    //,36X,'NUMERICAL ELECTROMAGNETICS CODE (NEC-2D)',//,33X,  &
    '*********************************************')
128   FORMAT (////,37X,'- - - - COMMENTS - - - -',//)
129   FORMAT (25X,20A4)
130   FORMAT (///,10X,'INCORRECT LABEL FOR A COMMENT CARD')
135   FORMAT (/////)
136   FORMAT (a2,i3,3I5,6E10.3)
137   FORMAT (1X,'***** DATA CARD NO.',i3,3X,a2,1X,i3,3(1X,i5),  &
    6(1X,1P,e12.5))
138   FORMAT (///,10X,'FAULTY DATA CARD LABEL AFTER GEOMETRY SECTION')
139   FORMAT (///,10X,'NUMBER OF LOADING CARDS EXCEEDS STORAGE ALLOTTED' )
140   FORMAT (///,10X,'DATA FAULT ON LOADING CARD NO.=',i5,5X,  &
    'ITAG STEP1=',i5,'  IS GREATER THAN ITAG STEP2=',i5)
141   FORMAT (///,10X,'NUMBER OF EXCITATION CARDS EXCEEDS STORAGE ALLO',  &
    'TTED')
142   FORMAT (///,10X,'NUMBER OF NETWORK CARDS EXCEEDS STORAGE ALLOTTE', 'D')
143   FORMAT(///,10X,'WHEN MULTIPLE FREQUENCIES ARE REQUESTED, ONLY ON',  &
    'E NEAR FIELD CARD CAN BE USED -',/,10X,'LAST CARD READ IS USED')
145   FORMAT (////,33X,'- - - - - - FREQUENCY - - - - - -',//,36X,  &
    'FREQUENCY=',1P,e11.4,' MHZ',/,36X,'WAVELENGTH=',e11.4,' METERS')
146   FORMAT (///,30X,' - - - STRUCTURE IMPEDANCE LOADING - - -')
147   FORMAT (/ ,35X,'THIS STRUCTURE IS NOT LOADED')
148   FORMAT (///,34X,'- - - ANTENNA ENVIRONMENT - - -',/)
149   FORMAT (40X,'MEDIUM UNDER SCREEN -')
150   FORMAT (40X,'RELATIVE DIELECTRIC CONST.=',f7.3,/,40X,'CONDUCTIV',  &
    'ITY=',1P,e10.3,' MHOS/METER',/,40X,'COMPLEX DIELECTRIC CONSTANT=' ,2E12.5)
151   FORMAT (  42X,'PERFECT GROUND')
152   FORMAT (  44X,'FREE SPACE')
153   FORMAT (///,32X,'- - - MATRIX TIMING - - -',//,24X,'FILL=',f9.3,  &
    ' SEC.,  FACTOR=',f9.3,' SEC.')
154   FORMAT (///,40X,'- - - EXCITATION - - -')
155   FORMAT (/,4X,'PLANE WAVE',4X,'THETA=',f7.2,' DEG,  PHI=',f7.2,  &
    ' DEG,  ETA=',f7.2,' DEG,  TYPE -',a6,'=  AXIAL RATIO=',f6.3)
156   FORMAT (/,31X,'POSITION (METERS)',14X,'ORIENTATION (DEG)=',/,28X,  &
    'X',12X,'Y',12X,'Z',10X,'ALPHA',5X,'BETA',4X,'DIPOLE MOMENT',//  &
    ,4X,'CURRENT SOURCE',1X,3(3X,f10.5),1X,2(3X,f7.2),4X,f8.3)
157   FORMAT (4X,4(i5,1X),1P,6(3X,e11.4),3X,a8)
158   FORMAT (///,44X,'- - - NETWORK DATA - - -')
159   FORMAT (/,6X,'- FROM -    - TO -',11X,'TRANSMISSION LINE',15X,  &
    '-  -  SHUNT ADMITTANCES (MHOS)  -  -',14X,'LINE',/,6X,'TAG  SEG.'  &
,'   TAG  SEG.',6X,'IMPEDANCE',6X,'LENGTH',12X,'- END ONE -',17X,  &
'- END TWO -',12X,'TYPE',/,6X,'NO.   NO.   NO.   NO.',9X,'OHMS',  &
    8X,'METERS',9X,'REAL',10X,'IMAG.',9X,'REAL',10X,'IMAG.')
160   FORMAT (/,6X,'- FROM -',4X,'- TO -',26X,'-  -  ADMITTANCE MATRIX',  &
    ' ELEMENTS (MHOS)  -  -',/,6X,'TAG  SEG.   TAG  SEG.',13X,'(ON',  &
    'E,ONE)',19X,'(ONE,TWO)',19X,'(TWO,TWO)',/ ,6X,'NO.   NO.   NO',  &
    '.   NO.',8X,'REAL',10X,'IMAG.',9X,'REAL',10X,'IMAG.',9X,'REAL', 10X,'IMAG.')
161   FORMAT (///,29X,'- - - CURRENTS AND LOCATION - - -',//,33X,  &
    'DISTANCES IN WAVELENGTHS')
162   FORMAT (  //,2X,'SEG.',2X,'TAG',4X,'COORD. OF SEG. CENTER',5X,  &
    'SEG.',12X,'- - - CURRENT (AMPS) - - -',/,2X,'NO.',3X,'NO.',  &
    5X,'X',8X,'Y',8X,'Z',6X,'LENGTH',5X,'REAL',8X,'IMAG.',7X,'MAG.', 8X,'PHASE')
163   FORMAT (///,33X,'- - - RECEIVING PATTERN PARAMETERS - - -',/,43X,  &
    'ETA=',f7.2,' DEGREES',/,43X,'TYPE -',a6,/,43X,'AXIAL RATIO=',  &
    f6.3,//,11X,'THETA',6X,'PHI',10X,'-  CURRENT  -',9X,'SEG',/,  &
    11X,'(DEG)',5X,'(DEG)',7X,'MAGNITUDE',4X,'PHASE',6X,'NO.',/)
164   FORMAT (10X,2(f7.2,3X),1X,1P,e11.4,3X,0P,f7.2,4X,i5)
165   FORMAT (1X,2I5,3F9.4,f9.5,1X,1P,3E12.4,0P,f9.3)
166   FORMAT (///,40X,'- - - POWER BUDGET - - -',//,43X,'INPUT POWER   =  &
    ',1P,e11.4,' WATTS',/ ,43X,'RADIATED POWER=',e11.4,' WATTS',  &
    /,43X,'STRUCTURE LOSS=',e11.4,' WATTS',/,43X,'NETWORK LOSS  =',  &
    e11.4,' WATTS',/,43X,'EFFICIENCY    =',0P,f7.2,' PERCENT')
170   FORMAT (40X,'RADIAL WIRE GROUND SCREEN',/,40X,i5,' WIRES',/,40X,  &
    'WIRE LENGTH=',f8.2,' METERS',/,40X,'WIRE RADIUS=',1P,e10.3, ' METERS')
181   FORMAT (///,4X,'RECEIVING PATTERN STORAGE TOO SMALL,ARRAY TRUNCA',  &
    'TED')
182   FORMAT (///,32X,'- - - NORMALIZED RECEIVING PATTERN - - -',/,41X,  &
    'NORMALIZATION FACTOR=',1P,e11.4,/,41X,'ETA=',0P,f7.2,' DEGREES',  &
    /,41X,'TYPE -',a6,/,41X,'AXIAL RATIO=',f6.3,/,41X,'SEGMENT NO.=',  &
    i5,//,21X,'THETA',6X,'PHI',9X,'-  PATTERN  -',/,21X,'(DEG)',5X,  &
    '(DEG)',8X,'DB',8X,'MAGNITUDE',/)
183   FORMAT (20X,2(f7.2,3X),1X,f7.2,4X,1P,e11.4)
184   FORMAT (///,36X,'- - - INPUT impedance DATA - - -',/   ,45X, &
    'SOURCE SEGMENT NO.',i4,/  ,45X,'NORMALIZATION factor=',1P,e12.5,//,  &
    7X,'FREQ.',13X,'-  -  unnormalized impedance  -  -',21X, &
    '-  -  normalized impedance  -  -',/    ,19X,'RESISTANCE',4X, &
    'REACTANCE',6X,'MAGNITUDE',4X,'PHASE',7X,'RESISTANCE',4X,'REACTANCE',6X,  &
    'MAGNITUDE',4X,'PHASE',/    ,8X,'MHZ',11X,'OHMS',10X,'OHMS',11X,  &
    'OHMS',5X,'DEGREES',47X,'DEGREES',/)
185   FORMAT (///,4X,'STORAGE for impedance normalization too small, array truncated')
186   FORMAT (3X,f9.3,2X,1P,2(2X,e12.5),3X,e12.5,2X,0P,f7.2,2X,1P,2(2X,e12.5), &
    3X,e12.5,2X,0P,f7.2)
196   FORMAT(   ////,20X,'APPROXIMATE integration employed for segments more than', &
    f8.3,' wavelengths apart')
197   FORMAT(   ////,41X,'- - - - surface patch currents - - - -',//,  &
    50X,'DISTANCE in wavelengths',/,50X,'CURRENT in amps/meter',  &
    //,28X,'- - surface components - -',19X,'- - - rectangular components - - -',/, &
    6X,'PATCH center',6X,'TANGENT vector 1',3X,  &
    'TANGENT vector 2',11X,'X',19X,'Y',19X,'Z',/,5X,'X',6X,'Y',6X,  &
    'Z',5X,'MAG.',7X,'PHASE',3X,'MAG.',7X,'PHASE',3(4X,'REAL',6X, 'IMAG. '))
198   FORMAT(1X,i4,/,1X,3F7.3,2(1P,e11.4,0P,f8.2),1P,6E10.2)
201   FORMAT(/,' run time =',f10.3)
315   FORMAT(///,34X,'- - - charge densities - - -',//,36X,  &
    'DISTANCES in wavelengths',///,2X,'SEG.',2X,'TAG',4X,  &
    'COORD. of seg. center',5X,'SEG.',10X,  &
    'CHARGE density (coulombs/meter)',/,2X,'NO.',3X,'NO.',5X,'X',8X,  &
    'Y',8X,'Z',6X,'LENGTH',5X,'REAL',8X,'IMAG.',7X,'MAG.',8X,'PHASE')
321   FORMAT( /,20X,'THE extended thin wire kernel will be USEd')
303   FORMAT(/,' error - ',a2,' card is NOT allowed with n.g.f.')
327   FORMAT(/,35X,' loading only in n.g.f. section')
302   FORMAT(' error - n.g.f. in USE.  cannot WRITE NEW n.g.f.')
313   FORMAT(/,' NUMBER of segments in coupling calculation (cp) exceeds limit')
390   FORMAT(' radial wire g. s. approximation may NOT be USEd with sommerfeld ground option')
391   FORMAT(40X,'FINITE ground.  reflection coefficient approximation')
392   FORMAT(40X,'FINITE ground.  sommerfeld solution')
393   FORMAT(/,' error in ground PARAMETERs -',/, &
    ' COMPLEX dielectric constant from FILE is',1P,2E12.5,/,32X,'REQUESTED',2E12.5)
900   FORMAT(' ERROR OPENING SOMMERFELD GROUND FILE - SOM2D.NEC')

!--------------------------------------------------------------------------------

CONTAINS

SUBROUTINE show_program_info()
    USE nec2dpar

    PRINT *, ''
    PRINT *, 'Numerical Electromagnetics Code, '
    PRINT *, ''
    PRINT *, 'double precision version (nec2d)'
    PRINT *, ''
    PRINT *, 'developed at Lawrence Livermore Lab., Livermore, CA.'
    PRINT *, 'by G. Burke (burke@icdc.llnl.gov) and A. Poggio.'
    PRINT *, ''
    PRINT *, 'Fortran file was created 4/11/80'
    PRINT *, 'changed: Jan 15, 96, by J. Bergervoet (bergervo@prl.philips.nl)'

    PRINT *, ''
    PRINT *, 'Maximum number of segments in core : MAXMAT=',maxmat
    IF(maxseg /= maxmat) PRINT *, 'Maximum when using swap files      : MAXSEG=',maxseg

    PRINT *, ''
    PRINT *, 'Merged nec2d/som2d file created by Arie Voors. (4nec2@gmx.net)'
    PRINT *, ''
    PRINT *,'Build 2.7  30-jan-08  ','(maxLD=',loadmx,', MaxEX=',nsmax,', MaxTL=',netmx,')'
    PRINT *, ''
    PRINT *,'Using ',g77port        ! 'XX port for G77 version YY'
    PRINT *, ''

END SUBROUTINE show_program_info
!----------------------------------------------------------------------------

SUBROUTINE get_filenames(infile, outfile)
        CHARACTER(LEN=*), INTENT(OUT)          :: infile
        CHARACTER(LEN=*), INTENT(OUT)          :: outfile

706     CONTINUE
        WRITE(*,700)
        READ(*,701,ERR=706,END=708) infile
        OPEN (UNIT=2,FILE=infile,STATUS='OLD',ERR=702)

707     CONTINUE
        WRITE(*,703)
        READ(*,701,ERR=707,END=706) outfile
        OPEN (UNIT=3,FILE=outfile,STATUS='UNKNOWN',ERR=704)
        GO TO 705

702     PRINT *, 'Error opening input-file:',infile
        GO TO 706

704     PRINT *, 'Error opening output-file:',outfile
        GO TO 707

708     STOP

705     CONTINUE
        PRINT *,''

700     FORMAT(' ENTER NAME OF INPUT FILE >',$)
701     FORMAT(a)
703     FORMAT(' ENTER NAME OF OUTPUT FILE >',$)
END SUBROUTINE get_filenames

! ################## START OF SOM2D INCLUDE ########################

!----------------------------------------------------------------------------

SUBROUTINE som2d (rmhz, repr, rsig)
        USE somset

        IMPLICIT REAL(NEC2REAL)(a-h,o-z)

        REAL(NEC2REAL), INTENT(IN)                         :: rmhz
        REAL(NEC2REAL), INTENT(IN)                         :: repr
        REAL(NEC2REAL), INTENT(IN)                         :: rsig
        !***
        COMPLEX*16 ck1,ck1sq,erv,ezv,erh,eph,cksm,ct1,ct2,ct3,cl1,cl2,con
        COMMON /evlcom/ cksm,ct1,ct2,ct3,ck1,ck1sq,ck2,ck2sq,tkmag,tsmag,ck1r,zph,rho,jh

        CHARACTER (LEN=3), DIMENSION(4)  :: lcomp = (/'ERV','EZV','ERH','EPH'/)

        epr = repr
        sig = rsig
        fmhz = rmhz
        ipt=0            ! No printing

        !deb  write (*,100) fmhz,epr,sig
        !deb  100 format (' Som2d: Freq=',d10.5,' Diel=',d10.5,' Cond=',d10.5)

        !***
        IF (sig < 0.) GO TO 1
        wlam=299.8/fmhz
        epscf=DCMPLX(epr,-sig*wlam*59.96)
        GO TO 2

1       epscf=DCMPLX(epr,sig)
2       CONTINUE
        ck2=6.283185308
        ck2sq=ck2*ck2
        !
        !     SOMMERFELD INTEGRAL EVALUATION USES EXP(-JWT), NEC USES EXP(+JWT),
        !     HENCE NEED CONJG(EPSCF).  CONJUGATE OF FIELDS OCCURS IN SUBROUTINE
        !     EVLUA.
        !
        ck1sq=ck2sq*DCONJG(epscf)
        ck1=SQRT(ck1sq)
        ck1r=dREAL(ck1)
        tkmag=100.*ABS(ck1)
        tsmag=100.*ck1*DCONJG(ck1)
        cksm=ck2sq/(ck1sq+ck2sq)
        ct1=.5*(ck1sq-ck2sq)
        erv=ck1sq*ck1sq
        ezv=ck2sq*ck2sq
        ct2=.125*(erv-ezv)
        erv=erv*ck1sq
        ezv=ezv*ck2sq
        ct3=.0625*(erv-ezv)
        !
        !     LOOP OVER 3 GRID REGIONS
        !
        DO  k=1,3
          nr=nxa(k)
          nth=nya(k)
          dr=dxa(k)
          dth=dya(k)
          r=xsa(k)-dr
          irs=1
          IF (k == 1) r=xsa(k)
          IF (k == 1) irs=2
          !
          !     LOOP OVER R.  (R=SQRT(RHO**2 + (Z+H)**2))
          !
          DO  ir=irs,nr
            r=r+dr
            thet=ysa(k)-dth
            !
            !     LOOP OVER THETA.  (THETA=ATAN((Z+H)/RHO))
            !
            DO  ith=1,nth
              thet=thet+dth
              rho=r*COS(thet)
              zph=r*SIN(thet)
              IF (rho < 1.e-7) rho=1.e-8
              IF (zph < 1.e-7) zph=0.
              CALL evlua (erv,ezv,erh,eph)
              rk=ck2*r
              con=-(0.,4.77147)*r/DCMPLX(COS(rk),-SIN(rk))
              SELECT CASE ( k )
                CASE (    1)
                  GO TO 3
                CASE (    2)
                  GO TO 4
                CASE (    3)
                  GO TO 5
              END SELECT

3             ar1(ir,ith,1)=erv*con
              ar1(ir,ith,2)=ezv*con
              ar1(ir,ith,3)=erh*con
              ar1(ir,ith,4)=eph*con
              CYCLE

4             ar2(ir,ith,1)=erv*con
              ar2(ir,ith,2)=ezv*con
              ar2(ir,ith,3)=erh*con
              ar2(ir,ith,4)=eph*con
              CYCLE

5             ar3(ir,ith,1)=erv*con
              ar3(ir,ith,2)=ezv*con
              ar3(ir,ith,3)=erh*con
              ar3(ir,ith,4)=eph*con
            END DO
          END DO
        END DO
        !
        !     FILL GRID 1 FOR R EQUAL TO ZERO.
        !
        cl2=-(0.,188.370)*(epscf-1.)/(epscf+1.)
        cl1=cl2/(epscf+1.)
        ezv=epscf*cl1
        thet=-dth
        nth=nya(1)
        DO  ith=1,nth
          thet=thet+dth
          IF (ith == nth) GO TO 7
          tfac2=COS(thet)
          tfac1=(1.-SIN(thet))/tfac2
          tfac2=tfac1/tfac2
          erv=epscf*cl1*tfac1
          erh=cl1*(tfac2-1.)+cl2
          eph=cl1*tfac2-cl2
          GO TO 8

7         erv=0.
          erh=cl2-.5*cl1
          eph=-erh
8         ar1(1,ith,1)=erv
          ar1(1,ith,2)=ezv
          ar1(1,ith,3)=erh
          ar1(1,ith,4)=eph
        END DO
        !
        !     WRITE GRID ON TAPE21
        !
        IF (ipt == 0) RETURN
        !
        !     PRINT GRID
        !
        OPEN (UNIT=9,FILE='SOM2D.OUT',STATUS='UNKNOWN',ERR=14)
        WRITE(3,17) epscf
        DO  k=1,3
          nr=nxa(k)
          nth=nya(k)
          WRITE(9,18) k,xsa(k),dxa(k),nr,ysa(k),dya(k),nth
          DO  l=1,4
            WRITE(9,19) lcomp(l)
            DO  ir=1,nr
              SELECT CASE ( k )
                CASE (    1)
                  GO TO 10
                CASE (    2)
                  GO TO 11
                CASE (    3)
                  GO TO 12
              END SELECT
10      WRITE(9,20) ir,(ar1(ir,ith,l),ith=1,nth)
              CYCLE
11      WRITE(9,20) ir,(ar2(ir,ith,l),ith=1,nth)
              CYCLE
12      WRITE(9,20) ir,(ar3(ir,ith,l),ith=1,nth)
            END DO
          END DO
        END DO
14      RETURN

16    FORMAT (' time=',1PE12.5)
17    FORMAT ('1NEC ground interpolation grid',/,' dielectric constant=',1P2E12.5)
18    FORMAT (///,5H grid,i2,/,4X,5HR(1)=,f7.4,4X,3HDR=,f7.4,4X,3HNR=,i3,  &
              /,9H thet(1)=,f7.4,3X,4HDTH=,f7.4,3X,4HNTH=,i3,//)
19    FORMAT (///,1X,a3)
20    FORMAT (4H ir=,i3,/,1X,(1P10E12.5))
22    FORMAT(' STARTING COMPUTATION OF SOMMERFELD INTEGRAL TABLES')
END SUBROUTINE som2d

!----------------------------------------------------------------------------

SUBROUTINE bessel (z,j0,j0p)
!***********************************************************************
!
!     BESSEL EVALUATES THE ZERO-ORDER BESSEL FUNCTION AND ITS DERIVATIVE
!     FOR COMPLEX ARGUMENT Z.
!
        IMPLICIT REAL(NEC2REAL)(a-h,o-z)

        COMPLEX*16, INTENT(IN)                   :: z
        COMPLEX*16, INTENT(OUT)                  :: j0
        COMPLEX*16, INTENT(OUT)                  :: j0p
        SAVE
        COMPLEX*16  p0z,p1z,q0z,q1z, zi,zi2,zk,fj,cz,sz,j0x,j0px
        DIMENSION m(101), a1(25), a2(25)

        REAL(NEC2REAL)                           :: c3  = 0.7978845608
        REAL(NEC2REAL)                           :: p10 = 0.0703125
        REAL(NEC2REAL)                           :: p20 = 0.1121520996
        REAL(NEC2REAL)                           :: q10 = 0.125
        REAL(NEC2REAL)                           :: q20 = 0.0732421875

        REAL(NEC2REAL)                           :: p11 = 0.1171875
        REAL(NEC2REAL)                           :: p21 = 0.1441955566
        REAL(NEC2REAL)                           :: q11 = 0.375
        REAL(NEC2REAL)                           :: q21 = 0.1025390625

        REAL(NEC2REAL)                           :: pof = 0.7853981635
        INTEGER                                  :: init = 0

        REAL(NEC2REAL), DIMENSION(2)             :: fjx = (/0.0, 1.0/)
        EQUIVALENCE (fj,fjx)


        IF (init == 0) THEN
          !     INITIALIZATION OF CONSTANTS
          DO  k=1,25
            a1(k)=-.25D0/(k*k)
            a2(k)=1.d0/(k+1.d0)
          END DO
loop8:    DO  i=1,101
            test=1.d0
            DO  k=1,24
              init=k
              test=-test*i*a1(k)
              IF (test < 1.d-6) EXIT
            END DO
            m(i)=init
          END DO loop8
        END IF

        zms=z*DCONJG(z)
        IF (zms > 1.e-12) GO TO 2
        j0=(1.,0.)
        j0p=-.5*z
        RETURN

2       ib=0
        IF (zms > 37.21) GO TO 4
        IF (zms > 36.) ib=1
        !     SERIES EXPANSION
        iz=1.+zms
        miz=m(iz)
        j0=(1.,0.)
        j0p=j0
        zk=j0
        zi=z*z
        DO  k=1,miz
        zk=zk*a1(k)*zi
        j0=j0+zk
        j0p=j0p+a2(k)*zk
        END DO
        j0p=-.5*z*j0p
        IF (ib == 0) RETURN
        j0x=j0
        j0px=j0p
        !     ASYMPTOTIC EXPANSION
4       zi=1./z
        zi2=zi*zi
        p0z=1.+(p20*zi2-p10)*zi2
        p1z=1.+(p11-p21*zi2)*zi2
        q0z=(q20*zi2-q10)*zi
        q1z=(q11-q21*zi2)*zi
        zk=EXP(fj*(z-pof))
        zi2=1./zk
        cz=.5*(zk+zi2)
        sz=fj*.5*(zi2-zk)
        zk=c3*SQRT(zi)
        j0=zk*(p0z*cz-q0z*sz)
        j0p=-zk*(p1z*sz+q1z*cz)
        IF (ib == 0) RETURN
        zms=COS((SQRT(zms)-6.)*31.41592654)
        j0=.5*(j0x*(1.+zms)+j0*(1.-zms))
        j0p=.5*(j0px*(1.+zms)+j0p*(1.-zms))
        RETURN

END SUBROUTINE bessel
!----------------------------------------------------------------------------
!
!     EVALUA CONTROLS THE INTEGRATION CONTOUR IN THE COMPLEX LAMBDA
!     PLANE FOR EVALUATION OF THE SOMMERFELD INTEGRALS.
!
SUBROUTINE evlua (erv,ezv,erh,eph)
        IMPLICIT REAL(NEC2REAL)(a-h,o-z)

        COMPLEX*16, INTENT(OUT)                  :: erv
        COMPLEX*16, INTENT(OUT)                  :: ezv
        COMPLEX*16, INTENT(OUT)                  :: erh
        COMPLEX*16, INTENT(OUT)                  :: eph
        SAVE
        COMPLEX*16  a,b,ck1,ck1sq,bk,delta,delta2,cp1,cp2,cp3,cksm,ct1,ct2,ct3
        COMPLEX*16, DIMENSION(6)                 :: sum
        COMPLEX*16, DIMENSION(6)                 :: ans
        COMMON /cntour/ a,b
        COMMON /evlcom/ cksm,ct1,ct2,ct3,ck1,ck1sq,ck2,ck2sq,tkmag,tsmag,ck1r,zph,rho,jh

        REAL(NEC2REAL)                           :: ptp = 0.6283185308

        del=zph
        IF (rho > del) del=rho
        IF (zph < 2.*rho) GO TO 4
        !
        !     BESSEL FUNCTION FORM OF SOMMERFELD INTEGRALS
        !
        jh=0
        a=(0.,0.)
        del=1./del
        IF (del <= tkmag) GO TO 2
        b=DCMPLX(.1*tkmag,-.1*tkmag)
        CALL rom1 (6,sum,2)
        a=b
        b=DCMPLX(del,-del)
        CALL rom1 (6,ans,2)
        DO  i=1,6
          sum(i)=sum(i)+ans(i)
        END DO
        GO TO 3

2       b=DCMPLX(del,-del)
        CALL rom1 (6,sum,2)
3       delta=ptp*del
        CALL gshank (b,delta,ans,6,sum,0,b,b)
        GO TO 10
        !
        !     HANKEL FUNCTION FORM OF SOMMERFELD INTEGRALS
        !
4       jh=1
        cp1=DCMPLX(0.d0,.4*ck2)
        cp2=DCMPLX(.6*ck2,-.2*ck2)
        cp3=DCMPLX(1.02*ck2,-.2*ck2)
        a=cp1
        b=cp2
        CALL rom1 (6,sum,2)
        a=cp2
        b=cp3
        CALL rom1 (6,ans,2)
        DO  i=1,6
          sum(i)=-(sum(i)+ans(i))
        END DO
        !     PATH FROM IMAGINARY AXIS TO -INFINITY
        slope=1000.
        IF (zph > .001*rho) slope=rho/zph
        del=ptp/del
        delta=DCMPLX(-1.d0,slope)*del/SQRT(1.+slope*slope)
        delta2=-DCONJG(delta)
        CALL gshank (cp1,delta,ans,6,sum,0,bk,bk)
        rmis=rho*(dREAL(ck1)-ck2)
        IF (rmis < 2.*ck2) GO TO 8
        IF (rho < 1.e-10) GO TO 8
        IF (zph < 1.e-10) GO TO 6
        bk=DCMPLX(-zph,rho)*(ck1-cp3)
        rmis=-dREAL(bk)/ABS(DIMAG(bk))
        IF(rmis > 4.*rho/zph)GO TO 8
        !     INTEGRATE UP BETWEEN BRANCH CUTS, THEN TO + INFINITY
6       cp1=ck1-(.1,.2)
        cp2=cp1+.2
        bk=DCMPLX(0.d0,del)
        CALL gshank (cp1,bk,sum,6,ans,0,bk,bk)
        a=cp1
        b=cp2
        CALL rom1 (6,ans,1)
        DO  i=1,6
          ans(i)=ans(i)-sum(i)
        END DO
        CALL gshank (cp3,bk,sum,6,ans,0,bk,bk)
        CALL gshank (cp2,delta2,ans,6,sum,0,bk,bk)
        GO TO 10
        
        !     INTEGRATE BELOW BRANCH POINTS, THEN TO + INFINITY
8       DO  i=1,6
          sum(i)=-ans(i)
        END DO
        rmis=dREAL(ck1)*1.01
        IF (ck2+1. > rmis) rmis=ck2+1.
        bk=DCMPLX(rmis,.99*DIMAG(ck1))
        delta=bk-cp3
        delta=delta*del/ABS(delta)
        CALL gshank (cp3,delta,ans,6,sum,1,bk,delta2)
10      ans(6)=ans(6)*ck1
        !     CONJUGATE SINCE NEC USES EXP(+JWT)
        erv=DCONJG(ck1sq*ans(3))
        ezv=DCONJG(ck1sq*(ans(2)+ck2sq*ans(5)))
        erh=DCONJG(ck2sq*(ans(1)+ans(6)))
        eph=-DCONJG(ck2sq*(ans(4)+ans(6)))
        RETURN

END SUBROUTINE evlua
!----------------------------------------------------------------------------
!
!     GSHANK INTEGRATES THE 6 SOMMERFELD INTEGRALS FROM START TO
!     INFINITY (UNTIL CONVERGENCE) IN LAMBDA.  AT THE BREAK POINT, BK,
!     THE STEP INCREMENT MAY BE CHANGED FROM DELA TO DELB.  SHANK S
!     ALGORITHM TO ACCELERATE CONVERGENCE OF A SLOWLY CONVERGING SERIES
!     IS USED
!
SUBROUTINE gshank (start,dela,sum,nans,seed,ibk,bk,delb)
IMPLICIT REAL(NEC2REAL)(a-h,o-z)

COMPLEX*16, INTENT(IN)                   :: start
COMPLEX*16, INTENT(IN)                   :: dela
COMPLEX*16, INTENT(IN OUT)               :: sum(6)
INTEGER, INTENT(IN)                      :: nans
COMPLEX*16, INTENT(IN)                   :: seed(6)
INTEGER, INTENT(IN)                      :: ibk
COMPLEX*16, INTENT(IN)                   :: bk
COMPLEX*16, INTENT(IN)                   :: delb
SAVE
COMPLEX*16  a,b,q1,q2,ans1,ans2,a1,a2, as1,as2,del,aa
COMMON /cntour/ a,b
DIMENSION q1(6,20), q2(6,20), ans1(6), ans2(6)
DATA crit/1.e-4/,maxh/20/

rbk=dREAL(bk)
del=dela
ibx=0
IF (ibk == 0) ibx=1
DO  i=1,nans
  ans2(i)=seed(i)
END DO

b=start

loop20: DO  INT=1,maxh
  inx=INT
  a=b
  b=b+del
  IF (ibx == 0.AND.dREAL(b) >= rbk) GO TO 5
  CALL rom1 (nans,sum,2)
  DO  i=1,nans
    ans1(i)=ans2(i)+sum(i)
  END DO
  a=b
  b=b+del
  IF (ibx == 0.AND.dREAL(b) >= rbk) GO TO 6
  CALL rom1 (nans,sum,2)
  DO  i=1,nans
    ans2(i)=ans1(i)+sum(i)
  END DO
  GO TO 11

!     HIT BREAK POINT.  RESET SEED AND START OVER.
  5     ibx=1
  GO TO 7
  6     ibx=2
  7     b=bk
  del=delb
  CALL rom1 (nans,sum,2)
  IF (ibx == 2) GO TO 9
  DO  i=1,nans
    ans2(i)=ans2(i)+sum(i)
  END DO
  CYCLE loop20          ! hwh GO TO 2

  9     DO  i=1,nans
    ans2(i)=ans1(i)+sum(i)
  END DO
  CYCLE loop20          ! hwh GO TO 2
  11    den=0.
  DO  i=1,nans
    as1=ans1(i)
    as2=ans2(i)
    IF (INT < 2) GO TO 17
    DO  j=2,INT
      jm=j-1
      aa=q2(i,jm)
      a1=q1(i,jm)+as1-2.*aa
      IF (dREAL(a1) == 0..AND.DIMAG(a1) == 0.) GO TO 12
      a2=aa-q1(i,jm)
      a1=q1(i,jm)-a2*a2/a1
      GO TO 13
      12    a1=q1(i,jm)
      13    a2=aa+as2-2.*as1
      IF (dREAL(a2) == 0..AND.DIMAG(a2) == 0.) GO TO 14
      a2=aa-(as1-aa)*(as1-aa)/a2
      GO TO 15
      14    a2=aa
      15    q1(i,jm)=as1
      q2(i,jm)=as2
      as1=a1
      as2=a2
    END DO
    17    q1(i,INT)=as1
    q2(i,INT)=as2
    amg=ABS(dREAL(as2))+ABS(DIMAG(as2))
    IF (amg > den) den=amg
  END DO
  denm=1.e-3*den*crit
  jm=INT-3
  IF (jm < 1) jm=1
  DO  j=jm,INT
    DO  i=1,nans
      a1=q2(i,j)
      den=(ABS(dREAL(a1))+ABS(DIMAG(a1)))*crit
      IF (den < denm) den=denm
      a1=q1(i,j)-a1
      amg=ABS(dREAL(a1))+ABS(DIMAG(a1))
      IF (amg > den) CYCLE loop20
    END DO
  END DO
  GO TO 22
END DO loop20
WRITE(*,24)
DO  i=1,nans
  WRITE(*,25) q1(i,inx),q2(i,inx)
END DO
22    DO  i=1,nans
  sum(i)=.5*(q1(i,inx)+q2(i,inx))
END DO
RETURN
!
24    FORMAT (46H **** no convergence in SUBROUTINE gshank ****)
25    FORMAT (1X,1P10E12.5)
END SUBROUTINE gshank

!***********************************************************************
!----------------------------------------------------------------------------

SUBROUTINE hankel (z,h0,h0p)
!***********************************************************************
!
!     HANKEL EVALUATES HANKEL FUNCTION OF THE FIRST KIND, ORDER ZERO,
!     AND ITS DERIVATIVE FOR COMPLEX ARGUMENT Z.
!
    USE nec2dpar,  ONLY : pi

    IMPLICIT REAL(NEC2REAL)(a-h,o-z)

    COMPLEX*16, INTENT(IN)                   :: z
    COMPLEX*16, INTENT(OUT)                  :: h0
    COMPLEX*16, INTENT(OUT)                  :: h0p
    SAVE
    COMPLEX*16 clogz, j0,j0p,p0z,p1z,q0z,q1z,y0,y0p, zi,zi2,zk, fj
    DIMENSION m(101), a1(25), a2(25), a3(25), a4(25), fjx(2)
    EQUIVALENCE (fj,fjx)
    DATA gamma,c1,c2,c3,p10,p20/.5772156649,-.0245785095,.3674669052,.7978845608,.0703125,.1121520996/
    DATA q10,q20,p11,p21,q11,q21/.125,.0732421875,.1171875,.1441955566,.375,.1025390625/
    DATA pof,init/.7853981635,0/,fjx/0.,1./

    IF (init == 0) GO TO 5

    1     zms=z*DCONJG(z)
    IF (zms /= 0.) GO TO 2
    WRITE(*,9)
    STOP
    2     ib=0
    IF (zms > 16.81) GO TO 4
    IF (zms > 16.) ib=1
    !     SERIES EXPANSION
    iz=1.+zms
    miz=m(iz)
    j0=(1.,0.)
    j0p=j0
    y0=(0.,0.)
    y0p=y0
    zk=j0
    zi=z*z
    DO  k=1,miz
      zk=zk*a1(k)*zi
      j0=j0+zk
      j0p=j0p+a2(k)*zk
      y0=y0+a3(k)*zk
      y0p=y0p+a4(k)*zk
    END DO
    j0p=-.5*z*j0p
    clogz=LOG(.5*z)
    y0=(2.*j0*clogz-y0)/pi+c2
    y0p=(2./z+2.*j0p*clogz+.5*y0p*z)/pi+c1*z
    h0=j0+fj*y0
    h0p=j0p+fj*y0p
    IF (ib == 0) RETURN
    y0=h0
    y0p=h0p
    !     ASYMPTOTIC EXPANSION
    4     zi=1./z
    zi2=zi*zi
    p0z=1.+(p20*zi2-p10)*zi2
    p1z=1.+(p11-p21*zi2)*zi2
    q0z=(q20*zi2-q10)*zi
    q1z=(q11-q21*zi2)*zi
    zk=EXP(fj*(z-pof))*SQRT(zi)*c3
    h0=zk*(p0z+fj*q0z)
    h0p=fj*zk*(p1z+fj*q1z)
    IF (ib == 0) RETURN
    zms=COS((SQRT(zms)-4.)*31.41592654)
    h0=.5*(y0*(1.+zms)+h0*(1.-zms))
    h0p=.5*(y0p*(1.+zms)+h0p*(1.-zms))
    RETURN

!     INITIALIZATION OF CONSTANTS
5   psi=-gamma
    DO  k=1,25
      a1(k)=-.25D0/(k*k)
      a2(k)=1.d0/(k+1.d0)
      psi=psi+1.d0/k
      a3(k)=psi+psi
      a4(k)=(psi+psi+1.d0/(k+1.d0))/(k+1.d0)
    END DO
loop8:  DO  i=1,101
      test=1.d0
      DO  k=1,24
        init=k
        test=-test*i*a1(k)
        IF (test*a3(k) < 1.d-6) EXIT
      END DO
      m(i)=init
    END DO loop8
    GO TO 1

9     FORMAT (34H error - hankel NOT valid for z=0.)
END SUBROUTINE hankel

!***********************************************************************
!----------------------------------------------------------------------------

SUBROUTINE lambda (t,xlam,dxlam)
!***********************************************************************
!
!     COMPUTE INTEGRATION PARAMETER XLAM=LAMBDA FROM PARAMETER T.
!
IMPLICIT REAL(NEC2REAL)(a-h,o-z)

REAL(NEC2REAL), INTENT(IN)                         :: t
COMPLEX*16, INTENT(OUT)                  :: xlam
COMPLEX*16, INTENT(OUT)                  :: dxlam
SAVE
COMPLEX*16 a,b
COMMON /cntour/ a,b

dxlam=b-a
xlam=a+dxlam*t
RETURN
END SUBROUTINE lambda

!***********************************************************************
!----------------------------------------------------------------------------

SUBROUTINE rom1 (n,sum,nx)
!***********************************************************************
!
!     ROM1 INTEGRATES THE 6 SOMMERFELD INTEGRALS FROM A TO B IN LAMBDA.
!     THE METHOD OF VARIABLE INTERVAL WIDTH ROMBERG INTEGRATION IS USED.
!
IMPLICIT REAL(NEC2REAL)(a-h,o-z)

INTEGER, INTENT(IN)                      :: n
COMPLEX*16, INTENT(OUT)                  :: sum(6)
INTEGER, INTENT(IN)                      :: nx
SAVE
COMPLEX*16 a,b, g1,g2,g3,g4,g5,t00,t01,t10,t02,t11,t20
COMMON /cntour/ a,b
DIMENSION  g1(6), g2(6), g3(6), g4(6), g5(6), t01(6), t10(6 ), t20(6)
DATA nm,nts,rx/131072,4,1.e-4/

lstep=0
z=0.
ze=1.
s=1.
ep=s/(1.e4*nm)
zend=ze-ep
DO  i=1,n
  sum(i)=(0.,0.)
END DO
ns=nx
nt=0
CALL saoa (z,g1)
2     dz=s/ns
IF (z+dz <= ze) GO TO 3
dz=ze-z
IF (dz <= ep) GO TO 17
3     dzot=dz*.5
CALL saoa (z+dzot,g3)
CALL saoa (z+dz,g5)
4     nogo=0
DO  i=1,n
  t00=(g1(i)+g5(i))*dzot
  t01(i)=(t00+dz*g3(i))*.5
  t10(i)=(4.*t01(i)-t00)/3.
!     TEST CONVERGENCE OF 3 POINT ROMBERG RESULT
  CALL test (dREAL(t01(i)),dREAL(t10(i)),tr,DIMAG(t01(i)),DIMAG(t10  &
      (i)),ti,0.d0)
  IF (tr > rx.OR.ti > rx) nogo=1
END DO
IF (nogo /= 0) GO TO 7
DO  i=1,n
  sum(i)=sum(i)+t10(i)
END DO
nt=nt+2
GO TO 11
7     CALL saoa (z+dz*.25,g2)
CALL saoa (z+dz*.75,g4)
nogo=0
DO  i=1,n
  t02=(t01(i)+dzot*(g2(i)+g4(i)))*.5
  t11=(4.*t02-t01(i))/3.
  t20(i)=(16.*t11-t10(i))/15.
!     TEST CONVERGENCE OF 5 POINT ROMBERG RESULT
  CALL test (dREAL(t11),dREAL(t20(i)),tr,DIMAG(t11),DIMAG(t20(i)),ti ,0.d0)
  IF (tr > rx.OR.ti > rx) nogo=1
END DO
IF (nogo /= 0) GO TO 13
9     DO  i=1,n
  sum(i)=sum(i)+t20(i)
END DO
nt=nt+1
11    z=z+dz
IF (z > zend) GO TO 17
DO  i=1,n
  g1(i)=g5(i)
END DO
IF (nt < nts.OR.ns <= nx) GO TO 2
ns=ns/2
nt=1
GO TO 2
13    nt=0
IF (ns < nm) GO TO 15
IF (lstep == 1) GO TO 9
lstep=1
CALL lambda (z,t00,t11)
WRITE(*,18) t00
WRITE(*,19) z,dz,a,b
DO  i=1,n
  WRITE(*,19) g1(i),g2(i),g3(i),g4(i),g5(i)
END DO
GO TO 9
15    ns=ns*2
dz=s/ns
dzot=dz*.5
DO  i=1,n
  g5(i)=g3(i)
  g3(i)=g2(i)
END DO
GO TO 4
17    CONTINUE
RETURN
!
18    FORMAT (38H rom1 -- step size limited at lambda =,1P2E12.5)
19    FORMAT (1X,1P10E12.5)
END SUBROUTINE rom1

!***********************************************************************
!----------------------------------------------------------------------------

SUBROUTINE saoa (t,ans)
!***********************************************************************
!
!     SAOA COMPUTES THE INTEGRAND FOR EACH OF THE 6
!     SOMMERFELD INTEGRALS FOR SOURCE AND OBSERVER ABOVE GROUND
!
IMPLICIT REAL(NEC2REAL)(a-h,o-z)

REAL(NEC2REAL), INTENT(IN)                       :: t
COMPLEX*16, INTENT(OUT)                  :: ans(6)
SAVE
COMPLEX*16  xl,dxl,cgam1,cgam2,b0,b0p,com,ck1,ck1sq,cksm,ct1,  &
    ct2,ct3,dgam,den1,den2
COMMON /evlcom/ cksm,ct1,ct2,ct3,ck1,ck1sq,ck2,ck2sq,tkmag,tsmag,ck1r,zph,rho,jh


CALL lambda (t,xl,dxl)
IF (jh > 0) GO TO 1
!     BESSEL FUNCTION FORM
CALL bessel (xl*rho,b0,b0p)
b0=2.*b0
b0p=2.*b0p
cgam1=SQRT(xl*xl-ck1sq)
cgam2=SQRT(xl*xl-ck2sq)
IF (dREAL(cgam1) == 0.) cgam1=DCMPLX(0.d0,-ABS(DIMAG(cgam1)))
IF (dREAL(cgam2) == 0.) cgam2=DCMPLX(0.d0,-ABS(DIMAG(cgam2)))
GO TO 2
!     HANKEL FUNCTION FORM
1     CALL hankel (xl*rho,b0,b0p)
com=xl-ck1
cgam1=SQRT(xl+ck1)*SQRT(com)
IF (dREAL(com) < 0..AND.DIMAG(com) >= 0.) cgam1=-cgam1
com=xl-ck2
cgam2=SQRT(xl+ck2)*SQRT(com)
IF (dREAL(com) < 0..AND.DIMAG(com) >= 0.) cgam2=-cgam2
2     xlr=xl*DCONJG(xl)
IF (xlr < tsmag) GO TO 3
IF (DIMAG(xl) < 0.) GO TO 4
xlr=dREAL(xl)
IF (xlr < ck2) GO TO 5
IF (xlr > ck1r) GO TO 4
3     dgam=cgam2-cgam1
GO TO 7
4     SIGN=1.
GO TO 6
5     SIGN=-1.
6     dgam=1./(xl*xl)
dgam=SIGN*((ct3*dgam+ct2)*dgam+ct1)/xl
7     den2=cksm*dgam/(cgam2*(ck1sq*cgam2+ck2sq*cgam1))
den1=1./(cgam1+cgam2)-cksm/cgam2
com=dxl*xl*EXP(-cgam2*zph)
ans(6)=com*b0*den1/ck1
com=com*den2
IF (rho == 0.) GO TO 8
b0p=b0p/rho
ans(1)=-com*xl*(b0p+b0*xl)
ans(4)=com*xl*b0p
GO TO 9
8     ans(1)=-com*xl*xl*.5
ans(4)=ans(1)
9     ans(2)=com*cgam2*cgam2*b0
ans(3)=-ans(4)*cgam2*rho
ans(5)=com*b0
RETURN
END SUBROUTINE saoa

!***********************************************************************

SUBROUTINE arc (itg,ns,rada,ang1,ang2,rad)
!***********************************************************************
! ***
!     DOUBLE PRECISION 6/4/85
!
USE nec2dpar
USE data

IMPLICIT REAL(NEC2REAL)(a-h,o-z)

INTEGER, INTENT(IN)                      :: itg
INTEGER, INTENT(IN)                      :: ns
REAL(NEC2REAL), INTENT(IN)                       :: rada
REAL(NEC2REAL), INTENT(IN)                       :: ang1
REAL(NEC2REAL), INTENT(IN)                       :: ang2
REAL(NEC2REAL), INTENT(IN)                       :: rad

REAL(NEC2REAL)                                   :: ta = 0.01745329252D+0
! ***
!
!     ARC GENERATES SEGMENT GEOMETRY DATA FOR AN ARC OF NS SEGMENTS
!
DIMENSION x2(1), y2(1), z2(1)
EQUIVALENCE (x2,si), (y2,alp), (z2,bet)

ist=n+1
n=n+ns
np=n
mp=m
ipsym=0
IF (ns < 1) RETURN
IF (ABS(ang2-ang1) < 360.00001D+0) GO TO 1
WRITE(3,3)
STOP
1     ang=ang1*ta
dang=(ang2-ang1)*ta/ns
xs1=rada*COS(ang)
zs1=rada*SIN(ang)
DO  i=ist,n
  ang=ang+dang
  xs2=rada*COS(ang)
  zs2=rada*SIN(ang)
  x(i)=xs1
  y(i)=0.
  z(i)=zs1
  x2(i)=xs2
  y2(i)=0.
  z2(i)=zs2
  xs1=xs2
  zs1=zs2
  bi(i)=rad
  itag(i)=itg
END DO
RETURN
!
3     FORMAT (40H error -- arc angle exceeds 360. degrees)
END SUBROUTINE arc
!------------------------------------------------------------------

!
!     ATGN2 IS ARCTANGENT FUNCTION MODIFIED TO RETURN 0. WHEN X=Y=0.
!
REAL(NEC2REAL) FUNCTION atgn2 (x,y)
    IMPLICIT NONE

    REAL(NEC2REAL), INTENT(IN)                     :: x
    REAL(NEC2REAL), INTENT(IN)                     :: y

    IF (x == 0.0) THEN
        IF (y == 0.0) THEN
            atgn2=0.0
            RETURN
        END IF
    END IF

    atgn2=ATAN2(x,y)
    RETURN
END FUNCTION atgn2
!----------------------------------------------------------------------------

SUBROUTINE blckot (ar,nunit,ix1,ix2,nblks,neof)
    ! ***
    !     DOUBLE PRECISION 6/4/85
    !
    ! FIXME eliminate unUSEd PARAMETERs???
    IMPLICIT REAL(NEC2REAL)(a-h,o-z)

    COMPLEX*16, INTENT(IN)                   :: ar(1)
    INTEGER, INTENT(IN)                      :: nunit
    INTEGER, INTENT(IN)                      :: ix1
    INTEGER, INTENT(IN)                      :: ix2
    INTEGER, INTENT(IN)                      :: nblks
    INTEGER, INTENT(IN)                      :: neof
    ! ***
    !
    !     BLCKOT CONTROLS THE READING AND WRITING OF MATRIX BLOCKS ON FILES
    !     FOR THE OUT-OF-CORE MATRIX SOLUTION.
    !
    i1=(ix1+1)/2
    i2=(ix2+1)/2
1   WRITE (nunit) (ar(j),j=i1,i2)
    RETURN

END SUBROUTINE blckot
!----------------------------------------------------------------------------

SUBROUTINE blckin(ar,nunit,ix1,ix2,nblks,neof)
    IMPLICIT REAL(NEC2REAL)(a-h,o-z)

    COMPLEX*16, INTENT(IN OUT)               :: ar(1)
    INTEGER, INTENT(IN)                      :: nunit
    INTEGER, INTENT(IN)                      :: ix1
    INTEGER, INTENT(IN)                      :: ix2
    INTEGER, INTENT(IN)                      :: nblks
    INTEGER, INTENT(IN)                      :: neof
    ! ***
    !
    !     BLCKOT CONTROLS THE READING AND WRITING OF MATRIX BLOCKS ON FILES
    !     FOR THE OUT-OF-CORE MATRIX SOLUTION.
    !

    i1=(ix1+1)/2
    i2=(ix2+1)/2
    DO  i=1,nblks
      READ (nunit,END=1) (ar(j),j=i1,i2)
    END DO
    RETURN

1   WRITE(3,2)  nunit,nblks,neof
    IF (neof /= 777) STOP
    ! hwh neof=0
    RETURN
    !
2   FORMAT ('  eof on UNIT',i3,'  nblks= ',i3,'  neof= ',i5)
END SUBROUTINE blckin
!----------------------------------------------------------------------------

SUBROUTINE cabc (curx)
! ***
!     DOUBLE PRECISION 6/4/85
!
USE nec2dpar
USE data
USE crnt
USE angl
USE segj
USE vsorc

IMPLICIT REAL(NEC2REAL)(a-h,o-z)

COMPLEX*16, INTENT(OUT)                  :: curx(1)
! ***
!
!     CABC COMPUTES COEFFICIENTS OF THE CONSTANT (A), SINE (B), AND
!     COSINE (C) TERMS IN THE CURRENT INTERPOLATION FUNCTIONS FOR THE
!     CURRENT VECTOR CUR.
!
COMPLEX*16 curd,ccj,cs1,cs2

DIMENSION t1x(1), t1y(1), t1z(1), t2x(1), t2y(1), t2z(1)
DIMENSION  ccjx(2)

EQUIVALENCE (t1x,si), (t1y,alp), (t1z,bet), (t2x,icon1), (t2y,icon2), (t2z,itag)
EQUIVALENCE (ccj,ccjx)

DATA tp/6.283185308D+0/,ccjx/0.,-0.01666666667D+0/

IF (n == 0) GO TO 6
DO  i=1,n
  air(i)=0.
  aii(i)=0.
  bir(i)=0.
  bii(i)=0.
  cir(i)=0.
  cii(i)=0.
END DO
DO  i=1,n
  ar=dREAL(curx(i))
  ai=DIMAG(curx(i))
  CALL tbf (i,1)
  DO  jx=1,jsno
    j=jco(jx)
    air(j)=air(j)+ax(jx)*ar
    aii(j)=aii(j)+ax(jx)*ai
    bir(j)=bir(j)+bx(jx)*ar
    bii(j)=bii(j)+bx(jx)*ai
    cir(j)=cir(j)+cx(jx)*ar
    cii(j)=cii(j)+cx(jx)*ai
  END DO
END DO
IF (nqds == 0) GO TO 4
DO  is=1,nqds
  i=iqds(is)
  jx=icon1(i)
  icon1(i)=0
  CALL tbf (i,0)
  icon1(i)=jx
  sh=si(i)*.5
  curd=ccj*vqds(is)/((LOG(2.*sh/bi(i))-1.)*(bx(jsno)*COS(tp*sh)+cx(  &
      jsno)*SIN(tp*sh))*wlam)
  ar=dREAL(curd)
  ai=DIMAG(curd)
  DO  jx=1,jsno
    j=jco(jx)
    air(j)=air(j)+ax(jx)*ar
    aii(j)=aii(j)+ax(jx)*ai
    bir(j)=bir(j)+bx(jx)*ar
    bii(j)=bii(j)+bx(jx)*ai
    cir(j)=cir(j)+cx(jx)*ar
    cii(j)=cii(j)+cx(jx)*ai
  END DO
END DO
4     DO  i=1,n
  curx(i)=DCMPLX(air(i)+cir(i),aii(i)+cii(i))
END DO
6     IF (m == 0) RETURN
!     CONVERT SURFACE CURRENTS FROM T1,T2 COMPONENTS TO X,Y,Z COMPONENTS
k=ld-m
jco1=n+2*m+1
jco2=jco1+m
DO  i=1,m
  k=k+1
  jco1=jco1-2
  jco2=jco2-3
  cs1=curx(jco1)
  cs2=curx(jco1+1)
  curx(jco2)=cs1*t1x(k)+cs2*t2x(k)
  curx(jco2+1)=cs1*t1y(k)+cs2*t2y(k)
  curx(jco2+2)=cs1*t1z(k)+cs2*t2z(k)
END DO
RETURN
END SUBROUTINE cabc

FUNCTION cang (z)
! ***
!     DOUBLE PRECISION 6/4/85
!
IMPLICIT REAL(NEC2REAL)(a-h,o-z)

COMPLEX*16, INTENT(IN OUT)               :: z
! ***
!
!     CANG RETURNS THE PHASE ANGLE OF A COMPLEX NUMBER IN DEGREES.
!
cang=atgn2(DIMAG(z),dREAL(z))*57.29577951D+0
RETURN
END FUNCTION cang
!----------------------------------------------------------------------------

SUBROUTINE cmngf (cb,cc,cd,nb,nc,nd,rkhx,iexkx)
! ***
!     DOUBLE PRECISION 6/4/85
!
USE nec2dpar
USE matpar
USE data
USE zload
USE segj

IMPLICIT REAL(NEC2REAL)(a-h,o-z)

COMPLEX*16, INTENT(OUT)                  :: cb(nb,1)
COMPLEX*16, INTENT(OUT)                  :: cc(nc,1)
COMPLEX*16, INTENT(OUT)                  :: cd(nd,1)
INTEGER, INTENT(IN)                      :: nb
INTEGER, INTENT(IN)                      :: nc
INTEGER, INTENT(IN)                      :: nd
REAL(NEC2REAL), INTENT(IN)                         :: rkhx
INTEGER, INTENT(IN)                      :: iexkx

INTEGER                                  :: ix

! ***
!     CMNGF FILLS INTERACTION MATRICIES B, C, AND D FOR N.G.F. SOLUTION
COMPLEX*16  exk,eyk,ezk,exs,eys,ezs,exc,eyc,ezc

COMMON /dataj/ s,b,xj,yj,zj,cabj,sabj,salpj,exk,eyk,ezk,exs,eys,  &
    ezs,exc,eyc,ezc,rkh,ind1,indd1,ind2,indd2,iexk,ipgnd


rkh=rkhx
iexk=iexkx
m1eq=2*m1
m2eq=m1eq+1
meq=2*m
neqp=nd-npcon*2
neqs=neqp-nscon
neqsp=neqs+nc
neqn=nc+n-n1
itx=1
IF (nscon > 0) itx=2
IF (icasx == 1) GO TO 1
REWIND 12
REWIND 14
REWIND 15
IF (icasx > 2) GO TO 5
1     DO  j=1,nd
  DO  i=1,nd
    cd(i,j)=(0.,0.)
  END DO
  DO  i=1,nb
    cb(i,j)=(0.,0.)
    cc(i,j)=(0.,0.)
  END DO
END DO
5     ist=n-n1+1
it=npbx
isv=-npbx
!     LOOP THRU 24 FILLS B.  FOR ICASX=1 OR 2 ALSO FILLS D(WW), D(WS)
DO  iblk=1,nbbx
  isv=isv+npbx
  IF (iblk == nbbx) it=nlbx
  IF (icasx < 3) GO TO 7
  DO  j=1,nd
    DO  i=1,it
      cb(i,j)=(0.,0.)
    END DO
  END DO
  7     i1=isv+1
  i2=isv+it
  in2=i2
  IF (in2 > n1) in2=n1
  im1=i1-n1
  im2=i2-n1
  IF (im1 < 1) im1=1
  imx=1
  IF (i1 <= n1) imx=n1-i1+2
  IF (n2 > n) GO TO 12
!     FILL B(WW),B(WS).  FOR ICASX=1,2 FILL D(WW),D(WS)
  DO  j=n2,n
    CALL trio (j)
    DO  i=1,jsno
      jss=jco(i)
      IF (jss < n2) GO TO 8
!     SET JCO WHEN SOURCE IS NEW BASIS FUNCTION ON NEW SEGMENT
      jco(i)=jss-n1
      CYCLE
!     SOURCE IS PORTION OF MODIFIED BASIS FUNCTION ON NEW SEGMENT
      8     jco(i)=neqs+iconx(jss)
    END DO
    IF (i1 <= in2) CALL cmww (j,i1,in2,cb,nb,cb,nb,0)
    IF (im1 <= im2) CALL cmws (j,im1,im2,cb(imx,1),nb,cb,nb,0)
    IF (icasx > 2) CYCLE
    CALL cmww (j,n2,n,cd,nd,cd,nd,1)
    IF (m2 <= m) CALL cmws (j,m2eq,meq,cd(1,ist),nd,cd,nd,1)
!     LOADING IN D(WW)
    IF (nload == 0) CYCLE
    ir=j-n1
    exk=zarray(j)
    DO  i=1,jsno
      jss=jco(i)
      cd(jss,ir)=cd(jss,ir)-(ax(i)+cx(i))*exk
    END DO
  END DO
  12    IF (nscon == 0) GO TO 20
!     FILL B(WW)PRIME
  DO  i=1,nscon
    j=iscon(i)
!     SOURCES ARE NEW OR MODIFIED BASIS FUNCTIONS ON OLD SEGMENTS WHICH
!     CONNECT TO NEW SEGMENTS
    CALL trio (j)
    jss=0
    DO  ix=1,jsno
      ir=jco(ix)
      IF (ir < n2) GO TO 13
      ir=ir-n1
      GO TO 14
      13    ir=iconx(ir)
      IF (ir == 0) CYCLE
      ir=neqs+ir
      14    jss=jss+1
      jco(jss)=ir
      ax(jss)=ax(ix)
      bx(jss)=bx(ix)
      cx(jss)=cx(ix)
    END DO
    jsno=jss
    IF (i1 <= in2) CALL cmww (j,i1,in2,cb,nb,cb,nb,0)
    IF (im1 <= im2) CALL cmws (j,im1,im2,cb(imx,1),nb,cb,nb,0)
!     SOURCE IS SINGULAR COMPONENT OF PATCH CURRENT THAT IS PART OF
!     MODIFIED BASIS FUNCTION FOR OLD SEGMENT THAT CONNECTS TO A NEW
!     SEGMENT ON END OPPOSITE PATCH.
    IF (i1 <= in2) CALL cmsw (j,i,i1,in2,cb,cb,0,nb,-1)
    IF (nlodf == 0) GO TO 17
    jx=j-isv
    IF (jx < 1.OR.jx > it) GO TO 17
    exk=zarray(j)
    DO  ix=1,jsno
      jss=jco(ix)
      cb(jx,jss)=cb(jx,jss)-(ax(ix)+cx(ix))*exk
    END DO
!     SOURCES ARE PORTIONS OF MODIFIED BASIS FUNCTION J ON OLD SEGMENTS
!     EXCLUDING OLD SEGMENTS THAT DIRECTLY CONNECT TO NEW SEGMENTS.
    17    CALL tbf (j,1)
    jsx=jsno
    jsno=1
    ir=jco(1)
    jco(1)=neqs+i
    DO  ix=1,jsx
      IF (ix == 1) GO TO 18
      ir=jco(ix)
      ax(1)=ax(ix)
      bx(1)=bx(ix)
      cx(1)=cx(ix)
      18    IF (ir > n1) CYCLE
      IF (iconx(ir) /= 0) CYCLE
      IF (i1 <= in2) CALL cmww (ir,i1,in2,cb,nb,cb,nb,0)
      IF (im1 <= im2) CALL cmws (ir,im1,im2,cb(imx,1),nb,cb,nb,0)
!     LOADING FOR B(WW)PRIME
      IF (nlodf == 0) CYCLE
      jx=ir-isv
      IF (jx < 1.OR.jx > it) CYCLE
      exk=zarray(ir)
      jss=jco(1)
      cb(jx,jss)=cb(jx,jss)-(ax(1)+cx(1))*exk
    END DO
  END DO
  20    IF (npcon == 0) GO TO 22
  jss=neqp
!     FILL B(SS)PRIME TO SET OLD PATCH BASIS FUNCTIONS TO ZERO FOR
!     PATCHES THAT CONNECT TO NEW SEGMENTS
  DO  i=1,npcon
    ix=ipcon(i)*2+n1-isv
    ir=ix-1
    jss=jss+1
    IF (ir > 0.AND.ir <= it) cb(ir,jss)=(1.,0.)
    jss=jss+1
    IF (ix > 0.AND.ix <= it) cb(ix,jss)=(1.,0.)
  END DO
  22    IF (m2 > m) GO TO 23
!     FILL B(SW) AND B(SS)
  IF (i1 <= in2) CALL cmsw (m2,m,i1,in2,cb(1,ist),cb,n1,nb,0)
  IF (im1 <= im2) CALL cmss (m2,m,im1,im2,cb(imx,ist),nb,0)
  23    IF (icasx == 1) CYCLE
  WRITE (14) ((cb(i,j),i=1,it),j=1,nd)
END DO
!     FILLING B COMPLETE.  START ON C AND D
it=npbl
isv=-npbl
DO  iblk=1,nbbl
  isv=isv+npbl
  isvv=isv+nc
  IF (iblk == nbbl) it=nlbl
  IF (icasx < 3) GO TO 27
  DO  j=1,it
    DO  i=1,nc
      cc(i,j)=(0.,0.)
    END DO
    DO  i=1,nd
      cd(i,j)=(0.,0.)
    END DO
  END DO
  27    i1=isvv+1
  i2=isvv+it
  in1=i1-m1eq
  in2=i2-m1eq
  IF (in2 > n) in2=n
  im1=i1-n
  im2=i2-n
  IF (im1 < m2eq) im1=m2eq
  IF (im2 > meq) im2=meq
  imx=1
  IF (in1 <= in2) imx=neqn-i1+2
  IF (icasx < 3) GO TO 32
  IF (n2 > n) GO TO 32
!     SAME AS DO 24 LOOP TO FILL D(WW) FOR ICASX GREATER THAN 2
  DO  j=n2,n
    CALL trio (j)
    DO  i=1,jsno
      jss=jco(i)
      IF (jss < n2) GO TO 28
      jco(i)=jss-n1
      CYCLE
      28    jco(i)=neqs+iconx(jss)
    END DO
    IF (in1 <= in2) CALL cmww (j,in1,in2,cd,nd,cd,nd,1)
    IF (im1 <= im2) CALL cmws (j,im1,im2,cd(1,imx),nd,cd,nd,1)
    IF (nload == 0) CYCLE
    ir=j-n1-isv
    IF (ir < 1.OR.ir > it) CYCLE
    exk=zarray(j)
    DO  i=1,jsno
      jss=jco(i)
      cd(jss,ir)=cd(jss,ir)-(ax(i)+cx(i))*exk
    END DO
  END DO
  32    IF (m2 > m) GO TO 33
!     FILL D(SW) AND D(SS)
  IF (in1 <= in2) CALL cmsw (m2,m,in1,in2,cd(ist,1),cd,n1,nd,1)
  IF (im1 <= im2) CALL cmss (m2,m,im1,im2,cd(ist,imx),nd,1)
  33    IF (n1 < 1) GO TO 39
!     FILL C(WW),C(WS), D(WW)PRIME, AND D(WS)PRIME.
  DO  j=1,n1
    CALL trio (j)
    IF (nscon == 0) GO TO 36
    DO  ix=1,jsno
      jss=jco(ix)
      IF (jss < n2) GO TO 34
      jco(ix)=jss+m1eq
      CYCLE
      34    ir=iconx(jss)
      IF (ir /= 0) jco(ix)=neqsp+ir
    END DO
    36    IF (in1 <= in2) CALL cmww (j,in1,in2,cc,nc,cd,nd,itx)
    IF (im1 <= im2) CALL cmws (j,im1,im2,cc(1,imx),nc,cd(1,imx),nd,itx )
  END DO
  IF (nscon == 0) GO TO 39
!     FILL C(WW)PRIME
  DO  ix=1,nscon
    ir=iscon(ix)
    jss=neqs+ix-isv
    IF (jss > 0.AND.jss <= it) cc(ir,jss)=(1.,0.)
  END DO
  39    IF (npcon == 0) GO TO 41
  jss=neqp-isv
!     FILL C(SS)PRIME
  DO  i=1,npcon
    ix=ipcon(i)*2+n1
    ir=ix-1
    jss=jss+1
    IF (jss > 0.AND.jss <= it) cc(ir,jss)=(1.,0.)
    jss=jss+1
    IF (jss > 0.AND.jss <= it) cc(ix,jss)=(1.,0.)
  END DO
  41    IF (m1 < 1) GO TO 42
!     FILL C(SW) AND C(SS)
  IF (in1 <= in2) CALL cmsw (1,m1,in1,in2,cc(n2,1),cc,0,nc,1)
  IF (im1 <= im2) CALL cmss (1,m1,im1,im2,cc(n2,imx),nc,1)
  42    CONTINUE
  IF (icasx == 1) CYCLE
  WRITE (12) ((cd(j,i),j=1,nd),i=1,it)
  WRITE (15) ((cc(j,i),j=1,nc),i=1,it)
END DO
IF(icasx == 1)RETURN
REWIND 12
REWIND 14
REWIND 15
RETURN
END SUBROUTINE cmngf

!----------------------------------------------------------------------------

SUBROUTINE cmset (nrow,cm,rkhx,iexkx)
! ***
!     DOUBLE PRECISION 6/4/85
!
USE nec2dpar
USE matpar
USE data
USE zload
USE segj

IMPLICIT REAL(NEC2REAL)(a-h,o-z)

INTEGER, INTENT(IN)                      :: nrow
COMPLEX*16, INTENT(OUT)                  :: cm(nrow,1)
REAL(NEC2REAL), INTENT(IN)                         :: rkhx
INTEGER, INTENT(IN)                      :: iexkx
! ***
!
!     CMSET SETS UP THE COMPLEX STRUCTURE MATRIX IN THE ARRAY CM
!
COMPLEX*16  zaj,exk,eyk,ezk,exs,eys,ezs,exc,eyc,ezc,ssx, d,deter
COMMON /smat/ ssx(16,16)
COMMON /scratm/ d(2*maxseg)

COMMON /dataj/ s,b,xj,yj,zj,cabj,sabj,salpj,exk,eyk,ezk,exs,eys,  &
    ezs,exc,eyc,ezc,rkh,ind1,indd1,ind2,indd2,iexk,ipgnd


mp2=2*mp
npeq=np+mp2
neq=n+2*m
nop=neq/npeq
IF (icase > 2) REWIND 11
rkh=rkhx
iexk=iexkx
iout=2*npblk*nrow
it=npblk
!
!     CYCLE OVER MATRIX BLOCKS
!
DO  ixblk1=1,nbloks
  isv=(ixblk1-1)*npblk
  IF (ixblk1 == nbloks) it=nlast
  DO  i=1,nrow
    DO  j=1,it
      cm(i,j)=(0.,0.)
    END DO
  END DO
  i1=isv+1
  i2=isv+it
  in2=i2
  IF (in2 > np) in2=np
  im1=i1-np
  im2=i2-np
  IF (im1 < 1) im1=1
  ist=1
  IF (i1 <= np) ist=np-i1+2
  IF (n == 0) GO TO 5
!
!     WIRE SOURCE LOOP
!
  DO  j=1,n
    CALL trio (j)
    DO  i=1,jsno
      ij=jco(i)
      jco(i)=((ij-1)/np)*mp2+ij
    END DO
    IF (i1 <= in2) CALL cmww (j,i1,in2,cm,nrow,cm,nrow,1)
    IF (im1 <= im2) CALL cmws (j,im1,im2,cm(1,ist),nrow,cm,nrow,1)
    IF (nload == 0) CYCLE
!
!     MATRIX ELEMENTS MODIFIED BY LOADING
!
    IF (j > np) CYCLE
    ipr=j-isv
    IF (ipr < 1.OR.ipr > it) CYCLE
    zaj=zarray(j)
    DO  i=1,jsno
      jss=jco(i)
      cm(jss,ipr)=cm(jss,ipr)-(ax(i)+cx(i))*zaj
    END DO
  END DO
  5     IF (m == 0) GO TO 7
!     MATRIX ELEMENTS FOR PATCH CURRENT SOURCES
  jm1=1-mp
  jm2=0
  jst=1-mp2
  DO  i=1,nop
    jm1=jm1+mp
    jm2=jm2+mp
    jst=jst+npeq
    IF (i1 <= in2) CALL cmsw (jm1,jm2,i1,in2,cm(jst,1),cm,0,nrow,1)
    IF (im1 <= im2) CALL cmss (jm1,jm2,im1,im2,cm(jst,ist),nrow,1)
  END DO
  7     IF (icase == 1) CYCLE
  IF (icase == 3) GO TO 12
!     COMBINE ELEMENTS FOR SYMMETRY MODES
  DO  i=1,it
    DO  j=1,npeq
      DO  k=1,nop
        ka=j+(k-1)*npeq
        d(k)=cm(ka,i)
      END DO
      deter=d(1)
      DO  kk=2,nop
        deter=deter+d(kk)
      END DO
      cm(j,i)=deter
      DO  k=2,nop
        ka=j+(k-1)*npeq
        deter=d(1)
        DO  kk=2,nop
          deter=deter+d(kk)*ssx(k,kk)
        END DO
        cm(ka,i)=deter
      END DO
    END DO
  END DO
  IF (icase < 3) CYCLE
!     WRITE BLOCK FOR OUT-OF-CORE CASES.
  12    CALL blckot (cm,11,1,iout,1,31)
END DO
IF (icase > 2) REWIND 11
RETURN
END SUBROUTINE cmset
!----------------------------------------------------------------------------

SUBROUTINE cmss (j1,j2,im1,im2,cm,nrow,itrp)
! ***
!     DOUBLE PRECISION 6/4/85
!
USE nec2dpar
USE data
USE angl

IMPLICIT REAL(NEC2REAL)(a-h,o-z)

INTEGER, INTENT(IN)                      :: j1
INTEGER, INTENT(IN)                      :: j2
INTEGER, INTENT(IN OUT)                  :: im1
INTEGER, INTENT(IN OUT)                  :: im2
COMPLEX*16, INTENT(OUT)                  :: cm(nrow,1)
INTEGER, INTENT(IN)                      :: nrow
INTEGER, INTENT(IN)                      :: itrp
! ***
!     CMSS COMPUTES MATRIX ELEMENTS FOR SURFACE-SURFACE INTERACTIONS.
COMPLEX*16 g11,g12,g21,g22, exk,eyk,ezk,exs,eys,ezs,exc,eyc,ezc
COMMON /dataj/ s,b,xj,yj,zj,cabj,sabj,salpj,exk,eyk,ezk,exs,eys,  &
    ezs,exc,eyc,ezc,rkh,ind1,indd1,ind2,indd2,iexk,ipgnd

DIMENSION t1x(1), t1y(1), t1z(1), t2x(1), t2y(1), t2z(1)
EQUIVALENCE (t1x,si), (t1y,alp), (t1z,bet), (t2x,icon1), (t2y,icon2), (t2z,itag)
EQUIVALENCE (t1xj,cabj), (t1yj,sabj), (t1zj,salpj), (t2xj,b), (t2yj,ind1), (t2zj,ind2)

ldp=ld+1
i1=(im1+1)/2
i2=(im2+1)/2
icomp=i1*2-3
ii1=-1
IF (icomp+2 < im1) ii1=-2
!     LOOP OVER OBSERVATION PATCHES
DO  i=i1,i2
  il=ldp-i
  icomp=icomp+2
  ii1=ii1+2
  ii2=ii1+1
  t1xi=t1x(il)*salp(il)
  t1yi=t1y(il)*salp(il)
  t1zi=t1z(il)*salp(il)
  t2xi=t2x(il)*salp(il)
  t2yi=t2y(il)*salp(il)
  t2zi=t2z(il)*salp(il)
  xi=x(il)
  yi=y(il)
  zi=z(il)
  jj1=-1
!     LOOP OVER SOURCE PATCHES
  DO  j=j1,j2
    jl=ldp-j
    jj1=jj1+2
    jj2=jj1+1
    s=bi(jl)
    xj=x(jl)
    yj=y(jl)
    zj=z(jl)
    t1xj=t1x(jl)
    t1yj=t1y(jl)
    t1zj=t1z(jl)
    t2xj=t2x(jl)
    t2yj=t2y(jl)
    t2zj=t2z(jl)
    CALL hintg (xi,yi,zi)
    g11=-(t2xi*exk+t2yi*eyk+t2zi*ezk)
    g12=-(t2xi*exs+t2yi*eys+t2zi*ezs)
    g21=-(t1xi*exk+t1yi*eyk+t1zi*ezk)
    g22=-(t1xi*exs+t1yi*eys+t1zi*ezs)
    IF (i /= j) GO TO 1
    g11=g11-.5
    g22=g22+.5
    1     IF (itrp /= 0) GO TO 3
!     NORMAL FILL
    IF (icomp < im1) GO TO 2
    cm(ii1,jj1)=g11
    cm(ii1,jj2)=g12
    2     IF (icomp >= im2) CYCLE
    cm(ii2,jj1)=g21
    cm(ii2,jj2)=g22
    CYCLE
!     TRANSPOSED FILL
    3     IF (icomp < im1) GO TO 4
    cm(jj1,ii1)=g11
    cm(jj2,ii1)=g12
    4     IF (icomp >= im2) CYCLE
    cm(jj1,ii2)=g21
    cm(jj2,ii2)=g22
  END DO
END DO
RETURN
END SUBROUTINE cmss
!----------------------------------------------------------------------------

SUBROUTINE cmsw (j1,j2,i1,i2,cm,cw,ncw,nrow,itrp)
! ***
!     DOUBLE PRECISION 6/4/85
!
USE nec2dpar
USE data
USE gnd
USE angl
USE segj

IMPLICIT REAL(NEC2REAL)(a-h,o-z)

INTEGER, INTENT(IN)                      :: j1
INTEGER, INTENT(IN)                      :: j2
INTEGER, INTENT(IN)                      :: i1
INTEGER, INTENT(IN)                      :: i2
COMPLEX*16, INTENT(OUT)                  :: cm(nrow,1)
COMPLEX*16, INTENT(OUT)                  :: cw(nrow,1)
INTEGER, INTENT(IN)                      :: ncw
INTEGER, INTENT(IN)                      :: nrow
INTEGER, INTENT(IN)                      :: itrp

INTEGER                                  :: ip

! ***
!     COMPUTES MATRIX ELEMENTS FOR E ALONG WIRES DUE TO PATCH CURRENT
COMPLEX*16  exk,eyk,ezk,exs,eys,ezs,exc,eyc,ezc ,emel
COMMON /dataj/ s,b,xj,yj,zj,cabj,sabj,salpj,exk,eyk,ezk,exs,eys,  &
    ezs,exc,eyc,ezc,rkh,ind1,indd1,ind2,indd2,iexk,ipgnd

DIMENSION cab(1), sab(1)
DIMENSION t1x(1), t1y(1), t1z(1), t2x(1), t2y(1), t2z(1), emel(9)
EQUIVALENCE (t1x,si), (t1y,alp), (t1z,bet), (t2x,icon1), (t2y,icon2), (t2z,itag), (cab,alp), (sab,bet)
EQUIVALENCE (t1xj,cabj), (t1yj,sabj), (t1zj,salpj), (t2xj,b), (t2yj,ind1), (t2zj,ind2)

ldp=ld+1
neqs=n-n1+2*(m-m1)
IF (itrp < 0) GO TO 13
k=0
icgo=1
!     OBSERVATION LOOP
DO  i=i1,i2
  k=k+1
  xi=x(i)
  yi=y(i)
  zi=z(i)
  cabi=cab(i)
  sabi=sab(i)
  salpi=salp(i)
  ipch=0
  IF (icon1(i) < 10000) GO TO 1
  ipch=icon1(i)-10000
  fsign=-1.
  1     IF (icon2(i) < 10000) GO TO 2
  ipch=icon2(i)-10000
  fsign=1.
  2     jl=0
!     SOURCE LOOP
  DO  j=j1,j2
    js=ldp-j
    jl=jl+2
    t1xj=t1x(js)
    t1yj=t1y(js)
    t1zj=t1z(js)
    t2xj=t2x(js)
    t2yj=t2y(js)
    t2zj=t2z(js)
    xj=x(js)
    yj=y(js)
    zj=z(js)
    s=bi(js)
!     GROUND LOOP
    DO  ip=1,ksymp
      ipgnd=ip
      IF (ipch /= j.AND.icgo == 1) GO TO 9
      IF (ip == 2) GO TO 9
      IF (icgo > 1) GO TO 6
      CALL pcint (xi,yi,zi,cabi,sabi,salpi,emel)
      py=pi*si(i)*fsign
      px=SIN(py)
      py=COS(py)
      exc=emel(9)*fsign
      CALL trio (i)
      IF (i > n1) GO TO 3
      il=neqs+iconx(i)
      GO TO 4
      3     il=i-ncw
      IF (i <= np) il=((il-1)/np)*2*mp+il
      4     IF (itrp /= 0) GO TO 5
      cw(k,il)=cw(k,il)+exc*(ax(jsno)+bx(jsno)*px+cx(jsno)*py)
      GO TO 6
      5     cw(il,k)=cw(il,k)+exc*(ax(jsno)+bx(jsno)*px+cx(jsno)*py)
      6     IF (itrp /= 0) GO TO 7
      cm(k,jl-1)=emel(icgo)
      cm(k,jl)=emel(icgo+4)
      GO TO 8
      7     cm(jl-1,k)=emel(icgo)
      cm(jl,k)=emel(icgo+4)
      8     icgo=icgo+1
      IF (icgo == 5) icgo=1
      GO TO 11
      9     CALL unere (xi,yi,zi)
      IF (itrp /= 0) GO TO 10
!     NORMAL FILL
      cm(k,jl-1)=cm(k,jl-1)+exk*cabi+eyk*sabi+ezk*salpi
      cm(k,jl)=cm(k,jl)+exs*cabi+eys*sabi+ezs*salpi
      GO TO 11
!     TRANSPOSED FILL
      10    cm(jl-1,k)=cm(jl-1,k)+exk*cabi+eyk*sabi+ezk*salpi
      cm(jl,k)=cm(jl,k)+exs*cabi+eys*sabi+ezs*salpi
      11    CONTINUE
    END DO
  END DO
END DO
RETURN
!     FOR OLD SEG. CONNECTING TO OLD PATCH ON ONE END AND NEW SEG. ON
!     OTHER END INTEGRATE SINGULAR COMPONENT (9) OF SURFACE CURRENT ONLY
13    IF (j1 < i1.OR.j1 > i2) GO TO 16
ipch=icon1(j1)
IF (ipch < 10000) GO TO 14
ipch=ipch-10000
fsign=-1.
GO TO 15
14    ipch=icon2(j1)
IF (ipch < 10000) GO TO 16
ipch=ipch-10000
fsign=1.
15    IF (ipch > m1) GO TO 16
js=ldp-ipch
ipgnd=1
t1xj=t1x(js)
t1yj=t1y(js)
t1zj=t1z(js)
t2xj=t2x(js)
t2yj=t2y(js)
t2zj=t2z(js)
xj=x(js)
yj=y(js)
zj=z(js)
s=bi(js)
xi=x(j1)
yi=y(j1)
zi=z(j1)
cabi=cab(j1)
sabi=sab(j1)
salpi=salp(j1)
CALL pcint (xi,yi,zi,cabi,sabi,salpi,emel)
py=pi*si(j1)*fsign
px=SIN(py)
py=COS(py)
exc=emel(9)*fsign
il=jco(jsno)
k=j1-i1+1
cw(k,il)=cw(k,il)+exc*(ax(jsno)+bx(jsno)*px+cx(jsno)*py)
16    RETURN
END SUBROUTINE cmsw
!----------------------------------------------------------------------------

SUBROUTINE cmws (j,i1,i2,cm,nr,cw,nw,itrp)
! ***
!     DOUBLE PRECISION 6/4/85
!
USE nec2dpar
USE data
USE angl
USE segj

IMPLICIT REAL(NEC2REAL)(a-h,o-z)

INTEGER, INTENT(IN OUT)                  :: j
INTEGER, INTENT(IN)                      :: i1
INTEGER, INTENT(IN)                      :: i2
COMPLEX*16, INTENT(OUT)                  :: cm(nr,1)
INTEGER, INTENT(IN)                      :: nr
COMPLEX*16, INTENT(OUT)                  :: cw(nw,1)
INTEGER, INTENT(IN)                      :: nw
INTEGER, INTENT(IN)                      :: itrp
! ***
!
!     CMWS COMPUTES MATRIX ELEMENTS FOR WIRE-SURFACE INTERACTIONS
!
COMPLEX*16  etk,ets,etc,exk,eyk,ezk,exs,eys,ezs,exc,eyc,ezc

COMMON /dataj/ s,b,xj,yj,zj,cabj,sabj,salpj,exk,eyk,ezk,exs,eys,  &
    ezs,exc,eyc,ezc,rkh,ind1,indd1,ind2,indd2,iexk,ipgnd
DIMENSION  cab(1), sab(1)
DIMENSION t1x(1), t1y(1), t1z(1), t2x(1), t2y(1), t2z(1)
EQUIVALENCE (cab,alp), (sab,bet), (t1x,si), (t1y,alp), (t1z,bet)
EQUIVALENCE (t2x,icon1), (t2y,icon2), (t2z,itag)

ldp=ld+1
s=si(j)
b=bi(j)
xj=x(j)
yj=y(j)
zj=z(j)
cabj=cab(j)
sabj=sab(j)
salpj=salp(j)
!
!     OBSERVATION LOOP
!
ipr=0
DO  i=i1,i2
  ipr=ipr+1
  ipatch=(i+1)/2
  ik=i-(i/2)*2
  IF (ik == 0.AND.ipr /= 1) GO TO 1
  js=ldp-ipatch
  xi=x(js)
  yi=y(js)
  zi=z(js)
  CALL hsfld (xi,yi,zi,0.d0)
  IF (ik == 0) GO TO 1
  tx=t2x(js)
  ty=t2y(js)
  tz=t2z(js)
  GO TO 2
  1     tx=t1x(js)
  ty=t1y(js)
  tz=t1z(js)
  2     etk=-(exk*tx+eyk*ty+ezk*tz)*salp(js)
  ets=-(exs*tx+eys*ty+ezs*tz)*salp(js)
  etc=-(exc*tx+eyc*ty+ezc*tz)*salp(js)
!
!     FILL MATRIX ELEMENTS.  ELEMENT LOCATIONS DETERMINED BY CONNECTION
!     DATA.
!
  IF (itrp /= 0) GO TO 4
!     NORMAL FILL
  DO  ij=1,jsno
    jx=jco(ij)
    cm(ipr,jx)=cm(ipr,jx)+etk*ax(ij)+ets*bx(ij)+etc*cx(ij)
  END DO
  CYCLE
  4     IF (itrp == 2) GO TO 6
!     TRANSPOSED FILL
  DO  ij=1,jsno
    jx=jco(ij)
    cm(jx,ipr)=cm(jx,ipr)+etk*ax(ij)+ets*bx(ij)+etc*cx(ij)
  END DO
  CYCLE
!     TRANSPOSED FILL - C(WS) AND D(WS)PRIME (=CW)
  6     DO  ij=1,jsno
    jx=jco(ij)
    IF (jx > nr) GO TO 7
    cm(jx,ipr)=cm(jx,ipr)+etk*ax(ij)+ets*bx(ij)+etc*cx(ij)
    CYCLE
    7     jx=jx-nr
    cw(jx,ipr)=cw(jx,ipr)+etk*ax(ij)+ets*bx(ij)+etc*cx(ij)
  END DO
END DO
RETURN
END SUBROUTINE cmws

!----------------------------------------------------------------------------

SUBROUTINE cmww (j,i1,i2,cm,nr,cw,nw,itrp)
! ***
!     DOUBLE PRECISION 6/4/85
!
USE nec2dpar
USE data
USE angl
USE segj

IMPLICIT REAL(NEC2REAL)(a-h,o-z)

INTEGER, INTENT(IN)                      :: j
INTEGER, INTENT(IN)                      :: i1
INTEGER, INTENT(IN)                      :: i2
COMPLEX*16, INTENT(OUT)                  :: cm(nr,1)
INTEGER, INTENT(IN)                      :: nr
COMPLEX*16, INTENT(OUT)                  :: cw(nw,1)
INTEGER, INTENT(IN)                      :: nw
INTEGER, INTENT(IN)                      :: itrp
! ***
!
!     CMWW COMPUTES MATRIX ELEMENTS FOR WIRE-WIRE INTERACTIONS
!
COMPLEX*16  etk,ets,etc,exk,eyk,ezk,exs,eys,ezs,exc,eyc,ezc

COMMON /dataj/ s,b,xj,yj,zj,cabj,sabj,salpj,exk,eyk,ezk,exs,eys,  &
    ezs,exc,eyc,ezc,rkh,ind1,indd1,ind2,indd2,iexk,ipgnd
DIMENSION  cab(1), sab(1)
EQUIVALENCE (cab,alp), (sab,bet)

!     SET SOURCE SEGMENT PARAMETERS

s=si(j)
b=bi(j)
xj=x(j)
yj=y(j)
zj=z(j)
cabj=cab(j)
sabj=sab(j)
salpj=salp(j)
IF (iexk == 0) GO TO 16

!     DECIDE WETHER EXT. T.W. APPROX. CAN BE USED

ipr=icon1(j)
IF(ipr > 10000)GO TO 5      !<---NEW, av016
IF (ipr < 0) THEN
  GO TO     1
ELSE IF (ipr == 0) THEN
  GO TO     6
ELSE
  GO TO     2
END IF

1     ipr=-ipr
IF (-icon1(ipr) /= j) GO TO 7
GO TO 4
2     IF (ipr /= j) GO TO 3
IF (cabj*cabj+sabj*sabj > 1.d-8) GO TO 7
GO TO 5
3     IF (icon2(ipr) /= j) GO TO 7
4     xi=ABS(cabj*cab(ipr)+sabj*sab(ipr)+salpj*salp(ipr))
IF (xi < 0.999999D+0) GO TO 7
IF (ABS(bi(ipr)/b-1.) > 1.d-6) GO TO 7
5     ind1=0
GO TO 8
6     ind1=1
GO TO 8
7     ind1=2

8     ipr=icon2(j)
IF(ipr > 10000)GO TO 15     !<---NEW, av016
IF (ipr < 0) THEN
  GO TO     9
ELSE IF (ipr == 0) THEN
  GO TO    14
ELSE
  GO TO    10
END IF

9     ipr=-ipr
IF (-icon2(ipr) /= j) GO TO 15
GO TO 12
10    IF (ipr /= j) GO TO 11
IF (cabj*cabj+sabj*sabj > 1.d-8) GO TO 15
GO TO 13
11    IF (icon1(ipr) /= j) GO TO 15
12    xi=ABS(cabj*cab(ipr)+sabj*sab(ipr)+salpj*salp(ipr))
IF (xi < 0.999999D+0) GO TO 15
IF (ABS(bi(ipr)/b-1.) > 1.d-6) GO TO 15
13    ind2=0
GO TO 16
14    ind2=1
GO TO 16
15    ind2=2
16    CONTINUE
!
!     OBSERVATION LOOP
!
ipr=0
DO  i=i1,i2
  ipr=ipr+1
  ij=i-j
  xi=x(i)
  yi=y(i)
  zi=z(i)
  ai=bi(i)
  cabi=cab(i)
  sabi=sab(i)
  salpi=salp(i)
  CALL efld (xi,yi,zi,ai,ij)
  etk=exk*cabi+eyk*sabi+ezk*salpi
  ets=exs*cabi+eys*sabi+ezs*salpi
  etc=exc*cabi+eyc*sabi+ezc*salpi
!
!     FILL MATRIX ELEMENTS.  ELEMENT LOCATIONS DETERMINED BY CONNECTION
!     DATA.
!
  IF (itrp /= 0) GO TO 18
!     NORMAL FILL
  DO  ij=1,jsno
    jx=jco(ij)
    cm(ipr,jx)=cm(ipr,jx)+etk*ax(ij)+ets*bx(ij)+etc*cx(ij)
  END DO
  CYCLE
  18    IF (itrp == 2) GO TO 20
!     TRANSPOSED FILL
  DO  ij=1,jsno
    jx=jco(ij)
    cm(jx,ipr)=cm(jx,ipr)+etk*ax(ij)+ets*bx(ij)+etc*cx(ij)
  END DO
  CYCLE
!     TRANS. FILL FOR C(WW) - TEST FOR ELEMENTS FOR D(WW)PRIME.  (=CW)
  20    DO  ij=1,jsno
    jx=jco(ij)
    IF (jx > nr) GO TO 21
    cm(jx,ipr)=cm(jx,ipr)+etk*ax(ij)+ets*bx(ij)+etc*cx(ij)
    CYCLE
    21    jx=jx-nr
    cw(jx,ipr)=cw(jx,ipr)+etk*ax(ij)+ets*bx(ij)+etc*cx(ij)
  END DO
END DO
RETURN
END SUBROUTINE cmww

!----------------------------------------------------------------------------

SUBROUTINE conect (ignd)
! ***
!     DOUBLE PRECISION 6/4/85
!
USE nec2dpar
USE data
USE segj

IMPLICIT REAL(NEC2REAL)(a-h,o-z)

INTEGER, INTENT(IN OUT)                  :: ignd
! ***
!
!     CONNECT SETS UP SEGMENT CONNECTION DATA IN ARRAYS ICON1 AND ICON2
!     BY SEARCHING FOR SEGMENT ENDS THAT ARE IN CONTACT.
!
DIMENSION x2(1), y2(1), z2(1)
EQUIVALENCE (x2,si), (y2,alp), (z2,bet)

DATA smin/1.d-3/,npmax/10/

nscon=0
npcon=0
IF (ignd == 0) GO TO 3
WRITE(3,54)
IF (ignd > 0) WRITE(3,55)
IF (ipsym /= 2) GO TO 1
np=2*np
mp=2*mp
1     IF (IABS(ipsym) <= 2) GO TO 2
np=n
mp=m
2     IF (np > n) STOP
IF (np == n.AND.mp == m) ipsym=0
3     IF (n == 0) GO TO 26
loop15:  DO  i=1,n
  iconx(i)=0
  xi1=x(i)
  yi1=y(i)
  zi1=z(i)
  xi2=x2(i)
  yi2=y2(i)
  zi2=z2(i)
  slen=SQRT((xi2-xi1)**2+(yi2-yi1)**2+(zi2-zi1)**2)*smin
!
!     DETERMINE CONNECTION DATA FOR END 1 OF SEGMENT.
!
  IF (ignd < 1) GO TO 5
  IF (zi1 > -slen) GO TO 4
  WRITE(3,56)  i
  STOP
  4     IF (zi1 > slen) GO TO 5
  icon1(i)=i
  z(i)=0.
  GO TO 9
  5     ic=i
  DO  j=2,n
    ic=ic+1
    IF (ic > n) ic=1
    sep=ABS(xi1-x(ic))+ABS(yi1-y(ic))+ABS(zi1-z(ic))
    IF (sep > slen) GO TO 6
    icon1(i)=-ic
    GO TO 8
    6     sep=ABS(xi1-x2(ic))+ABS(yi1-y2(ic))+ABS(zi1-z2(ic))
    IF (sep > slen) CYCLE
    icon1(i)=ic
    GO TO 8
  END DO
  IF (i < n2.AND.icon1(i) > 10000) GO TO 8
  icon1(i)=0
!
!     DETERMINE CONNECTION DATA FOR END 2 OF SEGMENT.
!
  8     IF (ignd < 1) GO TO 12
  9     IF (zi2 > -slen) GO TO 10
  WRITE(3,56)  i
  STOP
  10    IF (zi2 > slen) GO TO 12
  IF (icon1(i) /= i) GO TO 11
  WRITE(3,57)  i
  STOP
  11    icon2(i)=i
  z2(i)=0.
  CYCLE loop15
  12    ic=i
  DO  j=2,n
    ic=ic+1
    IF (ic > n) ic=1
    sep=ABS(xi2-x(ic))+ABS(yi2-y(ic))+ABS(zi2-z(ic))
    IF (sep > slen) GO TO 13
    icon2(i)=ic
    CYCLE loop15
    13    sep=ABS(xi2-x2(ic))+ABS(yi2-y2(ic))+ABS(zi2-z2(ic))
    IF (sep > slen) CYCLE
    icon2(i)=-ic
    CYCLE loop15
  END DO
  IF (i < n2.AND.icon2(i) > 10000) CYCLE loop15
  icon2(i)=0
END DO loop15
IF (m == 0) GO TO 26
!     FIND WIRE-SURFACE CONNECTIONS FOR NEW PATCHES
ix=ld+1-m1
i=m2
16    IF (i > m) GO TO 20
ix=ix-1
xs=x(ix)
ys=y(ix)
zs=z(ix)
DO  iseg=1,n
  xi1=x(iseg)
  yi1=y(iseg)
  zi1=z(iseg)
  xi2=x2(iseg)
  yi2=y2(iseg)
  zi2=z2(iseg)
  slen=(ABS(xi2-xi1)+ABS(yi2-yi1)+ABS(zi2-zi1))*smin
!     FOR FIRST END OF SEGMENT
  sep=ABS(xi1-xs)+ABS(yi1-ys)+ABS(zi1-zs)
  IF (sep > slen) GO TO 17
!     CONNECTION - DIVIDE PATCH INTO 4 PATCHES AT PRESENT ARRAY LOC.
  icon1(iseg)=10000+i
  ic=0
  CALL subph (i,ic,xi1,yi1,zi1,xi2,yi2,zi2,xa,ya,za,xs,ys,zs)
  EXIT
  17    sep=ABS(xi2-xs)+ABS(yi2-ys)+ABS(zi2-zs)
  IF (sep > slen) CYCLE
  icon2(iseg)=10000+i
  ic=0
  CALL subph (i,ic,xi1,yi1,zi1,xi2,yi2,zi2,xa,ya,za,xs,ys,zs)
  EXIT
END DO
19    i=i+1
GO TO 16
!     REPEAT SEARCH FOR NEW SEGMENTS CONNECTED TO NGF PATCHES.
20    IF (m1 == 0.OR.n2 > n) GO TO 26
ix=ld+1
i=1
21    IF (i > m1) GO TO 25
ix=ix-1
xs=x(ix)
ys=y(ix)
zs=z(ix)
DO  iseg=n2,n
  xi1=x(iseg)
  yi1=y(iseg)
  zi1=z(iseg)
  xi2=x2(iseg)
  yi2=y2(iseg)
  zi2=z2(iseg)
  slen=(ABS(xi2-xi1)+ABS(yi2-yi1)+ABS(zi2-zi1))*smin
  sep=ABS(xi1-xs)+ABS(yi1-ys)+ABS(zi1-zs)
  IF (sep > slen) GO TO 22
  icon1(iseg)=10001+m
  ic=1
  npcon=npcon+1
  ipcon(npcon)=i
  CALL subph (i,ic,xi1,yi1,zi1,xi2,yi2,zi2,xa,ya,za,xs,ys,zs)
  EXIT
  22    sep=ABS(xi2-xs)+ABS(yi2-ys)+ABS(zi2-zs)
  IF (sep > slen) CYCLE
  icon2(iseg)=10001+m
  ic=1
  npcon=npcon+1
  ipcon(npcon)=i
  CALL subph (i,ic,xi1,yi1,zi1,xi2,yi2,zi2,xa,ya,za,xs,ys,zs)
  EXIT
END DO
24    i=i+1
GO TO 21
25    IF (npcon <= npmax) GO TO 26
WRITE(3,62)  npmax
STOP
26    WRITE(3,58)  n,np,ipsym
IF (m > 0) WRITE(3,61)  m,mp
iseg=(n+m)/(np+mp)
IF (iseg == 1) GO TO 30
IF (ipsym < 0) THEN
  GO TO    28
ELSE IF (ipsym == 0) THEN
  GO TO    27
ELSE
  GO TO    29
END IF
27    STOP
28    WRITE(3,59) iseg
GO TO 30
29    ic=iseg/2
IF (iseg == 8) ic=3
WRITE(3,60)  ic
30    IF (n == 0) GO TO 48
WRITE(3,50)
iseg=0
!     ADJUST CONNECTED SEG. ENDS TO EXACTLY COINCIDE.  PRINT JUNCTIONS
!     OF 3 OR MORE SEG.  ALSO FIND OLD SEG. CONNECTING TO NEW SEG.
DO  j=1,n
  iend=-1
  jend=-1
  ix=icon1(j)
  ic=1
  jco(1)=-j
  xa=x(j)
  ya=y(j)
  za=z(j)
  31    IF (ix == 0) GO TO 43
  IF (ix == j) GO TO 43
  IF (ix > 10000) GO TO 43
  nsflg=0
  32    IF (ix < 0) THEN
    GO TO    33
  ELSE IF (ix == 0) THEN
    GO TO    49
  ELSE
    GO TO    34
  END IF
  33    ix=-ix
  GO TO 35
  34    jend=-jend
  35    IF (ix == j) GO TO 37
  IF (ix < j) GO TO 43
  ic=ic+1
  IF (ic > jmax) GO TO 49
  jco(ic)=ix*jend
  IF (ix > n1) nsflg=1
  IF (jend == 1) GO TO 36
  xa=xa+x(ix)
  ya=ya+y(ix)
  za=za+z(ix)
  ix=icon1(ix)
  GO TO 32
  36    xa=xa+x2(ix)
  ya=ya+y2(ix)
  za=za+z2(ix)
  ix=icon2(ix)
  GO TO 32
  37    sep=ic
  xa=xa/sep
  ya=ya/sep
  za=za/sep
  DO  i=1,ic
    ix=jco(i)
    IF (ix > 0) GO TO 38
    ix=-ix
    x(ix)=xa
    y(ix)=ya
    z(ix)=za
    CYCLE
    38    x2(ix)=xa
    y2(ix)=ya
    z2(ix)=za
  END DO
  IF (n1 == 0) GO TO 42
  IF (nsflg == 0) GO TO 42
  DO  i=1,ic
    ix=IABS(jco(i))
    IF (ix > n1) CYCLE
    IF (iconx(ix) /= 0) CYCLE
    nscon=nscon+1
    IF (nscon <= nsmax) GO TO 40
    WRITE(3,62)  nsmax
    STOP
    40    iscon(nscon)=ix
    iconx(ix)=nscon
  END DO
  42    IF (ic < 3) GO TO 43
  iseg=iseg+1
  WRITE(3,51) iseg,(jco(i),i=1,ic)
  43    IF (iend == 1) CYCLE
  iend=1
  jend=1
  ix=icon2(j)
  ic=1
  jco(1)=j
  xa=x2(j)
  ya=y2(j)
  za=z2(j)
  GO TO 31
END DO
IF (iseg == 0) WRITE(3,52)
IF (n1 == 0.OR.m1 == m) GO TO 48
!     FIND OLD SEGMENTS THAT CONNECT TO NEW PATCHES
DO  j=1,n1
  ix=icon1(j)
  IF (ix < 10000) GO TO 45
  ix=ix-10000
  IF (ix > m1) GO TO 46
  45    ix=icon2(j)
  IF (ix < 10000) CYCLE
  ix=ix-10000
  IF (ix < m2) CYCLE
  46    IF (iconx(j) /= 0) CYCLE
  nscon=nscon+1
  iscon(nscon)=j
  iconx(j)=nscon
END DO
48    CONTINUE
RETURN

49      WRITE(3,53)  ix
        STOP
!
50    FORMAT (//,9X,'- multiple wire junctions -',/,1X,'JUNCTION',4X,'segments  (- for END 1, + for END 2)')
51    FORMAT (1X,i5,5X,20I5,/,(11X,20I5))
52    FORMAT (2X,'NONE')
53    FORMAT (' connect - segment connection error for segment',i5)
54    FORMAT (/,3X,'GROUND plane specified.')
55    FORMAT (/,3X,'WHERE wire ends touch ground, current will be ', &
    'interpolated TO image in ground plane.',/)
56    FORMAT (' geometry DATA error-- segment',i5,' extends below ground')
57    FORMAT (' geometry DATA error--segment',i5,' lies in ground ','plane.')
58    FORMAT (/,3X,'TOTAL segments USEd=',i5,5X,'NO. seg. in ', &
    'A symmetric cell=',i5,5X,'SYMMETRY flag=',i3)
59    FORMAT (' structure has',i4,' fold rotational symmetry',/)
60    FORMAT (' structure has',i2,' planes of symmetry',/)
61    FORMAT (3X,'TOTAL patches USEd=',i5,6X,'NO. patches in a symmetric cell=',i5)
62    FORMAT (' ERROR - NO. NEW SEGMENTS CONNECTED TO N.G.F. SEGMENTS ',  &
    'OR PATCHES EXCEEDS LIMIT OF',i5)
END SUBROUTINE conect


!----------------------------------------------------------------------------

SUBROUTINE couple (cur,wlam)
! ***
!     DOUBLE PRECISION 6/4/85
!
USE nec2dpar
USE yparm
USE vsorc

IMPLICIT REAL(NEC2REAL)(a-h,o-z)

COMPLEX*16, INTENT(IN)                   :: cur(1)
REAL(NEC2REAL), INTENT(IN)                         :: wlam
! ***
!
!     COUPLE COMPUTES THE MAXIMUM COUPLING BETWEEN PAIRS OF SEGMENTS.
!
COMPLEX*16 y11,y12,y22,yl,yin,zl,zin,rho

IF (nsant /= 1.OR.nvqd /= 0) RETURN
j=isegno(nctag(icoup+1),ncseg(icoup+1))
IF (j /= isant(1)) RETURN
icoup=icoup+1
zin=vsant(1)
y11a(icoup)=cur(j)*wlam/zin
l1=(icoup-1)*(ncoup-1)
DO  i=1,ncoup
  IF (i == icoup) CYCLE
  k=isegno(nctag(i),ncseg(i))
  l1=l1+1
  y12a(l1)=cur(k)*wlam/zin
END DO
IF (icoup < ncoup) RETURN
WRITE(3,6)
npm1=ncoup-1
DO  i=1,npm1
  itt1=nctag(i)
  its1=ncseg(i)
  isg1=isegno(itt1,its1)
  l1=i+1
  DO  j=l1,ncoup
    itt2=nctag(j)
    its2=ncseg(j)
    isg2=isegno(itt2,its2)
    j1=j+(i-1)*npm1-1
    j2=i+(j-1)*npm1
    y11=y11a(i)
    y22=y11a(j)
    y12=.5*(y12a(j1)+y12a(j2))
    yin=y12*y12
    dbc=ABS(yin)
    c=dbc/(2.*dREAL(y11)*dREAL(y22)-dREAL(yin))
    IF (c < 0..OR.c > 1.) GO TO 4
    IF (c < .01) GO TO 2
    gmax=(1.-SQRT(1.-c*c))/c
    GO TO 3
    2     gmax=.5*(c+.25*c*c*c)
    3     rho=gmax*DCONJG(yin)/dbc
    yl=((1.-rho)/(1.+rho)+1.)*dREAL(y22)-y22
    zl=1./yl
    yin=y11-yin/(y22+yl)
    zin=1./yin
    dbc=db10(gmax)
    WRITE(3,7)  itt1,its1,isg1,itt2,its2,isg2,dbc,zl,zin
    CYCLE
    4     WRITE(3,8)  itt1,its1,isg1,itt2,its2,isg2,c
  END DO
END DO
RETURN
!
6     FORMAT (///,36X,'- - - isolation DATA - - -',//,6X, &
    '- - coupling between - -',8X,'MAXIMUM',15X,'- - - for maximum coupling - - -'  &
    ,/,12X,'SEG.',14X,'SEG.',3X,'COUPLING',4X,'LOAD impedance (2ND seg.)', &
    7X,'INPUT impedance',/,2X,'TAG/seg.',3X,'NO.',4X,'TAG/seg.',  &
    3X,'NO.',6X,'(db)',8X,'REAL',9X,'IMAG.',9X,'REAL',9X,'IMAG.')
7     FORMAT (2(1X,i4,1X,i4,1X,i5,2X),f9.3,2X,1P,2(2X,e12.5,1X,e12.5))
8     FORMAT (2(1X,i4,1X,i4,1X,i5,2X),'**error** coupling is NOT between 0 AND 1. (=',1P,e12.5,')')
END SUBROUTINE couple
!----------------------------------------------------------------------------

SUBROUTINE datagn
!
!     DATAGN IS THE MAIN ROUTINE FOR INPUT OF GEOMETRY DATA.
!
        USE nec2dpar
        USE plot
        USE data
        USE angl

        IMPLICIT REAL(NEC2REAL)(a-h,o-z)

        CHARACTER (LEN=2)  :: gm
        CHARACTER (LEN=1), DIMENSION(4), PARAMETER :: ipt = (/'P','R','T','Q'/)
        CHARACTER (LEN=2), DIMENSION(13), PARAMETER :: atst = (/'GW','GX','GR','GS','GE','GM','SP','SM','GF','GA','SC', 'GC','GH'/)
        CHARACTER (LEN=1), DIMENSION(2), PARAMETER :: ifx = (/' ','X'/)
        CHARACTER (LEN=1), DIMENSION(2), PARAMETER :: ify = (/' ','Y'/)
        CHARACTER (LEN=1), DIMENSION(2), PARAMETER :: ifz = (/' ','Z'/)

        DIMENSION x2(1), y2(1), z2(1), t1x(1), t1y(1), t1z(1), t2x(1), t2y(1), &
            t2z(1), cab(1), sab(1)

        EQUIVALENCE (t1x,si), (t1y,alp), (t1z,bet), (t2x,icon1), (t2y,icon2), &
            (t2z,itag), (x2,si), (y2,alp), (z2,bet), (cab,alp), (sab,bet)

        REAL(NEC2REAL)                  :: ta = 0.01745329252D+0
        REAL(NEC2REAL)                  :: td = 57.29577951D+0

        ipsym=0
        nwire=0
        n=0
        np=0
        m=0
        mp=0
        n1=0
        n2=1
        m1=0
        m2=1
        isct=0
        iphd=0
        !
        !   READ GEOMETRY DATA CARD AND BRANCH TO 
        !   SECTION FOR OPERATION REQUESTED
        !
1       CALL readgm(2,gm,itg,ns,xw1,yw1,zw1,xw2,yw2,zw2,rad)

        IF (n+m > ld) GO TO 37
        IF (gm == atst(9)) GO TO 27     ! GF
        IF (iphd == 1) GO TO 2
        WRITE(3,40)
        WRITE(3,41)
        iphd=1
2       IF (gm == atst(11)) GO TO 10  ! SC
        isct=0
        IF (gm == atst(1)) GO TO 3      ! GW
        IF (gm == atst(2)) GO TO 18     ! GX
        IF (gm == atst(3)) GO TO 19     ! GR
        IF (gm == atst(4)) GO TO 21     ! GS
        IF (gm == atst(5)) GO TO 29     ! GE
        IF (gm == atst(6)) GO TO 26     ! GM
        IF (gm == atst(7)) GO TO 9      ! SP
        IF (gm == atst(8)) GO TO 13     ! SM
        IF (gm == atst(10)) GO TO 8     ! GA
        !***
        IF (gm == atst(13)) GO TO 123   ! GH
        !***
        GO TO 36        ! geometry card error

        !---------------------
        !
        ! GW   GENERATE SEGMENT DATA FOR STRAIGHT WIRE.
        !
3       nwire=nwire+1
        i1=n+1
        i2=n+ns
        WRITE(3,43)  nwire,xw1,yw1,zw1,xw2,yw2,zw2,rad,ns,i1,i2,itg
        IF (rad == 0) GO TO 4
        xs1=1.
        ys1=1.
        GO TO 7
        4     CALL readgm(2,gm,ix,iy,xs1,ys1,zs1,dummy,dummy,dummy,dummy)
        !***
        IF (gm == atst(12)) GO TO 6             ! GC
        5     WRITE(3,48)
        STOP
        6     WRITE(3,61)  xs1,ys1,zs1
        IF (ys1 == 0.OR.zs1 == 0) GO TO 5
        rad=ys1
        ys1=(zs1/ys1)**(1./(ns-1.))
        7     CALL wire (xw1,yw1,zw1,xw2,yw2,zw2,rad,xs1,ys1,ns,itg)
        GO TO 1
        !=========
        !
        ! GA  GENERATE SEGMENT DATA FOR WIRE ARC
        !
8       nwire=nwire+1
        i1=n+1
        i2=n+ns
        WRITE(3,38)  nwire,xw1,yw1,zw1,xw2,ns,i1,i2,itg
        CALL arc (itg,ns,xw1,yw1,zw1,xw2)
        GO TO 1
        !=========
        !***
        !
        ! GH    GENERATE HELIX
        !
123     nwire=nwire+1
        i1=n+1
        i2=n+ns
        WRITE(3,124) xw1,yw1,nwire,zw1,xw2,yw2,zw2,rad,ns,i1,i2,itg
        CALL helix(xw1,yw1,zw1,xw2,yw2,zw2,rad,ns,itg)
        GO TO 1
        !
124     FORMAT(5X,'HELIX STRUCTURE-   AXIAL SPACING BETWEEN TURNS =',f8.3,  &
          ' TOTAL AXIAL LENGTH =',f8.3/1X,i5,2X,'RADIUS OF HELIX =',4(2X,  &
          f8.3),7X,f11.5,i8,4X,i5,1X,i5,3X,i5)
        !=========
        !
        ! SP    GENERATE SINGLE NEW PATCH
        !
9       i1=m+1
        ns=ns+1
        IF (itg /= 0) GO TO 17
        WRITE(3,51)  i1,ipt(ns),xw1,yw1,zw1,xw2,yw2,zw2
        IF (ns == 2.OR.ns == 4) isct=1
        IF (ns > 1) GO TO 14
        xw2=xw2*ta
        yw2=yw2*ta
        GO TO 16
        !=========
        !
        ! SC    
        !
10      IF (isct == 0) GO TO 17
        i1=m+1
        ns=ns+1
        IF (itg /= 0) GO TO 17
        IF (ns /= 2.AND.ns /= 4) GO TO 17
        xs1=x4
        ys1=y4
        zs1=z4
        xs2=x3
        ys2=y3
        zs2=z3
        x3=xw1
        y3=yw1
        z3=zw1
        IF (ns /= 4) GO TO 11
        x4=xw2
        y4=yw2
        z4=zw2
        11    xw1=xs1
        yw1=ys1
        zw1=zs1
        xw2=xs2
        yw2=ys2
        zw2=zs2
        IF (ns == 4) GO TO 12
        x4=xw1+x3-xw2
        y4=yw1+y3-yw2
        z4=zw1+z3-zw2
        12    WRITE(3,51)  i1,ipt(ns),xw1,yw1,zw1,xw2,yw2,zw2
        WRITE(3,39)  x3,y3,z3,x4,y4,z4
        GO TO 16
        !===========
        !
        ! SM    GENERATE MULTIPLE-PATCH SURFACE
        !
13      i1=m+1
        WRITE(3,59)  i1,ipt(2),xw1,yw1,zw1,xw2,yw2,zw2,itg,ns
        IF (itg < 1.OR.ns < 1) GO TO 17
        14    CALL readgm(2,gm,ix,iy,x3,y3,z3,x4,y4,z4,dummy)
        IF (ns /= 2.AND.itg < 1) GO TO 15
        x4=xw1+x3-xw2
        y4=yw1+y3-yw2
        z4=zw1+z3-zw2
        15    WRITE(3,39)  x3,y3,z3,x4,y4,z4
        IF (gm /= atst(11)) GO TO 17
        16    CALL patch (itg,ns,xw1,yw1,zw1,xw2,yw2,zw2,x3,y3,z3,x4,y4,z4)
        GO TO 1
        17    WRITE(3,60)
        STOP
        !===========
        !
        ! GX    REFLECT STRUCTURE ALONG X,Y, OR Z AXES OR ROTATE TO FORM CYLINDER.
        !
18      iy=ns/10
        iz=ns-iy*10
        ix=iy/10
        iy=iy-ix*10
        IF (ix /= 0) ix=1
        IF (iy /= 0) iy=1
        IF (iz /= 0) iz=1
        WRITE(3,44)  ifx(ix+1),ify(iy+1),ifz(iz+1),itg
        GO TO 20
        !===========
        !
        ! GR    
        !
19      WRITE(3,45)  ns,itg
        ix=-1
        20    CALL reflc (ix,iy,iz,itg,ns)
        GO TO 1
        !===========
        !
        ! GS    SCALE STRUCTURE DIMENSIONS BY FACTOR XW1.
        !
21      IF (n < n2) GO TO 23
        DO  i=n2,n
          x(i)=x(i)*xw1
          y(i)=y(i)*xw1
          z(i)=z(i)*xw1
          x2(i)=x2(i)*xw1
          y2(i)=y2(i)*xw1
          z2(i)=z2(i)*xw1
          bi(i)=bi(i)*xw1
        END DO
23      IF (m < m2) GO TO 25
        yw1=xw1*xw1
        ix=ld+1-m
        iy=ld-m1
        DO  i=ix,iy
          x(i)=x(i)*xw1
          y(i)=y(i)*xw1
          z(i)=z(i)*xw1
          bi(i)=bi(i)*yw1
        END DO
25      WRITE(3,46)  xw1
        GO TO 1
        !===========
        !
        ! GM    MOVE STRUCTURE OR REPRODUCE ORIGINAL STRUCTURE IN NEW POSITIONS.
        !
26      WRITE(3,47)  itg,ns,xw1,yw1,zw1,xw2,yw2,zw2,rad
        xw1=xw1*ta
        yw1=yw1*ta
        zw1=zw1*ta
        CALL move (xw1,yw1,zw1,xw2,yw2,zw2,INT(rad+.5),ns,itg)
        GO TO 1
        !===========
        !
        ! GF     READ NUMERICAL GREEN'S FUNCTION TAPE
        !
27      IF (n+m == 0) GO TO 28
        WRITE(3,52)
        STOP
        28    CALL gfil (itg)
        npsav=np
        mpsav=mp
        ipsav=ipsym
        GO TO 1
        !===========
        !
        ! GE  TERMINATE STRUCTURE GEOMETRY INPUT.
        !
        !***
29      IF(ns == 0) GO TO 290
        iplp1=1
        iplp2=1
290     ix=n1+m1
        !***
        IF (ix == 0) GO TO 30
        np=n
        mp=m
        ipsym=0
30      CALL conect (itg)
        IF (ix == 0) GO TO 31
        np=npsav
        mp=mpsav
        ipsym=ipsav
31      IF (n+m > ld) GO TO 37
        IF (n == 0) GO TO 33
        WRITE(3,53)
        WRITE(3,54)
        DO  i=1,n
          xw1=x2(i)-x(i)
          yw1=y2(i)-y(i)
          zw1=z2(i)-z(i)
          x(i)=(x(i)+x2(i))*.5
          y(i)=(y(i)+y2(i))*.5
          z(i)=(z(i)+z2(i))*.5
          xw2=xw1*xw1+yw1*yw1+zw1*zw1
          yw2=SQRT(xw2)
          yw2=(xw2/yw2+yw2)*.5
          si(i)=yw2
          cab(i)=xw1/yw2
          sab(i)=yw1/yw2
          xw2=zw1/yw2
          IF (xw2 > 1.) xw2=1.
          IF (xw2 < -1.) xw2=-1.
          salp(i)=xw2
          xw2=ASIN(xw2)*td
          yw2=atgn2(yw1,xw1)*td
          WRITE(3,55) i,x(i),y(i),z(i),si(i),xw2,yw2,bi(i),icon1(i),i,  &
              icon2(i),itag(i)
        !***
          IF(iplp1 /= 1) GO TO 320
          WRITE(8,*)x(i),y(i),z(i),si(i),xw2,yw2,bi(i),icon1(i),i,icon2(i)
          320   CONTINUE
        !***
          IF (si(i) > 1.d-20.AND.bi(i) > 0.) CYCLE
          WRITE(3,56)
          STOP
        END DO
33      IF (m == 0) GO TO 35
        WRITE(3,57)
        j=ld+1
        DO  i=1,m
          j=j-1
          xw1=(t1y(j)*t2z(j)-t1z(j)*t2y(j))*salp(j)
          yw1=(t1z(j)*t2x(j)-t1x(j)*t2z(j))*salp(j)
          zw1=(t1x(j)*t2y(j)-t1y(j)*t2x(j))*salp(j)
          WRITE(3,58) i,x(j),y(j),z(j),xw1,yw1,zw1,bi(j),t1x(j),t1y(j),  &
              t1z(j),t2x(j),t2y(j),t2z(j)
        END DO
35      RETURN

36      WRITE(3,48)     ! geometry card error
        WRITE(3,49)  gm,itg,ns,xw1,yw1,zw1,xw2,yw2,zw2,rad
        STOP

37      WRITE(3,50)
        STOP

        !
38    FORMAT (1X,i5,2X,'ARC radius =',f9.5,2X,'FROM',f8.3,' TO',f8.3, &
    ' degrees',11X,f11.5,2X,i5,4X,i5,1X,i5,3X,i5)
39    FORMAT (6X,3F11.5,1X,3F11.5)
40    FORMAT (////,33X,'- - - structure specification - - -',//,37X, &
    'coordinates must be INPUT in',/,37X,'METERS OR be scaled TO meters', &
    /,37X,'BEFORE structure INPUT is ended',//)
41    FORMAT (2X,'WIRE',79X,'NO. of',4X,'FIRST',2X,'LAST',5X,'TAG',/,2X,  &
    'NO.',8X,'X1',9X,'Y1',9X,'Z1',10X,'X2',9X,'Y2',9X,'Z2',6X,'RADIUS'  &
    ,3X,'SEG.',5X,'SEG.',3X,'SEG.',5X,'NO.')
42    FORMAT (a2,i3,i5,7F10.5)
43    FORMAT (1X,i5,3F11.5,1X,4F11.5,2X,i5,4X,i5,1X,i5,3X,i5)
44    FORMAT (6X,'STRUCTURE reflected along the axes',3(1X,a1),'.  tags incremented by',i5)
45    FORMAT (6X,'STRUCTURE rotated about z-axis',i3,' times.  labels incremented by',i5)
46    FORMAT (6X,'STRUCTURE scaled by factor',f10.5)
47    FORMAT (6X,'THE structure has been moved, move DATA card is -'/6X,  &
    i3,i5,7F10.5)
48    FORMAT (' geometry DATA card error')
49    FORMAT (1X,a2,i3,i5,7F10.5)
50    FORMAT (' NUMBER of wire segments AND surface patches exceeds DIMENSION limit.')
51    FORMAT (1X,i5,a1,f10.5,2F11.5,1X,3F11.5)
52    FORMAT (' error - gf must be first geometry DATA card')
53    FORMAT (////33X,'- - - - segmentation DATA - - - -',//,40X, &
    'coordinates in meters',//,25X,'I+ AND i- indicate the segments before AND after i',//)
54    FORMAT (2X,'SEG.',3X,'COORDINATES of seg. center',5X,'SEG.',5X,  &
    'orientation angles',4X,'WIRE',4X,'CONNECTION DATA',3X,'TAG',/,2X,  &
    'NO.',7X,'X',9X,'Y',9X,'Z',7X,'LENGTH',5X,'ALPHA',5X,'BETA',6X,'radius', &
    4X,2HI-,3X,1HI,4X,2HI+,4X,3HNO.)
55    FORMAT (1X,i5,4F10.5,1X,3F10.5,1X,3I5,2X,i5)
56    FORMAT (' segment DATA error')
57    FORMAT (////,44X,'- - - surface patch DATA - - -',//,49X,'coordinates in meters', &
    //,1X,'PATCH',5X,'COORD. of patch center',7X,'unit normal vector',6X,'PATCH', &
    12X,'COMPONENTS of UNIT tangent vectors',/,2X,'NO.',6X,'X',9X,'Y',9X,'Z', &
    9X,'X',7X,'Y',7X,'Z',7X,'AREA',7X,'X1',6X,'Y1',6X,'Z1',7X,'X2',6X,'Y2',6X,'Z2')
58    FORMAT (1X,i4,3F10.5,1X,3F8.4,f10.5,1X,3F8.4,1X,3F8.4)
59    FORMAT (1X,i5,a1,f10.5,2F11.5,1X,3F11.5,5X,'SURFACE -',i4,3H by,i3,' patches')
60    FORMAT (' patch DATA error')
61    FORMAT (9X,'ABOVE wire is tapered.  seg. length ratio =',f9.5,/, &
    33X,'RADIUS from',f9.5,' TO',f9.5)
END SUBROUTINE datagn
!---------------------------------------------------------------------------

! FIXME fix comment
!     FUNCTION DB-- RETURNS DB FOR MAGNITUDE (FIELD) OR MAG**2 (POWER) I
!
REAL(NEC2REAL) FUNCTION db10 (x)
    IMPLICIT NONE

    REAL(NEC2REAL), INTENT(IN)               :: x

    REAL(NEC2REAL), PARAMETER                :: f = 10.0

    IF (x < 1.d-20) THEN
        db10=-999.99
    ELSE
        db10=f*LOG10(x)
    END IF
    RETURN
END FUNCTION db10
!---------------------------------------------------------------------------

! FIXME fix comment
!     FUNCTION DB-- RETURNS DB FOR MAGNITUDE (FIELD) OR MAG**2 (POWER) I
!
REAL(NEC2REAL) FUNCTION db20 (x)
    IMPLICIT NONE

    REAL(NEC2REAL), INTENT(IN)               :: x

    REAL(NEC2REAL), PARAMETER                :: f = 20.0

    IF (x < 1.d-20) THEN
        db20=-999.99
    ELSE
        db20=f*LOG10(x)
    END IF
    RETURN
END FUNCTION db20
!----------------------------------------------------------------------------

SUBROUTINE efld (xi,yi,zi,ai,ij)
! ***
!     DOUBLE PRECISION 6/4/85
!
USE nec2dpar, ONLY : pi
USE gnd

IMPLICIT REAL(NEC2REAL)(a-h,o-z)

REAL(NEC2REAL), INTENT(IN)                       :: xi
REAL(NEC2REAL), INTENT(IN)                       :: yi
REAL(NEC2REAL), INTENT(IN)                       :: zi
REAL(NEC2REAL), INTENT(IN)                       :: ai
INTEGER, INTENT(IN)                      :: ij

INTEGER                                  :: ip
! ***
!
!     COMPUTE NEAR E FIELDS OF A SEGMENT WITH SINE, COSINE, AND
!     CONSTANT CURRENTS.  GROUND EFFECT INCLUDED.
!
COMPLEX*16 txk,tyk,tzk,txs,tys,tzs,txc,tyc,tzc,exk,eyk,ezk,exs,eys  &
    ,ezs,exc,eyc,ezc,epx,epy,refs,refps,zrsin,zratx,zscrn  &
    ,tezs,ters,tezc,terc,tezk,terk,egnd
COMMON /dataj/ s,b,xj,yj,zj,cabj,sabj,salpj,exk,eyk,ezk,exs,eys,  &
    ezs,exc,eyc,ezc,rkh,ind1,indd1,ind2,indd2,iexk,ipgnd
COMMON /incom/ xo,yo,zo,sn,xsn,ysn,isnor
DIMENSION egnd(9)
EQUIVALENCE (egnd(1),txk), (egnd(2),tyk), (egnd(3),tzk), (egnd(4),  &
    txs), (egnd(5),tys), (egnd(6),tzs), (egnd(7),txc), (egnd(8),tyc),  &
    (egnd(9),tzc)
DATA eta/376.73/,tp/6.283185308D+0/

xij=xi-xj
yij=yi-yj
ijx=ij
rfl=-1.
DO  ip=1,ksymp
  IF (ip == 2) ijx=1
  rfl=-rfl
  salpr=salpj*rfl
  zij=zi-rfl*zj
  zp=xij*cabj+yij*sabj+zij*salpr
  rhox=xij-cabj*zp
  rhoy=yij-sabj*zp
  rhoz=zij-salpr*zp
  rh=SQRT(rhox*rhox+rhoy*rhoy+rhoz*rhoz+ai*ai)
  IF (rh > 1.d-10) GO TO 1
  rhox=0.
  rhoy=0.
  rhoz=0.
  GO TO 2
  1     rhox=rhox/rh
  rhoy=rhoy/rh
  rhoz=rhoz/rh
  2     r=SQRT(zp*zp+rh*rh)
  IF (r < rkh) GO TO 3
!
!     LUMPED CURRENT ELEMENT APPROX. FOR LARGE SEPARATIONS
!
  rmag=tp*r
  cth=zp/r
  px=rh/r
  txk=DCMPLX(COS(rmag),-SIN(rmag))
  py=tp*r*r
  tyk=eta*cth*txk*DCMPLX(1.d+0,-1.d+0/rmag)/py
  tzk=eta*px*txk*DCMPLX(1.d+0,rmag-1.d+0/rmag)/(2.*py)
  tezk=tyk*cth-tzk*px
  terk=tyk*px+tzk*cth
  rmag=SIN(pi*s)/pi
  tezc=tezk*rmag
  terc=terk*rmag
  tezk=tezk*s
  terk=terk*s
  txs=(0.,0.)
  tys=(0.,0.)
  tzs=(0.,0.)
  GO TO 6
  3     IF (iexk == 1) GO TO 4
!
!     EKSC FOR THIN WIRE APPROX. OR EKSCX FOR EXTENDED T.W. APPROX.
!
  CALL eksc (s,zp,rh,tp,ijx,tezs,ters,tezc,terc,tezk,terk)
  GO TO 5
  4     CALL ekscx (b,s,zp,rh,tp,ijx,ind1,ind2,tezs,ters,tezc,terc,tezk,terk)
  5     txs=tezs*cabj+ters*rhox
  tys=tezs*sabj+ters*rhoy
  tzs=tezs*salpr+ters*rhoz
  6     txk=tezk*cabj+terk*rhox
  tyk=tezk*sabj+terk*rhoy
  tzk=tezk*salpr+terk*rhoz
  txc=tezc*cabj+terc*rhox
  tyc=tezc*sabj+terc*rhoy
  tzc=tezc*salpr+terc*rhoz
  IF (ip /= 2) GO TO 11
  IF (iperf > 0) GO TO 10
  zratx=zrati
  rmag=r
  xymag=SQRT(xij*xij+yij*yij)
!
!     SET PARAMETERS FOR RADIAL WIRE GROUND SCREEN.
!
  IF (nradl == 0) GO TO 7
  xspec=(xi*zj+zi*xj)/(zi+zj)
  yspec=(yi*zj+zi*yj)/(zi+zj)
  rhospc=SQRT(xspec*xspec+yspec*yspec+t2*t2)
  IF (rhospc > scrwl) GO TO 7
  zscrn=t1*rhospc*LOG(rhospc/t2)
  zratx=(zscrn*zrati)/(eta*zrati+zscrn)
  7     IF (xymag > 1.d-6) GO TO 8
!
!     CALCULATION OF REFLECTION COEFFICIENTS WHEN GROUND IS SPECIFIED.
!
  px=0.
  py=0.
  cth=1.
  zrsin=(1.,0.)
  GO TO 9
  8     px=-yij/xymag
  py=xij/xymag
  cth=zij/rmag
  zrsin=SQRT(1.-zratx*zratx*(1.-cth*cth))
  9     refs=(cth-zratx*zrsin)/(cth+zratx*zrsin)
  refps=-(zratx*cth-zrsin)/(zratx*cth+zrsin)
  refps=refps-refs
  epy=px*txk+py*tyk
  epx=px*epy
  epy=py*epy
  txk=refs*txk+refps*epx
  tyk=refs*tyk+refps*epy
  tzk=refs*tzk
  epy=px*txs+py*tys
  epx=px*epy
  epy=py*epy
  txs=refs*txs+refps*epx
  tys=refs*tys+refps*epy
  tzs=refs*tzs
  epy=px*txc+py*tyc
  epx=px*epy
  epy=py*epy
  txc=refs*txc+refps*epx
  tyc=refs*tyc+refps*epy
  tzc=refs*tzc
  10    exk=exk-txk*frati
  eyk=eyk-tyk*frati
  ezk=ezk-tzk*frati
  exs=exs-txs*frati
  eys=eys-tys*frati
  ezs=ezs-tzs*frati
  exc=exc-txc*frati
  eyc=eyc-tyc*frati
  ezc=ezc-tzc*frati
  CYCLE
  11    exk=txk
  eyk=tyk
  ezk=tzk
  exs=txs
  eys=tys
  ezs=tzs
  exc=txc
  eyc=tyc
  ezc=tzc
END DO
IF (iperf == 2) GO TO 13
RETURN
!
!     FIELD DUE TO GROUND USING SOMMERFELD/NORTON
!
13    sn=SQRT(cabj*cabj+sabj*sabj)
IF (sn < 1.d-5) GO TO 14
xsn=cabj/sn
ysn=sabj/sn
GO TO 15
14    sn=0.
xsn=1.
ysn=0.
!
!     DISPLACE OBSERVATION POINT FOR THIN WIRE APPROXIMATION
!
15    zij=zi+zj
salpr=-salpj
rhox=sabj*zij-salpr*yij
rhoy=salpr*xij-cabj*zij
rhoz=cabj*yij-sabj*xij
rh=rhox*rhox+rhoy*rhoy+rhoz*rhoz
IF (rh > 1.d-10) GO TO 16
xo=xi-ai*ysn
yo=yi+ai*xsn
zo=zi
GO TO 17
16    rh=ai/SQRT(rh)
IF (rhoz < 0.) rh=-rh
xo=xi+rh*rhox
yo=yi+rh*rhoy
zo=zi+rh*rhoz
17    r=xij*xij+yij*yij+zij*zij
IF (r > .95) GO TO 18
!
!     FIELD FROM INTERPOLATION IS INTEGRATED OVER SEGMENT
!
isnor=1
dmin=exk*DCONJG(exk)+eyk*DCONJG(eyk)+ezk*DCONJG(ezk)
dmin=.01*SQRT(dmin)
shaf=.5*s
CALL rom2 (-shaf,shaf,egnd,dmin)
GO TO 19
!
!     NORTON FIELD EQUATIONS AND LUMPED CURRENT ELEMENT APPROXIMATION
!
18    isnor=2
CALL sflds (0.d0,egnd)
GO TO 22
19    zp=xij*cabj+yij*sabj+zij*salpr
rh=r-zp*zp
IF (rh > 1.d-10) GO TO 20
dmin=0.
GO TO 21
20    dmin=SQRT(rh/(rh+ai*ai))
21    IF (dmin > .95) GO TO 22
px=1.-dmin
terk=(txk*cabj+tyk*sabj+tzk*salpr)*px
txk=dmin*txk+terk*cabj
tyk=dmin*tyk+terk*sabj
tzk=dmin*tzk+terk*salpr
ters=(txs*cabj+tys*sabj+tzs*salpr)*px
txs=dmin*txs+ters*cabj
tys=dmin*tys+ters*sabj
tzs=dmin*tzs+ters*salpr
terc=(txc*cabj+tyc*sabj+tzc*salpr)*px
txc=dmin*txc+terc*cabj
tyc=dmin*tyc+terc*sabj
tzc=dmin*tzc+terc*salpr
22    exk=exk+txk
eyk=eyk+tyk
ezk=ezk+tzk
exs=exs+txs
eys=eys+tys
ezs=ezs+tzs
exc=exc+txc
eyc=eyc+tyc
ezc=ezc+tzc
RETURN
END SUBROUTINE efld
!----------------------------------------------------------------------------

SUBROUTINE eksc (s,z,rh,xk,ij,ezs,ers,ezc,erc,ezk,erk)
! ***
!     DOUBLE PRECISION 6/4/85
!
IMPLICIT REAL(NEC2REAL)(a-h,o-z)

REAL(NEC2REAL), INTENT(IN)                         :: s
REAL(NEC2REAL), INTENT(IN)                         :: z
REAL(NEC2REAL), INTENT(IN)                         :: rh
REAL(NEC2REAL), INTENT(IN)                         :: xk
INTEGER, INTENT(IN)                      :: ij
COMPLEX*16, INTENT(OUT)                  :: ezs
COMPLEX*16, INTENT(OUT)                  :: ers
COMPLEX*16, INTENT(OUT)                  :: ezc
COMPLEX*16, INTENT(OUT)                  :: erc
COMPLEX*16, INTENT(OUT)                  :: ezk
COMPLEX*16, INTENT(OUT)                  :: erk
! ***
!     COMPUTE E FIELD OF SINE, COSINE, AND CONSTANT CURRENT FILAMENTS BY
!     THIN WIRE APPROXIMATION.
COMPLEX*16 con,gz1,gz2,gp1,gp2,gzp1,gzp2
COMMON /tmi/ zpk,rkb2,ijx
DIMENSION conx(2)
EQUIVALENCE (conx,con)
DATA conx/0.,4.771341189D+0/

ijx=ij
zpk=xk*z
rhk=xk*rh
rkb2=rhk*rhk
sh=.5*s
shk=xk*sh
ss=SIN(shk)
cs=COS(shk)
z2=sh-z
z1=-(sh+z)
CALL gx (z1,rh,xk,gz1,gp1)
CALL gx (z2,rh,xk,gz2,gp2)
gzp1=gp1*z1
gzp2=gp2*z2
ezs=con*((gz2-gz1)*cs*xk-(gzp2+gzp1)*ss)
ezc=-con*((gz2+gz1)*ss*xk+(gzp2-gzp1)*cs)
erk=con*(gp2-gp1)*rh
CALL intx (-shk,shk,rhk,ij,cint,sint)
ezk=-con*(gzp2-gzp1+xk*xk*DCMPLX(cint,-sint))
gzp1=gzp1*z1
gzp2=gzp2*z2
IF (rh < 1.d-10) GO TO 1
ers=-con*((gzp2+gzp1+gz2+gz1)*ss-(z2*gz2-z1*gz1)*cs*xk)/rh
erc=-con*((gzp2-gzp1+gz2-gz1)*cs+(z2*gz2+z1*gz1)*ss*xk)/rh
RETURN
1     ers=(0.,0.)
erc=(0.,0.)
RETURN
END SUBROUTINE eksc
!----------------------------------------------------------------------------

SUBROUTINE ekscx (bx,s,z,rhx,xk,ij,inx1,inx2,ezs,ers,ezc,erc,ezk,erk)
! ***
!     DOUBLE PRECISION 6/4/85
!
IMPLICIT REAL(NEC2REAL)(a-h,o-z)

REAL(NEC2REAL), INTENT(IN)                         :: bx
REAL(NEC2REAL), INTENT(IN)                         :: s
REAL(NEC2REAL), INTENT(IN)                         :: z
REAL(NEC2REAL), INTENT(IN)                         :: rhx
REAL(NEC2REAL), INTENT(IN)                         :: xk
INTEGER, INTENT(IN)                      :: ij
INTEGER, INTENT(IN OUT)                  :: inx1
INTEGER, INTENT(IN OUT)                  :: inx2
COMPLEX*16, INTENT(OUT)                  :: ezs
COMPLEX*16, INTENT(OUT)                  :: ers
COMPLEX*16, INTENT(OUT)                  :: ezc
COMPLEX*16, INTENT(OUT)                  :: erc
COMPLEX*16, INTENT(OUT)                  :: ezk
!hwh REAL, INTENT(IN OUT)                     :: rk
! ***
!     COMPUTE E FIELD OF SINE, COSINE, AND CONSTANT CURRENT FILAMENTS BY
!     EXTENDED THIN WIRE APPROXIMATION.
COMPLEX*16 con,gz1,gz2,gzp1,gzp2,gr1,gr2,grp1,grp2,grk1,grk2,erk,gzz1,gzz2
COMMON /tmi/ zpk,rkb2,ijx
DIMENSION conx(2)
EQUIVALENCE (conx,con)
DATA conx/0.,4.771341189D+0/

IF (rhx < bx) GO TO 1
rh=rhx
b=bx
ira=0
GO TO 2
1     rh=bx
b=rhx
ira=1
2     sh=.5*s
ijx=ij
zpk=xk*z
rhk=xk*rh
rkb2=rhk*rhk
shk=xk*sh
ss=SIN(shk)
cs=COS(shk)
z2=sh-z
z1=-(sh+z)
a2=b*b
IF (inx1 == 2) GO TO 3
CALL gxx (z1,rh,b,a2,xk,ira,gz1,gzp1,gr1,grp1,grk1,gzz1)
GO TO 4
3     CALL gx (z1,rhx,xk,gz1,grk1)
gzp1=grk1*z1
gr1=gz1/rhx
grp1=gzp1/rhx
grk1=grk1*rhx
gzz1=(0.,0.)
4     IF (inx2 == 2) GO TO 5
CALL gxx (z2,rh,b,a2,xk,ira,gz2,gzp2,gr2,grp2,grk2,gzz2)
GO TO 6
5     CALL gx (z2,rhx,xk,gz2,grk2)
gzp2=grk2*z2
gr2=gz2/rhx
grp2=gzp2/rhx
grk2=grk2*rhx
gzz2=(0.,0.)
6     ezs=con*((gz2-gz1)*cs*xk-(gzp2+gzp1)*ss)
ezc=-con*((gz2+gz1)*ss*xk+(gzp2-gzp1)*cs)
ers=-con*((z2*grp2+z1*grp1+gr2+gr1)*ss-(z2*gr2-z1*gr1)*cs*xk)
erc=-con*((z2*grp2-z1*grp1+gr2-gr1)*cs+(z2*gr2+z1*gr1)*ss*xk)
erk=con*(grk2-grk1)
CALL intx (-shk,shk,rhk,ij,cint,sint)
bk=b*xk
bk2=bk*bk*.25
ezk=-con*(gzp2-gzp1+xk*xk*(1.-bk2)*DCMPLX(cint,-sint)-bk2*(gzz2- gzz1))
RETURN
END SUBROUTINE ekscx

!----------------------------------------------------------------------------

SUBROUTINE etmns (p1,p2,p3,p4,p5,p6,ipr,e)
! ***
!     DOUBLE PRECISION 6/4/85
!
USE nec2dpar
USE data
USE gnd
USE angl
USE vsorc

IMPLICIT REAL(NEC2REAL)(a-h,o-z)

REAL(NEC2REAL), INTENT(IN)                         :: p1
REAL(NEC2REAL), INTENT(IN)                         :: p2
REAL(NEC2REAL), INTENT(IN)                         :: p3
REAL(NEC2REAL), INTENT(IN OUT)                     :: p4
REAL(NEC2REAL), INTENT(IN OUT)                     :: p5
REAL(NEC2REAL), INTENT(IN)                         :: p6
INTEGER, INTENT(IN OUT)                  :: ipr
COMPLEX*16, INTENT(OUT)                  :: e(2*maxseg)
! ***
!
!     ETMNS FILLS THE ARRAY E WITH THE NEGATIVE OF THE ELECTRIC FIELD
!     INCIDENT ON THE STRUCTURE.  E IS THE RIGHT HAND SIDE OF THE MATRIX
!     EQUATION.
!
COMPLEX*16  cx,cy,cz,er,et,ezh,erh,rrv,rrh,tt1,tt2

DIMENSION cab(1), sab(1)
DIMENSION t1x(1), t1y(1), t1z(1), t2x(1), t2y(1), t2z(1)
EQUIVALENCE (cab,alp), (sab,bet)
EQUIVALENCE (t1x,si), (t1y,alp), (t1z,bet), (t2x,icon1), (t2y,icon2), (t2z,itag)
DATA tp/6.283185308D+0/,reta/2.654420938D-3/

neq=n+2*m
nqds=0
IF (ipr > 0.AND.ipr /= 5) GO TO 5
!
!     APPLIED FIELD OF VOLTAGE SOURCES FOR TRANSMITTING CASE
!
DO  i=1,neq
  e(i)=(0.,0.)
END DO
IF (nsant == 0) GO TO 3
DO  i=1,nsant
  is=isant(i)
  e(is)=-vsant(i)/(si(is)*wlam)
END DO
3     IF (nvqd == 0) RETURN
DO  i=1,nvqd
  is=ivqd(i)
  CALL qdsrc (is,vqd(i),e)
END DO
RETURN
5     IF (ipr > 3) GO TO 19
!
!     INCIDENT PLANE WAVE, LINEARLY POLARIZED.
!
cth=COS(p1)
sth=SIN(p1)
cph=COS(p2)
sph=SIN(p2)
cet=COS(p3)
set=SIN(p3)
px=cth*cph*cet-sph*set
py=cth*sph*cet+cph*set
pz=-sth*cet
wx=-sth*cph
wy=-sth*sph
wz=-cth
qx=wy*pz-wz*py
qy=wz*px-wx*pz
qz=wx*py-wy*px
IF (ksymp == 1) GO TO 7
IF (iperf == 1) GO TO 6
rrv=SQRT(1.-zrati*zrati*sth*sth)
rrh=zrati*cth
rrh=(rrh-rrv)/(rrh+rrv)
rrv=zrati*rrv
rrv=-(cth-rrv)/(cth+rrv)
GO TO 7
6     rrv=-(1.,0.)
rrh=-(1.,0.)
7     IF (ipr > 1) GO TO 13
IF (n == 0) GO TO 10
DO  i=1,n
  arg=-tp*(wx*x(i)+wy*y(i)+wz*z(i))
  e(i)=-(px*cab(i)+py*sab(i)+pz*salp(i))*DCMPLX(COS(arg),SIN(arg))
END DO
IF (ksymp == 1) GO TO 10
tt1=(py*cph-px*sph)*(rrh-rrv)
cx=rrv*px-tt1*sph
cy=rrv*py+tt1*cph
cz=-rrv*pz
DO  i=1,n
  arg=-tp*(wx*x(i)+wy*y(i)-wz*z(i))
  e(i)=e(i)-(cx*cab(i)+cy*sab(i)+cz*salp(i))*DCMPLX(COS(arg), SIN(arg))
END DO
10    IF (m == 0) RETURN
i=ld+1
i1=n-1
DO  is=1,m
  i=i-1
  i1=i1+2
  i2=i1+1
  arg=-tp*(wx*x(i)+wy*y(i)+wz*z(i))
  tt1=DCMPLX(COS(arg),SIN(arg))*salp(i)*reta
  e(i2)=(qx*t1x(i)+qy*t1y(i)+qz*t1z(i))*tt1
  e(i1)=(qx*t2x(i)+qy*t2y(i)+qz*t2z(i))*tt1
END DO
IF (ksymp == 1) RETURN
tt1=(qy*cph-qx*sph)*(rrv-rrh)
cx=-(rrh*qx-tt1*sph)
cy=-(rrh*qy+tt1*cph)
cz=rrh*qz
i=ld+1
i1=n-1
DO  is=1,m
  i=i-1
  i1=i1+2
  i2=i1+1
  arg=-tp*(wx*x(i)+wy*y(i)-wz*z(i))
  tt1=DCMPLX(COS(arg),SIN(arg))*salp(i)*reta
  e(i2)=e(i2)+(cx*t1x(i)+cy*t1y(i)+cz*t1z(i))*tt1
  e(i1)=e(i1)+(cx*t2x(i)+cy*t2y(i)+cz*t2z(i))*tt1
END DO
RETURN
!
!     INCIDENT PLANE WAVE, ELLIPTIC POLARIZATION.
!
13    tt1=-(0.,1.)*p6
IF (ipr == 3) tt1=-tt1
IF (n == 0) GO TO 16
cx=px+tt1*qx
cy=py+tt1*qy
cz=pz+tt1*qz
DO  i=1,n
  arg=-tp*(wx*x(i)+wy*y(i)+wz*z(i))
  e(i)=-(cx*cab(i)+cy*sab(i)+cz*salp(i))*DCMPLX(COS(arg),SIN(arg))
END DO
IF (ksymp == 1) GO TO 16
tt2=(cy*cph-cx*sph)*(rrh-rrv)
cx=rrv*cx-tt2*sph
cy=rrv*cy+tt2*cph
cz=-rrv*cz
DO  i=1,n
  arg=-tp*(wx*x(i)+wy*y(i)-wz*z(i))
  e(i)=e(i)-(cx*cab(i)+cy*sab(i)+cz*salp(i))*DCMPLX(COS(arg), SIN(arg))
END DO
16    IF (m == 0) RETURN
cx=qx-tt1*px
cy=qy-tt1*py
cz=qz-tt1*pz
i=ld+1
i1=n-1
DO  is=1,m
  i=i-1
  i1=i1+2
  i2=i1+1
  arg=-tp*(wx*x(i)+wy*y(i)+wz*z(i))
  tt2=DCMPLX(COS(arg),SIN(arg))*salp(i)*reta
  e(i2)=(cx*t1x(i)+cy*t1y(i)+cz*t1z(i))*tt2
  e(i1)=(cx*t2x(i)+cy*t2y(i)+cz*t2z(i))*tt2
END DO
IF (ksymp == 1) RETURN
tt1=(cy*cph-cx*sph)*(rrv-rrh)
cx=-(rrh*cx-tt1*sph)
cy=-(rrh*cy+tt1*cph)
cz=rrh*cz
i=ld+1
i1=n-1
DO  is=1,m
  i=i-1
  i1=i1+2
  i2=i1+1
  arg=-tp*(wx*x(i)+wy*y(i)-wz*z(i))
  tt1=DCMPLX(COS(arg),SIN(arg))*salp(i)*reta
  e(i2)=e(i2)+(cx*t1x(i)+cy*t1y(i)+cz*t1z(i))*tt1
  e(i1)=e(i1)+(cx*t2x(i)+cy*t2y(i)+cz*t2z(i))*tt1
END DO
RETURN
!
!     INCIDENT FIELD OF AN ELEMENTARY CURRENT SOURCE.
!
19    wz=COS(p4)
wx=wz*COS(p5)
wy=wz*SIN(p5)
wz=SIN(p4)
ds=p6*59.958
dsh=p6/(2.*tp)
npm=n+m
is=ld+1
i1=n-1
DO  i=1,npm
  ii=i
  IF (i <= n) GO TO 20
  is=is-1
  ii=is
  i1=i1+2
  i2=i1+1
  20    px=x(ii)-p1
  py=y(ii)-p2
  pz=z(ii)-p3
  rs=px*px+py*py+pz*pz
  IF (rs < 1.d-30) CYCLE
  r=SQRT(rs)
  px=px/r
  py=py/r
  pz=pz/r
  cth=px*wx+py*wy+pz*wz
  sth=SQRT(1.-cth*cth)
  qx=px-wx*cth
  qy=py-wy*cth
  qz=pz-wz*cth
  arg=SQRT(qx*qx+qy*qy+qz*qz)
  IF (arg < 1.d-30) GO TO 21
  qx=qx/arg
  qy=qy/arg
  qz=qz/arg
  GO TO 22
  21    qx=1.
  qy=0.
  qz=0.
  22    arg=-tp*r
  tt1=DCMPLX(COS(arg),SIN(arg))
  IF (i > n) GO TO 23
  tt2=DCMPLX(1.d+0,-1.d+0/(r*tp))/rs
  er=ds*tt1*tt2*cth
  et=.5*ds*tt1*((0.,1.)*tp/r+tt2)*sth
  ezh=er*cth-et*sth
  erh=er*sth+et*cth
  cx=ezh*wx+erh*qx
  cy=ezh*wy+erh*qy
  cz=ezh*wz+erh*qz
  e(i)=-(cx*cab(i)+cy*sab(i)+cz*salp(i))
  CYCLE
  23    px=wy*qz-wz*qy
  py=wz*qx-wx*qz
  pz=wx*qy-wy*qx
  tt2=dsh*tt1*DCMPLX(1./r,tp)/r*sth*salp(ii)
  cx=tt2*px
  cy=tt2*py
  cz=tt2*pz
  e(i2)=cx*t1x(ii)+cy*t1y(ii)+cz*t1z(ii)
  e(i1)=cx*t2x(ii)+cy*t2y(ii)+cz*t2z(ii)
END DO
RETURN
END SUBROUTINE etmns
!----------------------------------------------------------------------------

SUBROUTINE facgf (a,b,c,d,bx,ip,ix,np,n1,mp,m1,n1c,n2c)
! ***
!     DOUBLE PRECISION 6/4/85
!
USE matpar

IMPLICIT REAL(NEC2REAL)(a-h,o-z)

COMPLEX*16, INTENT(IN OUT)               :: a(1)
COMPLEX*16, INTENT(IN OUT)               :: b(n1c,1)
COMPLEX*16, INTENT(IN OUT)               :: c(n1c,1)
COMPLEX*16, INTENT(OUT)                  :: d(n2c,1)
COMPLEX*16, INTENT(IN OUT)               :: bx(n1c,1)
INTEGER, INTENT(IN OUT)                  :: ip(1)
INTEGER, INTENT(IN OUT)                  :: ix
INTEGER, INTENT(IN OUT)                  :: np
INTEGER, INTENT(IN OUT)                  :: n1
INTEGER, INTENT(IN OUT)                  :: mp
INTEGER, INTENT(IN OUT)                  :: m1
INTEGER, INTENT(IN)                      :: n1c
INTEGER, INTENT(IN)                      :: n2c
! ***
!     FACGF COMPUTES AND FACTORS D-C(INV(A)B).
COMPLEX*16  sum
DIMENSION  ix (1)

IF (n2c == 0) RETURN
ibfl=14
IF (icasx < 3) GO TO 1
!     CONVERT B FROM BLOCKS OF ROWS ON T14 TO BLOCKS OF COL. ON T16
CALL reblk (b,c,n1c,npbx,n2c)
ibfl=16
1     npb=npbl
IF (icasx == 2) REWIND 14
!     COMPUTE INV(A)B AND WRITE ON TAPE14
DO  ib=1,nbbl
  IF (ib == nbbl) npb=nlbl
  IF (icasx > 1) READ (ibfl) ((bx(i,j),i=1,n1c),j=1,npb)
  CALL solves (a,ip,bx,n1c,npb,np,n1,mp,m1,13,13)
  IF (icasx == 2) REWIND 14
  IF (icasx > 1) WRITE (14) ((bx(i,j),i=1,n1c),j=1,npb)
END DO
IF (icasx == 1) GO TO 3
REWIND 11
REWIND 12
REWIND 15
REWIND ibfl
3     npc=npbl
!     COMPUTE D-C(INV(A)B) AND WRITE ON TAPE11
DO  ic=1,nbbl
  IF (ic == nbbl) npc=nlbl
  IF (icasx == 1) GO TO 4
  READ (15) ((c(i,j),i=1,n1c),j=1,npc)
  READ (12) ((d(i,j),i=1,n2c),j=1,npc)
  REWIND 14
  4     npb=npbl
  nic=0
  DO  ib=1,nbbl
    IF (ib == nbbl) npb=nlbl
    IF (icasx > 1) READ (14) ((b(i,j),i=1,n1c),j=1,npb)
    DO  i=1,npb
      ii=i+nic
      DO  j=1,npc
        sum=(0.,0.)
        DO  k=1,n1c
          sum=sum+b(k,i)*c(k,j)
        END DO
        d(ii,j)=d(ii,j)-sum
      END DO
    END DO
    nic=nic+npbl
  END DO
  IF (icasx > 1) WRITE (11) ((d(i,j),i=1,n2c),j=1,npbl)
END DO
IF (icasx == 1) GO TO 9
REWIND 11
REWIND 12
REWIND 14
REWIND 15
9     n1cp=n1c+1
!     FACTOR D-C(INV(A)B)
IF (icasx > 1) GO TO 10
CALL factr (n2c,d,ip(n1cp),n2c)
GO TO 13
10    IF (icasx == 4) GO TO 12
npb=npbl
ic=0
DO  ib=1,nbbl
  IF (ib == nbbl) npb=nlbl
  ii=ic+1
  ic=ic+n2c*npb
  READ (11) (b(i,1),i=ii,ic)
END DO
REWIND 11
CALL factr (n2c,b,ip(n1cp),n2c)
nic=n2c*n2c
WRITE (11) (b(i,1),i=1,nic)
REWIND 11
GO TO 13
12    nblsys=nblsym
npsys=npsym
nlsys=nlsym
icass=icase
nblsym=nbbl
npsym=npbl
nlsym=nlbl
icase=3
CALL facio (b,n2c,1,ix(n1cp),11,12,16,11)
CALL lunscr (b,n2c,1,ip(n1cp),ix(n1cp),12,11,16)
nblsym=nblsys
npsym=npsys
nlsym=nlsys
icase=icass
13    RETURN
END SUBROUTINE facgf
!----------------------------------------------------------------------------

SUBROUTINE facio (a,nrow,nop,ip,iu1,iu2,iu3,iu4)
! ***
!     DOUBLE PRECISION 6/4/85
!
USE matpar

IMPLICIT REAL(NEC2REAL)(a-h,o-z)

COMPLEX*16, INTENT(IN OUT)               :: a(nrow,1)
INTEGER, INTENT(IN)                      :: nrow
INTEGER, INTENT(IN)                      :: nop
INTEGER, INTENT(IN OUT)                  :: ip(nrow)
INTEGER, INTENT(IN)                      :: iu1
INTEGER, INTENT(IN)                      :: iu2
INTEGER, INTENT(IN)                      :: iu3
INTEGER, INTENT(IN)                      :: iu4
! ***
!
!     FACIO CONTROLS I/O FOR OUT-OF-CORE FACTORIZATION
!
REAL :: t1, t2, time         ! hwh


it=2*npsym*nrow
nbm=nblsym-1
i1=1
i2=it
i3=i2+1
i4=2*it
time=0.
REWIND iu1
REWIND iu2
DO  kk=1,nop
  ka=(kk-1)*nrow+1
  ifile3=iu1
  ifile4=iu3
  DO  ixblk1=1,nbm
    REWIND iu3
    REWIND iu4
    CALL blckin (a,ifile3,i1,i2,1,17)
    ixbp=ixblk1+1
    DO  ixblk2=ixbp,nblsym
      CALL blckin (a,ifile3,i3,i4,1,18)
      CALL second (t1)
      CALL lfactr (a,nrow,ixblk1,ixblk2,ip(ka))
      CALL second (t2)
      time=time+t2-t1
      IF (ixblk2 == ixbp) CALL blckot (a,iu2,i1,i2,1,19)
      IF (ixblk1 == nbm.AND.ixblk2 == nblsym) ifile4=iu2
      CALL blckot (a,ifile4,i3,i4,1,20)
    END DO
    ifile3=iu3
    ifile4=iu4
    IF ((ixblk1/2)*2 /= ixblk1) CYCLE
    ifile3=iu4
    ifile4=iu3
  END DO
END DO
REWIND iu1
REWIND iu2
REWIND iu3
REWIND iu4
WRITE(3,4)  time
RETURN
!
4     FORMAT (35H cp time taken for factorization = ,1P,e12.5)
END SUBROUTINE facio
!----------------------------------------------------------------------------

SUBROUTINE factr (n,a,ip,ndim)
! ***
!     DOUBLE PRECISION 6/4/85
!
USE nec2dpar
IMPLICIT REAL(NEC2REAL)(a-h,o-z)

INTEGER, INTENT(IN)                      :: n
COMPLEX*16, INTENT(IN OUT)               :: a(ndim,ndim)
INTEGER, INTENT(IN OUT)                  :: ip(ndim)
INTEGER, INTENT(IN)                      :: ndim
! ***
!
!     SUBROUTINE TO FACTOR A MATRIX INTO A UNIT LOWER TRIANGULAR MATRIX
!     AND AN UPPER TRIANGULAR MATRIX USING THE GAUSS-DOOLITTLE ALGORITHM
!     PRESENTED ON PAGES 411-416 OF A. RALSTON--A FIRST COURSE IN
!     NUMERICAL ANALYSIS.  COMMENTS BELOW REFER TO COMMENTS IN RALSTONS
!     TEXT.    (MATRIX TRANSPOSED.
!
COMPLEX*16  d,arj

COMMON /scratm/ d(2*maxseg)
INTEGER :: r,rm1,rp1,pj,pr
!
!     Un-transpose the matrix for Gauss elimination
!
DO  i=2,n
  DO  j=1,i-1
    arj=a(i,j)
    a(i,j)=a(j,i)
    a(j,i)=arj
  END DO
END DO
iflg=0
DO  r=1,n
!
!     STEP 1
!
  DO  k=1,n
    d(k)=a(k,r)
  END DO
!
!     STEPS 2 AND 3
!
  rm1=r-1
  IF (rm1 < 1) GO TO 4
  DO  j=1,rm1
    pj=ip(j)
    arj=d(pj)
    a(j,r)=arj
    d(pj)=d(j)
    jp1=j+1
    DO  i=jp1,n
      d(i)=d(i)-a(i,j)*arj
    END DO
  END DO
  4     CONTINUE
!
!     STEP 4
!
  dmax=dREAL(d(r)*DCONJG(d(r)))
  ip(r)=r
  rp1=r+1
  IF (rp1 > n) GO TO 6
  DO  i=rp1,n
    elmag=dREAL(d(i)*DCONJG(d(i)))
    IF (elmag < dmax) CYCLE
    dmax=elmag
    ip(r)=i
  END DO
  6     CONTINUE
  IF (dmax < 1.d-10) iflg=1
  pr=ip(r)
  a(r,r)=d(pr)
  d(pr)=d(r)
!
!     STEP 5
!
  IF (rp1 > n) GO TO 8
  arj=1./a(r,r)
  DO  i=rp1,n
    a(i,r)=d(i)*arj
  END DO
  8     CONTINUE
  IF (iflg == 0) CYCLE
  WRITE(3,10)  r,dmax
  iflg=0
END DO
RETURN
!
10    FORMAT (1H ,6HPIVOT(,i3,2H)=,1P,e16.8)
END SUBROUTINE factr
!----------------------------------------------------------------------------

SUBROUTINE factrs (np,nrow,a,ip,ix,iu1,iu2,iu3,iu4)
! ***
!     DOUBLE PRECISION 6/4/85
!
USE matpar

IMPLICIT REAL(NEC2REAL)(a-h,o-z)

INTEGER, INTENT(IN)                      :: np
INTEGER, INTENT(IN)                      :: nrow
COMPLEX*16, INTENT(IN OUT)               :: a(1)
INTEGER, INTENT(IN OUT)                  :: ip(nrow)
INTEGER, INTENT(IN OUT)                  :: ix(nrow)
INTEGER, INTENT(IN)                      :: iu1
INTEGER, INTENT(IN)                      :: iu2
INTEGER, INTENT(IN)                      :: iu3
INTEGER, INTENT(IN)                      :: iu4
! ***
!
!     FACTRS, FOR SYMMETRIC STRUCTURE, TRANSFORMS SUBMATRICIES TO FORM
!     MATRICIES OF THE SYMMETRIC MODES AND CALLS ROUTINE TO FACTOR
!     MATRICIES.  IF NO SYMMETRY, THE ROUTINE IS CALLED TO FACTOR THE
!     COMPLETE MATRIX.
!
nop=nrow/np
IF (icase > 2) GO TO 2
DO  kk=1,nop
  ka=(kk-1)*np+1
  CALL factr (np,a(ka),ip(ka),nrow)
END DO
RETURN
2     IF (icase > 3) GO TO 3
!
!     FACTOR SUBMATRICIES, OR FACTOR COMPLETE MATRIX IF NO SYMMETRY
!     EXISTS.
!
CALL facio (a,nrow,nop,ix,iu1,iu2,iu3,iu4)
CALL lunscr (a,nrow,nop,ip,ix,iu2,iu3,iu4)
RETURN
!
!     REWRITE THE MATRICES BY COLUMNS ON TAPE 13
!
3     i2=2*npblk*nrow
REWIND iu2
DO  k=1,nop
  REWIND iu1
  icols=npblk
  ir2=k*np
  ir1=ir2-np+1
  DO  l=1,nbloks
    IF (nbloks == 1.AND.k > 1) GO TO 4
    CALL blckin (a,iu1,1,i2,1,602)
    IF (l == nbloks) icols=nlast
    4     irr1=ir1
    irr2=ir2
    DO  icoldx=1,icols
      WRITE (iu2) (a(i),i=irr1,irr2)
      irr1=irr1+nrow
      irr2=irr2+nrow
    END DO
  END DO
END DO
REWIND iu1
REWIND iu2
IF (icase == 5) GO TO 8
REWIND iu3
irr1=np*np
DO  kk=1,nop
  ir1=1-np
  ir2=0
  DO  i=1,np
    ir1=ir1+np
    ir2=ir2+np
    READ (iu2) (a(j),j=ir1,ir2)
  END DO
  ka=(kk-1)*np+1
  CALL factr (np,a,ip(ka),np)
  WRITE (iu3) (a(i),i=1,irr1)
END DO
REWIND iu2
REWIND iu3
RETURN
8     i2=2*npsym*np
DO  kk=1,nop
  j2=npsym
  DO  l=1,nblsym
    IF (l == nblsym) j2=nlsym
    ir1=1-np
    ir2=0
    DO  j=1,j2
      ir1=ir1+np
      ir2=ir2+np
      READ (iu2) (a(i),i=ir1,ir2)
    END DO
    CALL blckot (a,iu1,1,i2,1,193)
  END DO
END DO
REWIND iu1
CALL facio (a,np,nop,ix,iu1,iu2,iu3,iu4)
CALL lunscr (a,np,nop,ip,ix,iu2,iu3,iu4)
RETURN
END SUBROUTINE factrs

COMPLEX*16 FUNCTION fbar(p)
! ***
!     DOUBLE PRECISION 6/4/85
!
IMPLICIT REAL(NEC2REAL)(a-h,o-z)

COMPLEX*16, INTENT(IN OUT)               :: p
! ***
!
!     FBAR IS SOMMERFELD ATTENUATION FUNCTION FOR NUMERICAL DISTANCE P
!
COMPLEX*16 z,zs,sum,pow,term, fj
DIMENSION fjx(2)
EQUIVALENCE (fj,fjx)
DATA tosp/1.128379167D+0/,accs/1.d-12/,sp/1.772453851D+0/ ,fjx/0.,1./

z=fj*SQRT(p)
IF (ABS(z) > 3.) GO TO 3
!
!     SERIES EXPANSION
!
zs=z*z
sum=z
pow=z
DO  i=1,100
  pow=-pow*zs/dfloat(i)
  term=pow/(2.*i+1.)
  sum=sum+term
  tms=dREAL(term*DCONJG(term))
  sms=dREAL(sum*DCONJG(sum))
  IF (tms/sms < accs) EXIT
END DO
2     fbar=1.-(1.-sum*tosp)*z*EXP(zs)*sp
RETURN
!
!     ASYMPTOTIC EXPANSION
!
3     IF (dREAL(z) >= 0.) GO TO 4
minus=1
z=-z
GO TO 5
4     minus=0
5     zs=.5/(z*z)
sum=(0.,0.)
term=(1.,0.)
DO  i=1,6
  term=-term*(2.*i-1.)*zs
  sum=sum+term
END DO
IF (minus == 1) sum=sum-2.*sp*z*EXP(z*z)
fbar=-sum
RETURN
END FUNCTION fbar
!----------------------------------------------------------------------------

SUBROUTINE fblock (nrow,ncol,imax,irngf,ipsym)
! ***
!     DOUBLE PRECISION 6/4/85
!
USE matpar

IMPLICIT REAL(NEC2REAL)(a-h,o-z)

INTEGER, INTENT(IN OUT)                  :: nrow
INTEGER, INTENT(IN OUT)                  :: ncol
INTEGER, INTENT(IN)                      :: imax
INTEGER, INTENT(IN)                      :: irngf
INTEGER, INTENT(IN OUT)                  :: ipsym
! ***
!     FBLOCK SETS PARAMETERS FOR OUT-OF-CORE SOLUTION FOR THE PRIMARY
!     MATRIX (A)
COMPLEX*16 ssx,deter
COMMON /smat/ ssx(16,16)

imx1=imax-irngf
IF (nrow*ncol > imx1) GO TO 2
nbloks=1
npblk=nrow
nlast=nrow
imat=nrow*ncol
IF (nrow /= ncol) GO TO 1
icase=1
RETURN
1     icase=2
GO TO 5
2     IF (nrow /= ncol) GO TO 3
icase=3
npblk=imax/(2*ncol)
npsym=imx1/ncol
IF (npsym < npblk) npblk=npsym
IF (npblk < 1) GO TO 12
nbloks=(nrow-1)/npblk
nlast=nrow-nbloks*npblk
nbloks=nbloks+1
nblsym=nbloks
npsym=npblk
nlsym=nlast
imat=npblk*ncol
WRITE(3,14)  nbloks,npblk,nlast
GO TO 11
3     npblk=imax/ncol
IF (npblk < 1) GO TO 12
IF (npblk > nrow) npblk=nrow
nbloks=(nrow-1)/npblk
nlast=nrow-nbloks*npblk
nbloks=nbloks+1
WRITE(3,14)  nbloks,npblk,nlast
IF (nrow*nrow > imx1) GO TO 4
icase=4
nblsym=1
npsym=nrow
nlsym=nrow
imat=nrow*nrow
WRITE(3,15)
GO TO 5
4     icase=5
npsym=imax/(2*nrow)
nblsym=imx1/nrow
IF (nblsym < npsym) npsym=nblsym
IF (npsym < 1) GO TO 12
nblsym=(nrow-1)/npsym
nlsym=nrow-nblsym*npsym
nblsym=nblsym+1
WRITE(3,16)  nblsym,npsym,nlsym
imat=npsym*nrow
5     nop=ncol/nrow
IF (nop*nrow /= ncol) GO TO 13
IF (ipsym > 0) GO TO 7
!
!     SET UP SSX MATRIX FOR ROTATIONAL SYMMETRY.
!
phaz=6.2831853072D+0/nop
DO  i=2,nop
  DO  j=i,nop
    arg=phaz*dfloat(i-1)*dfloat(j-1)
    ssx(i,j)=DCMPLX(COS(arg),SIN(arg))
    ssx(j,i)=ssx(i,j)
  END DO
END DO
GO TO 11
!
!     SET UP SSX MATRIX FOR PLANE SYMMETRY
!
7     kk=1
ssx(1,1)=(1.,0.)
IF ((nop == 2).OR.(nop == 4).OR.(nop == 8)) GO TO 8
STOP
8     ka=nop/2
IF (nop == 8) ka=3
DO  k=1,ka
  DO  i=1,kk
    DO  j=1,kk
      deter=ssx(i,j)
      ssx(i,j+kk)=deter
      ssx(i+kk,j+kk)=-deter
      ssx(i+kk,j)=deter
    END DO
  END DO
  kk=kk*2
END DO
11    RETURN
12    WRITE(3,17)  nrow,ncol
STOP
13    WRITE(3,18)  nrow,ncol
STOP
!
14    FORMAT (//' matrix FILE storage -  no. blocks=',i5, &
    ' columns per BLOCK=',i5,' columns in last BLOCK=',i5)
15    FORMAT (' submatricies fit in core')
16    FORMAT (' submatrix partitioning -  no. blocks=',i5, &
    ' columns per BLOCK=',i5,' columns in last BLOCK=',i5)
17    FORMAT (' error - insufficient storage for matrix',2I5)
18    FORMAT (' symmetry error - nrow,ncol=',2I5)
END SUBROUTINE fblock
!----------------------------------------------------------------------------

SUBROUTINE fbngf (neq,neq2,iresrv,ib11,ic11,id11,ix11)
! ***
!     DOUBLE PRECISION 6/4/85
!
USE matpar

IMPLICIT REAL(NEC2REAL)(a-h,o-z)

INTEGER, INTENT(IN OUT)                  :: neq
INTEGER, INTENT(IN OUT)                  :: neq2
INTEGER, INTENT(IN)                      :: iresrv
INTEGER, INTENT(OUT)                     :: ib11
INTEGER, INTENT(OUT)                     :: ic11
INTEGER, INTENT(OUT)                     :: id11
INTEGER, INTENT(OUT)                     :: ix11
! ***
!     FBNGF SETS THE BLOCKING PARAMETERS FOR THE B, C, AND D ARRAYS FOR
!     OUT-OF-CORE STORAGE.

iresx=iresrv-imat
nbln=neq*neq2
ndln=neq2*neq2
nbcd=2*nbln+ndln
IF (nbcd > iresx) GO TO 1
icasx=1
ib11=imat+1
GO TO 2
1     IF (icase < 3) GO TO 3
IF (nbcd > iresrv.OR.nbln > iresx) GO TO 3
icasx=2
ib11=1
2     nbbx=1
npbx=neq
nlbx=neq
nbbl=1
npbl=neq2
nlbl=neq2
GO TO 5
3     ir=iresrv
IF (icase < 3) ir=iresx
icasx=3
IF (ndln > ir) icasx=4
nbcd=2*neq+neq2
npbl=ir/nbcd
nlbl=ir/(2*neq2)
IF (nlbl < npbl) npbl=nlbl
IF (icase < 3) GO TO 4
nlbl=iresx/neq
IF (nlbl < npbl) npbl=nlbl
4     IF (npbl < 1) GO TO 6
nbbl=(neq2-1)/npbl
nlbl=neq2-nbbl*npbl
nbbl=nbbl+1
nbln=neq*npbl
ir=ir-nbln
npbx=ir/neq2
IF (npbx > neq) npbx=neq
nbbx=(neq-1)/npbx
nlbx=neq-nbbx*npbx
nbbx=nbbx+1
ib11=1
IF (icase < 3) ib11=imat+1
5     ic11=ib11+nbln
id11=ic11+nbln
ix11=imat+1
WRITE(3,11)  neq2
IF (icasx == 1) RETURN
WRITE(3,8)  icasx
WRITE(3,9)  nbbx,npbx,nlbx
WRITE(3,10)  nbbl,npbl,nlbl
RETURN
6     WRITE(3,7)  iresrv,imat,neq,neq2
STOP
!
7     FORMAT (' error - insufficient storage for interaction matricies',  &
    '  iresrv,imat,neq,neq2 =',4I5)
8     FORMAT (' FILE storage for NEW matrix sections -  icasx =',i2)
9     FORMAT (' b filled by rows -',15X,'NO. blocks =',i3,3X, &
    'ROWS per BLOCK =',i3,3X,'ROWS in last BLOCK =',i3)
10    FORMAT (' b by columns, c AND d by rows -',2X,'NO. blocks =',i3,  &
    4X,'R/c per BLOCK =',i3,4X,'R/c in last BLOCK =',i3)
11    FORMAT (//,' n.g.f. - NUMBER of NEW unknowns is',i4)
END SUBROUTINE fbngf
!----------------------------------------------------------------------------

SUBROUTINE ffld (thet,phi,eth,eph)
! ***
!     DOUBLE PRECISION 6/4/85
!
USE nec2dpar
USE data
USE gnd
USE crnt
USE angl

IMPLICIT REAL(NEC2REAL)(a-h,o-z)

REAL(NEC2REAL), INTENT(IN OUT)                   :: thet
REAL(NEC2REAL), INTENT(IN OUT)                   :: phi
COMPLEX*16, INTENT(OUT)                  :: eth
COMPLEX*16, INTENT(OUT)                  :: eph

INTEGER                                  :: ip

! ***
!
!     FFLD CALCULATES THE FAR ZONE RADIATED ELECTRIC FIELDS,
!     THE FACTOR EXP(J*K*R)/(R/LAMDA) NOT INCLUDED
!
COMPLEX*16 cix,ciy,ciz,exa, const,ccx,ccy,ccz,cdp
COMPLEX*16 zrsin,rrv,rrh,rrv1,rrh1,rrv2,rrh2,tix,tiy  &
    ,tiz,zscrn,ex,ey,ez,gx,gy,gz
DIMENSION cab(1), sab(1), consx(2)
EQUIVALENCE (cab,alp), (sab,bet), (const,consx)
DATA tp,eta/6.283185308D+0,376.73/
DATA consx/0.,-29.97922085D+0/

phx=-SIN(phi)
phy=COS(phi)
roz=COS(thet)
rozs=roz
thx=roz*phy
thy=-roz*phx
thz=-SIN(thet)
rox=-thz*phy
roy=thz*phx
IF (n == 0) GO TO 20
!
!     LOOP FOR STRUCTURE IMAGE IF ANY
!
DO  k=1,ksymp
!
!     CALCULATION OF REFLECTION COEFFECIENTS
!
  IF (k == 1) GO TO 4
  IF (iperf /= 1) GO TO 1
!
!     FOR PERFECT GROUND
!
  rrv=-(1.,0.)
  rrh=-(1.,0.)
  GO TO 2
!
!     FOR INFINITE PLANAR GROUND
!
  1     zrsin=SQRT(1.-zrati*zrati*thz*thz)
  rrv=-(roz-zrati*zrsin)/(roz+zrati*zrsin)
  rrh=(zrati*roz-zrsin)/(zrati*roz+zrsin)
  2     IF (ifar <= 1) GO TO 3
!
!     FOR THE CLIFF PROBLEM, TWO REFLCTION COEFFICIENTS CALCULATED
!
  rrv1=rrv
  rrh1=rrh
  tthet=TAN(thet)
  IF (ifar == 4) GO TO 3
  zrsin=SQRT(1.-zrati2*zrati2*thz*thz)
  rrv2=-(roz-zrati2*zrsin)/(roz+zrati2*zrsin)
  rrh2=(zrati2*roz-zrsin)/(zrati2*roz+zrsin)
  darg=-tp*2.*ch*roz
  3     roz=-roz
  ccx=cix
  ccy=ciy
  ccz=ciz
  4     cix=(0.,0.)
  ciy=(0.,0.)
  ciz=(0.,0.)
!
!     LOOP OVER STRUCTURE SEGMENTS
!
  DO  i=1,n
    omega=-(rox*cab(i)+roy*sab(i)+roz*salp(i))
    el=pi*si(i)
    sill=omega*el
    top=el+sill
    bot=el-sill
    IF (ABS(omega) < 1.d-7) GO TO 5
    a=2.*SIN(sill)/omega
    GO TO 6
    5     a=(2.-omega*omega*el*el/3.)*el
    6     IF (ABS(top) < 1.d-7) GO TO 7
    too=SIN(top)/top
    GO TO 8
    7     too=1.-top*top/6.
    8     IF (ABS(bot) < 1.d-7) GO TO 9
    boo=SIN(bot)/bot
    GO TO 10
    9     boo=1.-bot*bot/6.
    10    b=el*(boo-too)
    c=el*(boo+too)
    rr=a*air(i)+b*bii(i)+c*cir(i)
    ri=a*aii(i)-b*bir(i)+c*cii(i)
    arg=tp*(x(i)*rox+y(i)*roy+z(i)*roz)
    IF (k == 2.AND.ifar >= 2) GO TO 11
    exa=DCMPLX(COS(arg),SIN(arg))*DCMPLX(rr,ri)
!
!     SUMMATION FOR FAR FIELD INTEGRAL
!
    cix=cix+exa*cab(i)
    ciy=ciy+exa*sab(i)
    ciz=ciz+exa*salp(i)
    CYCLE
!
!     CALCULATION OF IMAGE CONTRIBUTION IN CLIFF AND GROUND SCREEN
!     PROBLEMS.
!
    11    dr=z(i)*tthet
!
!     SPECULAR POINT DISTANCE
!
    d=dr*phy+x(i)
    IF (ifar == 2) GO TO 13
    d=SQRT(d*d+(y(i)-dr*phx)**2)
    IF (ifar == 3) GO TO 13
    IF ((scrwl-d) < 0.) GO TO 12
!
!     RADIAL WIRE GROUND SCREEN REFLECTION COEFFICIENT
!
    d=d+t2
    zscrn=t1*d*LOG(d/t2)
    zscrn=(zscrn*zrati)/(eta*zrati+zscrn)
    zrsin=SQRT(1.-zscrn*zscrn*thz*thz)
    rrv=(roz+zscrn*zrsin)/(-roz+zscrn*zrsin)
    rrh=(zscrn*roz+zrsin)/(zscrn*roz-zrsin)
    GO TO 16
    12    IF (ifar == 4) GO TO 14
    IF (ifar == 5) d=dr*phy+x(i)
    13    IF ((cl-d) <= 0.) GO TO 15
    14    rrv=rrv1
    rrh=rrh1
    GO TO 16
    15    rrv=rrv2
    rrh=rrh2
    arg=arg+darg
    16    exa=DCMPLX(COS(arg),SIN(arg))*DCMPLX(rr,ri)
!
!     CONTRIBUTION OF EACH IMAGE SEGMENT MODIFIED BY REFLECTION COEF. ,
!     FOR CLIFF AND GROUND SCREEN PROBLEMS
!
    tix=exa*cab(i)
    tiy=exa*sab(i)
    tiz=exa*salp(i)
    cdp=(tix*phx+tiy*phy)*(rrh-rrv)
    cix=cix+tix*rrv+cdp*phx
    ciy=ciy+tiy*rrv+cdp*phy
    ciz=ciz-tiz*rrv
  END DO
  IF (k == 1) CYCLE
  IF (ifar >= 2) GO TO 18
!
!     CALCULATION OF CONTRIBUTION OF STRUCTURE IMAGE FOR INFINITE GROUND
!
  cdp=(cix*phx+ciy*phy)*(rrh-rrv)
  cix=ccx+cix*rrv+cdp*phx
  ciy=ccy+ciy*rrv+cdp*phy
  ciz=ccz-ciz*rrv
  CYCLE
  18    cix=cix+ccx
  ciy=ciy+ccy
  ciz=ciz+ccz
END DO
IF (m > 0) GO TO 21
eth=(cix*thx+ciy*thy+ciz*thz)*const
eph=(cix*phx+ciy*phy)*const
RETURN
20    cix=(0.,0.)
ciy=(0.,0.)
ciz=(0.,0.)
21    roz=rozs
!
!     ELECTRIC FIELD COMPONENTS
!
rfl=-1.
DO  ip=1,ksymp
  rfl=-rfl
  rrz=roz*rfl
  CALL fflds (rox,roy,rrz,cur(n+1),gx,gy,gz)
  IF (ip == 2) GO TO 22
  ex=gx
  ey=gy
  ez=gz
  CYCLE
  22    IF (iperf /= 1) GO TO 23
  gx=-gx
  gy=-gy
  gz=-gz
  GO TO 24
  23    rrv=SQRT(1.-zrati*zrati*thz*thz)
  rrh=zrati*roz
  rrh=(rrh-rrv)/(rrh+rrv)
  rrv=zrati*rrv
  rrv=-(roz-rrv)/(roz+rrv)
  eth=(gx*phx+gy*phy)*(rrh-rrv)
  gx=gx*rrv+eth*phx
  gy=gy*rrv+eth*phy
  gz=gz*rrv
  24    ex=ex+gx
  ey=ey+gy
  ez=ez-gz
END DO
ex=ex+cix*const
ey=ey+ciy*const
ez=ez+ciz*const
eth=ex*thx+ey*thy+ez*thz
eph=ex*phx+ey*phy
RETURN
END SUBROUTINE ffld
!----------------------------------------------------------------------------
!
!     CALCULATES THE XYZ COMPONENTS OF THE ELECTRIC FIELD DUE TO
!     SURFACE CURRENTS
!
SUBROUTINE fflds (rox,roy,roz,scur,ex,ey,ez)

    USE nec2dpar
    USE data

    IMPLICIT REAL(NEC2REAL)(a-h,o-z)

    REAL(NEC2REAL), INTENT(IN)                       :: rox
    REAL(NEC2REAL), INTENT(IN)                       :: roy
    REAL(NEC2REAL), INTENT(IN)                       :: roz
    COMPLEX*16, INTENT(IN)                   :: scur(1)
    COMPLEX*16, INTENT(OUT)                  :: ex
    COMPLEX*16, INTENT(OUT)                  :: ey
    COMPLEX*16, INTENT(OUT)                  :: ez

    COMPLEX*16 ct,cons

    DIMENSION xs(1), ys(1), zs(1), s(1), consx(2)

    EQUIVALENCE (xs,x), (ys,y), (zs,z), (s,bi), (cons,consx)

    DATA tpi/6.283185308D+0/,consx/0.,188.365/

    ex=(0.,0.)
    ey=(0.,0.)
    ez=(0.,0.)
    i=ld+1
    DO  j=1,m
      i=i-1
      arg=tpi*(rox*xs(i)+roy*ys(i)+roz*zs(i))
      ct=DCMPLX(COS(arg)*s(i),SIN(arg)*s(i))
      k=3*j
      ex=ex+scur(k-2)*ct
      ey=ey+scur(k-1)*ct
      ez=ez+scur(k)*ct
    END DO
    ct=rox*ex+roy*ey+roz*ez
    ex=cons*(ct*rox-ex)
    ey=cons*(ct*roy-ey)
    ez=cons*(ct*roz-ez)
    RETURN
END SUBROUTINE fflds
!----------------------------------------------------------------------------

SUBROUTINE gf (zk,co,si)
! ***
!     DOUBLE PRECISION 6/4/85
!
IMPLICIT REAL(NEC2REAL)(a-h,o-z)

REAL(NEC2REAL), INTENT(IN)                         :: zk
REAL(NEC2REAL), INTENT(OUT)                        :: co
REAL(NEC2REAL), INTENT(OUT)                        :: si
! ***
!
!     GF COMPUTES THE INTEGRAND EXP(JKR)/(KR) FOR NUMERICAL INTEGRATION.
!
COMMON /tmi/ zpk,rkb2,ij

zdk=zk-zpk
rk=SQRT(rkb2+zdk*zdk)
si=SIN(rk)/rk
IF (ij == 0) THEN
  GO TO     2
END IF
1     co=COS(rk)/rk
RETURN
2     IF (rk < .2) GO TO 3
co=(COS(rk)-1.)/rk
RETURN
3     rks=rk*rk
co=((-1.38888889D-3*rks+4.16666667D-2)*rks-.5)*rk
RETURN
END SUBROUTINE gf

!----------------------------------------------------------------------------

SUBROUTINE gfil (iprt)
! ***
!     DOUBLE PRECISION 6/4/85
!
USE nec2dpar
USE somset
USE matpar
USE cmb
USE data
USE save
USE csave
USE gnd
USE angl
USE zload

IMPLICIT REAL(NEC2REAL)(a-h,o-z)

INTEGER, INTENT(IN OUT)                  :: iprt
!INTEGER, PARAMETER :: iresrv=maxmat**2
! ***
!
!     GFIL READS THE N.G.F. FILE
!
COMPLEX*16 ssx

COMMON /smat/ ssx(16,16)

CHARACTER (LEN=80) :: ngfnam
COMMON /ngfnam/ ngfnam
!
!*** ERROR CORRECTED 11/20/89 *******************************
DIMENSION t2x(1),t2y(1),t2z(1)
EQUIVALENCE (t2x,icon1),(t2y,icon2),(t2z,itag)
!***
DATA igfl/20/

OPEN(UNIT=igfl,FILE=ngfnam,FORM='UNFORMATTED',STATUS='OLD',ERR=30)
GO TO 31

30    WRITE (3, '(2A)') 'ERROR opening NGF-file : ',ngfnam
STOP

31    REWIND igfl
READ (igfl) n1,np,m1,mp,wlam,fmhz,ipsym,ksymp,iperf,nradl,epsr,sig  &
    ,scrwlt,scrwrt,nlodf,kcom
n=n1
m=m1
n2=n1+1
m2=m1+1
IF (n1 == 0) GO TO 2
!     READ SEG. DATA AND CONVERT BACK TO END COORD. IN UNITS OF METERS
READ (igfl) (x(i),i=1,n1),(y(i),i=1,n1),(z(i),i=1,n1)
READ (igfl) (si(i),i=1,n1),(bi(i),i=1,n1),(alp(i),i=1,n1)
READ (igfl) (bet(i),i=1,n1),(salp(i),i=1,n1)
READ (igfl) (icon1(i),i=1,n1),(icon2(i),i=1,n1)
READ (igfl) (itag(i),i=1,n1)
IF (nlodf /= 0) READ (igfl) (zarray(i),i=1,n1)
DO  i=1,n1
  xi=x(i)*wlam
  yi=y(i)*wlam
  zi=z(i)*wlam
  dx=si(i)*.5*wlam
  x(i)=xi-alp(i)*dx
  y(i)=yi-bet(i)*dx
  z(i)=zi-salp(i)*dx
  si(i)=xi+alp(i)*dx
  alp(i)=yi+bet(i)*dx
  bet(i)=zi+salp(i)*dx
  bi(i)=bi(i)*wlam
END DO
2     IF (m1 == 0) GO TO 4
j=ld-m1+1
!     READ PATCH DATA AND CONVERT TO METERS
READ (igfl) (x(i),i=j,ld),(y(i),i=j,ld),(z(i),i=j,ld)
READ (igfl) (si(i),i=j,ld),(bi(i),i=j,ld),(alp(i),i=j,ld)
READ (igfl) (bet(i),i=j,ld),(salp(i),i=j,ld)
!*** ERROR CORRECTED 11/20/89 *******************************
READ (igfl) (t2x(i),i=j,ld),(t2y(i),i=j,ld)
READ (igfl) (t2z(i),i=j,ld)
!      READ (IGFL) (ICON1(I),I=J,LD),(ICON2(I),I=J,LD)
!      READ (IGFL) (ITAG(I),I=J,LD)
!
dx=wlam*wlam
DO  i=j,ld
  x(i)=x(i)*wlam
  y(i)=y(i)*wlam
  z(i)=z(i)*wlam
  bi(i)=bi(i)*dx
END DO
4     READ (igfl) icase,nbloks,npblk,nlast,nblsym,npsym,nlsym,imat
IF (iperf == 2) READ (igfl) ar1,ar2,ar3,epscf,dxa,dya,xsa,ysa,nxa, nya
neq=n1+2*m1
npeq=np+2*mp
nop=neq/npeq
IF (nop > 1) READ (igfl) ((ssx(i,j),i=1,nop),j=1,nop)
READ (igfl) (ip(i),i=1,neq),com
!     READ MATRIX A AND WRITE TAPE13 FOR OUT OF CORE
IF (icase > 2) GO TO 5
iout=neq*npeq
READ (igfl) (cm(i),i=1,iout)
GO TO 10
5     REWIND 13
IF (icase /= 4) GO TO 7
iout=npeq*npeq
DO  k=1,nop
  READ (igfl) (cm(j),j=1,iout)
  WRITE (13) (cm(j),j=1,iout)
END DO
GO TO 9
7     iout=npsym*npeq*2
nbl2=2*nblsym
DO  iop=1,nop
  DO  i=1,nbl2
    CALL blckin (cm,igfl,1,iout,1,206)
    CALL blckot (cm,13,1,iout,1,205)
  END DO
END DO
9     REWIND 13
10    REWIND igfl
!     WRITE(3,N) G.F. HEADING
WRITE(3,16)
WRITE(3,14)
WRITE(3,14)
WRITE(3,17)
WRITE(3,18)  n1,m1
IF (nop > 1) WRITE(3,19)  nop
WRITE(3,20)  imat,icase
IF (icase < 3) GO TO 11
nbl2=neq*npeq
WRITE(3,21)  nbl2
11    WRITE(3,22)  fmhz
IF (ksymp == 2.AND.iperf == 1) WRITE(3,23)
IF (ksymp == 2.AND.iperf == 0) WRITE(3,27)
IF (ksymp == 2.AND.iperf == 2) WRITE(3,28)
IF (ksymp == 2.AND.iperf /= 1) WRITE(3,24)  epsr,sig
WRITE(3,17)
DO  j=1,kcom
  WRITE(3,15)  (com(i,j),i=1,19)
END DO
WRITE(3,17)
WRITE(3,14)
WRITE(3,14)
WRITE(3,16)
IF (iprt == 0) RETURN
WRITE(3,25)
DO  i=1,n1
  WRITE(3,26)  i,x(i),y(i),z(i),si(i),alp(i),bet(i)
END DO
RETURN
!
14    FORMAT (5X,'**************************************************',  &
    '**********************************')
15    FORMAT (5X,'** ',19A4,' **')
16    FORMAT (////)
17    FORMAT (5X,'**',80X,'**')
18    FORMAT (5X,"** NUMERICAL GREEN'S FUNCTION",53X,2H**,/,5X, &
    '** NO. SEGMENTS =',I4,10X,'NO. PATCHES =',I4,34X,2H**)
19    FORMAT (5X,'** no. symmetric sections =',i4,51X,2H**)
20    FORMAT (5X,'** n.g.f. matrix -  core storage =',i7, &
    ' COMPLEX NUMBERS,  CASE',i2,16X,'**')
21    FORMAT (5X,'**',19X,'MATRIX SIZE =',i7,' COMPLEX NUMBERS',25X,'**')
22    FORMAT (5X,'** frequency =',1P,e12.5,' mhz.',51X,'**')
23    FORMAT (5X,'** perfect ground',65X,'**')
24    FORMAT (5X,'** ground PARAMETERs - dielectric constant =',1P,  &
    e12.5,26X,'**',/,5X,'**',21X,'CONDUCTIVITY =',e12.5,' mhos/m.', 25X,'**')
25    FORMAT (39X,"NUMERICAL GREEN'S FUNCTION DATA",/,41X, &
    'COORDINATES OF SEGMENT ENDS',/,51X,'(METERS)',/,5X,'SEG.',11X, &
    '- - - END ONE - - -',26X,'- - - END TWO - - -',/,6X,'NO.', &
    6X,'X',14X,'Y',14X,'Z',14X,'X',14X,'Y',14X,'Z')
26    FORMAT (1X,i7,1P,6E15.6)
27    FORMAT (5X,'** finite ground.  reflection coefficient approximation',27X,'**')
28    FORMAT (5X,'** finite ground.  sommerfeld solution',44X,'**')
END SUBROUTINE gfil
!----------------------------------------------------------------------------

SUBROUTINE gfld (rho,phi,rz,eth,epi,erd,ux,ksymp)
! ***
!     DOUBLE PRECISION 6/4/85
!
USE nec2dpar
USE data
USE crnt
USE angl
USE gwav

IMPLICIT REAL(NEC2REAL)(a-h,o-z)

REAL(NEC2REAL), INTENT(IN)                       :: rho
REAL(NEC2REAL), INTENT(IN OUT)                   :: phi
REAL(NEC2REAL), INTENT(IN)                       :: rz
COMPLEX*16, INTENT(OUT)                  :: eth
COMPLEX*16, INTENT(OUT)                  :: epi
COMPLEX*16, INTENT(OUT)                  :: erd
COMPLEX*16, INTENT(IN)                   :: ux
INTEGER, INTENT(IN OUT)                  :: ksymp
! ***
!
!     GFLD COMPUTES THE RADIATED FIELD INCLUDING GROUND WAVE.
!
COMPLEX*16 cix,ciy,ciz,exa,erv,ezv,erh,eph
COMPLEX*16 ezh,ex,ey
DIMENSION cab(1), sab(1)
EQUIVALENCE (cab(1),alp(1)), (sab(1),bet(1))
DATA tp/6.283185308D+0/

r=SQRT(rho*rho+rz*rz)
IF (ksymp == 1) GO TO 1
IF (ABS(ux) > .5) GO TO 1
IF (r > 1.e5) GO TO 1
GO TO 4
!
!     COMPUTATION OF SPACE WAVE ONLY
!
1     IF (rz < 1.d-20) GO TO 2
thet=ATAN(rho/rz)
GO TO 3
2     thet=pi*.5
3     CALL ffld (thet,phi,eth,epi)
arg=-tp*r
exa=DCMPLX(COS(arg),SIN(arg))/r
eth=eth*exa
epi=epi*exa
erd=(0.,0.)
RETURN
!
!     COMPUTATION OF SPACE AND GROUND WAVES.
!
4     u=ux
u2=u*u
phx=-SIN(phi)
phy=COS(phi)
rx=rho*phy
ry=-rho*phx
cix=(0.,0.)
ciy=(0.,0.)
ciz=(0.,0.)
!
!     SUMMATION OF FIELD FROM INDIVIDUAL SEGMENTS
!
DO  i=1,n
  dx=cab(i)
  dy=sab(i)
  dz=salp(i)
  rix=rx-x(i)
  riy=ry-y(i)
  rhs=rix*rix+riy*riy
  rhp=SQRT(rhs)
  IF (rhp < 1.d-6) GO TO 5
  rhx=rix/rhp
  rhy=riy/rhp
  GO TO 6
  5     rhx=1.
  rhy=0.
  6     calp=1.-dz*dz
  IF (calp < 1.d-6) GO TO 7
  calp=SQRT(calp)
  cbet=dx/calp
  sbet=dy/calp
  cph=rhx*cbet+rhy*sbet
  sph=rhy*cbet-rhx*sbet
  GO TO 8
  7     cph=rhx
  sph=rhy
  8     el=pi*si(i)
  rfl=-1.
!
!     INTEGRATION OF (CURRENT)*(PHASE FACTOR) OVER SEGMENT AND IMAGE FOR
!     CONSTANT, SINE, AND COSINE CURRENT DISTRIBUTIONS
!
  DO  k=1,2
    rfl=-rfl
    riz=rz-z(i)*rfl
    rxyz=SQRT(rix*rix+riy*riy+riz*riz)
    rnx=rix/rxyz
    rny=riy/rxyz
    rnz=riz/rxyz
    omega=-(rnx*dx+rny*dy+rnz*dz*rfl)
    sill=omega*el
    top=el+sill
    bot=el-sill
    IF (ABS(omega) < 1.d-7) GO TO 9
    a=2.*SIN(sill)/omega
    GO TO 10
    9     a=(2.-omega*omega*el*el/3.)*el
    10    IF (ABS(top) < 1.d-7) GO TO 11
    too=SIN(top)/top
    GO TO 12
    11    too=1.-top*top/6.
    12    IF (ABS(bot) < 1.d-7) GO TO 13
    boo=SIN(bot)/bot
    GO TO 14
    13    boo=1.-bot*bot/6.
    14    b=el*(boo-too)
    c=el*(boo+too)
    rr=a*air(i)+b*bii(i)+c*cir(i)
    ri=a*aii(i)-b*bir(i)+c*cii(i)
    arg=tp*(x(i)*rnx+y(i)*rny+z(i)*rnz*rfl)
    exa=DCMPLX(COS(arg),SIN(arg))*DCMPLX(rr,ri)/tp
    IF (k == 2) GO TO 15
    xx1=exa
    r1=rxyz
    zmh=riz
    CYCLE
    15    xx2=exa
    r2=rxyz
    zph=riz
  END DO
!
!     CALL SUBROUTINE TO COMPUTE THE FIELD OF SEGMENT INCLUDING GROUND
!     WAVE.
!
  CALL gwave (erv,ezv,erh,ezh,eph)
  erh=erh*cph*calp+erv*dz
  eph=eph*sph*calp
  ezh=ezh*cph*calp+ezv*dz
  ex=erh*rhx-eph*rhy
  ey=erh*rhy+eph*rhx
  cix=cix+ex
  ciy=ciy+ey
  ciz=ciz+ezh
END DO
arg=-tp*r
exa=DCMPLX(COS(arg),SIN(arg))
cix=cix*exa
ciy=ciy*exa
ciz=ciz*exa
rnx=rx/r
rny=ry/r
rnz=rz/r
thx=rnz*phy
thy=-rnz*phx
thz=-rho/r
eth=cix*thx+ciy*thy+ciz*thz
epi=cix*phx+ciy*phy
erd=cix*rnx+ciy*rny+ciz*rnz
RETURN
END SUBROUTINE gfld

!----------------------------------------------------------------------------

SUBROUTINE gfout
! ***
!     DOUBLE PRECISION 6/4/85
!
USE nec2dpar
USE somset
USE matpar
USE cmb
USE data
USE save
USE csave
USE gnd
USE angl
USE zload

!PARAMETER (iresrv=maxmat**2)
IMPLICIT REAL(NEC2REAL)(a-h,o-z)
! ***
!
!     WRITE N.G.F. FILE
!
COMPLEX*16 ssx

!COMMON /ggrid/ ar1(11,10,4),ar2(17,5,4),ar3(9,8,4),epscf,dxa(3), &
!    dya(3),xsa(3),ysa(3),nxa(3),nya(3)
COMMON /smat/ ssx(16,16)

CHARACTER (LEN=80) :: ngfnam
COMMON /ngfnam/ ngfnam
!
!*** ERROR CORRECTED 11/20/89 *******************************
DIMENSION t2x(1),t2y(1),t2z(1)
EQUIVALENCE (t2x,icon1),(t2y,icon2),(t2z,itag)
!***
DATA igfl/20/

OPEN(UNIT=igfl,FILE=ngfnam,FORM='UNFORMATTED',STATUS='UNKNOWN')

neq=n+2*m
npeq=np+2*mp
nop=neq/npeq
WRITE (igfl) n,np,m,mp,wlam,fmhz,ipsym,ksymp,iperf,nradl,epsr,  &
    sig,scrwlt,scrwrt,nload,kcom
IF (n == 0) GO TO 1
WRITE (igfl) (x(i),i=1,n),(y(i),i=1,n),(z(i),i=1,n)
WRITE (igfl) (si(i),i=1,n),(bi(i),i=1,n),(alp(i),i=1,n)
WRITE (igfl) (bet(i),i=1,n),(salp(i),i=1,n)
WRITE (igfl) (icon1(i),i=1,n),(icon2(i),i=1,n)
WRITE (igfl) (itag(i),i=1,n)
IF (nload > 0) WRITE (igfl) (zarray(i),i=1,n)
1     IF (m == 0) GO TO 2
j=ld-m+1
WRITE (igfl) (x(i),i=j,ld),(y(i),i=j,ld),(z(i),i=j,ld)
WRITE (igfl) (si(i),i=j,ld),(bi(i),i=j,ld),(alp(i),i=j,ld)
WRITE (igfl) (bet(i),i=j,ld),(salp(i),i=j,ld)
!
!*** ERROR CORRECTED 11/20/89 *******************************

WRITE (igfl) (t2x(i),i=j,ld),(t2y(i),i=j,ld)
WRITE (igfl) (t2z(i),i=j,ld)
!      WRITE (IGFL) (ICON1(I),I=J,LD),(ICON2(I),I=J,LD)
!      WRITE (IGFL) (ITAG(I),I=J,LD)
!
2     WRITE (igfl) icase,nbloks,npblk,nlast,nblsym,npsym,nlsym,imat
IF (iperf == 2) WRITE (igfl) ar1,ar2,ar3,epscf,dxa,dya,xsa,ysa,nxa ,nya
IF (nop > 1) WRITE (igfl) ((ssx(i,j),i=1,nop),j=1,nop)
WRITE (igfl) (ip(i),i=1,neq),com
IF (icase > 2) GO TO 3
iout=neq*npeq
WRITE (igfl) (cm(i),i=1,iout)
GO TO 12
3     IF (icase /= 4) GO TO 5
REWIND 13
i=npeq*npeq
DO  k=1,nop
  READ (13) (cm(j),j=1,i)
  WRITE (igfl) (cm(j),j=1,i)
END DO
REWIND 13
GO TO 12
5     REWIND 13
REWIND 14
IF (icase == 5) GO TO 8
iout=npblk*neq*2
DO  i=1,nbloks
  CALL blckin (cm,13,1,iout,1,201)
  CALL blckot (cm,igfl,1,iout,1,202)
END DO
DO  i=1,nbloks
  CALL blckin (cm,14,1,iout,1,203)
  CALL blckot (cm,igfl,1,iout,1,204)
END DO
GO TO 12
8     iout=npsym*npeq*2
DO  iop=1,nop
  DO  i=1,nblsym
    CALL blckin (cm,13,1,iout,1,205)
    CALL blckot (cm,igfl,1,iout,1,206)
  END DO
  DO  i=1,nblsym
    CALL blckin (cm,14,1,iout,1,207)
    CALL blckot (cm,igfl,1,iout,1,208)
  END DO
END DO
REWIND 13
REWIND 14
12    REWIND igfl
WRITE(3,13)  igfl,imat
RETURN
!
13    FORMAT (///," ****NUMERICAL GREEN'S FUNCTION FILE ON TAPE",i3,  &
    " ****",/,5X,"MATRIX STORAGE -",i7," COMPLEX NUMBERS",///)
END SUBROUTINE gfout
!----------------------------------------------------------------------------

SUBROUTINE gh (zk,hr,hi)
! ***
!     DOUBLE PRECISION 6/4/85
!
IMPLICIT REAL(NEC2REAL)(a-h,o-z)

REAL(NEC2REAL), INTENT(IN)                         :: zk
REAL(NEC2REAL), INTENT(OUT)                        :: hr
REAL(NEC2REAL), INTENT(OUT)                        :: hi
! ***
!     INTEGRAND FOR H FIELD OF A WIRE
COMMON /tmh/ zpk,rhks

rs=zk-zpk
rs=rhks+rs*rs
r=SQRT(rs)
ckr=COS(r)
skr=SIN(r)
rr2=1./rs
rr3=rr2/r
hr=skr*rr2+ckr*rr3
hi=ckr*rr2-skr*rr3
RETURN
END SUBROUTINE gh

!----------------------------------------------------------------------------

SUBROUTINE gwave (erv,ezv,erh,ezh,eph)
! ***
!     DOUBLE PRECISION 6/4/85
!
USE gwav

IMPLICIT REAL(NEC2REAL)(a-h,o-z)

COMPLEX*16, INTENT(OUT)                  :: erv
COMPLEX*16, INTENT(OUT)                  :: ezv
COMPLEX*16, INTENT(OUT)                  :: erh
COMPLEX*16, INTENT(OUT)                  :: ezh
COMPLEX*16, INTENT(OUT)                  :: eph
! ***
!
!     GWAVE COMPUTES THE ELECTRIC FIELD, INCLUDING GROUND WAVE, OF A
!     CURRENT ELEMENT OVER A GROUND PLANE USING FORMULAS OF K.A. NORTON
!     (PROC. IRE, SEPT., 1937, PP.1203,1236.)
!
COMPLEX*16 fj,tpj,rk1,rk2,t1,t2,t3,t4,p1,rv,omr,w,f,q1,rh,v,g  &
    ,xr1,xr2,x1,x2,x3,x4,x5,x6,x7,econ,fbar

DIMENSION fjx(2), tpjx(2), econx(2)
EQUIVALENCE (fj,fjx), (tpj,tpjx), (econ,econx)
DATA fjx/0.,1./,tpjx/0.,6.283185308D+0/
DATA econx/0.,-188.367/

sppp=zmh/r1
sppp2=sppp*sppp
cppp2=1.-sppp2
IF (cppp2 < 1.d-20) cppp2=1.d-20
cppp=SQRT(cppp2)
spp=zph/r2
spp2=spp*spp
cpp2=1.-spp2
IF (cpp2 < 1.d-20) cpp2=1.d-20
cpp=SQRT(cpp2)
rk1=-tpj*r1
rk2=-tpj*r2
t1=1.-u2*cpp2
t2=SQRT(t1)
t3=(1.-1./rk1)/rk1
t4=(1.-1./rk2)/rk2
p1=rk2*u2*t1/(2.*cpp2)
rv=(spp-u*t2)/(spp+u*t2)
omr=1.-rv
w=1./omr
w=(4.,0.)*p1*w*w
f=fbar(w)
q1=rk2*t1/(2.*u2*cpp2)
rh=(t2-u*spp)/(t2+u*spp)
v=1./(1.+rh)
v=(4.,0.)*q1*v*v
g=fbar(v)
xr1=xx1/r1
xr2=xx2/r2
x1=cppp2*xr1
x2=rv*cpp2*xr2
x3=omr*cpp2*f*xr2
x4=u*t2*spp*2.*xr2/rk2
x5=xr1*t3*(1.-3.*sppp2)
x6=xr2*t4*(1.-3.*spp2)
ezv=(x1+x2+x3-x4-x5-x6)*econ
x1=sppp*cppp*xr1
x2=rv*spp*cpp*xr2
x3=cpp*omr*u*t2*f*xr2
x4=spp*cpp*omr*xr2/rk2
x5=3.*sppp*cppp*t3*xr1
x6=cpp*u*t2*omr*xr2/rk2*.5
x7=3.*spp*cpp*t4*xr2
erv=-(x1+x2-x3+x4-x5+x6-x7)*econ
ezh=-(x1-x2+x3-x4-x5-x6+x7)*econ
x1=sppp2*xr1
x2=rv*spp2*xr2
x4=u2*t1*omr*f*xr2
x5=t3*(1.-3.*cppp2)*xr1
x6=t4*(1.-3.*cpp2)*(1.-u2*(1.+rv)-u2*omr*f)*xr2
x7=u2*cpp2*omr*(1.-1./rk2)*(f*(u2*t1-spp2-1./rk2)+1./rk2)*xr2
erh=(x1-x2-x4-x5+x6+x7)*econ
x1=xr1
x2=rh*xr2
x3=(rh+1.)*g*xr2
x4=t3*xr1
x5=t4*(1.-u2*(1.+rv)-u2*omr*f)*xr2
x6=.5*u2*omr*(f*(u2*t1-spp2-1./rk2)+1./rk2)*xr2/rk2
eph=-(x1-x2+x3-x4+x5+x6)*econ
RETURN
END SUBROUTINE gwave
!----------------------------------------------------------------------------

SUBROUTINE gx (zz,rh,xk,gz,gzp)
! ***
!     DOUBLE PRECISION 6/4/85
!
IMPLICIT REAL(NEC2REAL)(a-h,o-z)

REAL(NEC2REAL), INTENT(IN)                         :: zz
REAL(NEC2REAL), INTENT(IN)                         :: rh
REAL(NEC2REAL), INTENT(IN)                         :: xk
COMPLEX*16, INTENT(OUT)                  :: gz
COMPLEX*16, INTENT(OUT)                  :: gzp
! ***
!     SEGMENT END CONTRIBUTIONS FOR THIN WIRE APPROX.


r2=zz*zz+rh*rh
r=SQRT(r2)
rk=xk*r
gz=DCMPLX(COS(rk),-SIN(rk))/r
gzp=-DCMPLX(1.d+0,rk)*gz/r2
RETURN
END SUBROUTINE gx
!----------------------------------------------------------------------------

SUBROUTINE gxx (zz,rh,a,a2,xk,ira,g1,g1p,g2,g2p,g3,gzp)
! ***
!     DOUBLE PRECISION 6/4/85
!
IMPLICIT REAL(NEC2REAL)(a-h,o-z)

REAL(NEC2REAL), INTENT(IN)                         :: zz
REAL(NEC2REAL), INTENT(IN)                         :: rh
REAL(NEC2REAL), INTENT(IN)                         :: a
REAL(NEC2REAL), INTENT(IN)                         :: a2
REAL(NEC2REAL), INTENT(IN)                         :: xk
INTEGER, INTENT(IN OUT)                  :: ira
COMPLEX*16, INTENT(OUT)                  :: g1
COMPLEX*16, INTENT(OUT)                  :: g1p
COMPLEX*16, INTENT(OUT)                  :: g2
COMPLEX*16, INTENT(OUT)                  :: g2p
COMPLEX*16, INTENT(OUT)                  :: g3
COMPLEX*16, INTENT(OUT)                  :: gzp
! ***
!     SEGMENT END CONTRIBUTIONS FOR EXT. THIN WIRE APPROX.
COMPLEX*16 gz,c1,c2,c3

r2=zz*zz+rh*rh
r=SQRT(r2)
r4=r2*r2
rk=xk*r
rk2=rk*rk
rh2=rh*rh
t1=.25*a2*rh2/r4
t2=.5*a2/r2
c1=DCMPLX(1.d+0,rk)
c2=3.*c1-rk2
c3=DCMPLX(6.d+0,rk)*rk2-15.*c1
gz=DCMPLX(COS(rk),-SIN(rk))/r
g2=gz*(1.+t1*c2)
g1=g2-t2*c1*gz
gz=gz/r2
g2p=gz*(t1*c3-c1)
gzp=t2*c2*gz
g3=g2p+gzp
g1p=g3*zz
IF (ira == 1) GO TO 2
g3=(g3+gzp)*rh
gzp=-zz*c1*gz
IF (rh > 1.d-10) GO TO 1
g2=0.
g2p=0.
RETURN
1     g2=g2/rh
g2p=g2p*zz/rh
RETURN
2     t2=.5*a
g2=-t2*c1*gz
g2p=t2*gz*c2/r2
g3=rh2*g2p-a*gz*c1
g2p=g2p*zz
gzp=-zz*c1*gz
RETURN
END SUBROUTINE gxx
!----------------------------------------------------------------------------

SUBROUTINE helix(s,hl,a1,b1,a2,b2,rad,ns,itg)
! ***
!     DOUBLE PRECISION 6/4/85
!
USE nec2dpar
USE data

IMPLICIT REAL(NEC2REAL)(a-h,o-z)

REAL(NEC2REAL), INTENT(IN OUT)                     :: s
REAL(NEC2REAL), INTENT(IN OUT)                     :: hl
REAL(NEC2REAL), INTENT(IN)                         :: a1
REAL(NEC2REAL), INTENT(OUT)                        :: b1
REAL(NEC2REAL), INTENT(IN)                         :: a2
REAL(NEC2REAL), INTENT(OUT)                        :: b2
REAL(NEC2REAL), INTENT(IN)                         :: rad
INTEGER, INTENT(IN)                      :: ns
INTEGER, INTENT(IN)                      :: itg
! ***
!     SUBROUTINE HELIX GENERATES SEGMENT GEOMETRY DATA FOR A HELIX OF NS
!     SEGMENTS
DIMENSION x2(1),y2(1),z2(1)
EQUIVALENCE (x2(1),si(1)), (y2(1),alp(1)), (z2(1),bet(1))

ist=n+1
n=n+ns
np=n
mp=m
ipsym=0
IF(ns < 1) RETURN
turns=ABS(hl/s)
zinc=ABS(hl/ns)
z(ist)=0.
DO  i=ist,n
  bi(i)=rad
  itag(i)=itg
  IF(i /= ist) z(i)=z(i-1)+zinc
  z2(i)=z(i)+zinc
  IF(a2 /= a1) GO TO 10
  IF(b1 == 0) b1=a1
  x(i)=a1*COS(2.*pi*z(i)/s)
  y(i)=b1*SIN(2.*pi*z(i)/s)
  x2(i)=a1*COS(2.*pi*z2(i)/s)
  y2(i)=b1*SIN(2.*pi*z2(i)/s)
  GO TO 20
  10    IF(b2 == 0) b2=a2
  x(i)=(a1+(a2-a1)*z(i)/ABS(hl))*COS(2.*pi*z(i)/s)
  y(i)=(b1+(b2-b1)*z(i)/ABS(hl))*SIN(2.*pi*z(i)/s)
  x2(i)=(a1+(a2-a1)*z2(i)/ABS(hl))*COS(2.*pi*z2(i)/s)
  y2(i)=(b1+(b2-b1)*z2(i)/ABS(hl))*SIN(2.*pi*z2(i)/s)
  20    IF(hl > 0) CYCLE
  copy=x(i)
  x(i)=y(i)
  y(i)=copy
  copy=x2(i)
  x2(i)=y2(i)
  y2(i)=copy
END DO
IF(a2 == a1) GO TO 21
sangle=ATAN(a2/(ABS(hl)+(ABS(hl)*a1)/(a2-a1)))
WRITE(3,104)  sangle
104   FORMAT(5X,'THE CONE ANGLE OF THE SPIRAL IS',f10.4)
RETURN
21    IF(a1 /= b1) GO TO 30
hdia=2.*a1
turn=hdia*pi
pitch=ATAN(s/(pi*hdia))
turn=turn/COS(pitch)
pitch=180.*pitch/pi
GO TO 40
30    IF(a1 < b1) GO TO 34
hmaj=2.*a1
hmin=2.*b1
GO TO 35
34    hmaj=2.*b1
hmin=2.*a1
35    hdia=SQRT((hmaj**2+hmin**2)/2*hmaj)
turn=2.*pi*hdia
pitch=(180./pi)*ATAN(s/(pi*hdia))
40    WRITE(3,105) pitch,turn
105   FORMAT(5X,'THE PITCH ANGLE IS',f10.4/5X,'THE LENGTH OF WIRE/TURN I  &
    S',f10.4)
RETURN
END SUBROUTINE helix
!----------------------------------------------------------------------------

SUBROUTINE hfk (el1,el2,rhk,zpkx,sgr,sgi)
! ***
!     DOUBLE PRECISION 6/4/85
!
IMPLICIT REAL(NEC2REAL)(a-h,o-z)

REAL(NEC2REAL), INTENT(IN)                         :: el1
REAL(NEC2REAL), INTENT(IN)                         :: el2
REAL(NEC2REAL), INTENT(IN)                         :: rhk
REAL(NEC2REAL), INTENT(IN)                         :: zpkx
REAL(NEC2REAL), INTENT(OUT)                        :: sgr
REAL(NEC2REAL), INTENT(OUT)                        :: sgi
! ***
!     HFK COMPUTES THE H FIELD OF A UNIFORM CURRENT FILAMENT BY
!     NUMERICAL INTEGRATION
COMMON /tmh/ zpk,rhks
DATA nx,nm,nts,rx/1,65536,4,1.d-4/

zpk=zpkx
rhks=rhk*rhk
z=el1
ze=el2
s=ze-z
ep=s/(10.*nm)
zend=ze-ep
sgr=0.0
sgi=0.0
ns=nx
nt=0
CALL gh (z,g1r,g1i)
1     dz=s/ns
zp=z+dz
IF (zp-ze > 0.0) THEN
  GO TO     2
ELSE
  GO TO     3
END IF
2     dz=ze-z
IF (ABS(dz)-ep > 0.0) THEN
  GO TO     3
ELSE
  GO TO    17
END IF
3     dzot=dz*.5
zp=z+dzot
CALL gh (zp,g3r,g3i)
zp=z+dz
CALL gh (zp,g5r,g5i)
4     t00r=(g1r+g5r)*dzot
t00i=(g1i+g5i)*dzot
t01r=(t00r+dz*g3r)*0.5
t01i=(t00i+dz*g3i)*0.5
t10r=(4.0*t01r-t00r)/3.0
t10i=(4.0*t01i-t00i)/3.0
CALL test (t01r,t10r,te1r,t01i,t10i,te1i,0.d0)
IF (te1i-rx > 0.0) THEN
  GO TO     6
END IF
5     IF (te1r-rx > 0.0) THEN
  GO TO     6
ELSE
  GO TO     8
END IF
6     zp=z+dz*0.25
CALL gh (zp,g2r,g2i)
zp=z+dz*0.75
CALL gh (zp,g4r,g4i)
t02r=(t01r+dzot*(g2r+g4r))*0.5
t02i=(t01i+dzot*(g2i+g4i))*0.5
t11r=(4.0*t02r-t01r)/3.0
t11i=(4.0*t02i-t01i)/3.0
t20r=(16.0*t11r-t10r)/15.0
t20i=(16.0*t11i-t10i)/15.0
CALL test (t11r,t20r,te2r,t11i,t20i,te2i,0.d0)
IF (te2i-rx > 0.0) THEN
  GO TO    14
END IF
7     IF (te2r-rx > 0.0) THEN
  GO TO    14
ELSE
  GO TO     9
END IF
8     sgr=sgr+t10r
sgi=sgi+t10i
nt=nt+2
GO TO 10
9     sgr=sgr+t20r
sgi=sgi+t20i
nt=nt+1
10    z=z+dz
IF (z-zend < 0.0) THEN
  GO TO    11
ELSE
  GO TO    17
END IF
11    g1r=g5r
g1i=g5i
IF (nt-nts < 0) THEN
  GO TO     1
END IF
12    IF (ns-nx > 0) THEN
  GO TO    13
ELSE
  GO TO     1
END IF
13    ns=ns/2
nt=1
GO TO 1
14    nt=0
IF (ns-nm < 0) THEN
  GO TO    16
END IF
15    WRITE(3,18)  z
GO TO 9
16    ns=ns*2
dz=s/ns
dzot=dz*0.5
g5r=g3r
g5i=g3i
g3r=g2r
g3i=g2i
GO TO 4
17    CONTINUE
sgr=sgr*rhk*.5
sgi=sgi*rhk*.5
RETURN
!
18    FORMAT (24H step size limited at z=,f10.5)
END SUBROUTINE hfk
!----------------------------------------------------------------------------

SUBROUTINE hintg (xi,yi,zi)
! ***
!     DOUBLE PRECISION 6/4/85
!
USE gnd

IMPLICIT REAL(NEC2REAL)(a-h,o-z)

REAL(NEC2REAL), INTENT(IN)                         :: xi
REAL(NEC2REAL), INTENT(IN)                         :: yi
REAL(NEC2REAL), INTENT(IN)                         :: zi

INTEGER                                    :: ip

! ***
!     HINTG COMPUTES THE H FIELD OF A PATCH CURRENT
COMPLEX*16 exk,eyk,ezk,exs,eys,ezs,exc,eyc,ezc,gam,  &
    f1x,f1y,f1z,f2x,f2y,f2z,rrv,rrh
COMMON /dataj/ s,b,xj,yj,zj,cabj,sabj,salpj,exk,eyk,ezk,exs,eys,  &
    ezs,exc,eyc,ezc,rkh,ind1,indd1,ind2,indd2,iexk,ipgnd
EQUIVALENCE (t1xj,cabj), (t1yj,sabj), (t1zj,salpj), (t2xj,b), (t2yj,ind1), (t2zj,ind2)
DATA fpi/12.56637062D+0/,tp/6.283185308D+0/

rx=xi-xj
ry=yi-yj
rfl=-1.
exk=(0.,0.)
eyk=(0.,0.)
ezk=(0.,0.)
exs=(0.,0.)
eys=(0.,0.)
ezs=(0.,0.)
DO  ip=1,ksymp
  rfl=-rfl
  rz=zi-zj*rfl
  rsq=rx*rx+ry*ry+rz*rz
  IF (rsq < 1.d-20) CYCLE
  r=SQRT(rsq)
  rk=tp*r
  cr=COS(rk)
  sr=SIN(rk)
  gam=-(DCMPLX(cr,-sr)+rk*DCMPLX(sr,cr))/(fpi*rsq*r)*s
  exc=gam*rx
  eyc=gam*ry
  ezc=gam*rz
  t1zr=t1zj*rfl
  t2zr=t2zj*rfl
  f1x=eyc*t1zr-ezc*t1yj
  f1y=ezc*t1xj-exc*t1zr
  f1z=exc*t1yj-eyc*t1xj
  f2x=eyc*t2zr-ezc*t2yj
  f2y=ezc*t2xj-exc*t2zr
  f2z=exc*t2yj-eyc*t2xj
  IF (ip == 1) GO TO 4
  IF (iperf /= 1) GO TO 1
  f1x=-f1x
  f1y=-f1y
  f1z=-f1z
  f2x=-f2x
  f2y=-f2y
  f2z=-f2z
  GO TO 4
  1     xymag=SQRT(rx*rx+ry*ry)
  IF (xymag > 1.d-6) GO TO 2
  px=0.
  py=0.
  cth=1.
  rrv=(1.,0.)
  GO TO 3
  2     px=-ry/xymag
  py=rx/xymag
  cth=rz/r
  rrv=SQRT(1.-zrati*zrati*(1.-cth*cth))
  3     rrh=zrati*cth
  rrh=(rrh-rrv)/(rrh+rrv)
  rrv=zrati*rrv
  rrv=-(cth-rrv)/(cth+rrv)
  gam=(f1x*px+f1y*py)*(rrv-rrh)
  f1x=f1x*rrh+gam*px
  f1y=f1y*rrh+gam*py
  f1z=f1z*rrh
  gam=(f2x*px+f2y*py)*(rrv-rrh)
  f2x=f2x*rrh+gam*px
  f2y=f2y*rrh+gam*py
  f2z=f2z*rrh
  4     exk=exk+f1x
  eyk=eyk+f1y
  ezk=ezk+f1z
  exs=exs+f2x
  eys=eys+f2y
  ezs=ezs+f2z
END DO
RETURN
END SUBROUTINE hintg
!----------------------------------------------------------------------------

SUBROUTINE hsfld (xi,yi,zi,ai)
! ***
!     DOUBLE PRECISION 6/4/85
!
USE gnd

IMPLICIT REAL(NEC2REAL)(a-h,o-z)

REAL(NEC2REAL), INTENT(IN)                         :: xi
REAL(NEC2REAL), INTENT(IN)                         :: yi
REAL(NEC2REAL), INTENT(IN)                         :: zi
REAL(NEC2REAL), INTENT(IN)                         :: ai

INTEGER                                    :: ip

! ***
!     HSFLD COMPUTES THE H FIELD FOR CONSTANT, SINE, AND COSINE CURRENT
!     ON A SEGMENT INCLUDING GROUND EFFECTS.
COMPLEX*16 exk,eyk,ezk,exs,eys,ezs,exc,eyc,ezc,  &
    hpk,hps,hpc,qx,qy,qz,rrv,rrh,zratx
COMMON /dataj/ s,b,xj,yj,zj,cabj,sabj,salpj,exk,eyk,ezk,exs,eys,  &
    ezs,exc,eyc,ezc,rkh,ind1,indd1,ind2,indd2,iexk,ipgnd
DATA eta/376.73/

xij=xi-xj
yij=yi-yj
rfl=-1.
DO  ip=1,ksymp
  rfl=-rfl
  salpr=salpj*rfl
  zij=zi-rfl*zj
  zp=xij*cabj+yij*sabj+zij*salpr
  rhox=xij-cabj*zp
  rhoy=yij-sabj*zp
  rhoz=zij-salpr*zp
  rh=SQRT(rhox*rhox+rhoy*rhoy+rhoz*rhoz+ai*ai)
  IF (rh > 1.d-10) GO TO 1
  exk=0.
  eyk=0.
  ezk=0.
  exs=0.
  eys=0.
  ezs=0.
  exc=0.
  eyc=0.
  ezc=0.
  CYCLE
  1     rhox=rhox/rh
  rhoy=rhoy/rh
  rhoz=rhoz/rh
  phx=sabj*rhoz-salpr*rhoy
  phy=salpr*rhox-cabj*rhoz
  phz=cabj*rhoy-sabj*rhox
  CALL hsflx (s,rh,zp,hpk,hps,hpc)
  IF (ip /= 2) GO TO 6
  IF (iperf == 1) GO TO 5
  zratx=zrati
  rmag=SQRT(zp*zp+rh*rh)
  xymag=SQRT(xij*xij+yij*yij)
!
!     SET PARAMETERS FOR RADIAL WIRE GROUND SCREEN.
!
  IF (nradl == 0) GO TO 2
  xspec=(xi*zj+zi*xj)/(zi+zj)
  yspec=(yi*zj+zi*yj)/(zi+zj)
  rhospc=SQRT(xspec*xspec+yspec*yspec+t2*t2)
  IF (rhospc > scrwl) GO TO 2
  rrv=t1*rhospc*LOG(rhospc/t2)
  zratx=(rrv*zrati)/(eta*zrati+rrv)
  2     IF (xymag > 1.d-6) GO TO 3
!
!     CALCULATION OF REFLECTION COEFFICIENTS WHEN GROUND IS SPECIFIED.
!
  px=0.
  py=0.
  cth=1.
  rrv=(1.,0.)
  GO TO 4
  3     px=-yij/xymag
  py=xij/xymag
  cth=zij/rmag
  rrv=SQRT(1.-zratx*zratx*(1.-cth*cth))
  4     rrh=zratx*cth
  rrh=-(rrh-rrv)/(rrh+rrv)
  rrv=zratx*rrv
  rrv=(cth-rrv)/(cth+rrv)
  qy=(phx*px+phy*py)*(rrv-rrh)
  qx=qy*px+phx*rrh
  qy=qy*py+phy*rrh
  qz=phz*rrh
  exk=exk-hpk*qx
  eyk=eyk-hpk*qy
  ezk=ezk-hpk*qz
  exs=exs-hps*qx
  eys=eys-hps*qy
  ezs=ezs-hps*qz
  exc=exc-hpc*qx
  eyc=eyc-hpc*qy
  ezc=ezc-hpc*qz
  CYCLE
  5     exk=exk-hpk*phx
  eyk=eyk-hpk*phy
  ezk=ezk-hpk*phz
  exs=exs-hps*phx
  eys=eys-hps*phy
  ezs=ezs-hps*phz
  exc=exc-hpc*phx
  eyc=eyc-hpc*phy
  ezc=ezc-hpc*phz
  CYCLE
  6     exk=hpk*phx
  eyk=hpk*phy
  ezk=hpk*phz
  exs=hps*phx
  eys=hps*phy
  ezs=hps*phz
  exc=hpc*phx
  eyc=hpc*phy
  ezc=hpc*phz
END DO
RETURN
END SUBROUTINE hsfld
!----------------------------------------------------------------------------

SUBROUTINE hsflx (s,rh,zpx,hpk,hps,hpc)
! ***
!     DOUBLE PRECISION 6/4/85
!
IMPLICIT REAL(NEC2REAL)(a-h,o-z)

REAL(NEC2REAL), INTENT(IN)                         :: s
REAL(NEC2REAL), INTENT(IN)                         :: rh
REAL(NEC2REAL), INTENT(IN)                         :: zpx
COMPLEX*16, INTENT(OUT)                  :: hpk
COMPLEX*16, INTENT(OUT)                  :: hps
COMPLEX*16, INTENT(OUT)                  :: hpc
! ***
!     CALCULATES H FIELD OF SINE COSINE, AND CONSTANT CURRENT OF SEGMENT
COMPLEX*16 fj,fjk,ekr1,ekr2,t1,t2,cons
DIMENSION fjx(2), fjkx(2)
EQUIVALENCE (fj,fjx), (fjk,fjkx)
DATA tp/6.283185308D+0/,fjx/0.,1./,fjkx/0.,-6.283185308D+0/
DATA pi8/25.13274123D+0/

IF (rh < 1.d-10) GO TO 6
IF (zpx < 0.) GO TO 1
zp=zpx
hss=1.
GO TO 2
1     zp=-zpx
hss=-1.
2     dh=.5*s
z1=zp+dh
z2=zp-dh
IF (z2 < 1.d-7) GO TO 3
rhz=rh/z2
GO TO 4
3     rhz=1.
4     dk=tp*dh
cdk=COS(dk)
sdk=SIN(dk)
CALL hfk (-dk,dk,rh*tp,zp*tp,hkr,hki)
hpk=DCMPLX(hkr,hki)
IF (rhz < 1.d-3) GO TO 5
rh2=rh*rh
r1=SQRT(rh2+z1*z1)
r2=SQRT(rh2+z2*z2)
ekr1=EXP(fjk*r1)
ekr2=EXP(fjk*r2)
t1=z1*ekr1/r1
t2=z2*ekr2/r2
hps=(cdk*(ekr2-ekr1)-fj*sdk*(t2+t1))*hss
hpc=-sdk*(ekr2+ekr1)-fj*cdk*(t2-t1)
cons=-fj/(2.*tp*rh)
hps=cons*hps
hpc=cons*hpc
RETURN
5     ekr1=DCMPLX(cdk,sdk)/(z2*z2)
ekr2=DCMPLX(cdk,-sdk)/(z1*z1)
t1=tp*(1./z1-1./z2)
t2=EXP(fjk*zp)*rh/pi8
hps=t2*(t1+(ekr1+ekr2)*sdk)*hss
hpc=t2*(-fj*t1+(ekr1-ekr2)*cdk)
RETURN
6     hps=(0.,0.)
hpc=(0.,0.)
hpk=(0.,0.)
RETURN
END SUBROUTINE hsflx
!----------------------------------------------------------------------------

SUBROUTINE intrp (x,y,f1,f2,f3,f4)
! ***
!     DOUBLE PRECISION 6/4/85
!
USE somset
IMPLICIT REAL(NEC2REAL)(a-h,o-z)

REAL(NEC2REAL), INTENT(IN OUT)                     :: x
REAL(NEC2REAL), INTENT(IN OUT)                     :: y
COMPLEX*16, INTENT(OUT)                  :: f1
COMPLEX*16, INTENT(OUT)                  :: f2
COMPLEX*16, INTENT(OUT)                  :: f3
COMPLEX*16, INTENT(OUT)                  :: f4
! ***
!
!     INTRP USES BIVARIATE CUBIC INTERPOLATION TO OBTAIN THE VALUES OF
!     4 FUNCTIONS AT THE POINT (X,Y).
!
COMPLEX*16  a,b,c,d,fx1,fx2,fx3,fx4,p1,p2,p3,p4,a11,a12  &
    ,a13,a14,a21,a22,a23,a24,a31,a32,a33,a34,a41,a42,a43,a44,b11,b12  &
    ,b13,b14,b21,b22,b23,b24,b31,b32,b33,b34,b41,b42,b43,b44,c11,c12  &
    ,c13,c14,c21,c22,c23,c24,c31,c32,c33,c34,c41,c42,c43,c44,d11,d12  &
    ,d13,d14,d21,d22,d23,d24,d31,d32,d33,d34,d41,d42,d43,d44
COMPLEX*16 arl1,arl2,arl3

DIMENSION a(4,4), b(4,4), c(4,4), d(4,4), arl1(1), arl2(1), arl3(1 )

EQUIVALENCE (a(1,1),a11), (a(1,2),a12), (a(1,3),a13), (a(1,4),a14)
EQUIVALENCE (a(2,1),a21), (a(2,2),a22), (a(2,3),a23), (a(2,4),a24)
EQUIVALENCE (a(3,1),a31), (a(3,2),a32), (a(3,3),a33), (a(3,4),a34)
EQUIVALENCE (a(4,1),a41), (a(4,2),a42), (a(4,3),a43), (a(4,4),a44)
EQUIVALENCE (b(1,1),b11), (b(1,2),b12), (b(1,3),b13), (b(1,4),b14)
EQUIVALENCE (b(2,1),b21), (b(2,2),b22), (b(2,3),b23), (b(2,4),b24)
EQUIVALENCE (b(3,1),b31), (b(3,2),b32), (b(3,3),b33), (b(3,4),b34)
EQUIVALENCE (b(4,1),b41), (b(4,2),b42), (b(4,3),b43), (b(4,4),b44)
EQUIVALENCE (c(1,1),c11), (c(1,2),c12), (c(1,3),c13), (c(1,4),c14)
EQUIVALENCE (c(2,1),c21), (c(2,2),c22), (c(2,3),c23), (c(2,4),c24)
EQUIVALENCE (c(3,1),c31), (c(3,2),c32), (c(3,3),c33), (c(3,4),c34)
EQUIVALENCE (c(4,1),c41), (c(4,2),c42), (c(4,3),c43), (c(4,4),c44)
EQUIVALENCE (d(1,1),d11), (d(1,2),d12), (d(1,3),d13), (d(1,4),d14)
EQUIVALENCE (d(2,1),d21), (d(2,2),d22), (d(2,3),d23), (d(2,4),d24)
EQUIVALENCE (d(3,1),d31), (d(3,2),d32), (d(3,3),d33), (d(3,4),d34)
EQUIVALENCE (d(4,1),d41), (d(4,2),d42), (d(4,3),d43), (d(4,4),d44)

EQUIVALENCE (arl1,ar1), (arl2,ar2), (arl3,ar3), (xs2,xsa(2)), (ys3 ,ysa(3))

INTEGER                         :: ixs  = -10
INTEGER                         :: iys  = -10
INTEGER                         :: igrs = -10

REAL(NEC2REAL)                          :: dx   = 1.0
REAL(NEC2REAL)                          :: dy   = 1.0
REAL(NEC2REAL)                          :: xs   = 0.0
REAL(NEC2REAL)                          :: ys   = 0.0

INTEGER, DIMENSION(3)           :: nda  = (/11,17,9/)
INTEGER, DIMENSION(3)           :: ndpa = (/110,85,72/)
INTEGER                         :: ixeg = 0
INTEGER                         :: iyeg = 0


IF (x < xs.OR.y < ys) GO TO 1
ix=INT((x-xs)/dx)+1
iy=INT((y-ys)/dy)+1
!
!     IF POINT LIES IN SAME 4 BY 4 POINT REGION AS PREVIOUS POINT, OLD
!     VALUES ARE REUSED
!
IF (ix < ixeg.OR.iy < iyeg) GO TO 1
IF (IABS(ix-ixs) < 2.AND.IABS(iy-iys) < 2) GO TO 12
!
!     DETERMINE CORRECT GRID AND GRID REGION
!
1     IF (x > xs2) GO TO 2
igr=1
GO TO 3
2     igr=2
IF (y > ys3) igr=3
3     IF (igr == igrs) GO TO 4
igrs=igr
dx=dxa(igrs)
dy=dya(igrs)
xs=xsa(igrs)
ys=ysa(igrs)
nxm2=nxa(igrs)-2
nym2=nya(igrs)-2
nxms=((nxm2+1)/3)*3+1
nyms=((nym2+1)/3)*3+1
nd=nda(igrs)
ndp=ndpa(igrs)
ix=INT((x-xs)/dx)+1
iy=INT((y-ys)/dy)+1
4     ixs=((ix-1)/3)*3+2
IF (ixs < 2) ixs=2
ixeg=-10000
IF (ixs <= nxm2) GO TO 5
ixs=nxm2
ixeg=nxms
5     iys=((iy-1)/3)*3+2
IF (iys < 2) iys=2
iyeg=-10000
IF (iys <= nym2) GO TO 6
iys=nym2
iyeg=nyms
!
!     COMPUTE COEFFICIENTS OF 4 CUBIC POLYNOMIALS IN X FOR THE 4 GRID
!     VALUES OF Y FOR EACH OF THE 4 FUNCTIONS
!
6     iadz=ixs+(iys-3)*nd-ndp
DO  k=1,4
  iadz=iadz+ndp
  iadd=iadz
  DO  i=1,4
    iadd=iadd+nd
    SELECT CASE ( igrs )
      CASE (    1)
        GO TO 7
      CASE (    2)
        GO TO 8
      CASE (    3)
        GO TO 9
    END SELECT
!     P1=AR1(IXS-1,IYS-2+I,K)
    7     p1=arl1(iadd-1)
    p2=arl1(iadd)
    p3=arl1(iadd+1)
    p4=arl1(iadd+2)
    GO TO 10
    8     p1=arl2(iadd-1)
    p2=arl2(iadd)
    p3=arl2(iadd+1)
    p4=arl2(iadd+2)
    GO TO 10
    9     p1=arl3(iadd-1)
    p2=arl3(iadd)
    p3=arl3(iadd+1)
    p4=arl3(iadd+2)
    10    a(i,k)=(p4-p1+3.*(p2-p3))*.1666666667D+0
    b(i,k)=(p1-2.*p2+p3)*.5
    c(i,k)=p3-(2.*p1+3.*p2+p4)*.1666666667D+0
    d(i,k)=p2
  END DO
END DO
xz=(ixs-1)*dx+xs
yz=(iys-1)*dy+ys
!
!     EVALUATE POLYMOMIALS IN X AND THEN USE CUBIC INTERPOLATION IN Y
!     FOR EACH OF THE 4 FUNCTIONS.
!
12    xx=(x-xz)/dx
yy=(y-yz)/dy
fx1=((a11*xx+b11)*xx+c11)*xx+d11
fx2=((a21*xx+b21)*xx+c21)*xx+d21
fx3=((a31*xx+b31)*xx+c31)*xx+d31
fx4=((a41*xx+b41)*xx+c41)*xx+d41
p1=fx4-fx1+3.*(fx2-fx3)
p2=3.*(fx1-2.*fx2+fx3)
p3=6.*fx3-2.*fx1-3.*fx2-fx4
f1=((p1*yy+p2)*yy+p3)*yy*.1666666667D+0+fx2
fx1=((a12*xx+b12)*xx+c12)*xx+d12
fx2=((a22*xx+b22)*xx+c22)*xx+d22
fx3=((a32*xx+b32)*xx+c32)*xx+d32
fx4=((a42*xx+b42)*xx+c42)*xx+d42
p1=fx4-fx1+3.*(fx2-fx3)
p2=3.*(fx1-2.*fx2+fx3)
p3=6.*fx3-2.*fx1-3.*fx2-fx4
f2=((p1*yy+p2)*yy+p3)*yy*.1666666667D+0+fx2
fx1=((a13*xx+b13)*xx+c13)*xx+d13
fx2=((a23*xx+b23)*xx+c23)*xx+d23
fx3=((a33*xx+b33)*xx+c33)*xx+d33
fx4=((a43*xx+b43)*xx+c43)*xx+d43
p1=fx4-fx1+3.*(fx2-fx3)
p2=3.*(fx1-2.*fx2+fx3)
p3=6.*fx3-2.*fx1-3.*fx2-fx4
f3=((p1*yy+p2)*yy+p3)*yy*.1666666667D+0+fx2
fx1=((a14*xx+b14)*xx+c14)*xx+d14
fx2=((a24*xx+b24)*xx+c24)*xx+d24
fx3=((a34*xx+b34)*xx+c34)*xx+d34
fx4=((a44*xx+b44)*xx+c44)*xx+d44
p1=fx4-fx1+3.*(fx2-fx3)
p2=3.*(fx1-2.*fx2+fx3)
p3=6.*fx3-2.*fx1-3.*fx2-fx4
f4=((p1*yy+p2)*yy+p3)*yy*.1666666667D+0+fx2
RETURN
END SUBROUTINE intrp
!----------------------------------------------------------------------------

SUBROUTINE intx (el1,el2,b,ij,sgr,sgi)
! ***
!     DOUBLE PRECISION 6/4/85
!
IMPLICIT REAL(NEC2REAL)(a-h,o-z)

REAL(NEC2REAL), INTENT(IN)                         :: el1
REAL(NEC2REAL), INTENT(IN)                         :: el2
REAL(NEC2REAL), INTENT(IN)                         :: b
INTEGER, INTENT(IN)                        :: ij
REAL(NEC2REAL), INTENT(OUT)                        :: sgr
REAL(NEC2REAL), INTENT(OUT)                        :: sgi
! ***
!
!     INTX PERFORMS NUMERICAL INTEGRATION OF EXP(JKR)/R BY THE METHOD OF
!     VARIABLE INTERVAL WIDTH ROMBERG INTEGRATION.  THE INTEGRAND VALUE
!     IS SUPPLIED BY SUBROUTINE GF.
!
DATA nx,nm,nts,rx/1,65536,4,1.d-4/

z=el1
ze=el2
IF (ij == 0) ze=0.
s=ze-z
fnm=nm
ep=s/(10.*fnm)
zend=ze-ep
sgr=0.
sgi=0.
ns=nx
nt=0
CALL gf (z,g1r,g1i)
1     fns=ns
dz=s/fns
zp=z+dz
IF (zp-ze > 0.0) THEN
  GO TO     2
ELSE
  GO TO     3
END IF
2     dz=ze-z
IF (ABS(dz)-ep > 0.0) THEN
  GO TO     3
ELSE
  GO TO    17
END IF
3     dzot=dz*.5
zp=z+dzot
CALL gf (zp,g3r,g3i)
zp=z+dz
CALL gf (zp,g5r,g5i)
4     t00r=(g1r+g5r)*dzot
t00i=(g1i+g5i)*dzot
t01r=(t00r+dz*g3r)*0.5
t01i=(t00i+dz*g3i)*0.5
t10r=(4.0*t01r-t00r)/3.0
t10i=(4.0*t01i-t00i)/3.0
!
!     TEST CONVERGENCE OF 3 POINT ROMBERG RESULT.
!
CALL test (t01r,t10r,te1r,t01i,t10i,te1i,0.d0)
IF (te1i-rx > 0.0) THEN
  GO TO     6
END IF
5     IF (te1r-rx > 0.0) THEN
  GO TO     6
ELSE
  GO TO     8
END IF
6     zp=z+dz*0.25
CALL gf (zp,g2r,g2i)
zp=z+dz*0.75
CALL gf (zp,g4r,g4i)
t02r=(t01r+dzot*(g2r+g4r))*0.5
t02i=(t01i+dzot*(g2i+g4i))*0.5
t11r=(4.0*t02r-t01r)/3.0
t11i=(4.0*t02i-t01i)/3.0
t20r=(16.0*t11r-t10r)/15.0
t20i=(16.0*t11i-t10i)/15.0
!
!     TEST CONVERGENCE OF 5 POINT ROMBERG RESULT.
!
CALL test (t11r,t20r,te2r,t11i,t20i,te2i,0.d0)
IF (te2i-rx > 0.0) THEN
  GO TO    14
END IF
7     IF (te2r-rx > 0.0) THEN
  GO TO    14
ELSE
  GO TO     9
END IF
8     sgr=sgr+t10r
sgi=sgi+t10i
nt=nt+2
GO TO 10
9     sgr=sgr+t20r
sgi=sgi+t20i
nt=nt+1
10    z=z+dz
IF (z-zend < 0.0) THEN
  GO TO    11
ELSE
  GO TO    17
END IF
11    g1r=g5r
g1i=g5i
IF (nt-nts < 0) THEN
  GO TO     1
END IF
12    IF (ns-nx > 0) THEN
  GO TO    13
ELSE
  GO TO     1
END IF
!
!     DOUBLE STEP SIZE
!
13    ns=ns/2
nt=1
GO TO 1
14    nt=0
IF (ns-nm < 0) THEN
  GO TO    16
END IF
15    WRITE(3,20)  z
GO TO 9
!
!     HALVE STEP SIZE
!
16    ns=ns*2
fns=ns
dz=s/fns
dzot=dz*0.5
g5r=g3r
g5i=g3i
g3r=g2r
g3i=g2i
GO TO 4
17    CONTINUE
IF (ij == 0) THEN
  GO TO    18
ELSE
  GO TO    19
END IF
!
!     ADD CONTRIBUTION OF NEAR SINGULARITY FOR DIAGONAL TERM
!
18    sgr=2.*(sgr+LOG((SQRT(b*b+s*s)+s)/b))
sgi=2.*sgi
19    CONTINUE
RETURN
!
20    FORMAT (24H step size limited at z=,f10.5)
END SUBROUTINE intx
!---------------------------------------------------------------------

FUNCTION isegno (itagi,mx)
! ***
!     DOUBLE PRECISION 6/4/85
!
USE nec2dpar
USE data

IMPLICIT REAL(NEC2REAL)(a-h,o-z)

INTEGER, INTENT(IN)                      :: itagi
INTEGER, INTENT(IN)                      :: mx
! ***
!
!     ISEGNO RETURNS THE SEGMENT NUMBER OF THE MTH SEGMENT HAVING THE
!     TAG NUMBER ITAGI.  IF ITAGI=0 SEGMENT NUMBER M IS RETURNED.
!
IF (mx > 0) GO TO 1
WRITE(3,6)
STOP
1     icnt=0
IF (itagi /= 0) GO TO 2
isegno=mx
RETURN
2     IF (n < 1) GO TO 4
DO  i=1,n
  IF (itag(i) /= itagi) CYCLE
  icnt=icnt+1
  IF (icnt == mx) GO TO 5
END DO
4     WRITE(3,7)  itagi
STOP
5     isegno=i
RETURN
!
6     FORMAT (4X,'CHECK DATA, PARAMETER specifying segment POSITION in a group of equal tags must NOT be zero')
7     FORMAT (///,10X,'NO segment has an itag of ',i5)
END FUNCTION isegno
!----------------------------------------------------------------------------

SUBROUTINE lfactr (a,nrow,ix1,ix2,ip)
! ***
!     DOUBLE PRECISION 6/4/85
!
USE nec2dpar
USE matpar

IMPLICIT REAL(NEC2REAL)(a-h,o-z)

COMPLEX*16, INTENT(IN OUT)               :: a(nrow,1)
INTEGER, INTENT(IN)                      :: nrow
INTEGER, INTENT(IN OUT)                  :: ix1
INTEGER, INTENT(IN OUT)                  :: ix2
INTEGER, INTENT(IN OUT)                  :: ip(nrow)
! ***
!
!     LFACTR PERFORMS GAUSS-DOOLITTLE MANIPULATIONS ON THE TWO BLOCKS OF
!     THE TRANSPOSED MATRIX IN CORE STORAGE.  THE GAUSS-DOOLITTLE
!     ALGORITHM IS PRESENTED ON PAGES 411-416 OF A. RALSTON -- A FIRST
!     COURSE IN NUMERICAL ANALYSIS.  COMMENTS BELOW REFER TO COMMENTS IN
!     RALSTONS TEXT.
!
COMPLEX*16  d,ajr
INTEGER :: r,r1,r2,pj,pr
LOGICAL :: l1,l2,l3
COMMON /scratm/ d(2*maxseg)


iflg=0
!
!     INITIALIZE R1,R2,J1,J2
!
l1=ix1 == 1.AND.ix2 == 2
l2=(ix2-1) == ix1
l3=ix2 == nblsym
IF (l1) GO TO 1
GO TO 2
1     r1=1
r2=2*npsym
j1=1
j2=-1
GO TO 5
2     r1=npsym+1
r2=2*npsym
j1=(ix1-1)*npsym+1
IF (l2) GO TO 3
GO TO 4
3     j2=j1+npsym-2
GO TO 5
4     j2=j1+npsym-1
5     IF (l3) r2=npsym+nlsym
DO  r=r1,r2
!
!     STEP 1
!
  DO  k=j1,nrow
    d(k)=a(k,r)
  END DO
!
!     STEPS 2 AND 3
!
  IF (l1.OR.l2) j2=j2+1
  IF (j1 > j2) GO TO 9
  ixj=0
  DO  j=j1,j2
    ixj=ixj+1
    pj=ip(j)
    ajr=d(pj)
    a(j,r)=ajr
    d(pj)=d(j)
    jp1=j+1
    DO  i=jp1,nrow
      d(i)=d(i)-a(i,ixj)*ajr
    END DO
  END DO
  9     CONTINUE
!
!     STEP 4
!
  j2p1=j2+1
  IF (l1.OR.l2) GO TO 11
  IF (nrow < j2p1) CYCLE
  DO  i=j2p1,nrow
    a(i,r)=d(i)
  END DO
  CYCLE
  11    dmax=dREAL(d(j2p1)*DCONJG(d(j2p1)))
  ip(j2p1)=j2p1
  j2p2=j2+2
  IF (j2p2 > nrow) GO TO 13
  DO  i=j2p2,nrow
    elmag=dREAL(d(i)*DCONJG(d(i)))
    IF (elmag < dmax) CYCLE
    dmax=elmag
    ip(j2p1)=i
  END DO
  13    CONTINUE
  IF (dmax < 1.d-10) iflg=1
  pr=ip(j2p1)
  a(j2p1,r)=d(pr)
  d(pr)=d(j2p1)
!
!     STEP 5
!
  IF (j2p2 > nrow) GO TO 15
  ajr=1./a(j2p1,r)
  DO  i=j2p2,nrow
    a(i,r)=d(i)*ajr
  END DO
  15    CONTINUE
  IF (iflg == 0) CYCLE
  WRITE(3,17)  j2,dmax
  iflg=0
END DO
RETURN
!
17    FORMAT (1H ,6HPIVOT(,i3,2H)=,1P,e16.8)
END SUBROUTINE lfactr

!----------------------------------------------------------------------------

SUBROUTINE load (ldtyp,ldtag,ldtagf,ldtagt,zlr,zli,zlc)
! ***
!     DOUBLE PRECISION 6/4/85
!
USE nec2dpar
USE data
USE zload

IMPLICIT REAL(NEC2REAL)(a-h,o-z)

INTEGER, INTENT(IN)                      :: ldtyp(1)
INTEGER, INTENT(IN)                      :: ldtag(1)
INTEGER, INTENT(IN)                      :: ldtagf(1)
INTEGER, INTENT(IN)                      :: ldtagt(1)
REAL(NEC2REAL), INTENT(IN)                         :: zlr(1)
REAL(NEC2REAL), INTENT(IN)                         :: zli(1)
REAL(NEC2REAL), INTENT(IN)                         :: zlc(1)
! ***
!
!     LOAD CALCULATES THE IMPEDANCE OF SPECIFIED SEGMENTS FOR VARIOUS
!     TYPES OF LOADING
!
COMPLEX*16 zt,tpcj,zint
DIMENSION  tpcjx(2)
EQUIVALENCE (tpcj,tpcjx)
DATA tpcjx/0.,1.883698955D+9/
!
!     WRITE(3,HEADING)
!
WRITE(3,25)
!
!     INITIALIZE D ARRAY, USED FOR TEMPORARY STORAGE OF LOADING
!     INFORMATION.
!
DO  i=n2,n
  zarray(i)=(0.,0.)
END DO
iwarn=0
!
!     CYCLE OVER LOADING CARDS
!
istep=0
2    istep=istep+1
IF (istep <= nload) GO TO 5
IF (iwarn == 1) WRITE(3,26)
IF (n1+2*m1 > 0) GO TO 4
nop=n/np
IF (nop == 1) GO TO 4
DO  i=1,np
  zt=zarray(i)
  l1=i
  DO  l2=2,nop
    l1=l1+np
    zarray(l1)=zt
  END DO
END DO
4    RETURN
5    IF (ldtyp(istep) <= 5) GO TO 6
WRITE(3,27)  ldtyp(istep)
STOP
6    ldtags=ldtag(istep)
jump=ldtyp(istep)+1
ichk=0
!
!     SEARCH SEGMENTS FOR PROPER ITAGS
!
l1=n2
l2=n
IF (ldtags /= 0) GO TO 7
IF (ldtagf(istep) == 0.AND.ldtagt(istep) == 0) GO TO 7
l1=ldtagf(istep)
l2=ldtagt(istep)
IF (l1 > n1) GO TO 7
WRITE(3,29)
STOP
7    DO  i=l1,l2
  IF (ldtags == 0) GO TO 8
  IF (ldtags /= itag(i)) CYCLE
  IF (ldtagf(istep) == 0) GO TO 8
  ichk=ichk+1
  IF (ichk >= ldtagf(istep).AND.ichk <= ldtagt(istep)) GO TO 9
  CYCLE
  8    ichk=1
!
!     CALCULATION OF LAMDA*IMPED. PER UNIT LENGTH, JUMP TO APPROPRIATE
!     SECTION FOR LOADING TYPE
!
  9    SELECT CASE ( jump )
    CASE (    1)
      GO TO 10
    CASE (    2)
      GO TO 11
    CASE (    3)
      GO TO 12
    CASE (    4)
      GO TO 13
    CASE (    5)
      GO TO 14
    CASE (    6)
      GO TO 15
  END SELECT
  10   zt=zlr(istep)/si(i)+tpcj*zli(istep)/(si(i)*wlam)
  IF (ABS(zlc(istep)) > 1.d-20) zt=zt+wlam/(tpcj*si(i)*zlc(istep))
  GO TO 16
  11   zt=tpcj*si(i)*zlc(istep)/wlam
  IF (ABS(zli(istep)) > 1.d-20) zt=zt+si(i)*wlam/(tpcj*zli(istep))
  IF (ABS(zlr(istep)) > 1.d-20) zt=zt+si(i)/zlr(istep)
  zt=1./zt
  GO TO 16
  12   zt=zlr(istep)*wlam+tpcj*zli(istep)
  IF (ABS(zlc(istep)) > 1.d-20) zt=zt+1./(tpcj*si(i)*si(i)*zlc(istep))
  GO TO 16
  13   zt=tpcj*si(i)*si(i)*zlc(istep)
  IF (ABS(zli(istep)) > 1.d-20) zt=zt+1./(tpcj*zli(istep))
  IF (ABS(zlr(istep)) > 1.d-20) zt=zt+1./(zlr(istep)*wlam)
  zt=1./zt
  GO TO 16
  14   zt=DCMPLX(zlr(istep),zli(istep))/si(i)
  GO TO 16
  15   zt=zint(zlr(istep)*wlam,bi(i))
  16   IF ((ABS(dREAL(zarray(i)))+ABS(DIMAG(zarray(i)))) > 1.d-20) iwarn=1
  zarray(i)=zarray(i)+zt
END DO
IF (ichk /= 0) GO TO 18
WRITE(3,28)  ldtags
STOP
!
!     PRINTING THE SEGMENT LOADING DATA, JUMP TO PROPER PRINT
!
18   SELECT CASE ( jump )
  CASE (    1)
    GO TO 19
  CASE (    2)
    GO TO 20
  CASE (    3)
    GO TO 21
  CASE (    4)
    GO TO 22
  CASE (    5)
    GO TO 23
  CASE (    6)
    GO TO 24
END SELECT
19   CALL prnt (ldtags,ldtagf(istep),ldtagt(istep),zlr(istep),zli(istep  &
    ),zlc(istep),0.d0,0.d0,0.d0,' SERIES ')
GO TO 2
20   CALL prnt (ldtags,ldtagf(istep),ldtagt(istep),zlr(istep),zli(istep  &
    ),zlc(istep),0.d0,0.d0,0.d0,'PARALLEL')
GO TO 2
21   CALL prnt (ldtags,ldtagf(istep),ldtagt(istep),zlr(istep),zli(istep  &
    ),zlc(istep),0.d0,0.d0,0.d0,' SERIES (PER METER) ')
GO TO 2
22   CALL prnt (ldtags,ldtagf(istep),ldtagt(istep),zlr(istep),zli(istep  &
    ),zlc(istep),0.d0,0.d0,0.d0,'PARALLEL (PER METER)')
GO TO 2
23   CALL prnt (ldtags,ldtagf(istep),ldtagt(istep),0.d0,0.d0,0.d0,  &
    zlr(istep),zli(istep),0.d0,'FIXED IMPEDANCE ')
GO TO 2
24   CALL prnt (ldtags,ldtagf(istep),ldtagt(istep),0.d0,0.d0,0.d0,0.d0,  &
    0.d0,zlr(istep),'  WIRE  ')
GO TO 2
!
25   FORMAT (//,7X,'LOCATION',10X,'RESISTANCE',3X,'INDUCTANCE',2X, &
    'CAPACITANCE',7X,'IMPEDANCE (ohms)',5X,'CONDUCTIVITY',4X,'TYPE',/,  &
    4X,'ITAG',' from thru',10X,'OHMS',8X,'HENRYS',7X,'FARADS',8X,'REAl', &
    6X,'IMAGINARY',4X,'MHOS/meter')
26   FORMAT (/,10X,'NOTE, some of the above segments have been loaded twice - impedances added')
27   FORMAT (/,10X,'IMPROPER load TYPE choosen, requested TYPE is ',i3 )
28   FORMAT (/,10X,'LOADING DATA card error, no segment has an itag = ',i5)
29   FORMAT (' error - loading may NOT be added TO segments in n.g.f. section')
END SUBROUTINE load
!----------------------------------------------------------------------------

SUBROUTINE ltsolv (a,nrow,ix,b,neq,nrh,ifl1,ifl2)
! ***
!     DOUBLE PRECISION 6/4/85
!
    USE nec2dpar
    USE matpar

    IMPLICIT REAL(NEC2REAL)(a-h,o-z)

    COMPLEX*16, INTENT(IN OUT)               :: a(nrow,nrow)
    INTEGER, INTENT(IN)                      :: nrow
    INTEGER, INTENT(IN)                      :: ix(neq)
    COMPLEX*16, INTENT(IN OUT)               :: b(neq,nrh)
    INTEGER, INTENT(IN)                      :: neq
    INTEGER, INTENT(IN)                      :: nrh
    INTEGER, INTENT(IN)                      :: ifl1
    INTEGER, INTENT(IN)                      :: ifl2
    ! ***
    !
    !     LTSOLV SOLVES THE MATRIX EQ. Y(R)*LU(T)=B(R) WHERE (R) DENOTES ROW
    !     VECTOR AND LU(T) DENOTES THE LU DECOMPOSITION OF THE TRANSPOSE OF
    !     THE ORIGINAL COEFFICIENT MATRIX.  THE LU(T) DECOMPOSITION IS
    !     STORED ON TAPE 5 IN BLOCKS IN ASCENDING ORDER AND ON FILE 3 IN
    !     BLOCKS OF DESCENDING ORDER.
    !
    COMPLEX*16  y,sum
    COMMON /scratm/ y(2*maxseg)

    !
    !     FORWARD SUBSTITUTION
    !
    i2=2*npsym*nrow
    DO  ixblk1=1,nblsym
      CALL blckin (a,ifl1,1,i2,1,121)
      k2=npsym
      IF (ixblk1 == nblsym) k2=nlsym
      jst=(ixblk1-1)*npsym
      DO  ic=1,nrh
        j=jst
        DO  k=1,k2
          jm1=j
          j=j+1
          sum=(0.,0.)
          IF (jm1 < 1) GO TO 2
          DO  i=1,jm1
            sum=sum+a(i,k)*b(i,ic)
          END DO
          2     b(j,ic)=(b(j,ic)-sum)/a(j,k)
        END DO
      END DO
    END DO
    !
    !     BACKWARD SUBSTITUTION
    !
    jst=nrow+1
    DO  ixblk1=1,nblsym
      CALL blckin (a,ifl2,1,i2,1,122)
      k2=npsym
      IF (ixblk1 == 1) k2=nlsym
      DO  ic=1,nrh
        kp=k2+1
        j=jst
        DO  k=1,k2
          kp=kp-1
          jp1=j
          j=j-1
          sum=(0.,0.)
          IF (nrow < jp1) CYCLE
          DO  i=jp1,nrow
            sum=sum+a(i,kp)*b(i,ic)
          END DO
          b(j,ic)=b(j,ic)-sum
        END DO
      END DO
      jst=jst-k2
    END DO
    !
    !     UNSCRAMBLE SOLUTION
    !
    DO  ic=1,nrh
      DO  i=1,nrow
        ixi=ix(i)
        y(ixi)=b(i,ic)
      END DO
      DO  i=1,nrow
        b(i,ic)=y(i)
      END DO
    END DO
    RETURN
END SUBROUTINE ltsolv
!----------------------------------------------------------------------------

SUBROUTINE lunscr (a,nrow,nop,ix,ip,iu2,iu3,iu4)
! ***
!     DOUBLE PRECISION 6/4/85
!
USE matpar

IMPLICIT REAL(NEC2REAL)(a-h,o-z)

COMPLEX*16, INTENT(IN OUT)               :: a(nrow,1)
INTEGER, INTENT(IN)                      :: nrow
INTEGER, INTENT(IN)                      :: nop
INTEGER, INTENT(OUT)                     :: ix(nrow)
INTEGER, INTENT(IN)                      :: ip(nrow)
INTEGER, INTENT(IN)                      :: iu2
INTEGER, INTENT(IN)                      :: iu3
INTEGER, INTENT(IN)                      :: iu4
! ***
!
!     S/R WHICH UNSCRAMBLES, SCRAMBLED FACTORED MATRIX
!
COMPLEX*16  temp

i1=1
i2=2*npsym*nrow
nm1=nrow-1
REWIND iu2
REWIND iu3
REWIND iu4
DO  kk=1,nop
  ka=(kk-1)*nrow
  DO  ixblk1=1,nblsym
    CALL blckin (a,iu2,i1,i2,1,121)
    k1=(ixblk1-1)*npsym+2
    IF (nm1 < k1) GO TO 3
    j2=0
    DO  k=k1,nm1
      IF (j2 < npsym) j2=j2+1
      ipk=ip(k+ka)
      DO  j=1,j2
        temp=a(k,j)
        a(k,j)=a(ipk,j)
        a(ipk,j)=temp
      END DO
    END DO
    3     CONTINUE
    CALL blckot (a,iu3,i1,i2,1,122)
  END DO
  DO  ixblk1=1,nblsym
    BACKSPACE iu3
    IF (ixblk1 /= 1) BACKSPACE iu3
    CALL blckin (a,iu3,i1,i2,1,123)
    CALL blckot (a,iu4,i1,i2,1,124)
  END DO
  DO  i=1,nrow
    ix(i+ka)=i
  END DO
  DO  i=1,nrow
    ipi=ip(i+ka)
    ixt=ix(i+ka)
    ix(i+ka)=ix(ipi+ka)
    ix(ipi+ka)=ixt
  END DO
  IF (nop == 1) CYCLE
  nb1=nblsym-1
!     SKIP NB1 LOGICAL RECORDS FORWARD
  DO  ixblk1=1,nb1
    CALL blckin (a,iu3,i1,i2,1,125)
  END DO
END DO
REWIND iu2
REWIND iu3
REWIND iu4
RETURN
END SUBROUTINE lunscr
!----------------------------------------------------------------------------

SUBROUTINE move (rox,roy,roz,xs,ys,zs,its,nrpt,itgi)
! ***
!     DOUBLE PRECISION 6/4/85
!
USE nec2dpar
USE data
USE angl

IMPLICIT REAL(NEC2REAL)(a-h,o-z)

REAL(NEC2REAL), INTENT(IN OUT)                     :: rox
REAL(NEC2REAL), INTENT(IN OUT)                     :: roy
REAL(NEC2REAL), INTENT(IN OUT)                     :: roz
REAL(NEC2REAL), INTENT(IN)                         :: xs
REAL(NEC2REAL), INTENT(IN)                         :: ys
REAL(NEC2REAL), INTENT(IN)                         :: zs
INTEGER, INTENT(IN)                        :: its
INTEGER, INTENT(IN)                        :: nrpt
INTEGER, INTENT(IN)                        :: itgi
! ***
!
!     SUBROUTINE MOVE MOVES THE STRUCTURE WITH RESPECT TO ITS
!     COORDINATE SYSTEM OR REPRODUCES STRUCTURE IN NEW POSITIONS.
!     STRUCTURE IS ROTATED ABOUT X,Y,Z AXES BY ROX,ROY,ROZ
!     RESPECTIVELY, THEN SHIFTED BY XS,YS,ZS
!
DIMENSION t1x(1), t1y(1), t1z(1), t2x(1), t2y(1), t2z(1), x2(1), y2(1), z2(1)
EQUIVALENCE (x2(1),si(1)), (y2(1),alp(1)), (z2(1),bet(1))
EQUIVALENCE (t1x,si), (t1y,alp), (t1z,bet), (t2x,icon1), (t2y,icon2), (t2z,itag)

IF (ABS(rox)+ABS(roy) > 1.d-10) ipsym=ipsym*3
sps=SIN(rox)
cps=COS(rox)
sth=SIN(roy)
cth=COS(roy)
sph=SIN(roz)
cph=COS(roz)
xx=cph*cth
xy=cph*sth*sps-sph*cps
xz=cph*sth*cps+sph*sps
yx=sph*cth
yy=sph*sth*sps+cph*cps
yz=sph*sth*cps-cph*sps
zx=-sth
zy=cth*sps
zz=cth*cps
nrp=nrpt
IF (nrpt == 0) nrp=1
ix=1
IF (n < n2) GO TO 3
i1=isegno(its,1)
IF (i1 < n2) i1=n2
ix=i1
k=n
IF (nrpt == 0) k=i1-1
DO  ir=1,nrp
  DO  i=i1,n
    k=k+1
    xi=x(i)
    yi=y(i)
    zi=z(i)
    x(k)=xi*xx+yi*xy+zi*xz+xs
    y(k)=xi*yx+yi*yy+zi*yz+ys
    z(k)=xi*zx+yi*zy+zi*zz+zs
    xi=x2(i)
    yi=y2(i)
    zi=z2(i)
    x2(k)=xi*xx+yi*xy+zi*xz+xs
    y2(k)=xi*yx+yi*yy+zi*yz+ys
    z2(k)=xi*zx+yi*zy+zi*zz+zs
    bi(k)=bi(i)
    itag(k)=itag(i)
    IF(itag(i) /= 0)itag(k)=itag(i)+itgi
  END DO
  i1=n+1
  n=k
END DO
3     IF (m < m2) GO TO 6
i1=m2
k=m
ldi=ld+1
IF (nrpt == 0) k=m1
DO  ii=1,nrp
  DO  i=i1,m
    k=k+1
    ir=ldi-i
    kr=ldi-k
    xi=x(ir)
    yi=y(ir)
    zi=z(ir)
    x(kr)=xi*xx+yi*xy+zi*xz+xs
    y(kr)=xi*yx+yi*yy+zi*yz+ys
    z(kr)=xi*zx+yi*zy+zi*zz+zs
    xi=t1x(ir)
    yi=t1y(ir)
    zi=t1z(ir)
    t1x(kr)=xi*xx+yi*xy+zi*xz
    t1y(kr)=xi*yx+yi*yy+zi*yz
    t1z(kr)=xi*zx+yi*zy+zi*zz
    xi=t2x(ir)
    yi=t2y(ir)
    zi=t2z(ir)
    t2x(kr)=xi*xx+yi*xy+zi*xz
    t2y(kr)=xi*yx+yi*yy+zi*yz
    t2z(kr)=xi*zx+yi*zy+zi*zz
    salp(kr)=salp(ir)
    bi(kr)=bi(ir)
  END DO
  i1=m+1
  m=k
END DO
6     IF ((nrpt == 0).AND.(ix == 1)) RETURN
np=n
mp=m
ipsym=0
RETURN
END SUBROUTINE move

!----------------------------------------------------------------------------
!
!     NEFLD COMPUTES THE NEAR FIELD AT SPECIFIED POINTS IN SPACE AFTER
!     THE STRUCTURE CURRENTS HAVE BEEN COMPUTED.
!
SUBROUTINE nefld (xob,yob,zob,ex,ey,ez)
USE nec2dpar
USE data
USE gnd
USE crnt
USE angl

IMPLICIT REAL(NEC2REAL)(a-h,o-z)

REAL(NEC2REAL), INTENT(IN)                       :: xob
REAL(NEC2REAL), INTENT(IN)                       :: yob
REAL(NEC2REAL), INTENT(IN)                       :: zob
COMPLEX*16, INTENT(OUT)                  :: ex
COMPLEX*16, INTENT(OUT)                  :: ey
COMPLEX*16, INTENT(OUT)                  :: ez

INTEGER                                  :: ip

COMPLEX*16  acx,bcx,ccx,exk,eyk,ezk,exs,eys,ezs,exc,eyc,ezc
COMMON /dataj/ s,b,xj,yj,zj,cabj,sabj,salpj,exk,eyk,ezk,exs,eys,  &
    ezs,exc,eyc,ezc,rkh,ind1,indd1,ind2,indd2,iexk,ipgnd
DIMENSION cab(1), sab(1), t1x(1), t1y(1), t1z(1), t2x(1), t2y(1), t2z(1)
EQUIVALENCE (cab,alp), (sab,bet)
EQUIVALENCE (t1x,si), (t1y,alp), (t1z,bet), (t2x,icon1), (t2y,icon2), (t2z,itag)
EQUIVALENCE (t1xj,cabj), (t1yj,sabj), (t1zj,salpj), (t2xj,b), &
    (t2yj,ind1), (t2zj,ind2)

ex=(0.,0.)
ey=(0.,0.)
ez=(0.,0.)
ax=0.
IF (n == 0) GO TO 20
DO  i=1,n
  xj=xob-x(i)
  yj=yob-y(i)
  zj=zob-z(i)
  zp=cab(i)*xj+sab(i)*yj+salp(i)*zj
  IF (ABS(zp) > 0.5001*si(i)) CYCLE
  zp=xj*xj+yj*yj+zj*zj-zp*zp
  xj=bi(i)
  IF (zp > 0.9*xj*xj) CYCLE
  ax=xj
  EXIT
END DO
2     DO  i=1,n
  s=si(i)
  b=bi(i)
  xj=x(i)
  yj=y(i)
  zj=z(i)
  cabj=cab(i)
  sabj=sab(i)
  salpj=salp(i)
  IF (iexk == 0) GO TO 18
  
  ipr=icon1(i)
  IF(ipr > 10000)GO TO 9      !<---NEW, av016
  IF (ipr < 0) THEN
    GO TO     3
  ELSE IF (ipr == 0) THEN
    GO TO     8
  ELSE
    GO TO     4
  END IF
  
  3     ipr=-ipr
  IF (-icon1(ipr) /= i) GO TO 9
  GO TO 6
  4     IF (ipr /= i) GO TO 5
  IF (cabj*cabj+sabj*sabj > 1.d-8) GO TO 9
  GO TO 7
  5     IF (icon2(ipr) /= i) GO TO 9
  6     xi=ABS(cabj*cab(ipr)+sabj*sab(ipr)+salpj*salp(ipr))
  IF (xi < 0.999999D+0) GO TO 9
  IF (ABS(bi(ipr)/b-1.) > 1.d-6) GO TO 9
  7     ind1=0
  GO TO 10
  8     ind1=1
  GO TO 10
  9     ind1=2
  
  10    ipr=icon2(i)
  IF(ipr > 10000)GO TO 17    !<---NEW, av016
  IF (ipr < 0) THEN
    GO TO    11
  ELSE IF (ipr == 0) THEN
    GO TO    16
  ELSE
    GO TO    12
  END IF
  
  11    ipr=-ipr
  IF (-icon2(ipr) /= i) GO TO 17
  GO TO 14
  12    IF (ipr /= i) GO TO 13
  IF (cabj*cabj+sabj*sabj > 1.d-8) GO TO 17
  GO TO 15
  13    IF (icon1(ipr) /= i) GO TO 17
  14    xi=ABS(cabj*cab(ipr)+sabj*sab(ipr)+salpj*salp(ipr))
  IF (xi < 0.999999D+0) GO TO 17
  IF (ABS(bi(ipr)/b-1.) > 1.d-6) GO TO 17
  15    ind2=0
  GO TO 18
  16    ind2=1
  GO TO 18
  17    ind2=2
  18    CONTINUE
  CALL efld (xob,yob,zob,ax,1)
  acx=DCMPLX(air(i),aii(i))
  bcx=DCMPLX(bir(i),bii(i))
  ccx=DCMPLX(cir(i),cii(i))
  ex=ex+exk*acx+exs*bcx+exc*ccx
  ey=ey+eyk*acx+eys*bcx+eyc*ccx
  ez=ez+ezk*acx+ezs*bcx+ezc*ccx
END DO
IF (m == 0) RETURN
20    jc=n
jl=ld+1
DO  i=1,m
  jl=jl-1
  s=bi(jl)
  xj=x(jl)
  yj=y(jl)
  zj=z(jl)
  t1xj=t1x(jl)
  t1yj=t1y(jl)
  t1zj=t1z(jl)
  t2xj=t2x(jl)
  t2yj=t2y(jl)
  t2zj=t2z(jl)
  jc=jc+3
  acx=t1xj*cur(jc-2)+t1yj*cur(jc-1)+t1zj*cur(jc)
  bcx=t2xj*cur(jc-2)+t2yj*cur(jc-1)+t2zj*cur(jc)
  DO  ip=1,ksymp
    ipgnd=ip
    CALL unere (xob,yob,zob)
    ex=ex+acx*exk+bcx*exs
    ey=ey+acx*eyk+bcx*eys
    ez=ez+acx*ezk+bcx*ezs
  END DO
END DO
RETURN
END SUBROUTINE nefld

!----------------------------------------------------------------------------

SUBROUTINE netwk (cm,cmb,cmc,cmd,ip,einc)
! ***
!     DOUBLE PRECISION 6/4/85
!
USE nec2dpar
USE data
USE crnt
USE vsorc
USE netcx

IMPLICIT REAL(NEC2REAL)(a-h,o-z)

COMPLEX*16, INTENT(IN OUT)               :: cm(1)
COMPLEX*16, INTENT(IN OUT)               :: cmb(1)
COMPLEX*16, INTENT(IN OUT)               :: cmc(1)
COMPLEX*16, INTENT(IN OUT)               :: cmd(1)
INTEGER, INTENT(IN OUT)                  :: ip(1)
COMPLEX*16, INTENT(IN OUT)               :: einc(1)
! ***
!
!     SUBROUTINE NETWK SOLVES FOR STRUCTURE CURRENTS FOR A GIVEN
!     EXCITATION INCLUDING THE EFFECT OF NON-RADIATING NETWORKS IF
!     PRESENT.
!
COMPLEX*16 cmn,rhnt,ymit,rhs,vlt,vsrc,rhnx,cux

DIMENSION cmn(netmx,netmx), rhnt(netmx), ipnt(netmx),  &
    nteqa(netmx), ntsca(netmx), rhs(3*maxseg), vsrc(netmx), rhnx(netmx)

!hwh  DATA NDIMN,NDIMNP/netmx,netmx+1/,TP/6.283185308D+0/
DATA ndimn,ndimnp/netmx,netmxp1/,tp/6.283185308D+0/ ! hwh

neqz2=neq2
IF(neqz2 == 0)neqz2=1
pin=0.
pnls=0.
neqt=neq+neq2
IF (ntsol /= 0) GO TO 42
nop=neq/npeq
IF (masym == 0) GO TO 14
!
!     COMPUTE RELATIVE MATRIX ASYMMETRY
!
irow1=0
IF (nonet == 0) GO TO 5
DO  i=1,nonet
  nseg1=iseg1(i)
  loop3:  DO  isc1=1,2
    IF (irow1 == 0) GO TO 2
    DO  j=1,irow1
      IF (nseg1 == ipnt(j)) GO TO 3
    END DO
2   irow1=irow1+1
    ipnt(irow1)=nseg1
3   nseg1=iseg2(i)
  END DO loop3
END DO
5     IF (nsant == 0) GO TO 9
loop8:  DO  i=1,nsant
  nseg1=isant(i)
  IF (irow1 == 0) GO TO 7
  DO  j=1,irow1
    IF (nseg1 == ipnt(j)) CYCLE loop8
  END DO
  7     irow1=irow1+1
  ipnt(irow1)=nseg1
END DO loop8

9      IF (irow1 < ndimnp) GO TO 10
WRITE(3,59)
STOP
10    IF (irow1 < 2) GO TO 14
DO  i=1,irow1
  isc1=ipnt(i)
  asm=si(isc1)
  DO  j=1,neqt
    rhs(j)=(0.,0.)
  END DO
  rhs(isc1)=(1.,0.)
  CALL solgf (cm,cmb,cmc,cmd,rhs,ip,np,n1,n,mp,m1,m,neq,neq2,neqz2)
  CALL cabc (rhs)
  DO  j=1,irow1
    isc1=ipnt(j)
    cmn(j,i)=rhs(isc1)/asm
  END DO
END DO
asm=0.
asa=0.
DO  i=2,irow1
  isc1=i-1
  DO  j=1,isc1
    cux=cmn(i,j)
    pwr=ABS((cux-cmn(j,i))/cux)
    asa=asa+pwr*pwr
    IF (pwr < asm) CYCLE
    asm=pwr
    nteq=ipnt(i)
    ntsc=ipnt(j)
  END DO
END DO
asa=SQRT(asa*2./dfloat(irow1*(irow1-1)))
WRITE(3,58)  asm,nteq,ntsc,asa
14    IF (nonet == 0) GO TO 48
!
!     SOLUTION OF NETWORK EQUATIONS
!
DO  i=1,ndimn
  rhnx(i)=(0.,0.)
  DO  j=1,ndimn
    cmn(i,j)=(0.,0.)
  END DO
END DO
nteq=0
ntsc=0
!
!     SORT NETWORK AND SOURCE DATA AND ASSIGN EQUATION NUMBERS TO
!     SEGMENTS.
!
DO  j=1,nonet
  nseg1=iseg1(j)
  nseg2=iseg2(j)
  IF (ntyp(j) > 1) GO TO 16
  y11r=x11r(j)
  y11i=x11i(j)
  y12r=x12r(j)
  y12i=x12i(j)
  y22r=x22r(j)
  y22i=x22i(j)
  GO TO 17
  16    y22r=tp*x11i(j)/wlam
  y12r=0.
  y12i=1./(x11r(j)*SIN(y22r))
  y11r=x12r(j)
  y11i=-y12i*COS(y22r)
  y22r=x22r(j)
  y22i=y11i+x22i(j)
  y11i=y11i+x12i(j)
  IF (ntyp(j) == 2) GO TO 17
  y12r=-y12r
  y12i=-y12i
  17    IF (nsant == 0) GO TO 19
  DO  i=1,nsant
    IF (nseg1 /= isant(i)) CYCLE
    isc1=i
    GO TO 22
  END DO
  19    isc1=0
  IF (nteq == 0) GO TO 21
  DO  i=1,nteq
    IF (nseg1 /= nteqa(i)) CYCLE
    irow1=i
    GO TO 25
  END DO
  21    nteq=nteq+1
  irow1=nteq
  nteqa(nteq)=nseg1
  GO TO 25
  22    IF (ntsc == 0) GO TO 24
  DO  i=1,ntsc
    IF (nseg1 /= ntsca(i)) CYCLE
    irow1=ndimnp-i
    GO TO 25
  END DO
  24    ntsc=ntsc+1
  irow1=ndimnp-ntsc
  ntsca(ntsc)=nseg1
  vsrc(ntsc)=vsant(isc1)
  25    IF (nsant == 0) GO TO 27
  DO  i=1,nsant
    IF (nseg2 /= isant(i)) CYCLE
    isc2=i
    GO TO 30
  END DO
  27    isc2=0
  IF (nteq == 0) GO TO 29
  DO  i=1,nteq
    IF (nseg2 /= nteqa(i)) CYCLE
    irow2=i
    GO TO 33
  END DO
  29    nteq=nteq+1
  irow2=nteq
  nteqa(nteq)=nseg2
  GO TO 33
  30    IF (ntsc == 0) GO TO 32
  DO  i=1,ntsc
    IF (nseg2 /= ntsca(i)) CYCLE
    irow2=ndimnp-i
    GO TO 33
  END DO
  32    ntsc=ntsc+1
  irow2=ndimnp-ntsc
  ntsca(ntsc)=nseg2
  vsrc(ntsc)=vsant(isc2)
  33    IF (ntsc+nteq < ndimnp) GO TO 34
  WRITE(3,59)
  STOP
!
!     FILL NETWORK EQUATION MATRIX AND RIGHT HAND SIDE VECTOR WITH
!     NETWORK SHORT-CIRCUIT ADMITTANCE MATRIX COEFFICIENTS.
!
  34    IF (isc1 /= 0) GO TO 35
  cmn(irow1,irow1)=cmn(irow1,irow1)-DCMPLX(y11r,y11i)*si(nseg1)
  cmn(irow1,irow2)=cmn(irow1,irow2)-DCMPLX(y12r,y12i)*si(nseg1)
  GO TO 36
  35    rhnx(irow1)=rhnx(irow1)+DCMPLX(y11r,y11i)*vsant(isc1)/wlam
  rhnx(irow2)=rhnx(irow2)+DCMPLX(y12r,y12i)*vsant(isc1)/wlam
  36    IF (isc2 /= 0) GO TO 37
  cmn(irow2,irow2)=cmn(irow2,irow2)-DCMPLX(y22r,y22i)*si(nseg2)
  cmn(irow2,irow1)=cmn(irow2,irow1)-DCMPLX(y12r,y12i)*si(nseg2)
  CYCLE
  37    rhnx(irow1)=rhnx(irow1)+DCMPLX(y12r,y12i)*vsant(isc2)/wlam
  rhnx(irow2)=rhnx(irow2)+DCMPLX(y22r,y22i)*vsant(isc2)/wlam
END DO
!
!     ADD INTERACTION MATRIX ADMITTANCE ELEMENTS TO NETWORK EQUATION
!     MATRIX
!
DO  i=1,nteq
  DO  j=1,neqt
    rhs(j)=(0.,0.)
  END DO
  irow1=nteqa(i)
  rhs(irow1)=(1.,0.)
  CALL solgf (cm,cmb,cmc,cmd,rhs,ip,np,n1,n,mp,m1,m,neq,neq2,neqz2)
  CALL cabc (rhs)
  DO  j=1,nteq
    irow1=nteqa(j)
    cmn(i,j)=cmn(i,j)+rhs(irow1)
  END DO
END DO
!
!     FACTOR NETWORK EQUATION MATRIX
!
CALL factr (nteq,cmn,ipnt,ndimn)
!
!     ADD TO NETWORK EQUATION RIGHT HAND SIDE THE TERMS DUE TO ELEMENT
!     INTERACTIONS
!
42    IF (nonet == 0) GO TO 48
DO  i=1,neqt
  rhs(i)=einc(i)
END DO
CALL solgf (cm,cmb,cmc,cmd,rhs,ip,np,n1,n,mp,m1,m,neq,neq2,neqz2)
CALL cabc (rhs)
DO  i=1,nteq
  irow1=nteqa(i)
  rhnt(i)=rhnx(i)+rhs(irow1)
END DO
!
!     SOLVE NETWORK EQUATIONS
!
CALL solve (nteq,cmn,ipnt,rhnt,ndimn)
!
!     ADD FIELDS DUE TO NETWORK VOLTAGES TO ELECTRIC FIELDS APPLIED TO
!     STRUCTURE AND SOLVE FOR INDUCED CURRENT
!
DO  i=1,nteq
  irow1=nteqa(i)
  einc(irow1)=einc(irow1)-rhnt(i)
END DO
CALL solgf (cm,cmb,cmc,cmd,einc,ip,np,n1,n,mp,m1,m,neq,neq2,neqz2)
CALL cabc (einc)
IF (nprint == 0) WRITE(3,61)
IF (nprint == 0) WRITE(3,60)
DO  i=1,nteq
  irow1=nteqa(i)
  vlt=rhnt(i)*si(irow1)*wlam
  cux=einc(irow1)*wlam
  ymit=cux/vlt
  zped=vlt/cux
  irow2=itag(irow1)
  pwr=.5*dREAL(vlt*DCONJG(cux))
  pnls=pnls-pwr
  IF (nprint == 0) WRITE(3,62)  irow2,irow1,vlt,cux,zped,ymit,pwr
END DO
IF (ntsc == 0) GO TO 49
DO  i=1,ntsc
  irow1=ntsca(i)
  vlt=vsrc(i)
  cux=einc(irow1)*wlam
  ymit=cux/vlt
  zped=vlt/cux
  irow2=itag(irow1)
  pwr=.5*dREAL(vlt*DCONJG(cux))
  pnls=pnls-pwr
  IF (nprint == 0) WRITE(3,62)  irow2,irow1,vlt,cux,zped,ymit,pwr
END DO
GO TO 49
!
!     SOLVE FOR CURRENTS WHEN NO NETWORKS ARE PRESENT
!
48    CALL solgf (cm,cmb,cmc,cmd,einc,ip,np,n1,n,mp,m1,m,neq,neq2,neqz2)
CALL cabc (einc)
ntsc=0
49    IF (nsant+nvqd == 0) RETURN
WRITE(3,63)
WRITE(3,60)
IF (nsant == 0) GO TO 56
DO  i=1,nsant
  isc1=isant(i)
  vlt=vsant(i)
  IF (ntsc == 0) GO TO 51
  DO  j=1,ntsc
    IF (ntsca(j) == isc1) GO TO 52
  END DO
  51    cux=einc(isc1)*wlam
  irow1=0
  GO TO 54
  52    irow1=ndimnp-j
  cux=rhnx(irow1)
  DO  j=1,nteq
    cux=cux-cmn(j,irow1)*rhnt(j)
  END DO
  cux=(einc(isc1)+cux)*wlam
  54    ymit=cux/vlt
  zped=vlt/cux
  pwr=.5*dREAL(vlt*DCONJG(cux))
  pin=pin+pwr
  IF (irow1 /= 0) pnls=pnls+pwr
  irow2=itag(isc1)
  WRITE(3,62)  irow2,isc1,vlt,cux,zped,ymit,pwr
END DO
56    IF (nvqd == 0) RETURN
DO  i=1,nvqd
  isc1=ivqd(i)
  vlt=vqd(i)
  cux=DCMPLX(air(isc1),aii(isc1))
  ymit=DCMPLX(bir(isc1),bii(isc1))
  zped=DCMPLX(cir(isc1),cii(isc1))
  pwr=si(isc1)*tp*.5
  cux=(cux-ymit*SIN(pwr)+zped*COS(pwr))*wlam
  ymit=cux/vlt
  zped=vlt/cux
  pwr=.5*dREAL(vlt*DCONJG(cux))
  pin=pin+pwr
  irow2=itag(isc1)
  WRITE(3,64)  irow2,isc1,vlt,cux,zped,ymit,pwr
END DO
RETURN
!
58    FORMAT (///,3X,'MAXIMUM relative asymmetry of the driving point',  &
    ' admittance matrix is',1P,e10.3,' for segments',i5,' AND',i5,/,  &
    3X,'RMS relative asymmetry is',e10.3)
59    FORMAT (1X,'ERROR - - network array DIMENSIONs too small')
60    FORMAT (/,3X,'TAG',3X,'SEG.',4X,'VOLTAGE (volts)',9X, &
    'CURRENT (amps)',9X,'IMPEDANCE (ohms)',8X,'ADMITTANCE (mhos)',6X,'POWER',/,  &
    3X,'NO.',3X,'NO.',4X,'REAL',8X,'IMAG.',3(7X,'REAL',8X,'IMAG.'),5X, '(watts)')
61    FORMAT (///,27X,'- - - structure excitation DATA at network connection points - - -')
62    FORMAT (2(1X,i5),1P,9E12.5)
63    FORMAT (///,42X,'- - - antenna INPUT PARAMETERs - - -')
64    FORMAT (1X,i5,2H *,i4,1P,9E12.5)
END SUBROUTINE netwk
!----------------------------------------------------------------------------
! 
!     COMPUTE NEAR E OR H FIELDS OVER A RANGE OF POINTS
!
SUBROUTINE nfpat
USE nec2dpar
USE plot
USE data
USE fpat

IMPLICIT REAL(NEC2REAL)(a-h,o-z)
COMPLEX*16 ex,ey,ez

REAL(NEC2REAL)                   :: ta = 1.745329252D-02

IF (nfeh == 1) GO TO 1
WRITE(3,10)
GO TO 2
1     WRITE(3,12)
2     znrt=znr-dznr
DO  i=1,nrz
  znrt=znrt+dznr
  IF (near == 0) GO TO 3
  cth=COS(ta*znrt)
  sth=SIN(ta*znrt)
  3     ynrt=ynr-dynr
  DO  j=1,nry
    ynrt=ynrt+dynr
    IF (near == 0) GO TO 4
    cph=COS(ta*ynrt)
    sph=SIN(ta*ynrt)
    4     xnrt=xnr-dxnr
    DO  kk=1,nrx
      xnrt=xnrt+dxnr
      IF (near == 0) GO TO 5
      xob=xnrt*sth*cph
      yob=xnrt*sth*sph
      zob=xnrt*cth
      GO TO 6
      5     xob=xnrt
      yob=ynrt
      zob=znrt
      6     tmp1=xob/wlam
      tmp2=yob/wlam
      tmp3=zob/wlam
      IF (nfeh == 1) GO TO 7
      CALL nefld (tmp1,tmp2,tmp3,ex,ey,ez)
      GO TO 8
      7     CALL nhfld (tmp1,tmp2,tmp3,ex,ey,ez)
      8     tmp1=ABS(ex)
      tmp2=cang(ex)
      tmp3=ABS(ey)
      tmp4=cang(ey)
      tmp5=ABS(ez)
      tmp6=cang(ez)
      WRITE(3,11)  xob,yob,zob,tmp1,tmp2,tmp3,tmp4,tmp5,tmp6
!***
      IF(iplp1 /= 2) CYCLE
      SELECT CASE ( iplp4 )
        CASE (    1)
          GO TO 14
        CASE (    2)
          GO TO 15
        CASE (    3)
          GO TO 16
      END SELECT
      14    xxx=xob
      GO TO 17
      15    xxx=yob
      GO TO 17
      16    xxx=zob
      17    CONTINUE
      IF(iplp2 /= 2) GO TO 13
      IF(iplp3 == 1) WRITE(8,*) xxx,tmp1,tmp2
      IF(iplp3 == 2) WRITE(8,*) xxx,tmp3,tmp4
      IF(iplp3 == 3) WRITE(8,*) xxx,tmp5,tmp6
      IF(iplp3 == 4) WRITE(8,*) xxx,tmp1,tmp2,tmp3,tmp4,tmp5,tmp6
      CYCLE
      13    IF(iplp2 /= 1) CYCLE
      IF(iplp3 == 1) WRITE(8,*) xxx,ex
      IF(iplp3 == 2) WRITE(8,*) xxx,ey
      IF(iplp3 == 3) WRITE(8,*) xxx,ez
      IF(iplp3 == 4) WRITE(8,*) xxx,ex,ey,ez
!***
    END DO
  END DO
END DO
RETURN
!
10    FORMAT (///,35X,'- - - near electric fields - - -',//,12X, &
    '-  location  -',21X,'-  ex  -',15X,'-  ey  -',15X,'-  ez  -',/,8X,'X', &
    10X,'Y',10X,'Z',10X,'MAGNITUDE',3X,'PHASE',6X,'MAGNITUDE',3X,'PHASE', &
    6X,'MAGNITUDE',3X,'PHASE',/,6X,'METERS',5X,'METERS',5X,'METERS',  &
    8X,'VOLTS/m',3X,'DEGREES',6X,'VOLTS/m',3X,'DEGREES',6X,'VOLTS/m',3x,'DEGREES')
11    FORMAT (2X,3(2X,f9.4),1X,3(3X,1P,e11.4,2X,0P,f7.2))
12    FORMAT (///,35X,'- - - near magnetic fields - - -',//,12X, &
    '-  location  -',21X,'-  hx  -',15X,'-  hy  -',15X,'-  hz  -',/,8X,'X', &
    10X,'Y',10X,'Z',10X,'MAGNITUDE',3X,'PHASE',6X,'MAGNITUDE',3X,'PHASE', &
    6X,'MAGNITUDE',3X,'PHASE',/,6X,'METERS',5X,'METERS',5X,'METERS',  &
    9X,'AMPS/M',3X,'DEGREES',7X,'AMPS/M',3X,'DEGREES',7X,'AMPS/M',3X,'DEGREES')
END SUBROUTINE nfpat
!----------------------------------------------------------------------------

SUBROUTINE nhfld (xob,yob,zob,hx,hy,hz)
!
!     NHFLD COMPUTES THE NEAR FIELD AT SPECIFIED POINTS IN SPACE AFTER
!     THE STRUCTURE CURRENTS HAVE BEEN COMPUTED.
!
USE nec2dpar
USE data
USE gnd
USE crnt
USE angl

IMPLICIT REAL(NEC2REAL)(a-h,o-z)

REAL(NEC2REAL), INTENT(IN)                         :: xob
REAL(NEC2REAL), INTENT(IN)                         :: yob
REAL(NEC2REAL), INTENT(IN)                         :: zob
COMPLEX*16, INTENT(OUT)                  :: hx
COMPLEX*16, INTENT(OUT)                  :: hy
COMPLEX*16, INTENT(OUT)                  :: hz
COMPLEX*16  acx,bcx,ccx,exk,eyk,ezk,exs,eys,ezs,exc, eyc,ezc
COMPLEX*16 con
COMPLEX*16 expx,exmx,expy,exmy,expz,exmz
COMPLEX*16 eypx,eymx,eypy,eymy,eypz,eymz
COMPLEX*16 ezpx,ezmx,ezpy,ezmy,ezpz,ezmz
COMMON /dataj/ s,b,xj,yj,zj,cabj,sabj,salpj,exk,eyk,ezk,exs,eys,  &
    ezs,exc,eyc,ezc,rkh,ind1,indd1,ind2,indd2,iexk,ipgnd
DIMENSION cab(1), sab(1)
DIMENSION t1x(1), t1y(1), t1z(1), t2x(1), t2y(1), t2z(1), xs(1), ys(1), zs(1)
EQUIVALENCE (t1x,si), (t1y,alp), (t1z,bet), (t2x,icon1), (t2y,icon2), &
    (t2z,itag), (xs,x), (ys,y), (zs,z)
EQUIVALENCE (t1xj,cabj), (t1yj,sabj), (t1zj,salpj), (t2xj,b), &
    (t2yj,ind1), (t2zj,ind2)
EQUIVALENCE (cab,alp), (sab,bet)

IF (iperf == 2) GO TO 6

hx=(0.,0.)
hy=(0.,0.)
hz=(0.,0.)
ax=0.
IF (n == 0) GO TO 4
DO  i=1,n
  xj=xob-x(i)
  yj=yob-y(i)
  zj=zob-z(i)
  zp=cab(i)*xj+sab(i)*yj+salp(i)*zj
  IF (ABS(zp) > 0.5001*si(i)) CYCLE
  zp=xj*xj+yj*yj+zj*zj-zp*zp
  xj=bi(i)
  IF (zp > 0.9*xj*xj) CYCLE
  ax=xj
  EXIT
END DO
2     DO  i=1,n
  s=si(i)
  b=bi(i)
  xj=x(i)
  yj=y(i)
  zj=z(i)
  cabj=cab(i)
  sabj=sab(i)
  salpj=salp(i)
  CALL hsfld (xob,yob,zob,ax)
  acx=DCMPLX(air(i),aii(i))
  bcx=DCMPLX(bir(i),bii(i))
  ccx=DCMPLX(cir(i),cii(i))
  hx=hx+exk*acx+exs*bcx+exc*ccx
  hy=hy+eyk*acx+eys*bcx+eyc*ccx
  hz=hz+ezk*acx+ezs*bcx+ezc*ccx
END DO
IF (m == 0) RETURN
4     jc=n
jl=ld+1
DO  i=1,m
  jl=jl-1
  s=bi(jl)
  xj=x(jl)
  yj=y(jl)
  zj=z(jl)
  t1xj=t1x(jl)
  t1yj=t1y(jl)
  t1zj=t1z(jl)
  t2xj=t2x(jl)
  t2yj=t2y(jl)
  t2zj=t2z(jl)
  CALL hintg (xob,yob,zob)
  jc=jc+3
  acx=t1xj*cur(jc-2)+t1yj*cur(jc-1)+t1zj*cur(jc)
  bcx=t2xj*cur(jc-2)+t2yj*cur(jc-1)+t2zj*cur(jc)
  hx=hx+acx*exk+bcx*exs
  hy=hy+acx*eyk+bcx*eys
  hz=hz+acx*ezk+bcx*ezs
END DO
RETURN
!
!     GET H BY FINITE DIFFERENCE OF E FOR SOMMERFELD GROUND
!     CON=j/(2*pi*eta)
!     DELT is the increment for getting central differences
!
6     delt=1.e-3
con=(0.,4.2246E-4)
CALL nefld (xob+delt,yob,zob,expx,eypx,ezpx)
CALL nefld (xob-delt,yob,zob,exmx,eymx,ezmx)
CALL nefld (xob,yob+delt,zob,expy,eypy,ezpy)
CALL nefld (xob,yob-delt,zob,exmy,eymy,ezmy)
CALL nefld (xob,yob,zob+delt,expz,eypz,ezpz)
CALL nefld (xob,yob,zob-delt,exmz,eymz,ezmz)
hx=con*(ezpy-ezmy-eypz+eymz)/(2.*delt)
hy=con*(expz-exmz-ezpx+ezmx)/(2.*delt)
hz=con*(eypx-eymx-expy+exmy)/(2.*delt)
RETURN
END SUBROUTINE nhfld
!----------------------------------------------------------------------------
!
!     PATCH GENERATES AND MODIFIES PATCH GEOMETRY DATA
!     NEW PATCHES.  FOR NX=0, NY=1,2,3,4 PATCH IS (RESPECTIVELY)
!     ARBITRARY, RECTAGULAR, TRIANGULAR, OR QUADRILATERAL.
!     FOR NX AND NY .GT. 0 A RECTANGULAR SURFACE IS PRODUCED WITH
!     NX BY NY RECTANGULAR PATCHES.
!
SUBROUTINE patch (nx,ny,x1,y1,z1,x2,y2,z2,x3,y3,z3,x4,y4,z4)
    USE nec2dpar
    USE data
    USE angl

    IMPLICIT REAL(NEC2REAL)(a-h,o-z)

    INTEGER, INTENT(IN)                      :: nx
    INTEGER, INTENT(IN)                      :: ny
    REAL(NEC2REAL), INTENT(IN)                       :: x1
    REAL(NEC2REAL), INTENT(IN)                       :: y1
    REAL(NEC2REAL), INTENT(IN)                       :: z1
    REAL(NEC2REAL), INTENT(IN)                       :: x2
    REAL(NEC2REAL), INTENT(IN)                       :: y2
    REAL(NEC2REAL), INTENT(IN)                       :: z2
    REAL(NEC2REAL), INTENT(IN)                       :: x3
    REAL(NEC2REAL), INTENT(IN)                       :: y3
    REAL(NEC2REAL), INTENT(IN)                       :: z3
    REAL(NEC2REAL), INTENT(IN)                       :: x4
    REAL(NEC2REAL), INTENT(IN)                       :: y4
    REAL(NEC2REAL), INTENT(IN)                       :: z4

    INTEGER                                  :: ix

    DIMENSION t1x(1), t1y(1), t1z(1), t2x(1), t2y(1), t2z(1)
    EQUIVALENCE (t1x,si), (t1y,alp), (t1z,bet), (t2x,icon1), (t2y,icon2), (t2z,itag)

    m=m+1
    mi=ld+1-m
    ntp=ny
    IF (nx > 0) ntp=2
    IF (ntp > 1) GO TO 2
    x(mi)=x1
    y(mi)=y1
    z(mi)=z1
    bi(mi)=z2
    znv=COS(x2)
    xnv=znv*COS(y2)
    ynv=znv*SIN(y2)
    znv=SIN(x2)
    xa=SQRT(xnv*xnv+ynv*ynv)
    IF (xa < 1.d-6) GO TO 1
    t1x(mi)=-ynv/xa
    t1y(mi)=xnv/xa
    t1z(mi)=0.
    GO TO 6
1     t1x(mi)=1.
    t1y(mi)=0.
    t1z(mi)=0.
    GO TO 6
2     s1x=x2-x1
    s1y=y2-y1
    s1z=z2-z1
    s2x=x3-x2
    s2y=y3-y2
    s2z=z3-z2
    IF (nx == 0) GO TO 3
    s1x=s1x/nx
    s1y=s1y/nx
    s1z=s1z/nx
    s2x=s2x/ny
    s2y=s2y/ny
    s2z=s2z/ny
3     xnv=s1y*s2z-s1z*s2y
    ynv=s1z*s2x-s1x*s2z
    znv=s1x*s2y-s1y*s2x
    xa=SQRT(xnv*xnv+ynv*ynv+znv*znv)
    xnv=xnv/xa
    ynv=ynv/xa
    znv=znv/xa
    xst=SQRT(s1x*s1x+s1y*s1y+s1z*s1z)
    t1x(mi)=s1x/xst
    t1y(mi)=s1y/xst
    t1z(mi)=s1z/xst
    IF (ntp > 2) GO TO 4
    x(mi)=x1+.5*(s1x+s2x)
    y(mi)=y1+.5*(s1y+s2y)
    z(mi)=z1+.5*(s1z+s2z)
    bi(mi)=xa
    GO TO 6

4     IF (ntp == 4) GO TO 5
    x(mi)=(x1+x2+x3)/3.
    y(mi)=(y1+y2+y3)/3.
    z(mi)=(z1+z2+z3)/3.
    bi(mi)=.5*xa
    GO TO 6

5     s1x=x3-x1
    s1y=y3-y1
    s1z=z3-z1
    s2x=x4-x1
    s2y=y4-y1
    s2z=z4-z1
    xn2=s1y*s2z-s1z*s2y
    yn2=s1z*s2x-s1x*s2z
    zn2=s1x*s2y-s1y*s2x
    xst=SQRT(xn2*xn2+yn2*yn2+zn2*zn2)
    salpn=1./(3.*(xa+xst))
    x(mi)=(xa*(x1+x2+x3)+xst*(x1+x3+x4))*salpn
    y(mi)=(xa*(y1+y2+y3)+xst*(y1+y3+y4))*salpn
    z(mi)=(xa*(z1+z2+z3)+xst*(z1+z3+z4))*salpn
    bi(mi)=.5*(xa+xst)
    s1x=(xnv*xn2+ynv*yn2+znv*zn2)/xst
    IF (s1x > 0.9998) GO TO 6
    WRITE(3,14)
    STOP

6     t2x(mi)=ynv*t1z(mi)-znv*t1y(mi)
    t2y(mi)=znv*t1x(mi)-xnv*t1z(mi)
    t2z(mi)=xnv*t1y(mi)-ynv*t1x(mi)
    salp(mi)=1.
    IF (nx == 0) GO TO 8
    m=m+nx*ny-1
    xn2=x(mi)-s1x-s2x
    yn2=y(mi)-s1y-s2y
    zn2=z(mi)-s1z-s2z
    xs=t1x(mi)
    ys=t1y(mi)
    zs=t1z(mi)
    xt=t2x(mi)
    yt=t2y(mi)
    zt=t2z(mi)
    mi=mi+1
    DO  iy=1,ny
      xn2=xn2+s2x
      yn2=yn2+s2y
      zn2=zn2+s2z
      DO  ix=1,nx
        xst=ix
        mi=mi-1
        x(mi)=xn2+xst*s1x
        y(mi)=yn2+xst*s1y
        z(mi)=zn2+xst*s1z
        bi(mi)=xa
        salp(mi)=1.
        t1x(mi)=xs
        t1y(mi)=ys
        t1z(mi)=zs
        t2x(mi)=xt
        t2y(mi)=yt
        t2z(mi)=zt
      END DO
    END DO
8     ipsym=0
    np=n
    mp=m
    RETURN

14    FORMAT (' error -- corners of quadrilateral patch DO NOT lie in a plane')

END SUBROUTINE patch
!----------------------------------------------------------------------------
!
!     DIVIDE PATCH FOR WIRE CONNECTION
!
SUBROUTINE subph (nx,ny,x1,y1,z1,x2,y2,z2,x3,y3,z3,x4,y4,z4)
    USE nec2dpar
    USE data
    USE angl

    IMPLICIT REAL(NEC2REAL)(a-h,o-z)

    INTEGER, INTENT(IN)                      :: nx
    INTEGER, INTENT(IN)                      :: ny
    REAL(NEC2REAL), INTENT(IN)                       :: x1
    REAL(NEC2REAL), INTENT(IN)                       :: y1
    REAL(NEC2REAL), INTENT(IN)                       :: z1
    REAL(NEC2REAL), INTENT(IN)                       :: x2
    REAL(NEC2REAL), INTENT(IN)                       :: y2
    REAL(NEC2REAL), INTENT(IN)                       :: z2
    REAL(NEC2REAL), INTENT(IN)                       :: x3
    REAL(NEC2REAL), INTENT(IN)                       :: y3
    REAL(NEC2REAL), INTENT(IN)                       :: z3
    REAL(NEC2REAL), INTENT(IN)                       :: x4
    REAL(NEC2REAL), INTENT(IN)                       :: y4
    REAL(NEC2REAL), INTENT(IN)                       :: z4

    INTEGER                                  :: ix

    DIMENSION t1x(1), t1y(1), t1z(1), t2x(1), t2y(1), t2z(1)

    EQUIVALENCE (t1x,si), (t1y,alp), (t1z,bet), (t2x,icon1), (t2y,icon2), (t2z,itag)

    IF (ny > 0) GO TO 10
    IF (nx == m) GO TO 10
    nxp=nx+1
    ix=ld-m
    DO  iy=nxp,m
      ix=ix+1
      nyp=ix-3
      x(nyp)=x(ix)
      y(nyp)=y(ix)
      z(nyp)=z(ix)
      bi(nyp)=bi(ix)
      salp(nyp)=salp(ix)
      t1x(nyp)=t1x(ix)
      t1y(nyp)=t1y(ix)
      t1z(nyp)=t1z(ix)
      t2x(nyp)=t2x(ix)
      t2y(nyp)=t2y(ix)
      t2z(nyp)=t2z(ix)
    END DO
10    mi=ld+1-nx
    xs=x(mi)
    ys=y(mi)
    zs=z(mi)
    xa=bi(mi)*.25
    xst=SQRT(xa)*.5
    s1x=t1x(mi)
    s1y=t1y(mi)
    s1z=t1z(mi)
    s2x=t2x(mi)
    s2y=t2y(mi)
    s2z=t2z(mi)
    saln=salp(mi)
    xt=xst
    yt=xst
    IF (ny > 0) GO TO 11
    mia=mi
    GO TO 12
11    m=m+1
    mp=mp+1
    mia=ld+1-m
12    DO  ix=1,4
      x(mia)=xs+xt*s1x+yt*s2x
      y(mia)=ys+xt*s1y+yt*s2y
      z(mia)=zs+xt*s1z+yt*s2z
      bi(mia)=xa
      t1x(mia)=s1x
      t1y(mia)=s1y
      t1z(mia)=s1z
      t2x(mia)=s2x
      t2y(mia)=s2y
      t2z(mia)=s2z
      salp(mia)=saln
      IF (ix == 2) yt=-yt
      IF (ix == 1.OR.ix == 3) xt=-xt
      mia=mia-1
    END DO
    m=m+3
    IF (nx <= mp) mp=mp+3
    IF (ny > 0) z(mi)=10000.
    RETURN
    !
END SUBROUTINE subph
!----------------------------------------------------------------------------

SUBROUTINE pcint (xi,yi,zi,cabi,sabi,salpi,e)
! ***
!     DOUBLE PRECISION 6/4/85
!
IMPLICIT REAL(NEC2REAL)(a-h,o-z)

REAL(NEC2REAL), INTENT(IN OUT)                     :: xi
REAL(NEC2REAL), INTENT(IN OUT)                     :: yi
REAL(NEC2REAL), INTENT(IN OUT)                     :: zi
REAL(NEC2REAL), INTENT(IN)                         :: cabi
REAL(NEC2REAL), INTENT(IN)                         :: sabi
REAL(NEC2REAL), INTENT(IN)                         :: salpi
COMPLEX*16, INTENT(OUT)                  :: e(9)
! ***
!     INTEGRATE OVER PATCHES AT WIRE CONNECTION POINT
COMPLEX*16 exk,eyk,ezk,exs,eys,ezs,exc,eyc,ezc, e1,e2,e3,e4,e5 ,e6,e7,e8,e9
COMMON /dataj/ s,b,xj,yj,zj,cabj,sabj,salpj,exk,eyk,ezk,exs,eys,  &
    ezs,exc,eyc,ezc,rkh,ind1,indd1,ind2,indd2,iexk,ipgnd

EQUIVALENCE (t1xj,cabj), (t1yj,sabj), (t1zj,salpj), (t2xj,b), &
    (t2yj,ind1), (t2zj,ind2)
DATA tpi/6.283185308D+0/,nint/10/

d=SQRT(s)*.5
ds=4.*d/dfloat(nint)
da=ds*ds
gcon=1./s
fcon=1./(2.*tpi*d)
xxj=xj
xyj=yj
xzj=zj
xs=s
s=da
s1=d+ds*.5
xss=xj+s1*(t1xj+t2xj)
yss=yj+s1*(t1yj+t2yj)
zss=zj+s1*(t1zj+t2zj)
s1=s1+d
s2x=s1
e1=(0.,0.)
e2=(0.,0.)
e3=(0.,0.)
e4=(0.,0.)
e5=(0.,0.)
e6=(0.,0.)
e7=(0.,0.)
e8=(0.,0.)
e9=(0.,0.)
DO  i1=1,nint
  s1=s1-ds
  s2=s2x
  xss=xss-ds*t1xj
  yss=yss-ds*t1yj
  zss=zss-ds*t1zj
  xj=xss
  yj=yss
  zj=zss
  DO  i2=1,nint
    s2=s2-ds
    xj=xj-ds*t2xj
    yj=yj-ds*t2yj
    zj=zj-ds*t2zj
    CALL unere (xi,yi,zi)
    exk=exk*cabi+eyk*sabi+ezk*salpi
    exs=exs*cabi+eys*sabi+ezs*salpi
    g1=(d+s1)*(d+s2)*gcon
    g2=(d-s1)*(d+s2)*gcon
    g3=(d-s1)*(d-s2)*gcon
    g4=(d+s1)*(d-s2)*gcon
    f2=(s1*s1+s2*s2)*tpi
    f1=s1/f2-(g1-g2-g3+g4)*fcon
    f2=s2/f2-(g1+g2-g3-g4)*fcon
    e1=e1+exk*g1
    e2=e2+exk*g2
    e3=e3+exk*g3
    e4=e4+exk*g4
    e5=e5+exs*g1
    e6=e6+exs*g2
    e7=e7+exs*g3
    e8=e8+exs*g4
    e9=e9+exk*f1+exs*f2
  END DO
END DO
e(1)=e1
e(2)=e2
e(3)=e3
e(4)=e4
e(5)=e5
e(6)=e6
e(7)=e7
e(8)=e8
e(9)=e9
xj=xxj
yj=xyj
zj=xzj
s=xs
RETURN
END SUBROUTINE pcint
!----------------------------------------------------------------------------

SUBROUTINE prnt(in1,in2,in3,fl1,fl2,fl3,fl4,fl5,fl6,ctype)
!
!     Purpose:
!     PRNT prints the input data for impedance loading, inserting blanks
!     for numbers that are zero.
!
!     INPUT:
!     IN1-3 = INTEGER VALUES TO BE PRINTED
!     FL1-6 = REAL VALUES TO BE PRINTED
!     CTYPE = CHARACTER STRING TO BE PRINTED
!
    IMPLICIT NONE

    INTEGER, INTENT(IN)                      :: in1
    INTEGER, INTENT(IN)                      :: in2
    INTEGER, INTENT(IN)                      :: in3
    REAL(NEC2REAL), INTENT(IN)                       :: fl1
    REAL(NEC2REAL), INTENT(IN)                       :: fl2
    REAL(NEC2REAL), INTENT(IN)                       :: fl3
    REAL(NEC2REAL), INTENT(IN)                       :: fl4
    REAL(NEC2REAL), INTENT(IN)                       :: fl5
    REAL(NEC2REAL), INTENT(IN)                       :: fl6
    CHARACTER (LEN=*), INTENT(IN)            :: ctype

    INTEGER                                  :: i
    CHARACTER (LEN=5)                        :: cint(3)
    CHARACTER (LEN=13)                       :: cflt(6)

    DO  i=1,3
      cint(i)='     '
    END DO
    IF(in1 == 0.AND.in2 == 0.AND.in3 == 0)THEN
      cint(1)='  ALL'
    ELSE
      IF(in1 /= 0)WRITE(cint(1),90)in1
      IF(in2 /= 0)WRITE(cint(2),90)in2
      IF(in3 /= 0)WRITE(cint(3),90)in3
    END IF
    DO  i=1,6
      cflt(i)='     '
    END DO
    IF(ABS(fl1) > 1.e-30)WRITE(cflt(1),91)fl1
    IF(ABS(fl2) > 1.e-30)WRITE(cflt(2),91)fl2
    IF(ABS(fl3) > 1.e-30)WRITE(cflt(3),91)fl3
    IF(ABS(fl4) > 1.e-30)WRITE(cflt(4),91)fl4
    IF(ABS(fl5) > 1.e-30)WRITE(cflt(5),91)fl5
    IF(ABS(fl6) > 1.e-30)WRITE(cflt(6),91)fl6
    WRITE(3,92)(cint(i),i=1,3),(cflt(i),i=1,6),ctype
    RETURN
!
90    FORMAT(i5)
91    FORMAT(1P,e13.4)
92    FORMAT(/,3X,3A,3X,6A,3X,a)
END SUBROUTINE prnt

!----------------------------------------------------------------------------

SUBROUTINE qdsrc (is,v,e)
! ***
!     DOUBLE PRECISION 6/4/85
!
USE nec2dpar
USE data
USE angl
USE zload
USE segj
USE vsorc

IMPLICIT REAL(NEC2REAL)(a-h,o-z)

INTEGER, INTENT(IN)                      :: is
COMPLEX*16, INTENT(IN)                   :: v
COMPLEX*16, INTENT(OUT)                  :: e(1)
! ***
!     FILL INCIDENT FIELD ARRAY FOR CHARGE DISCONTINUITY VOLTAGE SOURCE
COMPLEX*16 curd,ccj,exk,eyk,ezk,exs,eys,ezs,exc,eyc,ezc  &
    ,etk,ets,etc

COMMON /dataj/ s,b,xj,yj,zj,cabj,sabj,salpj,exk,eyk,ezk,exs,eys,  &
    ezs,exc,eyc,ezc,rkh,ind1,indd1,ind2,indd2,iexk,ipgnd
DIMENSION ccjx(2), cab(1), sab(1)
DIMENSION t1x(1), t1y(1), t1z(1), t2x(1), t2y(1), t2z(1)
EQUIVALENCE (ccj,ccjx), (cab,alp), (sab,bet)
EQUIVALENCE (t1x,si), (t1y,alp), (t1z,bet), (t2x,icon1), (t2y,icon2), (t2z,itag)
DATA tp/6.283185308D+0/,ccjx/0.,-.01666666667D+0/

i=icon1(is)
icon1(is)=0
CALL tbf (is,0)
icon1(is)=i
s=si(is)*.5
curd=ccj*v/((LOG(2.*s/bi(is))-1.)*(bx(jsno)*COS(tp*s)+cx(jsno)*SIN(tp*s))*wlam)
nqds=nqds+1
vqds(nqds)=v
iqds(nqds)=is
DO  jx=1,jsno
  j=jco(jx)
  s=si(j)
  b=bi(j)
  xj=x(j)
  yj=y(j)
  zj=z(j)
  cabj=cab(j)
  sabj=sab(j)
  salpj=salp(j)
  IF (iexk == 0) GO TO 16
  
  ipr=icon1(j)
  IF(ipr > 10000)GO TO 7
  IF (ipr < 0) THEN
    GO TO     1
  ELSE IF (ipr == 0) THEN
    GO TO     6
  ELSE
    GO TO     2
  END IF
  
  1     ipr=-ipr
  IF (-icon1(ipr) /= j) GO TO 7
  GO TO 4
  2     IF (ipr /= j) GO TO 3
  IF (cabj*cabj+sabj*sabj > 1.d-8) GO TO 7
  GO TO 5
  3     IF (icon2(ipr) /= j) GO TO 7
  4     xi=ABS(cabj*cab(ipr)+sabj*sab(ipr)+salpj*salp(ipr))
  IF (xi < 0.999999D+0) GO TO 7
  IF (ABS(bi(ipr)/b-1.) > 1.d-6) GO TO 7
  5     ind1=0
  GO TO 8
  6     ind1=1
  GO TO 8
  7     ind1=2
  
  8     ipr=icon2(j)
  IF(ipr > 10000)GO TO 15
  IF (ipr < 0) THEN
    GO TO     9
  ELSE IF (ipr == 0) THEN
    GO TO    14
  ELSE
    GO TO    10
  END IF
  
  9     ipr=-ipr
  IF (-icon2(ipr) /= j) GO TO 15
  GO TO 12
  10    IF (ipr /= j) GO TO 11
  IF (cabj*cabj+sabj*sabj > 1.d-8) GO TO 15
  GO TO 13
  11    IF (icon1(ipr) /= j) GO TO 15
  12    xi=ABS(cabj*cab(ipr)+sabj*sab(ipr)+salpj*salp(ipr))
  IF (xi < 0.999999D+0) GO TO 15
  IF (ABS(bi(ipr)/b-1.) > 1.d-6) GO TO 15
  13    ind2=0
  GO TO 16
  14    ind2=1
  GO TO 16
  15    ind2=2
  16    CONTINUE
  DO  i=1,n
    ij=i-j
    xi=x(i)
    yi=y(i)
    zi=z(i)
    ai=bi(i)
    CALL efld (xi,yi,zi,ai,ij)
    cabi=cab(i)
    sabi=sab(i)
    salpi=salp(i)
    etk=exk*cabi+eyk*sabi+ezk*salpi
    ets=exs*cabi+eys*sabi+ezs*salpi
    etc=exc*cabi+eyc*sabi+ezc*salpi
    e(i)=e(i)-(etk*ax(jx)+ets*bx(jx)+etc*cx(jx))*curd
  END DO
  IF (m == 0) GO TO 19
  ij=ld+1
  i1=n
  DO  i=1,m
    ij=ij-1
    xi=x(ij)
    yi=y(ij)
    zi=z(ij)
    CALL hsfld (xi,yi,zi,0.d0)
    i1=i1+1
    tx=t2x(ij)
    ty=t2y(ij)
    tz=t2z(ij)
    etk=exk*tx+eyk*ty+ezk*tz
    ets=exs*tx+eys*ty+ezs*tz
    etc=exc*tx+eyc*ty+ezc*tz
    e(i1)=e(i1)+(etk*ax(jx)+ets*bx(jx)+etc*cx(jx))*curd*salp(ij)
    i1=i1+1
    tx=t1x(ij)
    ty=t1y(ij)
    tz=t1z(ij)
    etk=exk*tx+eyk*ty+ezk*tz
    ets=exs*tx+eys*ty+ezs*tz
    etc=exc*tx+eyc*ty+ezc*tz
    e(i1)=e(i1)+(etk*ax(jx)+ets*bx(jx)+etc*cx(jx))*curd*salp(ij)
  END DO
  19    IF (nload > 0.OR.nlodf > 0) e(j)=e(j)+zarray(j)*curd*(ax(jx)+cx( jx))
END DO
RETURN
END SUBROUTINE qdsrc
!----------------------------------------------------------------------------

SUBROUTINE rdpat
! ***
!     DOUBLE PRECISION 6/4/85
!
USE nec2dpar
USE plot
USE data
USE save
USE gnd
USE fpat

PARAMETER(normax=4*maxseg)
IMPLICIT REAL(NEC2REAL)(a-h,o-z)
! ***
!     COMPUTE RADIATION PATTERN, GAIN, NORMALIZED GAIN

CHARACTER (LEN=6), DIMENSION(4), PARAMETER    :: igtp  = (/'    - ','POWER ','- DIRE','CTIVE '/)
CHARACTER (LEN=6), DIMENSION(3), PARAMETER    :: hpol  = (/'LINEAR','RIGHT ','LEFT  '/)
CHARACTER (LEN=1), PARAMETER                  :: hblk  = ' '
CHARACTER (LEN=6), PARAMETER                  :: hcir  = 'CIRCLE'
CHARACTER (LEN=6), DIMENSION(4), PARAMETER    :: igax  = (/' MAJOR',' MINOR',' VERT.',' HOR. '/)
CHARACTER (LEN=6), DIMENSION(10), PARAMETER   :: igntp = (/' MAJOR',' AXIS ',' MINOR',' AXIS ', &
    '   VER','TICAL ', ' HORIZ','ONTAL ','      ','TOTAL '/)
CHARACTER (LEN=6)                             :: hclif
CHARACTER (LEN=6)                             :: isens

REAL(NEC2REAL)                                        ::  ta = 1.745329252D-02
REAL(NEC2REAL)                                        ::  td = 57.29577951D+0

COMPLEX*16 eth,eph,erd,t1
COMMON /scratm/ gain(normax)

IF (ifar < 2) GO TO 2
WRITE(3,35)
IF (ifar <= 3) GO TO 1
WRITE(3,36)  nradl,scrwlt,scrwrt
IF (ifar == 4) GO TO 2
1     IF (ifar == 2.OR.ifar == 5) hclif=hpol(1)
IF (ifar == 3.OR.ifar == 6) hclif=hcir
cl=clt/wlam
ch=cht/wlam
zrati2=SQRT(1./DCMPLX(epsr2,-sig2*wlam*59.96))
WRITE(3,37)  hclif,clt,cht,epsr2,sig2
2     IF (ifar /= 1) GO TO 3
WRITE(3,41)
GO TO 5
3     i=2*ipd+1
j=i+1
itmp1=2*iax+1
itmp2=itmp1+1
WRITE(3,38)
IF (rfld < 1.d-20) GO TO 4
exrm=1./rfld
exra=rfld/wlam
exra=-360.*(exra-AINT(exra))
WRITE(3,39)  rfld,exrm,exra
4     WRITE(3,40)  igtp(i),igtp(j),igax(itmp1),igax(itmp2)
5     IF (ixtyp == 0.OR.ixtyp == 5) GO TO 7
IF (ixtyp == 4) GO TO 6
prad=0.
gcon=4.*pi/(1.+xpr6*xpr6)
gcop=gcon
GO TO 8
6     pinr=394.51*xpr6*xpr6*wlam*wlam
7     gcop=wlam*wlam*2.*pi/(376.73*pinr)
prad=pinr-ploss-pnlr
gcon=gcop
IF (ipd /= 0) gcon=gcon*pinr/prad
8     i=0
gmax=-1.e10
pint=0.
tmp1=dph*ta
tmp2=.5*dth*ta
phi=phis-dph
DO  kph=1,nph
  phi=phi+dph
  pha=phi*ta
  thet=thets-dth
  DO  kth=1,nth
    thet=thet+dth
    IF (ksymp == 2.AND.thet > 90.01.AND.ifar /= 1) CYCLE
    tha=thet*ta
    IF (ifar == 1) GO TO 9
    CALL ffld (tha,pha,eth,eph)
    GO TO 10
    9     CALL gfld (rfld/wlam,pha,thet/wlam,eth,eph,erd,zrati,ksymp)
    erdm=ABS(erd)
    erda=cang(erd)
    10    ethm2=dREAL(eth*DCONJG(eth))
    ethm=SQRT(ethm2)
    etha=cang(eth)
    ephm2=dREAL(eph*DCONJG(eph))
    ephm=SQRT(ephm2)
    epha=cang(eph)
    IF (ifar == 1) GO TO 28
!     ELLIPTICAL POLARIZATION CALC.
    IF (ethm2 > 1.d-20.OR.ephm2 > 1.d-20) GO TO 11
    tilta=0.
    emajr2=0.
    eminr2=0.
    axrat=0.
    isens=hblk
    GO TO 16
    11    dfaz=epha-etha
    IF (epha < 0.) GO TO 12
    dfaz2=dfaz-360.
    GO TO 13
    12    dfaz2=dfaz+360.
    13    IF (ABS(dfaz) > ABS(dfaz2)) dfaz=dfaz2
    cdfaz=COS(dfaz*ta)
    tstor1=ethm2-ephm2
    tstor2=2.*ephm*ethm*cdfaz
    tilta=.5*atgn2(tstor2,tstor1)
    stilta=SIN(tilta)
    tstor1=tstor1*stilta*stilta
    tstor2=tstor2*stilta*COS(tilta)
    emajr2=-tstor1+tstor2+ethm2
    eminr2=tstor1-tstor2+ephm2
    IF (eminr2 < 0.) eminr2=0.
    axrat=SQRT(eminr2/emajr2)
    tilta=tilta*td
    IF (axrat > 1.d-5) GO TO 14
    isens=hpol(1)
    GO TO 16
    14    IF (dfaz > 0.) GO TO 15
    isens=hpol(2)
    GO TO 16
    15    isens=hpol(3)
    16    gnmj=db10(gcon*emajr2)
    gnmn=db10(gcon*eminr2)
    gnv=db10(gcon*ethm2)
    gnh=db10(gcon*ephm2)
    gtot=db10(gcon*(ethm2+ephm2))
    IF (inor < 1) GO TO 23
    i=i+1
    IF (i > normax) GO TO 23
    SELECT CASE ( inor )
      CASE (    1)
        GO TO 17
      CASE (    2)
        GO TO 18
      CASE (    3)
        GO TO 19
      CASE (    4)
        GO TO 20
      CASE (    5)
        GO TO 21
    END SELECT
    17    tstor1=gnmj
    GO TO 22
    18    tstor1=gnmn
    GO TO 22
    19    tstor1=gnv
    GO TO 22
    20    tstor1=gnh
    GO TO 22
    21    tstor1=gtot
    22    gain(i)=tstor1
    IF (tstor1 > gmax) gmax=tstor1
    23    IF (iavp == 0) GO TO 24
    tstor1=gcop*(ethm2+ephm2)
    tmp3=tha-tmp2
    tmp4=tha+tmp2
    IF (kth == 1) tmp3=tha
    IF (kth == nth) tmp4=tha
    da=ABS(tmp1*(COS(tmp3)-COS(tmp4)))
    IF (kph == 1.OR.kph == nph) da=.5*da
    pint=pint+tstor1*da
    IF (iavp == 2) CYCLE
    24    IF (iax == 1) GO TO 25
    tmp5=gnmj
    tmp6=gnmn
    GO TO 26
    25    tmp5=gnv
    tmp6=gnh
    26    ethm=ethm*wlam
    ephm=ephm*wlam
    IF (rfld < 1.d-20) GO TO 27
    ethm=ethm*exrm
    etha=etha+exra
    ephm=ephm*exrm
    epha=epha+exra
    27    WRITE(3,42)  thet,phi,tmp5,tmp6,gtot,axrat,tilta,isens,ethm,etha  &
        ,ephm,epha
!      GO TO 29
!***
!28    WRITE(3,43)  RFLD,PHI,THET,ETHM,ETHA,EPHM,EPHA,ERDM,ERDA
    IF(iplp1 /= 3) GO TO 299
    IF(iplp3 == 0) GO TO 290
    IF(iplp2 == 1 .AND. iplp3 == 1) WRITE(8,*) thet,ethm,etha
    IF(iplp2 == 1 .AND. iplp3 == 2) WRITE(8,*) thet,ephm,epha
    IF(iplp2 == 2 .AND. iplp3 == 1) WRITE(8,*) phi,ethm,etha
    IF(iplp2 == 2 .AND. iplp3 == 2) WRITE(8,*) phi,ephm,epha
    IF(iplp4 == 0) GO TO 299
    290   IF(iplp2 == 1 .AND. iplp4 == 1) WRITE(8,*) thet,tmp5
    IF(iplp2 == 1 .AND. iplp4 == 2) WRITE(8,*) thet,tmp6
    IF(iplp2 == 1 .AND. iplp4 == 3) WRITE(8,*) thet,gtot
    IF(iplp2 == 2 .AND. iplp4 == 1) WRITE(8,*) phi,tmp5
    IF(iplp2 == 2 .AND. iplp4 == 2) WRITE(8,*) phi,tmp6
    IF(iplp2 == 2 .AND. iplp4 == 3) WRITE(8,*) phi,gtot
    GO TO 299
    28    WRITE(3,43)  rfld,phi,thet,ethm,etha,ephm,epha,erdm,erda
    299   CONTINUE
!***
  END DO
END DO
IF (iavp == 0) GO TO 30
tmp3=thets*ta
tmp4=tmp3+dth*ta*dfloat(nth-1)
tmp3=ABS(dph*ta*dfloat(nph-1)*(COS(tmp3)-COS(tmp4)))
pint=pint/tmp3
tmp3=tmp3/pi
WRITE(3,44)  pint,tmp3
30    IF (inor == 0) GO TO 34
IF (ABS(gnor) > 1.d-20) gmax=gnor
itmp1=(inor-1)*2+1
itmp2=itmp1+1
WRITE(3,45)  igntp(itmp1),igntp(itmp2),gmax
itmp2=nph*nth
IF (itmp2 > normax) itmp2=normax
itmp1=(itmp2+2)/3
itmp2=itmp1*3-itmp2
itmp3=itmp1
itmp4=2*itmp1
IF (itmp2 == 2) itmp4=itmp4-1
DO  i=1,itmp1
  itmp3=itmp3+1
  itmp4=itmp4+1
  j=(i-1)/nth
  tmp1=thets+dfloat(i-j*nth-1)*dth
  tmp2=phis+dfloat(j)*dph
  j=(itmp3-1)/nth
  tmp3=thets+dfloat(itmp3-j*nth-1)*dth
  tmp4=phis+dfloat(j)*dph
  j=(itmp4-1)/nth
  tmp5=thets+dfloat(itmp4-j*nth-1)*dth
  tmp6=phis+dfloat(j)*dph
  tstor1=gain(i)-gmax
  IF (i == itmp1.AND.itmp2 /= 0) GO TO 32
  tstor2=gain(itmp3)-gmax
  pint=gain(itmp4)-gmax
  WRITE(3,46)  tmp1,tmp2,tstor1,tmp3,tmp4,tstor2,tmp5,tmp6,pint
END DO
GO TO 34
32    IF (itmp2 == 2) GO TO 33
tstor2=gain(itmp3)-gmax
WRITE(3,46)  tmp1,tmp2,tstor1,tmp3,tmp4,tstor2
GO TO 34
33    WRITE(3,46)  tmp1,tmp2,tstor1
34    RETURN
!
35    FORMAT (///,31X,'- - - FAR FIELD GROUND PARAMETERS - - -',//)
36    FORMAT (40X,'RADIAL WIRE GROUND SCREEN',/,40X,i5,' WIRES',/,40X, &
    'WIRE LENGTH=',f8.2,' METERS',/,40X,'WIRE RADIUS=',1P,e10.3,' METERS')
37    FORMAT (40X,a6,' CLIFF',/,40X,'EDGE DISTANCE=',f9.2,' METERS',/,40X, &
    'HEIGHT=',f8.2,' METERS',/,40X,'SECOND MEDIUM -',/,40X, &
    'RELATIVE DIELECTRIC CONST.=',f7.3,/,40X,'CONDUCTIVITY=',1P,e10.3, ' MHOS')
38    FORMAT (///,48X,'- - - RADIATION PATTERNS - - -')
39    FORMAT (54X,'RANGE=',1P,e13.6,' METERS',/,54X,'EXP(-JKR)/R=',  &
    e12.5,' AT PHASE',0P,f7.2,8H degrees,/)
40    FORMAT (/,2X,'- - ANGLES - -',7X,2A6,'GAINS -',7X, &
    '- - - POLARIZATION - - -',4X,'- - - E(THETA) - - -',4X,'- - - E(PHI) - - -',  &
    /,2X,'THETA',5X,'PHI',7X,a6,2X,a6,3X,'TOTAL',6X,'AXIAL',5X,'TILT',  &
    3X,'SENSE',2(5X,'MAGNITUDE',4X,'PHASE '),/,2(1X,'DEGREES',1X), &
    3(6X,'DB'),8X,'RATIO',5X,'DEG.',8X,2(6X,'VOLTS/M',4X,'DEGREES'))
41    FORMAT (///,28X,' - - - RADIATED FIELDS NEAR GROUND - - -',//,8X,  &
    '- - - LOCATION - - -',10X,'- - E(THETA) - -',8X,'- - E(PHI) - -', &
    8X,'- - E(RADIAL) - -',/,7X,'RHO',6X,'PHI',9X,'Z',12X,'MAG',6X  &
    ,'PHASE',9X,'MAG',6X,'PHASE',9X,'MAG',6X,'PHASE',/,5X,'METERS',3X,  &
    'DEGREES',4X,'METERS',8X,'VOLTS/M',3X,'DEGREES',6X,'VOLTS/M',3X, &
    'DEGREES',6X,'VOLTS/M',3X,'DEGREES',/)
42    FORMAT(1X,f7.2,f9.2,3X,3F8.2,f11.5,f9.2,2X,a6,2(1P,e15.5,0P,f9.2))
43    FORMAT (3X,f9.2,2X,f7.2,2X,f9.2,1X,3(3X,1P,e11.4,2X,0P,f7.2))
44    FORMAT (//,3X,'AVERAGE POWER GAIN=',1P,e12.5,7X, &
    'SOLID ANGLE USED IN AVERAGING=(',0P,f7.4,')*PI STERADIANS.',//)
45    FORMAT (//,37X,'- - - - NORMALIZED GAIN - - - -',//,37X,2A6, &
    'GAIN',/,38X,'NORMALIZATION factor =',f9.2,' DB',//, &
    3(4X,'- - ANGLES - -',6X,'GAIN',7X),/,3(4X,'THETA',5X,'PHI',8X,'DB',8X), &
    /,3(3X,'DEGREES',2X,'DEGREES',16X))
46    FORMAT (3(1X,2F9.2,1X,f9.2,6X))
END SUBROUTINE rdpat
!----------------------------------------------------------------------------

SUBROUTINE readgm(inunit,code,i1,i2,r1,r2,r3,r4,r5,r6,r7)
!
!  READGM reads a geometry record and parses it.
!
!  *****  Passed variables
!     CODE        two letter mnemonic code
!     I1 - I2     INTEGER values from record
!     R1 - R7     REAL values from record
!
    IMPLICIT REAL(NEC2REAL)(a-h,o-z)

    INTEGER, INTENT(IN)                      :: inunit
    CHARACTER (LEN=*), INTENT(OUT)           :: code
    INTEGER, INTENT(OUT)                     :: i1
    INTEGER, INTENT(OUT)                     :: i2
    REAL(NEC2REAL), INTENT(OUT)                      :: r1
    REAL(NEC2REAL), INTENT(OUT)                      :: r2
    REAL(NEC2REAL), INTENT(OUT)                      :: r3
    REAL(NEC2REAL), INTENT(OUT)                      :: r4
    REAL(NEC2REAL), INTENT(OUT)                      :: r5
    REAL(NEC2REAL), INTENT(OUT)                      :: r6
    REAL(NEC2REAL), INTENT(OUT)                      :: r7

    DIMENSION intval(2),reaval(7)
!
!  Call the routine to read the record and parse it.
!
    CALL parsit(inunit,2,7,code,intval,reaval,ieof)
    !
    !  Set the return variables to the buffer array elements.
    !
    IF(ieof < 0)code='GE'
    i1=intval(1)
    i2=intval(2)
    r1=reaval(1)
    r2=reaval(2)
    r3=reaval(3)
    r4=reaval(4)
    r5=reaval(5)
    r6=reaval(6)
    r7=reaval(7)
    RETURN
END SUBROUTINE readgm
!----------------------------------------------------------------------------

SUBROUTINE readmn(inunit,code,i1,i2,i3,i4,f1,f2,f3,f4,f5,f6)
!
!  READMN reads a control record and parses it.
!
    IMPLICIT NONE

    INTEGER, INTENT(IN)                      :: inunit
    CHARACTER (LEN=*), INTENT(OUT)           :: code
    INTEGER, INTENT(OUT)                     :: i1
    INTEGER, INTENT(OUT)                     :: i2
    INTEGER, INTENT(OUT)                     :: i3
    INTEGER, INTENT(OUT)                     :: i4
    REAL(NEC2REAL), INTENT(OUT)                      :: f1
    REAL(NEC2REAL), INTENT(OUT)                      :: f2
    REAL(NEC2REAL), INTENT(OUT)                      :: f3
    REAL(NEC2REAL), INTENT(OUT)                      :: f4
    REAL(NEC2REAL), INTENT(OUT)                      :: f5
    REAL(NEC2REAL), INTENT(OUT)                      :: f6

    INTEGER, DIMENSION(4)                    :: intval
    REAL(NEC2REAL), DIMENSION(6)                     :: reaval
    INTEGER                                  :: ieof
    !
    !  Call the routine to read the record and parse it.
    !
    CALL parsit(inunit,4,6,code,intval,reaval,ieof)
    !
    !  Set the return variables to the buffer array elements.
    IF(ieof < 0) code='EN'
    i1=intval(1)
    i2=intval(2)
    i3=intval(3)
    i4=intval(4)
    f1=reaval(1)
    f2=reaval(2)
    f3=reaval(3)
    f4=reaval(4)
    f5=reaval(5)
    f6=reaval(6)
    RETURN
END SUBROUTINE readmn
!----------------------------------------------------------------------------
!
!
SUBROUTINE parsit(inunit,maxint,maxrea,cmnd,intfld,reafld,ieof)

!  UPDATED:  21 July 87

!  Called by:   READGM    READMN

!  PARSIT reads an input record and parses it.

!  *****  Passed variables
!     MAXINT     total number of INTEGERs in record
!     MAXREA     total number of REAL values in record
!     CMND       two letter mnemonic code
!     INTFLD     INTEGER values from record
!     REAFLD     REAL values from record

!  *****  Internal Variables
!     BGNFLD     list of starting indices
!     BUFFER     text buffer
!     ENDFLD     list of ending indices
!     FLDTRM     flag to indicate that pointer is in field position
!     REC        input line as read
!     TOTCOL     total number of columns in REC
!     TOTFLD     number of numeric fields
    USE nec2dpar, ONLY : debugging

    IMPLICIT REAL(NEC2REAL)(a-h,o-z)

    INTEGER, INTENT(IN)                      :: inunit
    INTEGER, INTENT(IN)                      :: maxint
    INTEGER, INTENT(IN)                      :: maxrea
    CHARACTER (LEN=2), INTENT(OUT)           :: cmnd
    INTEGER, INTENT(OUT)                     :: intfld(maxint)
    REAL(NEC2REAL), INTENT(OUT)                      :: reafld(maxrea)
    INTEGER, INTENT(IN OUT)                  :: ieof

    !  *****  Global variables
    CHARACTER (LEN=80) :: ngfnam
    COMMON /ngfnam/ ngfnam

    CHARACTER (LEN=80) :: REC
    CHARACTER (LEN=20) :: buffer

    INTEGER :: bgnfld(12), endfld(12), totcol, totfld
    LOGICAL :: fldtrm

    !
    READ(inunit, 8000, IOSTAT=ieof) REC

if (debugging) THEN
    write (*,7193) REC
7193 format (' parsit() card read: REC=',a)
    END IF

    CALL upcase( REC, REC, totcol )

    !
    !  Store opcode and clear field arrays.
    !
    cmnd= REC(1:2)
    DO  i=1,maxint
      intfld(i)= 0
    END DO
    DO  i=1,maxrea
      reafld(i)= 0.0
    END DO
    DO  i=1,12
      bgnfld(i)= 0
      endfld(i)= 0
    END DO

    !
    !  Find the beginning and ending of each field as well as the total number of
    !  fields.
    !
    totfld= 0
    fldtrm= .false.
    last= maxrea + maxint
    DO  j=3,totcol
      k= ICHAR( REC(j:j) )
    !
    !  Check for end of line comment (`!').  This is a new modification to allow
    !  VAX-like comments at the end of data records, i.e.
    !       GW 1 7 0 0 0 0 0 .5 .0001 ! DIPOLE WIRE
    !       GE ! END OF GEOMETRY
    !
      IF (k == 33) THEN                    ! .eq. '!'
        IF (fldtrm) endfld(totfld)= j - 1
        GO TO 5000
    !
    !  Set the ending index when the character is a comma or space and the pointer
    !  is in a field position (FLDTRM = .TRUE.).
    !
      ELSE IF (k == 32  .OR.  k == 44) THEN    ! space or comma ?
        IF (fldtrm) THEN
          endfld(totfld)= j - 1
          fldtrm= .false.
        END IF
    !
    !  Set the beginning index when the character is not a comma or space and the
    !  pointer is not currently in a field position (FLDTRM = .FALSE).
    !
      ELSE IF (.NOT. fldtrm) THEN
        totfld= totfld + 1
        fldtrm= .true.
        bgnfld(totfld)= j
      END IF
    END DO
    IF (fldtrm) endfld(totfld)= totcol

    !  Check to see if the total number of value fields is within the precribed
    !  limits.

5000  IF ((cmnd == 'WG').OR.(cmnd == 'GF')) THEN  ! Init default NGFNAM
      ngfnam='NGF2D.NEC'
    END IF
    IF (totfld == 0) THEN
      RETURN
    ELSE IF (totfld > last) THEN
      WRITE(3, 8001 )
      GO TO 9010
    END IF
    j= MIN( totfld, maxint )

    !  Parse out INTEGER values and store into INTEGER buffer array.

    DO  i=1,j
      length= endfld(i) - bgnfld(i) + 1
      buffer= REC(bgnfld(i):endfld(i))
      
      IF (((cmnd == 'WG').OR.(cmnd == 'GF')).AND.  &
            (buffer(1:1) /= '0') .AND. (buffer(1:1) /= '1')) THEN       ! Text field
        ngfnam = REC(bgnfld(i):endfld(i))
        RETURN
      END IF
      
      ind= INDEX( buffer(1:length), '.' )
      IF (ind > 0  .AND.  ind < length) GO TO 9000
      IF (ind == length) length= length - 1
      READ( buffer(1:length), *, ERR=9000 ) intfld(i)
    END DO

    !  Parse out REAL values and store into REAL buffer array.

    IF (totfld > maxint) THEN
      j= maxint + 1
      DO  i=j,totfld
        length= endfld(i) - bgnfld(i) + 1
        buffer= REC(bgnfld(i):endfld(i))
        ind= INDEX( buffer(1:length), '.' )
        IF (ind == 0) THEN
          inde= INDEX( buffer(1:length), 'E' )
          length= length + 1
          IF (inde == 0) THEN
            buffer(length:length)= '.'
          ELSE
            buffer= buffer(1:inde-1)//'.'// buffer(inde:length-1)
          END IF
        END IF
        READ( buffer(1:length), *, ERR=9000 ) reafld(i-maxint)
      END DO
    END IF
    RETURN

    !  Print out text of record line when error occurs.

9000   IF (i <= maxint) THEN
      WRITE(3, 8002 ) i
    ELSE
      i= i - maxint
      WRITE(3, 8003 ) i
    END IF
9010   WRITE(3, 8004 ) REC
    STOP 'CARD ERROR'
!
!  Input formats and output messages.
!
8000   FORMAT (a80)
8001   FORMAT (//,' ***** CARD ERROR - TOO MANY FIELDS IN RECORD')
8002   FORMAT (//,' ***** CARD ERROR - INVALID NUMBER AT INTEGER',' POSITION ',i1)
8003   FORMAT (//,' ***** CARD ERROR - INVALID NUMBER AT REAL',' POSITION ',i1)
8004   FORMAT (' ***** TEXT -->  ',a80)
END SUBROUTINE parsit
!----------------------------------------------------------------------------

SUBROUTINE upcase( intext, outtxt, length )
!
!  UPCASE finds the length of INTEXT and converts it to upper case.
!

    CHARACTER (LEN=*), INTENT(IN)            :: intext
    CHARACTER (LEN=*), INTENT(OUT)           :: outtxt
    INTEGER, INTENT(OUT)                     :: length

    length = LEN( intext )
    DO  i=1,length
        j  = ICHAR( intext(i:i) )
        IF (j >= 96) j = j - 32
        outtxt(i:i) = CHAR( j )
    END DO
    RETURN
END SUBROUTINE upcase
!----------------------------------------------------------------------------

SUBROUTINE reblk (b,bx,nb,nbx,n2c)
! ***
!     DOUBLE PRECISION 6/4/85
!
    USE matpar

    IMPLICIT REAL(NEC2REAL)(a-h,o-z)

    COMPLEX*16, INTENT(OUT)                  :: b(nb,1)
    COMPLEX*16, INTENT(IN OUT)               :: bx(nbx,1)
    INTEGER, INTENT(IN)                      :: nb
    INTEGER, INTENT(IN)                      :: nbx
    INTEGER, INTENT(IN)                      :: n2c
    ! ***
    !     REBLOCK ARRAY B IN N.G.F. SOLUTION FROM BLOCKS OF ROWS ON TAPE14
    !     TO BLOCKS OF COLUMNS ON TAPE16
    REWIND 16

    nib=0
    npb=npbl
    DO  ib=1,nbbl
      IF (ib == nbbl) npb=nlbl
      REWIND 14
      nix=0
      npx=npbx
      DO  ibx=1,nbbx
        IF (ibx == nbbx) npx=nlbx
        READ (14) ((bx(i,j),i=1,npx),j=1,n2c)
        DO  i=1,npx
          ix=i+nix
          DO  j=1,npb
            b(ix,j)=bx(i,j+nib)
          END DO
        END DO
        nix=nix+npbx
      END DO
      WRITE (16) ((b(i,j),i=1,nb),j=1,npb)
      nib=nib+npbl
    END DO
    REWIND 14
    REWIND 16
    RETURN
END SUBROUTINE reblk
!----------------------------------------------------------------------------

SUBROUTINE reflc (ix,iy,iz,itx,nop)
! ***
!     DOUBLE PRECISION 6/4/85
!
USE nec2dpar
USE data
USE angl

IMPLICIT REAL(NEC2REAL)(a-h,o-z)

INTEGER, INTENT(IN OUT)                  :: ix
INTEGER, INTENT(IN OUT)                  :: iy
INTEGER, INTENT(IN OUT)                  :: iz
INTEGER, INTENT(IN)                      :: itx
INTEGER, INTENT(IN)                      :: nop
! ***
!
!     REFLC REFLECTS PARTIAL STRUCTURE ALONG X,Y, OR Z AXES OR ROTATES
!     STRUCTURE TO COMPLETE A SYMMETRIC STRUCTURE.
!
DIMENSION t1x(1), t1y(1), t1z(1), t2x(1), t2y(1), t2z(1), x2(1), y2(1), z2(1)
EQUIVALENCE (t1x,si), (t1y,alp), (t1z,bet), (t2x,icon1), &
    (t2y,icon2), (t2z,itag), (x2,si), (y2,alp), (z2,bet)

np=n
mp=m
ipsym=0
iti=itx
IF (ix < 0) GO TO 19
IF (nop == 0) RETURN
ipsym=1
IF (iz == 0) GO TO 6
!
!     REFLECT ALONG Z AXIS
!
ipsym=2
IF (n < n2) GO TO 3
DO  i=n2,n
  nx=i+n-n1
  e1=z(i)
  e2=z2(i)
  IF (ABS(e1)+ABS(e2) > 1.d-5.AND.e1*e2 >= -1.d-6) GO TO 1
  WRITE(3,24)  i
  STOP
  1     x(nx)=x(i)
  y(nx)=y(i)
  z(nx)=-e1
  x2(nx)=x2(i)
  y2(nx)=y2(i)
  z2(nx)=-e2
  itagi=itag(i)
  IF (itagi == 0) itag(nx)=0
  IF (itagi /= 0) itag(nx)=itagi+iti
  bi(nx)=bi(i)
END DO
n=n*2-n1
iti=iti*2
3     IF (m < m2) GO TO 6
nxx=ld+1-m1
DO  i=m2,m
  nxx=nxx-1
  nx=nxx-m+m1
  IF (ABS(z(nxx)) > 1.d-10) GO TO 4
  WRITE(3,25)  i
  STOP
  4     x(nx)=x(nxx)
  y(nx)=y(nxx)
  z(nx)=-z(nxx)
  t1x(nx)=t1x(nxx)
  t1y(nx)=t1y(nxx)
  t1z(nx)=-t1z(nxx)
  t2x(nx)=t2x(nxx)
  t2y(nx)=t2y(nxx)
  t2z(nx)=-t2z(nxx)
  salp(nx)=-salp(nxx)
  bi(nx)=bi(nxx)
END DO
m=m*2-m1
6     IF (iy == 0) GO TO 12
!
!     REFLECT ALONG Y AXIS
!
IF (n < n2) GO TO 9
DO  i=n2,n
  nx=i+n-n1
  e1=y(i)
  e2=y2(i)
  IF (ABS(e1)+ABS(e2) > 1.d-5.AND.e1*e2 >= -1.d-6) GO TO 7
  WRITE(3,24)  i
  STOP
  7     x(nx)=x(i)
  y(nx)=-e1
  z(nx)=z(i)
  x2(nx)=x2(i)
  y2(nx)=-e2
  z2(nx)=z2(i)
  itagi=itag(i)
  IF (itagi == 0) itag(nx)=0
  IF (itagi /= 0) itag(nx)=itagi+iti
  bi(nx)=bi(i)
END DO
n=n*2-n1
iti=iti*2
9     IF (m < m2) GO TO 12
nxx=ld+1-m1
DO  i=m2,m
  nxx=nxx-1
  nx=nxx-m+m1
  IF (ABS(y(nxx)) > 1.d-10) GO TO 10
  WRITE(3,25)  i
  STOP
  10    x(nx)=x(nxx)
  y(nx)=-y(nxx)
  z(nx)=z(nxx)
  t1x(nx)=t1x(nxx)
  t1y(nx)=-t1y(nxx)
  t1z(nx)=t1z(nxx)
  t2x(nx)=t2x(nxx)
  t2y(nx)=-t2y(nxx)
  t2z(nx)=t2z(nxx)
  salp(nx)=-salp(nxx)
  bi(nx)=bi(nxx)
END DO
m=m*2-m1
12    IF (ix == 0) GO TO 18
!
!     REFLECT ALONG X AXIS
!
IF (n < n2) GO TO 15
DO  i=n2,n
  nx=i+n-n1
  e1=x(i)
  e2=x2(i)
  IF (ABS(e1)+ABS(e2) > 1.d-5.AND.e1*e2 >= -1.d-6) GO TO 13
  WRITE(3,24)  i
  STOP
  13    x(nx)=-e1
  y(nx)=y(i)
  z(nx)=z(i)
  x2(nx)=-e2
  y2(nx)=y2(i)
  z2(nx)=z2(i)
  itagi=itag(i)
  IF (itagi == 0) itag(nx)=0
  IF (itagi /= 0) itag(nx)=itagi+iti
  bi(nx)=bi(i)
END DO
n=n*2-n1
15    IF (m < m2) GO TO 18
nxx=ld+1-m1
DO  i=m2,m
  nxx=nxx-1
  nx=nxx-m+m1
  IF (ABS(x(nxx)) > 1.d-10) GO TO 16
  WRITE(3,25)  i
  STOP
  16    x(nx)=-x(nxx)
  y(nx)=y(nxx)
  z(nx)=z(nxx)
  t1x(nx)=-t1x(nxx)
  t1y(nx)=t1y(nxx)
  t1z(nx)=t1z(nxx)
  t2x(nx)=-t2x(nxx)
  t2y(nx)=t2y(nxx)
  t2z(nx)=t2z(nxx)
  salp(nx)=-salp(nxx)
  bi(nx)=bi(nxx)
END DO
m=m*2-m1
18    RETURN
!
!     REPRODUCE STRUCTURE WITH ROTATION TO FORM CYLINDRICAL STRUCTURE
!
19    fnop=nop
ipsym=-1
sam=6.283185308D+0/fnop
cs=COS(sam)
ss=SIN(sam)
IF (n < n2) GO TO 21
n=n1+(n-n1)*nop
nx=np+1
DO  i=nx,n
  k=i-np+n1
  xk=x(k)
  yk=y(k)
  x(i)=xk*cs-yk*ss
  y(i)=xk*ss+yk*cs
  z(i)=z(k)
  xk=x2(k)
  yk=y2(k)
  x2(i)=xk*cs-yk*ss
  y2(i)=xk*ss+yk*cs
  z2(i)=z2(k)
  itagi=itag(k)
  IF (itagi == 0) itag(i)=0
  IF (itagi /= 0) itag(i)=itagi+iti
  bi(i)=bi(k)
END DO
21    IF (m < m2) GO TO 23
m=m1+(m-m1)*nop
nx=mp+1
k=ld+1-m1
DO  i=nx,m
  k=k-1
  j=k-mp+m1
  xk=x(k)
  yk=y(k)
  x(j)=xk*cs-yk*ss
  y(j)=xk*ss+yk*cs
  z(j)=z(k)
  xk=t1x(k)
  yk=t1y(k)
  t1x(j)=xk*cs-yk*ss
  t1y(j)=xk*ss+yk*cs
  t1z(j)=t1z(k)
  xk=t2x(k)
  yk=t2y(k)
  t2x(j)=xk*cs-yk*ss
  t2y(j)=xk*ss+yk*cs
  t2z(j)=t2z(k)
  salp(j)=salp(k)
  bi(j)=bi(k)
END DO
23    RETURN
!
24    FORMAT (' GEOMETRY DATA ERROR--SEGMENT',i5,' LIES IN PLANE OF SYMMETRY')
25    FORMAT (' GEOMETRY DATA ERROR--PATCH',i4,' LIES IN PLANE OF SYMMETRY')
END SUBROUTINE reflc
!----------------------------------------------------------------------------
!
!     FOR THE SOMMERFELD GROUND OPTION, ROM2 INTEGRATES OVER THE SOURCE
!     SEGMENT TO OBTAIN THE TOTAL FIELD DUE TO GROUND.  THE METHOD OF
!     VARIABLE INTERVAL WIDTH ROMBERG INTEGRATION IS USED.  THERE ARE 9
!     FIELD COMPONENTS - THE X, Y, AND Z COMPONENTS DUE TO CONSTANT,
!     SINE, AND COSINE CURRENT DISTRIBUTIONS.
!
SUBROUTINE rom2 (a,b,sum,dmin)
IMPLICIT REAL(NEC2REAL)(a-h,o-z)

REAL(NEC2REAL), INTENT(IN)                         :: a
REAL(NEC2REAL), INTENT(IN)                         :: b
COMPLEX*16, INTENT(OUT)                    :: sum(9)
REAL(NEC2REAL), INTENT(IN OUT)                     :: dmin

REAL(NEC2REAL)                                     :: rx  = 1.d-4
INTEGER                                    :: nm  = 65536
INTEGER                                    :: nts = 4
INTEGER                                    :: nx  = 1
INTEGER                                    :: n   = 9

COMPLEX*16  g1,g2,g3,g4,g5,t00,t01,t10,t02,t11,t20
DIMENSION  g1(9), g2(9), g3(9), g4(9), g5(9), t01(9), t10(9 ), t20(9)


z=a
ze=b
s=b-a
IF (s >= 0.) GO TO 1
WRITE(3,18)
STOP
1     ep=s/(1.e4*nm)
zend=ze-ep
DO  i=1,n
  sum(i)=(0.,0.)
END DO
ns=nx
nt=0
CALL sflds (z,g1)
3     dz=s/ns
IF (z+dz <= ze) GO TO 4
dz=ze-z
IF (dz <= ep) GO TO 17
4     dzot=dz*.5
CALL sflds (z+dzot,g3)
CALL sflds (z+dz,g5)
5     tmag1=0.
tmag2=0.
!
!     EVALUATE 3 POINT ROMBERG RESULT AND TEST CONVERGENCE.
!
DO  i=1,n
  t00=(g1(i)+g5(i))*dzot
  t01(i)=(t00+dz*g3(i))*.5
  t10(i)=(4.*t01(i)-t00)/3.
  IF (i > 3) CYCLE
  tr=dREAL(t01(i))
  ti=DIMAG(t01(i))
  tmag1=tmag1+tr*tr+ti*ti
  tr=dREAL(t10(i))
  ti=DIMAG(t10(i))
  tmag2=tmag2+tr*tr+ti*ti
END DO
tmag1=SQRT(tmag1)
tmag2=SQRT(tmag2)
CALL test(tmag1,tmag2,tr,0.d0,0.d0,ti,dmin)
IF(tr > rx)GO TO 8
DO  i=1,n
  sum(i)=sum(i)+t10(i)
END DO
nt=nt+2
GO TO 12
8     CALL sflds (z+dz*.25,g2)
CALL sflds (z+dz*.75,g4)
tmag1=0.
tmag2=0.
!
!     EVALUATE 5 POINT ROMBERG RESULT AND TEST CONVERGENCE.
!
DO  i=1,n
  t02=(t01(i)+dzot*(g2(i)+g4(i)))*.5
  t11=(4.*t02-t01(i))/3.
  t20(i)=(16.*t11-t10(i))/15.
  IF (i > 3) CYCLE
  tr=dREAL(t11)
  ti=DIMAG(t11)
  tmag1=tmag1+tr*tr+ti*ti
  tr=dREAL(t20(i))
  ti=DIMAG(t20(i))
  tmag2=tmag2+tr*tr+ti*ti
END DO
tmag1=SQRT(tmag1)
tmag2=SQRT(tmag2)
CALL test(tmag1,tmag2,tr,0.d0,0.d0,ti,dmin)
IF(tr > rx)GO TO 14
10    DO  i=1,n
  sum(i)=sum(i)+t20(i)
END DO
nt=nt+1
12    z=z+dz
IF (z > zend) GO TO 17
DO  i=1,n
  g1(i)=g5(i)
END DO
IF (nt < nts.OR.ns <= nx) GO TO 3
ns=ns/2
nt=1
GO TO 3
14    nt=0
IF (ns < nm) GO TO 15
WRITE(3,19)  z
GO TO 10
15    ns=ns*2
dz=s/ns
dzot=dz*.5
DO  i=1,n
  g5(i)=g3(i)
  g3(i)=g2(i)
END DO
GO TO 5
17    CONTINUE
RETURN
!
18    FORMAT (30H error - b less than a in rom2)
19    FORMAT (33H rom2 -- step size limited at z =,1P,e12.5)
END SUBROUTINE rom2
!----------------------------------------------------------------------------

SUBROUTINE sbf (i,is,aa,bb,cc)
! ***
!     DOUBLE PRECISION 6/4/85
!
USE nec2dpar
USE data

IMPLICIT REAL(NEC2REAL)(a-h,o-z)

INTEGER, INTENT(IN)                      :: i
INTEGER, INTENT(IN)                      :: is
REAL(NEC2REAL), INTENT(OUT)                      :: aa
REAL(NEC2REAL), INTENT(OUT)                      :: bb
REAL(NEC2REAL), INTENT(OUT)                      :: cc
! ***
!     COMPUTE COMPONENT OF BASIS FUNCTION I ON SEGMENT IS.
aa=0.
bb=0.
cc=0.
june=0
jsno=0
pp=0.
jcox=icon1(i)
IF (jcox > 10000) jcox=i
jend=-1
iend=-1
sig=-1.
IF (jcox < 0) THEN
  GO TO     1
ELSE IF (jcox == 0) THEN
  GO TO    11
ELSE
  GO TO     2
END IF
1     jcox=-jcox
GO TO 3
2     sig=-sig
jend=-jend
3     jsno=jsno+1
IF (jsno >= jmax) GO TO 24
d=pi*si(jcox)
sdh=SIN(d)
cdh=COS(d)
sd=2.*sdh*cdh
IF (d > 0.015) GO TO 4
omc=4.*d*d
omc=((1.3888889D-3*omc-4.1666666667D-2)*omc+.5)*omc
GO TO 5
4     omc=1.-cdh*cdh+sdh*sdh
5     aj=1./(LOG(1./(pi*bi(jcox)))-.577215664D+0)
pp=pp-omc/sd*aj
IF (jcox /= is) GO TO 6
aa=aj/sd*sig
bb=aj/(2.*cdh)
cc=-aj/(2.*sdh)*sig
june=iend
6     IF (jcox == i) GO TO 9
IF (jend == 1) GO TO 7
jcox=icon1(jcox)
GO TO 8
7     jcox=icon2(jcox)
8     IF (IABS(jcox) == i) GO TO 10
IF (jcox < 0) THEN
  GO TO     1
ELSE IF (jcox == 0) THEN
  GO TO    24
ELSE
  GO TO     2
END IF
9     IF (jcox == is) bb=-bb
10    IF (iend == 1) GO TO 12
11    pm=-pp
pp=0.
njun1=jsno
jcox=icon2(i)
IF (jcox > 10000) jcox=i
jend=1
iend=1
sig=-1.
IF (jcox < 0) THEN
  GO TO     1
ELSE IF (jcox == 0) THEN
  GO TO    12
ELSE
  GO TO     2
END IF
12    njun2=jsno-njun1
d=pi*si(i)
sdh=SIN(d)
cdh=COS(d)
sd=2.*sdh*cdh
cd=cdh*cdh-sdh*sdh
IF (d > 0.015) GO TO 13
omc=4.*d*d
omc=((1.3888889D-3*omc-4.1666666667D-2)*omc+.5)*omc
GO TO 14
13    omc=1.-cd
14    ap=1./(LOG(1./(pi*bi(i)))-.577215664D+0)
aj=ap
IF (njun1 == 0) GO TO 19
IF (njun2 == 0) GO TO 21
qp=sd*(pm*pp+aj*ap)+cd*(pm*ap-pp*aj)
qm=(ap*omc-pp*sd)/qp
qp=-(aj*omc+pm*sd)/qp
IF (june < 0) THEN
  GO TO    15
ELSE IF (june == 0) THEN
  GO TO    18
ELSE
  GO TO    16
END IF
15    aa=aa*qm
bb=bb*qm
cc=cc*qm
GO TO 17
16    aa=-aa*qp
bb=bb*qp
cc=-cc*qp
17    IF (i /= is) RETURN
18    aa=aa-1.
bb=bb+(aj*qm+ap*qp)*sdh/sd
cc=cc+(aj*qm-ap*qp)*cdh/sd
RETURN
19    IF (njun2 == 0) GO TO 23
qp=pi*bi(i)
xxi=qp*qp
xxi=qp*(1.-.5*xxi)/(1.-xxi)
qp=-(omc+xxi*sd)/(sd*(ap+xxi*pp)+cd*(xxi*ap-pp))
IF (june /= 1) GO TO 20
aa=-aa*qp
bb=bb*qp
cc=-cc*qp
IF (i /= is) RETURN
20    aa=aa-1.
d=cd-xxi*sd
bb=bb+(sdh+ap*qp*(cdh-xxi*sdh))/d
cc=cc+(cdh+ap*qp*(sdh+xxi*cdh))/d
RETURN
21    qm=pi*bi(i)
xxi=qm*qm
xxi=qm*(1.-.5*xxi)/(1.-xxi)
qm=(omc+xxi*sd)/(sd*(aj-xxi*pm)+cd*(pm+xxi*aj))
IF (june /= -1) GO TO 22
aa=aa*qm
bb=bb*qm
cc=cc*qm
IF (i /= is) RETURN
22    aa=aa-1.
d=cd-xxi*sd
bb=bb+(aj*qm*(cdh-xxi*sdh)-sdh)/d
cc=cc+(cdh-aj*qm*(sdh+xxi*cdh))/d
RETURN
23    aa=-1.
qp=pi*bi(i)
xxi=qp*qp
xxi=qp*(1.-.5*xxi)/(1.-xxi)
cc=1./(cdh-xxi*sdh)
RETURN
24    WRITE(3,25)  i
STOP
!
25    FORMAT (43H sbf - segment connection error for segment,i5)
END SUBROUTINE sbf

!----------------------------------------------------------------------------

SUBROUTINE sflds (t,e)
! ***
!     DOUBLE PRECISION 6/4/85
!
USE nec2dpar, ONLY : pi
USE gnd
USE gwav

IMPLICIT REAL(NEC2REAL)(a-h,o-z)

REAL(NEC2REAL), INTENT(IN)                         :: t
COMPLEX*16, INTENT(OUT)                  :: e(9)
! ***
!
!     SFLDX RETURNS THE FIELD DUE TO GROUND FOR A CURRENT ELEMENT ON
!     THE SOURCE SEGMENT AT T RELATIVE TO THE SEGMENT CENTER.
!
COMPLEX*16  erv,ezv,erh,ezh,eph,t1,exk,eyk,ezk,exs,eys,ezs,exc  &
    ,eyc,ezc,er,et,hrv,hzv,hrh
COMMON /dataj/ s,b,xj,yj,zj,cabj,sabj,salpj,exk,eyk,ezk,exs,eys,  &
    ezs,exc,eyc,ezc,rkh,ind1,indd1,ind2,indd2,iexk,ipgnd
COMMON /incom/ xo,yo,zo,sn,xsn,ysn,isnor

DATA tp/6.283185308D+0/,pot/1.570796327D+0/

xt=xj+t*cabj
yt=yj+t*sabj
zt=zj+t*salpj
rhx=xo-xt
rhy=yo-yt
rhs=rhx*rhx+rhy*rhy
rho=SQRT(rhs)
IF (rho > 0.) GO TO 1
rhx=1.
rhy=0.
phx=0.
phy=1.
GO TO 2
1     rhx=rhx/rho
rhy=rhy/rho
phx=-rhy
phy=rhx
2     cph=rhx*xsn+rhy*ysn
sph=rhy*xsn-rhx*ysn
IF (ABS(cph) < 1.d-10) cph=0.
IF (ABS(sph) < 1.d-10) sph=0.
zph=zo+zt
zphs=zph*zph
r2s=rhs+zphs
r2=SQRT(r2s)
rk=r2*tp
xx2=DCMPLX(COS(rk),-SIN(rk))
IF (isnor == 1) GO TO 3
!
!     USE NORTON APPROXIMATION FOR FIELD DUE TO GROUND.  CURRENT IS
!     LUMPED AT SEGMENT CENTER WITH CURRENT MOMENT FOR CONSTANT, SINE,
!     OR COSINE DISTRIBUTION.
!
zmh=1.
r1=1.
xx1=0.
CALL gwave (erv,ezv,erh,ezh,eph)
et=-(0.,4.77134)*frati*xx2/(r2s*r2)
er=2.*et*DCMPLX(1.d+0,rk)
et=et*DCMPLX(1.d+0-rk*rk,rk)
hrv=(er+et)*rho*zph/r2s
hzv=(zphs*er-rhs*et)/r2s
hrh=(rhs*er-zphs*et)/r2s
erv=erv-hrv
ezv=ezv-hzv
erh=erh+hrh
ezh=ezh+hrv
eph=eph+et
erv=erv*salpj
ezv=ezv*salpj
erh=erh*sn*cph
ezh=ezh*sn*cph
eph=eph*sn*sph
erh=erv+erh
e(1)=(erh*rhx+eph*phx)*s
e(2)=(erh*rhy+eph*phy)*s
e(3)=(ezv+ezh)*s
e(4)=0.
e(5)=0.
e(6)=0.
sfac=pi*s
sfac=SIN(sfac)/sfac
e(7)=e(1)*sfac
e(8)=e(2)*sfac
e(9)=e(3)*sfac
RETURN
!
!     INTERPOLATE IN SOMMERFELD FIELD TABLES
!
3     IF (rho < 1.d-12) GO TO 4
thet=ATAN(zph/rho)
GO TO 5
4     thet=pot
5     CALL intrp (r2,thet,erv,ezv,erh,eph)
!     COMBINE VERTICAL AND HORIZONTAL COMPONENTS AND CONVERT TO X,Y,Z
!     COMPONENTS.  MULTIPLY BY EXP(-JKR)/R.
xx2=xx2/r2
sfac=sn*cph
erh=xx2*(salpj*erv+sfac*erh)
ezh=xx2*(salpj*ezv-sfac*erv)
eph=sn*sph*xx2*eph
!     X,Y,Z FIELDS FOR CONSTANT CURRENT
e(1)=erh*rhx+eph*phx
e(2)=erh*rhy+eph*phy
e(3)=ezh
rk=tp*t
!     X,Y,Z FIELDS FOR SINE CURRENT
sfac=SIN(rk)
e(4)=e(1)*sfac
e(5)=e(2)*sfac
e(6)=e(3)*sfac
!     X,Y,Z FIELDS FOR COSINE CURRENT
sfac=COS(rk)
e(7)=e(1)*sfac
e(8)=e(2)*sfac
e(9)=e(3)*sfac
RETURN
END SUBROUTINE sflds
!----------------------------------------------------------------------------

SUBROUTINE solgf (a,b,c,d,xy,ip,np,n1,n,mp,m1,m,n1c,n2c,n2cz)
! ***
!     DOUBLE PRECISION 6/4/85
!
USE nec2dpar
USE matpar
USE segj

IMPLICIT REAL(NEC2REAL)(a-h,o-z)

COMPLEX*16, INTENT(IN OUT)               :: a(1)
COMPLEX*16, INTENT(IN OUT)               :: b(n1c,1)
COMPLEX*16, INTENT(IN OUT)               :: c(n1c,1)
COMPLEX*16, INTENT(IN OUT)               :: d(n2cz,1)
COMPLEX*16, INTENT(IN OUT)               :: xy(1)
INTEGER, INTENT(IN OUT)                  :: ip(1)
INTEGER, INTENT(IN OUT)                  :: np
INTEGER, INTENT(IN)                      :: n1
INTEGER, INTENT(IN)                      :: n
INTEGER, INTENT(IN OUT)                  :: mp
INTEGER, INTENT(IN)                      :: m1
INTEGER, INTENT(IN OUT)                  :: m
INTEGER, INTENT(IN)                      :: n1c
INTEGER, INTENT(IN)                      :: n2c
INTEGER, INTENT(IN OUT)                  :: n2cz
! ***
!     SOLVE FOR CURRENT IN N.G.F. PROCEDURE
COMPLEX*16  sum, y
COMMON /scratm/ y(2*maxseg)

ifl=14
IF (icasx > 0) ifl=13
IF (n2c > 0) GO TO 1
!     NORMAL SOLUTION.  NOT N.G.F.
CALL solves (a,ip,xy,n1c,1,np,n,mp,m,13,ifl)
GO TO 22
1     IF (n1 == n.OR.m1 == 0) GO TO 5
!     REORDER EXCITATION ARRAY
n2=n1+1
jj=n+1
npm=n+2*m1
DO  i=n2,npm
  y(i)=xy(i)
END DO
j=n1
DO  i=jj,npm
  j=j+1
  xy(j)=y(i)
END DO
DO  i=n2,n
  j=j+1
  xy(j)=y(i)
END DO
5     neqs=nscon+2*npcon
IF (neqs == 0) GO TO 7
neq=n1c+n2c
neqs=neq-neqs+1
!     COMPUTE INV(A)E1
DO  i=neqs,neq
  xy(i)=(0.,0.)
END DO
7     CALL solves (a,ip,xy,n1c,1,np,n1,mp,m1,13,ifl)
ni=0
npb=npbl
!     COMPUTE E2-C(INV(A)E1)
DO  jj=1,nbbl
  IF (jj == nbbl) npb=nlbl
  IF (icasx > 1) READ (15) ((c(i,j),i=1,n1c),j=1,npb)
  ii=n1c+ni
  DO  i=1,npb
    sum=(0.,0.)
    DO  j=1,n1c
      sum=sum+c(j,i)*xy(j)
    END DO
    j=ii+i
    xy(j)=xy(j)-sum
  END DO
  ni=ni+npbl
END DO
IF (icasx > 1) REWIND 15
jj=n1c+1
!     COMPUTE INV(D)(E2-C(INV(A)E1)) = I2
IF (icasx > 1) GO TO 11
CALL solve (n2c,d,ip(jj),xy(jj),n2c)
GO TO 13
11    IF (icasx == 4) GO TO 12
ni=n2c*n2c
READ (11) (b(j,1),j=1,ni)
REWIND 11
CALL solve (n2c,b,ip(jj),xy(jj),n2c)
GO TO 13
12    nblsys=nblsym
npsys=npsym
nlsys=nlsym
icass=icase
nblsym=nbbl
npsym=npbl
nlsym=nlbl
icase=3
REWIND 11
REWIND 16
CALL ltsolv (b,n2c,ip(jj),xy(jj),n2c,1,11,16)
REWIND 11
REWIND 16
nblsym=nblsys
npsym=npsys
nlsym=nlsys
icase=icass
13    ni=0
npb=npbl
!     COMPUTE INV(A)E1-(INV(A)B)I2 = I1
DO  jj=1,nbbl
  IF (jj == nbbl) npb=nlbl
  IF (icasx > 1) READ (14) ((b(i,j),i=1,n1c),j=1,npb)
  ii=n1c+ni
  DO  i=1,n1c
    sum=(0.,0.)
    DO  j=1,npb
      jp=ii+j
      sum=sum+b(i,j)*xy(jp)
    END DO
    xy(i)=xy(i)-sum
  END DO
  ni=ni+npbl
END DO
IF (icasx > 1) REWIND 14
IF (n1 == n.OR.m1 == 0) GO TO 20
!     REORDER CURRENT ARRAY
DO  i=n2,npm
  y(i)=xy(i)
END DO
jj=n1c+1
j=n1
DO  i=jj,npm
  j=j+1
  xy(j)=y(i)
END DO
DO  i=n2,n1c
  j=j+1
  xy(j)=y(i)
END DO
20    IF (nscon == 0) GO TO 22
j=neqs-1
DO  i=1,nscon
  j=j+1
  jj=iscon(i)
  xy(jj)=xy(j)
END DO
22    RETURN
END SUBROUTINE solgf
!----------------------------------------------------------------------------

SUBROUTINE solve (n,a,ip,b,ndim)
! ***
!     DOUBLE PRECISION 6/4/85
!
    USE nec2dpar, ONLY : maxseg

    IMPLICIT REAL(NEC2REAL)(a-h,o-z)

    INTEGER, INTENT(IN)                      :: n
    COMPLEX*16, INTENT(IN)                   :: a(ndim,ndim)
    INTEGER, INTENT(IN)                      :: ip(ndim)
    COMPLEX*16, INTENT(IN OUT)               :: b(ndim)
    INTEGER, INTENT(IN)                      :: ndim
    ! ***
    !
    !     SUBROUTINE TO SOLVE THE MATRIX EQUATION LU*X=B WHERE L IS A UNIT
    !     LOWER TRIANGULAR MATRIX AND U IS AN UPPER TRIANGULAR MATRIX BOTH
    !     OF WHICH ARE STORED IN A.  THE RHS VECTOR B IS INPUT AND THE
    !     SOLUTION IS RETURNED THROUGH VECTOR B.
    !
    COMPLEX*16  y,sum
    INTEGER :: pi
    COMMON /scratm/ y(2*maxseg)

    !
    !     FORWARD SUBSTITUTION
    !
    DO  i=1,n
      pi=ip(i)
      y(i)=b(pi)
      b(pi)=b(i)
      ip1=i+1
      IF (ip1 > n) GO TO 2
      DO  j=ip1,n
        b(j)=b(j)-a(j,i)*y(i)
      END DO
2     CONTINUE
    END DO
    !
    !     BACKWARD SUBSTITUTION
    !
    DO  k=1,n
      i=n-k+1
      sum=(0.,0.)
      ip1=i+1
      IF (ip1 > n) GO TO 5
      DO  j=ip1,n
        sum=sum+a(i,j)*b(j)
      END DO
5     CONTINUE
      b(i)=(y(i)-sum)/a(i,i)
    END DO
    RETURN
END SUBROUTINE solve
!----------------------------------------------------------------------------

SUBROUTINE solves (a,ip,b,neq,nrh,np,n,mp,m,ifl1,ifl2)
! ***
!     DOUBLE PRECISION 6/4/85
!
USE nec2dpar
USE matpar

IMPLICIT REAL(NEC2REAL)(a-h,o-z)

COMPLEX*16, INTENT(IN OUT)               :: a(1)
INTEGER, INTENT(IN OUT)                  :: ip(1)
COMPLEX*16, INTENT(IN OUT)               :: b(neq,nrh)
INTEGER, INTENT(IN)                      :: neq
INTEGER, INTENT(IN)                      :: nrh
INTEGER, INTENT(IN)                      :: np
INTEGER, INTENT(IN)                      :: n
INTEGER, INTENT(IN)                      :: mp
INTEGER, INTENT(IN)                      :: m
INTEGER, INTENT(IN)                      :: ifl1
INTEGER, INTENT(IN)                      :: ifl2
! ***
!
!     SUBROUTINE SOLVES, FOR SYMMETRIC STRUCTURES, HANDLES THE
!     TRANSFORMATION OF THE RIGHT HAND SIDE VECTOR AND SOLUTION OF THE
!     MATRIX EQ.
!
COMPLEX*16  y,sum,ssx
COMMON /smat/ ssx(16,16)
COMMON /scratm/ y(2*maxseg)

npeq=np+2*mp
nop=neq/npeq
fnop=nop
fnorm=1./fnop
nrow=neq
IF (icase > 3) nrow=npeq
IF (nop == 1) GO TO 11
DO  ic=1,nrh
  IF (n == 0.OR.m == 0) GO TO 6
  DO  i=1,neq
    y(i)=b(i,ic)
  END DO
  kk=2*mp
  ia=np
  ib=n
  j=np
  DO  k=1,nop
    IF (k == 1) GO TO 3
    DO  i=1,np
      ia=ia+1
      j=j+1
      b(j,ic)=y(ia)
    END DO
    IF (k == nop) CYCLE
    3     DO  i=1,kk
      ib=ib+1
      j=j+1
      b(j,ic)=y(ib)
    END DO
  END DO
!
!     TRANSFORM MATRIX EQ. RHS VECTOR ACCORDING TO SYMMETRY MODES
!
  6     DO  i=1,npeq
    DO  k=1,nop
      ia=i+(k-1)*npeq
      y(k)=b(ia,ic)
    END DO
    sum=y(1)
    DO  k=2,nop
      sum=sum+y(k)
    END DO
    b(i,ic)=sum*fnorm
    DO  k=2,nop
      ia=i+(k-1)*npeq
      sum=y(1)
      DO  j=2,nop
        sum=sum+y(j)*DCONJG(ssx(k,j))
      END DO
      b(ia,ic)=sum*fnorm
    END DO
  END DO
END DO
11    IF (icase < 3) GO TO 12
REWIND ifl1
REWIND ifl2
!
!     SOLVE EACH MODE EQUATION
!
12    DO  kk=1,nop
  ia=(kk-1)*npeq+1
  ib=ia
  IF (icase /= 4) GO TO 13
  i=npeq*npeq
  READ (ifl1) (a(j),j=1,i)
  ib=1
  13    IF (icase == 3.OR.icase == 5) GO TO 15
  DO  ic=1,nrh
    CALL solve (npeq,a(ib),ip(ia),b(ia,ic),nrow)
  END DO
  CYCLE
  15    CALL ltsolv (a,npeq,ip(ia),b(ia,1),neq,nrh,ifl1,ifl2)
END DO
IF (nop == 1) RETURN
!
!     INVERSE TRANSFORM THE MODE SOLUTIONS
!
DO  ic=1,nrh
  DO  i=1,npeq
    DO  k=1,nop
      ia=i+(k-1)*npeq
      y(k)=b(ia,ic)
    END DO
    sum=y(1)
    DO  k=2,nop
      sum=sum+y(k)
    END DO
    b(i,ic)=sum
    DO  k=2,nop
      ia=i+(k-1)*npeq
      sum=y(1)
      DO  j=2,nop
        sum=sum+y(j)*ssx(k,j)
      END DO
      b(ia,ic)=sum
    END DO
  END DO
  IF (n == 0.OR.m == 0) CYCLE
  DO  i=1,neq
    y(i)=b(i,ic)
  END DO
  kk=2*mp
  ia=np
  ib=n
  j=np
  DO  k=1,nop
    IF (k == 1) GO TO 23
    DO  i=1,np
      ia=ia+1
      j=j+1
      b(ia,ic)=y(j)
    END DO
    IF (k == nop) CYCLE
    23    DO  i=1,kk
      ib=ib+1
      j=j+1
      b(ib,ic)=y(j)
    END DO
  END DO
END DO
RETURN
END SUBROUTINE solves
!----------------------------------------------------------------------------

SUBROUTINE tbf (i,icap)
! ***
!     DOUBLE PRECISION 6/4/85
!
USE nec2dpar
USE data
USE segj

IMPLICIT REAL(NEC2REAL)(a-h,o-z)

INTEGER, INTENT(IN)                      :: i
INTEGER, INTENT(IN)                      :: icap
! ***
!     COMPUTE BASIS FUNCTION I

jsno=0
pp=0.
jcox=icon1(i)
IF (jcox > 10000) jcox=i
jend=-1
iend=-1
sig=-1.
IF (jcox < 0) THEN
  GO TO     1
ELSE IF (jcox == 0) THEN
  GO TO    10
ELSE
  GO TO     2
END IF
1     jcox=-jcox
GO TO 3
2     sig=-sig
jend=-jend
3     jsno=jsno+1
IF (jsno >= jmax) GO TO 28
jco(jsno)=jcox
d=pi*si(jcox)
sdh=SIN(d)
cdh=COS(d)
sd=2.*sdh*cdh
IF (d > 0.015) GO TO 4
omc=4.*d*d
omc=((1.3888889D-3*omc-4.1666666667D-2)*omc+.5)*omc
GO TO 5
4     omc=1.-cdh*cdh+sdh*sdh
5     aj=1./(LOG(1./(pi*bi(jcox)))-.577215664D+0)
pp=pp-omc/sd*aj
ax(jsno)=aj/sd*sig
bx(jsno)=aj/(2.*cdh)
cx(jsno)=-aj/(2.*sdh)*sig
IF (jcox == i) GO TO 8
IF (jend == 1) GO TO 6
jcox=icon1(jcox)
GO TO 7
6     jcox=icon2(jcox)
7     IF (IABS(jcox) == i) GO TO 9
IF (jcox < 0) THEN
  GO TO     1
ELSE IF (jcox == 0) THEN
  GO TO    28
ELSE
  GO TO     2
END IF
8     bx(jsno)=-bx(jsno)
9     IF (iend == 1) GO TO 11
10    pm=-pp
pp=0.
njun1=jsno
jcox=icon2(i)
IF (jcox > 10000) jcox=i
jend=1
iend=1
sig=-1.
IF (jcox < 0) THEN
  GO TO     1
ELSE IF (jcox == 0) THEN
  GO TO    11
ELSE
  GO TO     2
END IF
11    njun2=jsno-njun1
jsnop=jsno+1
jco(jsnop)=i
d=pi*si(i)
sdh=SIN(d)
cdh=COS(d)
sd=2.*sdh*cdh
cd=cdh*cdh-sdh*sdh
IF (d > 0.015) GO TO 12
omc=4.*d*d
omc=((1.3888889D-3*omc-4.1666666667D-2)*omc+.5)*omc
GO TO 13
12    omc=1.-cd
13    ap=1./(LOG(1./(pi*bi(i)))-.577215664D+0)
aj=ap
IF (njun1 == 0) GO TO 16
IF (njun2 == 0) GO TO 20
qp=sd*(pm*pp+aj*ap)+cd*(pm*ap-pp*aj)
qm=(ap*omc-pp*sd)/qp
qp=-(aj*omc+pm*sd)/qp
bx(jsnop)=(aj*qm+ap*qp)*sdh/sd
cx(jsnop)=(aj*qm-ap*qp)*cdh/sd
DO  iend=1,njun1
  ax(iend)=ax(iend)*qm
  bx(iend)=bx(iend)*qm
  cx(iend)=cx(iend)*qm
END DO
jend=njun1+1
DO  iend=jend,jsno
  ax(iend)=-ax(iend)*qp
  bx(iend)=bx(iend)*qp
  cx(iend)=-cx(iend)*qp
END DO
GO TO 27
16    IF (njun2 == 0) GO TO 24
IF (icap /= 0) GO TO 17
xxi=0.
GO TO 18
17    qp=pi*bi(i)
xxi=qp*qp
xxi=qp*(1.-.5*xxi)/(1.-xxi)
18    qp=-(omc+xxi*sd)/(sd*(ap+xxi*pp)+cd*(xxi*ap-pp))
d=cd-xxi*sd
bx(jsnop)=(sdh+ap*qp*(cdh-xxi*sdh))/d
cx(jsnop)=(cdh+ap*qp*(sdh+xxi*cdh))/d
DO  iend=1,njun2
  ax(iend)=-ax(iend)*qp
  bx(iend)=bx(iend)*qp
  cx(iend)=-cx(iend)*qp
END DO
GO TO 27
20    IF (icap /= 0) GO TO 21
xxi=0.
GO TO 22
21    qm=pi*bi(i)
xxi=qm*qm
xxi=qm*(1.-.5*xxi)/(1.-xxi)
22    qm=(omc+xxi*sd)/(sd*(aj-xxi*pm)+cd*(pm+xxi*aj))
d=cd-xxi*sd
bx(jsnop)=(aj*qm*(cdh-xxi*sdh)-sdh)/d
cx(jsnop)=(cdh-aj*qm*(sdh+xxi*cdh))/d
DO  iend=1,njun1
  ax(iend)=ax(iend)*qm
  bx(iend)=bx(iend)*qm
  cx(iend)=cx(iend)*qm
END DO
GO TO 27
24    bx(jsnop)=0.
IF (icap /= 0) GO TO 25
xxi=0.
GO TO 26
25    qp=pi*bi(i)
xxi=qp*qp
xxi=qp*(1.-.5*xxi)/(1.-xxi)
26    cx(jsnop)=1./(cdh-xxi*sdh)
27    jsno=jsnop
ax(jsno)=-1.
RETURN
28    WRITE(3,29)  i
STOP
!
29    FORMAT (43H tbf - segment connection error for segment,i5)
END SUBROUTINE tbf

!----------------------------------------------------------------------------

SUBROUTINE test (f1r,f2r,tr,f1i,f2i,ti,dmin)
! ***
!     DOUBLE PRECISION 6/4/85
!
    IMPLICIT NONE

    REAL(NEC2REAL), INTENT(IN)                         :: f1r
    REAL(NEC2REAL), INTENT(IN)                         :: f2r
    REAL(NEC2REAL), INTENT(OUT)                        :: tr
    REAL(NEC2REAL), INTENT(IN)                         :: f1i
    REAL(NEC2REAL), INTENT(IN)                         :: f2i
    REAL(NEC2REAL), INTENT(OUT)                        :: ti
    REAL(NEC2REAL), INTENT(IN)                         :: dmin

    REAL(NEC2REAL)                                     :: den
    ! ***
    !
    !     TEST FOR CONVERGENCE IN NUMERICAL INTEGRATION
    !
    den=ABS(f2r)
    tr=ABS(f2i)
    IF (den < tr) den=tr
    IF (den < dmin) den=dmin
    IF (den < 1.d-37) THEN
        tr=0.
        ti=0.
        RETURN
    END IF
    tr=ABS((f1r-f2r)/den)
    ti=ABS((f1i-f2i)/den)
    RETURN

END SUBROUTINE test

!----------------------------------------------------------------------------

SUBROUTINE trio (j)
! ***
!     DOUBLE PRECISION 6/4/85
!
USE nec2dpar
USE data
USE segj

IMPLICIT REAL(NEC2REAL)(a-h,o-z)

INTEGER, INTENT(IN)                      :: j
! ***
!     COMPUTE THE COMPONENTS OF ALL BASIS FUNCTIONS ON SEGMENT J

jsno=0
jcox=icon1(j)
IF (jcox > 10000) GO TO 7
jend=-1
iend=-1
IF (jcox < 0) THEN
  GO TO     1
ELSE IF (jcox == 0) THEN
  GO TO     7
ELSE
  GO TO     2
END IF
1     jcox=-jcox
GO TO 3
2     jend=-jend
3     IF (jcox == j) GO TO 6
jsno=jsno+1
IF (jsno >= jmax) GO TO 9
CALL sbf (jcox,j,ax(jsno),bx(jsno),cx(jsno))
jco(jsno)=jcox
IF (jend == 1) GO TO 4
jcox=icon1(jcox)
GO TO 5
4     jcox=icon2(jcox)
5     IF (jcox < 0) THEN
  GO TO     1
ELSE IF (jcox == 0) THEN
  GO TO     9
ELSE
  GO TO     2
END IF
6     IF (iend == 1) GO TO 8
7     jcox=icon2(j)
IF (jcox > 10000) GO TO 8
jend=1
iend=1
IF (jcox < 0) THEN
  GO TO     1
ELSE IF (jcox == 0) THEN
  GO TO     8
ELSE
  GO TO     2
END IF
8     jsno=jsno+1
CALL sbf (j,j,ax(jsno),bx(jsno),cx(jsno))
jco(jsno)=j
RETURN
9     WRITE(3,10)  j
STOP
!
10    FORMAT (44H trio - segment connention error for segment,i5)
END SUBROUTINE trio
!----------------------------------------------------------------------------

SUBROUTINE unere (xob,yob,zob)
! ***
!     DOUBLE PRECISION 6/4/85
!
USE gnd

IMPLICIT REAL(NEC2REAL)(a-h,o-z)

REAL(NEC2REAL), INTENT(IN)                         :: xob
REAL(NEC2REAL), INTENT(IN)                         :: yob
REAL(NEC2REAL), INTENT(IN)                         :: zob
! ***
!     CALCULATES THE ELECTRIC FIELD DUE TO UNIT CURRENT IN THE T1 AND T2
!     DIRECTIONS ON A PATCH
COMPLEX*16 exk,eyk,ezk,exs,eys,ezs,exc,eyc,ezc,t1  &
    ,er,q1,q2,rrv,rrh,edp
COMMON /dataj/ s,b,xj,yj,zj,cabj,sabj,salpj,exk,eyk,ezk,exs,eys,  &
    ezs,exc,eyc,ezc,rkh,ind1,indd1,ind2,indd2,iexk,ipgnd
EQUIVALENCE (t1xj,cabj), (t1yj,sabj), (t1zj,salpj), (t2xj,b), &
    (t2yj,ind1), (t2zj,ind2)
DATA tpi,const/6.283185308D+0,4.771341188D+0/
!     CONST=ETA/(8.*PI**2)
zr=zj
t1zr=t1zj
t2zr=t2zj
IF (ipgnd /= 2) GO TO 1
zr=-zr
t1zr=-t1zr
t2zr=-t2zr
1     rx=xob-xj
ry=yob-yj
rz=zob-zr
r2=rx*rx+ry*ry+rz*rz
IF (r2 > 1.d-20) GO TO 2
exk=(0.,0.)
eyk=(0.,0.)
ezk=(0.,0.)
exs=(0.,0.)
eys=(0.,0.)
ezs=(0.,0.)
RETURN
2     r=SQRT(r2)
tt1=-tpi*r
tt2=tt1*tt1
rt=r2*r
er=DCMPLX(SIN(tt1),-COS(tt1))*(const*s)
q1=DCMPLX(tt2-1.,tt1)*er/rt
q2=DCMPLX(3.-tt2,-3.*tt1)*er/(rt*r2)
er=q2*(t1xj*rx+t1yj*ry+t1zr*rz)
exk=q1*t1xj+er*rx
eyk=q1*t1yj+er*ry
ezk=q1*t1zr+er*rz
er=q2*(t2xj*rx+t2yj*ry+t2zr*rz)
exs=q1*t2xj+er*rx
eys=q1*t2yj+er*ry
ezs=q1*t2zr+er*rz
IF (ipgnd == 1) GO TO 6
IF (iperf /= 1) GO TO 3
exk=-exk
eyk=-eyk
ezk=-ezk
exs=-exs
eys=-eys
ezs=-ezs
GO TO 6
3     xymag=SQRT(rx*rx+ry*ry)
IF (xymag > 1.d-6) GO TO 4
px=0.
py=0.
cth=1.
rrv=(1.,0.)
GO TO 5
4     px=-ry/xymag
py=rx/xymag
cth=rz/SQRT(xymag*xymag+rz*rz)
rrv=SQRT(1.-zrati*zrati*(1.-cth*cth))
5     rrh=zrati*cth
rrh=(rrh-rrv)/(rrh+rrv)
rrv=zrati*rrv
rrv=-(cth-rrv)/(cth+rrv)
edp=(exk*px+eyk*py)*(rrh-rrv)
exk=exk*rrv+edp*px
eyk=eyk*rrv+edp*py
ezk=ezk*rrv
edp=(exs*px+eys*py)*(rrh-rrv)
exs=exs*rrv+edp*px
eys=eys*rrv+edp*py
ezs=ezs*rrv
6     RETURN
END SUBROUTINE unere
!----------------------------------------------------------------------------

SUBROUTINE wire (xw1,yw1,zw1,xw2,yw2,zw2,rad,rdel,rrad,ns,itg)
! ***
!     DOUBLE PRECISION 6/4/85
!
USE nec2dpar
USE data

IMPLICIT REAL(NEC2REAL)(a-h,o-z)

REAL(NEC2REAL), INTENT(IN)                         :: xw1
REAL(NEC2REAL), INTENT(IN)                         :: yw1
REAL(NEC2REAL), INTENT(IN)                         :: zw1
REAL(NEC2REAL), INTENT(IN)                         :: xw2
REAL(NEC2REAL), INTENT(IN)                         :: yw2
REAL(NEC2REAL), INTENT(IN)                         :: zw2
REAL(NEC2REAL), INTENT(IN)                         :: rad
REAL(NEC2REAL), INTENT(IN)                         :: rdel
REAL(NEC2REAL), INTENT(IN)                         :: rrad
INTEGER, INTENT(IN)                      :: ns
INTEGER, INTENT(IN)                      :: itg
! ***
!
!     SUBROUTINE WIRE GENERATES SEGMENT GEOMETRY DATA FOR A STRAIGHT
!     WIRE OF NS SEGMENTS.
!
DIMENSION x2(1), y2(1), z2(1)
EQUIVALENCE (x2(1),si(1)), (y2(1),alp(1)), (z2(1),bet(1))

ist=n+1
n=n+ns
np=n
mp=m
ipsym=0
IF (ns < 1) RETURN
xd=xw2-xw1
yd=yw2-yw1
zd=zw2-zw1
IF (ABS(rdel-1.) < 1.d-6) GO TO 1
delz=SQRT(xd*xd+yd*yd+zd*zd)
xd=xd/delz
yd=yd/delz
zd=zd/delz
delz=delz*(1.-rdel)/(1.-rdel**ns)
rd=rdel
GO TO 2
1     fns=ns
xd=xd/fns
yd=yd/fns
zd=zd/fns
delz=1.
rd=1.
2     radz=rad
xs1=xw1
ys1=yw1
zs1=zw1
DO  i=ist,n
  itag(i)=itg
  xs2=xs1+xd*delz
  ys2=ys1+yd*delz
  zs2=zs1+zd*delz
  x(i)=xs1
  y(i)=ys1
  z(i)=zs1
  x2(i)=xs2
  y2(i)=ys2
  z2(i)=zs2
  bi(i)=radz
  delz=delz*rd
  radz=radz*rrad
  xs1=xs2
  ys1=ys2
  zs1=zs2
END DO
x2(n)=xw2
y2(n)=yw2
z2(n)=zw2
RETURN
END SUBROUTINE wire
!--------------------------------------------------------------------
!
!     ZINT COMPUTES THE INTERNAL IMPEDANCE OF A CIRCULAR WIRE
!
!
COMPLEX*16 FUNCTION zint(sigl,rolam)
        USE nec2dpar, ONLY : pi

        IMPLICIT REAL(NEC2REAL)(a-h,o-z)

        REAL(NEC2REAL), INTENT(IN)                         :: sigl
        REAL(NEC2REAL), INTENT(IN)                         :: rolam

        REAL(NEC2REAL), PARAMETER                          :: pot   = 1.5707963D+0
        REAL(NEC2REAL), PARAMETER                          :: tp    = 6.2831853D+0
        REAL(NEC2REAL), PARAMETER                          :: tpcmu = 2.368705D+3

        REAL(NEC2REAL), PARAMETER                          :: cmotp = 60.00
        REAL(NEC2REAL), PARAMETER, DIMENSION(2)            :: fjx   = (/0.0, 1.0/)
        REAL(NEC2REAL), PARAMETER, DIMENSION(2)            :: cnx   = (/0.70710678D+0, 0.70710678D+0/)

        REAL(NEC2REAL), PARAMETER, DIMENSION(28)           :: ccn   = &
            (/6.d-7,1.9D-6,-3.4D-6,5.1D-6,-2.52D-5,0.,-9.06D-5,-9.01D-5,  &
            0.,-9.765D-4,.0110486D+0,-.0110485D+0,0.,-.3926991D+0,1.6D-6,  &
            -3.2D-6,1.17D-5,-2.4D-6,3.46D-5,3.38D-5,5.d-7,2.452D-4,-1.3813D-3,  &
            1.3811D-3,-6.25001D-2,-1.d-7,.7071068D+0,.7071068D+0/)

        COMPLEX*16 th,ph,f,g,fj,cn,br1,br2
        COMPLEX*16 cc1,cc2,cc3,cc4,cc5,cc6,cc7,cc8,cc9,cc10,cc11,cc12 ,cc13,cc14

        EQUIVALENCE (fj,fjx), (cn,cnx), (cc1,ccn(1)), (cc2,ccn(3)), &
            (cc3,ccn(5)), (cc4,ccn(7)), (cc5,ccn(9)), (cc6,ccn(11)), (cc7,ccn(13)),  &
            (cc8,ccn(15)), (cc9,ccn(17)), (cc10,ccn(19)), (cc11,ccn(21)), &
            (cc12,ccn(23)), (cc13,ccn(25)), (cc14,ccn(27))


        th(d)=(((((cc1*d+cc2)*d+cc3)*d+cc4)*d+cc5)*d+cc6)*d+cc7
        ph(d)=(((((cc8*d+cc9)*d+cc10)*d+cc11)*d+cc12)*d+cc13)*d+cc14
        f(d)=SQRT(pot/d)*EXP(-cn*d+th(-8./x))
        g(d)=EXP(cn*d+th(8./x))/SQRT(tp*d)
        x=SQRT(tpcmu*sigl)*rolam
        IF (x > 110.) GO TO 2
        IF (x > 8.) GO TO 1
        y=x/8.
        y=y*y
        s=y*y
        ber=((((((-9.01D-6*s+1.22552D-3)*s-.08349609D+0)*s+2.6419140D+0)  &
            *s-32.363456D+0)*s+113.77778D+0)*s-64.)*s+1.
        bei=((((((1.1346D-4*s-.01103667D+0)*s+.52185615D+0)*s-  &
            10.567658D+0)*s+72.817777D+0)*s-113.77778D+0)*s+16.)*y
        br1=DCMPLX(ber,bei)
        ber=(((((((-3.94D-6*s+4.5957D-4)*s-.02609253D+0)*s+.66047849D+0)  &
            *s-6.0681481D+0)*s+14.222222D+0)*s-4.)*y)*x
        bei=((((((4.609D-5*s-3.79386D-3)*s+.14677204D+0)*s-2.3116751D+0)  &
            *s+11.377778D+0)*s-10.666667D+0)*s+.5)*x
        br2=DCMPLX(ber,bei)
        br1=br1/br2
        GO TO 3

1       br2=fj*f(x)/pi
        br1=g(x)+br2
        br2=g(x)*ph(8./x)-br2*ph(-8./x)
        br1=br1/br2
        GO TO 3

2       br1=DCMPLX(.70710678D+0,-.70710678D+0)
3       zint=fj*SQRT(cmotp/sigl)*br1/rolam
        RETURN
END FUNCTION zint

END PROGRAM nec2dxs

