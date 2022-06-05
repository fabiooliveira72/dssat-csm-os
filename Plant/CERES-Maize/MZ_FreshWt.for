!======================================================================
!  MZ_FreshWt, Subroutine
!
!  Maize fresh weight
!----------------------------------------------------------------------
!  Revision history
!  04/21/2008 CHP Written.
!  05/11/2021 KJB/JO Updated equations for Sillage.
!  05/11/2021 FO Added Forage.OUT output file.
!  05/11/2021 FO Better organize the code structure.
!  05/12/2021 FO Added Species file inputs
!----------------------------------------------------------------------
!
!  Called : MZ_GROSUB?
!----------------------------------------------------------------------

      SUBROUTINE MZ_FreshWt (ISWITCH, ISWFWT,
     &    CUMDTTEG, EARS, EARWT, ISTAGE, MDATE, SLPF,
     &    STGDOY, SUMDTT, SWFAC, NSTRES, YRPLT,
     &    WTNCAN, WTNSD, WTNVEG, STOVWT, TOPWT,
     &    PODWT, SDWT, SKERWT, SHELPC, P5,RELDTTEG,
     &    TOTFWT, PODFWT, TOTDMC, COBDMC, EARDMC, EARFRC,
     &    GRNDMC,MILKLN,
     &    STPCT, OMDIG, CPPCT, UFL, UFLHA)
!----------------------------------------------------------------------
      USE ModuleDefs
      USE ModuleData
      IMPLICIT  NONE
      SAVE
!----------------------------------------------------------------------
!                         Variable Declaration
!----------------------------------------------------------------------
! KJB - REAL FOR:  SWFAC, WTNCAN, WTNSD, WTNVEG, STOVWT, TOTFWT, TOTDMC, EARFRC, SDFRC, TOPWT, SDWT, SKERWT, SHELPC, P5, RELDTTEG
      REAL        WTNCAN, WTNSD, WTNVEG, PODWT, PODFWT
      REAL        STOVWT, TOTFWT, TOTDMC, EARFRC
      REAL        SDFRC, TOPWT, SDWT, SKERWT, SHELPC
      REAL         P5, RELDTTEG
! KJB - REAL FOR:  VEGDMC, SLPVEG, SLPGRN, MILKZ0, GRNDMC,STAVEG, STAGRN, STACON, CONDIG, OMDIG, NEL, UFL, UFL2, UFLHA
      REAL        VEGDMC, SLPVEG, SLPGRN, MILKZ0
      REAL        GRNDMC,STAVEG, STAGRN, STACON, CONDIG, OMDIG
      REAL        NEL, UFL, UFL2, UFLHA, MILKLN, STATCON

      REAL        EARS1_2
      REAL        FWYLD1_2
      REAL        CUMDTTEG
      INTEGER     DOY
      INTEGER     DYNAMIC
      REAL        EARDMC
      REAL        EARFWT
      REAL        EARMKT
      REAL        EARS
      REAL        EARWT
      CHARACTER*6 ERRKEY
      PARAMETER       (ERRKEY='MZ_FWt')
      CHARACTER*6  SECTION
      CHARACTER*10, PARAMETER :: SGFile = "FORAGE.OUT"
      CHARACTER*12 FILES
      CHARACTER*30 FILEIO
      CHARACTER*80 C80
      CHARACTER*80 PATHSR
      CHARACTER*92 FILECC
      REAL        EARSFCY
      REAL        FWYLDFCY
      REAL        FWYIELD
      INTEGER     ISTAGE
      CHARACTER*1 ISWFWT
      INTEGER     MDATE
      REAL        MKTFWYLD
      INTEGER     NOUTSG
      REAL        NSTRES
      REAL        SLPF
      INTEGER     STGDOY(20)
      REAL        SUMDTT
      REAL        SWFAC
      REAL        XFWYLDFCY
      REAL        IRDMC
      REAL        CVGDMC
      REAL        STPCT, CPPCT, SLPMLK, COBDMC, SDFWT
      REAL        SLPCOB, COBDMCIS4
      INTEGER     YRDOY
      INTEGER     LUNIO, LNUM, LUNCRP, FOUND, ISECT

      TYPE (SwitchType)  ISWITCH
      LOGICAL FEXIST
      INTEGER DAP, DAS, ERRNUM, FROP, TIMDIF, YEAR, YRPLT, ERR

      TYPE (ControlType) CONTROL

      IF (ISWFWT == 'N' .OR. ISWITCH % IDETG == 'N') RETURN

      CALL GET (CONTROL)
      DYNAMIC = CONTROL%DYNAMIC
      FILEIO  = CONTROL % FILEIO 

!----------------------------------------------------------------------
!                     DYNAMIC = SEASINIT
!----------------------------------------------------------------------

      IF(DYNAMIC == SEASINIT) THEN

        TOTFWT   = 0.0
        VEGDMC   = 0.0
        TOTDMC   = 0.0
        PODFWT   = 0.0
        EARDMC   = 0.0
        COBDMC   = 0.0
        SDFWT    = 0.0
        GRNDMC   = 0.0
        EARFRC   = 0.0
        SDFRC   = 0.0
        MILKLN   = 0.0
        STACON   = 0.0
        STPCT   = 0.0
        CPPCT   = 0.0
        OMDIG   = 0.0
        NEL   = 0.0
        UFL   = 0.0
        UFL2   = 0.0
        UFLHA   = 0.0
        
! FO - FORAGE.OUT file
          CALL GETLUN(SGFile,  NOUTSG)

          INQUIRE (FILE = SGFile, EXIST = FEXIST)
          IF (FEXIST) THEN
            OPEN (UNIT = NOUTSG, FILE = SGFile, STATUS='OLD',
     &        IOSTAT=ERRNUM, POSITION='APPEND')
          ELSE
            OPEN (UNIT = NOUTSG, FILE = SGFile, STATUS='NEW',
     &        IOSTAT = ERRNUM)
              WRITE(NOUTSG,'("*Silage daily output")')
          ENDIF

          CALL HEADER(SEASINIT, NOUTSG, CONTROL%RUN)
          WRITE (NOUTSG, 1002)

 1002 FORMAT ('@YEAR DOY   DAS   DAP',
     &   '  TOFWT  VGDMC  TODMC  ERFWT  ERDMC  SDFWT  COBMC  SDDMC',
     &   '  ERFRC  SDFRC  TOPWT  PODWT   SDWT  MILKL  STCON  STPCT',
     &   '  CPPCT  OMDIG    NEL    UFL   UFL2  UFLHA  WTNCA')

C READ AND PASS IN: SLPVEG, SLPGRN, MILKZ0, STAVEG, STAGRN, CONDIG
!-------------------------------------------------------
!     Read input file name (ie. DSSAT45.INP) and path
!-------------------------------------------------------
      CALL GETLUN('FILEIO', LUNIO)
      OPEN (LUNIO, FILE = FILEIO,STATUS = 'OLD',IOSTAT=ERR)
      IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEIO,0)
      REWIND (LUNIO)
      READ(LUNIO,50,IOSTAT=ERR) FILES, PATHSR; LNUM = 7
   50 FORMAT(//////,15X,A12,1X,A80)
      IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEIO,LNUM)
      CLOSE(LUNIO)
!----------------------------------------------------------------
!       Find and Read FORAGE SILAGE Section
!----------------------------------------------------------------
      FILECC =  TRIM(PATHSR) // FILES
      CALL GETLUN('FILEC', LUNCRP)
      OPEN (LUNCRP,FILE = FILECC, STATUS = 'OLD',IOSTAT=ERR)
      IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,0)

      SECTION = '*FORAG'
      CALL FIND(LUNCRP, SECTION, LNUM, FOUND)
      IF (FOUND .EQ. 0) THEN
        CALL ERROR(SECTION, 42, FILECC, LNUM)
      ELSE
        !IGNORE HEADER Line
        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        READ(C80,'(10(F7.0))',IOSTAT=ERR)
     &     STAVEG,SLPVEG,SLPCOB,SLPGRN,STAGRN,CONDIG,
     &     MILKZ0,IRDMC,CVGDMC, SLPMLK
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

      ENDIF

      CLOSE(LUNCRP)
      
      !Initialize 
      VEGDMC = CVGDMC
!----------------------------------------------------------------

C  SET TO ZERO FOR NEW VARIABLES COMPUTED HERE (NOT IN LIST BELOW, LOOK ABOVE FOR NEW REAL)
          EARS1_2   = 0.0
          FWYLD1_2  = 0.0
          EARDMC    = 0.0
          EARFWT    = 0.0
          EARMKT    = 0.0
          EARSFCY   = 0.0
          FWYLDFCY  = 0.0
          FWYIELD   = 0.0
          MKTFWYLD  = 0.0
          XFWYLDFCY = 0.0

!-----------------------------------------------------------------------
!                     DYNAMIC = INTEGR
!-----------------------------------------------------------------------
      ELSEIF(DYNAMIC.EQ.INTEGR) THEN

! KJB - MAYBE INCREASE TOTDMC AS EARDMC STARTS TO INCREASE.  SEPARATE PART MATH
        TOTFWT = TOPWT / CVGDMC
        TOTDMC = TOPWT / TOTFWT

! KJB - STACON = STAVEG (MAY WANT EXTERNAL CONSTANT FOR 0.15)
        STACON = STAVEG
! KJB - PERCENT STARCH CONCENTRATION AND PERCENT PROTEIN
        STPCT = 100.0*(STACON)
        CPPCT = 100.0*(6.25 * WTNCAN/TOPWT)
! KJB - OMDIG = STACON + 6.25 * WTNCAN/TOPWT + CONDIG (CONDIG IS DIGESTIBILITY OF NON-PROTEIN, NON-STARCH)
! KJB - ASSUMES THAT STARCH AND PROTEIN ARE 100% DIGESTIBLE.
        OMDIG = 100.0*(STACON + 6.25 * WTNCAN/TOPWT + CONDIG)
        NEL = 0.0174 * OMDIG + 0.000076 * OMDIG**2
! KJB - FROM FLORES (2004)  UNITS ARE  MCal/kgDM
        UFL = NEL / 1.70
        UFLHA = UFL * TOPWT
! KJB - UFL IS FEED UNITS FOR MILK
! KJB - WANT UFLHA AS FEED UNITS PER HA



          IF (ISTAGE .EQ. 3) THEN

! JIL 04/03/2006 Calculate ear fresh weight
              IF (CUMDTTEG .GT. 0.001) THEN
                COBDMC = IRDMC + 0.0002 * CUMDTTEG ! Fraction (0.05-0.1)
                PODFWT = PODWT / COBDMC           ! g/M2
                EARDMC = COBDMC
! KJB - NEW MATH:  VEGDMC = 0.18 (NEED THIS AS EXTERNAL CONSTANT)
! KJB -           TOTFWT = TOPWT / VEGDMC
! KJB - MAYBE INCREASE TOTDMC AS EARDMC STARTS TO INCREASE.  SEPARATE PART MATH
                TOTFWT = (TOPWT-PODWT) / CVGDMC + PODWT/COBDMC
                TOTDMC = TOPWT / TOTFWT
                EARFRC = PODWT / TOPWT
                SDFRC = 0.0
                MILKLN = 0.0
! KJB - STACON = STAVEG (MAY WANT EXTERNAL CONSTANT FOR 0.15)
                STACON = STAVEG
! KJB - PERCENT STARCH CONCENTRATION AND PERCENT PROTEIN
                STPCT = 100.0*(STACON)
                CPPCT = 100.0*(6.25 * WTNCAN/TOPWT)
! KJB - OMDIG = STACON + 6.25 * WTNCAN/TOPWT + CONDIG (CONDIG IS DIGESTIBILITY OF NON-PROTEIN, NON-STARCH)
! KJB - ASSUMES THAT STARCH AND PROTEIN ARE 100% DIGESTIBLE.
                OMDIG = 100.0*(STACON + 6.25 * WTNCAN/TOPWT + CONDIG)
                NEL = 0.0174 * OMDIG + 0.000076 * OMDIG**2
! KJB - FROM FLORES (2004)  UNITS ARE  MCal/kgDM
                UFL = NEL / 1.70
                UFLHA = UFL * TOPWT
! KJB - UFL IS FEED UNITS FOR MILK
! KJB - WANT UFLHA AS FEED UNITS PER HA
! KJB - FOR ALTERNATIVE TO GET TO UFL,
                UFL2 = EARFRC * 1.08 + (1.0 - EARFRC) * 0.61
C  THIS COMES FROM BRAGA ET AL. 2008
              ENDIF

!      --------------------------------------------------------------------
!         ISTAGE = 4 (Silking to beginning of effective grain filling period)
!      --------------------------------------------------------------------

          ELSEIF (ISTAGE .EQ. 4) THEN

! JIL 04/03/2006 Calculate cob fresh weight (Ear - grain)
            COBDMC = IRDMC + 0.05 + 0.0002*SUMDTT
            COBDMCIS4 = COBDMC
            PODFWT = PODWT / COBDMC           ! g/M2
            EARDMC = COBDMC
! KJB -  NEW MATH:  VEGDMC = 0.18 (NEED THIS AS EXTERNAL CONSTANT)
! KJB -            TOTFWT = TOPWT / VEGDMC
! KJB - MAYBE INCREASE TOTDMC AS EARDMC STARTS TO INCREASE.  SEPARATE PART MATH
            TOTFWT = (TOPWT-PODWT) / CVGDMC + PODWT/COBDMC
            TOTDMC = TOPWT / TOTFWT
            EARFRC = PODWT / TOPWT
            SDFRC = 0.0
            MILKLN = 0.0
! KJB - STACON = STAVEG (MAY WANT EXTERNAL CONSTANT FOR 0.15)
            STACON = STAVEG
! KJB - PERCENT STARCH CONCENTRATION AND PERCENT PROTEIN
            STPCT = 100.0*(STACON)
            CPPCT = 100.0*(6.25 * WTNCAN/TOPWT)
! KJB - OMDIG = STACON + 6.25 * WTNCAN/TOPWT + CONDIG (CONDIG IS DIGESTIBILITY OF NON-PROTEIN, NON-STARCH)
! KJB - ASSUMES THAT STARCH AND PROTEIN ARE 100% DIGESTIBLE.
            OMDIG = 100.0*(STACON + 6.25 * WTNCAN/TOPWT + CONDIG)
            NEL = 0.0174 * OMDIG + 0.000076 * OMDIG**2
! KJB - FROM FLORES (2004)  UNITS ARE  MCal/kgDM
            UFL = NEL / 1.70
            UFLHA = UFL * TOPWT
! KJB - UFL IS FEED UNITS FOR MILK
! KJB - WANT UFLHA AS FEED UNITS PER HA
! KJB - FOR ALTERNATIVE TO GET TO UFL,
            UFL2 = EARFRC * 1.08 + (1.0 - EARFRC) * 0.61
! KJB - THIS COMES FROM BRAGA ET AL. 2008
          !-------------------------------------------------------------
          !   ISTAGE = 5 Effective Grain Filling Period
          !-------------------------------------------------------------

          ELSEIF (ISTAGE .EQ. 5) THEN

! JIL 04/03/2006 Calculate ear fresh weight
! KJB/FO - Removed this equation and calculated using
!      a dummy COBDMCIS4 as a function of MILKLN
!            COBDMC = IRDMC + 0.05 + 0.0002*SUMDTT
!            COBDMC = IRDMC + 0.05 + SLPCOB * 0.0005 * SUMDTT
!            PODFWT = PODWT / COBDMC           ! g/M2
! KJB -           TOTFWT = TOPWT / VEGDMC
            EARFRC = PODWT / TOPWT
            SDFRC = SDWT / TOPWT
! KJB -           MILKLN = FUNCTION OF P5 PROGRESS (0 TO 1.00 CAN WE CREATE IT, 0 STARTS LATER THAN ONSET ISTAGE 5, ENDS PRIOR TO END ISTAGE 5)
! KJB -           NEED VALUE FOR SUMDTT, OR FRACTION OF DISTANCE OF P5
            IF (SUMDTT .GE. MILKZ0) THEN
              MILKLN = SLPMLK * (SUMDTT - MILKZ0) / P5
            ELSE
              MILKLN = 0.0
            ENDIF
! KJB - COMMENTED OUT VEGDMC IS BAD
            VEGDMC = CVGDMC + SLPVEG * MILKLN
            COBDMC = COBDMCIS4 + SLPCOB * MILKLN
            GRNDMC = VEGDMC + SLPGRN * MILKLN
! KJB -  MAYBE INCREASE TOTDMC AS EARDMC STARTS TO INCREASE.  SEPARATE PART MATH
            SDFWT  = SDWT / GRNDMC
            EARFWT = (PODWT - SDWT) / COBDMC + SDFWT
            TOTFWT = (TOPWT - PODWT) / VEGDMC + EARFWT
            TOTDMC = TOPWT / TOTFWT
            EARDMC = PODWT / EARFWT

            PODFWT = PODWT / EARDMC           ! g/M2
! KJB -  TOTAL?           STATOT = SDWT*CONS2*MILKLN+ TOPWT * 0.05 (CONS2 AND 0.05 MAY WANT EXTERNAL CONSTANTS).  !GET CONC,DIVIDE THIS BY TOPWT?
! KJB - STACON AND OMDIG ARE FRACTIONS
! KJB -           STACON = CONS2*MILKLN+ STAVEG (CONS2 AND 0.05 MAY WANT EXTERNAL CONSTANTS AS STARCH CONCENTRATIONS)
            STACON = STAGRN*MILKLN+ STAVEG
! KJB - PERCENT STARCH CONCENTRATION AND PERCENT PROTEIN
            STPCT = 100.0*(STACON)
            CPPCT = 100.0*(6.25 * WTNCAN/TOPWT)
! KJB -           OMDIG = STACON + 6.25 * WTNCAN/TOPWT + CONDIG (CONDIG IS DIGESTIBILITY OF NON-PROTEIN, NON-STARCH)
! KJB -    ASSUMES THAT STARCH AND PROTEIN ARE 100% DIGESTIBLE.
            OMDIG = 100.0*(STACON + 6.25 * WTNCAN/TOPWT + CONDIG)
            NEL = 0.0174* OMDIG + 0.000076 * OMDIG**2
! KJB - FROM FLORES (2004)  UNITS ARE  MCal/kgDM
            UFL = NEL / 1.70
            UFLHA = UFL * TOPWT
! KJB -   UFL IS FEED UNITS FOR MILK
! KJB - WANT UFLHA AS FEED UNITS PER HA
! KJB - FOR ALTERNATIVE TO GET TO UFL,
            UFL2 = EARFRC * 1.08 + (1.0 - EARFRC) * 0.61
! KJB -  THIS COMES FROM BRAGA ET AL. 2008
          ENDIF


!----------------------------------------------------------------------
!   The following code is executed each day regardless of ISTAGE value
!----------------------------------------------------------------------


          !------------------------------------------------------------
          !             MARKET FRESH WEIGHT AND EAR QUALITY
          !------------------------------------------------------------

!     04/04/2006 JIL, Calculating Market fresh weight and ear numbers

          FWYIELD  = EARFWT * EARS * 10.0  ! Total FW yield (kg/ha)
          MKTFWYLD = AMAX1(0.0,(0.9872*FWYIELD-1453.8)*SLPF
     &               *AMIN1(SWFAC,NSTRES)) ! Marketable FW yield (kg/ha)

!            Max fraction of Fancy FW in Marketable FW
          IF (MKTFWYLD .GT. 0.0) THEN
             XFWYLDFCY = AMAX1(0.0,0.919 * (1.0-EXP(-0.00012*MKTFWYLD)))
             FWYLDFCY = XFWYLDFCY * MKTFWYLD
             EARSFCY  = 4.053 * FWYLDFCY
          ELSE
             FWYLDFCY = 0.0
             EARSFCY  = 0.0
          ENDIF

          FWYLD1_2 = MKTFWYLD - FWYLDFCY ! US1+US2 ears FW yield (kg/ha)
          EARS1_2  = 7.0375 * FWYLD1_2   ! US1+US2 ear number (ear/ha)
          EARMKT = EARSFCY + EARS1_2     ! Marketable ear number(ear/ha)

!----------------------------------------------------------------------
!                     DYNAMIC = OUTPUT
!----------------------------------------------------------------------

      ELSEIF(DYNAMIC.EQ.OUTPUT) THEN

!     04/04/2006 JIL, Output file for sweet corn fresh mass simulation
      IF (ISTAGE .GE. 3) THEN

        FROP = CONTROL % FROP
        DAS  = CONTROL % DAS
        YRDOY= CONTROL % YRDOY
        DAP  = TIMDIF(YRPLT, YRDOY)

        IF ((MOD(DAS,FROP) .EQ. 0)    !Daily output every FROP days,
!     &      .OR. (YRDOY .EQ. STGDOY(3))     !on tassel init date, and
     &      .OR. (YRDOY .EQ. MDATE)) THEN   !at harvest maturity

          CALL YR_DOY(YRDOY, YEAR, DOY)

 ! KJB - NEW OUTPUT:  TOTFWT,VEGDMC, TOTDMC, EARFWT,EARDMC, EARFRC,SDFRC,TOPWT, PODWT,SDWT,
 ! KJB - NEW OUTPUT:  TOTDMC, VEGDMC, SLPVEG, SLPGRN, GRNDMC, STAVEG, STAGRN, CONDIG, MILKLN,STACON,OMDIG,NEL,UFL,UFL2, UFLHA
 ! KJB - TOTFWT & UFLHA & OTHERS? NEED TO BE OUTPUT PER HECTARE.  SEE OTHER OUTPUT MATH, AND MULTIPLY BY 10 TO GO G/M2 TO KG/HA

!OVERVIEW.OUT
!TOTFWT, PODFWT, TOTDMC, EARDMC, GRNDMC,MILKLN, STPCT, OMDIG, CPPCT, UFL, UFLHA
! PODWT - Check if exists on OVerview.out

 ! FO - Write output for Forage.OUT
           WRITE (NOUTSG,1111) YEAR, DOY, DAS, DAP,
     &            NINT(TOTFWT*10),VEGDMC*100.0,TOTDMC*100.0,
     &            NINT(PODFWT*10.0),EARDMC*100.0,NINT(SDFWT*10.0),
     &            COBDMC*100.0,GRNDMC*100.0,
     &            EARFRC,SDFRC,NINT(TOPWT* 10.),
     &            NINT(PODWT*10.0),NINT(SDWT*10.0),
     &            MILKLN,STACON,STPCT,CPPCT,OMDIG,
     &            NEL,UFL,UFL2,NINT(UFLHA*10.0),WTNCAN*10.0

 1111 FORMAT(1X,I4,1X,I3.3,2(1X,I5),I7,2(F7.2),
     &       I7,F7.2,I7,4(F7.2),3(I7),8(F7.2),I7,F7.1)

        ENDIF
      ENDIF


!----------------------------------------------------------------------
!                     DYNAMIC = SEASEND
!----------------------------------------------------------------------

      ELSEIF(DYNAMIC.EQ.SEASEND) THEN

        CLOSE (NOUTSG)
      ENDIF       !Endif for DYNAMIC LOOP

      RETURN

      END SUBROUTINE MZ_FreshWt
