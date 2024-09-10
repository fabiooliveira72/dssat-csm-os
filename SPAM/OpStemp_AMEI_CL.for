C=======================================================================
C  OPSTEMP_AMEI, Subroutine, C.H.Porter
C  Generates output for daily Canopy level
C-----------------------------------------------------------------------
C  REVISION HISTORY
C  09/01/2024 FO Written
C-----------------------------------------------------------------------
C  Called from:   STEMP
C  Calls:         None
C=======================================================================
      SUBROUTINE OPSTEMP_AMEI_CL(CONTROL, ISWITCH, 
     &                           EOS,ES,EO,ET)

!-----------------------------------------------------------------------
      USE ModuleDefs
      USE ModuleData
!     VSH
      USE CsvOutput
      USE Linklist
      IMPLICIT NONE
      EXTERNAL GETLUN, HEADER, YR_DOY
      SAVE
!-----------------------------------------------------------------------
      CHARACTER*1  RNMODE, METMP
      CHARACTER*2  FM, STM, CMONTH, CDAY
      CHARACTER*50 OUTCL, SITE

      INTEGER DAS, DOY, DYNAMIC, ERRNUM, FROP, L, N_LYR
      INTEGER NOUTDT, RUN, YEAR, YRDOY, MONTH, DAY, REPNO
      REAL ST(NL), SRFTEMP, SW(NL), EOS,ES,EO,ET

      LOGICAL FEXIST, DOPRINT

      CHARACTER*1, PARAMETER:: TAB = ACHAR(9)

!-----------------------------------------------------------------------
!     The variable "CONTROL" is of constructed type "ControlType" as
!     defined in ModuleDefs.for, and contains the following variables.
!     The components are copied into local variables for use here.
!-----------------------------------------------------------------------
      TYPE (ControlType) CONTROL
      TYPE (SwitchType)  ISWITCH
      TYPE (SoilType)    SOILPROP

      IF (INDEX('N0',ISWITCH % IDETL) > 0) RETURN

      DAS     = CONTROL % DAS
      DYNAMIC = CONTROL % DYNAMIC
      FROP    = CONTROL % FROP
      YRDOY   = CONTROL % YRDOY

      FMOPT   = ISWITCH % FMOPT
      METMP   = ISWITCH % METMP
      
      CALL YR_DOY(YRDOY, YEAR, DOY)
!***********************************************************************
!***********************************************************************
!     Seasonal initialization - run once per season
!***********************************************************************
!      ELSEIF (DYNAMIC .EQ. SEASINIT) THEN
      IF (DYNAMIC .EQ. SEASINIT) THEN
!-----------------------------------------------------------------------
      RNMODE  = CONTROL % RNMODE
      REPNO   = CONTROL % REPNO
      RUN     = CONTROL % RUN

      IF (FMOPT == 'A' .OR. FMOPT == ' ') THEN   ! VSH
        CALL GETLUN('OUTCL',NOUTDT)
!       Create file name.
!       STempModelCode|ModelingFrameworkCode|LayersAimes|Year.txt
        FM = 'DC'
        SITE = 'Aimes'

        SELECT CASE (METMP)
          CASE('F') ! BIOMA-Parton
            STM = 'PS'
            WRITE(OUTCL,'(A2,A2,A,I4,A4)')STM,FM,
     &                   TRIM(SITE),YEAR,'.txt'
          CASE('G') ! BIOMA-SWAT
            STM = 'DS'
            WRITE(OUTCL,'(A2,A2,A,I4,A4)')STM,FM,
     &                   TRIM(SITE),YEAR,'.txt'
          CASE('H') ! C2ML DSSAT-EPIC
            STM = 'DE'
            WRITE(OUTCL,'(A2,A2,A,I4,A4)')STM,FM,
     &                   TRIM(SITE),YEAR,'.txt'
          CASE('I') ! C2ML DSSAT
            STM = 'DC'
            WRITE(OUTCL,'(A2,A2,A,I4,A4)')STM,FM,
     &                   TRIM(SITE),YEAR,'.txt'
          CASE('J') ! MONICA
            STM = 'MO'
            WRITE(OUTCL,'(A2,A2,A,I4,A4)')STM,FM,
     &                   TRIM(SITE),YEAR,'.txt'
          CASE('K') ! Simplace
            STM = 'SA'
            WRITE(OUTCL,'(A2,A2,A,I4,A4)')STM,FM,
     &                   TRIM(SITE),YEAR,'.txt'
          CASE('L') ! SIRIUS-Quality
            STM = 'SQ'
            WRITE(OUTCL,'(A2,A2,A,I4,A4)')STM,FM,
     &                   TRIM(SITE),YEAR,'.txt'
          CASE('M') ! STICS
            STM = 'ST'
            WRITE(OUTCL,'(A2,A2,A,I4,A4)')STM,FM,
     &                   TRIM(SITE),YEAR,'.txt'
          CASE DEFAULT
            OUTCL = 'SoilTemp_AMEI_No_ST_Model.OUT'
            STM = 'DF'
        END SELECT
        
        INQUIRE (FILE = OUTCL, EXIST = FEXIST)
        IF (FEXIST) THEN
          OPEN (UNIT=NOUTDT, FILE=OUTCL, STATUS='REPLACE',
     &      IOSTAT = ERRNUM)

        ELSE
          OPEN (UNIT=NOUTDT, FILE=OUTCL, STATUS='NEW',
     &      IOSTAT = ERRNUM)
        ENDIF
      ENDIF

 !        Write headers info to daily output file
          WRITE(NOUTDT,'(A)') 'AMEI Aimes fallow'
          WRITE(NOUTDT,'(A)') 'Model: DSSAT'
          WRITE(NOUTDT,'(A)') 'Modeler_name: ' //
     &    'Fabio Oliveira, Gerrit Hoogenboom and Thiago Ferreira'
          WRITE(NOUTDT,'(A)') 'potential_evaporation	' //
     &    'soil_evaporation_daily	potential_evapotrans	' //
     &    'evapotranspiration_daily	ground_heat_daily	' //
     &    'latent_heat_daily	net_radiation_daily'
          WRITE(NOUTDT,'(A)') 'Framework	Model	Date	' //
     &    'mm/d	mm/d	mm/d	mm/d	w/m2	w/m2	w/m2'
          WRITE(NOUTDT,'(A)') '(2letters)	(2letters)	(YYYY-MM-DD)	' //
     &    'EPAD	ESAD	EOAD	ETAD	GHFD	LHFD	RHFD'

      ENDIF !DYNAMIC
!***********************************************************************
!***********************************************************************
!     Daily Output
!***********************************************************************
      DOPRINT = .FALSE.
      SELECT CASE (DYNAMIC)
      CASE (OUTPUT)
        IF (MOD(DAS, FROP) == 0) THEN
          DOPRINT = .TRUE.
        ENDIF
      CASE (SEASEND)
        IF (MOD(DAS, FROP) /= 0) THEN
          DOPRINT = .TRUE.
        ENDIF
      END SELECT

      CALL GET(SOILPROP)

      IF (DOPRINT) THEN
        IF (FMOPT == 'A' .OR. FMOPT == ' ') THEN
          CALL YR_DOY(YRDOY, YEAR, DOY)
          CALL ETAD_NAILUJ (DOY,YEAR,MONTH,DAY)
          IF(MONTH < 10) THEN
            WRITE(CMONTH,'(A1,I1)') '0',MONTH
          ELSE
            WRITE(CMONTH,'(I2)') MONTH
          ENDIF
          IF(DAY < 10) THEN
            WRITE(CDAY,'(A1,I1)') '0',DAY
          ELSE
            WRITE(CDAY,'(I2)') DAY
          ENDIF


          WRITE(NOUTDT,300) FM, TAB, STM, 
     &        TAB, YEAR,'-',CMONTH,'-',CDAY,
     &        TAB, EOS, TAB, ES, TAB, EO, TAB, ET, 
     &        TAB, 'na', TAB, 'na', TAB, 'na'
     
  300 FORMAT(A,A1,A,A1,
     &       I4,A1,A2,A1,A2,A1,
     &       4(F8.3,A1),
     &       3(A2,A1))
          
        ENDIF

      ENDIF

!***********************************************************************
!***********************************************************************
!     SEASEND
!***********************************************************************
!      IF (DYNAMIC .EQ. SEASEND) THEN
      IF ((DYNAMIC == SEASEND)
     & .AND. (FMOPT == 'A'.OR.FMOPT == ' ')) THEN ! VSH
!-----------------------------------------------------------------------
        CLOSE (NOUTDT)

!***********************************************************************
!***********************************************************************
!     END OF DYNAMIC IF CONSTRUCT
!***********************************************************************
      ENDIF
!***********************************************************************
      RETURN
      END SUBROUTINE OPSTEMP_AMEI_CL
!***********************************************************************
