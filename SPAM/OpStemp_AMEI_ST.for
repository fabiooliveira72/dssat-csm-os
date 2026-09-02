C=======================================================================
C  OPSTEMP_AMEI, Subroutine, C.H.Porter
C  Generates output for daily soil temperature data for AMEI
C-----------------------------------------------------------------------
C  REVISION HISTORY
C  09/01/2024 FO Written
C-----------------------------------------------------------------------
C  Called from:   STEMP
C  Calls:         None
C=======================================================================
      SUBROUTINE OPSTEMP_AMEI_ST(CONTROL, ISWITCH, DOY, SRFTEMP, 
     &                         ST, SW)

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
      CHARACTER*50 OUTST, SITE

      INTEGER DAS, DOY, DYNAMIC, ERRNUM, FROP, L, N_LYR
      INTEGER NOUTDT, RUN, YEAR, YRDOY, MONTH, DAY, REPNO
      INTEGER DATE_TIME(8), TRTNUM
      REAL ST(NL), SRFTEMP, SW(NL)

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
      TRTNUM  = CONTROL % TRTNUM

      FMOPT   = ISWITCH % FMOPT
      METMP   = ISWITCH % METMP
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
        CALL GETLUN('OUTST',NOUTDT)
!       Create file name.
!       STempModelCode|ModelingFrameworkCode|LayersMaricopa|Trt|.txt
        FM = 'DC'
        SITE = 'Maricopa'

        CALL YR_DOY(YRDOY, YEAR, DOY)

        SELECT CASE (METMP)
          CASE('F') ! APSIM
            STM = 'AP'
          CASE('G') ! BIOMA-Parton
            STM = 'PS'
          CASE('H') ! BIOMA-SWAT
            STM = 'SW'
          CASE('I') ! C2ML DSSAT-EPIC
            STM = 'DE'
          CASE('J') ! C2ML DSSAT
            STM = 'DS'
          CASE('K') ! MONICA
            STM = 'MO'
          CASE('L') ! Simplace
            STM = 'SA'
          CASE('M') ! SIRIUS-Quality
            STM = 'SQ'
          CASE('N') ! STICS
            STM = 'ST'
          CASE DEFAULT
            STM = 'DF'
        END SELECT

        IF (INDEX('FGHIJKLMN',METMP) > 0) THEN
          WRITE(OUTST,'(A2,A2,A,A,I3,A4)')STM,FM,
     &          'Layers',TRIM(SITE),TRTNUM,'.txt'
        ELSE
          OUTST = 'SoilTemp_AMEI_No_ST_Model.txt'
          STM = 'DF'
        ENDIF
        
        INQUIRE (FILE = OUTST, EXIST = FEXIST)
        IF (FEXIST) THEN
          OPEN (UNIT=NOUTDT, FILE=OUTST, STATUS='REPLACE',
     &      IOSTAT = ERRNUM)
        ELSE
          OPEN (UNIT=NOUTDT, FILE=OUTST, STATUS='NEW',
     &      IOSTAT = ERRNUM)
        ENDIF
      ENDIF


 !        Write headers info to daily output file
          WRITE(NOUTDT,'(A)') 'Maricopa Wheat FACE'
        CALL DATE_AND_TIME (VALUES=DATE_TIME)
        WRITE (NOUTDT,100) 'Model: DSSAT Cropping System Model Ver. ',
     &    Version%Major,'.', Version%Minor,'.',
     &    Version%Model,'.', Version%Build,
     &    VBranch, MonthTxt(DATE_TIME(2)),' ',
     &    DATE_TIME(3), ', ', DATE_TIME(1), '; ', 
     &    DATE_TIME(5), ':',DATE_TIME(6),':', DATE_TIME(7)
  100   FORMAT (A,
     &    I1,A1,I1,A1,
     &    I1,A1,I3.3,4X,
     &    A,4X,A3,A1,
     &    I2.2,A2,I4,A2,
     &    I2.2,A1,I2.2,A1,I2.2)
          WRITE(NOUTDT,'(A)') 'Modeler_name: ' //
     &    'Fabio Oliveira, Gerrit Hoogenboom and Thiago Ferreira'
          WRITE(NOUTDT,'(A)') 'framework_ID	model_ID	' //
     &    'treatment_ID	date	soil_layer_top_depth	' //
     &    'soil_layer_base_depth	soil_temp_daily_avg	' //
     &    'maximum_soil_temp_daily	minimum_soil_temp_daily	' //
     &    'soil_water_by_layer	soil_N_by_layer'
          WRITE(NOUTDT,'(A)') 'text	text	text	(YYYY-MM-DD)	' //
     &    'cm	cm	°C	°C	°C	cm3/cm3	kg[N]/ha'
          WRITE(NOUTDT,'(A)') 'FRAMEWORK_ID	MODEL_ID	TREAT_ID	' //
     &    'DATE	SLLT	SLLB	TSAV	TSMX	TSMN	SWLD	SNLD'


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
          

          DO L = 1, SOILPROP % NLAYR
            IF(L .EQ. 1) THEN
              WRITE (NOUTDT,300) FM, TAB, STM, TAB, TRTNUM, TAB,
     &          YEAR,'-',CMONTH,'-',CDAY,
     &          TAB, INT(SOILPROP%DS(L)-SOILPROP%DS(L)), TAB, 
     &          INT(SOILPROP%DS(L)), 
     &          TAB, ST(L), TAB, 'na', TAB, 'na', TAB,
     &          SW(L), TAB, 'na'
            ELSE
              WRITE (NOUTDT,300) FM, TAB, STM, TAB, TRTNUM, TAB,
     &          YEAR,'-',CMONTH,'-',CDAY,
     &          TAB, INT(SOILPROP%DS(L-1)), TAB, 
     &          INT(SOILPROP%DS(L)), 
     &          TAB, ST(L), TAB, 'na', TAB, 'na', TAB, 
     &          SW(L), TAB, 'na'
            ENDIF
          ENDDO

  300 FORMAT(A2,A1,A2,A1,I3,A1
     &       I4,A1,A2,A1,A2,A1,
     &       I4,A1,
     &       I4,A1,
     &       F8.3,A1,A,A1,A,A1,
     &       F8.3,A1,A)
          
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
      END SUBROUTINE OPSTEMP_AMEI_ST
!***********************************************************************
