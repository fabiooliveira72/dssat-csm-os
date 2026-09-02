!=======================================================================
!  Module for AMEI output data
!-----------------------------------------------------------------------
!  01/30/2026 FO  Written
!-----------------------------------------------------------------------
!=======================================================================
      MODULE AMEISummaryData
        IMPLICIT NONE
        SAVE
        TYPE AMEIOutputSM
        INTEGER PDATE	!YYYY-MM-DD	planting_date	Growth stage of planting, as date
        INTEGER PLDAE	!YYYY-MM-DD	emergence_date	Growth stage date, emergence
        INTEGER ADAT	!YYYY-MM-DD	anthesis_date	Growth stage date, anthesis
        INTEGER MDAT	!YYYY-MM-DD	physiologic_maturity_dat	Growth stage of physiol. maturity, as date
        REAL LnoSM	!leaf\mainstem	leaf_no_per_stem_matur	Final leaf number per mainstem
        REAL LAIX	!m²/m²	leaf_area_index_maximum	Leaf area index, maximum during season
        !REAL LIPCCM	!%	PAR_interception_over_season	Cumultative PAR (light) interception, planting to maturity
        INTEGER CWAA	!kg[DM]/ha	tops_dry_weight_anthesis	Tops dry weight at anthesis
        INTEGER CWAM	!kg[DM]/ha	tops_dry_weight_maturity	Tops dry weight at maturity
        INTEGER GWAM	!kg[DM]/ha	grain_dry_wt_at_mat	Grain dry wt at maturity
        REAL HnoAM	!number/m2	harvest_no_at_maturity	Harvest number per area at maturity (e.g., seed or tubers)
        REAL GWGM	!mg[DM]/grain	grain_unit_dry_wt_matur	Grain unit dry wt at maturity
        REAL CNAA	!kg[N]/ha	tops_N_at_anthesis	Nitrogen in above ground plant parts at anthesis
        INTEGER CNAM	!kg[N]/ha	tops_N_at_maturity	Nitrogen in above ground plant parts at maturity
        INTEGER GNAM	!kg[N]/ha	grain_N_at_maturity	Grain N at maturity
        !GNGM	mg[N]/grain	grain_unit_N_matur	Grain N at maturity
        REAL RDPM	!m	root_depth_maximum	Root depth, maximum extent druring season
        !WAVSSM	mm	avail_water_soil_profile_sow_mat	Total available water in soil profile from sowing to maturity
        INTEGER DRCM	!mm	drainage_over_season	Cumulative drainage from bottom of soil profile over season, planting to maturity
        INTEGER ROCM	!mm	runoff_over_season	Cumulative runoff over season, planting to maturity
        !NIAVSSM	kg[N]/ha	avail_N_inorg_soil_profile_sow_mat	Total available soil N in root profile, planting to maturity
        INTEGER NLCM	!kg[N]/ha	N_leached_during_season	Cumulative N leaching from bottom of soil profile over season, planting to maturity
        REAL NMNCM	!kg[N]/ha	N_mineralization_during_season	Cumulative N minieralization from bottom of soil profile over season, planting to maturity
        REAL N2OECM	!kg[N]/ha	N2O_emissions__over_season	Cumulative N2O emission over season, planting to maturity
        REAL NIMCM	!kg[N]/ha	N_immobilization_cumul	Cumulative N immobilisation over season, planting to maturity
        REAL NDNCM	!kg[N]/ha	N_denitrification_over_season	Cumulative N denitrification over season, planting to maturity
        REAL EOCM	!mm	potential_evapotrans_over_season	Cumulative potential evapotranspiration over season, planting to maturity
        REAL ETCM	!mm	evapotrans_over_season	Cumulative evapotranspiration over season, planting to maturity
        REAL EPSCM	!mm	potential_soil_evaporation_over_season	Cumulative potential soil evaporation over season, planting to maturity
        REAL ESCM	!mm	soil_evap_over_season	Cumulative soil evaporation over season, planting to maturity
        REAL EPPCM	!mm	potential_transpiration_over_season	Cumulative potential plant transpiration over season, planting to maturity
        REAL EPCM	!mm	transpiration_over_season	Cumulative plant transpiration over season, planting to maturity
        END TYPE

        TYPE(AMEIOutputSM) SUMMOUTAMEI

      END MODULE
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
      SUBROUTINE OPSTEMP_AMEI_SM(CONTROL, ISWITCH)

!-----------------------------------------------------------------------
      USE ModuleDefs
      USE ModuleData
!     VSH
      USE CsvOutput
      USE Linklist
      USE AMEISummaryData
      IMPLICIT NONE
      EXTERNAL GETLUN, HEADER, YR_DOY, YRDOYTOYYYYMMDD
      SAVE
!-----------------------------------------------------------------------
      CHARACTER*1  RNMODE, METMP
      CHARACTER*2  FM, STM, CMONTH, CDAY
      CHARACTER*8  CRNET
      CHARACTER*10 CPDATE, CPLDAE, CADAT, CMDAT 
      CHARACTER*50 OUTSM, SITE

      INTEGER DAS, DOY, DYNAMIC, ERRNUM, FROP, L, N_LYR
      INTEGER NOUTDT, RUN, YEAR, YRDOY, MONTH, DAY, REPNO
      INTEGER DATE_TIME(8), TRTNUM
      REAL ST(NL), SRFTEMP, SW(NL), EOS,ES,EO,ET, RNET


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
      
      CALL YR_DOY(YRDOY, YEAR, DOY)
!***********************************************************************
!***********************************************************************
!     Seasonal initialization - run once per season
!***********************************************************************
      IF (DYNAMIC .EQ. SEASINIT) THEN
!-----------------------------------------------------------------------
      RNMODE  = CONTROL % RNMODE
      REPNO   = CONTROL % REPNO
      RUN     = CONTROL % RUN

      IF (FMOPT == 'A' .OR. FMOPT == ' ') THEN   ! VSH
        CALL GETLUN('OUTSM',NOUTDT)
!       Create file name.
!       STempModelCode|ModelingFrameworkCode|Summary|Maricopa|.txt
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
          WRITE(OUTSM,'(A2,A2,A,A,A4)')STM,FM,
     &          'Summary',TRIM(SITE),'.txt'
        ELSE
          OUTSM = 'SoilTemp_AMEI_No_ST_Model.txt'
          STM = 'DF'
        ENDIF
        
        DOPRINT = .FALSE.
        INQUIRE (FILE = OUTSM, EXIST = FEXIST)
        IF (FEXIST .AND. RUN == 1) THEN
          OPEN (UNIT=NOUTDT, FILE=OUTSM, STATUS='REPLACE',
     &      IOSTAT = ERRNUM)
          DOPRINT = .TRUE.
        ELSE
          OPEN (UNIT=NOUTDT, FILE=OUTSM, STATUS='NEW',
     &      IOSTAT = ERRNUM)
          DOPRINT = .TRUE.
        ENDIF
      ENDIF

        IF (DOPRINT .AND. RUN == 1) THEN
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
     &    'treatment_ID	planting_date	emergence_date	' //
     &    'anthesis_date	physiologic_maturity_dat	' //
     &    'leaf_no_per_stem_matur	leaf_area_index_maximum	' //
     &    'PAR_interception_over_season	' //
     &    'tops_dry_weight_anthesis	tops_dry_weight_maturity	' //
     &    'grain_dry_wt_at_mat	harvest_no_at_maturity	' //
     &    'grain_unit_dry_wt_matur	tops_N_at_anthesis	' //
     &    'tops_N_at_maturity	grain_N_at_maturity	' //
     &    'grain_unit_N_matur	root_depth_maximum	' //
     &    'avail_water_soil_profile_sow_mat	' //
     &    'drainage_over_season	runoff_over_season	' //
     &    'avail_N_inorganic_soil_profile_over_season	' //
     &    'N_leached_during_season	' //
     &    'N_mineralization_during_season	' //
     &    'N2O_emissions__over_season	N_immobilization_cumul	' //
     &    'N_denitrification_over_season	' //
     &    'potential_evapotrans_over_season	' //
     &    'evapotrans_over_season	' //
     &    'potential_soil_evaporation_over_season	' //
     &    'soil_evap_over_season	' //
     &    'potential_transpiration_over_season	' //
     &    'transpiration_over_season'
          WRITE(NOUTDT,'(A)') 'text	text	text	date	date	' //
     &    'date	date	leaf\mainstem	m /m 	%	kg[DM]/ha	' //
     &    'kg[DM]/ha	kg[DM]/ha	number/m2	mg[DM]/grain	' //
     &    'kg[N]/ha	kg[N]/ha	kg[N]/ha	mg[N]/grain	m	mm	' //
     &    'mm	mm	kg[N]/ha	kg[N]/ha	kg[N]/ha	kg[N]/ha	' //
     &    'kg[N]/ha	kg[N]/ha	mm	mm	mm	mm	mm	mm'
          WRITE(NOUTDT,'(A)') 'FRAMEWORK_ID	MODEL_ID	' //
     &    'TREAT_ID	PDATE	PLDAE	ADAT	MDAT	LnoSM	LAIX	' //
     &    'LIPCCM	CWAA	CWAM	GWAM	HnoAM	GWGM	CNAA	' //
     &    'CNAM	GNAM	GNGM	RDPM	WAVSSM	DRCM	ROCM	' //
     &    'NIAVSSM	NLCM	NMNCM	N2OECM	NIMCM	NDNCM	' //
     &    'EOCM	ETCM	EPSCM	ESCM	EPPCM	EPCM'
        ENDIF
      ENDIF !DYNAMIC
!***********************************************************************
!***********************************************************************
!     Daily Output
!***********************************************************************
      DOPRINT = .FALSE.
      SELECT CASE (DYNAMIC)
      CASE (SEASEND)
          DOPRINT = .TRUE.
      END SELECT

      IF (DOPRINT) THEN
        IF (FMOPT == 'A' .OR. FMOPT == ' ') THEN
          CALL YRDOYTOYYYYMMDD(SUMMOUTAMEI % PDATE,CPDATE)
          CALL YRDOYTOYYYYMMDD(SUMMOUTAMEI % PLDAE,CPLDAE)
          CALL YRDOYTOYYYYMMDD(SUMMOUTAMEI % ADAT ,CADAT)
          CALL YRDOYTOYYYYMMDD(SUMMOUTAMEI % MDAT ,CMDAT)

          WRITE(NOUTDT,300) FM, TAB, STM, TAB, TRTNUM, 
     &        TAB,CPDATE,TAB,CPLDAE,TAB,CADAT,TAB,CMDAT,
     &        TAB, SUMMOUTAMEI % LnoSM  , 
     &        TAB, SUMMOUTAMEI % LAIX   ,
     &        TAB, 'na'                 , !LIPCCM	  
     &        TAB, SUMMOUTAMEI % CWAA   ,  
     &        TAB, SUMMOUTAMEI % CWAM   , 
     &        TAB, SUMMOUTAMEI % GWAM   , 
     &        TAB, SUMMOUTAMEI % HnoAM	,  
     &        TAB, SUMMOUTAMEI % GWGM   , 
     &        TAB, SUMMOUTAMEI % CNAA	  , 
     &        TAB, SUMMOUTAMEI % CNAM   , 
     &        TAB, SUMMOUTAMEI % GNAM   , 
     &        TAB, 'na'                 , !GNGM
     &        TAB, SUMMOUTAMEI % RDPM	  , 
     &        TAB, 'na'                 , !WAVSSM
     &        TAB, SUMMOUTAMEI % DRCM   ,
     &        TAB, SUMMOUTAMEI % ROCM   ,
     &        TAB, 'na'                 , !NIAVSSM
     &        TAB, SUMMOUTAMEI % NLCM   , 
     &        TAB, SUMMOUTAMEI % NMNCM  , 
     &        TAB, SUMMOUTAMEI % N2OECM , 
     &        TAB, SUMMOUTAMEI % NIMCM  , 
     &        TAB, SUMMOUTAMEI % NDNCM  , 
     &        TAB, SUMMOUTAMEI % EOCM	  , 
     &        TAB, SUMMOUTAMEI % ETCM   , 
     &        TAB, SUMMOUTAMEI % EPSCM  ,   
     &        TAB, SUMMOUTAMEI % ESCM	  , 
     &        TAB, SUMMOUTAMEI % EPPCM  ,
     &        TAB, SUMMOUTAMEI % EPCM     

  300 FORMAT(A,A1,A,A1,I3,
     &       4(A1,A10),
     &        A1, F15.3,
     &        A1, F15.3,
     &        A1, A2,
     &        A1, I10,
     &        A1, I10,
     &        A1, I10,
     &        A1, F15.3,
     &        A1, F15.3,
     &        A1, F15.3,
     &        A1, I10,
     &        A1, I10,
     &        A1, A2,
     &        A1, F15.3,
     &        A1, A2,
     &        A1, I10,
     &        A1, I10,
     &        A1, A2,
     &        A1, I10,
     &        A1, F15.3,
     &        A1, F15.3,
     &        A1, F15.3,
     &        A1, F15.3,
     &        A1, F15.3,
     &        A1, F15.3,
     &        A1, F15.3,
     &        A1, F15.3,
     &        A1, F15.3,
     &        A1, F15.3)

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
        !CLOSE (NOUTDT)

!***********************************************************************
!***********************************************************************
!     END OF DYNAMIC IF CONSTRUCT
!***********************************************************************
      ENDIF
!***********************************************************************
      RETURN
      END SUBROUTINE OPSTEMP_AMEI_SM
!***********************************************************************
!=======================================================================
!  Convert YRDOY to YYYY-MM-DD, FO
!-----------------------------------------------------------------------
!  01/30/2026 FO  Written
!-----------------------------------------------------------------------
!=======================================================================
      SUBROUTINE YRDOYTOYYYYMMDD(YRDOY,YYYYMMDD)
        IMPLICIT NONE
        EXTERNAL YR_DOY, ETAD_NAILUJ

        INTEGER YRDOY, YEAR, DOY, MONTH, DAY
        CHARACTER*2 CMONTH, CDAY
        CHARACTER*10 YYYYMMDD

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

          WRITE(YYYYMMDD,'(I4,A1,A2,A1,A2)')
     &        YEAR,'-',CMONTH,'-',CDAY 
        
        RETURN
      END SUBROUTINE