      MODULE AMEIDailyData
        IMPLICIT NONE
        SAVE
        TYPE AMEIOutputCL
        REAL :: LNUM	= 0.0 !leaf\mainstem	leaf_number_as_haun_stg	Leaf number of cereals as Haun stage
        REAL :: GSTZD = 0.0 !	number	growth_stage_Zadoks	Zdaoks growth stage
        REAL :: LAID	= 0.0 !m²/m²	leaf_area_index	Leaf area index on a given day
        REAL :: LIPCD = 0.0	!%	PAR_interception_daily	Fraction of incident PAR intercepted by the canopy
        REAL :: CWAD	 = 0.0!kg[DM]/ha	tops_dry_weight	Tops dry weight on a given day
        REAL :: GWAD	 = 0.0!kg[DM]/ha	grain_dry_weight	Grain dry weight on a given day
        REAL :: GWGD	 = 0.0!mg[DM]/grain	grain_unit_dry_weight	Grain unit dry weight on a given day
        REAL :: CNAD	 = 0.0!kg[N]/ha	tops_N	Nitrogen in above ground plant parts on a given day
        REAL :: GNAD	 = 0.0!kg[N]/ha	grain_N	Grain N on a given day
        REAL :: GNGD	 = 0.0!mg[N]/grain	grain_unit_N	Grain N mass per grain on a given day
        REAL :: RDPD	 = 0.0!m	root_depth	Root depth, maximum extent at date
        !SWWPD	cm3/cm3	soil_water_whole_profile	Stem dry weight per plant
        REAL :: DRND	 = 0.0 !mm/d	drainage_daily	Drainage, daily from bottom of profile
        REAL :: ROFD	 = 0.0 !mm/d	runoff_surface	Runoff, daily from soil surface
        REAL :: NIAD	 = 0.0 !kg[N]/ha/d	N_inorganic_day	N, inorganic, in whole soil profile on a given day
        REAL :: NLCD	 = 0.0 !kg[N]/ha/d	N_leached_day	N leached, daily from bottom of profile
        REAL :: NMND	 = 0.0 !kg[N]/ha/d	N_mineralization_day	Soil nitrogen mineralization, daily for the whole soil profile
        REAL :: N2OED = 0.0 !	kg[N]/ha/d	N2O_emissions_day	Daily N2O emitted from soil
        REAL :: NIMD	 = 0.0 !kg[N]/ha/d	N_immobilization_day	Soil nitrogen immobilization, daily for the whole soil profile
        REAL :: NDND	 = 0.0 !kg[N]/ha/d	N_denitrification_day	Soil nitrogen denitrification, daily for the whole soil profile
        !GHFD	w/m2	ground_heat_daily	Daily ground heat flux
        !LHFD	w/m2	latent_heat_daily	Daily latent heat flux
        !HHFD	w/m2	sensible_heat_daily	Daily sensible heat flux
        !RND	w/m2	net_radiation_daily	Daily net radiation
        REAL :: TSSAV = 0.0 !°C	soil_temp_surface_daily_avg	Temperature of soil surface, daily average
        !TSSMX	°C	soil_temp_surface_daily_max	Temperature of soil surface, daily maximum
        !TSSMN	°C	soil_temp_surface_daily_min	Temperature of soil surface, daily minimum
        REAL :: TGAV	= 0.0 !°C	canopy_temp_daily_avg	Temperature of canopy, daily average
        !TGMX	°C	canopy_temp_daily_max	Temperature of canopy, daily maximum
        !TGMN	°C	canopy_temp_daily_min	Temperature of canopy, daily minimum
        REAL :: EOAD	 = 0.0 !mm/d	potential_evapotrans	Daily evapotranspiration, potential
        REAL :: ETAD	 = 0.0 !mm/d	evapotranspiration_daily	Daily evapotranspiration, actual
        REAL :: EPSAD = 0.0 !	mm/d	potential_soil_evaporation_daily	Daily soil evaporation, potential
        REAL :: ESAD	 = 0.0 !mm/d	soil_evaporation_daily	Daily soil evaporation, actual
        REAL :: EPPAD = 0.0 !	mm/d	potential_transpiration_daily	Daily transpiration, potential
        REAL :: EPAD	 = 0.0 !mm/d	transpiration_daily	Daily transpiration, actual
        END TYPE

        TYPE(AMEIOutputCL) DAILYOUTAMEI

        CONTAINS

        SUBROUTINE INITAMEICL()
          IMPLICIT NONE
          DAILYOUTAMEI = AMEIOutputCL()
        END SUBROUTINE INITAMEICL

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
      SUBROUTINE OPSTEMP_AMEI_CL(CONTROL, ISWITCH)

!-----------------------------------------------------------------------
      USE ModuleDefs
      USE ModuleData
!     VSH
      USE CsvOutput
      USE Linklist
      USE AMEIDailyData
      IMPLICIT NONE
      EXTERNAL GETLUN, HEADER, YR_DOY
      SAVE
!-----------------------------------------------------------------------
      CHARACTER*1  RNMODE, METMP
      CHARACTER*2  FM, STM, CMONTH, CDAY
      CHARACTER*8  CRNET
      CHARACTER*10 CYRDOY
      CHARACTER*50 OUTCL, SITE

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
!      ELSEIF (DYNAMIC .EQ. SEASINIT) THEN
      IF (DYNAMIC .EQ. SEASINIT) THEN
!-----------------------------------------------------------------------
      RNMODE  = CONTROL % RNMODE
      REPNO   = CONTROL % REPNO
      RUN     = CONTROL % RUN

      IF (FMOPT == 'A' .OR. FMOPT == ' ') THEN   ! VSH
        CALL GETLUN('OUTCL',NOUTDT)
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
          WRITE(OUTCL,'(A2,A2,A,A,I3,A4)')STM,FM,
     &          'Daily',TRIM(SITE),TRTNUM,'.txt'
        ELSE
          OUTCL = 'SoilTemp_AMEI_No_ST_Model.txt'
          STM = 'DF'
        ENDIF
        
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
     &    'treatment_ID	date	leaf_number_as_haun_stg	' //
     &    'growth_stage_Zadoks	leaf_area_index	' //
     &    'PAR_interception_daily	tops_dry_weight	' //
     &    'grain_dry_weight	grain_unit_dry_weight	tops_N	' //
     &    'grain_N	grain_unit_N	root_depth	' //
     &    'soil_water_whole_profile	drainage_daily	' //
     &    'runoff_surface	N_inorganic_day	N_leached_day	' //
     &    'N_mineralization_day	N2O_emissions_day	' //
     &    'N_immobilization_day	N_denitrification_day	' // 
     &    'ground_heat_daily	latent_heat_daily	' //
     &    'sensible_heat_daily	net_radiation_daily	' //
     &    'soil_temp_surface_daily_avg	' //
     &    'soil_temp_surface_daily_max	' //
     &    'soil_temp_surface_daily_min	canopy_temp_daily_avg	' //
     &    'canopy_temp_daily_max	canopy_temp_daily_min	' //
     &    'potential_evapotrans	evapotranspiration_daily	' //
     &    'portential_soil_evaporation_daily	' //
     &    'soil_evaporation_daily	potential_transpiration_daily	' //
     &    'transpiration_daily'
          WRITE(NOUTDT,'(A)') 'text	text	text	(YYYY-MM-DD)	' //
     &    'leaf\mainstem	number	m²/m²	%	kg[DM]/ha	kg[DM]/ha	' //
     &    'mg[DM]/grain	kg[N]/ha	kg[N]/ha	mg[N]/grain	m	' //
     &    'cm3/cm3	mm/d	mm/d	kg[N]/ha/d	kg[N]/ha/d	' //
     &    'kg[N]/ha/d	kg[N]/ha/d	kg[N]/ha/d	kg[N]/ha/d	' //
     &    'w/m2	w/m2	w/m2	w/m2	°C	°C	°C	°C	°C	°C	' //
     &    'mm/d	mm/d	mm/d	mm/d	mm/d	mm/d'
          WRITE(NOUTDT,'(A)') 'FRAMEWORK_ID	MODEL_ID	' //
     &    'TREAT_ID	DATE	LNUM	GSTZD	LAID	LIPCD	CWAD	' //
     &    'GWAD	GWGD	CNAD	GNAD	GNGD	RDPD	SWWPD	DRND	' //
     &    'ROFD	NIAD	NLCD	NMND	N2OED	NIMD	NDND	GHFD	' //
     &    'LHFD	HHFD	RND	TSSAV	TSSMX	TSSMN	TGAV	TGMX	' //
     &    'TGMN	EOAD	ETAD	EPSAD	ESAD	EPPAD	EPAD'

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
          CALL YRDOYTOYYYYMMDD(YRDOY,CYRDOY)

          WRITE(NOUTDT,300) FM, TAB, STM, TAB, TRTNUM, 
     &        TAB, CYRDOY,
     &        TAB, DAILYOUTAMEI % LNUM,
     &        TAB, DAILYOUTAMEI % GSTZD,
     &        TAB, DAILYOUTAMEI % LAID,
     &        TAB, 'na',
     &        TAB, DAILYOUTAMEI % CWAD, 
     &        TAB, DAILYOUTAMEI % GWAD,
     &        TAB, DAILYOUTAMEI % GWGD,
     &        TAB, DAILYOUTAMEI % CNAD,
     &        TAB, DAILYOUTAMEI % GNAD,
     &        TAB, DAILYOUTAMEI % GNGD,
     &        TAB, DAILYOUTAMEI % RDPD,
     &        TAB, 'na',
     &        TAB, DAILYOUTAMEI % DRND,
     &        TAB, DAILYOUTAMEI % ROFD,
     &        TAB, DAILYOUTAMEI % NIAD,
     &        TAB, DAILYOUTAMEI % NLCD,
     &        TAB, DAILYOUTAMEI % NMND,
     &        TAB, DAILYOUTAMEI % N2OED,
     &        TAB, DAILYOUTAMEI % NIMD,
     &        TAB, DAILYOUTAMEI % NDND,
     &        TAB, 'na',
     &        TAB, 'na',
     &        TAB, 'na',
     &        TAB, 'na',
     &        TAB, DAILYOUTAMEI % TSSAV,
     &        TAB, 'na',
     &        TAB, 'na',
     &        TAB, DAILYOUTAMEI % TGAV,
     &        TAB, 'na',
     &        TAB, 'na',
     &        TAB, DAILYOUTAMEI % EOAD,
     &        TAB, DAILYOUTAMEI % ETAD,
     &        TAB, DAILYOUTAMEI % EPSAD,
     &        TAB, DAILYOUTAMEI % ESAD,
     &        TAB, DAILYOUTAMEI % EPPAD,
     &        TAB, DAILYOUTAMEI % EPAD
     
  300 FORMAT(A,A1,A,A1,I3,
     &       A1,A10,
     &       3(A1,F15.3),
     &       A1,A2,
     &       7(A1,F15.3),
     &       A1,A2,
     &       8(A1,F15.3),
     &       4(A1,A2),
     &       A1,F15.3,
     &       2(A1,A2),
     &       A1,F15.3,
     &       2(A1,A2),
     &       6(A1,F15.3))

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
      