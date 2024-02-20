C=======================================================================
      SUBROUTINE INTF_C2ML_STICS(INPITF, OUTITF)

      USE ModuleDefs
      USE ModuleData
      USE InterfaceDataMod
      USE Soil_tempmod
      IMPLICIT NONE
      SAVE

!     Control and data structures
      TYPE (INTFDataInput) INPITF
      TYPE (INTFDataOutput) OUTITF

!     Control model variables
      REAL air_temp_day1, max_canopy_temp, min_canopy_temp
      REAL prev_canopy_temp, temp_amp
      REAL , ALLOCATABLE, DIMENSION(: ):: prev_temp_profile
      REAL , ALLOCATABLE, DIMENSION(: ):: temp_profile
      REAL , ALLOCATABLE, DIMENSION(: ):: layer_temp
      INTEGER, DIMENSION(0:NL):: LTHICK
!*********************************************************************** 
!     INTERFACE INDIVIDUAL ATTRIBUTIONS
!***********************************************************************

!*********************************************************************** 
!     SEASINIT
!***********************************************************************
      IF (INPITF % DYNAMIC .EQ. SEASINIT) THEN
            LTHICK = INT(INPITF % THICK)

            CALL init_temp_profile(
     &            INPITF % TMIN, !min_air_temp
     &            INPITF % T2M, !air_temp_day1
     &            LTHICK(1:INPITF % NLAYR), !layer_thick 
     &            temp_amp, 
     &            prev_temp_profile, 
     &            prev_canopy_temp)
!*********************************************************************** 
!     RATE
!***********************************************************************
      ELSEIF (INPITF % DYNAMIC .EQ. RATE) THEN

            CALL model_soil_temp(
     &            INPITF % TMIN, !min_temp
     &            INPITF % TMAX, !max_temp
     &            prev_temp_profile,
     &            prev_canopy_temp,
     &            INPITF % TMIN, !min_air_temp
     &            INPITF % T2M, !air_temp_day1
     &            LTHICK(1:INPITF % NLAYR), !layer_thick
     &            INPITF % TMIN, !min_canopy_temp
     &            INPITF % TMAX,!max_canopy_temp
     &            temp_amp, !temp_amp 
     &            temp_profile, 
     &            layer_temp,
     &            OUTITF % TSLD(0)) !canopy_temp_avg

            prev_temp_profile = temp_profile
            prev_canopy_temp  = OUTITF % TSLD(0)

            OUTITF % TSLD(1:INPITF%NLAYR) = layer_temp(1:INPITF%NLAYR)
            OUTITF % TSLX = -99.0
            OUTITF % TSLN = -99.0            
      ENDIF
C***********************************************************************      

      RETURN
      END SUBROUTINE
C=======================================================================