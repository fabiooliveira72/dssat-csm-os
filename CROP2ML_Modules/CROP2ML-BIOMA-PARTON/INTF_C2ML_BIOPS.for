C=======================================================================
      SUBROUTINE INTF_C2ML_BIOPS(INPITF, OUTITF)

      USE ModuleDefs
      USE ModuleData
      USE InterfaceDataMod
      USE Soiltemperatureswatmod
      USE Surfacepartonsoilswatcmod
      IMPLICIT NONE
      SAVE

!     Control and data structures
      TYPE (INTFDataInput) INPITF
      TYPE (INTFDataOutput) OUTITF

      REAL LagCoefficient, SoilProfileDepth
      REAL SurfaceTemperatureMinimum
      REAL SurfaceTemperatureMaximum
      REAL,DIMENSION(0:INPITF % NLAYR):: THICKNESS
      INTEGER I

!*********************************************************************** 
!     INTERFACE INDIVIDUAL ATTRIBUTIONS
!***********************************************************************
      SoilProfileDepth = INPITF % SLDP / 100
      DO I = 0, INPITF % NLAYR
            THICKNESS(I) = INPITF % THICK(I)/100
      ENDDO
      LagCoefficient = 0.8
      OUTITF % TSLX = -99.0
      OUTITF % TSLN = -99.0
!*********************************************************************** 
!     SEASINIT
!***********************************************************************
      IF (INPITF % DYNAMIC .EQ. SEASINIT) THEN
      
            CALL init_soiltemperatureswat(
     &            INPITF % SWLD, ! VolumetricWaterContent
     &            THICKNESS(1:INPITF % NLAYR), ! LayerThickness
     &            LagCoefficient, 
     &            INPITF % TAV, ! AirTemperatureAnnualAverage
     &            INPITF % SLBDM, ! BulkDensity 
     &            SoilProfileDepth, 
     &            OUTITF % TSLD(1:INPITF % NLAYR))! SoilTemperatureByLayers
!*********************************************************************** 
!     RATE
!***********************************************************************
      ELSEIF (INPITF % DYNAMIC .EQ. RATE) THEN

            CALL model_surfacepartonsoilswatc(
     &            INPITF % DAYLD, ! DayLength
     &            INPITF % SRAD, ! GlobalSolarRadiation
     &            INPITF % CWAD, ! AboveGroundBiomass
     &            INPITF % TMIN, ! AirTemperatureMinimum
     &            INPITF % TMAX, ! AirTemperatureMaximum
     &            THICKNESS(1:INPITF % NLAYR), ! LayerThickness
     &            INPITF % SLBDM, ! BulkDensity
     &            SoilProfileDepth, 
     &            INPITF % TAV, ! AirTemperatureAnnualAverage
     &            INPITF % SWLD, ! VolumetricWaterContent, 
     &            LagCoefficient, 
     &            OUTITF % TSLN(0), ! SurfaceTemperatureMinimum
     &            OUTITF % TSLX(0), ! SurfaceTemperatureMaximum
     &            OUTITF % TSLD(0), ! SurfaceSoilTemperature
     &            OUTITF % TSLD(1:INPITF % NLAYR))! SoilTemperatureByLayers

      ENDIF
C***********************************************************************      

      RETURN
      END SUBROUTINE
C=======================================================================