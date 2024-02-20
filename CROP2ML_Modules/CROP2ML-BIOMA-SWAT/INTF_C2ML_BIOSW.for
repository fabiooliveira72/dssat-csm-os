C=======================================================================
      SUBROUTINE INTF_C2ML_BIOSW(INPITF, OUTITF)

      USE ModuleDefs
      USE ModuleData
      USE InterfaceDataMod
      USE Soiltemperatureswatmod_SW
      USE Surfaceswatsoilswatcmod_SW
      IMPLICIT NONE
      SAVE

!     Control and data structures
      TYPE (INTFDataInput) INPITF
      TYPE (INTFDataOutput) OUTITF

      REAL LagCoefficient, SoilProfileDepth
      REAL SurfaceTemperatureMinimum
      REAL SurfaceTemperatureMaximum
      REAL HeatCapacity
      REAL WaterEquivalentOfSnowPack
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

            CALL init_soiltemperatureswat_SW(
     &            INPITF % SWLD, ! VolumetricWaterContent
     &            THICKNESS(1:INPITF % NLAYR), ! LayerThickness
     &            LagCoefficient, 
     &            INPITF % TAV, ! AirTemperatureAnnualAverage
     &            INPITF % SLBDM, ! BulkDensity
     &            SoilProfileDepth, 
     &            OUTITF % TSLD(1:INPITF % NLAYR)) ! SoilTemperatureByLayers

!*********************************************************************** 
!     RATE
!***********************************************************************
      ELSEIF (INPITF % DYNAMIC .EQ. RATE) THEN

            CALL model_surfaceswatsoilswatc_SW(
     &            INPITF % TMAX, ! AirTemperatureMaximum
     &            INPITF % TMIN, ! AirTemperatureMinimum
     &            INPITF % SRAD, ! GlobalSolarRadiation
     &            INPITF % CWAD, ! AboveGroundBiomass
     &            INPITF % SNOW, ! WaterEquivalentOfSnowPack 
     &            INPITF % SALB, ! Albedo
     &            INPITF % SLBDM, ! BulkDensity
     &            INPITF % TAV, ! AirTemperatureAnnualAverage
     &            INPITF % SWLD, ! VolumetricWaterContent
     &            SoilProfileDepth, 
     &            LagCoefficient, 
     &            THICKNESS(1:INPITF % NLAYR), ! LayerThickness
     &            OUTITF % TSLD(0), ! SurfaceSoilTemperature
     &            OUTITF % TSLD(1:INPITF % NLAYR)) ! SoilTemperatureByLayers

      ENDIF
C***********************************************************************      

      RETURN
      END SUBROUTINE
C=======================================================================