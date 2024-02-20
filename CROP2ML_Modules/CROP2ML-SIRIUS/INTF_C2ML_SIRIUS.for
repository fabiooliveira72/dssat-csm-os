C=======================================================================
      SUBROUTINE INTF_C2ML_SIRIUS(INPITF, OUTITF)

      USE ModuleDefs
      USE ModuleData
      USE InterfaceDataMod
      USE Soiltemperaturemod_SIRIUS
      IMPLICIT NONE
      SAVE

!     Control and data structures
      TYPE (INTFDataInput) INPITF
      TYPE (INTFDataOutput) OUTITF

!     Control variables for SIRIUS
      REAL deepLayerT, lambda_, a,b,c
      REAL maxTSoil, minTSoil
      REAL, DIMENSION(24) :: hourlySoilT

!*********************************************************************** 
!     INTERFACE INDIVIDUAL ATTRIBUTIONS
!***********************************************************************
      lambda_ = 2.454
      hourlySoilT = 0.0
      a = 0.0
      b = 0.0
      c = 0.0
      OUTITF % TSLX = -99.0
      OUTITF % TSLN = -99.0
!*********************************************************************** 
!     SEASINIT
!***********************************************************************
      IF (INPITF % DYNAMIC .EQ. SEASINIT) THEN

            CALL init_calculatesoiltemperature(
     &            INPITF % T2M, ! meanTAir 
     &            INPITF % TMIN, ! minTAir
     &            lambda_, 
     &            INPITF % TAV, ! meanAnnualAirTemp
     &            INPITF % TMAX, ! maxTAir
     &            deepLayerT)
!*********************************************************************** 
!     RATE
!***********************************************************************
      ELSEIF (INPITF % DYNAMIC .EQ. RATE) THEN

            CALL model_soiltemperature(
     &            INPITF % T2M,  ! meanTAir
     &            INPITF % TMIN, ! minTAir
     &            lambda_, 
     &            INPITF % TAV, ! meanAnnualAirTemp
     &            INPITF % G, ! heatFlux (g/m2/d)
     &            INPITF % TMAX, ! maxTAir
     &            b, 
     &            c, 
     &            a, 
     &            INPITF % DAYLD, ! dayLength
     &            minTSoil, 
     &            deepLayerT, 
     &            maxTSoil, 
     &            hourlySoilT)

            OUTITF % TSLD(0) = -99
            OUTITF % TSLD(1) = (maxTsoil + minTSoil)/2
            OUTITF % TSLD(2:INPITF % NLAYR) = deepLayerT
            OUTITF % TSLX(1) = maxTSoil
            OUTITF % TSLN(1) = minTSoil
      ENDIF
C***********************************************************************      

      RETURN
      END SUBROUTINE
C=======================================================================