C=======================================================================
      SUBROUTINE INTF_C2ML_DEPIC(INPITF, OUTITF)

      USE ModuleDefs
      USE ModuleData
      USE InterfaceDataMod
      IMPLICIT NONE
      SAVE

!     Control and data structures
      TYPE (INTFDataInput) INPITF
      TYPE (INTFDataOutput) OUTITF

!     Control variables
      CHARACTER ISWWAT
      INTEGER NDays
      INTEGER, DIMENSION(30) :: WetDay
      REAL CUMDPT, X2_PREV, TDL
      REAL, DIMENSION(NL) :: DSMID
      REAL, DIMENSION(5) :: TMA

!*********************************************************************** 
!     INTERFACE INDIVIDUAL ATTRIBUTIONS
!***********************************************************************
      ISWWAT = 'Y'
!*********************************************************************** 
!     SEASINIT
!***********************************************************************
      IF (INPITF % DYNAMIC .EQ. SEASINIT) THEN
            CALL init_stemp_epic(NL, ISWWAT, 
     &            INPITF % SLBDM, INPITF % THICK(1:INPITF % NLAYR), 
     &            INPITF % SLLB(1:INPITF % NLAYR), INPITF % SLDUL, 
     &            INPITF % SLLL, INPITF % NLAYR, INPITF % TAMP, 
     &            INPITF % RAIN, INPITF % SWLD, INPITF % T2M, 
     &            INPITF % TMAX, INPITF % TMIN, INPITF % TAV, 
     &            INPITF % IRVAL, 
     &            INPITF % CWAD, 
     &            INPITF % MLTHK, 
     &            INPITF % SNOW, 
     &            CUMDPT, 
     &            DSMID, 
     &            TDL,
     &            TMA, 
     &            NDays, 
     &            WetDay, 
     &            X2_PREV, 
     &            OUTITF % TSLD(0), OUTITF % TSLD(1:INPITF % NLAYR))
!*********************************************************************** 
!     RATE
!***********************************************************************
      ELSEIF (INPITF % DYNAMIC .EQ. RATE) THEN
            CALL model_stemp_epic(NL, ISWWAT, 
     &            INPITF % SLBDM, INPITF % THICK(1:INPITF % NLAYR), 
     &            INPITF % SLLB(1:INPITF % NLAYR), INPITF % SLDUL, 
     &            INPITF % SLLL, INPITF % NLAYR, INPITF % TAMP, 
     &            INPITF % RAIN, INPITF % SWLD, INPITF % T2M, 
     &            INPITF % TMAX, INPITF % TMIN, INPITF % TAV, 
     &            CUMDPT, 
     &            DSMID,
     &            TDL, 
     &            TMA, 
     &            NDays, 
     &            WetDay, 
     &            X2_PREV, 
     &            OUTITF % TSLD(0), OUTITF % TSLD(1:INPITF % NLAYR), 
     &            INPITF % IRVAL,
     &            INPITF % CWAD, 
     &            INPITF % MLTHK,
     &            INPITF % SNOW)

            OUTITF % TSLX = -99.0
            OUTITF % TSLN = -99.0

      ENDIF
C***********************************************************************      

      RETURN
      END SUBROUTINE
C=======================================================================