C=======================================================================
C  INCOMP  Subroutine
C     This subroutine initializes parameters for composition of tissues
C     which vary with genotype at the beginning of each run.
C----------------------------------------------------------------------
C  REVISION HISTORY
C  03/31/1991 JWW Separated old INPHEN into INPHEN, INVEG, INCOMP
C  04/01/1991 GH  Adapted for CROPGRO
C  09/18/1998 CHP Moved to PLANT module and added input statements
C  05/10/1999 GH  Incorporated in CROPGRO
C  08/12/2003 CHP Added I/O error checking
!  11/26/2007 CHP THRESH, SDPRO, SDLIP moved from eco to cul file
C-----------------------------------------------------------------------
C  Called : PLANT
C  Calls  : ERROR, FIND, IGNORE
C=======================================================================

      SUBROUTINE INCOMP(DYNAMIC,
     &    FILECC, FILEIO, FRLF, FRRT, FRSTM, CO2,         !Input
     &    AGRLF, AGRNOD, AGRRT, AGRSD1, AGRSD2, AGRSH1,   !Output
     &    AGRSH2, AGRSTM, AGRVG, AGRVG2, SDPROR)          !Output

C-----------------------------------------------------------------------
      USE ModuleDefs     !Definitions of constructed variable types, 
                         ! which contain control information, soil
                         ! parameters, hourly weather data.
      IMPLICIT NONE
      EXTERNAL GETLUN, FIND, ERROR, IGNORE, TABEX
      SAVE

      CHARACTER*6 ERRKEY
      PARAMETER (ERRKEY = 'INCOMP')
      CHARACTER*6 SECTION
      CHARACTER*30 FILEIO
      CHARACTER*80 C80
      CHARACTER*92 FILECC

      INTEGER LUNCRP, LUNIO
      INTEGER DYNAMIC, ERR, FOUND, ISECT, LINC, LNUM, I

      REAL,DIMENSION(7) :: CCO2,CPROLFI,CPCARLF,CPROSTI,CPCARST
      
      REAL  AGRLF , AGRNOD, AGRRT , AGRSD1, AGRSD2, AGRSH1,
     &      AGRSH2, AGRSTM, AGRVG , AGRVG2,
     &      FRLF  , FRRT  , FRSTM,
     &      PCARLF, PCARNO, PCARRT, PCARSD, PCARSH, PCARST,
     &      PLIGLF, PLIGNO, PLIGRT, PLIGSD, PLIGSH, PLIGST,
     &      PLIPLF, PLIPNO, PLIPRT,                 PLIPSH, PLIPST,
     &      PMINLF, PMINNO, PMINRT, PMINSD, PMINSH, PMINST,
     &      POALF , POANO , POART , POASD , POASH , POAST ,
     &              PROLFI, PRORTI,                 PROSHI, PROSTI,
     &      RCH2O , RLIG  , RLIP  , RMIN  , RNO3C , ROA   ,
     &      SDLIP,  SDPRO , SDPROR, SDPROS, CO2, TABEX

!***********************************************************************
!***********************************************************************
!     Run Initialization - Called once per simulation
!***********************************************************************
      IF (DYNAMIC .EQ. RUNINIT) THEN
!-----------------------------------------------------------------------
!     Read INP file
!-----------------------------------------------------------------------
      CALL GETLUN('FILEIO', LUNIO)
      OPEN (LUNIO, FILE = FILEIO,STATUS = 'OLD',IOSTAT=ERR)
      IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEIO,0)
      LNUM = 0
!-----------------------------------------------------------------------
!    Find and read 2ND Cultivar Section
!-----------------------------------------------------------------------
      SECTION = '*CULTI'
      CALL FIND(LUNIO, SECTION, LINC, FOUND) ; LNUM = LNUM + LINC

      SECTION = '*CULTI'
      CALL FIND(LUNIO, SECTION, LINC, FOUND) ; LNUM = LNUM + LINC
      IF (FOUND == 0) THEN
        CALL ERROR(SECTION, 42, FILEIO, LNUM)
      ELSE
        READ(LUNIO,'(126X,2F6.0)',IOSTAT=ERR) SDPRO, SDLIP
        LNUM = LNUM + 1
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEIO,LNUM)
      ENDIF

      CLOSE (LUNIO)

!-----------------------------------------------------------------------
!     Read in values from species file
!-----------------------------------------------------------------------
      CALL GETLUN('FILEC', LUNCRP)
      OPEN (LUNCRP,FILE = FILECC, STATUS = 'OLD',IOSTAT=ERR)
      IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,0)
      LNUM = 0
!-----------------------------------------------------------------------
!    Find and Read Respiration Section
!-----------------------------------------------------------------------
!     Subroutine FIND finds appropriate SECTION in a file by
!     searching for the specified 6-character string at beginning
!     of each line.
!-----------------------------------------------------------------------
      SECTION = '!*RESP'
      CALL FIND(LUNCRP, SECTION, LINC, FOUND) ; LNUM = LNUM + LINC
      IF (FOUND .EQ. 0) THEN
        CALL ERROR(SECTION, 42, FILECC, LNUM)
      ELSE
        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        READ(C80,'(F6.0)',IOSTAT=ERR) RNO3C
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        READ(C80,'(5F6.0)',IOSTAT=ERR) RCH2O, RLIP, RLIG, ROA, RMIN
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)
      ENDIF

      SECTION = '!*PLAN'
      CALL FIND(LUNCRP, SECTION, LINC, FOUND) ; LNUM = LNUM + LINC
      IF (FOUND .EQ. 0) THEN
        CALL ERROR(SECTION, 42, FILECC, LNUM)
      ELSE
        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
!        READ(C80,'(F6.0,6X,2F6.0)',IOSTAT=ERR) PROLFI, PROLFF, PROSTI
        READ(C80,'(F6.0,12X,F6.0)',IOSTAT=ERR) PROLFI, PROSTI
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        READ(C80,'(F6.0,12X,F6.0)',IOSTAT=ERR) PRORTI, PROSHI
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        READ(C80,'(F6.0)',IOSTAT=ERR) SDPROS
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        READ(C80,'(6F6.0)',IOSTAT=ERR)
     &          PCARLF, PCARST, PCARRT, PCARSH, PCARSD, PCARNO
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        READ(C80,'(5F6.0)',IOSTAT=ERR)
     &          PLIPLF, PLIPST, PLIPRT, PLIPSH, PLIPNO
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        READ(C80,'(6F6.0)',IOSTAT=ERR)
     &          PLIGLF, PLIGST, PLIGRT, PLIGSH, PLIGSD, PLIGNO
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        READ(C80,'(6F6.0)',IOSTAT=ERR)
     &          POALF, POAST, POART, POASH, POASD, POANO
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        READ(C80,'(6F6.0)',IOSTAT=ERR)
     &          PMINLF, PMINST, PMINRT, PMINSH, PMINSD, PMINNO
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)
      ENDIF
!-----------------------------------------------------------------------
! CARBON DIOXIDE EFFECT ON COMPOSITION
!-----------------------------------------------------------------------
      SECTION = '!*COMP'
      CALL FIND(LUNCRP, SECTION, LINC, FOUND) ; LNUM = LNUM + LINC
      IF (FOUND .EQ. 0) THEN
        CALL ERROR(SECTION, 42, FILECC, LNUM)
      ELSE        
        CCO2    = 0.0 
        CPROLFI = 0.0
        CPCARLF = 0.0
        CPROSTI = 0.0
        CPCARST = 0.0            
        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        READ(C80,'(7F6.0)',IOSTAT=ERR) (CCO2(I),I=1,7)
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)
        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        READ(C80,'(7F6.0)',IOSTAT=ERR) (CPROLFI(I),I=1,7)
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)
        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        READ(C80,'(7F6.0)',IOSTAT=ERR) (CPCARLF(I),I=1,7)
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)
        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        READ(C80,'(7F6.0)',IOSTAT=ERR) (CPROSTI(I),I=1,7)
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)
        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        READ(C80,'(7F6.0)',IOSTAT=ERR) (CPCARST(I),I=1,7)
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)
      ENDIF 

      CLOSE (LUNCRP)

!C-----------------------------------------------------------------------
!C    Read Ecotype Parameter File
!C-----------------------------------------------------------------------
!      CALL GETLUN('FILEE', LUNECO)
!      OPEN (LUNECO,FILE = FILEGC,STATUS = 'OLD',IOSTAT=ERR)
!      IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEGC,0)
!      ECOTYP = '      '
!      LNUM = 0
!      DO WHILE (ECOTYP .NE. ECONO)
!        CALL IGNORE(LUNECO, LNUM, ISECT, C255)
!        IF (ISECT .EQ. 1 .AND. C255(1:1) .NE. ' ' .AND.
!     &          C255(1:1) .NE. '*') THEN
!          READ (C255,'(A6,108X,2F6.0)',IOSTAT=ERR)
!     &        ECOTYP, SDPRO, SDLIP
!          IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEGC,LNUM)
!          IF (ECOTYP .EQ. ECONO) THEN
!            EXIT
!          ENDIF
!
!        ELSE IF (ISECT .EQ. 0) THEN
!          IF (ECONO .EQ. 'DFAULT') CALL ERROR(ERRKEY,35,FILEGC,LNUM)
!          ECONO = 'DFAULT'
!          REWIND(LUNECO)
!          LNUM = 0
!        ENDIF
!      ENDDO
!
!      CLOSE (LUNECO)
!
!***********************************************************************
!***********************************************************************
!     Seasonal initialization - run once per season
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. SEASINIT) THEN
!-----------------------------------------------------------------------
! CARBON DIOXIDE EFFECT ON COMPOSITION
!-----------------------------------------------------------------------
        PROLFI = TABEX(CPROLFI,CCO2,CO2,7)  
        PCARLF = TABEX(CPCARLF,CCO2,CO2,7)  
        PROSTI = TABEX(CPROSTI,CCO2,CO2,7)  
        PCARST = TABEX(CPCARST,CCO2,CO2,7) 
C-----------------------------------------------------------------------
C     COMPUTE RESPIRATION COEFFICIENTS BASED ON PLANT COMPOSITION
C-----------------------------------------------------------------------
C
      AGRLF  = PLIPLF*RLIP + PLIGLF*RLIG + POALF*ROA
     &         + PMINLF*RMIN + PCARLF*RCH2O
      AGRSTM = PLIPST*RLIP + PLIGST*RLIG + POAST*ROA
     &         + PMINST*RMIN + PCARST*RCH2O
      AGRRT  =  PLIPRT*RLIP + PLIGRT*RLIG + POART*ROA
     &         + PMINRT*RMIN + PCARRT*RCH2O
      AGRNOD =  PLIPNO*RLIP + PLIGNO*RLIG + POANO*ROA
     &         + PMINNO*RMIN + PCARNO*RCH2O

!-----------------------------------------------------------------------
!     AGRVG2, AGRSH2, AGRSD2 include protein component of vegetative 
!     growth cost
!-----------------------------------------------------------------------
      AGRVG  = AGRLF * FRLF + AGRRT * FRRT + AGRSTM * FRSTM
      AGRVG2 = AGRVG + (FRLF*PROLFI+FRRT*PRORTI+FRSTM*PROSTI)*RNO3C

!-----------------------------------------------------------------------
      AGRSH1 =  PLIPSH*RLIP + PLIGSH*RLIG + POASH*ROA
     &         + PMINSH*RMIN + PCARSH*RCH2O
      AGRSH2 =  AGRSH1 + PROSHI*RNO3C 

!-----------------------------------------------------------------------
      SDPROR = (SDPRO - SDPROS) / ( SDLIP + PCARSD )
      AGRSD1 = PMINSD*RMIN + PLIGSD*RLIG + POASD*ROA
     &         + (SDLIP*RLIP + PCARSD*RCH2O)*(1. - SDPROR)
      AGRSD2 = AGRSD1 + SDPRO*RNO3C 

!***********************************************************************
!***********************************************************************
!     END OF DYNAMIC IF CONSTRUCT
!***********************************************************************
      ENDIF
!-----------------------------------------------------------------------
      RETURN
      END ! SUBROUTINE INCOMP
!=======================================================================

!-----------------------------------------------------------------------
!       Variable definitions
!-----------------------------------------------------------------------
! AGRLF   Mass of CH2O required for new leaf growth 
! AGRNOD  CH2O requirement for nodule growth 
! AGRRT   Mass of CH2O required for new root growth 
! AGRSD1  CH2O requirement for seed growth, excluding cost for protein 
!           content 
! AGRSD2  CH2O requirement for seed growth, including cost for protein 
!           content 
! AGRSH1  CH2O required for shell growth, excluding cost for protein 
!           content 
! AGRSH2  CH2O requirement for shell growth, including cost for protein 
!           content 
! AGRSTM  Mass of CH2O required for new stem growth 
! AGRVG   Mass of CH2O required for vegetative tissue growth including 
!           stoichiometry and respiration 
! AGRVG2  Total mass of CH2O required for vegetative tissue growth 
! ECONO   Ecotype code - used to match ECOTYP in .ECO file
!           ","IPDMND, IPGROW, IPIBS, IPPHENOL, PODS, IPPLNT
! ECOTYP  Ecotype code for this simulation "
! ERR     Error code for file operation 
! FILECC  Path plus filename for species file (*.spe) 
! FILEGC  Pathname plus filename for ECO file "
! FOUND   Indicator that good data was read from file by subroutine FIND (0 
!           - End-of-file encountered, 1 - NAME was found) 
! FRLF    Fraction of vegetative tissue growth that goes to leaves on a day
!           
! FRRT    Fraction of vegetative tissue growth that goes to roots on a day 
! FRSTM   Fraction of vegetative tissue growth that goes to stems on a day 
! ISECT   Data record code (0 - End of file encountered, 1 - Found a good 
!           line to read, 2 - End of Section in file encountered, denoted 
!           by * in column 1
! LNUM    Current line number of input file 
! LUNCRP  Logical unit number for FILEC (*.spe file) 
! LUNECO  Logical unit number for FILEE (*.eco file) "
! PCARLF  Proportion of leaf tissue that is carbohydrate
!           (fraction)","IPGROW, INCOMP
! PCARNO  Proportion of nodule tissue that is carbohydrate
!           (fraction)","IPGROW, INCOMP
! PCARRT  Proportion of root tissue that is carbohydrate
!           (fraction)","IPGROW, INCOMP
! PCARSD  Proportion of seed tissue that is carbohydrate
!           (g[CH2O] / g[seed])","IPGROW, INCOMP, IPPLNT
! PCARSH  Proportion of shell tissue that is carbohydrate
!           (fraction)","IPGROW, INCOMP, IPPLNT
! PCARST  Proportion of stem tissue that is carbohydrate
!           (fraction)","IPGROW, INCOMP
! PLIGLF  Proportion of leaf tissue that is lignin
!           (fraction)","IPGROW, INCOMP
! PLIGNO  Proportion of nodule tissue that is lignin
!           (fraction)","IPGROW, INCOMP
! PLIGRT  Proportion of root tissue that is lignin
!           (fraction)","IPGROW, INCOMP
! PLIGSD  Proportion of seed tissue that is lignin
!           (fraction)","IPPLNT, IPDMND, PODCOMP, IPGROW, INC
! PLIGSH  Proportion of shell tissue that is lignin
!           (fraction)","IPPLNT, IPGROW, INCOMP
! PLIGST  Proportion of stem tissue that is lignin
!           (fraction)","IPGROW, INCOMP
! PLIPLF  Proportion of leaf tissue that is lipid
!           (fraction)","IPGROW, INCOMP
! PLIPNO  Proportion of nodule tissue that is lipid
!           (fraction)","IPGROW, INCOMP
! PLIPRT  Proportion of root tissue that is lipid
!           (fraction)","IPGROW, INCOMP
! PLIPSH  Proportion of shell tissue that is lipid
!           (fraction)","IPPLNT, IPGROW, INCOMP
! PLIPST  Proportion of stem tissue that is lipid
!           (fraction)","IPGROW, INCOMP
! PMINLF  Proportion of leaf tissue that is mineral
!           (fraction)","IPGROW, INCOMP
! PMINNO  Proportion of nodule tissue that is mineral
!           (fraction)","IPGROW, INCOMP
! PMINRT  Proportion of root tissue that is mineral
!           (fraction)","IPGROW, INCOMP
! PMINSD  Proportion of seed tissue that is mineral
!           (fraction)","IPPLNT, IPDMND, PODCOMP, IPGROW, INC
! PMINSH  Proportion of shell tissue that is mineral
!           (fraction)","IPPLNT, IPGROW, INCOMP
! PMINST  Proportion of stem tissue that is mineral
!           (fraction)","IPGROW, INCOMP
! POALF   Proportion of leaf tissue that is organic acid
!           (fraction)","IPGROW, INCOMP
! POANO   Proportion of nodule tissue that is organic acid
!           (fraction)","IPGROW, INCOMP
! POART   Proportion of root tissue that is organic acid
!           (fraction)","IPGROW, INCOMP
! POASD   Proportion of seed tissue that is organic acid
!           (fraction)","IPPLNT, IPDMND, PODCOMP, IPGROW, INC
! POASH   Proportion of shell tissue that is organic acid
!           (fraction)","IPPLNT, IPGROW, INCOMP
! POAST   Proportion of stem tissue that is organic acid
!           (fraction)","IPGROW, INCOMP
! PROLFI  Maximum protein composition in leaves during growth with 
!           luxurious supply of N
!           (g[protein] / g[leaf tissue])","IPPLNT, IPDMND, I
! PRORTI  Maximum protein composition in roots during growth with luxurious 
!           supply of N (g[protein] / g[root])","IPPLNT, IPDMND, IPGROW, 
! PROSHI  Maximum protein composition in shells during growth with 
!           luxurious supply of N
!           ( g[protein] / g[shell tissue])","PODS, IPPLNT, I
! PROSTF  Minimum stem protein composition after N mining
!           (g[protein] / g[stem])","IPGROW, INCOMP, IPDMND
! PROSTI  Maximum protein composition in stems during growth with luxurious 
!           supply of N (g[protein] / g[stem])","IPPLNT, IPDMND, IPGROW, 
! RCH2O   Respiration required for synthesizing CH2O structure
!           (g[CH2O] / g[tissue])","IPDMND, PODCOMP, IPPLNT, 
! RLIG    Respiration required for synthesizing lignin structure
!           (g[CH2O] / g[lignin])","IPDMND, PODCOMP, IPPLNT, 
! RLIP    Respiration required for synthesizing lipid structure
!           (g[CH2O] / g[lipid])","IPDMND, PODCOMP, IPPLNT, I
! RMIN    Respiration required for synthesizing mineral structure
!           (g[CH2O] / g[mineral])","IPPLNT, IPDMND, PODCOMP
! RNO3C   Respiration required for reducing NO3 to protein
!           (g[CH2O] / g[protein])","IPDMND, IPPLNT, INCOMP
! ROA     Respiration required for synthesizing organic acids
!           (g[CH2O] / g[product])","IPDMND, PODCOMP, IPPLNT
! SDLIP   Maximum lipid composition in seed
!           (fraction)","IPDMND, IPGROW, INCOMP
! SDPRO   Seed protein fraction at 25oC
!           (g[protein] / g[seed])","IPDMND, IPGROW, INCOMP
! SDPROR  Ratio to adjust lipid and carbohydrate proportions when seed 
!           protein differs from protein composition of standard cultivar 
!           (SDPROS) 
! SDPROS  Seed protein fraction of standard cultivar at 25oC
!           (g[protein] / g[seed])","INCOMP
!-----------------------------------------------------------------------
!      END SUBROUTINE INCOMP
!=======================================================================
