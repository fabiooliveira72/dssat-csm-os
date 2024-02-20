MODULE Soiltemperatureswatmod
    IMPLICIT NONE
CONTAINS

    SUBROUTINE init_soiltemperatureswat(VolumetricWaterContent, &
        LayerThickness, &
        LagCoefficient, &
        AirTemperatureAnnualAverage, &
        BulkDensity, &
        SoilProfileDepth, &
        SoilTemperatureByLayers)
        IMPLICIT NONE
        INTEGER:: i_cyml_r
        REAL , DIMENSION(: ), INTENT(IN) :: VolumetricWaterContent
        REAL , DIMENSION(: ), INTENT(IN) :: LayerThickness
        REAL, INTENT(IN) :: LagCoefficient
        REAL, INTENT(IN) :: AirTemperatureAnnualAverage
        REAL , DIMENSION(: ), INTENT(IN) :: BulkDensity
        REAL, INTENT(IN) :: SoilProfileDepth
        REAL , DIMENSION(: ), INTENT(OUT) :: SoilTemperatureByLayers
        INTEGER:: i
        SoilTemperatureByLayers = 0.0
        DO i = 0 , SIZE(LayerThickness)-1, 1
            SoilTemperatureByLayers(i+1) = REAL(15)
        END DO
    END SUBROUTINE init_soiltemperatureswat

    SUBROUTINE model_soiltemperatureswat(VolumetricWaterContent, &
        SurfaceSoilTemperature, &
        LayerThickness, &
        LagCoefficient, &
        SoilTemperatureByLayers, &
        AirTemperatureAnnualAverage, &
        BulkDensity, &
        SoilProfileDepth)
        IMPLICIT NONE
        INTEGER:: i_cyml_r
        REAL , DIMENSION(: ), INTENT(IN) :: VolumetricWaterContent
        REAL, INTENT(IN) :: SurfaceSoilTemperature
        REAL , DIMENSION(: ), INTENT(IN) :: LayerThickness
        REAL, INTENT(IN) :: LagCoefficient
        REAL , DIMENSION(: ), INTENT(INOUT) :: SoilTemperatureByLayers
        REAL, INTENT(IN) :: AirTemperatureAnnualAverage
        REAL , DIMENSION(: ), INTENT(IN) :: BulkDensity
        REAL, INTENT(IN) :: SoilProfileDepth
        INTEGER:: i
        REAL:: SoilProfileDepthmm
        REAL:: TotalWaterContentmm
        REAL:: MaximumDumpingDepth
        REAL:: DumpingDepth
        REAL:: ScalingFactor
        REAL:: DepthBottom
        REAL:: RatioCenter
        REAL:: DepthFactor
        REAL:: DepthCenterLayer
        !- Name: SoilTemperatureSWAT -Version: 001, -Time step: 1
        !- Description:
    !            * Title: SoilTemperatureSWAT model
    !            * Authors: simone.bregaglio
    !            * Reference: ('http://bioma.jrc.ec.europa.eu/ontology/JRC_MARS_biophysical_domain.owl',)
    !            * Institution: University Of Milan
    !            * ExtendedDescription: Strategy for the calculation of soil temperature with SWAT method. Reference: Neitsch,S.L., Arnold, J.G., Kiniry, J.R., Williams, J.R., King, K.W. Soil and Water Assessment Tool. Theoretical documentation. Version 2000. http://swatmodel.tamu.edu/media/1290/swat2000theory.pdf
    !            * ShortDescription: None
        !- inputs:
    !            * name: VolumetricWaterContent
    !                          ** description : Volumetric soil water content
    !                          ** inputtype : variable
    !                          ** variablecategory : exogenous
    !                          ** datatype : DOUBLEARRAY
    !                          ** len : 
    !                          ** max : 0.8
    !                          ** min : 0
    !                          ** default : 0.25
    !                          ** unit : m3 m-3
    !            * name: SurfaceSoilTemperature
    !                          ** description : Average surface soil temperature
    !                          ** inputtype : variable
    !                          ** variablecategory : auxiliary
    !                          ** datatype : DOUBLE
    !                          ** max : 60
    !                          ** min : -60
    !                          ** default : 25
    !                          ** unit : degC
    !            * name: LayerThickness
    !                          ** description : Soil layer thickness
    !                          ** inputtype : parameter
    !                          ** parametercategory : constant
    !                          ** datatype : DOUBLEARRAY
    !                          ** len : 
    !                          ** max : 3
    !                          ** min : 0.005
    !                          ** default : 0.05
    !                          ** unit : m
    !            * name: LagCoefficient
    !                          ** description : Lag coefficient that controls the influence of the previous day's temperature on the current day's temperature
    !                          ** inputtype : parameter
    !                          ** parametercategory : constant
    !                          ** datatype : DOUBLE
    !                          ** max : 1
    !                          ** min : 0
    !                          ** default : 0.8
    !                          ** unit : dimensionless
    !            * name: SoilTemperatureByLayers
    !                          ** description : Soil temperature of each layer
    !                          ** inputtype : variable
    !                          ** variablecategory : state
    !                          ** datatype : DOUBLEARRAY
    !                          ** len : 
    !                          ** max : 60
    !                          ** min : -60
    !                          ** default : 15
    !                          ** unit : degC
    !            * name: AirTemperatureAnnualAverage
    !                          ** description : Annual average air temperature
    !                          ** inputtype : parameter
    !                          ** parametercategory : constant
    !                          ** datatype : DOUBLE
    !                          ** max : 50
    !                          ** min : -40
    !                          ** default : 15
    !                          ** unit : degC
    !            * name: BulkDensity
    !                          ** description : Bulk density
    !                          ** inputtype : parameter
    !                          ** parametercategory : constant
    !                          ** datatype : DOUBLEARRAY
    !                          ** len : 
    !                          ** max : 1.8
    !                          ** min : 0.9
    !                          ** default : 1.3
    !                          ** unit : t m-3
    !            * name: SoilProfileDepth
    !                          ** description : Soil profile depth
    !                          ** inputtype : parameter
    !                          ** parametercategory : constant
    !                          ** datatype : DOUBLE
    !                          ** max : 50
    !                          ** min : 0
    !                          ** default : 3
    !                          ** unit : m
        !- outputs:
    !            * name: SoilTemperatureByLayers
    !                          ** description : Soil temperature of each layer
    !                          ** datatype : DOUBLEARRAY
    !                          ** variablecategory : state
    !                          ** len : 
    !                          ** max : 60
    !                          ** min : -60
    !                          ** unit : degC
        SoilProfileDepthmm = SoilProfileDepth * 1000
        TotalWaterContentmm = REAL(0)
        DO i = 0 , SIZE(LayerThickness)-1, 1
            TotalWaterContentmm = TotalWaterContentmm +  &
                    (VolumetricWaterContent(i+1) * LayerThickness(i+1))
        END DO
        TotalWaterContentmm = TotalWaterContentmm * 1000
        MaximumDumpingDepth = REAL(0)
        DumpingDepth = REAL(0)
        ScalingFactor = REAL(0)
        DepthBottom = REAL(0)
        RatioCenter = REAL(0)
        DepthFactor = REAL(0)
        DepthCenterLayer = LayerThickness(1) * 1000 / 2
        MaximumDumpingDepth = 1000 + (2500 * BulkDensity(1) /  &
                (BulkDensity(1) + (686 * EXP((-5.63) * BulkDensity(1)))))
        ScalingFactor = TotalWaterContentmm / ((0.356 - (0.144 *  &
                BulkDensity(1))) * SoilProfileDepthmm)
        DumpingDepth = MaximumDumpingDepth * EXP(LOG(500 /  &
                MaximumDumpingDepth) *  (((1 - ScalingFactor) / (1 +  &
                ScalingFactor)) ** 2))
        RatioCenter = DepthCenterLayer / DumpingDepth
        DepthFactor = RatioCenter / (RatioCenter + EXP(-0.867 - (2.078 *  &
                RatioCenter)))
        SoilTemperatureByLayers(1) = LagCoefficient *  &
                SoilTemperatureByLayers(1) + ((1 - LagCoefficient) * (DepthFactor *  &
                (AirTemperatureAnnualAverage - SurfaceSoilTemperature) +  &
                SurfaceSoilTemperature))
        DO i = 1 , SIZE(LayerThickness)-1, 1
            DepthBottom = DepthBottom + (LayerThickness((i - 1)+1) * 1000)
            DepthCenterLayer = DepthBottom + (LayerThickness(i+1) * 1000 / 2)
            MaximumDumpingDepth = 1000 + (2500 * BulkDensity(i+1) /  &
                    (BulkDensity(i+1) + (686 * EXP((-5.63) * BulkDensity(i+1)))))
            ScalingFactor = TotalWaterContentmm / ((0.356 - (0.144 *  &
                    BulkDensity(i+1))) * SoilProfileDepthmm)
            DumpingDepth = MaximumDumpingDepth * EXP(LOG(500 /  &
                    MaximumDumpingDepth) *  (((1 - ScalingFactor) / (1 +  &
                    ScalingFactor)) ** 2))
            RatioCenter = DepthCenterLayer / DumpingDepth
            DepthFactor = RatioCenter / (RatioCenter + EXP(-0.867 - (2.078 *  &
                    RatioCenter)))
            SoilTemperatureByLayers(i+1) = LagCoefficient *  &
                    SoilTemperatureByLayers(i+1) + ((1 - LagCoefficient) * (DepthFactor  &
                    * (AirTemperatureAnnualAverage - SurfaceSoilTemperature) +  &
                    SurfaceSoilTemperature))
        END DO
    END SUBROUTINE model_soiltemperatureswat

END MODULE
