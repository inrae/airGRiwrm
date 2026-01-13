# Seine_05: Calibration of an open-loop influenced flow semi-distributed model network

``` r
library(airGRiwrm)
#> Loading required package: airGR
#> 
#> Attaching package: 'airGRiwrm'
#> The following objects are masked from 'package:airGR':
#> 
#>     Calibration, CreateCalibOptions, CreateInputsCrit,
#>     CreateInputsModel, CreateRunOptions, RunModel
```

This vignette aims at showing an example of calibrating the SD model on
influenced flows while injecting observed uptakes and releases of the
lakes. It will use influenced observation flows directly measured at
gauging stations and flows recorded at reservoir inlets and outlets.

### Set the data

Loading naturalized data and influenced flows configuration:

``` r
load("_cache/V04.RData")
```

We remove extra items from a complete configuration to keep only the
Marne system:

``` r
selectedNodes <- c("MARNE_P23", "STDIZ_04", "LOUVE_19", "VITRY_25", "MARNE_P28", "MARNE_R25", "CHALO_21", "MONTR_18", "NOISI_17")
griwrm3 <- griwrm2[griwrm2$id %in% selectedNodes,]
griwrm3[griwrm3$id == "NOISI_17", c("down", "length")] = NA # Downstream station instead of PARIS_05
plot(griwrm3)
```

![](V05_Open-loop_influenced_flow_calibration_files/figure-html/griwrm3-1.png)

We can now generate the new `GRiwrmInputsModel` object:

``` r
library(seinebasin)
data(QOBS)
iEnd <- which(DatesR == as.POSIXct("2008-07-31", tz = "UTC"))

data(Qreservoirs)
QresMarne <- Qreservoirs[1:iEnd, grep("MARNE", colnames(Qreservoirs))]
id_GR_nodes <- griwrm3$id[!is.na(griwrm3$model)]
InputsModel3 <- CreateInputsModel(griwrm3,
                                  DatesR[1:iEnd],
                                  Precip[1:iEnd, id_GR_nodes],
                                  PotEvap[1:iEnd, id_GR_nodes],
                                  QresMarne)
#> CreateInputsModel.GRiwrm: Processing sub-basin STDIZ_04...
#> CreateInputsModel.GRiwrm: Processing sub-basin MONTR_18...
#> CreateInputsModel.GRiwrm: Processing sub-basin LOUVE_19...
#> CreateInputsModel.GRiwrm: Processing sub-basin VITRY_25...
#> CreateInputsModel.GRiwrm: Processing sub-basin CHALO_21...
#> CreateInputsModel.GRiwrm: Processing sub-basin NOISI_17...
```

### GriwmRunOptions object

We first define the run period:

``` r
IndPeriod_Run <- seq.int(
  which(DatesR == (DatesR[1] + 365 * 24 * 60 * 60)), # Set aside warm-up period
  iEnd # Until the end of the time series
)
```

We define the (optional but recommended) warm up period as a one-year
period before the run period:

``` r
IndPeriod_WarmUp <- seq.int(1, IndPeriod_Run[1] - 1)
```

``` r
RunOptions <- CreateRunOptions(
  InputsModel3,
  IndPeriod_WarmUp = IndPeriod_WarmUp,
  IndPeriod_Run = IndPeriod_Run
)
```

### InputsCrit object

We define the objective function for the calibration:

``` r
InputsCrit <- CreateInputsCrit(
  InputsModel = InputsModel3,
  FUN_CRIT = ErrorCrit_KGE2,
  RunOptions = RunOptions, Obs = Qobs[IndPeriod_Run,]
)
```

### GRiwrmCalibOptions object

``` r
CalibOptions <- CreateCalibOptions(InputsModel3)
str(CalibOptions)
#> List of 6
#>  $ STDIZ_04:List of 4
#>   ..$ FixedParam       : logi [1:5] NA NA NA NA NA
#>   ..$ SearchRanges     : num [1:2, 1:5] 1.00e-02 2.00e+01 4.59e-05 2.18e+04 -1.09e+04 ...
#>   ..$ FUN_TRANSFO      :function (ParamIn, Direction)  
#>   ..$ StartParamDistrib: num [1:3, 1:5] 1.25 2.5 5 169.02 247.15 ...
#>   ..- attr(*, "class")= chr [1:5] "CalibOptions" "daily" "GR" "SD" ...
#>  $ MONTR_18:List of 4
#>   ..$ FixedParam       : logi [1:4] NA NA NA NA
#>   ..$ SearchRanges     : num [1:2, 1:4] 4.59e-05 2.18e+04 -1.09e+04 1.09e+04 4.59e-05 ...
#>   ..$ FUN_TRANSFO      :function (ParamIn, Direction)  
#>   ..$ StartParamDistrib: num [1:3, 1:4] 169.017 247.151 432.681 -2.376 -0.649 ...
#>   ..- attr(*, "class")= chr [1:4] "CalibOptions" "daily" "GR" "HBAN"
#>  $ LOUVE_19:List of 4
#>   ..$ FixedParam       : logi [1:4] NA NA NA NA
#>   ..$ SearchRanges     : num [1:2, 1:4] 4.59e-05 2.18e+04 -1.09e+04 1.09e+04 4.59e-05 ...
#>   ..$ FUN_TRANSFO      :function (ParamIn, Direction)  
#>   ..$ StartParamDistrib: num [1:3, 1:4] 169.017 247.151 432.681 -2.376 -0.649 ...
#>   ..- attr(*, "class")= chr [1:4] "CalibOptions" "daily" "GR" "HBAN"
#>  $ VITRY_25:List of 4
#>   ..$ FixedParam       : logi [1:4] NA NA NA NA
#>   ..$ SearchRanges     : num [1:2, 1:4] 4.59e-05 2.18e+04 -1.09e+04 1.09e+04 4.59e-05 ...
#>   ..$ FUN_TRANSFO      :function (ParamIn, Direction)  
#>   ..$ StartParamDistrib: num [1:3, 1:4] 169.017 247.151 432.681 -2.376 -0.649 ...
#>   ..- attr(*, "class")= chr [1:4] "CalibOptions" "daily" "GR" "HBAN"
#>  $ CHALO_21:List of 4
#>   ..$ FixedParam       : logi [1:5] NA NA NA NA NA
#>   ..$ SearchRanges     : num [1:2, 1:5] 1.00e-02 2.00e+01 4.59e-05 2.18e+04 -1.09e+04 ...
#>   ..$ FUN_TRANSFO      :function (ParamIn, Direction)  
#>   ..$ StartParamDistrib: num [1:3, 1:5] 1.25 2.5 5 169.02 247.15 ...
#>   ..- attr(*, "class")= chr [1:5] "CalibOptions" "daily" "GR" "SD" ...
#>  $ NOISI_17:List of 4
#>   ..$ FixedParam       : logi [1:5] NA NA NA NA NA
#>   ..$ SearchRanges     : num [1:2, 1:5] 1.00e-02 2.00e+01 4.59e-05 2.18e+04 -1.09e+04 ...
#>   ..$ FUN_TRANSFO      :function (ParamIn, Direction)  
#>   ..$ StartParamDistrib: num [1:3, 1:5] 1.25 2.5 5 169.02 247.15 ...
#>   ..- attr(*, "class")= chr [1:5] "CalibOptions" "daily" "GR" "SD" ...
#>  - attr(*, "class")= chr [1:2] "GRiwrmCalibOptions" "list"
```

### Calibration

The optimization (i.e. calibration) of parameters can now be performed:

``` r
OutputsCalib <- Calibration(InputsModel3, RunOptions, InputsCrit, CalibOptions)
#> Calibration.GRiwrmInputsModel: Processing sub-basin 'STDIZ_04'...
#> Grid-Screening in progress (0% 20% 40% 60% 80% 100%)
#>   Screening completed (243 runs)
#>       Param =    5.000,  169.017,   -0.020,   83.096,    2.384
#>       Crit. KGE2[Q]      = 0.8618
#> Steepest-descent local search in progress
#>   Calibration completed (61 iterations, 813 runs)
#>       Param =   19.990,  165.875,   -0.244,   68.481,    3.756
#>       Crit. KGE2[Q]      = 0.9188
#> Calibration.GRiwrmInputsModel: Processing sub-basin 'MONTR_18'...
#> Grid-Screening in progress (0% 20% 40% 60% 80% 100%)
#>   Screening completed (81 runs)
#>       Param =  247.151,   -0.649,   42.098,    2.384
#>       Crit. KGE2[Q]      = 0.8117
#> Steepest-descent local search in progress
#>   Calibration completed (32 iterations, 331 runs)
#>       Param =  198.455,   -1.070,   77.183,    2.473
#>       Crit. KGE2[Q]      = 0.8311
#> Calibration.GRiwrmInputsModel: Processing sub-basin 'LOUVE_19'...
#> Grid-Screening in progress (0% 20% 40% 60% 80% 100%)
#>   Screening completed (81 runs)
#>       Param =  247.151,   -2.376,   83.096,    2.384
#>       Crit. KGE2[Q]      = 0.9123
#> Steepest-descent local search in progress
#>   Calibration completed (25 iterations, 270 runs)
#>       Param =  174.509,   -3.018,   96.535,    2.344
#>       Crit. KGE2[Q]      = 0.9306
#> Calibration.GRiwrmInputsModel: Processing sub-basin 'VITRY_25'...
#> Grid-Screening in progress (0% 20% 40% 60% 80% 100%)
#>   Screening completed (81 runs)
#>       Param =  432.681,   -0.649,   83.096,    2.384
#>       Crit. KGE2[Q]      = 0.8712
#> Steepest-descent local search in progress
#>   Calibration completed (64 iterations, 612 runs)
#>       Param =  299.290,   -1.228,   91.986,    5.101
#>       Crit. KGE2[Q]      = 0.9531
#> Calibration.GRiwrmInputsModel: Processing sub-basin 'CHALO_21'...
#> Parameter regularization: test a priori parameters from node STDIZ_04: 19.99, 165.875, -0.244, 68.481, 3.198
#> Crit. KGE2[Q] = 0.7892
#>  SubCrit. KGE2[Q] cor(sim, obs, "pearson") = 0.9100 
#>  SubCrit. KGE2[Q] cv(sim)/cv(obs)          = 1.0743 
#>  SubCrit. KGE2[Q] mean(sim)/mean(obs)      = 1.1756 
#> 
#> Parameter regularization: test a priori parameters from node LOUVE_19: 1, 174.509, -3.018, 96.535, 3.25
#> Crit. KGE2[Q] = 0.8629
#>  SubCrit. KGE2[Q] cor(sim, obs, "pearson") = 0.9408 
#>  SubCrit. KGE2[Q] cv(sim)/cv(obs)          = 1.0596 
#>  SubCrit. KGE2[Q] mean(sim)/mean(obs)      = 1.1084 
#> 
#> Parameter regularization: test a priori parameters from node VITRY_25: 1, 299.29, -1.228, 91.986, 4.485
#> Crit. KGE2[Q] = 0.8550
#>  SubCrit. KGE2[Q] cor(sim, obs, "pearson") = 0.9473 
#>  SubCrit. KGE2[Q] cv(sim)/cv(obs)          = 1.0380 
#>  SubCrit. KGE2[Q] mean(sim)/mean(obs)      = 1.1296 
#> 
#> Parameter regularization: set a priori parameters from node LOUVE_19: 1, 174.509, -3.018, 96.535, 3.25
#> Grid-Screening in progress (0% 20% 40% 60% 80% 100%)
#>   Screening completed (243 runs)
#>       Param =    1.250,  432.681,   -2.376,   20.697,    2.384
#>       Crit. Composite    = 0.9334
#> Steepest-descent local search in progress
#>   Calibration completed (28 iterations, 502 runs)
#>       Param =    0.600,  330.300,   -8.443,   69.408,    3.184
#>       Crit. Composite    = 0.9510
#>  Formula: sum(0.87 * KGE2[sqrt(Q)], 0.13 * GAPX[ParamT])
#> Calibration.GRiwrmInputsModel: Processing sub-basin 'NOISI_17'...
#> Parameter regularization: test a priori parameters from node MONTR_18: 1, 198.455, -1.07, 77.183, 3.825
#> Crit. KGE2[Q] = 0.6571
#>  SubCrit. KGE2[Q] cor(sim, obs, "pearson") = 0.9290 
#>  SubCrit. KGE2[Q] cv(sim)/cv(obs)          = 1.2613 
#>  SubCrit. KGE2[Q] mean(sim)/mean(obs)      = 1.2104 
#> 
#> Parameter regularization: test a priori parameters from node CHALO_21: 0.6, 330.3, -8.443, 69.408, 4.712
#> Crit. KGE2[Q] = 0.7661
#>  SubCrit. KGE2[Q] cor(sim, obs, "pearson") = 0.9410 
#>  SubCrit. KGE2[Q] cv(sim)/cv(obs)          = 1.2238 
#>  SubCrit. KGE2[Q] mean(sim)/mean(obs)      = 0.9658 
#> 
#> Parameter regularization: set a priori parameters from node CHALO_21: 0.6, 330.3, -8.443, 69.408, 4.712
#> Grid-Screening in progress (0% 20% 40% 60% 80% 100%)
#>   Screening completed (243 runs)
#>       Param =    1.250,  432.681,   -2.376,   83.096,    2.384
#>       Crit. Composite    = 0.8247
#> Steepest-descent local search in progress
#>   Calibration completed (29 iterations, 510 runs)
#>       Param =    0.770, 2951.297,   -2.617,   30.877,    4.004
#>       Crit. Composite    = 0.9401
#>  Formula: sum(0.88 * KGE2[sqrt(Q)], 0.12 * GAPX[ParamT])
```

### Run model with Michel calibration

Now that the model is calibrated, we can run it with the optimized
parameter values:

``` r
Param5 <- extractParam(OutputsCalib)

OutputsModels3 <- RunModel(
  InputsModel3,
  RunOptions = RunOptions,
  Param = Param5
)
#> RunModel.GRiwrmInputsModel: Processing sub-basin STDIZ_04...
#> Warning in RunModel_Lag(InputsModel, RunOptions, Param[1], OutputsModel): 141
#> time steps with negative flow, set to zero.
#> RunModel.GRiwrmInputsModel: Processing sub-basin MONTR_18...
#> RunModel.GRiwrmInputsModel: Processing sub-basin LOUVE_19...
#> RunModel.GRiwrmInputsModel: Processing sub-basin VITRY_25...
#> RunModel.GRiwrmInputsModel: Processing sub-basin CHALO_21...
#> RunModel.GRiwrmInputsModel: Processing sub-basin NOISI_17...
```

#### Comparison with simulated flows

We can compare these simulated flows with influenced discharge
measurements:

``` r
htmltools::tagList(lapply(
  griwrm3$id[!is.na(griwrm3$model)],
  function(x) {
    Q3 <- Qobs[RunOptions[[1]]$IndPeriod_Run, x]
    iQ3 <- which(!is.na(Q3))
    IndPeriod_Obs <- iQ3[1]:tail(iQ3, 1)
    OutputsModels <- ReduceOutputsModel(OutputsModels3[[x]], IndPeriod_Obs)
    plot(OutputsModels, Qobs = Q3[IndPeriod_Obs], main = x)
  }
))
#> Warning in plot.OutputsModel(OutputsModels, Qobs = Q3[IndPeriod_Obs], main =
#> x): zeroes detected in 'Qsim': some plots in the log space will not be created
#> using all time-steps
```

![](V05_Open-loop_influenced_flow_calibration_files/figure-html/plot-1.png)![](V05_Open-loop_influenced_flow_calibration_files/figure-html/plot-2.png)![](V05_Open-loop_influenced_flow_calibration_files/figure-html/plot-3.png)![](V05_Open-loop_influenced_flow_calibration_files/figure-html/plot-4.png)![](V05_Open-loop_influenced_flow_calibration_files/figure-html/plot-5.png)![](V05_Open-loop_influenced_flow_calibration_files/figure-html/plot-6.png)

## Save data for following vignettes

``` r
save(Param5, file = "_cache/V05.RData")
```
