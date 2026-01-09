# Seine_03: Calibration of a semi-distributed GR model network

The aim of this vignette is to perform a first calibration of the
semi-distributed hydrological model, using naturalized discharge.

## Loading libraries, network and time series data

Run `vignette("01_First_network", package = "airGRiwrm")` and
`vignette("02_First_run", package = "airGRiwrm")` before this one in
order to create the Rdata files loaded below:

``` r
library(airGRiwrm)
```

    ## Loading required package: airGR

    ## 
    ## Attaching package: 'airGRiwrm'

    ## The following objects are masked from 'package:airGR':
    ## 
    ##     Calibration, CreateCalibOptions, CreateInputsCrit,
    ##     CreateInputsModel, CreateRunOptions, RunModel

``` r
load("_cache/V01.RData")
load("_cache/V02.RData")
library(seinebasin)
data(QNAT)
```

## InputsCrit object

We need then to prepare the InputsCrit object that is necessary to
define the calibration objective function. We chose here the KGE’
criterion:

``` r
InputsCrit <- CreateInputsCrit(
  InputsModel = InputsModel,
  FUN_CRIT = ErrorCrit_KGE2,
  RunOptions = RunOptions,
  Obs = Qnat[IndPeriod_Run,]
)
str(InputsCrit)
```

    ## List of 25
    ##  $ TRANN_01:List of 8
    ##   ..$ FUN_CRIT:function (InputsCrit, OutputsModel, warnings = TRUE, verbose = TRUE)  
    ##   .. ..- attr(*, "class")= chr [1:2] "FUN_CRIT" "function"
    ##   ..$ Obs     : num [1:18628] 0.0943 0.0832 0.1443 0.1387 0.1498 ...
    ##   ..$ VarObs  : chr "Q"
    ##   ..$ BoolCrit: logi [1:18628] TRUE TRUE TRUE TRUE TRUE TRUE ...
    ##   ..$ idLayer : logi NA
    ##   ..$ transfo : chr ""
    ##   ..$ epsilon : NULL
    ##   ..$ Weights : NULL
    ##   ..- attr(*, "class")= chr [1:2] "Single" "InputsCrit"
    ##  $ STDIZ_04:List of 8
    ##   ..$ FUN_CRIT:function (InputsCrit, OutputsModel, warnings = TRUE, verbose = TRUE)  
    ##   .. ..- attr(*, "class")= chr [1:2] "FUN_CRIT" "function"
    ##   ..$ Obs     : num [1:18628] 0.11 0.155 0.409 0.184 0.258 ...
    ##   ..$ VarObs  : chr "Q"
    ##   ..$ BoolCrit: logi [1:18628] TRUE TRUE TRUE TRUE TRUE TRUE ...
    ##   ..$ idLayer : logi NA
    ##   ..$ transfo : chr ""
    ##   ..$ epsilon : NULL
    ##   ..$ Weights : NULL
    ##   ..- attr(*, "class")= chr [1:2] "Single" "InputsCrit"
    ##  $ BAR-S_06:List of 8
    ##   ..$ FUN_CRIT:function (InputsCrit, OutputsModel, warnings = TRUE, verbose = TRUE)  
    ##   .. ..- attr(*, "class")= chr [1:2] "FUN_CRIT" "function"
    ##   ..$ Obs     : num [1:18628] 0.118 0.111 0.111 0.118 0.111 ...
    ##   ..$ VarObs  : chr "Q"
    ##   ..$ BoolCrit: logi [1:18628] TRUE TRUE TRUE TRUE TRUE TRUE ...
    ##   ..$ idLayer : logi NA
    ##   ..$ transfo : chr ""
    ##   ..$ epsilon : NULL
    ##   ..$ Weights : NULL
    ##   ..- attr(*, "class")= chr [1:2] "Single" "InputsCrit"
    ##  $ CHAUM_07:List of 8
    ##   ..$ FUN_CRIT:function (InputsCrit, OutputsModel, warnings = TRUE, verbose = TRUE)  
    ##   .. ..- attr(*, "class")= chr [1:2] "FUN_CRIT" "function"
    ##   ..$ Obs     : num [1:18628] 0.599 0.239 0.239 0.12 0.2 ...
    ##   ..$ VarObs  : chr "Q"
    ##   ..$ BoolCrit: logi [1:18628] TRUE TRUE TRUE TRUE TRUE TRUE ...
    ##   ..$ idLayer : logi NA
    ##   ..$ transfo : chr ""
    ##   ..$ epsilon : NULL
    ##   ..$ Weights : NULL
    ##   ..- attr(*, "class")= chr [1:2] "Single" "InputsCrit"
    ##  $ CUSSY_08:List of 8
    ##   ..$ FUN_CRIT:function (InputsCrit, OutputsModel, warnings = TRUE, verbose = TRUE)  
    ##   .. ..- attr(*, "class")= chr [1:2] "FUN_CRIT" "function"
    ##   ..$ Obs     : num [1:18628] 0.0697 0.0348 0.0348 0.0348 0.0348 ...
    ##   ..$ VarObs  : chr "Q"
    ##   ..$ BoolCrit: logi [1:18628] TRUE TRUE TRUE TRUE TRUE TRUE ...
    ##   ..$ idLayer : logi NA
    ##   ..$ transfo : chr ""
    ##   ..$ epsilon : NULL
    ##   ..$ Weights : NULL
    ##   ..- attr(*, "class")= chr [1:2] "Single" "InputsCrit"
    ##  $ STGER_09:List of 8
    ##   ..$ FUN_CRIT:function (InputsCrit, OutputsModel, warnings = TRUE, verbose = TRUE)  
    ##   .. ..- attr(*, "class")= chr [1:2] "FUN_CRIT" "function"
    ##   ..$ Obs     : num [1:18628] 0.579 0.322 0.386 0.429 0.472 ...
    ##   ..$ VarObs  : chr "Q"
    ##   ..$ BoolCrit: logi [1:18628] TRUE TRUE TRUE TRUE TRUE TRUE ...
    ##   ..$ idLayer : logi NA
    ##   ..$ transfo : chr ""
    ##   ..$ epsilon : NULL
    ##   ..$ Weights : NULL
    ##   ..- attr(*, "class")= chr [1:2] "Single" "InputsCrit"
    ##  $ GUILL_10:List of 8
    ##   ..$ FUN_CRIT:function (InputsCrit, OutputsModel, warnings = TRUE, verbose = TRUE)  
    ##   .. ..- attr(*, "class")= chr [1:2] "FUN_CRIT" "function"
    ##   ..$ Obs     : num [1:18628] 0.0177 0.0177 0.0177 0.0177 0.0177 ...
    ##   ..$ VarObs  : chr "Q"
    ##   ..$ BoolCrit: logi [1:18628] TRUE TRUE TRUE TRUE TRUE TRUE ...
    ##   ..$ idLayer : logi NA
    ##   ..$ transfo : chr ""
    ##   ..$ epsilon : NULL
    ##   ..$ Weights : NULL
    ##   ..- attr(*, "class")= chr [1:2] "Single" "InputsCrit"
    ##  $ AISY-_11:List of 8
    ##   ..$ FUN_CRIT:function (InputsCrit, OutputsModel, warnings = TRUE, verbose = TRUE)  
    ##   .. ..- attr(*, "class")= chr [1:2] "FUN_CRIT" "function"
    ##   ..$ Obs     : num [1:18628] 0.064 0.064 0.064 0.064 0.064 ...
    ##   ..$ VarObs  : chr "Q"
    ##   ..$ BoolCrit: logi [1:18628] TRUE TRUE TRUE TRUE TRUE TRUE ...
    ##   ..$ idLayer : logi NA
    ##   ..$ transfo : chr ""
    ##   ..$ epsilon : NULL
    ##   ..$ Weights : NULL
    ##   ..- attr(*, "class")= chr [1:2] "Single" "InputsCrit"
    ##  $ EPISY_14:List of 8
    ##   ..$ FUN_CRIT:function (InputsCrit, OutputsModel, warnings = TRUE, verbose = TRUE)  
    ##   .. ..- attr(*, "class")= chr [1:2] "FUN_CRIT" "function"
    ##   ..$ Obs     : num [1:18628] 0.179 0.165 0.159 0.159 0.159 ...
    ##   ..$ VarObs  : chr "Q"
    ##   ..$ BoolCrit: logi [1:18628] TRUE TRUE TRUE TRUE TRUE TRUE ...
    ##   ..$ idLayer : logi NA
    ##   ..$ transfo : chr ""
    ##   ..$ epsilon : NULL
    ##   ..$ Weights : NULL
    ##   ..- attr(*, "class")= chr [1:2] "Single" "InputsCrit"
    ##  $ MONTR_18:List of 8
    ##   ..$ FUN_CRIT:function (InputsCrit, OutputsModel, warnings = TRUE, verbose = TRUE)  
    ##   .. ..- attr(*, "class")= chr [1:2] "FUN_CRIT" "function"
    ##   ..$ Obs     : num [1:18628] 0.263 0.263 0.263 0.263 0.263 ...
    ##   ..$ VarObs  : chr "Q"
    ##   ..$ BoolCrit: logi [1:18628] TRUE TRUE TRUE TRUE TRUE TRUE ...
    ##   ..$ idLayer : logi NA
    ##   ..$ transfo : chr ""
    ##   ..$ epsilon : NULL
    ##   ..$ Weights : NULL
    ##   ..- attr(*, "class")= chr [1:2] "Single" "InputsCrit"
    ##  $ LOUVE_19:List of 8
    ##   ..$ FUN_CRIT:function (InputsCrit, OutputsModel, warnings = TRUE, verbose = TRUE)  
    ##   .. ..- attr(*, "class")= chr [1:2] "FUN_CRIT" "function"
    ##   ..$ Obs     : num [1:18628] 0.15 0.187 0.206 0.337 0.281 ...
    ##   ..$ VarObs  : chr "Q"
    ##   ..$ BoolCrit: logi [1:18628] TRUE TRUE TRUE TRUE TRUE TRUE ...
    ##   ..$ idLayer : logi NA
    ##   ..$ transfo : chr ""
    ##   ..$ epsilon : NULL
    ##   ..$ Weights : NULL
    ##   ..- attr(*, "class")= chr [1:2] "Single" "InputsCrit"
    ##  $ LASSI_20:List of 8
    ##   ..$ FUN_CRIT:function (InputsCrit, OutputsModel, warnings = TRUE, verbose = TRUE)  
    ##   .. ..- attr(*, "class")= chr [1:2] "FUN_CRIT" "function"
    ##   ..$ Obs     : num [1:18628] 0.0591 0.0591 0.0986 0.0986 0.0986 ...
    ##   ..$ VarObs  : chr "Q"
    ##   ..$ BoolCrit: logi [1:18628] TRUE TRUE TRUE TRUE TRUE TRUE ...
    ##   ..$ idLayer : logi NA
    ##   ..$ transfo : chr ""
    ##   ..$ epsilon : NULL
    ##   ..$ Weights : NULL
    ##   ..- attr(*, "class")= chr [1:2] "Single" "InputsCrit"
    ##  $ VITRY_25:List of 8
    ##   ..$ FUN_CRIT:function (InputsCrit, OutputsModel, warnings = TRUE, verbose = TRUE)  
    ##   .. ..- attr(*, "class")= chr [1:2] "FUN_CRIT" "function"
    ##   ..$ Obs     : num [1:18628] 0.086 0.0737 0.0737 0.0737 0.0737 ...
    ##   ..$ VarObs  : chr "Q"
    ##   ..$ BoolCrit: logi [1:18628] TRUE TRUE TRUE TRUE TRUE TRUE ...
    ##   ..$ idLayer : logi NA
    ##   ..$ transfo : chr ""
    ##   ..$ epsilon : NULL
    ##   ..$ Weights : NULL
    ##   ..- attr(*, "class")= chr [1:2] "Single" "InputsCrit"
    ##  $ GURGY_02:List of 8
    ##   ..$ FUN_CRIT:function (InputsCrit, OutputsModel, warnings = TRUE, verbose = TRUE)  
    ##   .. ..- attr(*, "class")= chr [1:2] "FUN_CRIT" "function"
    ##   ..$ Obs     : num [1:18628] 0.21 0.219 0.285 0.271 0.256 ...
    ##   ..$ VarObs  : chr "Q"
    ##   ..$ BoolCrit: logi [1:18628] TRUE TRUE TRUE TRUE TRUE TRUE ...
    ##   ..$ idLayer : logi NA
    ##   ..$ transfo : chr ""
    ##   ..$ epsilon : NULL
    ##   ..$ Weights : NULL
    ##   ..- attr(*, "class")= chr [1:2] "Single" "InputsCrit"
    ##  $ BRIEN_03:List of 8
    ##   ..$ FUN_CRIT:function (InputsCrit, OutputsModel, warnings = TRUE, verbose = TRUE)  
    ##   .. ..- attr(*, "class")= chr [1:2] "FUN_CRIT" "function"
    ##   ..$ Obs     : num [1:18628] 0.113 0.104 0.104 0.113 0.104 ...
    ##   ..$ VarObs  : chr "Q"
    ##   ..$ BoolCrit: logi [1:18628] TRUE TRUE TRUE TRUE TRUE TRUE ...
    ##   ..$ idLayer : logi NA
    ##   ..$ transfo : chr ""
    ##   ..$ epsilon : NULL
    ##   ..$ Weights : NULL
    ##   ..- attr(*, "class")= chr [1:2] "Single" "InputsCrit"
    ##  $ CHABL_12:List of 8
    ##   ..$ FUN_CRIT:function (InputsCrit, OutputsModel, warnings = TRUE, verbose = TRUE)  
    ##   .. ..- attr(*, "class")= chr [1:2] "FUN_CRIT" "function"
    ##   ..$ Obs     : num [1:18628] 0.0929 0.0929 0.0155 0.0232 0.0155 ...
    ##   ..$ VarObs  : chr "Q"
    ##   ..$ BoolCrit: logi [1:18628] TRUE TRUE TRUE TRUE TRUE TRUE ...
    ##   ..$ idLayer : logi NA
    ##   ..$ transfo : chr ""
    ##   ..$ epsilon : NULL
    ##   ..$ Weights : NULL
    ##   ..- attr(*, "class")= chr [1:2] "Single" "InputsCrit"
    ##  $ CHALO_21:List of 8
    ##   ..$ FUN_CRIT:function (InputsCrit, OutputsModel, warnings = TRUE, verbose = TRUE)  
    ##   .. ..- attr(*, "class")= chr [1:2] "FUN_CRIT" "function"
    ##   ..$ Obs     : num [1:18628] 0.121 0.157 0.135 0.159 0.264 ...
    ##   ..$ VarObs  : chr "Q"
    ##   ..$ BoolCrit: logi [1:18628] TRUE TRUE TRUE TRUE TRUE TRUE ...
    ##   ..$ idLayer : logi NA
    ##   ..$ transfo : chr ""
    ##   ..$ epsilon : NULL
    ##   ..$ Weights : NULL
    ##   ..- attr(*, "class")= chr [1:2] "Single" "InputsCrit"
    ##  $ MERY-_22:List of 8
    ##   ..$ FUN_CRIT:function (InputsCrit, OutputsModel, warnings = TRUE, verbose = TRUE)  
    ##   .. ..- attr(*, "class")= chr [1:2] "FUN_CRIT" "function"
    ##   ..$ Obs     : num [1:18628] 0.0886 0.0975 0.0886 0.0886 0.082 ...
    ##   ..$ VarObs  : chr "Q"
    ##   ..$ BoolCrit: logi [1:18628] TRUE TRUE TRUE TRUE TRUE TRUE ...
    ##   ..$ idLayer : logi NA
    ##   ..$ transfo : chr ""
    ##   ..$ epsilon : NULL
    ##   ..$ Weights : NULL
    ##   ..- attr(*, "class")= chr [1:2] "Single" "InputsCrit"
    ##  $ ARCIS_24:List of 8
    ##   ..$ FUN_CRIT:function (InputsCrit, OutputsModel, warnings = TRUE, verbose = TRUE)  
    ##   .. ..- attr(*, "class")= chr [1:2] "FUN_CRIT" "function"
    ##   ..$ Obs     : num [1:18628] 0.0697 0.0529 0.0649 0.0601 0.0961 ...
    ##   ..$ VarObs  : chr "Q"
    ##   ..$ BoolCrit: logi [1:18628] TRUE TRUE TRUE TRUE TRUE TRUE ...
    ##   ..$ idLayer : logi NA
    ##   ..$ transfo : chr ""
    ##   ..$ epsilon : NULL
    ##   ..$ Weights : NULL
    ##   ..- attr(*, "class")= chr [1:2] "Single" "InputsCrit"
    ##  $ NOGEN_13:List of 8
    ##   ..$ FUN_CRIT:function (InputsCrit, OutputsModel, warnings = TRUE, verbose = TRUE)  
    ##   .. ..- attr(*, "class")= chr [1:2] "FUN_CRIT" "function"
    ##   ..$ Obs     : num [1:18628] 0.0762 0.0612 0.0612 0.0706 0.0612 ...
    ##   ..$ VarObs  : chr "Q"
    ##   ..$ BoolCrit: logi [1:18628] TRUE TRUE TRUE TRUE TRUE TRUE ...
    ##   ..$ idLayer : logi NA
    ##   ..$ transfo : chr ""
    ##   ..$ epsilon : NULL
    ##   ..$ Weights : NULL
    ##   ..- attr(*, "class")= chr [1:2] "Single" "InputsCrit"
    ##  $ NOISI_17:List of 8
    ##   ..$ FUN_CRIT:function (InputsCrit, OutputsModel, warnings = TRUE, verbose = TRUE)  
    ##   .. ..- attr(*, "class")= chr [1:2] "FUN_CRIT" "function"
    ##   ..$ Obs     : num [1:18628] 0.139 0.146 0.163 0.17 0.176 ...
    ##   ..$ VarObs  : chr "Q"
    ##   ..$ BoolCrit: logi [1:18628] TRUE TRUE TRUE TRUE TRUE TRUE ...
    ##   ..$ idLayer : logi NA
    ##   ..$ transfo : chr ""
    ##   ..$ epsilon : NULL
    ##   ..$ Weights : NULL
    ##   ..- attr(*, "class")= chr [1:2] "Single" "InputsCrit"
    ##  $ COURL_23:List of 8
    ##   ..$ FUN_CRIT:function (InputsCrit, OutputsModel, warnings = TRUE, verbose = TRUE)  
    ##   .. ..- attr(*, "class")= chr [1:2] "FUN_CRIT" "function"
    ##   ..$ Obs     : num [1:18628] 0.205 0.196 0.2 0.218 0.209 ...
    ##   ..$ VarObs  : chr "Q"
    ##   ..$ BoolCrit: logi [1:18628] TRUE TRUE TRUE TRUE TRUE TRUE ...
    ##   ..$ idLayer : logi NA
    ##   ..$ transfo : chr ""
    ##   ..$ epsilon : NULL
    ##   ..$ Weights : NULL
    ##   ..- attr(*, "class")= chr [1:2] "Single" "InputsCrit"
    ##  $ MONTE_15:List of 8
    ##   ..$ FUN_CRIT:function (InputsCrit, OutputsModel, warnings = TRUE, verbose = TRUE)  
    ##   .. ..- attr(*, "class")= chr [1:2] "FUN_CRIT" "function"
    ##   ..$ Obs     : num [1:18628] 0.122 0.129 0.132 0.127 0.137 ...
    ##   ..$ VarObs  : chr "Q"
    ##   ..$ BoolCrit: logi [1:18628] TRUE TRUE TRUE TRUE TRUE TRUE ...
    ##   ..$ idLayer : logi NA
    ##   ..$ transfo : chr ""
    ##   ..$ epsilon : NULL
    ##   ..$ Weights : NULL
    ##   ..- attr(*, "class")= chr [1:2] "Single" "InputsCrit"
    ##  $ ALFOR_16:List of 8
    ##   ..$ FUN_CRIT:function (InputsCrit, OutputsModel, warnings = TRUE, verbose = TRUE)  
    ##   .. ..- attr(*, "class")= chr [1:2] "FUN_CRIT" "function"
    ##   ..$ Obs     : num [1:18628] 0.194 0.196 0.198 0.198 0.201 ...
    ##   ..$ VarObs  : chr "Q"
    ##   ..$ BoolCrit: logi [1:18628] TRUE TRUE TRUE TRUE TRUE TRUE ...
    ##   ..$ idLayer : logi NA
    ##   ..$ transfo : chr ""
    ##   ..$ epsilon : NULL
    ##   ..$ Weights : NULL
    ##   ..- attr(*, "class")= chr [1:2] "Single" "InputsCrit"
    ##  $ PARIS_05:List of 8
    ##   ..$ FUN_CRIT:function (InputsCrit, OutputsModel, warnings = TRUE, verbose = TRUE)  
    ##   .. ..- attr(*, "class")= chr [1:2] "FUN_CRIT" "function"
    ##   ..$ Obs     : num [1:18628] 0.176 0.179 0.183 0.187 0.191 ...
    ##   ..$ VarObs  : chr "Q"
    ##   ..$ BoolCrit: logi [1:18628] TRUE TRUE TRUE TRUE TRUE TRUE ...
    ##   ..$ idLayer : logi NA
    ##   ..$ transfo : chr ""
    ##   ..$ epsilon : NULL
    ##   ..$ Weights : NULL
    ##   ..- attr(*, "class")= chr [1:2] "Single" "InputsCrit"
    ##  - attr(*, "class")= chr [1:2] "GRiwrmInputsCrit" "list"

## GRiwrmCalibOptions object

``` r
CalibOptions <- CreateCalibOptions(InputsModel)
str(CalibOptions)
```

    ## List of 25
    ##  $ TRANN_01:List of 4
    ##   ..$ FixedParam       : logi [1:4] NA NA NA NA
    ##   ..$ SearchRanges     : num [1:2, 1:4] 4.59e-05 2.18e+04 -1.09e+04 1.09e+04 4.59e-05 ...
    ##   ..$ FUN_TRANSFO      :function (ParamIn, Direction)  
    ##   ..$ StartParamDistrib: num [1:3, 1:4] 169.017 247.151 432.681 -2.376 -0.649 ...
    ##   ..- attr(*, "class")= chr [1:4] "CalibOptions" "daily" "GR" "HBAN"
    ##  $ STDIZ_04:List of 4
    ##   ..$ FixedParam       : logi [1:4] NA NA NA NA
    ##   ..$ SearchRanges     : num [1:2, 1:4] 4.59e-05 2.18e+04 -1.09e+04 1.09e+04 4.59e-05 ...
    ##   ..$ FUN_TRANSFO      :function (ParamIn, Direction)  
    ##   ..$ StartParamDistrib: num [1:3, 1:4] 169.017 247.151 432.681 -2.376 -0.649 ...
    ##   ..- attr(*, "class")= chr [1:4] "CalibOptions" "daily" "GR" "HBAN"
    ##  $ BAR-S_06:List of 4
    ##   ..$ FixedParam       : logi [1:4] NA NA NA NA
    ##   ..$ SearchRanges     : num [1:2, 1:4] 4.59e-05 2.18e+04 -1.09e+04 1.09e+04 4.59e-05 ...
    ##   ..$ FUN_TRANSFO      :function (ParamIn, Direction)  
    ##   ..$ StartParamDistrib: num [1:3, 1:4] 169.017 247.151 432.681 -2.376 -0.649 ...
    ##   ..- attr(*, "class")= chr [1:4] "CalibOptions" "daily" "GR" "HBAN"
    ##  $ CHAUM_07:List of 4
    ##   ..$ FixedParam       : logi [1:4] NA NA NA NA
    ##   ..$ SearchRanges     : num [1:2, 1:4] 4.59e-05 2.18e+04 -1.09e+04 1.09e+04 4.59e-05 ...
    ##   ..$ FUN_TRANSFO      :function (ParamIn, Direction)  
    ##   ..$ StartParamDistrib: num [1:3, 1:4] 169.017 247.151 432.681 -2.376 -0.649 ...
    ##   ..- attr(*, "class")= chr [1:4] "CalibOptions" "daily" "GR" "HBAN"
    ##  $ CUSSY_08:List of 4
    ##   ..$ FixedParam       : logi [1:4] NA NA NA NA
    ##   ..$ SearchRanges     : num [1:2, 1:4] 4.59e-05 2.18e+04 -1.09e+04 1.09e+04 4.59e-05 ...
    ##   ..$ FUN_TRANSFO      :function (ParamIn, Direction)  
    ##   ..$ StartParamDistrib: num [1:3, 1:4] 169.017 247.151 432.681 -2.376 -0.649 ...
    ##   ..- attr(*, "class")= chr [1:4] "CalibOptions" "daily" "GR" "HBAN"
    ##  $ STGER_09:List of 4
    ##   ..$ FixedParam       : logi [1:4] NA NA NA NA
    ##   ..$ SearchRanges     : num [1:2, 1:4] 4.59e-05 2.18e+04 -1.09e+04 1.09e+04 4.59e-05 ...
    ##   ..$ FUN_TRANSFO      :function (ParamIn, Direction)  
    ##   ..$ StartParamDistrib: num [1:3, 1:4] 169.017 247.151 432.681 -2.376 -0.649 ...
    ##   ..- attr(*, "class")= chr [1:4] "CalibOptions" "daily" "GR" "HBAN"
    ##  $ GUILL_10:List of 4
    ##   ..$ FixedParam       : logi [1:4] NA NA NA NA
    ##   ..$ SearchRanges     : num [1:2, 1:4] 4.59e-05 2.18e+04 -1.09e+04 1.09e+04 4.59e-05 ...
    ##   ..$ FUN_TRANSFO      :function (ParamIn, Direction)  
    ##   ..$ StartParamDistrib: num [1:3, 1:4] 169.017 247.151 432.681 -2.376 -0.649 ...
    ##   ..- attr(*, "class")= chr [1:4] "CalibOptions" "daily" "GR" "HBAN"
    ##  $ AISY-_11:List of 4
    ##   ..$ FixedParam       : logi [1:4] NA NA NA NA
    ##   ..$ SearchRanges     : num [1:2, 1:4] 4.59e-05 2.18e+04 -1.09e+04 1.09e+04 4.59e-05 ...
    ##   ..$ FUN_TRANSFO      :function (ParamIn, Direction)  
    ##   ..$ StartParamDistrib: num [1:3, 1:4] 169.017 247.151 432.681 -2.376 -0.649 ...
    ##   ..- attr(*, "class")= chr [1:4] "CalibOptions" "daily" "GR" "HBAN"
    ##  $ EPISY_14:List of 4
    ##   ..$ FixedParam       : logi [1:4] NA NA NA NA
    ##   ..$ SearchRanges     : num [1:2, 1:4] 4.59e-05 2.18e+04 -1.09e+04 1.09e+04 4.59e-05 ...
    ##   ..$ FUN_TRANSFO      :function (ParamIn, Direction)  
    ##   ..$ StartParamDistrib: num [1:3, 1:4] 169.017 247.151 432.681 -2.376 -0.649 ...
    ##   ..- attr(*, "class")= chr [1:4] "CalibOptions" "daily" "GR" "HBAN"
    ##  $ MONTR_18:List of 4
    ##   ..$ FixedParam       : logi [1:4] NA NA NA NA
    ##   ..$ SearchRanges     : num [1:2, 1:4] 4.59e-05 2.18e+04 -1.09e+04 1.09e+04 4.59e-05 ...
    ##   ..$ FUN_TRANSFO      :function (ParamIn, Direction)  
    ##   ..$ StartParamDistrib: num [1:3, 1:4] 169.017 247.151 432.681 -2.376 -0.649 ...
    ##   ..- attr(*, "class")= chr [1:4] "CalibOptions" "daily" "GR" "HBAN"
    ##  $ LOUVE_19:List of 4
    ##   ..$ FixedParam       : logi [1:4] NA NA NA NA
    ##   ..$ SearchRanges     : num [1:2, 1:4] 4.59e-05 2.18e+04 -1.09e+04 1.09e+04 4.59e-05 ...
    ##   ..$ FUN_TRANSFO      :function (ParamIn, Direction)  
    ##   ..$ StartParamDistrib: num [1:3, 1:4] 169.017 247.151 432.681 -2.376 -0.649 ...
    ##   ..- attr(*, "class")= chr [1:4] "CalibOptions" "daily" "GR" "HBAN"
    ##  $ LASSI_20:List of 4
    ##   ..$ FixedParam       : logi [1:4] NA NA NA NA
    ##   ..$ SearchRanges     : num [1:2, 1:4] 4.59e-05 2.18e+04 -1.09e+04 1.09e+04 4.59e-05 ...
    ##   ..$ FUN_TRANSFO      :function (ParamIn, Direction)  
    ##   ..$ StartParamDistrib: num [1:3, 1:4] 169.017 247.151 432.681 -2.376 -0.649 ...
    ##   ..- attr(*, "class")= chr [1:4] "CalibOptions" "daily" "GR" "HBAN"
    ##  $ VITRY_25:List of 4
    ##   ..$ FixedParam       : logi [1:4] NA NA NA NA
    ##   ..$ SearchRanges     : num [1:2, 1:4] 4.59e-05 2.18e+04 -1.09e+04 1.09e+04 4.59e-05 ...
    ##   ..$ FUN_TRANSFO      :function (ParamIn, Direction)  
    ##   ..$ StartParamDistrib: num [1:3, 1:4] 169.017 247.151 432.681 -2.376 -0.649 ...
    ##   ..- attr(*, "class")= chr [1:4] "CalibOptions" "daily" "GR" "HBAN"
    ##  $ GURGY_02:List of 4
    ##   ..$ FixedParam       : logi [1:5] NA NA NA NA NA
    ##   ..$ SearchRanges     : num [1:2, 1:5] 1.00e-02 2.00e+01 4.59e-05 2.18e+04 -1.09e+04 ...
    ##   ..$ FUN_TRANSFO      :function (ParamIn, Direction)  
    ##   ..$ StartParamDistrib: num [1:3, 1:5] 1.25 2.5 5 169.02 247.15 ...
    ##   ..- attr(*, "class")= chr [1:5] "CalibOptions" "daily" "GR" "SD" ...
    ##  $ BRIEN_03:List of 4
    ##   ..$ FixedParam       : logi [1:5] NA NA NA NA NA
    ##   ..$ SearchRanges     : num [1:2, 1:5] 1.00e-02 2.00e+01 4.59e-05 2.18e+04 -1.09e+04 ...
    ##   ..$ FUN_TRANSFO      :function (ParamIn, Direction)  
    ##   ..$ StartParamDistrib: num [1:3, 1:5] 1.25 2.5 5 169.02 247.15 ...
    ##   ..- attr(*, "class")= chr [1:5] "CalibOptions" "daily" "GR" "SD" ...
    ##  $ CHABL_12:List of 4
    ##   ..$ FixedParam       : logi [1:5] NA NA NA NA NA
    ##   ..$ SearchRanges     : num [1:2, 1:5] 1.00e-02 2.00e+01 4.59e-05 2.18e+04 -1.09e+04 ...
    ##   ..$ FUN_TRANSFO      :function (ParamIn, Direction)  
    ##   ..$ StartParamDistrib: num [1:3, 1:5] 1.25 2.5 5 169.02 247.15 ...
    ##   ..- attr(*, "class")= chr [1:5] "CalibOptions" "daily" "GR" "SD" ...
    ##  $ CHALO_21:List of 4
    ##   ..$ FixedParam       : logi [1:5] NA NA NA NA NA
    ##   ..$ SearchRanges     : num [1:2, 1:5] 1.00e-02 2.00e+01 4.59e-05 2.18e+04 -1.09e+04 ...
    ##   ..$ FUN_TRANSFO      :function (ParamIn, Direction)  
    ##   ..$ StartParamDistrib: num [1:3, 1:5] 1.25 2.5 5 169.02 247.15 ...
    ##   ..- attr(*, "class")= chr [1:5] "CalibOptions" "daily" "GR" "SD" ...
    ##  $ MERY-_22:List of 4
    ##   ..$ FixedParam       : logi [1:5] NA NA NA NA NA
    ##   ..$ SearchRanges     : num [1:2, 1:5] 1.00e-02 2.00e+01 4.59e-05 2.18e+04 -1.09e+04 ...
    ##   ..$ FUN_TRANSFO      :function (ParamIn, Direction)  
    ##   ..$ StartParamDistrib: num [1:3, 1:5] 1.25 2.5 5 169.02 247.15 ...
    ##   ..- attr(*, "class")= chr [1:5] "CalibOptions" "daily" "GR" "SD" ...
    ##  $ ARCIS_24:List of 4
    ##   ..$ FixedParam       : logi [1:5] NA NA NA NA NA
    ##   ..$ SearchRanges     : num [1:2, 1:5] 1.00e-02 2.00e+01 4.59e-05 2.18e+04 -1.09e+04 ...
    ##   ..$ FUN_TRANSFO      :function (ParamIn, Direction)  
    ##   ..$ StartParamDistrib: num [1:3, 1:5] 1.25 2.5 5 169.02 247.15 ...
    ##   ..- attr(*, "class")= chr [1:5] "CalibOptions" "daily" "GR" "SD" ...
    ##  $ NOGEN_13:List of 4
    ##   ..$ FixedParam       : logi [1:5] NA NA NA NA NA
    ##   ..$ SearchRanges     : num [1:2, 1:5] 1.00e-02 2.00e+01 4.59e-05 2.18e+04 -1.09e+04 ...
    ##   ..$ FUN_TRANSFO      :function (ParamIn, Direction)  
    ##   ..$ StartParamDistrib: num [1:3, 1:5] 1.25 2.5 5 169.02 247.15 ...
    ##   ..- attr(*, "class")= chr [1:5] "CalibOptions" "daily" "GR" "SD" ...
    ##  $ NOISI_17:List of 4
    ##   ..$ FixedParam       : logi [1:5] NA NA NA NA NA
    ##   ..$ SearchRanges     : num [1:2, 1:5] 1.00e-02 2.00e+01 4.59e-05 2.18e+04 -1.09e+04 ...
    ##   ..$ FUN_TRANSFO      :function (ParamIn, Direction)  
    ##   ..$ StartParamDistrib: num [1:3, 1:5] 1.25 2.5 5 169.02 247.15 ...
    ##   ..- attr(*, "class")= chr [1:5] "CalibOptions" "daily" "GR" "SD" ...
    ##  $ COURL_23:List of 4
    ##   ..$ FixedParam       : logi [1:5] NA NA NA NA NA
    ##   ..$ SearchRanges     : num [1:2, 1:5] 1.00e-02 2.00e+01 4.59e-05 2.18e+04 -1.09e+04 ...
    ##   ..$ FUN_TRANSFO      :function (ParamIn, Direction)  
    ##   ..$ StartParamDistrib: num [1:3, 1:5] 1.25 2.5 5 169.02 247.15 ...
    ##   ..- attr(*, "class")= chr [1:5] "CalibOptions" "daily" "GR" "SD" ...
    ##  $ MONTE_15:List of 4
    ##   ..$ FixedParam       : logi [1:5] NA NA NA NA NA
    ##   ..$ SearchRanges     : num [1:2, 1:5] 1.00e-02 2.00e+01 4.59e-05 2.18e+04 -1.09e+04 ...
    ##   ..$ FUN_TRANSFO      :function (ParamIn, Direction)  
    ##   ..$ StartParamDistrib: num [1:3, 1:5] 1.25 2.5 5 169.02 247.15 ...
    ##   ..- attr(*, "class")= chr [1:5] "CalibOptions" "daily" "GR" "SD" ...
    ##  $ ALFOR_16:List of 4
    ##   ..$ FixedParam       : logi [1:5] NA NA NA NA NA
    ##   ..$ SearchRanges     : num [1:2, 1:5] 1.00e-02 2.00e+01 4.59e-05 2.18e+04 -1.09e+04 ...
    ##   ..$ FUN_TRANSFO      :function (ParamIn, Direction)  
    ##   ..$ StartParamDistrib: num [1:3, 1:5] 1.25 2.5 5 169.02 247.15 ...
    ##   ..- attr(*, "class")= chr [1:5] "CalibOptions" "daily" "GR" "SD" ...
    ##  $ PARIS_05:List of 4
    ##   ..$ FixedParam       : logi [1:5] NA NA NA NA NA
    ##   ..$ SearchRanges     : num [1:2, 1:5] 1.00e-02 2.00e+01 4.59e-05 2.18e+04 -1.09e+04 ...
    ##   ..$ FUN_TRANSFO      :function (ParamIn, Direction)  
    ##   ..$ StartParamDistrib: num [1:3, 1:5] 1.25 2.5 5 169.02 247.15 ...
    ##   ..- attr(*, "class")= chr [1:5] "CalibOptions" "daily" "GR" "SD" ...
    ##  - attr(*, "class")= chr [1:2] "GRiwrmCalibOptions" "list"

## Calibration

The optimization (i.e. calibration) of parameters can now be performed:

``` r
OutputsCalib <- Calibration(InputsModel, RunOptions, InputsCrit, CalibOptions)
```

    ## Calibration.GRiwrmInputsModel: Processing sub-basin 'TRANN_01'...

    ## Grid-Screening in progress (0% 20% 40% 60% 80% 100%)
    ##   Screening completed (81 runs)
    ##       Param =  247.151,   -0.020,   83.096,    2.384
    ##       Crit. KGE2[Q]      = 0.8828
    ## Steepest-descent local search in progress
    ##   Calibration completed (47 iterations, 454 runs)
    ##       Param =  202.172,   -0.124,   76.792,    5.505
    ##       Crit. KGE2[Q]      = 0.9549
    ## Calibration.GRiwrmInputsModel: Processing sub-basin 'STDIZ_04'...
    ## Grid-Screening in progress (0% 20% 40% 60% 80% 100%)
    ##   Screening completed (81 runs)
    ##       Param =  169.017,   -0.020,   83.096,    2.384
    ##       Crit. KGE2[Q]      = 0.9015
    ## Steepest-descent local search in progress
    ##   Calibration completed (34 iterations, 342 runs)
    ##       Param =  161.253,   -0.238,   69.848,    3.746
    ##       Crit. KGE2[Q]      = 0.9472
    ## Calibration.GRiwrmInputsModel: Processing sub-basin 'BAR-S_06'...
    ## Grid-Screening in progress (0% 20% 40% 60% 80% 100%)
    ##   Screening completed (81 runs)
    ##       Param =  247.151,   -0.020,   83.096,    2.384
    ##       Crit. KGE2[Q]      = 0.8826
    ## Steepest-descent local search in progress
    ##   Calibration completed (27 iterations, 281 runs)
    ##       Param =  247.151,   -0.347,   91.836,    5.302
    ##       Crit. KGE2[Q]      = 0.9588
    ## Calibration.GRiwrmInputsModel: Processing sub-basin 'CHAUM_07'...
    ## Grid-Screening in progress (0% 20% 40% 60% 80% 100%)
    ##   Screening completed (81 runs)
    ##       Param =  432.681,   -0.020,   83.096,    1.417
    ##       Crit. KGE2[Q]      = 0.9019
    ## Steepest-descent local search in progress
    ##   Calibration completed (32 iterations, 331 runs)
    ##       Param =  269.272,    0.421,  166.665,    1.257
    ##       Crit. KGE2[Q]      = 0.9434
    ## Calibration.GRiwrmInputsModel: Processing sub-basin 'CUSSY_08'...
    ## Grid-Screening in progress (0% 20% 40% 60% 80% 100%)
    ##   Screening completed (81 runs)
    ##       Param =  247.151,   -0.649,   42.098,    2.384
    ##       Crit. KGE2[Q]      = 0.8985
    ## Steepest-descent local search in progress
    ##   Calibration completed (26 iterations, 278 runs)
    ##       Param =  214.586,   -0.974,   59.761,    2.087
    ##       Crit. KGE2[Q]      = 0.9066
    ## Calibration.GRiwrmInputsModel: Processing sub-basin 'STGER_09'...
    ## Grid-Screening in progress (0% 20% 40% 60% 80% 100%)
    ##   Screening completed (81 runs)
    ##       Param =  432.681,   -0.020,   83.096,    1.417
    ##       Crit. KGE2[Q]      = 0.9225
    ## Steepest-descent local search in progress
    ##   Calibration completed (28 iterations, 297 runs)
    ##       Param =  325.478,   -0.313,  142.419,    1.145
    ##       Crit. KGE2[Q]      = 0.9376
    ## Calibration.GRiwrmInputsModel: Processing sub-basin 'GUILL_10'...
    ## Grid-Screening in progress (0% 20% 40% 60% 80% 100%)
    ##   Screening completed (81 runs)
    ##       Param =  169.017,   -2.376,   42.098,    2.384
    ##       Crit. KGE2[Q]      = 0.8562
    ## Steepest-descent local search in progress
    ##   Calibration completed (32 iterations, 331 runs)
    ##       Param =  183.842,   -1.733,   24.004,    2.586
    ##       Crit. KGE2[Q]      = 0.8879
    ## Calibration.GRiwrmInputsModel: Processing sub-basin 'AISY-_11'...
    ## Grid-Screening in progress (0% 20% 40% 60% 80% 100%)
    ##   Screening completed (81 runs)
    ##       Param =  169.017,   -2.376,   83.096,    2.384
    ##       Crit. KGE2[Q]      = 0.8725
    ## Steepest-descent local search in progress
    ##   Calibration completed (41 iterations, 411 runs)
    ##       Param =  172.375,   -1.653,   49.462,    2.668
    ##       Crit. KGE2[Q]      = 0.9175
    ## Calibration.GRiwrmInputsModel: Processing sub-basin 'EPISY_14'...
    ## Grid-Screening in progress (0% 20% 40% 60% 80% 100%)
    ##   Screening completed (81 runs)
    ##       Param =  432.681,   -0.649,   83.096,    2.384
    ##       Crit. KGE2[Q]      = 0.8520
    ## Steepest-descent local search in progress
    ##   Calibration completed (36 iterations, 369 runs)
    ##       Param =  610.285,   -0.593,   41.294,    3.754
    ##       Crit. KGE2[Q]      = 0.9395
    ## Calibration.GRiwrmInputsModel: Processing sub-basin 'MONTR_18'...
    ## Grid-Screening in progress (0% 20% 40% 60% 80% 100%)
    ##   Screening completed (81 runs)
    ##       Param =  247.151,   -0.649,   42.098,    2.384
    ##       Crit. KGE2[Q]      = 0.8528
    ## Steepest-descent local search in progress
    ##   Calibration completed (18 iterations, 208 runs)
    ##       Param =  292.949,   -0.578,   40.854,    2.169
    ##       Crit. KGE2[Q]      = 0.8634
    ## Calibration.GRiwrmInputsModel: Processing sub-basin 'LOUVE_19'...
    ## Grid-Screening in progress (0% 20% 40% 60% 80% 100%)
    ##   Screening completed (81 runs)
    ##       Param =  169.017,   -0.649,   83.096,    2.384
    ##       Crit. KGE2[Q]      = 0.8814
    ## Steepest-descent local search in progress
    ##   Calibration completed (17 iterations, 200 runs)
    ##       Param =  162.390,   -1.099,   83.096,    3.438
    ##       Crit. KGE2[Q]      = 0.9138
    ## Calibration.GRiwrmInputsModel: Processing sub-basin 'LASSI_20'...
    ## Grid-Screening in progress (0% 20% 40% 60% 80% 100%)
    ##   Screening completed (81 runs)
    ##       Param =  247.151,   -0.649,   83.096,    2.384
    ##       Crit. KGE2[Q]      = 0.8510
    ## Steepest-descent local search in progress
    ##   Calibration completed (32 iterations, 322 runs)
    ##       Param =  223.632,   -1.160,   75.944,    4.355
    ##       Crit. KGE2[Q]      = 0.9075
    ## Calibration.GRiwrmInputsModel: Processing sub-basin 'VITRY_25'...
    ## Grid-Screening in progress (0% 20% 40% 60% 80% 100%)
    ##   Screening completed (81 runs)
    ##       Param =  432.681,   -0.649,   83.096,    2.384
    ##       Crit. KGE2[Q]      = 0.8652
    ## Steepest-descent local search in progress
    ##   Calibration completed (83 iterations, 785 runs)
    ##       Param =  278.332,   -1.382,   97.715,    5.076
    ##       Crit. KGE2[Q]      = 0.9550
    ## Calibration.GRiwrmInputsModel: Processing sub-basin 'GURGY_02'...
    ## Grid-Screening in progress (0% 20% 40% 60% 80% 100%)
    ##   Screening completed (243 runs)
    ##       Param =    1.250,  432.681,   -2.376,   83.096,    2.384
    ##       Crit. KGE2[Q]      = 0.9499
    ## Steepest-descent local search in progress
    ##   Calibration completed (43 iterations, 669 runs)
    ##       Param =    0.784,  405.646,   -3.350,  114.106,    2.234
    ##       Crit. KGE2[Q]      = 0.9623
    ## Calibration.GRiwrmInputsModel: Processing sub-basin 'BRIEN_03'...
    ## Grid-Screening in progress (0% 20% 40% 60% 80% 100%)
    ##   Screening completed (243 runs)
    ##       Param =    1.250,  169.017,   -0.649,   83.096,    2.384
    ##       Crit. KGE2[Q]      = 0.9229
    ## Steepest-descent local search in progress
    ##   Calibration completed (43 iterations, 662 runs)
    ##       Param =    0.664,  209.147,   -0.294,   59.743,    3.341
    ##       Crit. KGE2[Q]      = 0.9391
    ## Calibration.GRiwrmInputsModel: Processing sub-basin 'CHABL_12'...
    ## Grid-Screening in progress (0% 20% 40% 60% 80% 100%)
    ##   Screening completed (243 runs)
    ##       Param =    1.250,  247.151,   -2.376,   42.098,    2.384
    ##       Crit. KGE2[Q]      = 0.8999
    ## Steepest-descent local search in progress
    ##   Calibration completed (54 iterations, 780 runs)
    ##       Param =    0.840,  197.911,   -2.735,   45.323,    5.548
    ##       Crit. KGE2[Q]      = 0.9385
    ## Calibration.GRiwrmInputsModel: Processing sub-basin 'CHALO_21'...
    ## Grid-Screening in progress (0% 20% 40% 60% 80% 100%)
    ##   Screening completed (243 runs)
    ##       Param =    1.250,  432.681,   -2.376,   83.096,    1.417
    ##       Crit. KGE2[Q]      = 0.9247
    ## Steepest-descent local search in progress
    ##   Calibration completed (79 iterations, 1024 runs)
    ##       Param =    0.437,  622.380,   -2.780,   96.421,   11.967
    ##       Crit. KGE2[Q]      = 0.9667
    ## Calibration.GRiwrmInputsModel: Processing sub-basin 'MERY-_22'...
    ## Grid-Screening in progress (0% 20% 40% 60% 80% 100%)
    ##   Screening completed (243 runs)
    ##       Param =    1.250,  432.681,   -2.376,   42.098,    1.417
    ##       Crit. KGE2[Q]      = 0.8989
    ## Steepest-descent local search in progress
    ##   Calibration completed (44 iterations, 663 runs)
    ##       Param =    0.269,  724.085,   -2.376,   26.950,    5.557
    ##       Crit. KGE2[Q]      = 0.9574
    ## Calibration.GRiwrmInputsModel: Processing sub-basin 'ARCIS_24'...
    ## Grid-Screening in progress (0% 20% 40% 60% 80% 100%)
    ##   Screening completed (243 runs)
    ##       Param =    1.250,  432.681,   -2.376,   83.096,    2.384
    ##       Crit. KGE2[Q]      = 0.9243
    ## Steepest-descent local search in progress
    ##   Calibration completed (49 iterations, 720 runs)
    ##       Param =    0.326,  411.579,   -6.460,  168.047,    3.999
    ##       Crit. KGE2[Q]      = 0.9615
    ## Calibration.GRiwrmInputsModel: Processing sub-basin 'NOGEN_13'...
    ## Grid-Screening in progress (0% 20% 40% 60% 80% 100%)
    ##   Screening completed (243 runs)
    ##       Param =    1.250,  432.681,   -2.376,   20.697,    1.417
    ##       Crit. KGE2[Q]      = 0.9253
    ## Steepest-descent local search in progress
    ##   Calibration completed (197 iterations, 2307 runs)
    ##       Param =    0.257, 1128.402,   -6.699,  116.595,    3.572
    ##       Crit. KGE2[Q]      = 0.9630
    ## Calibration.GRiwrmInputsModel: Processing sub-basin 'NOISI_17'...
    ## Grid-Screening in progress (0% 20% 40% 60% 80% 100%)
    ##   Screening completed (243 runs)
    ##       Param =    1.250,  432.681,   -2.376,   20.697,    1.417
    ##       Crit. KGE2[Q]      = 0.8680
    ## Steepest-descent local search in progress
    ##   Calibration completed (47 iterations, 696 runs)
    ##       Param =    0.605, 1502.367,   -2.376,   18.669,    7.424
    ##       Crit. KGE2[Q]      = 0.9606
    ## Calibration.GRiwrmInputsModel: Processing sub-basin 'COURL_23'...
    ## Grid-Screening in progress (0% 20% 40% 60% 80% 100%)
    ##   Screening completed (243 runs)
    ##       Param =    1.250,  432.681,   -2.376,   83.096,    1.417
    ##       Crit. KGE2[Q]      = 0.9260
    ## Steepest-descent local search in progress
    ##   Calibration completed (96 iterations, 1240 runs)
    ##       Param =    1.218,  602.400,   -3.958,  261.056,    2.513
    ##       Crit. KGE2[Q]      = 0.9656
    ## Calibration.GRiwrmInputsModel: Processing sub-basin 'MONTE_15'...
    ## Grid-Screening in progress (0% 20% 40% 60% 80% 100%)
    ##   Screening completed (243 runs)
    ##       Param =    1.250,  432.681,   -2.376,   20.697,    1.417
    ##       Crit. KGE2[Q]      = 0.9602
    ## Steepest-descent local search in progress
    ##   Calibration completed (60 iterations, 826 runs)
    ##       Param =    0.346, 2886.511,   -5.863,   13.161,    1.634
    ##       Crit. KGE2[Q]      = 0.9762
    ## Calibration.GRiwrmInputsModel: Processing sub-basin 'ALFOR_16'...
    ## Grid-Screening in progress (0% 20% 40% 60% 80% 100%)
    ##   Screening completed (243 runs)
    ##       Param =    1.250,  432.681,   -2.376,   20.697,    1.417
    ##       Crit. KGE2[Q]      = 0.9615
    ## Steepest-descent local search in progress
    ##   Calibration completed (30 iterations, 520 runs)
    ##       Param =    1.700, 1096.633,   -2.376,   21.542,    2.501
    ##       Crit. KGE2[Q]      = 0.9783
    ## Calibration.GRiwrmInputsModel: Processing sub-basin 'PARIS_05'...
    ## Grid-Screening in progress (0% 20% 40% 60% 80% 100%)
    ##   Screening completed (243 runs)
    ##       Param =    1.250,  432.681,   -2.376,   20.697,    2.384
    ##       Crit. KGE2[Q]      = 0.9709
    ## Steepest-descent local search in progress
    ##   Calibration completed (35 iterations, 574 runs)
    ##       Param =    1.340,  854.059, -2826.665,   22.421,    8.630
    ##       Crit. KGE2[Q]      = 0.9747

## Run the GR4J model with parameters obtained from the Michel calibration

Now that the model is calibrated, we can run it with the optimized
parameter values:

``` r
ParamMichel <- extractParam(OutputsCalib)

OutputsModels <- RunModel(
  InputsModel,
  RunOptions = RunOptions,
  Param = ParamMichel
)
```

    ## RunModel.GRiwrmInputsModel: Processing sub-basin TRANN_01...

    ## RunModel.GRiwrmInputsModel: Processing sub-basin STDIZ_04...

    ## RunModel.GRiwrmInputsModel: Processing sub-basin BAR-S_06...

    ## RunModel.GRiwrmInputsModel: Processing sub-basin CHAUM_07...

    ## RunModel.GRiwrmInputsModel: Processing sub-basin CUSSY_08...

    ## RunModel.GRiwrmInputsModel: Processing sub-basin STGER_09...

    ## RunModel.GRiwrmInputsModel: Processing sub-basin GUILL_10...

    ## RunModel.GRiwrmInputsModel: Processing sub-basin AISY-_11...

    ## RunModel.GRiwrmInputsModel: Processing sub-basin EPISY_14...

    ## RunModel.GRiwrmInputsModel: Processing sub-basin MONTR_18...

    ## RunModel.GRiwrmInputsModel: Processing sub-basin LOUVE_19...

    ## RunModel.GRiwrmInputsModel: Processing sub-basin LASSI_20...

    ## RunModel.GRiwrmInputsModel: Processing sub-basin VITRY_25...

    ## RunModel.GRiwrmInputsModel: Processing sub-basin GURGY_02...

    ## RunModel.GRiwrmInputsModel: Processing sub-basin BRIEN_03...

    ## RunModel.GRiwrmInputsModel: Processing sub-basin CHABL_12...

    ## RunModel.GRiwrmInputsModel: Processing sub-basin CHALO_21...

    ## RunModel.GRiwrmInputsModel: Processing sub-basin MERY-_22...

    ## RunModel.GRiwrmInputsModel: Processing sub-basin ARCIS_24...

    ## RunModel.GRiwrmInputsModel: Processing sub-basin NOGEN_13...

    ## RunModel.GRiwrmInputsModel: Processing sub-basin NOISI_17...

    ## RunModel.GRiwrmInputsModel: Processing sub-basin COURL_23...

    ## RunModel.GRiwrmInputsModel: Processing sub-basin MONTE_15...

    ## RunModel.GRiwrmInputsModel: Processing sub-basin ALFOR_16...

    ## RunModel.GRiwrmInputsModel: Processing sub-basin PARIS_05...

## Plot the result for each basin

``` r
plot(OutputsModels, Qobs = Qnat[IndPeriod_Run,])
```

![](V03_First_Calibration_files/figure-html/plot-1.png)![](V03_First_Calibration_files/figure-html/plot-2.png)![](V03_First_Calibration_files/figure-html/plot-3.png)![](V03_First_Calibration_files/figure-html/plot-4.png)![](V03_First_Calibration_files/figure-html/plot-5.png)![](V03_First_Calibration_files/figure-html/plot-6.png)![](V03_First_Calibration_files/figure-html/plot-7.png)![](V03_First_Calibration_files/figure-html/plot-8.png)![](V03_First_Calibration_files/figure-html/plot-9.png)![](V03_First_Calibration_files/figure-html/plot-10.png)![](V03_First_Calibration_files/figure-html/plot-11.png)![](V03_First_Calibration_files/figure-html/plot-12.png)![](V03_First_Calibration_files/figure-html/plot-13.png)![](V03_First_Calibration_files/figure-html/plot-14.png)![](V03_First_Calibration_files/figure-html/plot-15.png)![](V03_First_Calibration_files/figure-html/plot-16.png)![](V03_First_Calibration_files/figure-html/plot-17.png)![](V03_First_Calibration_files/figure-html/plot-18.png)![](V03_First_Calibration_files/figure-html/plot-19.png)![](V03_First_Calibration_files/figure-html/plot-20.png)![](V03_First_Calibration_files/figure-html/plot-21.png)![](V03_First_Calibration_files/figure-html/plot-22.png)![](V03_First_Calibration_files/figure-html/plot-23.png)![](V03_First_Calibration_files/figure-html/plot-24.png)![](V03_First_Calibration_files/figure-html/plot-25.png)

## Save calibration data for next vignettes

``` r
save(ParamMichel, file = "_cache/V03.RData")
```
