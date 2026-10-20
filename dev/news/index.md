# Changelog

## airGRiwrm (development version)

### Breaking changes:

- CreateRunOptions \[Breaking change\]: The default value of
  `Outputs_Sim` has changed from `"all"` to a minimal set
  (`c("DatesR", "WarmUpQsim", "Qsim", "Qsim_m3", "QsimDown", "Param", "StateEnd")`)
  in order to reduce the size of the GRiwrmOutputsModel object for large
  networks. Use `Outputs_Sim = "all"` to restore the previous behaviour
  ([\#141](https://forge.inrae.fr/airgriwrm/airgriwrm/-/issues/141)).

### New features:

- CreateRunOptions: allow users to select which simulation outputs are
  returned via the `Outputs_Sim` argument, either as a character vector
  applied to all nodes, or as a named list with per-node selection
  ([\#141](https://forge.inrae.fr/airgriwrm/airgriwrm/-/issues/141)).
  CemaNeige nodes now also carry `Precip` in their default
  `Outputs_Sim`: the default plot of a CemaNeige node requires it in the
  *OutputsModel* to compute the precipitation panel axis as soon as the
  per-layer solid precipitation (`Psol`) is present (upstream airGR
  `ylimP` limitation; the standalone airGR bug is still to be fixed on
  the airGR side). `Precip` is a recorded meteo input, not a CemaNeige
  Fortran output: the CemaNeige `NOutputs` and the model run are
  unchanged
  ([\#178](https://forge.inrae.fr/airgriwrm/airgriwrm/-/issues/178)).
- CreateInputsCrit: allow user-defined a priori parameters for the De
  Lavenne criterion: entries of `AprioriIds` can now be numeric vectors
  (one value per model parameter of the node,
  e.g. `c(celerity, X1, X2, X3, X4)`) used as the a priori parameters
  instead of the parameters of an upstream sub-catchment
  ([\#61](https://forge.inrae.fr/airgriwrm/airgriwrm/-/issues/61)).
- plot.GRiwrmOutputsModel: display the meteorological time series
  (precipitation and potential evapotranspiration) at the
  whole-catchment scale: when available in the *GRiwrmOutputsModel*
  object, the time series of each node are replaced on the fly by their
  area-weighted mean over the catchment of the node and all its upstream
  nodes, so that the displayed meteorology reflects the whole catchment
  integrated in the simulated discharge
  ([\#178](https://forge.inrae.fr/airgriwrm/airgriwrm/-/issues/178)).
  For a CemaNeige node, the per-layer series displayed by
  [`airGR::plot.OutputsModel`](https://rdrr.io/pkg/airGR/man/plot.OutputsModel.html)
  are covered as well: the mean air temperature (`Temp`), the solid
  precipitation (`Psol`) and the snow pack state (`SnowPack`) of each
  elevation layer are replaced by their whole-catchment mean, as well as
  the mean over the layers that airGR derives from them. The same
  adjustment is available as a new exported function
  [`adjustMeteoToCatchment()`](https://inrae.github.io/airGRiwrm/dev/reference/adjustMeteoToCatchment.md):
  it computes, for a node of a *GRiwrmOutputsModel* object, the
  meteorological time series at the whole-catchment scale (including the
  CemaNeige layer series), so that users can compute statistics from the
  meteorological data of the whole catchment instead of the local
  sub-basin data of the node
  ([\#178](https://forge.inrae.fr/airgriwrm/airgriwrm/-/issues/178)).

### Changes:

- The minimum required version of ‘airGR’ has been raised to 1.7.8:
  `CreateRunOptions` reads the per-node `FeatFUN_MOD` attribute for
  every node, and Reservoir nodes require the fix of airGR issue
  [\#167](https://forge.inrae.fr/airgriwrm/airgriwrm/-/issues/167) (both
  provided by the airGR 1.7.8 CRAN release, 2025-12-12). Users on airGR
  \<= 1.7.6 now get a clear dependency error at installation instead of
  a runtime crash in
  [`CreateRunOptions()`](https://inrae.github.io/airGRiwrm/dev/reference/CreateRunOptions.md).

### Bug fixes:

- RunModel: an explicit `Outputs_Sim` selection now works on
  semi-distributed networks: the outputs required to propagate the
  simulated flows between nodes (in particular `WarmUpQsim` when the
  warm-up period is non-zero) are now included automatically in an
  explicit `Outputs_Sim` selection, instead of failing with the error
  “Time series WarmUpQsim in ‘QcontribDown’ should have the same length
  as ‘RunOptions\$IndPeriod_WarmUp’”
  ([\#98](https://forge.inrae.fr/airgriwrm/airgriwrm/-/issues/98)).

## airGRiwrm 0.7.0

CRAN release: 2024-09-22

### Breaking changes:

- CreateSupervisor \[Breaking change\]: Only allow to apply a command on
  DirectInjection and Diversion nodes
  ([\#101](https://forge.inrae.fr/airgriwrm/airgriwrm/-/issues/101))
- CreateInputsModel \[Breaking change\]: do not allow to use `Qobs` on
  nodes other than Direct Injection and Diversion
  ([\#99](https://forge.inrae.fr/airgriwrm/airgriwrm/-/issues/99))

### New features:

- Handle simple in-line reservoir inside the model
  ([\#51](https://forge.inrae.fr/airgriwrm/airgriwrm/-/issues/51))
- Use of non gauged stations in the network
  ([\#42](https://forge.inrae.fr/airgriwrm/airgriwrm/-/issues/42))
- RunModel.Supervision: handle NA values on injected flows
  ([\#94](https://forge.inrae.fr/airgriwrm/airgriwrm/-/issues/94))
- Add checks on Supervisor functions
  ([\#102](https://forge.inrae.fr/airgriwrm/airgriwrm/-/issues/102))
- Get `plot.Qm3s` available as a function
  ([\#104](https://forge.inrae.fr/airgriwrm/airgriwrm/-/issues/104))
- Implementation of “Diversion” node
  ([\#95](https://forge.inrae.fr/airgriwrm/airgriwrm/-/issues/95))
- CreateInputsModel: downgrade error on “`Qobs` only for nodes Direct
  Injection and Diversion” to warning
  ([\#109](https://forge.inrae.fr/airgriwrm/airgriwrm/-/issues/109))
- Diversion: Remove error “The downstream node of a Diversion node must
  be different than the downstream node of the node is attached to”
  ([\#111](https://forge.inrae.fr/airgriwrm/airgriwrm/-/issues/111))
- Ungauged node: allow to define donor manually
  ([\#129](https://forge.inrae.fr/airgriwrm/airgriwrm/-/issues/129))
- Ungauged node - CreateGRiwrm: Search donor through Diversion if not
  available by natural network
  ([\#132](https://forge.inrae.fr/airgriwrm/airgriwrm/-/issues/132))
- Highlight the water deficit at a node due to too much withdrawals
  ([\#144](https://forge.inrae.fr/airgriwrm/airgriwrm/-/issues/144))
- Integration of reservoirs as nodes in the network
  ([\#90](https://forge.inrae.fr/airgriwrm/airgriwrm/-/issues/90))
- Handle direct withdrawal in reservoirs
  ([\#147](https://forge.inrae.fr/airgriwrm/airgriwrm/-/issues/147))
- plot.GRiwrm: use web service instead of DiagrammeR::mermaid
  ([\#150](https://forge.inrae.fr/airgriwrm/airgriwrm/-/issues/150))
- plot.GRiwrm: sketch ungauged cluster with subgraphs
  ([\#151](https://forge.inrae.fr/airgriwrm/airgriwrm/-/issues/151))
- Implementation of non gauged station with donor other than a
  downstream gauged station
  ([\#92](https://forge.inrae.fr/airgriwrm/airgriwrm/-/issues/92))
- CreateInputsCrit: allow a priori node not only at upstream
  ([\#156](https://forge.inrae.fr/airgriwrm/airgriwrm/-/issues/156))
- Allow Diversion on Reservoir
  ([\#146](https://forge.inrae.fr/airgriwrm/airgriwrm/-/issues/146))
- CreateInputsModel: deprecate `Qobs` parameter and use `Qinf` instead
  ([\#120](https://forge.inrae.fr/airgriwrm/airgriwrm/-/issues/120))
- CreateInputsModel: Specify the error message: “‘Qobs’ column names
  must be included in ’id’s of the GRiwrm object”
  ([\#152](https://forge.inrae.fr/airgriwrm/airgriwrm/-/issues/152))
- `plot.GRiwrmOutputsModel`: handle other units than mm / time step
  ([\#105](https://forge.inrae.fr/airgriwrm/airgriwrm/-/issues/105))
- Feature request: function for getting parameters from
  `GRiwrmOutputsCalib`
  ([\#86](https://forge.inrae.fr/airgriwrm/airgriwrm/-/issues/86))
- Speed up RunModel_Lag for RunModel.Supervisor
  ([\#164](https://forge.inrae.fr/airgriwrm/airgriwrm/-/issues/164))
- plot.GRiwrm: allow to use optional parameters of `mermaid` function
  ([\#160](https://forge.inrae.fr/airgriwrm/airgriwrm/-/issues/160))
- Ungauged node cluster: improve outsider gauged node detection
  ([\#168](https://forge.inrae.fr/airgriwrm/airgriwrm/-/issues/168))

### Bug fixes:

- Wrong file name for the “Get started” vignette
  ([\#79](https://forge.inrae.fr/airgriwrm/airgriwrm/-/issues/79))
- CreateInputsCrit: wrong id in error message
  ([\#85](https://forge.inrae.fr/airgriwrm/airgriwrm/-/issues/85))
- ConvertMeteoSD: crash with upstream nodes with no area
  ([\#89](https://forge.inrae.fr/airgriwrm/airgriwrm/-/issues/89))
- Regularisation: check if current node and a priori node use the same
  model ([\#93](https://forge.inrae.fr/airgriwrm/airgriwrm/-/issues/93))
- Regularisation: taking into account X4 transformation
  ([\#88](https://forge.inrae.fr/airgriwrm/airgriwrm/-/issues/88))
- RunModel.Supervisor doesn’t work with Diversion node
  ([\#106](https://forge.inrae.fr/airgriwrm/airgriwrm/-/issues/106))
- Allow to use node with model RunModel_Lag
  ([\#107](https://forge.inrae.fr/airgriwrm/airgriwrm/-/issues/107))
- Calibration does not work when upstream catchments are both gauged and
  ungauged
  ([\#108](https://forge.inrae.fr/airgriwrm/airgriwrm/-/issues/108))
- Ungauged node: incorrect definition of donor with Reservoir and bug
  with Diversion nodes
  ([\#110](https://forge.inrae.fr/airgriwrm/airgriwrm/-/issues/110))
- Ungauged node: crash with upstream Diversion node
  ([\#113](https://forge.inrae.fr/airgriwrm/airgriwrm/-/issues/113))
- Ungauged node: crash on diversion to node outside the sub-network
  ([\#112](https://forge.inrae.fr/airgriwrm/airgriwrm/-/issues/112))
- Ungauged node: difference of ErrorCrit between Calibration and
  RunModel
  ([\#115](https://forge.inrae.fr/airgriwrm/airgriwrm/-/issues/115))
- getSD_Ids crashes on upstream nodes with Diversion
  ([\#116](https://forge.inrae.fr/airgriwrm/airgriwrm/-/issues/116))
- CreateSupervisor: wrong definition of allowed nodes for command
  ([\#117](https://forge.inrae.fr/airgriwrm/airgriwrm/-/issues/117))
- Calibration: Diversion is not handled on upstream nodes
  ([\#122](https://forge.inrae.fr/airgriwrm/airgriwrm/-/issues/122))
- Calibration: crash with ungauged node and multiple Diversions
  ([\#123](https://forge.inrae.fr/airgriwrm/airgriwrm/-/issues/123))
- CreateInputsModel: wrong area of sub-basin with upstream Reservoir,
  Lag or Direct Injection nodes
  ([\#124](https://forge.inrae.fr/airgriwrm/airgriwrm/-/issues/124))
- RunModel.Supervisor: error with Diversion node
  ([\#126](https://forge.inrae.fr/airgriwrm/airgriwrm/-/issues/126))
- Ungauged node: Diversions are not handled correctly in Calibration
  ([\#127](https://forge.inrae.fr/airgriwrm/airgriwrm/-/issues/127))
- Calibration: crash with a diverted ungauged node
  ([\#128](https://forge.inrae.fr/airgriwrm/airgriwrm/-/issues/128))
- Ungauged nodes: X4 transformation not handled with CemaNeige models
  ([\#135](https://forge.inrae.fr/airgriwrm/airgriwrm/-/issues/135))
- Crash with Hysteresis in CemaNeige
  ([\#134](https://forge.inrae.fr/airgriwrm/airgriwrm/-/issues/134))
- Ungauged nodes: crash with a reservoir and several upstream nodes
  ([\#136](https://forge.inrae.fr/airgriwrm/airgriwrm/-/issues/136))
- Ungauged node: Diversion to Reservoir crashes Calibration
  ([\#130](https://forge.inrae.fr/airgriwrm/airgriwrm/-/issues/130))
- Wrong calibration node order with multiple ungauged node clusters
  ([\#149](https://forge.inrae.fr/airgriwrm/airgriwrm/-/issues/149))
- CreateGRiwrm: several Diversions on the same node do not raise error
  ([\#125](https://forge.inrae.fr/airgriwrm/airgriwrm/-/issues/125))
- Reservoir output plot color palette issue
  ([\#154](https://forge.inrae.fr/airgriwrm/airgriwrm/-/issues/154))
- Wrong sorting for calibration of ungauged nodes
  ([\#155](https://forge.inrae.fr/airgriwrm/airgriwrm/-/issues/155))
- Donor defined on gauged model node turns the node into ungauged at
  Calibration
  ([\#157](https://forge.inrae.fr/airgriwrm/airgriwrm/-/issues/157))
- Calibration: crash when transferring from upstream donor to upstream
  receiver
  ([\#158](https://forge.inrae.fr/airgriwrm/airgriwrm/-/issues/158))
- plot.GRiwrm crashes with a single node
  ([\#153](https://forge.inrae.fr/airgriwrm/airgriwrm/-/issues/153))
- CreateInputsModel: Don’t allow ungauged donor
  ([\#131](https://forge.inrae.fr/airgriwrm/airgriwrm/-/issues/131))
- Rename internal function `RunModel.SD`
  ([\#166](https://forge.inrae.fr/airgriwrm/airgriwrm/-/issues/166))
- Calibration: crash with downstream receiver node with upstream
  reservoir
  ([\#167](https://forge.inrae.fr/airgriwrm/airgriwrm/-/issues/167))

### Documentation:

- Improve Supervisor documentation
  ([\#100](https://forge.inrae.fr/airgriwrm/airgriwrm/-/issues/100))
- Update package citation reference
  ([\#119](https://forge.inrae.fr/airgriwrm/airgriwrm/-/issues/119))
- Add a main page in the package documentation
  ([\#77](https://forge.inrae.fr/airgriwrm/airgriwrm/-/issues/77))
- Improve RunModel documentation
  ([\#133](https://forge.inrae.fr/airgriwrm/airgriwrm/-/issues/133))

### Internal changes:

- Check failed on release version
  ([\#78](https://forge.inrae.fr/airgriwrm/airgriwrm/-/issues/78))
- Transfer documentation to github pages
  ([\#148](https://forge.inrae.fr/airgriwrm/airgriwrm/-/issues/148))
- Publish documentation package on github with github workflow
  ([\#162](https://forge.inrae.fr/airgriwrm/airgriwrm/-/issues/162))
- CI: add check with version “dev” of airGR
  ([\#84](https://forge.inrae.fr/airgriwrm/airgriwrm/-/issues/84))

## airGRiwrm 0.6.2

CRAN release: 2023-10-24

- airGR changes cause failed check
  ([\#139](https://forge.inrae.fr/airgriwrm/airgriwrm/-/issues/139))

## airGRiwrm 0.6.1

CRAN release: 2022-03-08

### Changes:

- Simplify the use of airGR in airGRiwrm
  ([\#63](https://forge.inrae.fr/airgriwrm/airgriwrm/-/issues/63))
- `CreateInputsModel`: Make `Qobs` parameter optional
  ([\#60](https://forge.inrae.fr/airgriwrm/airgriwrm/-/issues/60))
- airGR compatibility: change on LengthHydro unit
  ([\#32](https://forge.inrae.fr/airgriwrm/airgriwrm/-/issues/32))
- `CreateInputsCrit`: Change obs parameter characteristics
  ([\#38](https://forge.inrae.fr/airgriwrm/airgriwrm/-/issues/38))
- Update URLs in the DESCRIPTION file
  ([\#45](https://forge.inrae.fr/airgriwrm/airgriwrm/-/issues/45))
- Use S3 plot method for GRiwrm class objects
  ([\#26](https://forge.inrae.fr/airgriwrm/airgriwrm/-/issues/26))
- Rename function GRiwrm to CreateGRiwrm
  ([\#46](https://forge.inrae.fr/airgriwrm/airgriwrm/-/issues/46))
- `CreateInputsCrit`: `transfo` is mandatory for parameter
  regularization
  ([\#56](https://forge.inrae.fr/airgriwrm/airgriwrm/-/issues/56))

### New features:

- `plot.Qm3s`: customize legend position
  ([\#75](https://forge.inrae.fr/airgriwrm/airgriwrm/-/issues/75))
- Regularisation: Add default value for parameter Celerity
  ([\#58](https://forge.inrae.fr/airgriwrm/airgriwrm/-/issues/58))
- Add network consistency checks in `GRiwrm`
  ([\#36](https://forge.inrae.fr/airgriwrm/airgriwrm/-/issues/36))
- Handle CemaNeige compatibility
  ([\#52](https://forge.inrae.fr/airgriwrm/airgriwrm/-/issues/52))
- Use S3 plot method for GRiwrmOutputsModel class objects
  ([\#26](https://forge.inrae.fr/airgriwrm/airgriwrm/-/issues/26))
- Handling correctly initial conditions
  ([\#48](https://forge.inrae.fr/airgriwrm/airgriwrm/-/issues/48))
- Calibration with parameter regularization
  ([\#54](https://forge.inrae.fr/airgriwrm/airgriwrm/-/issues/54))

### Bug fixes:

- Results differences between versions on vignette V04
  ([\#75](https://forge.inrae.fr/airgriwrm/airgriwrm/-/issues/75))
- `plot.GRiwrm` not working in gitlab-ci
  ([\#74](https://forge.inrae.fr/airgriwrm/airgriwrm/-/issues/74))
- `plot.GRiwrm`: mermaid code is displayed with the diagram
  ([\#73](https://forge.inrae.fr/airgriwrm/airgriwrm/-/issues/73))
- `CreateGRiwrm` crashes when keeping all columns and rename some
  ([\#64](https://forge.inrae.fr/airgriwrm/airgriwrm/-/issues/64))
- Breaking change in airGR in issue HYCAR-Hydro/airgr#137
  ([\#62](https://forge.inrae.fr/airgriwrm/airgriwrm/-/issues/62))
- Review documentation for publication on CRAN
  ([\#43](https://forge.inrae.fr/airgriwrm/airgriwrm/-/issues/43))
- Vignettes: working directory instability
  ([\#35](https://forge.inrae.fr/airgriwrm/airgriwrm/-/issues/35))
- airGR compatibility: debugged version of RunModel_Lag
  ([\#33](https://forge.inrae.fr/airgriwrm/airgriwrm/-/issues/33))
- `CreateInputsModel`: Error when using data.frame for Qobs
  ([\#37](https://forge.inrae.fr/airgriwrm/airgriwrm/-/issues/37))
- `RunModel.Supervisor`: Error in ctrlr$`U[seq.int(length(sv`$ts.index),
  i\] ([\#39](https://forge.inrae.fr/airgriwrm/airgriwrm/-/issues/39))
- Supervisor: measurement on network downstream node returns `NULL`
  ([\#40](https://forge.inrae.fr/airgriwrm/airgriwrm/-/issues/40))
- `RunModel`: Suspected bug on `OutputsModel$Qsim` in the training
  example
  ([\#41](https://forge.inrae.fr/airgriwrm/airgriwrm/-/issues/41))
- Test fail after airGR update on outputting warm-up Qsim
  ([\#50](https://forge.inrae.fr/airgriwrm/airgriwrm/-/issues/50))
- Wrong Qobs use in Lavenne function criteria
  ([\#57](https://forge.inrae.fr/airgriwrm/airgriwrm/-/issues/57))

### Internal changes:

- Prepare the package for v0.6.x CRAN submission
  ([\#71](https://forge.inrae.fr/airgriwrm/airgriwrm/-/issues/71))
- Clone on github master and dev branches of the repository
  ([\#68](https://forge.inrae.fr/airgriwrm/airgriwrm/-/issues/68))
- Update airGR dependency to CRAN v1.7.0
  ([\#69](https://forge.inrae.fr/airgriwrm/airgriwrm/-/issues/69))
- Add an airGR galaxy tab on the website?
  ([\#49](https://forge.inrae.fr/airgriwrm/airgriwrm/-/issues/49))
- CI: dependency issues with Latex in Check as CRAN
  ([\#53](https://forge.inrae.fr/airgriwrm/airgriwrm/-/issues/53))
- Remove dependency to R \> 3.5
  ([\#59](https://forge.inrae.fr/airgriwrm/airgriwrm/-/issues/59))
- pkgdown: wrong documentation for methods CreateRunOptions and
  CreateCalibOptions
  ([\#65](https://forge.inrae.fr/airgriwrm/airgriwrm/-/issues/65))
- Review documentation for publication on CRAN
  ([\#43](https://forge.inrae.fr/airgriwrm/airgriwrm/-/issues/43))
- Push roxygen outputs on the repository
  ([\#34](https://forge.inrae.fr/airgriwrm/airgriwrm/-/issues/34))
- Generation of the <https://airgriwrm.g-eau.net> site documentation
  ([\#44](https://forge.inrae.fr/airgriwrm/airgriwrm/-/issues/44))
- Automatically update website from package repository
  ([\#47](https://forge.inrae.fr/airgriwrm/airgriwrm/-/issues/47))

## airGRiwrm 0.5.0 (Release date: 2021-03-07)

### New features:

- Feedback control
  ([\#19](https://forge.inrae.fr/airgriwrm/airgriwrm/-/issues/19))
- RunModel of GRiwrm networks: add a data.frame of simulated flows in
  OutputsModel
  ([\#30](https://forge.inrae.fr/airgriwrm/airgriwrm/-/issues/30))
- Plot simulated flows of all the nodes in m3/s
  ([\#31](https://forge.inrae.fr/airgriwrm/airgriwrm/-/issues/31))

### Changes:

- RunModel: Uncoupling of hydrological and hydraulic models
  ([\#28](https://forge.inrae.fr/airgriwrm/airgriwrm/-/issues/28))

## airGRiwrm 0.4.0 (Release date: 2020-12-28)

### New features:

- Convert basin meteorological data to sub-basin level
  ([\#21](https://forge.inrae.fr/airgriwrm/airgriwrm/-/issues/21))

### Changes:

- Clarify dependency with ‘DiagrammeR’ package
  ([\#24](https://forge.inrae.fr/airgriwrm/airgriwrm/-/issues/24))

### Minor changes:

- Replace vignette examples on Seine River by a fake example from data
  provided by airGR
  ([\#13](https://forge.inrae.fr/airgriwrm/airgriwrm/-/issues/13))

### Bug fixes:

- Impossibility to inject flow associated to an area
  ([\#23](https://forge.inrae.fr/airgriwrm/airgriwrm/-/issues/23))
- Error in the area used for the sub basins
  ([\#22](https://forge.inrae.fr/airgriwrm/airgriwrm/-/issues/22))

## airGRiwrm 0.3.1 (Release date: 2020-08-07)

### New features:

- Calibration of influenced semi-distributed model
  ([\#11](https://forge.inrae.fr/airgriwrm/airgriwrm/-/issues/11))

## airGRiwrm 0.3.0 (Release date: 2020-08-07)

### New features:

- Add node of type “direct flow” in order to inject or withdraw flows
  into the model
  ([\#5](https://forge.inrae.fr/airgriwrm/airgriwrm/-/issues/5))

## airGRiwrm 0.2.1 (Release date: 2020-06-11)

### Changes:

- Remove Gits class object and use CreateInputsModel directly
  ([\#7](https://forge.inrae.fr/airgriwrm/airgriwrm/-/issues/7))
- Remove Girop class object and integrate hydrological model and area in
  Ginet ([\#9](https://forge.inrae.fr/airgriwrm/airgriwrm/-/issues/9))
- Rename “Ginet” class object to “Griwrm”
  ([\#10](https://forge.inrae.fr/airgriwrm/airgriwrm/-/issues/10))

## airGRiwrm 0.2.0 (Release date: 2020-06-06)

### New features:

- Calibration of semi-distributed model
  ([\#3](https://forge.inrae.fr/airgriwrm/airgriwrm/-/issues/3))

## airGRiwrm 0.1.0 (Release date: 2020-05-25)

### New features:

- Database structuring
  ([\#1](https://forge.inrae.fr/airgriwrm/airgriwrm/-/issues/1))
- Scheduling airGR model runs
  ([\#2](https://forge.inrae.fr/airgriwrm/airgriwrm/-/issues/2))
