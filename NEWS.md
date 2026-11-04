# airGRiwrm 0.8.0

Breaking changes:
-----------------

* CreateRunOptions [Breaking change]: The default value of `Outputs_Sim` has
  changed from `"all"` to a minimal set
  (`c("DatesR", "WarmUpQsim", "Qsim", "Qsim_m3", "QsimDown", "Param", "StateEnd")`)
  in order to reduce the size of the GRiwrmOutputsModel object for large
  networks. Use `Outputs_Sim = "all"` to restore the previous behaviour (#141).
* Calibration [Breaking change]: `Calibration` on a simple *InputsModel*
  now requires a `RunOptions` argument, inserted in the second position
  (`Calibration(InputsModel, RunOptions, CalibOptions, ...)`). Positional
  calls written as `Calibration(InputsModel, CalibOptions)` are broken;
  named calls and the calibration of network models (GRiwrmInputsModel)
  are unchanged (#199).
* CreateInputsCrit [Breaking change]: the default error criterion changed
  from `ErrorCrit_NSE` to `ErrorCrit_KGE2` and the default `AprioriIds`
  changed from `NULL` to a computed a priori node
  (`getDefaultAprioriIds(InputsModel)`); several a priori nodes are now
  tested and the best one is kept. The numeric results of the default
  criteria and of the calibration change (#197, #196).
* CreateInputsCrit [Breaking change]: the De Lavenne criterion now raises
  an error if the X4 parameter of the a priori node is below 0.5, and the
  a priori error criterion is now computed from the current sub-catchment
  error criterion; the values of `InputsCritLavenne` and the calibration
  results change accordingly (#193).
* Calibration [Breaking change]: reservoirs now follow the
  force-release/diversion logic during calibration by default
  (`forceReservoirObs = TRUE` in `RunModel`/`Calibration` of a
  GRiwrmInputsModel, with a calibration flag in the reservoir RunOptions);
  the simulated reservoir behaviour during calibration can change (#199).

New features:
-------------

* CreateRunOptions: allow users to select which simulation outputs are returned
  via the `Outputs_Sim` argument, either as a character vector applied to all
  nodes, or as a named list with per-node selection (#141). CemaNeige nodes
  now also carry `Precip` in their default `Outputs_Sim`: the default plot of
  a CemaNeige node requires it in the *OutputsModel* to compute the
  precipitation panel axis as soon as the per-layer solid precipitation
  (`Psol`) is present (upstream airGR `ylimP` limitation; the standalone
  airGR bug is still to be fixed on the airGR side). `Precip` is a recorded
  meteo input, not a CemaNeige Fortran output: the CemaNeige `NOutputs` and
  the model run are unchanged (#178).
* CreateInputsCrit: allow user-defined a priori parameters for the De Lavenne
  criterion: entries of `AprioriIds` can now be numeric vectors (one value per
  model parameter of the node, e.g. `c(celerity, X1, X2, X3, X4)`) used as the
  a priori parameters instead of the parameters of an upstream sub-catchment
  (#61).
* plot.GRiwrmOutputsModel: display the meteorological time series
  (precipitation and potential evapotranspiration) at the whole-catchment
  scale: when available in the *GRiwrmOutputsModel* object, the time series
  of each node are replaced on the fly by their area-weighted mean over the
  catchment of the node and all its upstream nodes, so that the displayed
  meteorology reflects the whole catchment integrated in the simulated
  discharge (#178). For a CemaNeige node, the per-layer series displayed by
  `airGR::plot.OutputsModel` are covered as well: the mean air temperature
  (`Temp`), the solid precipitation (`Psol`) and the snow pack state
  (`SnowPack`) of each elevation layer are replaced by their whole-catchment
  mean, as well as the mean over the layers that airGR derives from them.
  The same adjustment is available as a new exported function
  `adjustMeteoToCatchment()`: it computes, for a node of a
  *GRiwrmOutputsModel* object, the meteorological time series at the
  whole-catchment scale (including the CemaNeige layer series), so that
  users can compute statistics from the meteorological data of the whole
  catchment instead of the local sub-basin data of the node (#178).
* RunModel: a new S3 method `RunModel.GRiwrmOutputsModel` re-simulates a
  model from a previous *GRiwrmOutputsModel* object (with modified
  inputs), and a previous *OutputsModel* can now be merged with a new one
  (`merge.OutputsModel`, `merge.GRiwrmOutputsModel`); the new exported
  function `getNextTimeSteps()` helps to select the time steps of the
  re-simulation (#173).
* RunModel.Supervisor: a new `Yinit` argument sets the initial Y state for
  a hot restart; the Yinit can be computed from a previous
  *GRiwrmOutputsModel* (the warm-up outputs are kept in the object and feed
  the supervised run) (#208, #209). The initial Y state is now applied at
  every restart of the supervision (#210).
* CreateInputsModel: a new `FUN_REGUL` argument allows providing a
  user-defined regulation function ("local rule") for a node; the function
  is called with an extra named argument `e = environment()` (contract to
  document on the user side) (#29).
* CreateController: the variable to control/measure can now be selected at
  each node location (reservoir volume, derived flows as controlled/measured
  variables), and the `Y` and `U` locations accept `NULL` (#188, #174).
* CreateInputsModel: `ZInputs` now accepts a list (coerced to a numeric
  vector) (#206).
* RunModel_Reservoir: pass-through mode ("transparent" reservoir) with
  `Qrelease = NA` and `Vsim = 0` (#201).
* CreateInputsCrit: a new exported function `getDefaultAprioriIds(InputsModel)`
  returns the default a priori node ids (#197).
* CreateGRiwrm: gauged nodes inside an ungauged cluster are now detected
  (#168).
* plot.OutputsModelReservoir and plot.GRiwrmOutputsModel: the observed
  volume series (`Vobs`) is now displayed in the plots (#189).
* plot.GRiwrm: a new `server` argument and the `MERMAID_URL` environment
  variable provide an alternative to the mermaid.ink service (#184).

Changes:
--------

* The minimum required version of 'airGR' has been raised to 1.7.8:
  `CreateRunOptions` reads the per-node `FeatFUN_MOD` attribute for every
  node, and Reservoir nodes require the fix of airGR issue #167 (both
  provided by the airGR 1.7.8 CRAN release, 2025-12-12). Users on
  airGR <= 1.7.6 now get a clear dependency error at installation
  instead of a runtime crash in `CreateRunOptions()`.
* CreateRunOptions: the initial states (`IniStates`) are now taken into
  account and checked for Reservoir nodes (previously ignored); new
  errors/warnings may be raised on configurations that were previously
  accepted (#182).
* Calibration: the calibration of ungauged clusters now takes the
  `IniStates` of the involved nodes into account, and a new error is
  raised if a Reservoir node is in an ungauged cluster (#182).

Bug fixes:
----------

* RunModel: an explicit `Outputs_Sim` selection now works on
  semi-distributed networks: the outputs required to propagate the simulated
  flows between nodes (in particular `WarmUpQsim` when the warm-up period is
  non-zero) are now included automatically in an explicit `Outputs_Sim`
  selection, instead of failing with the error "Time series WarmUpQsim in
  'QcontribDown' should have the same length as
  'RunOptions$IndPeriod_WarmUp'" (#98).
* RunModel.Supervisor: crash with `RunModel_Lag` on a node (#191).
* RunModel.Supervisor: crash with Reservoir + Diversion nodes (#190).
* RunModel.Supervisor: wrong detection of the warm-up period (#174).
* RunModel.Supervision: crash at the hot restart (#176).
* RunModel.Supervision: missing attributes (class, `DatesR`) for GR and
  `OutputsModelReservoir` objects (#174, #177).
* RunModel.Supervision: wrong retrieval of the data for the direct
  injections (#211).
* RunModel: `NA` handling in `IniStates` (#174).
* RunModel.GRiwrmOutputsModel: the original warm-up data are now kept in
  the OutputsModel (#179).
* RunModel.OutputsModel: sign error of the Diversion flow (#173).
* RunModel_Lag (routing): crash at the `StateEnd` computation on very short
  simulations without a warm-up period (#192).
* CreateInputsCrit: the De Lavenne criterion now updates the upstream
  discharges before computing the criterion (#193).
* Calibration: `RunModel_ungauged` is now allowed for the a priori
  criterion of an ungauged cluster (#193).
* getDefaultAprioriIds: unique node ids guaranteed and bypass on reservoirs
  (#196).
* transferGRparams: wrong computation of the X4 parameter transferred from
  an upstream donor node; the transferred parameter values in ungauged
  cluster calibrations change accordingly (#205).
* CreateCalibOptions: crash when all parameters are fixed (#183).
* Ungauged clusters: fixed parameters in a cluster, a gauged node in a
  cluster without fixed parameters, the donor search with Diversion,
  receivers removed from the cluster, and the gauged node detection were
  fixed (#168, #167).
* CreateGRiwrm: wrong rank of nodes with an ungauged cluster and reservoirs
  (#168).
* CreateController: direct injection in `Y` with `var = "Qupstream"` and
  the case of a missing `Y` location (#188).
* CreateInputsModel: `PrecipScale` is now defined at the node level (#187)
  and `Qrelease` now accepts a matrix (#200).
* plot.OutputsModelReservoir: the `which` argument is now handled when the
  plot is called from `box` (#203).

Documentation:
--------------

* New vignette V07: example combining tactical and operational management
  (PID controller on a reservoir, `RunModel.GRiwrmOutputsModel`) (#180).
* Vignette V04: reformatted for readability and the NA handling in the
  controller measurements documented (#97).
* Documentation: the irstea.fr links replaced by the corresponding
  forge.inrae.fr pages, the irrigation and reservoir vignette examples
  revised, and the `CreateGRiwrm` documentation updated (#204).
* A documentation wave covered `Calibration`, `CreateCalibOptions`,
  `CreateInputsModel`, `RunModel.Supervisor`, `RunModel_Reservoir`
  (examples, target/observed release, PID supervisor examples),
  `transferGRparams` and `extractParam` (#172, #189).

# airGRiwrm 0.7.0

Breaking changes:
-----------------

* CreateSupervisor [Breaking change]: Only allow to apply a command on DirectInjection and Diversion nodes (#101)
* CreateInputsModel [Breaking change]: do not allow to use `Qobs` on nodes other than Direct Injection and Diversion (#99)

New features:
-------------

* Handle simple in-line reservoir inside the model (#51)
* Use of non gauged stations in the network (#42)
* RunModel.Supervision: handle NA values on injected flows (#94)
* Add checks on Supervisor functions (#102)
* Get `plot.Qm3s` available as a function (#104)
* Implementation of "Diversion" node (#95)
* CreateInputsModel: downgrade error on "`Qobs` only for nodes Direct Injection and Diversion" to warning (#109)
* Diversion: Remove error "The downstream node of a Diversion node must be different than the downstream node of the node is attached to" (#111)
* Ungauged node: allow to define donor manually (#129)
* Ungauged node - CreateGRiwrm: Search donor through Diversion if not available by natural network (#132)
* Highlight the water deficit at a node due to too much withdrawals (#144)
* Integration of reservoirs as nodes in the network (#90)
* Handle direct withdrawal in reservoirs (#147)
* plot.GRiwrm: use web service instead of DiagrammeR::mermaid (#150)
* plot.GRiwrm: sketch ungauged cluster with subgraphs (#151)
* Implementation of non gauged station with donor other than a downstream gauged station (#92)
* CreateInputsCrit: allow a priori node not only at upstream (#156)
* Allow Diversion on Reservoir (#146)
* CreateInputsModel: deprecate `Qobs` parameter and use `Qinf` instead (#120)
* CreateInputsModel: Specify the error message: "'Qobs' column names must be included in 'id's of the GRiwrm object" (#152)
* `plot.GRiwrmOutputsModel`: handle other units than mm / time step (#105)
* Feature request: function for getting parameters from `GRiwrmOutputsCalib` (#86)
* Speed up RunModel_Lag for RunModel.Supervisor (#164)
* plot.GRiwrm: allow to use optional parameters of `mermaid` function (#160)
* Ungauged node cluster: improve outsider gauged node detection (#168)

Bug fixes:
----------

* Wrong file name for the "Get started" vignette (#79)
* CreateInputsCrit: wrong id in error message (#85)
* ConvertMeteoSD: crash with upstream nodes with no area (#89)
* Regularisation: check if current node and a priori node use the same model (#93)
* Regularisation: taking into account X4 transformation (#88)
* RunModel.Supervisor doesn't work with Diversion node (#106)
* Allow to use node with model RunModel_Lag (#107)
* Calibration does not work when upstream catchments are both gauged and ungauged (#108)
* Ungauged node: incorrect definition of donor with Reservoir and bug with Diversion nodes (#110)
* Ungauged node: crash with upstream Diversion node (#113)
* Ungauged node: crash on diversion to node outside the sub-network (#112)
* Ungauged node: difference of ErrorCrit between Calibration and RunModel (#115)
* getSD_Ids crashes on upstream nodes with Diversion (#116)
* CreateSupervisor: wrong definition of allowed nodes for command (#117)
* Calibration: Diversion is not handled on upstream nodes (#122)
* Calibration: crash with ungauged node and multiple Diversions (#123)
* CreateInputsModel: wrong area of sub-basin with upstream Reservoir, Lag or Direct Injection nodes (#124)
* RunModel.Supervisor: error with Diversion node (#126)
* Ungauged node: Diversions are not handled correctly in Calibration (#127)
* Calibration: crash with a diverted ungauged node (#128)
* Ungauged nodes: X4 transformation not handled with CemaNeige models (#135)
* Crash with Hysteresis in CemaNeige (#134)
* Ungauged nodes: crash with a reservoir and several upstream nodes (#136)
* Ungauged node: Diversion to Reservoir crashes Calibration (#130)
* Wrong calibration node order with multiple ungauged node clusters (#149)
* CreateGRiwrm: several Diversions on the same node do not raise error (#125)
* Reservoir output plot color palette issue (#154)
* Wrong sorting for calibration of ungauged nodes (#155)
* Donor defined on gauged model node turns the node into ungauged at Calibration (#157)
* Calibration: crash when transferring from upstream donor to upstream receiver (#158)
* plot.GRiwrm crashes with a single node (#153)
* CreateInputsModel: Don't allow ungauged donor (#131)
* Rename internal function `RunModel.SD` (#166)
* Calibration: crash with downstream receiver node with upstream reservoir (#167)

Documentation:
--------------

* Improve Supervisor documentation (#100)
* Update package citation reference (#119)
* Add a main page in the package documentation (#77)
* Improve RunModel documentation (#133)

Internal changes:
-----------------

* Check failed on release version (#78)
* Transfer documentation to github pages (#148)
* Publish documentation package on github with github workflow (#162)
* CI: add check with version "dev" of airGR (#84)


# airGRiwrm 0.6.2

* airGR changes cause failed check (#139)


# airGRiwrm 0.6.1

Changes:
--------

* Simplify the use of airGR in airGRiwrm (#63)
* `CreateInputsModel`: Make `Qobs` parameter optional (#60)
* airGR compatibility: change on LengthHydro unit (#32)
* `CreateInputsCrit`: Change obs parameter characteristics (#38)
* Update URLs in the DESCRIPTION file (#45)
* Use S3 plot method for GRiwrm class objects (#26)
* Rename function GRiwrm to CreateGRiwrm (#46)
* `CreateInputsCrit`: `transfo` is mandatory for parameter regularization (#56)

New features:
-------------

* `plot.Qm3s`: customize legend position (#75)
* Regularisation: Add default value for parameter Celerity (#58)
* Add network consistency checks in `GRiwrm` (#36)
* Handle CemaNeige compatibility (#52)
* Use S3 plot method for GRiwrmOutputsModel class objects (#26)
* Handling correctly initial conditions (#48)
* Calibration with parameter regularization (#54)

Bug fixes:
----------

* Results differences between versions on vignette V04 (#75)
* `plot.GRiwrm` not working in gitlab-ci (#74)
* `plot.GRiwrm`: mermaid code is displayed with the diagram (#73)
* `CreateGRiwrm` crashes when keeping all columns and rename some (#64)
* Breaking change in airGR in issue HYCAR-Hydro/airgr#137 (#62)
* Review documentation for publication on CRAN (#43)
* Vignettes: working directory instability (#35)
* airGR compatibility: debugged version of RunModel_Lag (#33)
* `CreateInputsModel`: Error when using data.frame for Qobs (#37)
* `RunModel.Supervisor`: Error in ctrlr$U[seq.int(length(sv$ts.index), i] (#39)
* Supervisor: measurement on network downstream node returns `NULL` (#40)
* `RunModel`: Suspected bug on `OutputsModel$Qsim` in the training example (#41)
* Test fail after airGR update on outputting warm-up Qsim (#50)
* Wrong Qobs use in Lavenne function criteria (#57)

Internal changes:
-----------------

* Prepare the package for v0.6.x CRAN submission (#71)
* Clone on github master and dev branches of the repository (#68)
* Update airGR dependency to CRAN v1.7.0 (#69)
* Add an airGR galaxy tab on the website? (#49)
* CI: dependency issues with Latex in Check as CRAN (#53)
* Remove dependency to R > 3.5 (#59)
* pkgdown: wrong documentation for methods CreateRunOptions and CreateCalibOptions (#65)
* Review documentation for publication on CRAN (#43)
* Push roxygen outputs on the repository (#34)
* Generation of the https://airgriwrm.g-eau.net site documentation (#44)
* Automatically update website from package repository (#47)


# airGRiwrm 0.5.0 (Release date: 2021-03-07)

New features:
-------------

* Feedback control (#19)
* RunModel of GRiwrm networks: add a data.frame of simulated flows in OutputsModel (#30)
* Plot simulated flows of all the nodes in m3/s (#31)

Changes:
--------

* RunModel: Uncoupling of hydrological and hydraulic models (#28)


# airGRiwrm 0.4.0 (Release date: 2020-12-28)

New features:
-------------

* Convert basin meteorological data to sub-basin level (#21)

Changes:
--------

* Clarify dependency with 'DiagrammeR' package (#24)

Minor changes:
--------------

* Replace vignette examples on Seine River by a fake example from data provided by airGR (#13)

Bug fixes:
----------

* Impossibility to inject flow associated to an area (#23)
* Error in the area used for the sub basins (#22)


# airGRiwrm 0.3.1 (Release date: 2020-08-07)

New features:
-------------

* Calibration of influenced semi-distributed model (#11)


# airGRiwrm 0.3.0 (Release date: 2020-08-07)

New features:
-------------

* Add node of type "direct flow" in order to inject or withdraw flows into the model (#5)


# airGRiwrm 0.2.1 (Release date: 2020-06-11)

Changes:
--------

* Remove Gits class object and use CreateInputsModel directly (#7)
* Remove Girop class object and integrate hydrological model and area in Ginet (#9)
* Rename "Ginet" class object to "Griwrm" (#10)


# airGRiwrm 0.2.0 (Release date: 2020-06-06)

New features:
-------------

* Calibration of semi-distributed model (#3)


# airGRiwrm 0.1.0 (Release date: 2020-05-25)

New features:
-------------

* Database structuring (#1)
* Scheduling airGR model runs (#2)
