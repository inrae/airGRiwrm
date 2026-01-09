# Changelog

## airGRiwrm (development version)

## airGRiwrm 0.7.0

CRAN release: 2024-09-22

### Breaking changes:

- CreateSupervisor \[Breaking change\]: Only allow to apply a command on
  DirectInjection and Diversion nodes
  ([\#101](https://gitlab.irstea.fr/in-wop/airGRiwrm/-/issues/101))
- CreateInputsModel \[Breaking change\]: do not allow to use `Qobs` on
  nodes other than Direct Injection and Diversion
  ([\#99](https://gitlab.irstea.fr/in-wop/airGRiwrm/-/issues/99))

### New features:

- Handle simple in-line reservoir inside the model
  ([\#51](https://gitlab.irstea.fr/in-wop/airGRiwrm/-/issues/51))
- Use of non gauged stations in the network
  ([\#42](https://gitlab.irstea.fr/in-wop/airGRiwrm/-/issues/42))
- RunModel.Supervision: handle NA values on injected flows
  ([\#94](https://gitlab.irstea.fr/in-wop/airGRiwrm/-/issues/94))
- Add checks on Supervisor functions
  ([\#102](https://gitlab.irstea.fr/in-wop/airGRiwrm/-/issues/102))
- Get `plot.Qm3s` available as a function
  ([\#104](https://gitlab.irstea.fr/in-wop/airGRiwrm/-/issues/104))
- Implementation of “Diversion” node
  ([\#95](https://gitlab.irstea.fr/in-wop/airGRiwrm/-/issues/95))
- CreateInputsModel: downgrade error on “`Qobs` only for nodes Direct
  Injection and Diversion” to warning
  ([\#109](https://gitlab.irstea.fr/in-wop/airGRiwrm/-/issues/109))
- Diversion: Remove error “The downstream node of a Diversion node must
  be different than the downstream node of the node is attached to”
  ([\#111](https://gitlab.irstea.fr/in-wop/airGRiwrm/-/issues/111))
- Ungauged node: allow to define donor manually
  ([\#129](https://gitlab.irstea.fr/in-wop/airGRiwrm/-/issues/129))
- Ungauged node - CreateGRiwrm: Search donor through Diversion if not
  available by natural network
  ([\#132](https://gitlab.irstea.fr/in-wop/airGRiwrm/-/issues/132))
- Highlight the water deficit at a node due to too much withdrawals
  ([\#144](https://gitlab.irstea.fr/in-wop/airGRiwrm/-/issues/144))
- Integration of reservoirs as nodes in the network
  ([\#90](https://gitlab.irstea.fr/in-wop/airGRiwrm/-/issues/90))
- Handle direct withdrawal in reservoirs
  ([\#147](https://gitlab.irstea.fr/in-wop/airGRiwrm/-/issues/147))
- plot.GRiwrm: use web service instead of DiagrammeR::mermaid
  ([\#150](https://gitlab.irstea.fr/in-wop/airGRiwrm/-/issues/150))
- plot.GRiwrm: sketch ungauged cluster with subgraphs
  ([\#151](https://gitlab.irstea.fr/in-wop/airGRiwrm/-/issues/151))
- Implementation of non gauged station with donor other than a
  downstream gauged station
  ([\#92](https://gitlab.irstea.fr/in-wop/airGRiwrm/-/issues/92))
- CreateInputsCrit: allow a priori node not only at upstream
  ([\#156](https://gitlab.irstea.fr/in-wop/airGRiwrm/-/issues/156))
- Allow Diversion on Reservoir
  ([\#146](https://gitlab.irstea.fr/in-wop/airGRiwrm/-/issues/146))
- CreateInputsModel: deprecate `Qobs` parameter and use `Qinf` instead
  ([\#120](https://gitlab.irstea.fr/in-wop/airGRiwrm/-/issues/120))
- CreateInputsModel: Specify the error message: “‘Qobs’ column names
  must be included in ’id’s of the GRiwrm object”
  ([\#152](https://gitlab.irstea.fr/in-wop/airGRiwrm/-/issues/152))
- `plot.GRiwrmOutputsModel`: handle other units than mm / time step
  ([\#105](https://gitlab.irstea.fr/in-wop/airGRiwrm/-/issues/105))
- Feature request: function for getting parameters from
  `GRiwrmOutputsCalib`
  ([\#86](https://gitlab.irstea.fr/in-wop/airGRiwrm/-/issues/86))
- Speed up RunModel_Lag for RunModel.Supervisor
  ([\#164](https://gitlab.irstea.fr/in-wop/airGRiwrm/-/issues/164))
- plot.GRiwrm: allow to use optional parameters of `mermaid` function
  ([\#160](https://gitlab.irstea.fr/in-wop/airGRiwrm/-/issues/160))
- Ungauged node cluster: improve outsider gauged node detection
  ([\#168](https://gitlab.irstea.fr/in-wop/airGRiwrm/-/issues/168))

### Bug fixes:

- Wrong file name for the “Get started” vignette
  ([\#79](https://gitlab.irstea.fr/in-wop/airGRiwrm/-/issues/79))
- CreateInputsCrit: wrong id in error message
  ([\#85](https://gitlab.irstea.fr/in-wop/airGRiwrm/-/issues/85))
- ConvertMeteoSD: crash with upstream nodes with no area
  ([\#89](https://gitlab.irstea.fr/in-wop/airGRiwrm/-/issues/89))
- Regularisation: check if current node and a priori node use the same
  model ([\#93](https://gitlab.irstea.fr/in-wop/airGRiwrm/-/issues/93))
- Regularisation: taking into account X4 transformation
  ([\#88](https://gitlab.irstea.fr/in-wop/airGRiwrm/-/issues/88))
- RunModel.Supervisor doesn’t work with Diversion node
  ([\#106](https://gitlab.irstea.fr/in-wop/airGRiwrm/-/issues/106))
- Allow to use node with model RunModel_Lag
  ([\#107](https://gitlab.irstea.fr/in-wop/airGRiwrm/-/issues/107))
- Calibration does not work when upstream catchments are both gauged and
  ungauged
  ([\#108](https://gitlab.irstea.fr/in-wop/airGRiwrm/-/issues/108))
- Ungauged node: incorrect definition of donor with Reservoir and bug
  with Diversion nodes
  ([\#110](https://gitlab.irstea.fr/in-wop/airGRiwrm/-/issues/110))
- Ungauged node: crash with upstream Diversion node
  ([\#113](https://gitlab.irstea.fr/in-wop/airGRiwrm/-/issues/113))
- Ungauged node: crash on diversion to node outside the sub-network
  ([\#112](https://gitlab.irstea.fr/in-wop/airGRiwrm/-/issues/112))
- Ungauged node: difference of ErrorCrit between Calibration and
  RunModel
  ([\#115](https://gitlab.irstea.fr/in-wop/airGRiwrm/-/issues/115))
- getSD_Ids crashes on upstream nodes with Diversion
  ([\#116](https://gitlab.irstea.fr/in-wop/airGRiwrm/-/issues/116))
- CreateSupervisor: wrong definition of allowed nodes for command
  ([\#117](https://gitlab.irstea.fr/in-wop/airGRiwrm/-/issues/117))
- Calibration: Diversion is not handled on upstream nodes
  ([\#122](https://gitlab.irstea.fr/in-wop/airGRiwrm/-/issues/122))
- Calibration: crash with ungauged node and multiple Diversions
  ([\#123](https://gitlab.irstea.fr/in-wop/airGRiwrm/-/issues/123))
- CreateInputsModel: wrong area of sub-basin with upstream Reservoir,
  Lag or Direct Injection nodes
  ([\#124](https://gitlab.irstea.fr/in-wop/airGRiwrm/-/issues/124))
- RunModel.Supervisor: error with Diversion node
  ([\#126](https://gitlab.irstea.fr/in-wop/airGRiwrm/-/issues/126))
- Ungauged node: Diversions are not handled correctly in Calibration
  ([\#127](https://gitlab.irstea.fr/in-wop/airGRiwrm/-/issues/127))
- Calibration: crash with a diverted ungauged node
  ([\#128](https://gitlab.irstea.fr/in-wop/airGRiwrm/-/issues/128))
- Ungauged nodes: X4 transformation not handled with CemaNeige models
  ([\#135](https://gitlab.irstea.fr/in-wop/airGRiwrm/-/issues/135))
- Crash with Hysteresis in CemaNeige
  ([\#134](https://gitlab.irstea.fr/in-wop/airGRiwrm/-/issues/134))
- Ungauged nodes: crash with a reservoir and several upstream nodes
  ([\#136](https://gitlab.irstea.fr/in-wop/airGRiwrm/-/issues/136))
- Ungauged node: Diversion to Reservoir crashes Calibration
  ([\#130](https://gitlab.irstea.fr/in-wop/airGRiwrm/-/issues/130))
- Wrong calibration node order with multiple ungauged node clusters
  ([\#149](https://gitlab.irstea.fr/in-wop/airGRiwrm/-/issues/149))
- CreateGRiwrm: several Diversions on the same node do not raise error
  ([\#125](https://gitlab.irstea.fr/in-wop/airGRiwrm/-/issues/125))
- Reservoir output plot color palette issue
  ([\#154](https://gitlab.irstea.fr/in-wop/airGRiwrm/-/issues/154))
- Wrong sorting for calibration of ungauged nodes
  ([\#155](https://gitlab.irstea.fr/in-wop/airGRiwrm/-/issues/155))
- Donor defined on gauged model node turns the node into ungauged at
  Calibration
  ([\#157](https://gitlab.irstea.fr/in-wop/airGRiwrm/-/issues/157))
- Calibration: crash when transferring from upstream donor to upstream
  receiver
  ([\#158](https://gitlab.irstea.fr/in-wop/airGRiwrm/-/issues/158))
- plot.GRiwrm crashes with a single node
  ([\#153](https://gitlab.irstea.fr/in-wop/airGRiwrm/-/issues/153))
- CreateInputsModel: Don’t allow ungauged donor
  ([\#131](https://gitlab.irstea.fr/in-wop/airGRiwrm/-/issues/131))
- Rename internal function `RunModel.SD`
  ([\#166](https://gitlab.irstea.fr/in-wop/airGRiwrm/-/issues/166))
- Calibration: crash with downstream receiver node with upstream
  reservoir
  ([\#167](https://gitlab.irstea.fr/in-wop/airGRiwrm/-/issues/167))

### Documentation:

- Improve Supervisor documentation
  ([\#100](https://gitlab.irstea.fr/in-wop/airGRiwrm/-/issues/100))
- Update package citation reference
  ([\#119](https://gitlab.irstea.fr/in-wop/airGRiwrm/-/issues/119))
- Add a main page in the package documentation
  ([\#77](https://gitlab.irstea.fr/in-wop/airGRiwrm/-/issues/77))
- Improve RunModel documentation
  ([\#133](https://gitlab.irstea.fr/in-wop/airGRiwrm/-/issues/133))

### Internal changes:

- Check failed on release version
  ([\#78](https://gitlab.irstea.fr/in-wop/airGRiwrm/-/issues/78))
- Transfer documentation to github pages
  ([\#148](https://gitlab.irstea.fr/in-wop/airGRiwrm/-/issues/148))
- Publish documentation package on github with github workflow
  ([\#162](https://gitlab.irstea.fr/in-wop/airGRiwrm/-/issues/162))
- CI: add check with version “dev” of airGR
  ([\#84](https://gitlab.irstea.fr/in-wop/airGRiwrm/-/issues/84))

## airGRiwrm 0.6.2

CRAN release: 2023-10-24

- airGR changes cause failed check
  ([\#139](https://gitlab.irstea.fr/in-wop/airGRiwrm/-/issues/139))

## airGRiwrm 0.6.1

CRAN release: 2022-03-08

### Changes:

- Simplify the use of airGR in airGRiwrm
  ([\#63](https://gitlab.irstea.fr/in-wop/airGRiwrm/-/issues/63))
- `CreateInputsModel`: Make `Qobs` parameter optional
  ([\#60](https://gitlab.irstea.fr/in-wop/airGRiwrm/-/issues/60))
- airGR compatibility: change on LengthHydro unit
  ([\#32](https://gitlab.irstea.fr/in-wop/airGRiwrm/-/issues/32))
- `CreateInputsCrit`: Change obs parameter characteristics
  ([\#38](https://gitlab.irstea.fr/in-wop/airGRiwrm/-/issues/38))
- Update URLs in the DESCRIPTION file
  ([\#45](https://gitlab.irstea.fr/in-wop/airGRiwrm/-/issues/45))
- Use S3 plot method for GRiwrm class objects
  ([\#26](https://gitlab.irstea.fr/in-wop/airGRiwrm/-/issues/26))
- Rename function GRiwrm to CreateGRiwrm
  ([\#46](https://gitlab.irstea.fr/in-wop/airGRiwrm/-/issues/46))
- `CreateInputsCrit`: `transfo` is mandatory for parameter
  regularization
  ([\#56](https://gitlab.irstea.fr/in-wop/airGRiwrm/-/issues/56))

### New features:

- `plot.Qm3s`: customize legend position
  ([\#75](https://gitlab.irstea.fr/in-wop/airGRiwrm/-/issues/75))
- Regularisation: Add default value for parameter Celerity
  ([\#58](https://gitlab.irstea.fr/in-wop/airGRiwrm/-/issues/58))
- Add network consistency checks in `GRiwrm`
  ([\#36](https://gitlab.irstea.fr/in-wop/airGRiwrm/-/issues/36))
- Handle CemaNeige compatibility
  ([\#52](https://gitlab.irstea.fr/in-wop/airGRiwrm/-/issues/52))
- Use S3 plot method for GRiwrmOutputsModel class objects
  ([\#26](https://gitlab.irstea.fr/in-wop/airGRiwrm/-/issues/26))
- Handling correctly initial conditions
  ([\#48](https://gitlab.irstea.fr/in-wop/airGRiwrm/-/issues/48))
- Calibration with parameter regularization
  ([\#54](https://gitlab.irstea.fr/in-wop/airGRiwrm/-/issues/54))

### Bug fixes:

- Results differences between versions on vignette V04
  ([\#75](https://gitlab.irstea.fr/in-wop/airGRiwrm/-/issues/75))
- `plot.GRiwrm` not working in gitlab-ci
  ([\#74](https://gitlab.irstea.fr/in-wop/airGRiwrm/-/issues/74))
- `plot.GRiwrm`: mermaid code is displayed with the diagram
  ([\#73](https://gitlab.irstea.fr/in-wop/airGRiwrm/-/issues/73))
- `CreateGRiwrm` crashes when keeping all columns and rename some
  ([\#64](https://gitlab.irstea.fr/in-wop/airGRiwrm/-/issues/64))
- Breaking change in airGR in issue HYCAR-Hydro/airgr#137
  ([\#62](https://gitlab.irstea.fr/in-wop/airGRiwrm/-/issues/62))
- Review documentation for publication on CRAN
  ([\#43](https://gitlab.irstea.fr/in-wop/airGRiwrm/-/issues/43))
- Vignettes: working directory instability
  ([\#35](https://gitlab.irstea.fr/in-wop/airGRiwrm/-/issues/35))
- airGR compatibility: debugged version of RunModel_Lag
  ([\#33](https://gitlab.irstea.fr/in-wop/airGRiwrm/-/issues/33))
- `CreateInputsModel`: Error when using data.frame for Qobs
  ([\#37](https://gitlab.irstea.fr/in-wop/airGRiwrm/-/issues/37))
- `RunModel.Supervisor`: Error in
  ctrlr$U\lbrack seq.int(length(sv$ts.index), i\]
  ([\#39](https://gitlab.irstea.fr/in-wop/airGRiwrm/-/issues/39))
- Supervisor: measurement on network downstream node returns `NULL`
  ([\#40](https://gitlab.irstea.fr/in-wop/airGRiwrm/-/issues/40))
- `RunModel`: Suspected bug on `OutputsModel$Qsim` in the training
  example
  ([\#41](https://gitlab.irstea.fr/in-wop/airGRiwrm/-/issues/41))
- Test fail after airGR update on outputting warm-up Qsim
  ([\#50](https://gitlab.irstea.fr/in-wop/airGRiwrm/-/issues/50))
- Wrong Qobs use in Lavenne function criteria
  ([\#57](https://gitlab.irstea.fr/in-wop/airGRiwrm/-/issues/57))

### Internal changes:

- Prepare the package for v0.6.x CRAN submission
  ([\#71](https://gitlab.irstea.fr/in-wop/airGRiwrm/-/issues/71))
- Clone on github master and dev branches of the repository
  ([\#68](https://gitlab.irstea.fr/in-wop/airGRiwrm/-/issues/68))
- Update airGR dependency to CRAN v1.7.0
  ([\#69](https://gitlab.irstea.fr/in-wop/airGRiwrm/-/issues/69))
- Add an airGR galaxy tab on the website?
  ([\#49](https://gitlab.irstea.fr/in-wop/airGRiwrm/-/issues/49))
- CI: dependency issues with Latex in Check as CRAN
  ([\#53](https://gitlab.irstea.fr/in-wop/airGRiwrm/-/issues/53))
- Remove dependency to R \> 3.5
  ([\#59](https://gitlab.irstea.fr/in-wop/airGRiwrm/-/issues/59))
- pkgdown: wrong documentation for methods CreateRunOptions and
  CreateCalibOptions
  ([\#65](https://gitlab.irstea.fr/in-wop/airGRiwrm/-/issues/65))
- Review documentation for publication on CRAN
  ([\#43](https://gitlab.irstea.fr/in-wop/airGRiwrm/-/issues/43))
- Push roxygen outputs on the repository
  ([\#34](https://gitlab.irstea.fr/in-wop/airGRiwrm/-/issues/34))
- Generation of the <https://airgriwrm.g-eau.net> site documentation
  ([\#44](https://gitlab.irstea.fr/in-wop/airGRiwrm/-/issues/44))
- Automatically update website from package repository
  ([\#47](https://gitlab.irstea.fr/in-wop/airGRiwrm/-/issues/47))

## airGRiwrm 0.5.0 (Release date: 2021-03-07)

### New features:

- Feedback control
  ([\#19](https://gitlab.irstea.fr/in-wop/airGRiwrm/-/issues/19))
- RunModel of GRiwrm networks: add a data.frame of simulated flows in
  OutputsModel
  ([\#30](https://gitlab.irstea.fr/in-wop/airGRiwrm/-/issues/30))
- Plot simulated flows of all the nodes in m3/s
  ([\#31](https://gitlab.irstea.fr/in-wop/airGRiwrm/-/issues/31))

### Changes:

- RunModel: Uncoupling of hydrological and hydraulic models
  ([\#28](https://gitlab.irstea.fr/in-wop/airGRiwrm/-/issues/28))

## airGRiwrm 0.4.0 (Release date: 2020-12-28)

### New features:

- Convert basin meteorological data to sub-basin level
  ([\#21](https://gitlab.irstea.fr/in-wop/airGRiwrm/-/issues/21))

### Changes:

- Clarify dependency with ‘DiagrammeR’ package
  ([\#24](https://gitlab.irstea.fr/in-wop/airGRiwrm/-/issues/24))

### Minor changes:

- Replace vignette examples on Seine River by a fake example from data
  provided by airGR
  ([\#13](https://gitlab.irstea.fr/in-wop/airGRiwrm/-/issues/13))

### Bug fixes:

- Impossibility to inject flow associated to an area
  ([\#23](https://gitlab.irstea.fr/in-wop/airGRiwrm/-/issues/23))
- Error in the area used for the sub basins
  ([\#22](https://gitlab.irstea.fr/in-wop/airGRiwrm/-/issues/22))

## airGRiwrm 0.3.1 (Release date: 2020-08-07)

### New features:

- Calibration of influenced semi-distributed model
  ([\#11](https://gitlab.irstea.fr/in-wop/airGRiwrm/-/issues/11))

## airGRiwrm 0.3.0 (Release date: 2020-08-07)

### New features:

- Add node of type “direct flow” in order to inject or withdraw flows
  into the model
  ([\#5](https://gitlab.irstea.fr/in-wop/airGRiwrm/-/issues/5))

## airGRiwrm 0.2.1 (Release date: 2020-06-11)

### Changes:

- Remove Gits class object and use CreateInputsModel directly
  ([\#7](https://gitlab.irstea.fr/in-wop/airGRiwrm/-/issues/7))
- Remove Girop class object and integrate hydrological model and area in
  Ginet ([\#9](https://gitlab.irstea.fr/in-wop/airGRiwrm/-/issues/9))
- Rename “Ginet” class object to “Griwrm”
  ([\#10](https://gitlab.irstea.fr/in-wop/airGRiwrm/-/issues/10))

## airGRiwrm 0.2.0 (Release date: 2020-06-06)

### New features:

- Calibration of semi-distributed model
  ([\#3](https://gitlab.irstea.fr/in-wop/airGRiwrm/-/issues/3))

## airGRiwrm 0.1.0 (Release date: 2020-05-25)

### New features:

- Database structuring
  ([\#1](https://gitlab.irstea.fr/in-wop/airGRiwrm/-/issues/1))
- Scheduling airGR model runs
  ([\#2](https://gitlab.irstea.fr/in-wop/airGRiwrm/-/issues/2))
