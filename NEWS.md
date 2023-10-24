# airGRiwrm 0.6.2


Internal changes:
-----------------

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
