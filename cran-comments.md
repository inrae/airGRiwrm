# CRAN submission comments — airGRiwrm 0.8.0

This is a version 0.8.0 update of the airGRiwrm package
(currently 0.7.0 on CRAN).

The package extends `airGR` (>= 1.7.8, currently 1.7.9 on CRAN)
with the simulation of semi-distributed hydrological networks and
the management of human infrastructures (diversions, transfers,
regulated withdrawals, CemaNeige snow-melt networks).

## Changes since 0.7.0

See NEWS.md for the complete list: 5 breaking changes, 13 new
features, 3 changes, 21 bug fixes and 4 documentation updates.

Breaking changes (all documented in NEWS.md):

- `Outputs_Sim` in `CreateRunOptions` no longer defaults to `"all"`
  on networks (reduced default output set; `Outputs_Sim = "all"`
  restores the previous behaviour) — #141.
- `CreateInputsModel` gains a `Qrelease` argument for the
  `RunModel_Reservoir` model — #199.
- `forceReservoirObs` behaviour and the CemaNeige `Outputs_Sim`
  handling were reworked (#199) to keep the Fortran model runs
  unchanged.
- The `plot` method of `*GRiwrmOutputsModel` now handles shared node
  IDs (diversion/reservoir) with a documented, graceful exclusion —
  #178.
- Node IDs of the returned outputs are now consistent with the
  network definition (#197/#196).

## Checks performed

`R CMD check --no-manual --as-cran` (R 4.6.2, Linux x86_64):

- 0 errors, 0 warnings, 2 notes:
  - *CRAN incoming feasibility*: the users-list subscribe URL
    `https://groupes.renater.fr/sympa/subscribe/airgriwrm-users`
    returned HTTP 503 (Service Unavailable) at check time; the RENATER
    sytse service is up and the link is the official users list of
    the package. The one URL that had moved (IN-WOP project) was
    updated to its canonical target.
  - *examples*: the two Reservoir examples (`RunModel_Reservoir`,
    `plot.OutputsModelReservoir`) run a 9-year simulation with a
    1-year warm-up; their elapsed time (7–12 s on a shared Linux
    machine) is inherent to the simulation, not to package overhead.
- The full testthat suite and all vignettes build and pass
  (including the new V07 vignette).
- Spelling: no findings (`spelling::spell_check_package`);
  `inst/WORDLIST` lists the package terminology and the legitimate
  French bibliographic titles in the references.
- URL check (`urlchecker`): no dead links (the 503 above is a
  transient service response, not a dead link).

## Reverse dependencies

`airGRiwrm` has no reverse dependencies on CRAN, so no reverse
dependency check was performed.
