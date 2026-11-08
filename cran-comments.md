# CRAN submission comments — airGRiwrm 0.8.1

**Resubmission** — the 0.8.0 submission (2026-11-06) was rejected by
the pretest because two examples (`RunModel_Reservoir`,
`plot.OutputsModelReservoir`) exceeded the 5-second limit. The closed-
loop Supervisor block of the shared example
(`man-examples/RunModel_Reservoir.R`) is now wrapped in `\donttest{}`:
the code stays visible in the manual, `R CMD check` no longer executes
it (it calls R at every time step, which is intrinsically slow), and it
is still executed by the package CI and by `R CMD check
--run-donttest`. This 0.8.1 submission contains that fix.

This is a version 0.8.1 update of the airGRiwrm package
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

`R CMD check --no-manual --as-cran` (R 4.6.2, Linux x86_64), after the
examples fix (2026-11-07):

- 0 errors, 0 warnings. The two notes from the 2026-11-06 check are
  both resolved:
  - *examples*: with the Supervisor block wrapped in `\donttest`, the
    two Reservoir examples run in 3.5 s and 2.6 s respectively (measured
    twice under CRAN semantics, i.e. with the `\donttest` block
    excluded as `R CMD check` does), well below the 5-second limit.
  - *CRAN incoming feasibility*: the RENATER users-list subscribe URL
    `https://groupes.renater.fr/sympa/subscribe/airgriwrm-users`
    returned HTTP 503 (transient service issue) at the time of the
    previous submission; it returns HTTP 200 as of 2026-11-08 and the
    check now passes. The link is the official users list of the
    package; the one URL that had moved (IN-WOP project) was updated to
    its canonical target.
- The full testthat suite and all vignettes build and pass
  (including the new V07 vignette).
- R-hub check (2026-11-07, branch `master`): all five platforms pass
  with 0 errors, 0 warnings and 0 notes — ubuntu-release,
  ubuntu-next, ubuntu-gcc12, ubuntu-clang and Windows (R-devel) —
  including the `--run-donttest` examples run (the Supervisor block is
  still executed and correct).
- Spelling: no findings (`spelling::spell_check_package`);
  `inst/WORDLIST` lists the package terminology and the legitimate
  French bibliographic titles in the references.
- URL check (`urlchecker`): no dead links.

## Reverse dependencies

`airGRiwrm` has no reverse dependencies on CRAN, so no reverse
dependency check was performed.
