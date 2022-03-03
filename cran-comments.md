## R CMD check results

0 errors | 0 warnings | 0 note

## Context

This is a second submission for a first version to publish on CRAN. Here's below the mail of submission refusal: 

Date : 	Thu, 3 Mar 2022 10:11:56 +0100
From : 	Gregor Seyer <gregor.seyer@wu.ac.at>

Thanks,

Please add \value to .Rd files regarding exported methods and explain the functions results in the documentation. Please write about the structure of the output (class) and also what the output means. (If a function does not return a value, please document that too, e.g. \value{No return value, called for side effects} or similar)

Missing Rd-tags:

     plot.Qm3s.Rd: \value
     RunModel.InputsModel.Rd: \value

Please make sure that you do not change the user's options, par or working directory. If you really have to do so within functions, please ensure with an *immediate* call of on.exit() that the settings are reset when the function is exited. e.g.:

```
...
oldpar <- par(no.readonly = TRUE)    # code line i
on.exit(par(oldpar))            # code line i + 1
...
par(mfrow=c(2,2))            # somewhere after
...
```

e.g.: plot.GRiwrmOutputsModel.R

If you're not familiar with the function, please check ?on.exit. This function makes it possible to restore options before exiting a function even if the function breaks. Therefore it needs to be called immediately after the option change within a function.

Please always make sure to reset to user's options(), working directory or par() after you changed it in examples and vignettes and demos.

e.g.: inst/doc/V04_Closed-loop_regulated_withdrawal.R

```
oldpar <- par(mfrow = c(1,2))
...
par(oldpar)
```


Please fix and resubmit.

Best,
Gregor Seyer

## What have been done since?

### `value` tag issues

* I have added `value` tags to plot.Qm3s.Rd and RunModel.InputsModel.Rd. I have also completed the `value` tags of RunModel.GRiwrmInputsModel.Rd and plot.GRiwrmOutputsModel.Rd for more precision.

### User's options issues

* plot.GRiwrmOutputsModel.R and V04_Closed-loop_regulated_withdrawal.R have been modified accordingly to the advice above.

### Miscellaneous

* A small bug (https://gitlab.irstea.fr/in-wop/airGRiwrm/-/issues/75) has been fixed in a vignette
* A small new feature has been implemented on plot.Qm3s (https://gitlab.irstea.fr/in-wop/airGRiwrm/-/issues/76)
* DESCRIPTION and NEWS.md have been updated for taking into account version upgrade.

**Thanks for all :)**
