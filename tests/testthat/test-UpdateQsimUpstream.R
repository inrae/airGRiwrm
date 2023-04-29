# data set up
e <- setupRunModel()
# variables are copied from environment 'e' to the current environment
# https://stackoverflow.com/questions/9965577/r-copy-move-one-environment-to-another
for(x in ls(e)) assign(x, get(x, e))

test_that("updateQupstream works", {
  IM_54001 <- UpdateQsimUpstream(InputsModel[["54001"]],
                                 RunOptions[["54001"]],
                                 OM_GriwrmInputs)
  expect_equal(
    IM_54001$Qupstream[IndPeriod_Run],
    OM_GriwrmInputs[["54095"]]$Qsim_m3
  )
})
