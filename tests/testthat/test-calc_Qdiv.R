test_that("calc_Qdiv works", {
  expect_equal(
    calc_Qdiv(Qnat = 1, Qdiv = 1, Qmin = 0),
    list(Qsim = 0, Qdiv = 1)
  )
  expect_equal(
    calc_Qdiv(Qnat = 5, Qdiv = 4, Qmin = 3),
    list(Qsim = 3, Qdiv = 2)
  )
  expect_equal(
    calc_Qdiv(Qnat = 5, Qdiv = 10, Qmin = 3),
    list(Qsim = 3, Qdiv = 2)
  )
  expect_equal(
    calc_Qdiv(Qnat = 5, Qdiv = 1, Qmin = 3),
    list(Qsim = 4, Qdiv = 1)
  )
  expect_equal(
    calc_Qdiv(Qnat = 3, Qdiv = 1, Qmin = 4),
    list(Qsim = 3, Qdiv = 0)
  )
  expect_equal(
    calc_Qdiv(Qnat = 3, Qdiv = -1, Qmin = 5),
    list(Qsim = 4, Qdiv = -1)
  )
  expect_equal(
    calc_Qdiv(Qnat = 3, Qdiv = -1, Qmin = 2),
    list(Qsim = 4, Qdiv = -1)
  )
  expect_equal(
    calc_Qdiv(Qnat = rep(1E6, 2), Qdiv = rep(1000, 2), Qmin = rep(1E12, 2)),
    list(Qsim = rep(1E6, 2), Qdiv = rep(0, 2))
  )
})
