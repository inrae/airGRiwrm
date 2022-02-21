test_that("extra columns work (#64)", {
  text = "id_amont	lambert2.x	lambert2.y	area	nom	id_aval	distance_aval	model
H8100021	537912.994	2455749.314	64420.94	La Seine à Vernon	NA	NA	RunModel_CemaNeigeGR4J
H7900010	578113	2437649	61642.28	La Seine à Poissy	H8100021	76.28	RunModel_CemaNeigeGR4J
H5920010	602213	2427449	43824.66	La Seine à Paris [Austerlitz après création lacs]	H7900010	82.26	RunModel_CemaNeigeGR4J"

  BS_reseau <- read.csv(text = text, sep = "\t")

  expect_s3_class(CreateGRiwrm(
    BS_reseau,
    cols = list(
      id = "id_amont",
      down = "id_aval",
      length = "distance_aval"
    ),
    keep_all = TRUE
  ),
  "GRiwrm")

})
