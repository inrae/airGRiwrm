# Transfer GR parameters from one donor sub-basin to a receiver sub-basin

This function is used by `Calibration.GRiwrmInputsModel` for
transferring parameters to ungauged nodes and for providing a priori
parameters in parameter regularization (See
[CreateInputsCrit](https://inrae.github.io/airGRiwrm/dev/reference/CreateInputsCrit.md)).

## Usage

``` r
transferGRparams(
  InputsModel,
  Param,
  donor,
  receiver,
  default_param = NULL,
  verbose = FALSE
)
```

## Arguments

- InputsModel:

  Object of class
  [GRiwrmInputsModel](https://inrae.github.io/airGRiwrm/dev/reference/CreateInputsModel.GRiwrm.md),
  see
  [CreateInputsModel.GRiwrm](https://inrae.github.io/airGRiwrm/dev/reference/CreateInputsModel.GRiwrm.md)
  for details

- Param:

  [numeric](https://rdrr.io/r/base/numeric.html) vector of GR model
  parameters

- donor:

  [character](https://rdrr.io/r/base/character.html) id of the node
  which gives its parameters

- receiver:

  [character](https://rdrr.io/r/base/character.html) id of the node
  which receives the parameters from the donor

- default_param:

  [numeric](https://rdrr.io/r/base/numeric.html) vector of GR model
  parameters if parameters are missing from the donor

- verbose:

  [logical](https://rdrr.io/r/base/logical.html) Add information message
  on donor and receiver

## Value

A [numeric](https://rdrr.io/r/base/numeric.html)
[vector](https://rdrr.io/r/base/vector.html) of transferred parameters

## Details

`donor` and `receiver` nodes should have the same GR model with the same
snow module configuration.

The transfer takes care of:

- the presence/absence of hydraulic routing parameters between the donor
  and the receiver (But `default_param` should be provided if the
  receiver has more parameters than the donor)

- the transformation of the X4 parameters of GR models. Indeed, this
  parameter is correlated to the catchment area. Therefore, it has to be
  rescaled from the surface of the donor catchment to the surface of the
  receiver catchment, following the relationship suggested by Lobligeois
  (2014): \\X4\_{receiver} = X4\_{donor} \times (S\_{receiver} /
  S\_{donor})^{0.3}\\.

## References

Lobligeois, F., 2014. Mieux connaitre la distribution spatiale des
pluies améliore-t-il la modélisation des crues ? Diagnostic sur 181
bassins versants français. Thèse de Doctorat, Irstea (Antony),
AgroParisTech (Paris), 312 pp.
https://webgr.inrae.fr/Media/Files/biblio-theses/2014_lobligeois_these.pdf
