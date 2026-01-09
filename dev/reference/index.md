# Package index

## All functions

- [`Calibration()`](https://inrae.github.io/airGRiwrm/dev/reference/Calibration.md)
  : Calibration of the parameters of one catchment or a network of
  sub-catchments

- [`ConvertMeteoSD()`](https://inrae.github.io/airGRiwrm/dev/reference/ConvertMeteoSD.md)
  : Conversion of meteorological data from basin scale to sub-basin
  scale

- [`CreateCalibOptions.GRiwrmInputsModel`](https://inrae.github.io/airGRiwrm/dev/reference/CreateCalibOptions.md)
  [`CreateCalibOptions`](https://inrae.github.io/airGRiwrm/dev/reference/CreateCalibOptions.md)
  [`CreateCalibOptions.InputsModel`](https://inrae.github.io/airGRiwrm/dev/reference/CreateCalibOptions.md)
  [`CreateCalibOptions.character`](https://inrae.github.io/airGRiwrm/dev/reference/CreateCalibOptions.md)
  [`CreateCalibOptions.function`](https://inrae.github.io/airGRiwrm/dev/reference/CreateCalibOptions.md)
  [`CreateCalibOptions.RunModel_Reservoir`](https://inrae.github.io/airGRiwrm/dev/reference/CreateCalibOptions.md)
  : Creation of the CalibOptions object

- [`CreateController()`](https://inrae.github.io/airGRiwrm/dev/reference/CreateController.md)
  : Creation and adding of a controller in a supervisor

- [`CreateGRiwrm()`](https://inrae.github.io/airGRiwrm/dev/reference/CreateGRiwrm.md)
  : Generation of a network description containing all hydraulic nodes
  and the description of their connections

- [`CreateInputsCrit()`](https://inrae.github.io/airGRiwrm/dev/reference/CreateInputsCrit.md)
  :

  Creation of the InputsCrit object required to the `ErrorCrit`
  functions

- [`CreateInputsModel(`*`<GRiwrm>`*`)`](https://inrae.github.io/airGRiwrm/dev/reference/CreateInputsModel.GRiwrm.md)
  :

  Creation of an InputsModel object for an **airGRiwrm** network

- [`CreateInputsModel()`](https://inrae.github.io/airGRiwrm/dev/reference/CreateInputsModel.md)
  :

  Generic function for creating `InputsModel` object for either
  **airGR** or **airGRiwrm**

- [`CreateRunOptions.GRiwrmInputsModel`](https://inrae.github.io/airGRiwrm/dev/reference/CreateRunOptions.md)
  [`CreateRunOptions`](https://inrae.github.io/airGRiwrm/dev/reference/CreateRunOptions.md)
  [`CreateRunOptions.InputsModel`](https://inrae.github.io/airGRiwrm/dev/reference/CreateRunOptions.md)
  [`CreateRunOptions.character`](https://inrae.github.io/airGRiwrm/dev/reference/CreateRunOptions.md)
  [`CreateRunOptions.function`](https://inrae.github.io/airGRiwrm/dev/reference/CreateRunOptions.md)
  : Creation of the RunOptions object

- [`CreateSupervisor()`](https://inrae.github.io/airGRiwrm/dev/reference/CreateSupervisor.md)
  : Creation of a Supervisor for handling regulation in a model

- [`RunModel(`*`<GRiwrmInputsModel>`*`)`](https://inrae.github.io/airGRiwrm/dev/reference/RunModel.GRiwrmInputsModel.md)
  :

  RunModel function for *GRiwrmInputsModel* object

- [`RunModel(`*`<GRiwrmOutputsModel>`*`)`](https://inrae.github.io/airGRiwrm/dev/reference/RunModel.GRiwrmOutputsModel.md)
  : RunModel for hot restart after a previous simulation period

- [`RunModel(`*`<InputsModel>`*`)`](https://inrae.github.io/airGRiwrm/dev/reference/RunModel.InputsModel.md)
  :

  Wrapper for
  [airGR::RunModel](https://rdrr.io/pkg/airGR/man/RunModel.html) for one
  sub-basin

- [`RunModel()`](https://inrae.github.io/airGRiwrm/dev/reference/RunModel.md)
  :

  RunModel function for both **airGR** InputsModel and GRiwrmInputsModel
  object

- [`RunModel(`*`<Supervisor>`*`)`](https://inrae.github.io/airGRiwrm/dev/reference/RunModel.Supervisor.md)
  : RunModel function for a Supervisor

- [`RunModel_Reservoir()`](https://inrae.github.io/airGRiwrm/dev/reference/RunModel_Reservoir.md)
  : Run with a reservoir model

- [`Severn`](https://inrae.github.io/airGRiwrm/dev/reference/Severn.md)
  : Catchment attributes and hydro-meteorological timeseries for some
  gauging stations on the Severn River

- [`as.Qm3s()`](https://inrae.github.io/airGRiwrm/dev/reference/as.Qm3s.md)
  :

  Coerce [data.frame](https://rdrr.io/r/base/data.frame.html) or content
  of a [data.frame](https://rdrr.io/r/base/data.frame.html) into a
  *Qm3s* object ready for plotting

- [`extractParam()`](https://inrae.github.io/airGRiwrm/dev/reference/extractParam.md)
  : Extract calibrated parameters

- [`getNextTimeSteps()`](https://inrae.github.io/airGRiwrm/dev/reference/getNextTimeSteps.md)
  : Get the next time steps date/time of a simulation

- [`getNodeProperties()`](https://inrae.github.io/airGRiwrm/dev/reference/getNodeProperties.md)
  [`getAllNodesProperties()`](https://inrae.github.io/airGRiwrm/dev/reference/getNodeProperties.md)
  : Properties of GRiwrm nodes

- [`getNodeRanking()`](https://inrae.github.io/airGRiwrm/dev/reference/getNodeRanking.md)
  : Sorting of the nodes from upstream to downstream for RunModel and
  Calibration

- [`getSD_Ids()`](https://inrae.github.io/airGRiwrm/dev/reference/getSD_Ids.md)
  [`getNoSD_Ids()`](https://inrae.github.io/airGRiwrm/dev/reference/getSD_Ids.md)
  : Function to get the IDs of sub-basins using SD model or not

- [`isNodeDownstream()`](https://inrae.github.io/airGRiwrm/dev/reference/isNodeDownstream.md)
  [`isNodeUpstream()`](https://inrae.github.io/airGRiwrm/dev/reference/isNodeDownstream.md)
  : Check if a node is downstream or upstream another one

- [`merge(`*`<OutputsModel>`*`)`](https://inrae.github.io/airGRiwrm/dev/reference/merge.OutputsModel.md)
  [`merge(`*`<GRiwrmOutputsModel>`*`)`](https://inrae.github.io/airGRiwrm/dev/reference/merge.OutputsModel.md)
  : Merge Two outputs of airGR simulations

- [`mermaid()`](https://inrae.github.io/airGRiwrm/dev/reference/mermaid.md)
  [`mermaid_gen_link()`](https://inrae.github.io/airGRiwrm/dev/reference/mermaid.md)
  [`plot(`*`<mermaid>`*`)`](https://inrae.github.io/airGRiwrm/dev/reference/mermaid.md)
  : Plot a mermaid diagram

- [`plot(`*`<GRiwrm>`*`)`](https://inrae.github.io/airGRiwrm/dev/reference/plot.GRiwrm.md)
  : Plot of a diagram representing the network structure of a GRiwrm
  object

- [`plot(`*`<GRiwrmOutputsModel>`*`)`](https://inrae.github.io/airGRiwrm/dev/reference/plot.GRiwrmOutputsModel.md)
  : Function which creates screen plots giving an overview of the model
  outputs in the GRiwrm network

- [`plot(`*`<OutputsModelReservoir>`*`)`](https://inrae.github.io/airGRiwrm/dev/reference/plot.OutputsModelReservoir.md)
  : Plot simulated reservoir volume, inflows and released flows time
  series on a reservoir node

- [`plot(`*`<Qm3s>`*`)`](https://inrae.github.io/airGRiwrm/dev/reference/plot.Qm3s.md)
  :

  Plot of a `Qm3s` object (time series of simulated flows)

- [`reduceGRiwrm()`](https://inrae.github.io/airGRiwrm/dev/reference/reduceGRiwrm.md)
  : Reduce the size of a GRiwrm by selecting the subset of nodes
  corresponding to a downstream node

- [`sort(`*`<GRiwrm>`*`)`](https://inrae.github.io/airGRiwrm/dev/reference/sort.GRiwrm.md)
  : Sort a GRiwrm network in upstream-downstream order ready for
  Calibration

- [`transferGRparams()`](https://inrae.github.io/airGRiwrm/dev/reference/transferGRparams.md)
  : Transfer GR parameters from one donor sub-basin to a receiver
  sub-basin
