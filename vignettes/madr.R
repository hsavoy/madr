## ---- message=FALSE, echo=FALSE------------------------------------------
library(np)

## ------------------------------------------------------------------------
library(madr)
data("pumping")

## ---- fig.width=7.2, fig.height=4----------------------------------------
plot(pumping, "realizations")

## ---- results='hide'-----------------------------------------------------
pumping <- calcLikelihood(pumping, list(timesteps=100))
pumping <- calcPosterior(pumping)

## ---- fig.width=7.2, fig.height=4----------------------------------------
plot(pumping, "posteriors")

## ---- results='hide'-----------------------------------------------------
pumping.min <- reduceData(pumping, min)
pumping.min <- calcLikelihood(pumping.min)
pumping.min <- calcPosterior(pumping.min)

## ---- fig.width=7.2, fig.height=4----------------------------------------
plot(pumping.min, "posteriors")

## ---- results='hide'-----------------------------------------------------
pumping.matern <- reduceData(pumping, matern, init.matern)
pumping.matern <- calcLikelihood(pumping.matern)
pumping.matern <- calcPosterior(pumping.matern)

## ---- fig.width=7.2, fig.height=4----------------------------------------
plot(pumping.matern, "posteriors")

