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

