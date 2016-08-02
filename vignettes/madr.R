## ---- message=FALSE, echo=FALSE------------------------------------------
#library(np)

## ------------------------------------------------------------------------
library(devtools)
install_github("hsavoy/madr")
library(madr)
data(pumping)

## ---- fig.width=7.2, fig.height=4----------------------------------------
plot(pumping, "realizations")

