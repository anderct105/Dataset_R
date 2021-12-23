## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
num.permus <- 100
scores <- cbind(rbeta(num.permus, 1, 1),
                rbeta(num.permus, 5, 1),
                rbeta(num.permus, 2, 6),
                rbeta(num.permus, 10, 10),
                rbeta(num.permus, 1, 3))
head(scores)

## -----------------------------------------------------------------------------
library(permupack)
permu.list <- getPermutationFromScores(scores=scores, 
                                       type="ranking", decreasing=FALSE)


## ---- fig.width=10, fig.height=6, out.width="100%"----------------------------
margs <- getFirstOrderMarginals(permu.list, smoothed=TRUE)
margs
plot <- plotFirstOrderMarginals(permu.list, smoothed=TRUE)
plot

## ---- fig.width=10, fig.height=6, out.width="100%"----------------------------
library(ggplot2)
plot + scale_fill_gradient(low="yellow", high="darkblue")

