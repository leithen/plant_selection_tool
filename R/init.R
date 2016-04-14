## GA stuff
source('R/ga/ga.R')
source('R/ga/control.R')
source('R/mcmc.R')

## other stuff
source('R/plotting.R')
source('R/analysis/model_score.R')

id <- function(x) unique(sort(x))

## load and return loaded object
load.local <- function(file) {
 v <- load(file)
 stopifnot(length(v) == 1)
 get(v)
}
