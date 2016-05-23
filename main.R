## load genetic algorithm
source('R/ga.R')
source('R/control.R')
## load objective functions
source('R/objective_functions.R')

## load sample data (for native plants in California)
load('source-native.RData', verbose=TRUE)

## find the collection of 'k' plants that optimize the objective
## function 'f'.  k must be an integer.  See readme.pdf for potential
## values values of f.
##
## for example, optimizing 'abundance' for 9 species mixes: 
mix.a <- find.mix(f=abundance, k=9)
## optimizing 'richness': 
mix.r <- find.mix(f=richness, k=9)
## optimizing both 'abundance' and 'richness': 
mix.ar <- find.mix(f=abundance.richness, k=9)

## one can change parameters of the genetic algorithm by passing them
## in as arugments.  E.g., to run it for more iterations, one could do
## the following:
mix.ar <- find.mix(f=abundance.richness, k=9, n.gens=10000)
## for a full list of optional arguments, see readme.pdf or check the
## source code in 'R/control.R'

## compare model scores for the above mixes for abundance (higher is
## better):
score(c(abundance), mix.a)
score(c(abundance), mix.r)
score(c(abundance), mix.ar)

## for richness
score(c(richness),  mix.a)
score(c(richness),  mix.r)
score(c(richness),  mix.ar)

## for both abundance and richness
score(c(abundance.richness),  mix.a)
score(c(abundance.richness),  mix.r)
score(c(abundance.richness),  mix.ar)


