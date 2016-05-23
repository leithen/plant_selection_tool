setwd('~/Documents/projects_complete/2016/plant_selection_tool/tool_github')
rm(list=ls())

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
mix.a <- find.model(f=abundance, k=9)
## optimizing 'richness': 
mix.r <- find.model(f=richness, k=9)
## optimizing both 'abundance' and 'richness': 
mix.ar <- find.model(f=abundance.richness, k=9)

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


