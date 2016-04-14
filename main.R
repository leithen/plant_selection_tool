## *******************************************************
setwd('~/Dropbox/plant_selection_tool/tool')
rm(list=ls())
source('R/init.R')
## *******************************************************

## *******************************************************
## look at native plants
load('data/modified/source-native.RData')
v.mat <- d$v.mat
bloom.times <- d$bloom.times
flight.times <- d$flight.times
plants.by.polli <- d$plants.by.polli

get.naive('native', 9)
get.best('abundance', 'native', 9)

mix <- colnames(v.mat)[get.best(c('abundance'), 'native', 9)]

score(c(abundance), mix)
score(c(richness),  mix)


## *******************************************************
## look at exhaustive models
load('data/modified/source-native.RData')
v.mat <- d$v.mat
bloom.times <- d$bloom.times

## compare to best
load('saved/exhaustive/best-models.RData')

load('saved/exhaustive/abundance.RData')
sort(colnames(v.mat)[best.models$m.abundance])
sort(colnames(v.mat)[best.abundance[[4]]])

load('saved/exhaustive/richness.RData')
sort(colnames(v.mat)[best.models$m.richness])
sort(colnames(v.mat)[best.richness[[4]]])

load('saved/exhaustive/phenology.RData')
sort(colnames(v.mat)[best.models$m.phenology])
sort(colnames(v.mat)[best.phenology[[4]]])




## *******************************************************
setwd('~/Dropbox/plant_selection_tool/tool')
rm(list=ls())
source('data/manage/src/make_tool_source.R')
load('data/modified/source-native.RData')
source('R/init.R')
v.mat <- d$v.mat
bloom.times <- d$bloom.times
flight.times <- d$flight.times
plants.by.polli <- d$plants.by.polli
## *******************************************************

load('saved/best-models/9.RData')
score(c(abundance), best.models[[1]]$best.model)
score(c(richness), best.models[[1]]$best.model)

plot(sapply(2:15, function(x)
            phenology(get.best(c('phenology'), x))))

plot(sapply(2:15, function(x)
            richness(get.best(c('richness'), x))))


abundance(get.best(c('abundance'), 9))
abundance(get.best(c('richness'), 9))
abundance(get.best(c('phenology'), 9))

phenology(get.best(c('abundance'), 9))
phenology(get.best(c('richness'), 9))
phenology(get.best(c('phenology'), 9))

phenology(get.best(c('abundance'), 9))
phenology(get.best(c('richness'), 9))
phenology(get.best(c('phenology'), 9))
