#### (1) Initial state:
## initial.popn randomly generates N solutions for n.plants.
initial.popn <- function(N, n.plants, n.plants.tot, fitness) {
  make.model <- function(x) {
    m <- rep(FALSE, n.plants.tot)
    m[sample(n.plants.tot, n.plants)] <- TRUE
    m
  }

  popn <- lapply(seq_len(N), make.model)
  w <- sapply(popn, fitness)
  list(popn=popn,
       w=w,
       best.w=max(w),
       best.model=popn[[which.max(w)]])
}

#### (2) Mutation
## Next, take a population 'popn', with a fitness vector 'w' (where
## the ith element in 'w' gives the fitness of the ith model in
## 'popn').
##
## First, draw binomial random deviates to determine how many
## mutations will happen per model (mutations happen with probability
## 'p.mutate', and there are 'n.plants' available for mutation).
##
## Mutate the models that had at least one mutation, by passing them
## through to 'mutate'.
##
## Only mutated models need their fitness recalculated.
##
## Finally, return a list with the new population of models and their
## fitness vector.
mutate <- function(state, p.mutate, fitness) {

  popn <- state$popn
  w <- state$w
  nmutants <- rbinom(length(popn), length(popn[[1]]), p.mutate)
  nmutants <- sapply(nmutants, function(x) min(sum(popn[[1]]), x))
  mutated <- which(nmutants>0)
  
  if ( length(mutated) > 0 ) {
    popn[mutated] <-
      mapply(mutate.1, popn[mutated], nmutants[mutated],
             SIMPLIFY=FALSE)
    w[mutated] <- sapply(popn[mutated], fitness)
  }
  list(popn=popn,
       w=w,
       best.w=state$best.w,
       best.model=state$best.model)
}

## Take a model 'x', specifying which 'n.plants', and mutate it.  To
## do this (for a single mutant), randomly swap one included plant for
## one not included (i.e. a model cannot, therefore, mutate to
## itself).
mutate.1 <- function(x, n.mutants) {
  if(n.mutants > sum(x))
    cat("Mutation rate too high. Too many mutants\n")
  x1 <- sample(which(x), size=n.mutants)
  x2 <- sample(which(!x), size=n.mutants)
  x[x1] <- FALSE
  x[x2] <- TRUE
  x
}

#### (3) Selection:
## This selection regime is fairly straightforward; sample with
## replacement s*N models from the population, then take the N best
## models.
select <- function(state, s) {
  N <- length(state$popn)
  i <- sample(N, ceiling(s*N), TRUE)
  take <- i[order(state$w[i], decreasing=TRUE)[1:N]]
  list(popn=state$popn[take],
       w=state$w[take],
       best.w=state$best.w,
       best.model=state$best.model)
}

#### DIFFERENT SELECTION ALGORITHM
## select.weighted <- function(popn, w) {
##   N <- length(popn)
##   take <- sample(N, N, TRUE, w-min(w)+1)
##   list(popn=popn[take], w=w[take])
## }

#### (4) Recombiation
## Select a fraction of the population to be recombined.  These are
## recombined with randomly selected individuals in the previous
## generation (popn0).  The fitness of the recombinants is then
## calculated.
recombine <- function(state, popn0, p.sex, p.rec, fitness) {
  popn1 <- state$popn
  w1 <- state$w
  N <- length(popn1)
  nrec <- rbinom(1, N, p.sex)
  i <- sample(N, nrec)
  if ( length(i) > 0 ) {
    b <- popn0[sample(N, nrec, TRUE)]
    popn1[i] <- mapply(recombine.1, popn1[i], b, p=p.rec,
                       SIMPLIFY=FALSE)
    w1[i] <- sapply(popn1[i], fitness)
  }
  list(popn=popn1,
       w=w1,
       best.w=state$best.w,
       best.model=state$best.model)
}

## recombination a pair of models:
recombine.1 <- function(a, b, p) {
  m.a <- which(a)
  m.b <- which(b)
  in.both <- intersect(m.a, m.b)
  
  m.a <- setdiff(m.a, in.both)
  m.b <- setdiff(m.b, in.both)

  rr <- NULL
  
  if(length(m.a)>0) {
    rec <- runif(length(m.a)) < p
    if ( any(rec) ) {
      parent <- c(cumsum(rec)%%2)+1
      recombinant <- m.a
      recombinant[parent==2] <- m.b[parent==2]
      recombinant <- sort(c(recombinant, in.both))
      a <- rep(FALSE, length(a))
      a[recombinant] <- TRUE
      a
    }
  }
  a
}
