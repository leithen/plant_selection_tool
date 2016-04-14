#### (5) Step the population through one full generation:
ga.step <- function(state, s, p.mutate, p.sex, p.rec, fitness) {
  ## Mutation
  state.m <- update.state(state, mutate(state=state,
                                        p.mutate=p.mutate,
                                        fitness=fitness))

  ## Selection
  state.s <- update.state(state, select(state=state.m, s=s))
  
  ## Recombination
  state.r <- update.state(state, recombine(state=state.s,
                                           popn0=state$popn,
                                           p.sex=p.sex, p.rec=p.rec,
                                           fitness=fitness))
  state.r
}

## Bookkeeping to make sure that we keep the best model
update.state <- function(s0, s1) {
  if ( max(s1$w) > s0$best.w ) {
    s1$best.w     <- max(s1$w)
    s1$best.model <- s1$popn[[which.max(s1$w)]]
  } else {
    s1$best.w     <- s0$best.w
    s1$best.model <- s0$best.model
  }
  s1
}

## Run the GA for ngens generations, collecting the best fitness over
## time, and returning the state and best model.
run.ga <- function(n.plants, n.plants.tot, N, n.gens, s, p.mutate,
                   p.sex, p.rec, fitness) {
  x <- initial.popn(N, n.plants, n.plants.tot,
                    fitness=fitness)
  out <- vector("numeric", n.gens)
  for ( i in seq_len(n.gens) ){
    x <- ga.step(x, s, p.mutate, p.sex, p.rec, fitness)
    out[i] <- x$best.w
  }
  list(best.w=x$best.w, best.model=x$best.model, best.w.t=out)
}
