proposal <- function(m) {
  x1 <- sample(which(m), size=1)
  x2 <- sample(which(!m), size=1)
  m[x1] <- !m[x1]
  m[x2] <- !m[x2]
  m
}

step <- function(m) {
 m.p <- proposal(m)
 alpha <- target(m.p) / target(m)
 if ( alpha > 1 || runif(1) < alpha ) m.p else m
}

run.mcmc <- function(m, nsteps) {
 out <- vector(mode="list", length=nsteps)
 for ( i in seq_len(nsteps) )
   m <- out[[i]] <- step(m)
 out
}
