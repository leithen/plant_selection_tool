## pollinator abundance function
abundance <- function(m) sum(rowSums(v.mat[,m]))

## pollinator richness function
richness <- function(m) sum(rowSums(v.mat[,m]>0)>0)

## pollinator phenology function
phenology <- function(m) {
  geometric.mean <- function(x)
    (prod(x))^(1/length(x))
  possible.coverage <- unique(unlist(bloom.times))
  coverage <- rle(sort(unlist(bloom.times[m])))
  tmp <- rep(0, 12)
  tmp[coverage$values] <- coverage$lengths
  geometric.mean(tmp+1)
}

## number of fully supported species
numsupported <- function(m) {
  plants.by.polli <-
    apply(v.mat, 1, function(x) colnames(v.mat)[x>=1])
  m <- colnames(v.mat)[m]
  present <-
    lapply(plants.by.polli, function(x) intersect(x, m))
  any.coverage <- which(sapply(present, length)>0)
  coverage <-
    lapply(present[any.coverage], function(x)
           unique(unlist(sapply(x, function(x) bloom.times[x]))))
  covered <-
    !mapply(function(a, b) any(!a%in%b),
            flight.times[any.coverage], coverage)
  sum(covered)
}

## combination functions
abundance.richness <- function(m)
  abundance(m)*richness(m)
abundance.phenology <- function(m)
  abundance(m)*phenology(m)
phenology.richness <- function(m)
  phenology(m)*richness(m)
abundance.phenology.richness <- function(m)
  abundance(m)*phenology(m)*richness(m)
abundance.numsupported <- function(m)
  abundance(m)*numsupported(m)
numsupported.richness <- function(m)
  numsupported(m)*richness(m)
abundance.numsupported.richness <- function(m)
  abundance(m)*numsupported(m)*richness(m)

## compute score for model m
score <- function(f.list, m) {
  g <- function(f.list, m) sapply(f.list, function(y) y(m))
  prod(g(f.list, m))
}
