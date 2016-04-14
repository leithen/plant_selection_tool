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
num.supported <- function(m) {
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
richness.phenology <- function(m)
  richness(m)*phenology(m)
abundance.richness.phenology <- function(m)
  abundance(m)*richness(m)*phenology(m)
abundance.num.supported <- function(m)
  abundance(m)*num.supported(m)
richness.num.supported <- function(m)
  richness(m)*num.supported(m)
abundance.richness.num.supported <- function(m)
  abundance(m)*richness(m)*num.supported(m)

score <- function(f.list, m) {
  g <- function(f.list, m)
    sapply(f.list, function(y) y(m))
  prod(g(f.list, m))
}

get.best <- function(crit, s, k) {
  load(sprintf('saved/best-models/%s/%d.RData', s, k))
  cases <- lapply(best.models, function(x) x['case'][[1]])
  best <- which(sapply(cases, function(x)
                       length(x)==length(crit) & all(crit%in%x)))
  sort(best.models[[best]]['best.model'][[1]])
}

get.naive <- function(s, k) {
  load(sprintf('saved/naive-models/%s/%d.RData', s, k))
  sort(best.naive)
}
