#### by Valderio Reisen  -- Dec.2005

## MM:  This is 'not optimal' -- and I may have better in ../filters.R ? <<< FIXME >>>
diffseries <- function(x, d)
{
    x <- as.data.frame(x)
    names(x) <- "series"
    x <- x$series
    if (NCOL(x) > 1)
        stop("only implemented for univariate time series")
    if (any(is.na(x)))
        stop("NAs in x")
    n <- length(x)
    stopifnot(n >= 2)
    x <- x - mean(x)
    PI <- numeric(n)
    PI[1] <- -d
    for (k in 2:n) {
        PI[k] <- PI[k-1]*(k - 1 - d)/k
    }
    ydiff <- x
    for (i in 2:n) {
        ydiff[i] <- x[i] + sum(PI[1:(i-1)]*x[(i-1):1])
    }
    ## return numeric!
    ydiff
}

# ydiff must be a result returned by diffseries (in the fracdiff package).
# that means that (ydiff[1] == x[1] - mean(x)) of the original series x.
diffseriesinv <- function(ydiff, d, xi) {
  if (d %% 1 == 0 & d > 0) # order of integration is a positive integer
    return(diffinv(tail(ydiff, -d), differences = d, xi = head(xi, d)))
  
  n <- length(ydiff)
  PI <- numeric(n)
  PI[1] <- -d
  for (k in 2:n) {
    PI[k] <- PI[k - 1] * (k - 1 - d) / k
  }
  
  ymean <- head(xi, 1) - ydiff[1]
  x <- numeric(n)
  x[1] <- head(xi, 1)
  for (i in 2:n) {
    x[i] <- ydiff[i] + ymean - sum(PI[1:(i - 1)] * (x[(i - 1):1] - ymean))
  }
  x
}

