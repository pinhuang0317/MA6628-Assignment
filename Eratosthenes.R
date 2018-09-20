
Eratosthenes <- function(n) {
  if (n >= 2) {
    sieve <- seq(2,n)
    primes <- c()
    for (i in seq(2,n)) {
      if (any(sieve == i)) {
        primes <- c(primes, i)
        sieve <- c(sieve[(sieve %% i) != 0], i)
      }
    }
    return(primes)
  } else {
    stop("Input value of n should be at least 2.")
  }
}