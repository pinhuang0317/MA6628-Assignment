is_primes <- function(n) {
  p = 2:n
  i = 1
  while (p[i] <= sqrt(n)) {
    p = p[p %% p[i] !=0 | p == p[i]]
    i = i+1
  }
  p
}

Twin_primes <- function(n) {
  if (n < 5) stop("Input value of n should be at least 5.")
  primes_less_than_N = is_primes(n)
  Twin_primes1 = primes_less_than_N[diff(primes_less_than_N) == 2]
  Twin_primes2 = Twin_primes1 + 2
  Twin_primes_set = data.frame(Twin_primes1 = Twin_primes1, Twin_primes2 = Twin_primes2)
  return(Twin_primes_set)
}

nrow(Twin_primes(1000000)) - nrow(Twin_primes(1000)) 

tail(Twin_primes(1000000))
