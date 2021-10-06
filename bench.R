library(cli)
prime_numbers <- function(n) {
  if (n >= 2) {
    x <- seq(2, n)
    prime_nums <- c()
    for (i in seq(2, n)) {
      if (any(x == i)) {
        prime_nums <- c(prime_nums, i)
        x <- c(x[(x %% i) != 0], i)
      }
    }
    return(prime_nums)
  } else {
    stop("Input number should be at least 2.")
  }
}
single_t_bench <- system.time({
  prime_numbers(300000)
})

cli_h1("Single threaded prime test")
print(single_t_bench)

library(purrr)
library(parallel)
library(future)
library(furrr)

plan(multisession, workers = parallel::detectCores())
future_bench <- system.time({
  furrr::future_map(
    .x = seq(10000, 300000, by = 5000),
    .f = prime_numbers
  )
})

cli_h1("Future benchmark prime numbers")
print(future_bench)
