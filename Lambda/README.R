## ---- echo=FALSE---------------------------------------------------------
suppressPackageStartupMessages(library(rlang, quietly = TRUE))

## ------------------------------------------------------------------------
library(rlang)

## ------------------------------------------------------------------------
sapply(1:4, function(x) x**2)

## ------------------------------------------------------------------------
(1:4)**2

## ------------------------------------------------------------------------
make_args_list <- function(args) {
  res <- replicate(length(args), substitute())
  names(res) <- args
  as.pairlist(res)
}

## ------------------------------------------------------------------------
`:=` <- function(header, body) {
  header <- substitute(header)
  body <- substitute(body)
  args <- make_args_list(as.character(header))
  new_function(args, body, caller_env())
} 

## ------------------------------------------------------------------------
sapply(1:4, x := x**2)

## ------------------------------------------------------------------------
mapply(.(x,y) := x*y, x = 1:6, y = 1:2)

## ------------------------------------------------------------------------
mapply(x(x,y) := x*y, x = 1:6, y = 1:2)

## ------------------------------------------------------------------------
`:=` <- function(header, body) {
  header <- substitute(header)
  if (is.call(header)) header <- header[-1]
  body <- substitute(body)
  args <- make_args_list(as.character(header))
  new_function(args, body, caller_env())
} 

## ------------------------------------------------------------------------
mapply(x(x,y) := x*y, x = 1:6, y = 1:2)

## ------------------------------------------------------------------------
lambda <- function(...) {
  spec <- eval(substitute(alist(...)))
  n <- length(spec)
  args <- make_args_list(spec[-n])
  body <- spec[[n]]
  new_function(args, body, caller_env())
}

## ------------------------------------------------------------------------
sapply(1:4, lambda(x, 4 * x**2))
mapply(lambda(x, y, y*x), x = 1:4, y = 4:7)

## ------------------------------------------------------------------------
lambda <- structure(NA, class = "lambda")
`[.lambda` <- function(x, ...) {
  spec <- eval(substitute(alist(...)))
  n <- length(spec)
  args <- make_args_list(spec[-n])
  body <- spec[[n]]
  new_function(args, body, caller_env())
}

## ------------------------------------------------------------------------
sapply(1:4, lambda[x, 4 * x**2])
mapply(lambda[x, y, y*x], x = 1:4, y = 4:7)

