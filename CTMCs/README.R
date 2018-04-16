## ---- echo=FALSE---------------------------------------------------------
suppressPackageStartupMessages(library(magrittr, quietly = TRUE))
suppressPackageStartupMessages(library(rlang, quietly = TRUE))
suppressPackageStartupMessages(library(tibble, quietly = TRUE))
suppressPackageStartupMessages(library(expm, quietly = TRUE))

## ------------------------------------------------------------------------
cons <- function(car, cdr) list(car = car, cdr = cdr)
lst_length <- function(lst) {
  len <- 0
  while (!is.null(lst)) {
    lst <- lst$cdr
    len <- len + 1
  }
  len
}
lst_to_list <- function(lst) {
  v <- vector(mode = "list", length = lst_length(lst))
  index <- 1
  while (!is.null(lst)) {
    v[[index]] <- lst$car
    lst <- lst$cdr
    index <- index + 1
  }
  v
}

collect_symbols_rec <- function(expr, lst, bound) {
  if (is.symbol(expr) && expr != "") {
    if (as.character(expr) %in% bound) lst
    else cons(as.character(expr), lst)

  } else if (is.pairlist(expr)) {
    for (i in seq_along(expr)) {
      lst <- collect_symbols_rec(expr[[i]], lst, bound)
    }
    lst

  } else if (is.call(expr)) {
    if (expr[[1]] == as.symbol("function"))
      bound <- c(names(expr[[2]]), bound)

    for (i in 1:length(expr)) {
      lst <- collect_symbols_rec(expr[[i]], lst, bound)
    }
    lst

  } else {
    lst
  }
}

make_args_list <- function(args) {
  res <- replicate(length(args), substitute())
  names(res) <- args
  as.pairlist(res)
}

## ------------------------------------------------------------------------
collect_symbols_q <- function(expr, env) {
  bound <- c()
  lst <- collect_symbols_rec(expr, NULL, bound)
  lst %>% lst_to_list() %>% unique() %>%
    purrr::discard(exists, env) %>%
    unlist()
}

## ------------------------------------------------------------------------
ctmc <- function()
  structure(list(from = NULL,
                 rate = NULL,
                 to = NULL,
                 params = NULL),
            class = "ctmc")

## ------------------------------------------------------------------------
add_edge <- function(ctmc, from, rate, to) {
  from <- enexpr(from) ; stopifnot(is_symbol(from))
  to <- enexpr(to) ; stopifnot(is_symbol(to))
  
  from <- as_string(from)
  to <- as_string(to)
  
  ctmc$from <- cons(from, ctmc$from)
  ctmc$to <- cons(to, ctmc$to)

  r <- enquo(rate)
  ctmc$rate <- cons(r, ctmc$rate)
  ctmc$params <- cons(collect_symbols_q(get_expr(r), get_env(r)), 
                       ctmc$params)

  ctmc
}

## ------------------------------------------------------------------------
print.ctmc <- function(x, ...) {
  from <- lst_to_list(x$from) %>% rev()
  to <- lst_to_list(x$to) %>% rev()
  rate <- lst_to_list(x$rate) %>% rev()
  parameters <- lst_to_list(x$params) %>% 
    unlist() %>% unique() %>% rev()

  cat("CTMC:\\n")
  cat("parameters:", paste(parameters), "\\n")
  cat("transitions:\\n")
  for (i in seq_along(from)) {
    cat(from[[i]], "->", to[[i]], 
        "\\t[", deparse(get_expr(rate[[i]])), "]\\n")
  }
  cat("\\n")
}

## ------------------------------------------------------------------------
x <- 2
m <- ctmc() %>%
  add_edge(foo, a, bar) %>%
  add_edge(foo, 2*a, baz) %>%
  add_edge(foo, 4, qux) %>%
  add_edge(bar, b, baz) %>%
  add_edge(baz, a + x*b, qux) %>%
  add_edge(qux, a + UQ(x)*b, foo)
m

## ------------------------------------------------------------------------
get_rate_matrix_function <- function(ctmc) {
  from <- lst_to_list(ctmc$from) %>% rev()
  to <- lst_to_list(ctmc$to) %>% rev()
  rate <- lst_to_list(ctmc$rate) %>% rev()

  nodes <- c(from, to) %>% unique() %>% unlist()
  parameters <- lst_to_list(ctmc$params) %>% 
    unlist() %>% unique() %>% rev()

  n <- length(nodes)

  f <- function() {
    args <- as_list(environment())
    Q <- matrix(0, nrow = n, ncol = n)
    rownames(Q) <- colnames(Q) <- nodes
    for (i in seq_along(from)) {
      Q[from[[i]], to[[i]]] <- eval_tidy(rate[[i]], args)
    }
    diag(Q) <- -rowSums(Q)
    Q
  }
  formals(f) <- make_args_list(parameters)

  f
}

## ------------------------------------------------------------------------
Qf <- m %>% get_rate_matrix_function()
Qf

## ------------------------------------------------------------------------
Qf(a = 2, b = 4)

## ------------------------------------------------------------------------
x <- 1
Qf(a = 2, b = 4)

## ------------------------------------------------------------------------
ctmc_trace <- function(ctmc) {
  nodes <- c(lst_to_list(ctmc$from), lst_to_list(ctmc$to)) %>%
    unique %>% unlist
  structure(list(nodes = nodes, states = NULL, at = NULL),
            class = "ctmc_trace")
}

## ------------------------------------------------------------------------
add_observation <- function(trace, state, at) {
  state <- enexpr(state)
  stopifnot(is_symbol(state))
  state <- as_string(state)
  stopifnot(state %in% trace$nodes)
  stopifnot(is.numeric(at))
  stopifnot(is.null(trace$at) || at > trace$at$car)

  trace$states <- cons(state, trace$states)
  trace$at <- cons(at, trace$at)

  trace
}

## ------------------------------------------------------------------------
as_tibble.ctmc_trace <- function(x, ...) {
  states <- x$states %>% lst_to_list() %>% unlist() %>% rev()
  at <- x$at %>% lst_to_list() %>% unlist() %>% rev()
  tibble(state = states, at = at)
}
print.ctmc_trace <- function(x, ...) {
  df <- as_tibble(x)
  cat("CTMC trace:\\n")
  print(df)
}

## ------------------------------------------------------------------------
tr <- ctmc_trace(m) %>%
  add_observation(foo, at = 0.0) %>%
  add_observation(bar, at = 0.1) %>%
  add_observation(baz, at = 0.3) %>% 
  add_observation(qux, at = 0.5) %>%
  add_observation(foo, at = 0.7) %>%
  add_observation(baz, at = 1.1)
tr

## ------------------------------------------------------------------------
transition_probabilities <- function(Q, t) expm(Q * t)

## ------------------------------------------------------------------------
get_likelihood_function <- function(ctmc, trace) {
  rate_func <- ctmc %>% get_rate_matrix_function()
  trace_df <- as_tibble(trace)
  
  lhd_function <- function() {
    args <- as_list(environment())
    Q <- do.call(rate_func, args)
    
    n <- length(trace_df$state)
    from <- trace_df$state[-n]
    to <- trace_df$state[-1]
    delta_t <- trace_df$at[-1] - trace_df$at[-n]
    
    lhd <- 1
    for (i in seq_along(from)) {
      P <- transition_probabilities(Q, delta_t[i])
      lhd <- lhd * P[from[i],to[i]]
    }
    lhd
  }
  formals(lhd_function) <- formals(rate_func)
  
  lhd_function
}

## ------------------------------------------------------------------------
lhd <- m %>% get_likelihood_function(tr)
lhd(a = 2, b = 4)

