## ------------------------------------------------------------------------
library(magrittr) # for the %>% operator
library(Matrix)   # for the `exam` function

## ------------------------------------------------------------------------
cons <- function(car, cdr) list(car = car, cdr = cdr)
dag <- function() structure(list(edges = NULL), class = "dag")
`%=>%` <- function(from, to) c(from, to)
`+.dag` <- function(dag, edge) {
    dag$edges <- cons(edge, dag$edges)
    dag
}

## ------------------------------------------------------------------------
dag() + 
  "foo" %=>% "bar" +
  "bar" %=>% "baz"

## ------------------------------------------------------------------------
add_edge <- function(dag, from, to) {
  dag$edges <- cons(c(from, to), dag$edges)
  dag
}
add_edge(add_edge(dag(), "foo", "bar"), "bar", "baz")

## ------------------------------------------------------------------------
dag() %>% add_edge("foo", "bar") %>% add_edge("bar", "baz")

## ------------------------------------------------------------------------
node <- function(name) structure(name, class = "node")
`>.node` <- function(from, to) c(from, to)

## ------------------------------------------------------------------------
node("foo") > node("bar")

## ------------------------------------------------------------------------
dag() + node("foo") > node("bar")

## ------------------------------------------------------------------------
dag() + (node("foo") > node("bar"))

## ------------------------------------------------------------------------
`|.dag` <- function(dag, edge) {
  dag$edges <- cons(edge, dag$edges)
  dag
}
dag() | node("foo") > node("bar")

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

## ------------------------------------------------------------------------
ctmc <- function() 
  structure(list(from = NULL, 
                 rate = NULL, 
                 to = NULL), 
            class = "ctmc")
            
add_edge <- function(ctmc, from, rate, to) {
  ctmc$from <- cons(from, ctmc$from)
  ctmc$rate <- cons(rate, ctmc$rate)
  ctmc$to <- cons(to, ctmc$to)
  ctmc
}

## ------------------------------------------------------------------------
rate_matrix <- function(ctmc) {
  from <- lst_to_list(ctmc$from)
  to <- lst_to_list(ctmc$to)
  rate <- lst_to_list(ctmc$rate)
  nodes <- c(from, to) %>% unique %>% unlist
  
  n <- length(nodes)
  Q <- matrix(0, nrow = n, ncol = n)
  rownames(Q) <- colnames(Q) <- nodes
  
  for (i in seq_along(from)) {
    Q[from[[i]], to[[i]]] <- rate[[i]]
  }
  
  diag(Q) <- - rowSums(Q)
  
  Q
}

## ------------------------------------------------------------------------
Q <- ctmc() %>% 
  add_edge("foo", 1, "bar") %>% 
  add_edge("foo", 2, "baz") %>% 
  add_edge("bar", 2, "baz") %>% 
  rate_matrix()
Q

## ------------------------------------------------------------------------
transitions_over_time <- function(Q, t) expm(Q * t)
P <- Q %>% transitions_over_time(0.2)
P

## ------------------------------------------------------------------------
probs <- c(foo=0.1, bar=0.9, baz=0.0)

## ------------------------------------------------------------------------
evolve <- function(probs, Q, t) {
  probs <- probs[rownames(Q)]
  probs %*%transitions_over_time(Q, t)
}
probs %>% evolve(Q, 0.2)

## ------------------------------------------------------------------------
evolve <- function(Q, t, probs) {
  probs <- probs[rownames(Q)]
  probs %*%transitions_over_time(Q, t)
}
Q %>% evolve(0.2, probs)

## ------------------------------------------------------------------------
ctmc() %>% 
  add_edge("foo", 1, "bar") %>% 
  add_edge("foo", 2, "baz") %>% 
  add_edge("bar", 2, "baz") %>% 
  rate_matrix() %>%
  evolve(0.2, probs)

