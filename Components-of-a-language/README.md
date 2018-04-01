# Components of a programming language

This chapter describes the various components of computer languages and the phases involved in processing a domain specific language. We will use the following packages:


```r
library(magrittr) # for the %>% operator
library(Matrix)   # for the `exam` function
```


## Text, tokens, grammars, and semantics

## Specifying a grammar


```r
cons <- function(car, cdr) list(car = car, cdr = cdr)
dag <- function() structure(list(edges = NULL), class = "dag")
`%=>%` <- function(from, to) c(from, to)
`+.dag` <- function(dag, edge) {
    dag$edges <- cons(edge, dag$edges)
    dag
}
```


```r
dag() + 
  "foo" %=>% "bar" +
  "bar" %=>% "baz"
```

```
## $edges
## $edges$car
## [1] "bar" "baz"
## 
## $edges$cdr
## $edges$cdr$car
## [1] "foo" "bar"
## 
## $edges$cdr$cdr
## NULL
## 
## 
## 
## attr(,"class")
## [1] "dag"
```


```r
add_edge <- function(dag, from, to) {
  dag$edges <- cons(c(from, to), dag$edges)
  dag
}
add_edge(add_edge(dag(), "foo", "bar"), "bar", "baz")
```

```
## $edges
## $edges$car
## [1] "bar" "baz"
## 
## $edges$cdr
## $edges$cdr$car
## [1] "foo" "bar"
## 
## $edges$cdr$cdr
## NULL
## 
## 
## 
## attr(,"class")
## [1] "dag"
```


```r
dag() %>% add_edge("foo", "bar") %>% add_edge("bar", "baz")
```

```
## $edges
## $edges$car
## [1] "bar" "baz"
## 
## $edges$cdr
## $edges$cdr$car
## [1] "foo" "bar"
## 
## $edges$cdr$cdr
## NULL
## 
## 
## 
## attr(,"class")
## [1] "dag"
```


```r
node <- function(name) structure(name, class = "node")
`>.node` <- function(from, to) c(from, to)
```


```r
node("foo") > node("bar")
```

```
## [1] "foo" "bar"
```


```r
dag() + node("foo") > node("bar")
```

```
## $edges
## $edges$car
## [1] "foo"
## attr(,"class")
## [1] "node"
## 
## $edges$cdr
## NULL
## 
## 
## [[2]]
## [1] "bar"
```


```r
dag() + (node("foo") > node("bar"))
```

```
## $edges
## $edges$car
## [1] "foo" "bar"
## 
## $edges$cdr
## NULL
## 
## 
## attr(,"class")
## [1] "dag"
```


```r
`|.dag` <- function(dag, edge) {
  dag$edges <- cons(edge, dag$edges)
  dag
}
dag() | node("foo") > node("bar")
```

```
## $edges
## $edges$car
## [1] "foo" "bar"
## 
## $edges$cdr
## NULL
## 
## 
## attr(,"class")
## [1] "dag"
```


## Designing semantics


```r
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
```


```r
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
```


```r
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
```


```r
Q <- ctmc() %>% 
  add_edge("foo", 1, "bar") %>% 
  add_edge("foo", 2, "baz") %>% 
  add_edge("bar", 2, "baz") %>% 
  rate_matrix()
Q
```

```
##     bar foo baz
## bar  -2   0   2
## foo   1  -3   2
## baz   0   0   0
```


```r
transitions_over_time <- function(Q, t) expm(Q * t)
P <- Q %>% transitions_over_time(0.2)
P
```

```
## 3 x 3 Matrix of class "dgeMatrix"
##           bar       foo     baz
## bar 0.6703200 0.0000000 0.32968
## foo 0.1215084 0.5488116 0.32968
## baz 0.0000000 0.0000000 1.00000
```


```r
probs <- c(foo=0.1, bar=0.9, baz=0.0)
```


```r
evolve <- function(probs, Q, t) {
  probs <- probs[rownames(Q)]
  probs %*%transitions_over_time(Q, t)
}
probs %>% evolve(Q, 0.2)
```

```
## 1 x 3 Matrix of class "dgeMatrix"
##            bar        foo     baz
## [1,] 0.6154389 0.05488116 0.32968
```


```r
evolve <- function(Q, t, probs) {
  probs <- probs[rownames(Q)]
  probs %*%transitions_over_time(Q, t)
}
Q %>% evolve(0.2, probs)
```

```
## 1 x 3 Matrix of class "dgeMatrix"
##            bar        foo     baz
## [1,] 0.6154389 0.05488116 0.32968
```


```r
ctmc() %>% 
  add_edge("foo", 1, "bar") %>% 
  add_edge("foo", 2, "baz") %>% 
  add_edge("bar", 2, "baz") %>% 
  rate_matrix() %>%
  evolve(0.2, probs)
```

```
## 1 x 3 Matrix of class "dgeMatrix"
##            bar        foo     baz
## [1,] 0.6154389 0.05488116 0.32968
```

