# List comprehension



```r
qsort <- function(lst) {
  n <- length(lst)
  if (n < 2) return(lst)
  
  pivot <- lst[[sample(n, size = 1)]]
  smaller <- Filter(function(x) x < pivot, lst)
  equal <- Filter(function(x) x == pivot, lst)
  larger <- Filter(function(x) x > pivot, lst)
  c(qsort(smaller), equal, qsort(larger))
}
(lst <- sample(1:10))
```

```
##  [1]  4  5  7 10  1  9  8  2  6  3
```

```r
unlist(qsort(lst))
```

```
##  [1]  1  2  3  4  5  6  7  8  9 10
```



```r
library(rlang)
library(purrr)
```

```
## 
## Attaching package: 'purrr'
```

```
## The following objects are masked from 'package:rlang':
## 
##     %@%, %||%, as_function, flatten, flatten_chr, flatten_dbl,
##     flatten_int, flatten_lgl, invoke, list_along, modify, prepend,
##     rep_along, splice
```

```r
lc <- function(expr, ...) {
  expr <- enquo(expr)
  rest <- quos(...)
  
  lists <- map(rest[names(rest) != ""], eval_tidy)
  predicates <- map(rest[names(rest) == ""], get_expr)
  
  keep_index <- rep(TRUE, length(lists[[1]]))
  for (pred in predicates) {
    p <- new_function(lists, body = pred, env = get_env(expr))
    keep_index <- keep_index & unlist(pmap(lists, p))
  }
  filtered_lists <- map(lists, ~.x[keep_index])
  
  f <- new_function(lists, body = get_expr(expr), env = get_env(expr))
  pmap(filtered_lists, f)
}
```


```r
qsort <- function(lst) {
  n <- length(lst)
  if (n < 2) return(lst)
  
  pivot <- lst[[sample(n, size = 1)]]
  smaller <- lc(x, x = lst, x < pivot)
  equal <- lc(x, x = lst, x == pivot)
  larger <- lc(x, x = lst, x > pivot)
  
  c(qsort(smaller), equal, qsort(larger))
}

(lst <- sample(1:10))
```

```
##  [1]  4  7  5  3  8  9  1  6  2 10
```

```r
unlist(qsort(lst))
```

```
##  [1]  1  2  3  4  5  6  7  8  9 10
```


```r
not_primes <- lc(seq(from = 2*x, to = 100, by = x), x = 2:10) %>% 
    unlist %>% unique
not_primes
```

```
##  [1]   4   6   8  10  12  14  16  18  20  22  24  26  28  30  32  34  36
## [18]  38  40  42  44  46  48  50  52  54  56  58  60  62  64  66  68  70
## [35]  72  74  76  78  80  82  84  86  88  90  92  94  96  98 100   9  15
## [52]  21  27  33  39  45  51  57  63  69  75  81  87  93  99  25  35  55
## [69]  65  85  95  49  77  91
```

```r
primes <- lc(p, p = 2:100, !(p %in% not_primes)) %>% unlist
primes
```

```
##  [1]  2  3  5  7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83
## [24] 89 97
```


```r
get_primes <- function(n) {
  not_primes <- lc(seq(from = 2*x, to = n, by = x), x = 2:sqrt(n)) %>% 
      unlist %>% unique
  lc(p, p = 2:n, !(p %in% not_primes)) %>% unlist
}
get_primes(100)
```

```
##  [1]  2  3  5  7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83
## [24] 89 97
```


```r
get_primes <- function(n) {
  candidates <- 2:n
  primes <- NULL
  while (length(candidates) > 0) {
    p <- candidates[[1]]
    primes <- cons(p, primes)
    candidates <- lc(x, x = candidates, x %% p != 0)
  }
  primes %>% lst_to_list %>% unlist %>% rev
}
get_primes(100) 
```

```
## Error in cons(p, primes): could not find function "cons"
```


```r
zip <- function(x, y) {
  lc(c(x,y), x = x, y = y) %>% { do.call(rbind,.) }
}
zip(1:4,1:4)
```

```
##      [,1] [,2]
## [1,]    1    1
## [2,]    2    2
## [3,]    3    3
## [4,]    4    4
```
