# Lambda expressions



```r
library(rlang)
```


## Lambda expressions


```r
sapply(1:4, function(x) x**2)
```

```
## [1]  1  4  9 16
```


```r
(1:4)**2
```

```
## [1]  1  4  9 16
```


```r
make_args_list <- function(args) {
  res <- replicate(length(args), substitute())
  names(res) <- args
  as.pairlist(res)
}
```


```r
`:=` <- function(header, body) {
  header <- substitute(header)
  body <- substitute(body)
  args <- make_args_list(as.character(header))
  new_function(args, body, caller_env())
} 
```


```r
sapply(1:4, x := x**2)
```

```
## [1]  1  4  9 16
```



```r
mapply(.(x,y) := x*y, x = 1:6, y = 1:2)
```

```
## [1]  1  4  3  8  5 12
```


```r
mapply(x(x,y) := x*y, x = 1:6, y = 1:2)
```

```
## Error in (function (x, x, y) : argument 1 matches multiple formal arguments
```


```r
`:=` <- function(header, body) {
  header <- substitute(header)
  if (is.call(header)) header <- header[-1]
  body <- substitute(body)
  args <- make_args_list(as.character(header))
  new_function(args, body, caller_env())
} 
```


```r
mapply(x(x,y) := x*y, x = 1:6, y = 1:2)
```

```
## [1]  1  4  3  8  5 12
```


## Experiments with alternatives to the syntax



```r
lambda <- function(...) {
  spec <- eval(substitute(alist(...)))
  n <- length(spec)
  args <- make_args_list(spec[-n])
  body <- spec[[n]]
  new_function(args, body, caller_env())
}
```


```r
sapply(1:4, lambda(x, 4 * x**2))
```

```
## [1]  4 16 36 64
```

```r
mapply(lambda(x, y, y*x), x = 1:4, y = 4:7)
```

```
## [1]  4 10 18 28
```


```r
lambda <- structure(NA, class = "lambda")
`[.lambda` <- function(x, ...) {
  spec <- eval(substitute(alist(...)))
  n <- length(spec)
  args <- make_args_list(spec[-n])
  body <- spec[[n]]
  new_function(args, body, caller_env())
}
```


```r
sapply(1:4, lambda[x, 4 * x**2])
```

```
## [1]  4 16 36 64
```

```r
mapply(lambda[x, y, y*x], x = 1:4, y = 4:7)
```

```
## [1]  4 10 18 28
```

