# Environments and expressions {#sec:env_and_expr}


```r
library(rlang)
```


## Scopes and environments


```r
x <- 1
f <- function(y) {
  z <- 3
  function() x + y + z
}
g <- f(2)
h <- f(3)
g()
```

```
## [1] 6
```

```r
h()
```

```
## [1] 7
```



```r
environment(f)
```

```
## <environment: R_GlobalEnv>
```

```r
environment(g)
```

```
## <environment: 0x7fe8db839150>
```

```r
environment(h)
```

```
## <environment: 0x7fe8db851958>
```


## Default parameters, lazy evaluation, and promises


```r
f <- function(x, y) x
f(2, stop("error!"))
```

```
## [1] 2
```


```r
f <- function(y, z = 2 * y) y + z
```


```r
f(2, 1)
```

```
## [1] 3
```


```r
f(2)
```

```
## [1] 6
```


```r
y <- 2
f(2 * y)
```

```
## [1] 12
```



```r
g <- function(x) f(2 * x)
g(2 * y)
```

```
## [1] 24
```


```r
h <- function(x, y = 2 * w) {
  w <- 2
  x + y
}
h(1)
```

```
## [1] 5
```


```r
h <- function(x, y = 2 * w) {
  res <- x + y
  w <- 2
  res
}
h(1)
```

```
## Error in h(1): object 'w' not found
```


```r
h <- function(x, y = 2 * w) {
  w <- 1
  res <- x + y
  w <- 2
  res
}
h(1)
```

```
## [1] 3
```


```r
make_adder <- function(n) function(m) n + m
```


```r
add_1 <- make_adder(1)
add_2 <- make_adder(2)
add_1(1)
```

```
## [1] 2
```

```r
add_2(1)
```

```
## [1] 3
```


```r
adders <- vector("list", 3)
for (i in 1:3) adders[[i]] <- make_adder(i)
```


```r
adders[[1]](1)
```

```
## [1] 4
```


```r
i <- 1
adders[[1]](1)
```

```
## [1] 4
```


```r
adders[[2]](1)
```

```
## [1] 2
```


```r
make_adder <- function(n) {
  force(n)
  function(m) n + m
}
for (i in 1:3) adders[[i]] <- make_adder(i)
for (i in 1:3) print(adders[[i]](0))
```

```
## [1] 1
## [1] 2
## [1] 3
```


## Quotes and non-standard evaluation


```r
ex1 <- quote(2 * x + y)
ex1
```

```
## 2 * x + y
```

```r
f <- function(ex) substitute(ex)
ex2 <- f(2 * x + y)
ex2
```

```
## 2 * x + y
```


```r
g <- rlang::new_function(alist(x=, y=), body = ex1)
g
```

```
## function (x, y) 
## 2 * x + y
```

```r
g(1,3)
```

```
## [1] 5
```


```r
x <- 1
y <- 3
eval(ex1)
```

```
## [1] 5
```


```r
h <- function(x, y) eval(ex1)
h
```

```
## function(x, y) eval(ex1)
```

```r
h(1,3)
```

```
## [1] 5
```


```r
h <- function(x, y) eval(ex1, rlang::caller_env())
x <- y <- 1
h(4,4)
```

```
## [1] 3
```


```r
f <- function(x) rlang::new_function(alist(y=), ex1)
f(2)
```

```
## function (y) 
## 2 * x + y
## <environment: 0x7fe8db858ca0>
```

```r
f(2)(2)
```

```
## [1] 6
```


```r
g <- function(x) {
  rlang::new_function(alist(y=), ex1, rlang::caller_env())
}
g(2)
```

```
## function (y) 
## 2 * x + y
```

```r
g(2)(2)
```

```
## [1] 4
```



```r
eval(ex1, list(x = 4, y = 8))
```

```
## [1] 16
```

```r
df <- data.frame(x = 1:4, y = 1:4)
eval(ex1, df)
```

```
## [1]  3  6  9 12
```


```r
f <- function(expr, data, y) eval(expr, data)
g <- function(expr, data, y) eval(expr, data, rlang::caller_env())
```


```r
df <- data.frame(x = 1:4)
y <- 1:4
f(quote(x + y), df, y = 5:8) == 1:4 + 5:8
```

```
## [1] TRUE TRUE TRUE TRUE
```

```r
g(quote(x + y), df, y = 5:8) == 1:4 + 1:4
```

```
## [1] TRUE TRUE TRUE TRUE
```


```r
f <- function(expr, data) eval(expr, data, rlang::caller_env())
f(quote(u + v), data.frame(u = 1:4, v = 1:4))
```

```
## [1] 2 4 6 8
```



```r
fq <- function(expr, data) {
  eval(substitute(expr), data, rlang::caller_env())
}
fq(u + v, data.frame(u = 1:4, v = 1:4))
```

```
## [1] 2 4 6 8
```



```r
g <- function(expr) fq(expr, data.frame(u = 1:4, v = 1:4))
g(u + v)
```

```
## Error in eval(substitute(expr), data, rlang::caller_env()): object 'u' not found
```


```r
u <- v <- 5:8
g(u + v)
```

```
## [1] 10 12 14 16
```


```r
g <- function(expr) {
  fq(substitute(expr), data.frame(u = 1:4, v = 1:4))
}
g(u + v)
```

```
## expr
```



```r
g <- function(expr) {
  fq(quote(expr), data.frame(u = 1:4, v = 1:4))
}
g(u + v)
```

```
## expr
```


```r
g <- function(expr) {
  f(substitute(expr), data.frame(u = 1:4, v = 1:4))
}
g(u + v)
```

```
## [1] 2 4 6 8
```



```r
f <- function(expr, data) eval(expr, data, rlang::caller_env())
fq <- function(expr, data) f(substitute(expr), data)
fq(u + v, data.frame(u = 1:4, v = 1:4))
```

```
## [1] 2 4 6 8
```



```r
g <- function(x, y, z) {
  w <- x + y + z
  f(quote(w + u + v), data.frame(u = 1:4, v = 1:4))
}
h <- function(x, y, z) {
  w <- x + y + z
  fq(w + u + v, data.frame(u = 1:4, v = 1:4))
}
```



```r
g(1:4, 1:4, 1:4) == (1:4 + 1:4 + 1:4) + 1:4 + 1:4
```

```
## [1] TRUE TRUE TRUE TRUE
```

```r
h(1:4, 1:4, 1:4) == (1:4 + 1:4 + 1:4) + 1:4 + 1:4
```

```
## Error in eval(expr, data, rlang::caller_env()): object 'w' not found
```


```r
ff <- function(expr, data) {
  eval(expr[[2]], data, environment(expr))
}
ffq <- function(expr, data) {
  expr <- eval(substitute(~ expr))
  environment(expr) <- rlang::caller_env()
  ff(expr, data)
}
```



```r
g <- function(x, y, z) {
  w <- x + y + z
  ff(~ w + u + v, data.frame(u = 1:4, v = 1:4))
}
h <- function(x, y, z) {
  w <- x + y + z
  ffq(w + u + v, data.frame(u = 1:4, v = 1:4))
}
```


```r
g(1:4, 1:4, 1:4) == (1:4 + 1:4 + 1:4) + 1:4 + 1:4
```

```
## [1] TRUE TRUE TRUE TRUE
```

```r
h(1:4, 1:4, 1:4) == (1:4 + 1:4 + 1:4) + 1:4 + 1:4
```

```
## [1] TRUE TRUE TRUE TRUE
```

