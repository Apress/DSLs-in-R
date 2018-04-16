# Parsing and manipulating expressions


```r
library(purrr)
library(rlang)
```

```
## 
## Attaching package: 'rlang'
```

```
## The following objects are masked from 'package:purrr':
## 
##     %@%, %||%, as_function, flatten, flatten_chr, flatten_dbl,
##     flatten_int, flatten_lgl, invoke, list_along, modify, prepend,
##     rep_along, splice
```

```r
library(magrittr)
```

```
## 
## Attaching package: 'magrittr'
```

```
## The following object is masked from 'package:rlang':
## 
##     set_names
```

```
## The following object is masked from 'package:purrr':
## 
##     set_names
```

## Quoting and evaluating


```r
quote(2 * x + y)
```

```
## 2 * x + y
```


```r
expr <- quote(2 * x + y)
class(expr)
```

```
## [1] "call"
```


```r
expr[[1]]
```

```
## `+`
```

```r
expr[[2]]
```

```
## 2 * x
```

```r
expr[[3]]
```

```
## y
```



```r
expr[[2]][[1]]
```

```
## `*`
```

```r
expr[[2]][[2]]
```

```
## [1] 2
```

```r
expr[[2]][[3]]
```

```
## x
```


```r
f <- function(expr) expr[[1]]
f(quote(2 * x + y))
```

```
## `+`
```


```r
f(2 * x + y)
```

```
## Error in f(2 * x + y): object 'x' not found
```


```r
x <- 2
y <- 3
f(2 * x + y)
```

```
## [1] 7
```


```r
f <- function(expr) {
  expr <- quote(expr)
  expr[[1]]
}
f(2 * x + y)
```

```
## Error in expr[[1]]: object of type 'symbol' is not subsettable
```


```r
f <- function(expr) {
  expr <- substitute(expr)
  expr[[1]]
}
f(2 * x + y)
```

```
## `+`
```


```r
f <- function(expr) {
  expr <- substitute(expr + expr)
  expr
}
f(2 * x + y)
```

```
## 2 * x + y + (2 * x + y)
```


```r
f <- function(expr) {
  expr + expr
}
g <- function(expr) {
  x <- substitute(expr + expr)
  eval(x)
}
```


```r
x <- 2; y <- 3
f(2 * x + y)
```

```
## [1] 14
```


```r
g(2 * x + y)
```

```
## Error in 2 * x: non-numeric argument to binary operator
```


```r
g <- function(expr) {
  x <- substitute(expr + expr)
  eval(x, parent.frame())
}
g(2 * x + y)
```

```
## [1] 14
```

## Exploring expressions


```r
print_expression <- function(expr, indent = "") {
  if (is.atomic(expr)) {
    if (inherits(expr, "srcref")) {
      expr <- paste0("srcref = ", expr)
    }
    cat(indent, " - ", expr, "\n")
    
  } else if (is.name(expr)) {
    if (expr == "") {
      expr <- "MISSING"
    }
    cat(indent, " - ", expr, "\n")
    
  } else if (is.primitive(expr)) {
    cat(indent, " - ", expr, "\n")
    
  } else if (is.pairlist(expr)) {
    cat(indent, " - ", "[\n")
    new_indent <- paste0(indent, "       ")
    vars <- names(expr)
    for (i in seq_along(expr)) {
      cat(indent, "    ", vars[i], " ->\n")
      print_expression((expr[[i]]), new_indent)
    }
    cat(indent, "    ]\n")
    
  } else {
    print_expression((expr[[1]]), indent)
    new_indent <- paste0("  ", indent)
    for (i in 2:length(expr)) {
      print_expression(expr[[i]], new_indent)
    }
  }
}
```


```r
print_expression(quote(2 * x + y))
```

```
##   -  + 
##     -  * 
##       -  2 
##       -  x 
##     -  y
```


```r
print_expression(quote(function(x) x))
```

```
##   -  function 
##     -  [
##         x  ->
##            -  MISSING 
##        ]
##     -  x 
##     -  srcref = function(x) x
```


```r
print_expression(quote(function(x = 2 * 2 + 4) x))
```

```
##   -  function 
##     -  [
##         x  ->
##            -  + 
##              -  * 
##                -  2 
##                -  2 
##              -  4 
##        ]
##     -  x 
##     -  srcref = function(x = 2 * 2 + 4) x
```

```r
print_expression(quote(function(x, y = 2 * x) x + y))
```

```
##   -  function 
##     -  [
##         x  ->
##            -  MISSING 
##         y  ->
##            -  * 
##              -  2 
##              -  x 
##        ]
##     -  + 
##       -  x 
##       -  y 
##     -  srcref = function(x, y = 2 * x) x + y
```


```r
expr <- quote((function(x) x)(2))
print_expression(expr)
```

```
##   -  ( 
##     -  function 
##       -  [
##           x  ->
##              -  MISSING 
##          ]
##       -  x 
##       -  srcref = function(x) x 
##     -  2
```

```r
expr[[1]]
```

```
## (function(x) x)
```

```r
expr[[2]]
```

```
## [1] 2
```


```r
cons <- function(car, cdr) list(car = car, cdr = cdr)
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
```


```r
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
collect_symbols <- function(expr) {
  expr <- substitute(expr)
  bound <- c()
  lst <- collect_symbols_rec(expr, NULL, bound)
  lst %>% lst_to_list() %>% unique() %>% 
          purrr::discard(exists, parent.frame()) %>%
          unlist()
}
```



```r
rm(x) ; rm(y)
collect_symbols(2 * x + y + z)
```

```
## [1] "z" "y" "x"
```


```r
z <- 3
collect_symbols(2 * x + y + z)
```

```
## [1] "y" "x"
```


```r
collect_symbols(function(x) 2 * x + y + z)
```

```
## [1] "y"
```

```r
collect_symbols(function(x) function(y) f(2 * x + y))
```

```
## NULL
```


```r
collect_symbols(function(x, y = 2 * w) 2 * x + y)
```

```
## [1] "w"
```


```r
f <- function(expr) collect_symbols(expr)
```


```r
f(2 + y * w)
```

```
## NULL
```


```r
collect_symbols_ <- function(expr, env) {
  bound <- c()
  lst <- collect_symbols_rec(expr, NULL, bound)
  lst %>% lst_to_list() %>% unique() %>% 
    purrr::discard(exists, env) %>%
    unlist()
}
collect_symbols <- function(expr) {
  collect_symbols_(substitute(expr), parent.frame())
}
```

## Manipulating expressions


```r
f <- quote(function(x) 2 * x)
f
```

```
## function(x) 2 * x
```


```r
f[[2]]
```

```
## $x
```


```r
f[[2]][[1]] <- quote(2 * y)
f
```

```
## function(x = 2 * y) 2 * x
```


```r
names(f[[2]]) <- c("a")
f[[3]] <- quote(2 * a)
f
```

```
## function(a = 2 * y) 2 * a
```


```r
expr <- quote(2 * x + y)
expr
```

```
## 2 * x + y
```

```r
expr[[1]] <- as.symbol("/")
expr
```

```
## 2 * x/y
```

```r
expr[[2]][[1]] <- as.symbol("+")
expr
```

```
## (2 + x)/y
```


```r
call("+", quote(2 * x), quote(y))
```

```
## 2 * x + y
```

```r
call("+", call("*", 2, quote(x)), quote(y))
```

```
## 2 * x + y
```


```r
call("f", a = quote(2 * x), b = quote(y))
```

```
## f(a = 2 * x, b = y)
```


```r
z <- 2
call("+", 2 * z, quote(y))
```

```
## 4 + y
```

```r
library(rlang)
lang("+", quote(2 * x), quote(y))
```

```
## 2 * x + y
```

```r
new_language(as.symbol("+"), pairlist(quote(2 * x), quote(y)))
```

```
## 2 * x + y
```



```r
make_args_list <- function(args) {
  res <- replicate(length(args), substitute())
  names(res) <- args
  as.pairlist(res)
}
```


```r
f <- call("function", 
          make_args_list(c("x", "y")), 
          quote(2 * x + y))
f
```

```
## function(x, y) 2 * x + y
```


```r
f(2, 3)
```

```
## Error in f(2, 3): could not find function "f"
```


```r
f <- eval(f)
f
```

```
## function (x, y) 
## 2 * x + y
```

```r
f(2, 3)
```

```
## [1] 7
```


```r
f <- new_function(make_args_list(c("x", "y")), 
                  quote(2 * x + y))
f
```

```
## function (x, y) 
## 2 * x + y
```

```r
f(2, 3)
```

```
## [1] 7
```


```r
expr_to_function <- function(expr) {
  expr <- substitute(expr)
  unbound <- collect_symbols_(expr, caller_env())
  new_function(make_args_list(unbound), expr, caller_env())
}
```



```r
f <- expr_to_function(2 * x + y)
f
```

```
## function (y, x) 
## 2 * x + y
```

```r
f(x = 2, y = 3)
```

```
## [1] 7
```

```r
g <- expr_to_function(function(x) 2 * x + y)
g
```

```
## function (y) 
## function(x) 2 * x + y
```

```r
g(y = 3)(x = 2)
```

```
## [1] 7
```

