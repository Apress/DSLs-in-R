# Tidy evaluation {#sec:tidy-eval}


```r
library(magrittr)
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
library(tibble)
```


## Quosures


```r
q <- rlang::quo(2 * x)
q
```

```
## <quosure>
##   expr: ^2 * x
##   env:  global
```



```r
f <- function(expr) rlang::enquo(expr)
q <- f(2 * x)
q
```

```
## <quosure>
##   expr: ^2 * x
##   env:  global
```


```r
q[[2]]
```

```
## 2 * x
```

```r
environment(q)
```

```
## <environment: R_GlobalEnv>
```



```r
rlang::get_expr(q)
```

```
## 2 * x
```


```r
rlang::get_env(q)
```

```
## <environment: R_GlobalEnv>
```


```r
eval(q)
```

```
## <quosure>
##   expr: ^2 * x
##   env:  global
```


```r
x <- 1
rlang::eval_tidy(q)
```

```
## [1] 2
```

```r
x <- 2
rlang::eval_tidy(q)
```

```
## [1] 4
```



```r
f <- function(x, y) rlang::quo(x + y + z)
```


```r
q <- f(1, 2)
x <- y <- z <- 3
rlang::eval_tidy(q) # 1 + 2 + 3
```

```
## [1] 6
```

```r
x + y + z # 3 + 3 + 3
```

```
## [1] 9
```



```r
x <- 1:4
y <- 1:4
q <- quo(x+y)
rlang::eval_tidy(q)
```

```
## [1] 2 4 6 8
```

```r
rlang::eval_tidy(q, list(x = 5:8))
```

```
## [1]  6  8 10 12
```


```r
f <- function(expr,x) {
  q <- rlang::enquo(expr)
  rlang::eval_tidy(q)
}
g <- function(expr,x) {
  q <- rlang::enquo(expr)
  rlang::eval_tidy(q, environment())
}
f(x + y, x = 5:8)
```

```
## [1] 2 4 6 8
```

```r
g(x + y, x = 5:8)
```

```
## [1]  6  8 10 12
```


```r
rlang::eval_tidy(quote(x + y))
```

```
## [1] 2 4 6 8
```


```r
rlang::eval_tidy(quote(xx), env = list2env(list(xx = 5:8)))
```

```
## [1] 5 6 7 8
```


```r
rlang::eval_tidy(quo(xx), env = list2env(list(xx = 5:8)))
```

```
## Error in rlang::eval_tidy(quo(xx), env = list2env(list(xx = 5:8))): object 'xx' not found
```


```r
make_function <- function(args, body) {
  body <- rlang::enquo(body)
  rlang::new_function(args, rlang::get_expr(body), rlang::get_env(body))
}
f <- function(z) make_function(alist(x=, y=), x + y + z)
g <- f(z = 1:4)
g
```

```
## function (x, y) 
## x + y + z
## <environment: 0x7fc0915de7f8>
```

```r
g(x = 1:4, y = 1:4)
```

```
## [1]  3  6  9 12
```


```r
make_function_quo <- function(args, body) {
  body <- rlang::enquo(body)
  rlang::new_function(args, rlang::get_expr(body), rlang::get_env(body))
}
make_function_quote <- function(args, body) {
  body <- substitute(body)
  rlang::new_function(args, body, rlang::caller_env())
}
g <- make_function_quo(alist(x=, y=), x + y)
h <- make_function_quote(alist(x=, y=), x + y)
g(x = 1:4, y = 1:4)
```

```
## [1] 2 4 6 8
```

```r
h(x = 1:4, y = 1:4)
```

```
## [1] 2 4 6 8
```


```r
cons <- function(elm, lst) list(car=elm, cdr=lst)
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
expressions <- function() list(ex = NULL)
add_expression <- function(ex, expr) {
  ex$ex <- cons(rlang::enquo(expr), ex$ex)
  ex
}
```



```r
make_functions <- function(ex, args) {
  results <- vector("list", length = lst_length(ex$ex))
  i <- 1; lst <- ex$ex
  while (!is.null(lst)) {
    results[[i]] <- 
      rlang::new_function(args, rlang::get_expr(lst$car), 
                           rlang::get_env(lst$car))
    i <- i + 1
    lst <- lst$cdr
  }
  rev(results)
}
```


```r
make_line_expressions <- function(intercept) {
  expressions() %>% 
    add_expression(coef + intercept) %>%
    add_expression(2*coef + intercept) %>% 
    add_expression(3*coef + intercept) %>% 
    add_expression(4*coef + intercept)
}
```


```r
eval_line <- function(ex, coef) {
  ex %>% make_functions(alist(coef=)) %>%
    purrr::invoke_map(coef = coef) %>% unlist()
}
```


```r
make_line_expressions(intercept = 0) %>% eval_line(coef = 1)
```

```
## [1] 1 2 3 4
```

```r
make_line_expressions(intercept = 0) %>% eval_line(coef = 2)
```

```
## [1] 2 4 6 8
```

```r
make_line_expressions(intercept = 1) %>% eval_line(coef = 1)
```

```
## [1] 2 3 4 5
```



```r
add_expression <- function(ex, expr) {
  ex$ex <- cons(substitute(expr), ex$ex)
  ex
}
make_functions <- function(ex, args) {
  results <- vector("list", length = lst_length(ex$ex))
  i <- 1; lst <- ex$ex
  while (!is.null(lst)) {
    results[[i]] <- rlang::new_function(args, lst$car, rlang::caller_env())
    i <- i + 1
    lst <- lst$cdr
  }
  rev(results)
}
```


```r
make_line_expressions(intercept = 0) %>% eval_line(coef = 1)
```

```
## Error in (function (coef) : object 'intercept' not found
```


```r
rlang::quos(x, y, x+y)
```

```
## [[1]]
## <quosure>
##   expr: ^x
##   env:  global
## 
## [[2]]
## <quosure>
##   expr: ^y
##   env:  global
## 
## [[3]]
## <quosure>
##   expr: ^x + y
##   env:  global
```


```r
f <- function(...) rlang::quos(...)
f(x, y, z)
```

```
## [[1]]
## <quosure>
##   expr: ^x
##   env:  global
## 
## [[2]]
## <quosure>
##   expr: ^y
##   env:  global
## 
## [[3]]
## <quosure>
##   expr: ^z
##   env:  global
```

## Quasi-quoting


```r
df <- tribble(
  ~x, ~y,
   1,  1,
  NA,  2,
   3,  3,
   4, NA,
   5,  5,
  NA,  6,
   7, NA
)
df %>% dplyr::filter(!is.na(x))
```

```
## # A tibble: 5 x 2
##       x     y
##   <dbl> <dbl>
## 1    1.    1.
## 2    3.    3.
## 3    4.   NA 
## 4    5.    5.
## 5    7.   NA
```

```r
df %>% dplyr::filter(!is.na(y))
```

```
## # A tibble: 5 x 2
##       x     y
##   <dbl> <dbl>
## 1    1.    1.
## 2   NA     2.
## 3    3.    3.
## 4    5.    5.
## 5   NA     6.
```


```r
filter_on_na <- function(df, column) {
  column <- substitute(column)
  df %>% dplyr::filter(!is.na(column))
}
df %>% filter_on_na(x)
```

```
## Warning in is.na(column): is.na() applied to non-(list or vector) of type
## 'symbol'
```

```
## # A tibble: 7 x 2
##       x     y
##   <dbl> <dbl>
## 1    1.    1.
## 2   NA     2.
## 3    3.    3.
## 4    4.   NA 
## 5    5.    5.
## 6   NA     6.
## 7    7.   NA
```


```r
filter_on_na <- function(df, column) {
  column <- rlang::enexpr(column)
  df %>% dplyr::filter(!is.na(!!column))
}
df %>% filter_on_na(x)
```

```
## # A tibble: 5 x 2
##       x     y
##   <dbl> <dbl>
## 1    1.    1.
## 2    3.    3.
## 3    4.   NA 
## 4    5.    5.
## 5    7.   NA
```

```r
df %>% filter_on_na(y)
```

```
## # A tibble: 5 x 2
##       x     y
##   <dbl> <dbl>
## 1    1.    1.
## 2   NA     2.
## 3    3.    3.
## 4    5.    5.
## 5   NA     6.
```


```r
f <- function(x) substitute(x)
g <- function(x) rlang::enexpr(x)
```


```r
h <- function(func, var) func(!!var)
h(f, quote(x))
```

```
## !(!var)
```

```r
h(g, quote(x))
```

```
## x
```


```r
x <- y <- 1
quote(2 * x + !!y)
```

```
## 2 * x + (!(!y))
```

```r
rlang::expr(2 * x + !!y)
```

```
## 2 * x + 1
```

```r
rlang::quo(2 * x + !!y)
```

```
## <quosure>
##   expr: ^2 * x + 1
##   env:  global
```


```r
x <- y <- 2
rlang::expr(!!x + y)
```

```
## 2 + y
```



```r
rlang::expr(UQ(x) + y)
```

```
## 2 + y
```


```r
f <- function(df, summary_name, summary_expr) {
  summary_name <- rlang::enexpr(summary_name)
  summary_expr <- rlang::enquo(summary_expr)
  df %>% mutate(UQ(summary_name) := UQ(summary_expr))
}
tibble(x = 1:4, y = 1:4) %>% f(z, x + y)
```

```
## # A tibble: 4 x 3
##       x     y     z
##   <int> <int> <int>
## 1     1     1     2
## 2     2     2     4
## 3     3     3     6
## 4     4     4     8
```


```r
args <- rlang::quos(x = 1, y = 2)
q <- rlang::expr(f(rlang::UQS(args)))
q
```

```
## f(x = ~1, y = ~2)
```


```r
q <- rlang::expr(f(rlang::UQ(args)))
q
```

```
## f(list(x = ~1, y = ~2))
```


```r
q <- rlang::expr(f(!!!args))
q
```

```
## f(x = ~1, y = ~2)
```


```r
mean_expr <- function(ex, ...) {
  ex <- rlang::enquo(ex)
  extra_args <- rlang::dots_list(...)
  mean_call <- rlang::expr(with(
      data,
      mean(!!rlang::get_expr(ex), !!!extra_args))
  )
  rlang::new_function(args = alist(data=), 
                      body = mean_call,
                      env = rlang::get_env(ex))
}
mean_sum <- mean_expr(x + y, na.rm = TRUE)
mean_sum
```

```
## function (data) 
## with(data, mean(x + y, na.rm = TRUE))
```


```r
df
```

```
## # A tibble: 7 x 2
##       x     y
##   <dbl> <dbl>
## 1    1.    1.
## 2   NA     2.
## 3    3.    3.
## 4    4.   NA 
## 5    5.    5.
## 6   NA     6.
## 7    7.   NA
```

```r
mean_sum(df)
```

```
## [1] 6
```



```r
f <- function(z) mean_expr(x + y + z, na.rm = TRUE, trim = 0.1)
g <- f(z = 1:7)
g
```

```
## function (data) 
## with(data, mean(x + y + z, na.rm = TRUE, trim = 0.1))
## <environment: 0x7fc092bcc4a0>
```

```r
g(df)
```

```
## [1] 9
```


```r
f <- function(expr) rlang::enquo(expr)
g <- function(expr) f(rlang::enquo(expr))
f(x + y)
```

```
## <quosure>
##   expr: ^x + y
##   env:  global
```

```r
g(x + y)
```

```
## <quosure>
##   expr: ^rlang::enquo(expr)
##   env:  0x7fc08d72e1b8
```

```r
rlang::eval_tidy(f(x + y), list(x = 1, y = 2))
```

```
## [1] 3
```

```r
rlang::eval_tidy(g(x + y), list(x = 1, y = 2))
```

```
## <quosure>
##   expr: ^x + y
##   env:  global
```

