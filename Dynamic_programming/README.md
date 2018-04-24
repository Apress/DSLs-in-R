# Dynamic programming


```r
n <- 10
F <- vector("numeric", length = n)
F[1] <- F[2] <- 1
for (m in 3:n) {
    F[m] <- F[m-1] + F[m-2]
}
F[n]
```

```
## [1] 55
```



```r
x <-c("a", "b", "c")
y <- c("a", "b", "b", "c")
```


```r
n <- length(x)
m <- length(y)
E <- vector("numeric", length = (n + 1) * (m + 1))
dim(E) <- c(n + 1, m + 1)
for (i in 1:(n + 1))
    E[i, 1] <- i - 1
for (j in 1:(m + 1))
    E[1, j] <- j - 1
for (i in 2:(n + 1)) {
    for (j in 2:(m + 1)) {
        E[i, j] <- min(
            E[i - 1, j] + 1,
            E[i, j - 1] + 1,
            E[i - 1, j - 1] + (x[i - 1] != y[j - 1])
        )
    }
}
E
```

```
##      [,1] [,2] [,3] [,4] [,5]
## [1,]    0    1    2    3    4
## [2,]    1    0    1    2    3
## [3,]    2    1    0    1    2
## [4,]    3    2    1    1    1
```


```r
E[n + 1, m + 1]
```

```
## [1] 1
```



```r
#install.packages("dynprog")
library(dynprog)
```

```
## Warning: package 'dynprog' was built under R version 3.4.4
```

```r
fib <- {
    F[n] <- 1 ? n <= 2
    F[n] <- F[n - 1] + F[n - 2]
} %where% {
    n <- 1:10
}
```


```r
fib
```

```
##  [1]  1  1  2  3  5  8 13 21 34 55
```


```r
x <- c("a", "b", "c")
y <- c("a", "b", "b", "c")
edit <- {
  E[1,j] <- j - 1
  E[i,1] <- i - 1
  E[i,j] <- min(
      E[i - 1,j] + 1,
      E[i,j - 1] + 1,
      E[i - 1,j - 1] + (x[i - 1] != y[j - 1])
 ) ? i > 1 && j > 1
} %where% {
    i <- 1:(length(x) + 1)
    j <- 1:(length(y) + 1)
}
```



```r
edit
```

```
##      [,1] [,2] [,3] [,4] [,5]
## [1,]    0    1    2    3    4
## [2,]    1    0    1    2    3
## [3,]    2    1    0    1    2
## [4,]    3    2    1    1    1
```



```r
{
    F[1] <- 1
    F[2] <- 1
    F[n] <- F[n - 1] + F[n - 2]
} %where% {
    n <- 1:10
}
```

```
##  [1]  1  1  2  3  5  8 13 21 34 55
```



```r
{
  E[1,j] <- j - 1
  E[i,1] <- i - 1
  E[i,j] <- min(
      E[i - 1,j] + 1,
      E[i,j - 1] + 1,
      E[i - 1,j - 1] + (x[i - 1] != y[j - 1])
 )
} %where% {
    i <- 1:(length(x) + 1)
    j <- 1:(length(y) + 1)
}
```

```
##      [,1] [,2] [,3] [,4] [,5]
## [1,]    0    1    2    3    4
## [2,]    1    0    1    2    3
## [3,]    2    1    0    1    2
## [4,]    3    2    1    1    1
```


## Parsing expressions



```r
`%where%` <- function(recursion, ranges) {
    parsed <- list(
        recursions = parse_recursion(rlang::enquo(recursion)),
        ranges = parse_ranges(rlang::enquo(ranges))
    )
    eval_dynprog(parsed)
}
```


```r
parse_ranges <- function(ranges) {
    ranges_expr <- rlang::get_expr(ranges)
    ranges_env <- rlang::get_env(ranges)

    stopifnot(ranges_expr[[1]] == "{")
    ranges_definitions <- ranges_expr[-1]

    n <- length(ranges_definitions)
    result <- vector("list", length = n)
    indices <- vector("character", length = n)

    for (i in seq_along(ranges_definitions)) {
        assignment <- ranges_definitions[[i]]

        stopifnot(assignment[[1]] == "<-")
        range_var <- as.character(assignment[[2]])
        range_value <- eval(assignment[[3]], ranges_env)

        indices[[i]] <- range_var
        result[[i]] <- range_value
    }

    names(result) <- indices
    result
}
```


```r
parse_ranges(rlang::quo({
    n <- 1:10
}))
```

```
## $n
##  [1]  1  2  3  4  5  6  7  8  9 10
```

```r
parse_ranges(rlang::quo({
    i <- 1:(length(x) + 1)
    j <- 1:(length(y) + 1)
}))
```

```
## $i
## [1] 1 2 3 4
## 
## $j
## [1] 1 2 3 4 5
```


```r
parse_recursion <- function(recursion) {
    recursion_expr <- rlang::get_expr(recursion)
    recursion_env <- rlang::get_env(recursion)

    stopifnot(recursion_expr[[1]] == "{")
    recursion_cases <- recursion_expr[-1]

    n <- length(recursion_cases)
    patterns <- vector("list", length = n)
    conditions <- vector("list", length = n)
    recursions <- vector("list", length = n)

    for (i in seq_along(recursion_cases)) {
        case <- recursion_cases[[i]]

        condition <- TRUE
        stopifnot(rlang::is_call(case))
        if (case[[1]] == "?") {
            # NB: The order matters here!
            condition <- case[[3]]
            case <- case[[2]]
        }

        stopifnot(case[[1]] == "<-")
        pattern <- case[[2]]
        recursion <- case[[3]]

        patterns[[i]] <- pattern
        recursions[[i]] <- recursion
        conditions[[i]] <- condition
    }

    list(
        recursion_env = recursion_env,
        patterns = patterns,
        conditions = conditions,
        recursions = recursions
    )
}
```


```r
parse_recursion(rlang::quo({
  F[n] <- n * F[n - 1] ? n > 1
  F[n] <- 1            ? n <= 1
}))
```

```
## $recursion_env
## <environment: R_GlobalEnv>
## 
## $patterns
## $patterns[[1]]
## F[n]
## 
## $patterns[[2]]
## F[n]
## 
## 
## $conditions
## $conditions[[1]]
## n > 1
## 
## $conditions[[2]]
## n <= 1
## 
## 
## $recursions
## $recursions[[1]]
## n * F[n - 1]
## 
## $recursions[[2]]
## [1] 1
```


```r
parse_recursion(rlang::quo({
  E[1,j] <- j - 1
  E[i,1] <- i - 1
  E[i,j] <- min(
      E[i - 1,j] + 1,
      E[i,j - 1] + 1,
      E[i - 1,j - 1] + (x[i - 1] != y[j - 1])
  ) ? i > 1 && j > 1
}))
```

```
## $recursion_env
## <environment: R_GlobalEnv>
## 
## $patterns
## $patterns[[1]]
## E[1, j]
## 
## $patterns[[2]]
## E[i, 1]
## 
## $patterns[[3]]
## E[i, j]
## 
## 
## $conditions
## $conditions[[1]]
## [1] TRUE
## 
## $conditions[[2]]
## [1] TRUE
## 
## $conditions[[3]]
## i > 1 && j > 1
## 
## 
## $recursions
## $recursions[[1]]
## j - 1
## 
## $recursions[[2]]
## i - 1
## 
## $recursions[[3]]
## min(E[i - 1, j] + 1, E[i, j - 1] + 1, E[i - 1, j - 1] + (x[i - 
##     1] != y[j - 1]))
```

## Evaluating expressions


```r
ranges <- parse_ranges(rlang::quo({
    n <- 1:10
}))
head(do.call(expand.grid, ranges))
```

```
##   n
## 1 1
## 2 2
## 3 3
## 4 4
## 5 5
## 6 6
```


```r
ranges <- parse_ranges(rlang::quo({
    i <- 1:(length(x) + 1)
    j <- 1:(length(y) + 1)
}))
head(do.call(expand.grid, ranges))
```

```
##   i j
## 1 1 1
## 2 2 1
## 3 3 1
## 4 4 1
## 5 1 2
## 6 2 2
```


```r
edit_ranges <- do.call(expand.grid, ranges)
edit_ranges[1,]
```

```
##   i j
## 1 1 1
```

```r
edit_ranges[2,]
```

```
##   i j
## 2 2 1
```


```r
with(edit_ranges[3,], 
    cat(i, j, "\\n")
)
```

```
## 3 1 \n
```


```r
make_pattern_match <- function(pattern, range_vars) {
    matches <- vector("list", length = length(range_vars))
    stopifnot(pattern[[1]] == "[")
    for (i in seq_along(matches)) {
        matches[[i]] <- call(
            "==",
            pattern[[i + 2]],
            range_vars[[i]]
        )
    }
    rlang::expr(all(!!! matches))
}
```


```r
make_pattern_match(rlang::expr(F[1]), list(as.symbol("n")))
```

```
## all(1 == n)
```

```r
make_pattern_match(rlang::expr(F[n]), list(as.symbol("n")))
```

```
## all(n == n)
```

```r
make_pattern_match(rlang::expr(E[1,j]), 
                    list(as.symbol("i"), as.symbol("j")))
```

```
## all(1 == i, j == j)
```


```r
make_pattern_tests <- function(patterns, range_vars) {
    tests <- vector("list", length = length(patterns))
    for (i in seq_along(tests)) {
        tests[[i]] <- make_pattern_match(
            patterns[[i]],
            range_vars
        )
    }
    tests
}
```


```r
fib_ranges <- parse_ranges(rlang::quo({
    n <- 1:10
}))
fib_recursions <- parse_recursion(rlang::quo({
  F[n] <- F[n - 1] + F[n - 2] ? n > 2
  F[n] <- 1                   ? n <= 2
}))
```



```r
make_pattern_tests(
    fib_recursions$patterns,
    Map(as.symbol, names(fib_ranges))
)
```

```
## [[1]]
## all(n == n)
## 
## [[2]]
## all(n == n)
```



```r
make_condition_checks <- function(
    ranges,
    patterns,
    conditions,
    recursions
) {
    test_conditions <- make_pattern_tests(
        patterns,
        Map(as.symbol, names(ranges))
    )
    for (i in seq_along(conditions)) {
        test_conditions[[i]] <- rlang::call2(
            "&&", test_conditions[[i]], conditions[[i]]
        )
    }
    test_conditions
}
```



```r
fib_ranges <- parse_ranges(rlang::quo({
    n <- 1:10
}))
fib_recursions <- parse_recursion(rlang::quo({
  F[n] <- F[n - 1] + F[n - 2] ? n > 2
  F[n] <- 1                   ? n <= 2
}))
make_condition_checks(
    fib_ranges,
    fib_recursions$patterns,
    fib_recursions$conditions,
    fib_recursions$recursions
)
```

```
## [[1]]
## all(n == n) && n > 2
## 
## [[2]]
## all(n == n) && n <= 2
```


```r
make_recursion_case <- function(
    test_expr,
    value_expr,
    continue
) {
    if (rlang::is_null(continue)) {
        rlang::call2("if", test_expr, value_expr)
    } else {
        rlang::call2("if", test_expr, value_expr, continue)
    }
}

make_update_expr <- function(
    ranges,
    patterns,
    conditions,
    recursions
) {
    conditions <- make_condition_checks(
        ranges,
        patterns,
        conditions,
        recursions
    )
    continue <- NULL
    for (i in rev(seq_along(conditions))) {
        continue <- make_recursion_case(
            conditions[[i]], recursions[[i]], continue
        )
    }
    continue
}
```



```r
make_update_expr(
    fib_ranges,
    fib_recursions$patterns,
    fib_recursions$conditions,
    fib_recursions$recursions
)
```

```
## if (all(n == n) && n > 2) F[n - 1] + F[n - 2] else if (all(n == 
##     n) && n <= 2) 1
```



```r
make_loop_expr <- function(tbl_name, update_expr) {
    rlang::expr({
        combs <- do.call(expand.grid, ranges)
        rlang::UQ(tbl_name) <- vector(
            "numeric",
            length = nrow(combs)
        )
        dim(rlang::UQ(tbl_name)) <- Map(length, ranges)
        for (row in seq_along(rlang::UQ(tbl_name))) {
            rlang::UQ(tbl_name)[row] <- with(
                combs[row, , drop = FALSE], {
                    rlang::UQ(update_expr)
                }
            )
        }
        rlang::UQ(tbl_name)
    })
}
```



```r
get_table_name <- function(patterns) {
    p <- patterns[[1]]
    stopifnot(p[[1]] == "[")
    p[[2]]
}
get_table_name(fib_recursions$patterns)
```

```
## F
```



```r
make_loop_expr(get_table_name(fib_recursions$patterns),
                make_update_expr(
                    fib_ranges,
                    fib_recursions$patterns,
                    fib_recursions$conditions,
                    fib_recursions$recursions
                ))
```

```
## {
##     combs <- do.call(expand.grid, ranges)
##     F <- vector("numeric", length = nrow(combs))
##     dim(F) <- Map(length, ranges)
##     for (row in seq_along(F)) {
##         F[row] <- with(combs[row, , drop = FALSE], {
##             if (all(n == n) && n > 2) 
##                 F[n - 1] + F[n - 2]
##             else if (all(n == n) && n <= 2) 
##                 1
##         })
##     }
##     F
## }
```



```r
edit_ranges <- parse_ranges(rlang::quo({
    i <- 1:(length(x) + 1)
    j <- 1:(length(y) + 1)
}))
edit_recursions <- parse_recursion(rlang::quo({
  E[1,j] <- j - 1
  E[i,1] <- i - 1
  E[i,j] <- min(
      E[i - 1,j] + 1,
      E[i,j - 1] + 1,
      E[i - 1,j - 1] + (x[i - 1] != y[j - 1])
  )
}))

make_loop_expr(get_table_name(edit_recursions$patterns),
                make_update_expr(
                    edit_ranges,
                    edit_recursions$patterns,
                    edit_recursions$conditions,
                    edit_recursions$recursions
                ))
```

```
## {
##     combs <- do.call(expand.grid, ranges)
##     E <- vector("numeric", length = nrow(combs))
##     dim(E) <- Map(length, ranges)
##     for (row in seq_along(E)) {
##         E[row] <- with(combs[row, , drop = FALSE], {
##             if (all(1 == i, j == j) && TRUE) 
##                 j - 1
##             else if (all(i == i, 1 == j) && TRUE) 
##                 i - 1
##             else if (all(i == i, j == j) && TRUE) 
##                 min(E[i - 1, j] + 1, E[i, j - 1] + 1, E[i - 1, 
##                   j - 1] + (x[i - 1] != y[j - 1]))
##         })
##     }
##     E
## }
```


```r
eval_recursion <- function(ranges, recursions) {
    loop <- make_loop_expr(
                get_table_name(recursions$patterns),
                make_update_expr(
                    ranges,
                    recursions$patterns,
                    recursions$conditions,
                    recursions$recursions
                ))
    eval_env <- rlang::env_clone(
        environment(), # this function environment
        recursions$recursion_env # quosure environment
    )
    eval(loop, envir = eval_env)
}
```


```r
eval_recursion(fib_ranges, fib_recursions)
```

```
##  [1]  1  1  2  3  5  8 13 21 34 55
```


```r
eval_dynprog <- function(dynprog) {
    eval_recursion(dynprog$ranges, dynprog$recursions)
}
```


```r
eval_dynprog(list(
    ranges = fib_ranges, 
    recursions = fib_recursions
))
```

```
##  [1]  1  1  2  3  5  8 13 21 34 55
```

```r
eval_dynprog(list(
    ranges = edit_ranges, 
    recursions = edit_recursions
))
```

```
##      [,1] [,2] [,3] [,4] [,5]
## [1,]    0    1    2    3    4
## [2,]    1    0    1    2    3
## [3,]    2    1    0    1    2
## [4,]    3    2    1    1    1
```

## Fixing the evaluation environment



```r
eval_recursion <- function(ranges, recursions) {
    tbl_name <- get_table_name(recursions$patterns)
    tbl_name_string <- as.character(tbl_name)
    update_expr <- make_update_expr(
        ranges,
        recursions$patterns,
        recursions$conditions,
        recursions$recursions
    )
    eval_env <- rlang::child_env(recursions$recursion_env)
    
    combs <- do.call(expand.grid, ranges)
    tbl <- vector("numeric", length = nrow(combs))
    dim(tbl) <- Map(length, ranges)
    eval_env[[tbl_name_string]] <- tbl
    
    for (row in seq_along(tbl)) {
        val <- eval(
            rlang::expr(rlang::UQ(update_expr)),
            combs[row, , drop = FALSE],
            eval_env
        )
        eval(rlang::expr(
                rlang::UQ(tbl_name)[rlang::UQ(row)] 
                    <- rlang::UQ(val)
            ), eval_env)
    }
    
    eval_env[[tbl_name_string]]
}
```



```r
eval_dynprog(list(
    ranges = fib_ranges, 
    recursions = fib_recursions
))
```

```
##  [1]  1  1  2  3  5  8 13 21 34 55
```

```r
eval_dynprog(list(
    ranges = edit_ranges, 
    recursions = edit_recursions
))
```

```
##      [,1] [,2] [,3] [,4] [,5]
## [1,]    0    1    2    3    4
## [2,]    1    0    1    2    3
## [3,]    2    1    0    1    2
## [4,]    3    2    1    1    1
```

