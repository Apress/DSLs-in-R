## ------------------------------------------------------------------------
library(purrr)
library(rlang)
library(magrittr)

## ------------------------------------------------------------------------
quote(2 * x + y)

## ------------------------------------------------------------------------
expr <- quote(2 * x + y)
class(expr)

## ------------------------------------------------------------------------
expr[[1]]
expr[[2]]
expr[[3]]

## ------------------------------------------------------------------------
expr[[2]][[1]]
expr[[2]][[2]]
expr[[2]][[3]]

## ------------------------------------------------------------------------
f <- function(expr) expr[[1]]
f(quote(2 * x + y))

## ---- error=TRUE---------------------------------------------------------
f(2 * x + y)

## ------------------------------------------------------------------------
x <- 2
y <- 3
f(2 * x + y)

## ---- error=TRUE---------------------------------------------------------
f <- function(expr) {
  expr <- quote(expr)
  expr[[1]]
}
f(2 * x + y)

## ------------------------------------------------------------------------
f <- function(expr) {
  expr <- substitute(expr)
  expr[[1]]
}
f(2 * x + y)

## ------------------------------------------------------------------------
f <- function(expr) {
  expr <- substitute(expr + expr)
  expr
}
f(2 * x + y)

## ------------------------------------------------------------------------
f <- function(expr) {
  expr + expr
}
g <- function(expr) {
  x <- substitute(expr + expr)
  eval(x)
}

## ------------------------------------------------------------------------
x <- 2; y <- 3
f(2 * x + y)

## ---- error=TRUE---------------------------------------------------------
g(2 * x + y)

## ------------------------------------------------------------------------
g <- function(expr) {
  x <- substitute(expr + expr)
  eval(x, parent.frame())
}
g(2 * x + y)

## ------------------------------------------------------------------------
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

## ------------------------------------------------------------------------
print_expression(quote(2 * x + y))

## ------------------------------------------------------------------------
print_expression(quote(function(x) x))

## ------------------------------------------------------------------------
print_expression(quote(function(x = 2 * 2 + 4) x))
print_expression(quote(function(x, y = 2 * x) x + y))

## ------------------------------------------------------------------------
expr <- quote((function(x) x)(2))
print_expression(expr)
expr[[1]]
expr[[2]]

## ------------------------------------------------------------------------
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

## ------------------------------------------------------------------------
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
collect_symbols <- function(expr) {
  expr <- substitute(expr)
  bound <- c()
  lst <- collect_symbols_rec(expr, NULL, bound)
  lst %>% lst_to_list() %>% unique() %>% 
          purrr::discard(exists, parent.frame()) %>%
          unlist()
}

## ------------------------------------------------------------------------
rm(x) ; rm(y)
collect_symbols(2 * x + y + z)

## ------------------------------------------------------------------------
z <- 3
collect_symbols(2 * x + y + z)

## ------------------------------------------------------------------------
collect_symbols(function(x) 2 * x + y + z)
collect_symbols(function(x) function(y) f(2 * x + y))

## ------------------------------------------------------------------------
collect_symbols(function(x, y = 2 * w) 2 * x + y)

## ------------------------------------------------------------------------
f <- function(expr) collect_symbols(expr)

## ------------------------------------------------------------------------
f(2 + y * w)

## ------------------------------------------------------------------------
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

## ------------------------------------------------------------------------
f <- quote(function(x) 2 * x)
f

## ------------------------------------------------------------------------
f[[2]]

## ------------------------------------------------------------------------
f[[2]][[1]] <- quote(2 * y)
f

## ------------------------------------------------------------------------
names(f[[2]]) <- c("a")
f[[3]] <- quote(2 * a)
f

## ------------------------------------------------------------------------
expr <- quote(2 * x + y)
expr
expr[[1]] <- as.symbol("/")
expr
expr[[2]][[1]] <- as.symbol("+")
expr

## ------------------------------------------------------------------------
call("+", quote(2 * x), quote(y))
call("+", call("*", 2, quote(x)), quote(y))

## ------------------------------------------------------------------------
call("f", a = quote(2 * x), b = quote(y))

## ------------------------------------------------------------------------
z <- 2
call("+", 2 * z, quote(y))

## ------------------------------------------------------------------------
library(rlang)
lang("+", quote(2 * x), quote(y))
new_language(as.symbol("+"), pairlist(quote(2 * x), quote(y)))

## ------------------------------------------------------------------------
make_args_list <- function(args) {
  res <- replicate(length(args), substitute())
  names(res) <- args
  as.pairlist(res)
}

## ------------------------------------------------------------------------
f <- call("function", 
          make_args_list(c("x", "y")), 
          quote(2 * x + y))
f

## ---- error=TRUE---------------------------------------------------------
f(2, 3)

## ------------------------------------------------------------------------
f <- eval(f)
f
f(2, 3)

## ------------------------------------------------------------------------
f <- new_function(make_args_list(c("x", "y")), 
                  quote(2 * x + y))
f
f(2, 3)

## ------------------------------------------------------------------------
expr_to_function <- function(expr) {
  expr <- substitute(expr)
  unbound <- collect_symbols_(expr, caller_env())
  new_function(make_args_list(unbound), expr, caller_env())
}

## ------------------------------------------------------------------------
f <- expr_to_function(2 * x + y)
f
f(x = 2, y = 3)
g <- expr_to_function(function(x) 2 * x + y)
g
g(y = 3)(x = 2)

