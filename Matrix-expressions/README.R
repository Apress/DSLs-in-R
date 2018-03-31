## ------------------------------------------------------------------------
library(microbenchmark)

## ------------------------------------------------------------------------
m <- function(data) {
  structure(list(data = data), 
            nrow = nrow(data),
            ncol = ncol(data),
            def_expr = deparse(substitute(data)),
            class = c("matrix_data", "matrix_expr"))
}
matrix_mult <- function(A, B) {
  structure(list(left = A, right = B),
            nrow = nrow(A),
            ncol = ncol(B),
            class = c("matrix_mult", "matrix_expr"))
}
matrix_sum <- function(A, B) {
  structure(list(left = A, right = B),
            nrow = nrow(A),
            ncol = ncol(B),
            class = c("matrix_sum", "matrix_expr"))
}

## ------------------------------------------------------------------------
toString.matrix_data <- function(x, ...) {
  paste0("[", attr(x, "def_expr"), "]")
}
toString.matrix_mult <- function(x, ...) {
  paste0("(", toString(x$left), " * ", toString(x$right), ")")
}
toString.matrix_sum <- function(x, ...) {
  paste0("(", toString(x$left), " + ", toString(x$right), ")")
}
print.matrix_expr <- function(x, ...) {
  cat(toString(x), "\n")
}

## ------------------------------------------------------------------------
A <- matrix(1, nrow = 10, ncol = 20)
B <- matrix(1, nrow = 20, ncol = 10)
C <- matrix(1, nrow = 10, ncol = 10)

matrix_sum(matrix_mult(m(A), m(B)), m(C))

## ------------------------------------------------------------------------
`*.matrix_expr` <- function(A, B) {
  stopifnot(ncol(A) == nrow(B))
  matrix_mult(A, B)
}
`+.matrix_expr` <- function(A, B) {
  stopifnot(dim(A) == dim(B))
  matrix_sum(A, B)
}

## ------------------------------------------------------------------------
m(A) * m(B) + m(C)

## ------------------------------------------------------------------------
dim.matrix_expr <- function(x) {
  c(attr(x, "nrow"), attr(x, "ncol"))
}

## ------------------------------------------------------------------------
build_matrix_expr <- function(expr) {
  if (is.name(expr)) {
      return(substitute(m(name), list(name = expr)))
  }
  
  if (is.call(expr)) {
      if (expr[[1]] == as.name("(")) 
        return(build_matrix_expr(expr[[2]]))
      if (expr[[1]] == as.name("*") || 
          expr[[1]] == as.name("%*%")) {
          return(call('*', 
                      build_matrix_expr(expr[[2]]), 
                      build_matrix_expr(expr[[3]])))
      }
      if (expr[[1]] == as.name("+")) {
          return(call('+', 
                      build_matrix_expr(expr[[2]]), 
                      build_matrix_expr(expr[[3]])))
      }
  }
  stop(paste("Parse error for", expr))
}

## ------------------------------------------------------------------------
build_matrix_expr(quote(A * B))

## ------------------------------------------------------------------------
parse_matrix_expr <- function(expr) {
  expr <- substitute(expr)
  build_matrix_expr(expr)
}

## ------------------------------------------------------------------------
parse_matrix_expr(A * B)

## ------------------------------------------------------------------------
parse_matrix_expr <- function(expr) {
  expr <- substitute(expr)
  modified_expr <- build_matrix_expr(expr)
  eval(modified_expr, parent.frame())
}

## ------------------------------------------------------------------------
parse_matrix_expr(A * B)

## ------------------------------------------------------------------------
build_matrix_expr <- function(expr, env) {
  if (is.call(expr)) {
    if (expr[[1]] == as.name("(")) 
      return(build_matrix_expr(expr[[2]], env))
    if (expr[[1]] == as.name("*") || expr[[1]] == as.name("%*%"))
      return(matrix_mult(build_matrix_expr(expr[[2]], env), 
                         build_matrix_expr(expr[[3]], env)))
    if (expr[[1]] == as.name("+"))
      return(matrix_sum(build_matrix_expr(expr[[2]], env), 
                        build_matrix_expr(expr[[3]], env)))
  }
  data_matrix <- m(eval(expr, env))
  attr(data_matrix, "def_expr") <- deparse(expr)
  data_matrix
}

## ------------------------------------------------------------------------
parse_matrix_expr <- function(expr) {
  expr <- substitute(expr)
  build_matrix_expr(expr, parent.frame())
}

parse_matrix_expr(A * B + matrix(1, nrow = 10, ncol = 10))

## ------------------------------------------------------------------------
rearrange_matrix_expr <- function(expr) {
  UseMethod("rearrange_matrix_expr")
}
rearrange_matrix_expr.matrix_data <- function(expr) {
  expr
}
rearrange_matrix_expr.matrix_mult <- function(expr) {
  matrix_mult(rearrange_matrix_expr(expr$left),
              rearrange_matrix_expr(expr$right))
}
rearrange_matrix_expr.matrix_sum <- function(expr) {
  matrix_sum(rearrange_matrix_expr(expr$left),
             rearrange_matrix_expr(expr$right))
}

## ------------------------------------------------------------------------
arrange_optimal_matrix_mult <- function(matrices) {
  n <- length(matrices)
  dims <- matrix(0, nrow = n, ncol = 2)
  for (i in seq_along(matrices)) {
    dims[i,] <- dim(matrices[[i]])
  }
  
  N <- matrix(0, nrow = n, ncol = n)
  for (len in 2:n) {
    for (i in 1:(n - len + 1)) {
      j <- i + len - 1
      k <- i:(j - 1)
      N[i,j] <- min(dims[i,1]*dims[k,2]*dims[j,2] + N[i,k] + N[k + 1,j])
    }
  }
  
  # Backtrack through the table. This function will
  # be defined shortly.
  backtrack_matrix_mult(1, n, dims, N, matrices)  
}

## ------------------------------------------------------------------------
backtrack_matrix_mult <- function(i, j, dims, N, matrices) {
  if (i == j) {
    matrices[[i]]
  } else {
    k <- i:(j - 1)
    candidates <- dims[i,1]*dims[k,2]*dims[j,2] + N[i,k] + N[k + 1,j]
    split <- k[which(N[i,j] == candidates)][1]
    left <- backtrack_matrix_mult(i, split, dims, N, matrices)
    right <- backtrack_matrix_mult(split + 1, j, dims, N, matrices)
    matrix_mult(left, right)
  }
}

## ------------------------------------------------------------------------
leaf <- function(x) structure(x, class = c("leaf", "tree"))
inner <- function(left, right) 
  structure(list(left = left, right = right),
            class = c("inner", "tree"))

## ------------------------------------------------------------------------
tree <- inner(leaf(1), inner(inner(leaf(2), leaf(3)), leaf(4)))

## ------------------------------------------------------------------------
cons <- function(car, cdr) list(car = car, cdr = cdr)

## ------------------------------------------------------------------------
collect_leaves_rec <- function(tree, lst) 
  UseMethod("collect_leaves_rec")

collect_leaves_rec.leaf <- function(tree, lst) {
  cons(tree, lst)
}
collect_leaves_rec.inner <- function(tree, lst) {
  collect_leaves_rec(tree$left, collect_leaves_rec(tree$right, lst))
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
lst_to_vec <- function(lst) unlist(lst_to_list(lst))

## ------------------------------------------------------------------------
collect_leaves <- function(tree) {
  lst_to_vec(collect_leaves_rec(tree, NULL))
}
collect_leaves(tree)

## ------------------------------------------------------------------------
rearrange_matrix_expr.matrix_mult <- function(expr) {
  matrices <- collect_mult_components(expr)
  arrange_optimal_matrix_mult(matrices)
}

## ------------------------------------------------------------------------
collect_mult_components_rec <- function(expr, lst)
  UseMethod("collect_mult_components_rec")
collect_mult_components_rec.default <- function(expr, lst) 
  cons(rearrange_matrix_expr(expr), lst)

collect_mult_components_rec.matrix_mult <- function(expr, lst)
    collect_mult_components_rec(expr$left,
              collect_mult_components_rec(expr$right, lst))

collect_mult_components <- function(expr)
    lst_to_list(collect_mult_components_rec(expr, NULL))

## ------------------------------------------------------------------------
A <- matrix(1, nrow = 400, ncol = 300)
B <- matrix(1, nrow = 300, ncol = 30)
C <- matrix(1, nrow = 30, ncol = 500)
D <- matrix(1, nrow = 500, ncol = 400)

expr <- m(A) * m(B) * m(C) * m(D)

## ------------------------------------------------------------------------
expr

## ------------------------------------------------------------------------
rearrange_matrix_expr(expr)

## ------------------------------------------------------------------------
eval_matrix_expr <- function(expr) UseMethod("eval_matrix_expr")
eval_matrix_expr.matrix_data <- function(expr) expr$data
eval_matrix_expr.matrix_mult <- function(expr)
  eval_matrix_expr(expr$left) %*% eval_matrix_expr(expr$right)
eval_matrix_expr.matrix_sum <- function(expr)
  eval_matrix_expr(expr$left) + eval_matrix_expr(expr$right)

## ------------------------------------------------------------------------
v <- function(expr) eval_matrix_expr(rearrange_matrix_expr(expr))

## ------------------------------------------------------------------------
fast <- function(expr) {
  v(build_matrix_expr(substitute(expr), parent.frame()))
}

## ------------------------------------------------------------------------
all(A %*% B %*% C %*% D == fast(A %*% B %*% C %*% D))

## ------------------------------------------------------------------------
res <- microbenchmark(A %*% B %*% C %*% D,
                      fast(A %*% B %*% C %*% D))
options(microbenchmark.unit="relative")
print(res, signif = 3, order = "mean")

