
# Functions, classes and operators

## The S3 object-oriented programming system


### Objects and classes



```r
class(4)
```

```
## [1] "numeric"
```

```r
class("foo")
```

```
## [1] "character"
```

```r
class(TRUE)
```

```
## [1] "logical"
```

```r
class(sin)
```

```
## [1] "function"
```


```r
class(sin) <- "foo"
class(sin)
```

```
## [1] "foo"
```


```r
sin(0)
```

```
## [1] 0
```


### Generic functions



```r
foo <- function(x, y, z) UseMethod("foo")
```


```r
foo(1, 2, 3)
```

```
## Error in UseMethod("foo"): no applicable method for 'foo' applied to an object of class "c('double', 'numeric')"
```


```r
foo.default <- function(x, y, z) {
   cat("default foo\n")
}
```


```r
foo(1, 2, 3)
```

```
## default foo
```



```r
foo.numeric <- function(x, y, z) {
   cat("numeric\n")
}
```


```r
foo(1, 2, 3)
```

```
## numeric
```


```r
bar <- function(x, y, z) UseMethod("foo", y)
```


```r
foo("foo",2,3)
```

```
## default foo
```

```r
bar("foo",2,3)
```

```
## numeric
```

```r
bar(1,"bar",3)
```

```
## default foo
```


```r
x <- 1
foo(x, 2, 3)
```

```
## numeric
```


```r
class(x) <- c("a", "b", "c")
foo(x, 2, 3)
```

```
## default foo
```


```r
foo.a <- function(x, y, z) cat("a\n")
foo.b <- function(x, y, z) cat("b\n")
foo.c <- function(x, y, z) cat("c\n")
foo(x, 2, 3)
```

```
## a
```


```r
class(x) <- c("b", "a", "c")
foo(x, 2, 3)
```

```
## b
```

```r
class(x) <- c("c", "b", "a")
foo(x, 2, 3)
```

```
## c
```


```r
foo.a <- function(x, y, z) {
  cat("a\n")
  NextMethod()
}
foo.b <- function(x, y, z) {
  cat("b\n")
  NextMethod()
}
foo.c <- function(x, y, z) {
  cat("c\n")
  NextMethod()
}
```


```r
class(x) <- c("a", "b", "c")
foo(x, 2, 3)
```

```
## a
## b
## c
## default foo
```

```r
class(x) <- c("b", "a", "c")
foo(x, 2, 3)
```

```
## b
## a
## c
## default foo
```

```r
class(x) <- c("c", "b", "a")
foo(x, 2, 3)
```

```
## c
## b
## a
## default foo
```

### Operator overloading


```r
`+.a` <- function(e1, e2) {
  cat("+.a\n")
  NextMethod()
}
x + 2
```

```
## +.a
```

```
## [1] 3
## attr(,"class")
## [1] "c" "b" "a"
```


```r
x + 3
```

```
## +.a
```

```
## [1] 4
## attr(,"class")
## [1] "c" "b" "a"
```

```r
3 + x
```

```
## +.a
```

```
## [1] 4
## attr(,"class")
## [1] "c" "b" "a"
```


```r
x <- 1 ; y <- 3
class(x) <- "a"
class(y) <- "b"
x + y
```

```
## +.a
```

```
## [1] 4
## attr(,"class")
## [1] "a"
```

```r
y + x
```

```
## +.a
```

```
## [1] 4
## attr(,"class")
## [1] "b"
```


```r
`+.b` <- function(e1, e2) {
  cat("+.b\n")
  NextMethod()
}

x + y
```

```
## Warning: Incompatible methods ("+.a", "+.b") for "+"
```

```
## [1] 4
## attr(,"class")
## [1] "a"
```

```r
y + x
```

```
## Warning: Incompatible methods ("+.b", "+.a") for "+"
```

```
## [1] 4
## attr(,"class")
## [1] "b"
```


```r
class(x) <- c("a", "b")
x + 2
```

```
## +.a
## +.b
```

```
## [1] 3
## attr(,"class")
## [1] "a" "b"
```

```r
x + y
```

```
## Warning: Incompatible methods ("+.a", "+.b") for "+"
```

```
## [1] 4
## attr(,"class")
## [1] "a" "b"
```


```r
`!.a` <- function(x) {
  cat("Not for a\n")
  NextMethod()
}
!x
```

```
## Not for a
```

```
## [1] FALSE
```


```r
`+.a` <- function(e1, e2) {
  if (missing(e2)) {
    cat("Unary\n")
  } else {
    cat("Binary\n")
  }
  NextMethod()
}

class(x) <- "a"
+x
```

```
## Unary
```

```
## [1] 1
## attr(,"class")
## [1] "a"
```

```r
2+x
```

```
## Binary
```

```
## [1] 3
## attr(,"class")
## [1] "a"
```


### Group generics


```r
Ops.c <- function(e1, e2) {
  cat(paste0("Ops.c (", .Generic, ")\n"))
  NextMethod()
}

z <- 2
class(z) <- "c"
z + 1
```

```
## Ops.c (+)
```

```
## [1] 3
## attr(,"class")
## [1] "c"
```

```r
1 + z
```

```
## Ops.c (+)
```

```
## [1] 3
## attr(,"class")
## [1] "c"
```

```r
z ^ 3
```

```
## Ops.c (^)
```

```
## [1] 8
## attr(,"class")
## [1] "c"
```


```r
class(z) <- c("a", "c")
1 + z
```

```
## Binary
```

```
## [1] 3
## attr(,"class")
## [1] "a" "c"
```

```r
2 * z
```

```
## Ops.c (*)
```

```
## [1] 4
## attr(,"class")
## [1] "a" "c"
```


## Precedence and evaluation order


Operator      Usual meaning
--------      -----------------
`[` `[[`        Indexing
`^`              Exponentiation (Evaluates right to left)
`-` `+`          Unary minus and plus
`%any%`       Special operators
`*` `/`          Multiply, divide
`+` `-`          Binary add and subtract
`<` `>` 
`<=` `>=`     Ordering and comparison
`==` `!=` 
`!`              Negation
`&` `&&`        And
`|` `||`        Or
`:=`          Assignment
`->` `->>`    Assignment
`<-` `<<-`    Assignment (right to left)
`?`           Help


## Code blocks


```r
`%times%` <- function(n, body) {
  body <- substitute(body)
  for (i in 1:n)
    eval(body, parent.frame())
}
```


```r
4 %times% cat("foo\n")
```

```
## foo
## foo
## foo
## foo
```


```r
2 %times% {
  cat("foo\n")
  cat("bar\n")
}
```

```
## foo
## bar
## foo
## bar
```

