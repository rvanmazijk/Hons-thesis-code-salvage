# Something nicer than `!(x %in% y)` or `magrittr::not(x %in% y)`
# (the latter is still better than the former)
`%notin%` <- function(x, y) {
    !(x %in% y)
}
# E.g.
#
# > 1 %notin% 10:20
# [1] TRUE
