# A handy way to add a *new* member to a vector or list
# (essentially the same as `c(x, y)`, but checks for membership)
`%and%` <- function(x, y) {
    for (i in y) {
        if (i %notin% x) {
            x <- c(x, i)
        }
    }
    return(x)
}
# E.g.
#
# > 1:3 %and% 4
# [1] 1 2 3 4
#
# First argument determines whether return is list or vector, s.t.
#
# > list(1, 2, 3) %and% 4
# [[1]]
# [1] 1
#
# [[2]]
# [1] 2
#
# ...
#
# Versus:
#
# > 1 %and% list(2, 3, 4)
# [1] 1 2 3 4
