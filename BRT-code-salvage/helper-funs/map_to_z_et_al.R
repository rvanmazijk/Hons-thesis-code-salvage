map_0_to_1 <- function(x) {
    (x - min(x)) /
    (max(x) + min(x))
}
map_neg1_to_1 <- function(x) {
    2 * ((x - min(x)) / (max(x) + min(x))) - 1
}
map_to_z <- function(x) {
    (x - mean(x)) / sd(x)
}
