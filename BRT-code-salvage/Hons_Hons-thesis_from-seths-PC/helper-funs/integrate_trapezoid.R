integrate_trapezoid <- function(values, depths = c(sl1 = 0.00,
                                                   sl2 = 0.05,
                                                   sl3 = 0.15,
                                                   sl4 = 0.30,
                                                   sl5 = 0.60,
                                                   sl6 = 1.00,
                                                   sl7 = 2.00)) {
    depth_intervals          <- depths[2:7] - depths[1:6]
    depth_interval_weights   <- depth_intervals / depths[7]
    interval_values          <- (values[1:6] + values[2:7]) / 2
    weighted_interval_values <- depth_interval_weights * interval_values
    integral                 <- sum(weighted_interval_values)
    return(integral)
}
