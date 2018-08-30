quant_fit2 <- function(data, var_name) {
    fit_a_model2 <- function(data, tau) {
        rq(
            glue("
                log(rough_agg_{var_name}) ~ \\
                log(scale) + I(log(scale) ^ 2) + \\
                region * log(scale)
            "),
            tau = tau,
            data = data
        )
    }
    quantile_models <- list(
        five_percent_quantile       = fit_a_model2(data, 0.05),
        median                      = fit_a_model2(data, 0.50),
        ninetyfive_percent_quantile = fit_a_model2(data, 0.95)
    )
    return(quantile_models)
}
