quant_fit <- function(data, var_name) {
    fit_a_model <- function(tau, region_name) {
        rq(
            glue("
                log(rough_agg_{var_name}) ~ log(scale) + I(log(scale) ^ 2)
            "),
            tau = tau,
            data = data %>%
                filter(region == region_name)
        )
    }
    quantile_models <- list(
        five_percent_quantile = list(
            GCFR  = fit_a_model(0.05, "GCFR"),
            SWAFR = fit_a_model(0.05, "SWAFR")
        ),
        median = list(
            GCFR  = fit_a_model(0.50, "GCFR"),
            SWAFR = fit_a_model(0.50, "SWAFR")
        ),
        ninetyfive_percent_quantile = list(
            GCFR  = fit_a_model(0.95, "GCFR"),
            SWAFR = fit_a_model(0.95, "SWAFR")
        )
    )
    return(quantile_models)
}
