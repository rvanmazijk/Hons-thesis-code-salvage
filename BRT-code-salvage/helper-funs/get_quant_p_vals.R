get_quant_p_vals <- function(quant_fits, region_name) {
    get_p_vals_for_a_quantile <- function(quant_fits, region_name, quantile) {
        quant_fits %>%
            magrittr::extract2(quantile) %>%
            magrittr::extract2(region_name) %>%
            summary() %>%
            `$`(coefficients) %>%
            `[`(-c(1:9)) %>%  # keeps only the P-values
            `names<-`(c("int", "log_scale", "log_scale_2"))
    }
    quantile_p_vals <- list(
        five_percent_quantile = get_p_vals_for_a_quantile(
            quant_fits,
            region_name,
            "five_percent_quantile"
        ),
        median = get_p_vals_for_a_quantile(
            quant_fits,
            region_name,
            "median"
        ),
        ninetyfive_percent_quantile = get_p_vals_for_a_quantile(
            quant_fits,
            region_name,
            "ninetyfive_percent_quantile"
        )
    )
    return(quantile_p_vals)
}
