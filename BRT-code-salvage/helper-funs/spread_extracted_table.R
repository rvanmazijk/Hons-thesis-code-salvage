spread_extracted_table <- function(x, var_name) {
    x %>%
        spread_(
            key_col = "fact",
            value_col = paste0("rough_agg_", var_name)
        ) %>%
        na.omit()
}
