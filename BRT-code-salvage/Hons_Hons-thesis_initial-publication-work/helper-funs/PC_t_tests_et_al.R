PC_t_tests <- function(x) {

    a_PC_t_test <- function(x, PC_name) {

        x %<>% filter(PC == PC_name)

        var_names <- levels(x$variable)
        t_test_results <- vector("list", length = length(var_names))
        names(t_test_results) <- var_names

        for (i in seq_along(var_names)) {
            t_test_results[[i]] <- x %>%
                filter(variable == var_names[i]) %$%
                t.test(
                    PC_value[region == "GCFR"],
                    PC_value[region == "SWAFR"]
                )
        }

        return(t_test_results)

    }

    return(list(
        PC1 = a_PC_t_test(x, "PC1\nroughness"),
        PC2 = a_PC_t_test(x, "PC2\nspatial scale")
    ))

}

get_PC_t_test_p_vals <- function(x) {
    p_vals <- vector(length = length(x))
    names(p_vals) <- names(x)
    for (i in seq_along(x)) {
        p_vals[[i]] <- x[[i]]$p.value
    }
    return(p_vals)
}
