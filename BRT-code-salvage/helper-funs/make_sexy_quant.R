make_sexy_quant <- function(var_name, data_GCFR, data_SWAFR) {

    extracted_layer_agg_rough <- combine_roughness_across_scales_from_regions(
        data_GCFR,
        data_SWAFR
    )

    layer_quant_fits <- quant_fit(extracted_layer_agg_rough, var_name)

    p_vals <- list(
        GCFR = get_quant_p_vals(layer_quant_fits, "GCFR"),
        SWAFR = get_quant_p_vals(layer_quant_fits, "SWAFR")
    )

    sexy_plot <- quant_sexy_plot(extracted_layer_agg_rough, var_name)

    return(list(
        layer_quant_fits = layer_quant_fits,
        p_vals = p_vals,
        sexy_plot = sexy_plot
    ))

}
