#' Plots the 95%, 75%, 50% (= median), 25%, and 5% quantile regression fits,
#' and the mean regression fit, for some data-set of `x` vs `y`.
#' The fn also has nice convenient label arguments.
#' With `gridarrange` to make one big set of panels :)
#'
#' @param x A numeric vector, as a column in a data frame `dat` with `y`
#' @param y A numeric vector, as a column in a data frame `dat` with `x`
#' @param dat A data-frame containing `x` & `y`
#' @param x_lab A string, describing the x-axis label for the plot
#' @param y_lab A string, describing the y-axis label for the plot
#' @param main_lab A string, describing the plot title
#' @param sub_lab  A string, describing the plot subtitle
quant_sexy_plot2 <- function(data, var_name,
                             ylab = "y", main_lab = "", sub_lab = "",
                             ...) {

    # Change fact to continuous
    data$fact %<>% as.character() %>%
        as.numeric()
    # And now re-label it according to degree-squares
    data %<>%
        mutate(scale = 0.05 * fact)

    get_quantile_band <- function(data, region_name = c("GCFR", "SWAFR")) {
        quantiles_0.05_0.95 <- data %>%
            filter(region == region_name) %>%
            ggplot() +
            stat_quantile(
                aes_string(x = "scale", y = glue("rough_agg_{var_name}")),
                quantiles = c(0.05, 0.95),
                formula = y ~ x + I(x ^ 2)
            )
        quantiles_0.05_0.95 %<>%
            plot() %>%
            `$`(data) %>%
            `[[`(1)
        quantiles_0.05_0.95 <- tibble(
            x = quantiles_0.05_0.95$x[1:100],
            five = quantiles_0.05_0.95$y[
                quantiles_0.05_0.95$quantile == 0.05
            ],
            ninetyfive = quantiles_0.05_0.95$y[
                quantiles_0.05_0.95$quantile == 0.95
            ]
        )
        return(quantiles_0.05_0.95)
    }

    GCFR_0.05_0.95 <- get_quantile_band(data, "GCFR")
    SWAFR_0.05_0.95 <- get_quantile_band(data, "SWAFR")


    scatter_plot <- ggplot(data)
        # TODO: add scatter version in appendix
        #geom_jitter(
        #    alpha = 0.1,
        #    aes(x = scale, y = rough_agg_elev, col = region)
        #)

    # Regional 5%-95% quantile bands
    quantile_plot <- scatter_plot +
        geom_ribbon(
            data = GCFR_0.05_0.95,
            alpha = 0.5,
            fill = "#009E73",  # green
            aes(x = x, ymin = five, ymax = ninetyfive)
        ) +
        geom_ribbon(
            data = SWAFR_0.05_0.95,
            alpha = 0.5,
            fill = "#E69F00",  # orange
            aes(x = x, ymin = five, ymax = ninetyfive)
        )

    # Regional medians
    median_plot <- quantile_plot +
        geom_quantile(
            data = data %>%
                filter(region == "GCFR"),
            size = 1,
            col = "#009E73",  # green
            aes_string(x = "scale", y = glue("rough_agg_{var_name}")),
            quantiles = 0.50,
            formula = y ~ x + I(x ^ 2)
        ) +
        geom_quantile(
            data = data %>%
                filter(region == "SWAFR"),
            size = 1,
            col = "#E69F00",  # orange
            aes_string(x = "scale", y = glue("rough_agg_{var_name}")),
            quantiles = 0.50,
            formula = y ~ x + I(x ^ 2)
        )

    # Styling
    sexy_plot <- median_plot +
        labs(
            title = switch(var_name,
                # TODO: units!
                # TODO: nicer abbreviations for:
                "AWCh1"  = "AWCh1",           # *
                "BLDFIE" = "BLDFIE",          # *
                "CECSOL" = "CEC ()",
                "CLYPPT" = "CLYPPT",          # *
                "CRFVOL" = "CRFVOL",          # *
                "elev"   = "Elevation ()",
                "MAP"    = "MAP ()",
                "MLST"   = "MLST (K)",
                "NDVI"   = "NDVI",
                "OCDENS" = "Organic C ()",
                "PCV"    = "PCV",
                "PDQ"    = "PDQ ()",
                "PHIKCL" = "PHIKCL",          # *
                "PWQ"    = "PWQ ()",
                "SLTPPT" = "SLTPPT",          # *
                "SNDPPT" = "SNDPPT",          # *
                "TCQ"    = "TCQ (K)",
                "TWQ"    = "TWQ (K)"
            ),
            y = "Roughness",
            x = "Spatial scale"
        ) +
        scale_x_continuous(
            breaks = c(0.25, 0.50, 0.75),
            labels = c("0.25º", "0.50º", "0.75º")
        ) +
        theme_bw() +
        theme(panel.grid = element_blank(), axis.text.y = element_text(angle = 90))

    plot(sexy_plot)
    return(sexy_plot)

    # TODO: scatter versions for appendix

}
