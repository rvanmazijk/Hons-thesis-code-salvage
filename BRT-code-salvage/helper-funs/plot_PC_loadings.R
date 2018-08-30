# Plots the rotation values of a set of pseudo-continuous variables from a PCA,
# and fits a quadratic curve using `lm()` for the rotation ~ variable,
# for both PC1 & PC2, side-by-side

plot_PC_loadings <- function(x = "prcomp", var_name, region_name,
                             facts = 1:15, scales = 0.05 * facts,
                             plot_with_base = FALSE, plot_with_ggplot = FALSE,
                             plot_fits = FALSE) {

    stopifnot(
        class(x) == "prcomp",
        is.list(x)
    )

    if (not(plot_with_base) && not(plot_with_ggplot)) {
        message("Plot method unspecified.\nDefaulting to `base::plot()`")
        plot_with_base <- TRUE
    }

    make_base_plot_plot <- function(PCs, scales, facts) {

        op <- par()
        par(mfrow = c(1, 2))

        for (i in 1:2) {
            plot(
                PCs[[i]],
                ylim = c(-max(abs(PCs[[i]])), max(abs(PCs[[i]]))),
                main = glue::glue("PC{i} loadings"),
                ylab = "Rotation",
                xlab = "Scale (º)",
                xaxt = "n"
            )
            axis(1, labels = scales, at = facts)
            abline(h = 0, lty = 2)
            fit <- lm(x$rotation[, i] ~ c(scales) + I(c(scales) ^ 2))
            newdat <- data.frame(fact = seq(
                from = 1,
                to = length(scales),
                length.out = length(scales)
            ))
            newdat$fit <- predict(fit, newdat, interval = "confidence")[, 1]
            newdat$lwr <- predict(fit, newdat, interval = "confidence")[, 2]
            newdat$upr <- predict(fit, newdat, interval = "confidence")[, 3]
            with(newdat, lines(x = fact, y = upr))
            with(newdat, lines(x = fact, y = fit))
            with(newdat, lines(x = fact, y = lwr))
        }

        par(op)

    }

    make_ggplot_plot <- function(PCs, scales) {

        make_tidy_PCs <- function(PCs, scales) {
            tibble(
                scale = scales,
                PC1 = PCs$PC1,
                PC2 = PCs$PC2
            ) %>%
            gather(key = "PC", value = "loading", -scale)
        }

        tidy_PCs <- make_tidy_PCs(PCs, scales)

        basic_plot <- ggplot(tidy_PCs) +
            geom_hline(yintercept = 0, linetype = "dashed") +
            geom_point(
                col = "grey50",
                aes(x = scale, y = loading)
            ) +
            ylim(c(
                -max(abs(tidy_PCs$loading)) %>% multiply_by(1.5),
                max(abs(tidy_PCs$loading)) %>% multiply_by(1.5)
            ))

        if (plot_fits) {
            basic_plot <- basic_plot +
                geom_smooth(
                    col = "grey50",
                    aes(x = scale, y = loading),
                    method = "lm",
                    formula = y ~ x + I(x ^ 2)
                )
        }

        themed_plot <- basic_plot +
            facet_grid(~ PC) +
            labs(
                x = "Scale",
                y = "Rotation",
                title = glue("{var_name}")
            ) +
            scale_x_continuous(
                breaks = c(0.25, 0.50, 0.75),
                labels = c("0.25º", "0.50º", "0.75º")
            ) +
            theme_bw() +
            theme(
                panel.grid = element_blank(),
                legend.position = "none"
            )

        final_plot <- themed_plot
        #plot(final_plot)
        return(final_plot)

    }

    PCs <- force_positive_loadings(
        PC1 = x$rotation[, 1],
        PC2 = x$rotation[, 2]
    )

    if (plot_with_base) {
        make_base_plot_plot(PCs, scales, facts)
    } else if (plot_with_ggplot) {
        make_ggplot_plot(PCs, scales)
    }

}
