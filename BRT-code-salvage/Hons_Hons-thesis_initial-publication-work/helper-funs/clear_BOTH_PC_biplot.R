clear_BOTH_PC_biplot <- function(x, var_name, xlab, ylab) {

    tidy_PCs <- x %$% tibble(
        PC1 = PCA$x[, 1],
        PC2 = PCA$x[, 2],
        region = extracted_pts$region
    )

    basic_plot <- ggplot(tidy_PCs) +
        geom_hline(yintercept = 0, linetype = "dashed") +
        geom_vline(xintercept = 0, linetype = "dashed") +
        geom_point(
            alpha = 0.25,
            aes(x = PC1, y = PC2, col = region)
        ) +
        xlim(c(
            -max(abs(tidy_PCs$PC1)),
             max(abs(tidy_PCs$PC1))
        )) +
        ylim(c(
            -max(abs(tidy_PCs$PC2)),
             max(abs(tidy_PCs$PC2))
        ))

    themed_plot <- basic_plot +
        labs(title = var_name, x = xlab, y = ylab) +
        scale_color_manual(values = c(
            "#009E73", # green
            "#E69F00"  # orange
        )) +
        theme_bw() +
        theme(
            panel.grid = element_blank(),
            legend.title = element_blank()
        )
        # TODO: get legend colours with no transparency

    final_plot <- themed_plot
    #plot(final_plot)
    return(final_plot)

}
