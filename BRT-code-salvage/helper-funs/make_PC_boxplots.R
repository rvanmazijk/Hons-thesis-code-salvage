make_PC_boxplots <- function(x) {

    basic_plot <- x %>%
        ggplot(aes(x = region, col = region, y = PC_value_z)) +
        geom_boxplot(outlier.shape = NA) +  # !!
        geom_hline(
            yintercept = 0,
            linetype = "dashed",
            col = "grey50"
        ) +
        ylim(c(-4, 4)) +  # !!
        facet_grid(variable ~ PC) +
        stat_summary(fun.y = mean, geom = "point") +  # !!
        coord_flip()

    themed_plot <- basic_plot +
        labs(
            x = "",
            y = "Z-score of PC value"
        ) +
        scale_x_discrete(labels = c("", "")) +
        scale_color_manual(values = c(
            "#009E73", # green
            "#E69F00"  # orange
        )) +
        theme_bw() +
        theme(
            panel.grid = element_blank(),
            strip.background = element_rect(
                fill = "white",
                colour = "white"
            ),
            strip.text.y = element_text(angle = 0),
            legend.position = "left",
            legend.title = element_blank(),
            axis.ticks.y = element_blank()
        )

    final_plot <- themed_plot
    plot(final_plot)
    return(final_plot)

}
