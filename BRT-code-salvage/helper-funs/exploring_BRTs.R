make_r2_and_nt_plots <- function(region_saved_BRTs_summary) {

    r2_plot <- ggplot(region_saved_BRTs_summary) +
        geom_tile(aes(x = tc, y = factor(lr), fill = pseudo_r2)) +
        facet_grid( ~ scale) +
        labs(
            x = expression(paste(italic("tc"))),
            y = expression(paste(italic("lr")))
        ) +
        scale_y_discrete(labels = c(
            expression(paste("1x10" ^ -4)),
            expression(paste("5x10" ^ -4)),
            expression(paste("1x10" ^ -3)),
            expression(paste("5x10" ^ -3)),
            expression(paste("1x10" ^ -2))
        )) +
        scale_fill_distiller(
            palette = "Spectral",
            name = expression(paste("pseudo-", italic("R") ^ 2)),
            lim = c(0, 0.725)
        ) +
        theme_minimal()

    nt_plot <- ggplot(region_saved_BRTs_summary) +
        geom_tile(aes(x = tc, y = factor(lr), fill = nt)) +
        facet_grid( ~ scale) +
        labs(
            x = expression(paste(italic("tc"))),
            y = expression(paste(italic("lr")))
        ) +
        scale_y_discrete(labels = c(
            expression(paste("1x10" ^ -4)),
            expression(paste("5x10" ^ -4)),
            expression(paste("1x10" ^ -3)),
            expression(paste("5x10" ^ -3)),
            expression(paste("1x10" ^ -2))
        )) +
        scale_fill_distiller(
            palette = "Spectral",
            name = expression(paste(italic("nt"))),
            lim = c(0, 4250)
        ) +
        theme_minimal()

    plot(r2_plot)
    plot(nt_plot)
    return(list(
        r2_plot = r2_plot,
        nt_plot = nt_plot
    ))

}

make_contrib_summary <- function(region_saved_BRTs) {

    make_contrib_summary_for_a_scale <- function(x) {

        return(x %$% list(

            lr_0.01 = list(
                summary(tol_5e_04_tc_1_lr_0.01 ),
                summary(tol_5e_04_tc_2_lr_0.01 ),
                summary(tol_5e_04_tc_3_lr_0.01 ),
                summary(tol_5e_04_tc_4_lr_0.01 ),
                summary(tol_5e_04_tc_5_lr_0.01 )
            ),

            lr_0.005 = list(
                summary(tol_5e_04_tc_1_lr_0.005),
                summary(tol_5e_04_tc_2_lr_0.005),
                summary(tol_5e_04_tc_3_lr_0.005),
                summary(tol_5e_04_tc_4_lr_0.005),
                summary(tol_5e_04_tc_5_lr_0.005)
            ),

            lr_0.001 = list(
                summary(tol_5e_04_tc_1_lr_0.001),
                summary(tol_5e_04_tc_2_lr_0.001),
                summary(tol_5e_04_tc_3_lr_0.001),
                summary(tol_5e_04_tc_4_lr_0.001),
                summary(tol_5e_04_tc_5_lr_0.001)
            ),

            lr_5e_04 = list(
                summary(tol_5e_04_tc_1_lr_5e_04),
                summary(tol_5e_04_tc_2_lr_5e_04),
                summary(tol_5e_04_tc_3_lr_5e_04),
                summary(tol_5e_04_tc_4_lr_5e_04),
                summary(tol_5e_04_tc_5_lr_5e_04)
            ),

            lr_1e_04 = list(
                summary(tol_5e_04_tc_1_lr_1e_04),
                summary(tol_5e_04_tc_2_lr_1e_04),
                summary(tol_5e_04_tc_3_lr_1e_04),
                summary(tol_5e_04_tc_4_lr_1e_04),
                summary(tol_5e_04_tc_5_lr_1e_04)
            )

        ))

    }

    return(region_saved_BRTs %$% list(
        scale_QDS  = make_contrib_summary_for_a_scale(scale_QDS),
        scale_HDS  = make_contrib_summary_for_a_scale(scale_HDS),
        scale_3QDS = make_contrib_summary_for_a_scale(scale_3QDS)
    ))

}
