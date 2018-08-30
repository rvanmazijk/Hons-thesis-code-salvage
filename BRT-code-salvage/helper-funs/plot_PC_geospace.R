plot_PC_geospace <- function(x,
                             pc1, pc2,
                             region_a, region_b = NULL,
                             border_a, border_b = NULL,
                             pt_size = 2) {

    arrange_PC_geospace_child <- function(x_child,
                                          pc1, pc2,
                                          border, region,
                                          scale_limits_pc1, scale_limits_pc2,
                                          pt_size) {

        plot_PC1 <- ggplot(x_child) +
            geom_point(
                data = x_child,
                aes(x = lon, y = lat, col = pc1),
                size = pt_size
            ) +
            scale_color_distiller(
                name = "PC1",
                palette = "Spectral",
                limits = scale_limits_pc1
            ) +
            geom_polygon(
                data = border,
                aes(x = long, y = lat, group = group),
                colour = "black", fill = NA
            ) +
            labs(
                x = "Longitude (º)",
                y = "Latitude (º)"
            ) +
            theme_bw() +
            theme(legend.position = "top")

        plot_PC2 <- ggplot(x_child) +
            geom_point(
                data = x_child,
                aes(x = lon, y = lat, col = pc2),
                size = pt_size
            ) +
            scale_color_distiller(
                name = "PC2",
                palette = "Spectral",
                limits = scale_limits_pc2
            ) +
            geom_polygon(
                data = border,
                aes(x = long, y = lat, group = group),
                colour = "black", fill = NA
            ) +
            labs(
                x = "Longitude (º)",
                y = "Latitude (º)"
            ) +
            theme_bw() +
            theme(legend.position = "top")

        return(arrangeGrob(
            plot_PC1, plot_PC2,
            widths = c(15, 15), heights = 10
        ))

        # FIXME: tidy!

        #p <- ggplot(data = x_child, aes(x = lon, y = lat)) +
        #    geom_polygon(
        #        data = border, aes(x = long, y = lat, group = group),
        #        colour = "black", fill = NA
        #    ) +
        #    theme_classic() +
        #    theme(legend.position = "top")
        #p_pc1 <- p +
        #    geom_point(data = x_child, aes(col = pc1), size = pt_size) +
        #    scale_color_distiller(
        #        name = "PC1",
        #        palette = "Spectral", limits = scale_limits_pc1
        #    )
        #p_pc2 <- p +
        #    geom_point(data = x_child, aes(col = pc2), size = pt_size) +
        #    scale_color_distiller(
        #        name = "PC2",
        #        palette = "Spectral", limits = scale_limits_pc2
        #    )
        #return(arrangeGrob(
        #    p_pc1, p_pc2,
        #    widths = c(15, 15), heights = 10
        #))

    }

    if (not(is.null(region_b)) && not(is.null(border_b))) {

        x_a <- x %>%
            cbind(pc1 = pc1, pc2 = pc2) %>%
            filter(region == region_a)

        x_b <- x %>%
            cbind(pc1 = pc1, pc2 = pc2) %>%
            filter(region == region_b)

        p_a <- arrange_PC_geospace_child(
            x_child = x_a,
            pc1 = pc1, pc2 = pc2,
            region = region_a, border = border_a,
            scale_limits_pc1 = range(pc1),
            scale_limits_pc2 = range(pc2),
            pt_size = pt_size
        )

        p_b <- arrange_PC_geospace_child(
            x_child = x_b,
            pc1 = pc1, pc2 = pc2,
            region = region_b, border = border_b,
            scale_limits_pc1 = range(pc1),
            scale_limits_pc2 = range(pc2),
            pt_size = pt_size
        )

        p_a_b <- arrangeGrob(
            p_a, p_b,
            widths = 15, heights = c(10, 10)
        )

        #plot(p_a_b)
        return(p_a_b)

    } else if (is.null(region_b) && is.null(border_b)) {

        # FIXME: old code

        #x %<>% cbind(pc1 = pc1, pc2 = pc2)

        #p <- arrange_PC_geospace_child(
        #    x_child = x,
        #    pc1 = pc1, pc2 = pc2,
        #    region = region_a, border = border_a,
        #    scale_limits_pc1 = range(pc1),
        #    scale_limits_pc2 = range(pc2),
        #    pt_size = 2
        #)

        #plot(p)
        #return(p)

    } else {

        stop("
            You either need 2 regions w/ 2 borders, or 1 region w/ 1 border!
        ")

    }

}
