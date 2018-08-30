#' Plots the 95%, 75%, 50% (= median), 25%, and 5% quantile regression fits,
#' and the mean regression fit, for some data-set of `x` vs `y`.
#' The fn also has nice convenient label arguments.
#'
#' @param x A numeric vector, as a column in a data frame `dat` with `y`
#' @param y A numeric vector, as a column in a data frame `dat` with `x`
#' @param dat A data-frame containing `x` & `y`
#' @param x_lab A string, describing the x-axis label for the plot
#' @param y_lab A string, describing the y-axis label for the plot
#' @param main_lab A string, describing the plot title
#' @param sub_lab  A string, describing the plot subtitle
quant_combo_plot <- function(plot_with_base = TRUE, plot_with_ggplot2 = FALSE,
                             x, y, dat, jitter_amnt = 2,
                             x_lab = "x", y_lab = "y",
                             main_lab = "", sub_lab = "",
                             ...) {

    if (plot_with_base) {

        base_plot <- function(x, y, dat, jitter_amnt = 2,
                              x_lab = "x", y_lab = "y",
                              main_lab = "", sub_lab = "",
                              ...) {

            quant_fit <- function(x, y, dat, x_seq,
                                  tau = c(0.95, 0.75, 0.50, 0.25, 0.05)) {
                quantile_models <- vector("list", length = length(tau))
                for (i in seq_along(tau)) {
                    mq_tau <- rq(
                        log(y) ~ log(x) + I(log(x) ^ 2),
                        tau = tau[i],
                        data = dat
                    )
                    y_tau <- stats::predict(
                        mq_tau,
                        list(x = x_seq)
                    )
                    lines(
                        x = x_seq,
                        y = exp(y_tau),
                        # Make 95%q & 5%q dashed (others solid),
                        # And thick if median (others thin):
                        lty = if (tau[i] %in% c(0.95, 0.05)) 2
                              else 1,
                        lwd = if (tau[i] == 0.50) 2
                              else 1,
                        col = "red"
                    )
                }
            }

            lmmod_fit <- function(x, y, dat, x_seq) {
                m <- lm(
                    log(y) ~ log(x) + I(log(x) ^ 2),
                    data = dat
                )
                fit <- stats::predict(
                    m,
                    list(x = x_seq),
                    interval = "confidence"
                )
                for (i in 1:3) {
                    lines(
                        x = x_seq, y = exp(fit[, i]),
                        # Make mean solid, thick line,
                        # And 95%CIs dashes, thin lines:
                        lty = if (i == 1) 1
                              else 2,
                        lwd = if (i == 1) 2
                              else 1,
                        col = "black"
                    )
                }
            }

            plot(
                y ~ jitter(x, jitter_amnt),
                data = dat,
                ylab = y_lab,
                xlab = x_lab,
                main = main_lab,
                sub  = sub_lab,
                pch  = ".",
                col  = "grey"
            )

            x_seq <- dat %$% seq(
                from = min(x, na.rm = T),
                to = max(x, na.rm = T),
                by = (max(x, na.rm = T) - min(x, na.rm = T)) / 1000
            )

            lmmod_fit(x = x, y = y, dat = dat, x_seq = x_seq)
            quant_fit(x = x, y = y, dat = dat, x_seq = x_seq)

        }

        base_plot(
            x = x,
            y = y,
            dat = dat,
            jitter_amnt = jitter_amnt,
            x_lab = x_lab,
            y_lab = y_lab,
            main_lab = main_lab,
            sub_lab = sub_lab
        )

    } else if (plot_with_ggplot2) {

        ggplot2_plot <- function(x, y, dat, jitter_amnt = 2,
                                 x_lab = "x", y_lab = "y",
                                 main_lab = "", sub_lab = "",
                                 ...) {
            # TODO
        }

        ggplot2_plot(
            x, y, dat, jitter_amnt,
            x_lab, y_lab,
            main_lab, sub_lab
        )

    }

}
