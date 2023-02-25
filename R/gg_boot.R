#' Title
#'
#' @param data
#' @param SUR
#' @param SID
#' @param ylab
#'
#' @return
#' @export
#'
gg_boot <- function(data, SUR, SID, ylab = "Fjöldi í togi") {
  data %>%
    dplyr::filter(survey == SUR, sid == SID) %>%
    ggplot2::ggplot(ggplot2::aes(year, mean)) +
    ggplot2::theme_bw(base_size = 16) +
    ggplot2::geom_pointrange(ggplot2::aes(year, mean, ymin = lower.ci, ymax = upper.ci)) +
    ggplot2::scale_x_continuous(breaks = seq(1985, 2025, by = 5)) +
    ggplot2::expand_limits(y = 0) +
    ggplot2::labs(x = NULL, y = ylab)
}
