#' Title
#'
#' @param median
#' @param by.year
#' @param SUR
#' @param SID
#' @param var
#' @param lab
#'
#' @return
#' @export
#'
gg_length <- function(median, by.year, SUR, SID, var, lab = "Fjöldi í hverju lengdarbili") {

  ggplot2::ggplot() +
    ggplot2::theme_bw(base_size = 14) +
    ggplot2::geom_ribbon(data = median %>%
                           dplyr::filter(survey == SUR, sid == SID),
                         ggplot2::aes(length, ymax = {{ var }}, ymin = 0), fill = "grey") +
    ggplot2::geom_line(data = by.year  %>%
                         dplyr::filter(survey == SUR, sid == SID),
                       ggplot2::aes(length, {{ var }})) +
    ggplot2::facet_wrap(~ year, dir = "v", ncol = 3, strip.position = "right") +
    ggplot2::labs(x = NULL, y = lab) +
    ggplot2::scale_x_continuous(breaks = seq(10, 200, by = 20)) +
    guides(x = guide_axis(n.dodge = 2))

}

