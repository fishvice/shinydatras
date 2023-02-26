#' Title
#'
#' @param data
#' @param SUR
#' @param SID
#' @param var
#' @param lab
#' @param cl
#'
#' @return
#' @export
#'
gg_bubble <- function(data, SUR, SID, var, lab = "Fjöldi", cl) {



  NROW <-
    ifelse(SUR %in% c("NS-IBTS_3", "IE-IGFS_4"), 3, 4)

  data <-
    data %>%
    dplyr::filter(year %in% c(2000,
                              2001, 2002, 2003, 2004, 2005,
                              2006, 2007, 2008, 2009, 2010,
                              2011, 2012, 2013, 2014, 2015,
                              2016, 2017, 2018, 2019, 2020,
                              2021, 2022),
                  survey == SUR,
                  sid == SID)

  p <-
    ggplot() +
    theme_minimal(base_size = 12) +
    scale_x_continuous(NULL, NULL, expand = expansion(0)) +
    scale_y_continuous(NULL, NULL, expand = expansion(0)) +
    geom_polygon(data = cl, aes(lon, lat, group = group), colour = "grey", fill = "grey") +
    coord_quickmap(xlim = range(data$lon), ylim = range(data$lat)) +
    theme(panel.spacing.x=unit(0.3, "lines"),panel.spacing.y=unit(0.3, "lines"))

  p +
    ggplot2::geom_point(data = data,
                        ggplot2::aes(lon, lat, size = {{ var }}),
                        alpha = 0.2, colour = "red") +
    ggplot2::geom_point(data = data,
                        ggplot2::aes(lon, lat),
                        size = 0.1, colour = "blue") +
    ggplot2::scale_size_area(max_size = 30) +
    ggplot2::labs(size = lab) +
    ggplot2::facet_wrap(~ year, nrow = NROW, dir = "h")
}

#' Title
#'
#' @param data
#' @param SUR
#' @param SID
#' @param var
#' @param lab
#' @param cl
#'
#' @return
#' @export
#'
gg_bubble1 <- function(data, SUR, SID, var, lab = "Fjöldi", cl) {

  data <-
    data %>%
    dplyr::filter(survey == SUR,
                  sid == SID) |>
    filter(year == max(year))

  p <-
    ggplot() +
    theme_minimal(base_size = 12) +
    scale_x_continuous(NULL, NULL, expand = expansion(0)) +
    scale_y_continuous(NULL, NULL, expand = expansion(0)) +
    geom_polygon(data = cl, aes(lon, lat, group = group), colour = "grey", fill = "grey") +
    coord_quickmap(xlim = range(data$lon), ylim = range(data$lat)) +
    theme(panel.spacing.x=unit(0.3, "lines"),panel.spacing.y=unit(0.3, "lines"))

  p +
    ggplot2::geom_point(data = data,
                        ggplot2::aes(lon, lat, size = {{ var }}),
                        alpha = 0.2, colour = "red") +
    ggplot2::geom_point(data = data,
                        ggplot2::aes(lon, lat),
                        size = 0.1, colour = "blue") +
    ggplot2::scale_size_area(max_size = 30) +
    ggplot2::labs(size = lab)
}

