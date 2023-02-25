#' Title
#'
#' @param data
#' @param SUR
#' @param SID
#' @param now.year
#'
#' @return
#' @export
#'
gg_glyph <- function(data, SUR, SID, now.year) {

  # add a year before and after


  n.glyph <-
    data |>
    dplyr::filter(survey == SUR, sid == SID) %>%
    glyphs(x_major = "lon",
           y_major = "lat",
           x_minor = "year",
           y_minor = "Y",
           width = 1,
           height = 0.5)

  n.glyph %>%
    dplyr::mutate(years = ifelse(between(year, 2000, 2021), "history", "current"),
                  pos = ifelse(Y != 0, TRUE, FALSE),
                  base = lat - 0.25,
                  gy = ifelse(Y == 0 & between(year, 2000, 2022), gy + 0.005, gy)) %>%
    ggplot2::ggplot() +
    ggplot2::theme_minimal() +
    ggplot2::geom_linerange(ggplot2::aes(x = gx, ymin = base, ymax = gy,
                                         colour = years)) +
    ggplot2::geom_polygon(data = d$cl, ggplot2::aes(lon, lat, group = group), fill = "grey", alpha = 0.7) +
    scale_longitude_ices() +
    scale_latitude_ices() +
    ggplot2::scale_colour_manual(values = c("history" = "#377EB8", "current" = "#E41A1C")) +
    ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_line(size = 1),
                   axis.ticks = ggplot2::element_blank(),
                   legend.position = "none") +
    labs(x = NULL, y = NULL)
}

glyphs <- function (data, x_major, x_minor, y_major, y_minor, polar = FALSE,
                    height = ggplot2::rel(0.95), width = ggplot2::rel(0.95),
                    y_scale = identity, x_scale = identity) {
  data$gid <- interaction(data[[x_major]], data[[y_major]],
                          drop = TRUE)
  rescale01 <- function (x, xlim = NULL)  {
    if (is.null(xlim)) {
      rng <- range(x, na.rm = TRUE)
    }
    else {
      rng <- xlim
    }
    (x - rng[1])/(rng[2] - rng[1])
  }
  rescale11 <- function (x, xlim = NULL) {
    2 * rescale01(x, xlim) - 1
  }
  if (ggplot2:::is.rel(width)) {
    width <- resolution(data[[x_major]], zero = FALSE) *
      unclass(width)
    message("Using width ", format(width, digits = 3))
  }
  if (ggplot2:::is.rel(height)) {
    height <- resolution(data[[y_major]], zero = FALSE) *
      unclass(height)
    message("Using height ", format(height, digits = 3))
  }
  if (!identical(x_scale, identity) || !identical(y_scale,
                                                  identity)) {
    data <- ddply(data, "gid", function(df) {
      df[[x_minor]] <- x_scale(df[[x_minor]])
      df[[y_minor]] <- y_scale(df[[y_minor]])
      df
    })
  }
  if (polar) {
    theta <- 2 * pi * rescale01(data[[x_minor]])
    r <- rescale01(data[[y_minor]])
    data$gx <- data[[x_major]] + width/2 * r * sin(theta)
    data$gy <- data[[y_major]] + height/2 * r * cos(theta)
    data <- data[order(data[[x_major]], data[[x_minor]]),
    ]
  }
  else {
    data$gx <- data[[x_major]] + rescale11(data[[x_minor]]) *
      width/2
    data$gy <- data[[y_major]] + rescale11(data[[y_minor]]) *
      height/2
  }
  structure(data, width = width, height = height, polar = polar,
            x_major = x_major, y_major = y_major, class = c("glyphplot",
                                                            "data.frame"))
}

