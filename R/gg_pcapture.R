#' Title
#'
#' @param data
#' @param SUR
#' @param SID
#'
#' @return
#' @export
#'
gg_pcapture <- function(data, SUR, SID, cl) {

  data <-
    data |>
    filter(survey == SUR)
  squares <-
    data |>
    select(lon, lat) |>
    distinct() |>
    mutate(sq = shinydatras:::d2ir(lat, lon)) |>
    select(sq) |>
    distinct() |>
    mutate(lon = shinydatras:::ir2d(sq)$lon,
           lat = shinydatras:::ir2d(sq)$lat)

  data |>
    filter(sid == SID) |>
    ggplot() +
    theme_void() +
    scale_x_continuous(expand = expansion(mult = 0)) +
    scale_y_continuous(expand = expansion(mult = 0)) +
    geom_raster(data = squares, aes(lon, lat), fill = "grey90") +
    geom_raster(aes(lon, lat, fill = p), alpha = 0.7) +
    geom_polygon(data = cl, aes(lon, lat, group = group), colour = "grey", fill = "grey") +
    coord_quickmap(xlim = range(data$lon), ylim = range(data$lat)) +
    scale_fill_viridis_d(option = "B", direction = -1, drop = FALSE) +
    labs(fill = "Probability of\ncapture [%]") +
    labs(caption = "What should be put here?")
}