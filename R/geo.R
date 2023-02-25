ir2d <- function (ir, useI = FALSE) {
  lat <- substring(ir, 1, 2)
  lat <- as.numeric(lat)
  lat <- (lat + 71)/2 + 0.25
  lon1 <- substring(ir, 3, 3)
  lon1 <- toupper(lon1)
  lon1 <- match(lon1, LETTERS)
  if (!useI)
    lon1 <- ifelse(lon1 > 8, lon1 - 1, lon1)
  lon1 <- lon1 - 2
  lon2 <- substring(ir, 4)
  lon2 <- as.numeric(lon2)
  lon <- ifelse(lon1 < 0, -44 + lon2 + 0.5, -40 + 10 * lon1 +
                  lon2 + 0.5)
  data.frame(lat = lat, lon = lon)
}

d2ir <- function (lat, lon = NULL, useI = FALSE) {
  if (is.null(lon)) {
    lon <- lat$lon
    lat <- lat$lat
  }
  lat <- lat + 1e-06
  lon <- lon + 1e-06
  outside <- lat < 36 | lat >= 85.5 | lon <= -44 | lon > 68.5
  if (any(outside))
    warning("Positions outside of ICES statistical area")
  lat <- floor(lat * 2) - 71
  lat <- ifelse(lat < 10, paste("0", lat, sep = ""), lat)
  if (useI)
    lettersUsed <- LETTERS[1:12]
  else lettersUsed <- LETTERS[c(1:8, 10:13)]
  lon1 <- lettersUsed[(lon + 60)%/%10]
  lon2 <- ifelse(lon1 == "A", floor(lon%%4), floor(lon%%10))
  ir <- paste(lat, lon1, lon2, sep = "")
  ir[outside] <- NA
  ir
}
