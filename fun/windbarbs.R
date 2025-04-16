#'
#' Conventions: https://www.mesonet.org/images/site/Wind%20Barb%20Feb%202012.pdf
#' I would prefer to see a segment for speeds 1 < 5 kn (see demo)
#'
#' I decided to do all calculations (position, rotation) in "npc" and "snpc" coordinates.
#' Using a viewport per barb would be much simpler but significantly slower.
#'
#' Aesthetics:
#' - mag (wind speed)
#' - mag.unit (wind speed unit - ms, mph or knots)
#' - angle (wind direction)
#' - colour
#' - fill (of triangle)
#' - lwd (i.e. linewidth of segment, barb and triangle border)
#' - length (of shaft in mm)
#' - calm.size (diameter of calm conditions circle in mm)
#'
#'
#' Todo
#' - treatment of NA values
#' - "skip" functionality: similar to the one used in geom_arrow()?
#' - test geom against different scales/facets settings
#' - allow for u/v input instead of mag/angle
#'
#' https://stackoverflow.com/questions/47814998/how-to-make-segments-that-preserve-angles-in-different-aspect-ratios-in-ggplot2

#' Wind Barbs
#'
#' @param data Data
#' @param coord Coordinates
#' @param panel_params Panel parameters
#' @description
#'
wind_barb <- function(data, coord, panel_params, skip.x, skip.y) {
  # wind direction lookup table
  winddirs <- data.frame(
    text = c(
      "N",
      "NNE",
      "NE",
      "ENE",
      "E",
      "ESE",
      "SE",
      "SSE",
      "S",
      "SSW",
      "SW",
      "WSW",
      "W",
      "WNW",
      "NW",
      "NNW"
    ),
    angle = c(
      0,
      22.5,
      45,
      67.5,
      90,
      112.5,
      135,
      157.5,
      180,
      202.5,
      225,
      247.5,
      270,
      292.5,
      315,
      337.5
    )
  )

  # mph to knots function

  # coords <- data
  coords <- coord$transform(data, panel_params)

  # Dimensions
  length <- unit(coords$length[1], "mm")
  length <- convertUnit(length, "npc", valueOnly = TRUE)

  nx.data <- length(unique(coords$x))
  if (skip.x == "auto") {
    nx.target <- 1 / length
    skip.x <- pmax(0, ceiling(nx.data / nx.target) - 1)
  }
  pick.x <- unique(coords$x)[which(seq_len(nx.data) %% (skip.x + 1) == 0)]

  ny.data <- length(unique(coords$y))
  if (skip.y == "auto") {
    ny.target <- 1 / length
    skip.y <- pmax(0, ceiling(ny.data / ny.target) - 1)
  }
  pick.y <- unique(coords$y)[which(seq_len(ny.data) %% (skip.y + 1) == 0)]

  coords <- coords[coords$x %in% pick.x & coords$y %in% pick.y, ]
  if (any(c(skip.x, skip.y) > 0)) {
    message(sprintf("Skipping nx=%1.0f ny=%1.0f", skip.x, skip.y))
  }

  # Shortcuts
  n <- nrow(coords)
  x <- coords$x
  y <- coords$y
  #CONVERT MPH TO KNOT
  mag <- coords$mag
  col <- coords$colour
  fill <- coords$fill
  lwd <- coords$lwd
  calm.size <- unit(coords$calm.size, "mm")
  angle <- coords$angle

  # Unit conversions
  angle <- if (is.character(angle)) {
    (winddirs[match(x = angle, table = winddirs$text), "angle"] + 270) %% 360
  } else {
    (angle + 270) %% 360
  }

  # Spacing & barb length
  slots <- 6
  height <- length / 3
  width <- length / 6
  point.size <- width / 2
  nudge <- width / 10 # Increase spacing of 5s and 10s

  # Calculate which / how many triangles/barbs
  mag <- round(mag / 5) * 5
  mag <- pmax(0, mag)
  n50 <- floor(mag / 50)
  n10 <- floor((mag - n50 * 50) / 10)
  n5 <- floor((mag - n50 * 50 - n10 * 10) / 5)

  # Single triangle/barb/segment/point geometry
  g50 <- (function() {
    x <- c(-width / 2, width / 2, 0, -width / 2)
    y <- c(0, 0, -height * 0.95, 0) # 0.95, factor to compensate height for linejoin=mitre
    cbind(x, y)
  })()
  g10 <- (function() {
    x <- c(-nudge, width / 2 - nudge)
    y <- c(0, -height)
    cbind(x, y)
  })()
  g5 <- (function() {
    x <- c(-nudge, width / 4 - nudge)
    y <- c(0, -height / 2)
    cbind(x, y)
  })()
  gSegment <- (function() {
    x <- c(0, width)
    y <- c(0, 0)
    cbind(x, y)
  })()
  gPoint <- (function() {
    x <- 0
    y <- 0
    cbind(x, y)
  })()

  # Loop data (might be vectorised)
  grob.list <- lapply(seq_len(n), function(i) {
    #' Rotate/Stretch X functions
    #'
    #' @param x x coordinate
    #' @param y y coordinate
    #' @param a angle in decimal degrees
    #' @param u stretch x
    #' @param v stretch y
    #'
    rotX <- function(x, y, a, u = 1, v = 1) {
      a <- a * pi / 180
      u * (x * cos(a) + y * sin(a))
    }
    rotY <- function(x, y, a, u = 1, v = 1) {
      a <- a * pi / 180
      v * (y * cos(a) - x * sin(a))
    }

    # Slots and position used by triangle/barb (50 uses 1 slot, 10 and 5 uses 0.5 slots)
    slots <- c(rep(1.0, n50[i]), rep(0.5, n10[i]), rep(0.5, n5[i]))
    pos <- if (length(slots) > 0) c(1, cumsum(slots[-length(slots)]) + 1) else
      numeric(0)

    # Build geometries
    geom <- c(
      rep(list(g50), n50[i]),
      rep(list(g10), n10[i]),
      rep(list(g5), n5[i])
    )

    # Build barb grobs
    grob.barbs <- lapply(seq_along(geom), function(j) {
      x1 <- length - pos[j] * width + geom[[j]][, 1] + width / 2
      y1 <- geom[[j]][, 2]
      x2 <- rotX(x1, y1, angle[i])
      y2 <- rotY(x1, y1, angle[i])
      x3 <- unit(x[i], "npc") + unit(x2, "snpc")
      y3 <- unit(y[i], "npc") + unit(y2, "snpc")
      pathGrob(
        x = x3,
        y = y3,
        gp = gpar(
          col = col[i],
          fill = fill[i],
          lwd = lwd[i],
          linejoin = "mitre",
          lineend = "square"
        )
      )
    })

    # Compute shaft length depending on magnitude
    shaft.len <- length

    if (mag[i] == 5) {
      shaft.len <- length # full length
    } else if (mag[i] > 5 && mag[i] < 50 && length(slots) > 0) {
      # Find index of first non-zero-length barb (ignore triangles)
      non_triangle_indices <- which(slots == 0.5)
      if (length(non_triangle_indices) > 0) {
        inner_idx <- non_triangle_indices[1]
        shaft.len <- length - (pos[inner_idx] - 0.5) * width
      } else {
        shaft.len <- length # fallback, if only triangles (shouldn't occur here)
      }
    } else if (mag[i] < 5) {
      shaft.len <- NA # don't draw
    }

    # Build shaft segment
    if (!is.na(shaft.len)) {
      x1 <- c(point.size / 2, shaft.len)
      y1 <- c(0, 0)
      x2 <- rotX(x1, y1, angle[i])
      y2 <- rotY(x1, y1, angle[i])
      x3 <- unit(x[i], "npc") + unit(x2, "snpc")
      y3 <- unit(y[i], "npc") + unit(y2, "snpc")
      grob.segment <- pathGrob(
        x3,
        y3,
        gp = gpar(
          col = col[i],
          lwd = lwd[i],
          linejoin = "mitre",
          lineend = "square"
        )
      )
    } else {
      grob.segment <- nullGrob()
    }

    # Calm conditions: draw open circle only if mag == 0
    if (mag[i] == 0) {
      grob.point <- pointsGrob(
        x[i],
        y[i],
        pch = 1,
        size = calm.size[i],
        gp = gpar(col = col[i], fill = NA, lwd = lwd[i])
      )
    } else {
      grob.point <- nullGrob()
    }

    # Debug markers (optional — remove if not needed)
    debug.segment.end <- if (!is.null(debug)) nullGrob() else if (
      !is.na(shaft.len)
    )
      pointsGrob(
        x = unit(x[i], "npc") + unit(rotX(shaft.len, 0, angle[i]), "snpc"),
        y = unit(y[i], "npc") + unit(rotY(shaft.len, 0, angle[i]), "snpc"),
        pch = 1,
        size = unit(1.5, "mm"),
        gp = gpar(col = "red")
      ) else nullGrob()

    debug.slot.centers <- if (is.null(debug))
      lapply(pos, function(p) {
        center.x <- length - (p - 0.5) * width
        x_rot <- rotX(center.x, 0, angle[i])
        y_rot <- rotY(center.x, 0, angle[i])
        pointsGrob(
          x = unit(x[i], "npc") + unit(x_rot, "snpc"),
          y = unit(y[i], "npc") + unit(y_rot, "snpc"),
          pch = 16,
          size = unit(1.0, "mm"),
          gp = gpar(col = "blue")
        )
      }) else list(nullGrob())

    # Merge everything
    grobs <- do.call(
      gList,
      c(
        grob.barbs,
        list(grob.segment),
        debug.slot.centers,
        list(debug.segment.end),
        list(grob.point)
      )
    )

    grobs
  })

  grobs <- do.call(gList, grob.list)
  gTree(children = grobs)
}

GeomWindBarb <- ggplot2::ggproto(
  "GeomWindBarb",
  ggplot2::Geom,
  required_aes = c("x", "y"),
  default_aes = ggplot2::aes(
    color = "black",
    fill = "transparent",
    lwd = 1,
    linetype = 1,
    alpha = NA,
    angle = 0,
    mag = 0,
    mag.unit = "mph",
    length = 18,
    calm.size = 6
  ),
  draw_panel = function(
    data,
    panel_params,
    coord,
    grob,
    debug,
    skip.x,
    skip.y
  ) {
    if (!is.null(debug)) {
      coords <- coord$transform(data, panel_params)
      debug(data, coords)
    }
    if (grid::is.grob(grob)) {
      grob
    } else {
      if (!is.function(grob)) stop("Invalid grob in GeomWindBarb")
      grob(data, coord, panel_params, skip.x, skip.y)
    }
  }
)

#' Geom Wind Barbs
#'
#' @param mapping mapping
#' @param data data
#' @param stat stat
#' @param position position
#' @param inherit.aes inherit.aes
#' @param show.legend show.legend
#' @param key_glyph key_glyph
#' @param debug debug
#' @param skip.x skip.x
#' @param skip.y skip.y
#' @param skip.border Skip barbs that might overlap with plot border
#' @param ... Further arguments
#' @export
#'
geom_windbarb <- function(
  mapping = NULL,
  data = NULL,
  stat = "identity",
  position = "identity",
  inherit.aes = TRUE,
  show.legend = FALSE,
  key_glyph = NULL,
  debug = NULL,
  skip.x = "auto",
  skip.y = "auto",
  skip.border = FALSE,
  ...
) {
  # Skip rows/cols near border (to be implemented)
  # if(skip.border)

  layer(
    geom = GeomWindBarb,
    mapping = mapping,
    data = data,
    stat = stat,
    position = position,
    inherit.aes = inherit.aes,
    check.aes = FALSE,
    show.legend = show.legend,
    key_glyph = key_glyph,
    params = list(
      grob = wind_barb,
      debug = debug,
      skip.x = skip.x,
      skip.y = skip.y,
      ...
    )
  )
}

# Example
library(grid)
library(ggplot2)
set.seed(1)

# Dummy data
n <- 8
n2 <- n^2
xy <- expand.grid(x = letters[1:(n)], y = seq(1, n, length.out = n))
data <- data.frame(
  x = xy[, 1],
  y = xy[, 2],
  angle = rep(seq(-90, 0, length.out = n), n),
  mag = rep(seq(0, 227, length.out = n2)),
  group = rep(1:1, n)
)
# Use group = rep(c(1, 2), n/2) to show two panels

# Demo plot
pl <- ggplot(data, mapping = aes(x, y)) +
  facet_wrap(~group) +
  geom_windbarb(
    aes(mag = mag, angle = angle),
    data = data[],
    #length = 16,
    #calm.size = 4,
    skip.x = 0,
    skip.y = 0,
    lwd = 1,
    fill = "gray",
    colour = "gray"
  ) +
  #geom_text(aes(label = sprintf("%s kn", round(angle))), data, vjust = 0, nudge_y = -.25) +
  scale_fill_viridis_c() +
  scale_y_continuous(expand = expansion(.15), trans = "identity") +
  scale_x_discrete(expand = expansion(.15)) +
  coord_cartesian() +
  theme_bw()
print(pl)
# ggsave(file.path(dirs$temp, "test1.pdf"), width = 12, height = 12)
