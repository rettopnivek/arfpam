# Plotting
# Written by Kevin Potter
# email: kevin.w.potter@gmail.com
# Please email me directly if you
# have any questions or comments
# Last updated 2021-06-15

# Table of contents
# 1) blank_plot
# 2) palettes
# 3) hv_line
# 4) fill_plot
# 5) add_axes
# 6) col_to_hex
# 7) error_bars
# 8) add_f_to_plot

# TO DO
# - Add additional color palettes

###
### 1) blank_plot
###

#' Generate a Blank Plot
#'
#' This function generates a completely blank plot.
#'
#' @param xDim The lower and upper boundaries for the
#'   x-axis.
#' @param yDim The lower and upper boundaries for the
#'   y-axis.
#' @param margins Logical; if \code{TRUE} displays
#'   guidelines and details regarding the figure
#'   margins to aid in axis and legend specifications.
#' @param cex Text size to use when marking axis line
#'   positions.
#'
#' @return An empty plot.
#'
#' @examples
#' # With margin info
#' blank_plot(margins = TRUE)
#' @export

blank_plot <- function(xDim = c(0, 1), yDim = c(0, 1),
                       margins = FALSE, cex = 1) {
  plot(xDim, yDim,
    type = "n", ylab = " ", xlab = " ",
    xaxt = "n", yaxt = "n", bty = "n"
  )


  if (margins) {

    # User specified boundaries for
    # plotting window
    segments(xDim[c(1, 2, 1, 1)],
      yDim[c(1, 1, 1, 2)],
      xDim[c(1, 2, 2, 2)],
      yDim[c(2, 2, 1, 2)],
      col = "black",
      lwd = 2
    )

    # Adjusted boundaries for
    # plotting window

    bnd <- par("usr")

    segments(bnd[c(1, 2, 1, 1)],
      bnd[c(3, 3, 3, 4)],
      bnd[c(1, 2, 2, 2)],
      bnd[c(4, 4, 3, 4)],
      col = "grey90",
      lwd = 2
    )

    mrg <- par("mar")
    out <- sapply(1:4, function(s) {
      for (i in -1:floor(mrg[s])) {
        pst <- xDim[1] + diff(xDim) / 2
        if (s %in% c(2, 4)) {
          pst <- yDim[1] + diff(yDim) / 2
        }
        axis(s, pst, i, line = i, tick = F, cex.axis = cex)
      }
    })
  }
}

###
### 2) palettes
###

#' Various Color Palettes
#'
#' Returns a vector of hex color values
#' for a specified color palette.
#'
#' @param type The color palette to return. Options
#'   include...
#'   \itemize{
#'     \item 'Colorblind' (a colorblind-friendly palette);
#'     \item 'Grayscale' (4-bit grayscale palette).
#'   }
#' @param index An optional vector of integers or color
#'   names to extract a subset of colors from the
#'   specified palette.
#' @param plot Logical; if \code{TRUE}, generates
#'   a plot showcasing the specified color palette.
#'
#' @return A character vector of hex color values
#'   for the specified palette.
#'
#' @examples
#' # List of possible inputs to argument
#' # 'type' for each palette
#' palettes()
#'
#' # Plot of colors in each palette
#' palettes("Colorblind", plot = TRUE)
#' palettes("Grayscale", plot = TRUE)
#'
#' # Example of taking subset of colors
#' palettes("Colorblind", 1:2)
#' @export

palettes <- function(type = "colorblind",
                     index = NULL, plot = FALSE) {
  types <- list(
    colorblind = c(
      "Colorblind", "colorblind",
      "Colourblind", "colourblind",
      "CB", "cb",
      "1"
    ),
    grayscale = c(
      "Grayscale", "grayscale",
      "Greyscale", "greyscale",
      "Gray", "gray",
      "Grey", "grey",
      "2"
    )
  )

  if (is.null(type)) {
    for (i in 1:length(types)) {
      message(paste0(names(types[i])))
      message(paste0(
        "\t", types[[i]], "\n"
      ))
    }
  }

  list_of_colors <- NULL

  if (type %in% types$colorblind) {
    list_of_colors <- c(
      orange = "#E69F00",
      `light blue` = "#56B4E9",
      red = "#D55E00",
      green = "#009E73",
      yellow = "#F0E442",
      blue = "#0072B2",
      pink = "#CC79A7"
    )
  }

  if (type %in% types$grayscale) {
    list_of_colors <- c(
      black = "#000000FF",
      gray01 = "#111111FF",
      gray02 = "#222222FF",
      gray03 = "#333333FF",
      gray04 = "#444444FF",
      gray05 = "#555555FF",
      gray06 = "#666666FF",
      gray07 = "#777777FF",
      gray08 = "#888888FF",
      gray09 = "#999999FF",
      gray10 = "#AAAAAAFF",
      gray11 = "#BBBBBBFF",
      gray12 = "#CCCCCCFF",
      gray13 = "#DDDDDDFF",
      gray14 = "#EEEEEEFF",
      white = "#FFFFFFFF"
    )
  }

  if (!is.null(list_of_colors)) {
    if (is.null(index)) {
      index <- 1:length(list_of_colors)
    }

    if (plot) {
      n <- length(list_of_colors)

      par(mar = c(5.1, 5.1, .5, .5))
      plot(1:n, 1:n,
        xlab = "", ylab = "",
        xaxt = "n", yaxt = "n",
        bty = "n",
        pch = 15, cex = 4,
        col = list_of_colors
      )
      axis(1, 1:n, tick = F, cex = 1.25)
      axis(2, 1:n, names(list_of_colors),
        tick = F, cex = 1.25, las = 1
      )
      par(mar = c(5.1, 4.1, 4.1, 2.1))
    }

    return(list_of_colors[index])
  } else {
    stop(
      paste0(
        "Specified palette not found - options are...\n",
        paste(
          sapply(1:length(types), function(i) {
            types[[i]][1]
          }),
          collapse = "\n"
        )
      )
    )
  }
}

###
### 3) hv_line
###

#' Draw Horizontal/Vertical Lines
#'
#' Draws horizontal or vertical lines on an
#' existing figure.
#'
#' @param h A vector with the y-axis positions
#'   for horizontal lines.
#' @param v A vector with the x-axis positions
#'   for vertical lines.
#' @param l The lower and upper coordinates to determine
#'   the length of the line (if \code{y} is not \code{NULL},
#'   \code{l} is taken as the x-axis coordinates; if
#'   \code{x} is not \code{NULL}, \code{l} is taken as
#'   the y-axis coordinates).
#' @param ... Additional arguments to be
#'   passed to the \code{\link[graphics]{segments}}
#'   function.
#'
#' @examples
#' # Create a blank plot
#' blank_plot()
#'
#' # Draw horizontal line
#' hv_line(h = .5, lty = 2)
#' # Draw vertical line
#' hv_line(v = .5, lwd = 2)
#'
#' # Control width of horizontal line
#' hv_line(h = .25, l = c(.25, .75), col = "blue")
#' # Control height of vertical line
#' hv_line(v = .25, l = c(.25, .75), col = "orange")
#' @export

hv_line <- function(h = NULL, v = NULL, l = NULL, ...) {
  if (!is.null(v)) {
    n <- length(v)

    if (is.null(l)) {
      l <- par("usr")[3:4]
    }

    segments(
      v, rep(l[1], n),
      v, rep(l[2], n), ...
    )
  }

  if (!is.null(h)) {
    n <- length(h)

    if (is.null(l)) {
      l <- par("usr")[1:2]
    }

    segments(
      rep(l[1], n), h,
      rep(l[2], n), h, ...
    )
  }
}

###
### 4) fill_plot
###

#' Add Filled Vertical/Horizontal Bar
#'
#' Adds a vertical or horizontal filled bar
#' at the specified x or y-axis coordinates.
#'
#' @param x A vector of two x-axis coordinates,
#'   or a N x 2 matrix, giving the left and right
#'   boundaries of the vertical bars to draw.
#' @param y A vector of two y-axis coordinates,
#'   or a N x 2 matrix, giving the bottom and
#'   top boundaries of the horizontal bars to draw.
#' @param l The limits for either 1) the height
#'   of the bar(s) (if \code{x} is not \code{NULL}),
#'   or 2) the width of the bar(s) (if \code{y} is
#'   not \code{NULL}).
#' @param border The color of the border for the
#'   filled bar(s). If \code{NA}, no border is drawn.
#' @param ... Additional parameters to the function
#'   \code{\link[graphics]{polygon}}.
#'
#' @examples
#' # Create blank plot
#' xl <- c(0, 4)
#' yl <- c(0, 1)
#' blank_plot(xl, yl)
#' # Draw vertical grey bar
#' fill_plot(x = c(1, 2), l = yl, col = "grey90")
#' # Also set height and border of bar
#' fill_plot(x = c(3, 4), l = c(.25, .75), col = "grey80", border = "black")
#' # Draw horizontal bar
#' fill_plot(y = c(.9, 1), col = "black")
#'
#' # Multiple bars can be draw at once
#' xl <- c(0, 4)
#' yl <- c(0, 1)
#' blank_plot(xl, yl)
#' # Multiple vertical bars
#' fill_plot(x = rbind(c(1, 2), c(3, 4)), l = yl, col = "grey90")
#' # Multiple horizontal bars
#' fill_plot(y = rbind(c(0, .05), c(.95, 1)), l = xl, col = "black")
#' @export

fill_plot <- function(x = NULL, y = NULL, l = NULL,
                      border = NA, ...) {
  if (!is.null(x)) {
    if (is.vector(x) &
      length(x) == 2) {
      xv <- rbind(x[c(1, 1, 2, 2)])
    }

    if (is.matrix(x)) {
      if (ncol(x) == 2) {
        xv <- cbind(x[, 1], x[, 1], x[, 2], x[, 2])
      }
    }

    if (is.null(l)) {
      l <- par("usr")[3:4]
    }
    yv <- rbind(l[c(1, 2, 2, 1)])
    if (nrow(xv) > nrow(yv)) {
      yv <- matrix(yv, nrow(xv), 4, byrow = TRUE)
    }
  }

  if (!is.null(y)) {
    if (is.vector(y) &
      length(y) == 2) {
      yv <- rbind(y[c(1, 1, 2, 2)])
    }

    if (is.matrix(y)) {
      if (ncol(y) == 2) {
        yv <- cbind(y[, 1], y[, 2], y[, 2], y[, 1])
      }
    }

    if (is.null(l)) {
      l <- par("usr")[1:2]
    }
    xv <- rbind(l[c(1, 1, 2, 2)])
    if (nrow(yv) > nrow(xv)) {
      xv <- matrix(xv, nrow(yv), 4, byrow = TRUE)
    }
  }

  if (!is.null(x) | !is.null(y)) {
    for (i in 1:nrow(xv)) {
      polygon(xv[i, ], yv[i, ], border = border, ...)
    }
  }
}

###
### 5) add_axes
###

#' Add Axes to a Plot
#'
#' Wrapper for a call to either \code{\link[graphics]{axis}}
#' or \code{\link[graphics]{text}}, used to add axis labels
#' to an existing plot.
#'
#' @param at The positions (x or y-axis) at which to
#'   add axis labels.
#' @param labels Optional vector matching in length to
#'   \code{at} with user-defined labels.
#' @param side The side of the plot at which to add axis
#'   labels, either...
#'   \itemize{
#'     \item 1 = bottom;
#'     \item 2 = left;
#'     \item 3 = top;
#'     \item 4 = right.
#'   }
#' @param tick Logical; if \code{TRUE} draws tick marks
#'   at the specified positions.
#' @param line The number of lines into the margin at which
#'   the axis line will be drawn. If \code{degrees} does not
#'   equal \code{NULL}, must be specified relative
#'   current plotting region (use \code{\link[graphics:text]{par()$usr}}
#'   to get x and y-axis coordinates for plot region).
#' @param cex Size of the text.
#' @param degrees Number of degrees to rotate text
#'   (note results in a call to \code{\link[graphics:text]{text()}}
#'   rather than \code{\link[graphics:axis]{axis()}}).
#' @param xpd A logical value or \code{NA}. If \code{FALSE},
#'   all plotting is clipped to the plot region. If \code{TRUE},
#'   all plotting is clipped to the figure region, and if
#'   \code{NA}, all plotting is clipped to the device region.
#' @param adj ...
#' @param ... Additional parameters to be passed to
#'   either \code{\link[graphics]{axis}} or
#'   \code{\link[graphics]{text}} (if a value is
#'   provided for \code{degrees}).
#'
#' @examples
#' # Create blank plot
#' blank_plot()
#' # Draw boundaries
#' hv_line(x = 0)
#' hv_line(y = 0)
#'
#' # Add axes
#' add_axes(c(0, .5, 1), c("Start", "Middle", "End"))
#' add_axes(c(0, .5, 1), c("Bottom", "Middle", "Top"), side = 2)
#' add_axes(.5, "Title", side = 3, cex = 2)
#'
#' # Create blank plot with custom margins
#' par(mar = rep(4, 4))
#' blank_plot()
#' # Draw boundaries
#' hv_line(x = 0:1)
#' hv_line(y = 0:1)
#'
#' # Add rotated axes
#' add_axes(c(0, .5, 1), c("Start", "Middle", "End"),
#'   degrees = 45
#' )
#' add_axes(c(0, .5, 1), c("Start", "Middle", "End"),
#'   degrees = 45, side = 3
#' )
#' add_axes(c(0, .5, 1), c("Bottom", "Middle", "Top"),
#'   degrees = 45, side = 2
#' )
#' add_axes(c(0, .5, 1), c("Bottom", "Middle", "Top"),
#'   degrees = 45, side = 4
#' )
#' @export

add_axes <- function(at, labels = NULL,
                     side = 1, tick = F,
                     line = NULL, cex = 1.25,
                     degrees = NULL, xpd = NA,
                     adj = NULL, ...) {

  #< Check if rotated axes should be drawn
  if (is.null(degrees)) {

    # Default line position to draw axes at
    if (is.null(line)) line <- -.5

    axis(at, labels,
      side = side, tick = tick,
      line = line, cex.axis = cex, xpd = xpd,
      ...
    )

    # > Close conditional on no rotated axes
  } else {

    #<| By default labels are values to draw axes at
    if (is.null(labels)) {
      labels <- as.character(at)

      #|> Close conditional
    }

    #<| Whether rotated axes should be right or left-aligned
    if (is.null(adj)) {
      if (side %in% c(1, 3)) adj <- 1 # Right-aligned
      if (side %in% c(2, 4)) adj <- 0 # Left-aligned

      #|> Close conditional
    }

    # Get plot dimensions
    x_limits <- par()$usr[1:2]
    y_limits <- par()$usr[3:4]

    # Determine size of text
    txt_height <- strheight(labels[1], cex = cex)

    #<| Rotated axes at the bottom
    if (side == 1) {

      #<|< Default line position to draw axes
      if (is.null(line)) {
        line <- y_limits[1] - txt_height / 2

        # >|> Close conditional
      }

      text(
        x = at, y = rep(line, length(at)),
        labels = labels, srt = degrees, xpd = xpd,
        cex = cex, adj = adj, ...
      )

      #|> Close conditional
    }

    #<| Rotated axes at the top
    if (side == 3) {

      #<|< Default line position to draw axes
      if (is.null(line)) {
        line <- y_limits[2] + txt_height / 2

        # >|> Close conditional
      }

      text(
        x = at, y = rep(line, length(at)),
        labels = labels, srt = degrees, xpd = xpd,
        cex = cex, adj = adj, ...
      )

      #|> Close conditional
    }

    #<| Rotated axes at the left
    if (side == 2) {

      #<|< Default line position to draw axes
      if (is.null(line)) {
        line <- x_limits[1] - txt_height / 2

        # >|> Close conditional
      }

      text(
        x = rep(line, length(at)), y = at,
        labels = labels, srt = degrees, xpd = xpd,
        cex = cex, adj = adj, ...
      )

      #|> Close conditional
    }

    #<| Rotated axes at the right
    if (side == 4) {

      #<|< Default line position to draw axes
      if (is.null(line)) {
        line <- x_limits[2] + txt_height / 2

        # >|> Close conditional
      }

      text(
        x = rep(line, length(at)), y = at,
        labels = labels, srt = degrees, xpd = xpd,
        cex = cex, adj = adj, ...
      )

      #|> Close conditional
    }

    # > Close conditional on rotated axes
  }
}


###
### 6) col_to_hex
###

#' Convert Colors to Hex Codes
#'
#' Convert a color name to a hex color code.
#'
#' @param col A character string corresponding to
#'   a supported color name (e.g., 'blue', 'darkred', etc.).
#'   See \code{\link[grDevices]{colors}}.
#' @param alpha Degree of transparency from
#'   0 (transparent) to 1 (opaque).
#'
#' @examples
#' # Create scatter plot for bivariate normal
#' blank_plot(c(-4, 4), c(-4, 4))
#' # Draw semi-opaque blue points
#' points(rnorm(1000), rnorm(1000),
#'   pch = 19,
#'   col = col_to_hex("blue", alpha = .3)
#' )
#' @export

col_to_hex <- function(col, alpha = 1) {
  mat <- col2rgb(col)

  vec <- c(
    mat[1, 1] / 256,
    mat[2, 1] / 256,
    mat[3, 1] / 256
  )

  out <- rgb(
    red = vec[1],
    green = vec[2],
    blue = vec[3],
    alpha = alpha
  )
  return(out)
}

###
### 7) error_bars
###

#' Add Error Bars to a Plot
#'
#' Adds error bars to an existing plot.
#'
#' @param pos Either a single value or a numeric
#'   vector of N values indicating the position(s)
#'   at which an error bar should be drawn.
#' @param limits Either a vector of 2 values or a
#'   2 x N matrix giving the lower and upper limits,
#'   respectively, of the error bars.
#' @param lb A vector of N values with the lower limits
#'   for the error bars.
#' @param ub A vector of N values with the upper limits
#'   for the error bars.
#' @param arrow Logical; if \code{TRUE},
#'   \code{\link[graphics]{arrows}} are drawn at
#'   each position, otherwise a call is made to
#'   \code{\link[graphics]{polygon}} to create a
#'   filled-in segment to represent connected
#'   error bars.
#' @param flip Logical; if \code{TRUE}, bars are
#'   drawn horizontally instead of vertically.
#'   In this case \code{pos} denotes the
#'   position(s) on the y-axis. Otherwise,
#'   \code{pos} denotes the position(s) on the
#'   x-axis.
#' @param length The length of the arrowhead for the
#'   call to \code{\link[graphics]{arrows}}.
#' @param code Integer controlling whether to draw an
#'   arrowhead at the start (1), end (2), or at both ends (3)
#'   of the line for the call to \code{\link[graphics]{arrows}}.
#' @param angle The angle of the lines creating the arrowhead
#'   for the call to \code{\link[graphics]{arrows}}. Using
#'   90 degrees results in a flat bar per standard error bars,
#' @param ... Additional plotting parameters for
#'   the \code{\link[graphics]{arrows}} function,
#'   or if \code{arrow} is \code{FALSE},
#'   \code{\link[graphics]{polygon}}.
#'
#' @examples
#' # Simulate 5 variables with increasing means
#' dtf <- lapply(lin(-.5, .5, 5), function(x) rnorm(100, mean = x))
#' names(dtf) <- paste0("V", 1:5)
#' dtf <- data.frame(dtf)
#' # Extract sample means and standard errors
#' sm <- data.frame(
#'   M = colMeans(dtf),
#'   SE = apply(dtf, 2, sem)
#' )
#' # Compute 95% confidence intervals around
#' # sample means
#' sm$LB <- sm$M + qnorm(.025) * sm$SE
#' sm$UB <- sm$M + qnorm(.975) * sm$SE
#'
#' # Plot mean for each variable
#' plot(1:5, sm$M,
#'   pch = 19,
#'   xlab = "Variable", ylab = "Mean", ylim = c(-1, 1)
#' )
#' # Add error bars (as arrows) for 95% confidence intervals
#' error_bars(1:5, rbind(sm$LB, sm$UB))
#' # Add error bars (as connected filled segment)
#' error_bars(1:5, rbind(sm$LB, sm$UB),
#'  arrow = FALSE, col = rgb(.5, .5, .5, .2), border = NA
#' )
#'
#' # Histogram for draws from standard normal
#' set.seed(300)
#' x <- rnorm(1000)
#' hist(x,
#'   col = "grey", border = "white", main = "",
#'   xlab = "z-scores", freq = FALSE, xlim = c(-4, 4)
#' )
#' # Add horizontal bars showing difference between
#' # 95% and 68% coverage interval
#' error_bars(c(.1, .3),
#'   lb = qnorm(c(.025, .16)), ub = qnorm(c(.975, .84)),
#'   lwd = 2, flip = TRUE
#' )
#' # Label bars
#' text(c(-2.1, -1.1), c(.1, .3),
#'   c("95%", "68%"),
#'   pos = 2
#' )
#' @export

error_bars <- function(pos, limits = NULL,
                       lb = NULL, ub = NULL,
                       arrow = TRUE, flip = FALSE,
                       length = .05, code = 3,
                       angle = 90, ...) {

  # If a vector/matrix of lower and upper boundaries is
  # not provided
  if (is.null(limits)) {

    # Check if lower and upper boundaries were provided
    # as separate vectors
    if (!is.null(lb) & !is.null(ub)) {

      # Convert to either matrix/vector
      if (length(lb) > 1) {
        limits <- rbind(lb, ub)
      } else {
        limits <- c(lb, ub)
      }
    } else {

      # Return an error
      stop(paste0(
        "Must provide the argument 'limits' or the arguments ",
        "'lb' and 'ub' giving lower and upper limits for error bars"
      ), call. = FALSE)
    }
  }

  if (flip) {
    # If 'pos' is for y-axis positions

    if (is.matrix(limits)) {
      # If a 2 x N matrix for the lower and upper boundaries
      # is given

      if (arrow) {
        # If arrows should be drawn at each position

        arrows(limits[1, ], pos,
          limits[2, ], pos,
          length = length, code = code,
          angle = angle, ...
        )
      } else {

        # If positions are points on a unified polygon
        polygon(
          c(limits[1, ], rev(limits[2, ])),
          c(pos, rev(pos)),
          ...
        )
      }
    } else {
      # If a single set of limits was provided

      arrows(limits[1], pos,
        limits[2], pos,
        length = length, code = code,
        angle = angle, ...
      )
    }
  } else {
    # If 'pos' if for x-axis positions

    if (is.matrix(limits)) {
      # If a 2 x K matrix for the lower and upper boundaries
      # is given

      if (arrow) {
        # If arrows should be drawn at each position

        arrows(pos, limits[1, ],
          pos, limits[2, ],
          length = length, code = code,
          angle = angle, ...
        )
      } else {
        # If positions are points on a unified polygon

        polygon(
          c(pos, rev(pos)),
          c(limits[1, ], rev(limits[2, ])),
          ...
        )
      }
    } else {
      # If a single set of limits was provided

      arrows(pos, limits[1],
        pos, limits[2],
        length = length, code = code,
        angle = angle, ...
      )
    }
  }
}

###
### 8)
###

#' Applies a Function to Add Elements to a Plot
#'
#' Given a data frame of values to plot,
#' applies a plotting function (optionally
#' based on a grouping factor) to add
#' elements to an existing plot.
#'
#' @param dtf A data frame with the data to plot.
#' @param entries List of logical vectors, each
#'   matching in length to the number of rows in
#'   \code{dtf}.
#' @param f A plotting function. If \code{NULL}
#'   defaults to a function that draws
#'   connected lines with points based on
#'   the groupings defined by \code{entries}.
#' @param ... Additional arguments to be passed
#'   to the plotting function \code{f}.
#'
#' @examples
#' # Use longitudinal data set on chick weights
#' data( "ChickWeight" )
#' # Average over different chicks by
#' # time point and diet
#' dtf <- aggregate(
#'   ChickWeight$weight,
#'   list( Time = ChickWeight$Time, Diet = ChickWeight$Diet ),
#'   mean
#' )
#'
#' # Specify grouping factor
#' diet <- list_of_matches( dtf, 1:4, 'Diet' )
#'
#' # Specify aesthetics for points and lines
#' dtf$col.p <- assign_by_match( palettes('colorblind')[1:4], diet )
#' dtf$col.l <- dtf$col.p
#'
#' # Create base figure
#' xl = c(-1, 23); yl = c(20, 300)
#' par(mar = c(3, 3, .5, 4 )); blank_plot(xl, yl)
#'
#' # Axis lines
#' xl[2] <- 21; hv_line(h = yl[1], l = xl, lwd = 2)
#' hv_line(v = xl[1], l = yl, lwd = 2)
#' # Grid lines
#' hv_line(h = seq( 40, 280, 40), l = xl, lwd = 1, col = 'grey80' )
#'
#' # Axis labels and ticks
#' add_axes( c( 0, 7, 14, 21 ), side = 1, line = -1 )
#' mtext( 'Average weight', side = 2, line = 1.5, cex = 1.2 )
#' add_axes( seq( 40, 280, 40 ), side = 2, line = -1 )
#' mtext( 'Time', side = 1, line = 1.5, cex = 1.2 )
#'
#' # Legend
#' day_21 <- dtf$Time == 21
#' text( rep( 21.5, 4 ), dtf$x[day_21], 'Protein diet ' %p% 1:4,
#'       pos = 4, col = dtf$col.p[day_21], xpd = NA )
#'
#' # Plot separate lines per diet
#' apply_f_to_plot( dtf, entries = diet, vrb = c( 'Time', 'x' ) )
#'
#' # Demonstration of how to set options for default function
#' obs <- data.frame(
#'   x = c( 1:3, 1:3, 1:3 ),
#'   y = rep( 1:3, each = 3 ),
#'   group = rep( 1:3, each = 3 ),
#'   # Set line colors
#'   col.l = rep( palettes( index = 1:3 ), each = 3 ),
#'   # Set line widths
#'   lwd = rep( 1:3, each = 3 ),
#'   # Set line type
#'   lty = rep( 1:3, each = 3 ),
#'   # Set point type
#'   pch = rep( c(21,22,24), 3 ),
#'   # Set point color
#'   col.p = 'black',
#'   # Set background point color
#'   bg = 'grey',
#'   # Point size
#'   cex = rep( c( 1, 2, 3 ), 3 )
#' )
#'
#' blank_plot( c(.5,3.5), c(.5,3.5) )
#' apply_f_to_plot( obs, list_of_matches( obs$group, 1:3 ) )
#'
#' @export

apply_f_to_plot <- function(dtf,
                            entries = NULL,
                            f = NULL,
                            ...) {

  if ( is.null( dtf ) ) {

    func_template <- '
    f <- function(dtf,
                  vrb = NULL,
                  col.l = "black",
                  lwd = 1,
                  lty = 1,
                  bg = "white",
                  col.p = "black",
                  pch = 19,
                  cex = 1.2,
                  which_type = "both") {

      if ( is.null( vrb ) ) {
        vrb <- c( "x", "y" )
      }

      if ("col.l" %in% colnames(dtf)) {
        col.l <- dtf[["col.l"]]
      }
      if ("lwd" %in% colnames(dtf)) {
        lwd <- dtf[["lwd"]]
      }
      if ("lty" %in% colnames(dtf)) {
        lty <- dtf[["lty"]]
      }
      if ("bg" %in% colnames(dtf)) {
        bg <- dtf[["bg"]]
      }
      if ("col.p" %in% colnames(dtf)) {
        col.p <- dtf[["col.p"]]
      }
      if ("pch" %in% colnames(dtf)) {
        pch <- dtf[["pch"]]
      }
      if ("cex" %in% colnames(dtf)) {
        cex <- dtf[["cex"]]
      }

      if (which_type %in% c("lines", "l", "both", "b", "0", "1")) {
        lines(dtf[[ vrb[1] ]], dtf[[ vrb[2] ]],
              col = col.l, lwd = lwd, lty = lty
        )
      }
      if (which_type %in% c("points", "p", "both", "b", "0", "2")) {
        points(dtf[[ vrb[1] ]], dtf[[ vrb[2] ]],
               col = col.p, bg = bg, pch = pch, cex = cex
        )
      }
    }'
    cat( func_template )
    return( invisible(NULL) )
  }

  if (is.null(f)) {
    f <- function(dtf,
                  vrb = NULL,
                  col.l = "black",
                  lwd = 1,
                  lty = 1,
                  bg = "white",
                  col.p = "black",
                  pch = 19,
                  cex = 1.2,
                  which_type = "both") {

      if ( is.null( vrb ) ) {
        vrb <- c( 'x', 'y' )
      }

      if ("col.l" %in% colnames(dtf)) {
        col.l <- dtf[["col.l"]]
      }
      if ("lwd" %in% colnames(dtf)) {
        lwd <- dtf[["lwd"]]
      }
      if ("lty" %in% colnames(dtf)) {
        lty <- dtf[["lty"]]
      }
      if ("bg" %in% colnames(dtf)) {
        bg <- dtf[["bg"]]
      }
      if ("col.p" %in% colnames(dtf)) {
        col.p <- dtf[["col.p"]]
      }
      if ("pch" %in% colnames(dtf)) {
        pch <- dtf[["pch"]]
      }
      if ("cex" %in% colnames(dtf)) {
        cex <- dtf[["cex"]]
      }

      if (which_type %in% c("lines", "l", "both", "b", "0", "1")) {
        lines(dtf[[ vrb[1] ]], dtf[[ vrb[2] ]],
          col = col.l, lwd = lwd, lty = lty
        )
      }
      if (which_type %in% c("points", "p", "both", "b", "0", "2")) {
        points(dtf[[ vrb[1] ]], dtf[[ vrb[2] ]],
          col = col.p, bg = bg, pch = pch, cex = cex
        )
      }
    }
  }

  if (is.null(entries)) {
    f(dtf, ...)
  } else {
    n <- length(entries)

    for (i in 1:n) {
      f(dtf[entries[[i]], ], ...)
    }
  }
}
