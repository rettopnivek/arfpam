# Legacy functions
# Written by Kevin Potter
# email: kevin.w.potter@gmail.com
# Please email me directly if you
# have any questions or comments
# Last updated 2026-01-06

# Table of contents
# 1) blank_plot
# 2) plot_correlation_heatmap
#     3.2.1) Compute correlations and significance
#     3.2.2) Setup for figure
#       3.2.2.1) draw_box
#     3.2.3) Create figure
#     3.2.4) Legends
# 3) hv_line
# 4) add_axes
# 5) error_bars
# 6) fill_plot
# 7) save_png_figure

#### 1) blank_plot ####
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
#'
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

#### 2) plot_correlation_heatmap ####
#' Plot a Heatmap for a Correlation Matrix
#'
#' Generates a heatmap of the upper triangle of
#' a correlation matrix.
#'
#' @param x A data frame (all variables will be used
#'   when generating the correlation matrix).
#' @param ttl An optional title for the figure.
#' @param labels The labels for the rows/columns. Users
#'   can pass a list with two character vectors of matching
#'   length to provide separate labels for rows and columns.
#' @param lyt An optional matrix specifying the layout
#'   of the main panel (1) versus the side panel
#'   (2) with the color gradient.
#' @param gradient The final end colors for the negative
#'   and positive correlations, respectively.
#' @param txtSz The size of the text in the figure (a second
#'   value can be provided to adjust variable labels separately).
#' @param mc_adjust The method to use when correcting for
#'   multiple comparisons (see \code{\link[stats]{p.adjust}}).
#' @param cut_off Cut-off for statistical significance.
#' @param new Logical; if \code{TRUE} generates a new
#'   plotting window via \code{\link[grDevices]{x11}}.
#' @param H The height in inches of the figure if a new
#'   plotting window is generated.
#' @param W The width in inches of the figure if a new
#'   plotting window is generated.
#' @param abbr_labels Logical; if \code{TRUE} abbreviates
#'   labels to 4 characters. A second value can be provided
#'   to abbreviate labels on the top separately.
#' @param status Logical; if \code{TRUE} displays the
#'   progress of the function for debugging purposes.
#'
#' @return A heatmap for the upper-triangle portion of
#' the correlation matrix.
#'
#' @examples
#' # Load data
#' data("mtcars")
#' x <- mtcars[, c(1,3,4,5,6,7)]
#' plot_correlation_heatmap( x, new = FALSE )
#'
#' # Simulate a correlation matrix
#'
#' # 5 x 5 matrix of random values
#' rand_mat <- matrix( rnorm(25), 5, 5 )
#' # Create covariance matrix by
#' # multiplying matrix by its transpose
#' cov_mat <- rand_mat %*% t( rand_mat )
#' corr_mat <- cov_mat/sqrt(diag(cov_mat)%*%t(diag(cov_mat)))
#'
#' # Simulate data
#' x <- MASS::mvrnorm( 100, rep( 0, nrow( corr_mat ) ), corr_mat )
#' colnames( x ) <- paste0( 'V', 1:ncol( x ) )
#' x <- data.frame(x)
#' plot_correlation_heatmap( x, new = FALSE )
#'
#' @export

plot_correlation_heatmap <- function( x,
                                      ttl = "Correlation matrix",
                                      labels = NULL,
                                      lyt = NULL,
                                      gradient = c( "#E69F00", "#56B4E9" ),
                                      txtSz = 1.25,
                                      mc_adjust = "BH",
                                      cut_off = 0.05,
                                      new = T,
                                      H = 20/3, W = 25/3,
                                      abbr_labels = TRUE,
                                      status = FALSE ) {

  if ( !is.data.frame( x ) ) {
    stop( 'x must be a data frame' )
  }

  # Default is same text size for labels and legends
  if ( length( txtSz ) == 1 ) {
    txtSz <- rep( txtSz, 2 )
  }

  # Default is to abbreviate top labels
  if ( length( abbr_labels ) == 1 ) {
    abbr_labels <- c(
      abbr_labels,
      TRUE
    )
  }

  #### 3.2.1) Compute correlations and significance ####
  if ( status ) message( '  [1]' )

  # Determine observed correlation matrix
  omega <- cor( as.matrix(x) )

  # Row names
  rn <- rownames( omega )
  # Column names
  cn <- colnames( omega )

  # Number of variables
  NV <- nrow( omega )

  # Number of correlations
  NC <- NV * ( NV - 1 )/2

  # Initialize matrix to store p-values
  p_mat <- matrix( NA, NV, NV )

  # Vector of p-values for adjustment for multiple comparisons
  p_val <- rep( NA, NC )

  # Compute p-value for each correlation
  inc <- 1
  # Loop over rows
  for ( j in 1:NV ) {

    # Loop over columns starting from diagonal position
    for (k in j:NV) {

      # Non-diagonal entries
      if ( j != k ) {

        # Compute p-value for correlation
        tst = cor.test( x[[ rn[j] ]], x[[ cn[k] ]] )

        # Save to vector for later adjustment for
        # multiple comparisons
        if (k > j) {
          p_val[inc] <- tst$p.value
          inc %+=% 1
        }

        # Close 'Non-diagonal entries'
      }

      # Close 'Loop over columns starting from diagonal position'
    }

    # Close 'Loop over rows'
  }

  # Adjust p-values for multiple comparisons
  p_val <- p.adjust(p_val, method = mc_adjust)

  # Save adjusted p-values to matrix

  # Compute p-value for each correlation
  inc <- 1
  # Loop over rows
  for ( j in 1:NV ) {

    # Loop over columns starting from diagonal position
    for (k in j:NV) {

      # Non-diagonal entries
      if ( j != k ) {

        # Save adjusted p-values
        if (k > j) {
          p_mat[j, k] <- p_val[inc]
          p_mat[k, j] <- p_mat[j, k]
          inc %+=% 1
        }

        # Close 'Non-diagonal entries'
      }

      # Close 'Loop over columns starting from diagonal position'
    }

    # Close 'Loop over rows'
  }

  diag(p_mat) <- 0

  #### 3.2.2) Setup for figure ####
  if ( status ) message( '  [2]' )

  pos = seq( NV, 0, -1)

  # Default layout for...
  # [1] Panel with correlation matrix
  # [2] Panel with legend
  if (is.null(lyt)) {
    lyt = cbind(1, 1, 1, 1, 2)
  }

  # Plotting boundaries
  xl = range(pos)
  yl = range(pos)
  mrg = c(7, 7, 2, 0.5)

  # Color coding for correlation magnitudes
  r_range = c( 0, .1, .3, .5, .7, .9, 1 )
  color_f = colorRampPalette(c("white", gradient[2]))
  color_pos = color_f(length(r_range))
  color_f = colorRampPalette(c("white", gradient[1]))
  color_neg = color_f(length(r_range))

  #### 3.2.2.1) draw_box ####
  # Draw Box at Specified Coordinates
  #
  # Function to draw a box at a specified
  # set of coordinates with a given fill
  # color and border. Can also draw a
  # slash instead (to indicate
  # non-significance) and can adjust
  # the size of the box to also denote
  # the magnitude of a correlation.
  #
  # @param 'ri' The row index.
  # @param 'ci' The column index.
  # @param 'clr' The fill color.
  # @param 'brd' The border color.
  # @param 'slash' Logical; if TRUE
  #   draws a slash instead of a
  #   box.

  draw_box = function(pos, ri, ci,
                      clr = NULL, brd = NULL,
                      slash = FALSE, r = 1 ) {

    xa = c(ci + 1, ci + 1, ci, ci)
    ya = c(ri, ri + 1, ri + 1, ri)

    xv <- rev( pos )[ xa ]
    yv <- pos[ ya ]

    mmv <- list(
      xa == min( xa ), xa == max( xa )
    )
    xv[ mmv[[1]] ] <- xv[ mmv[[1]] ] + (1-r)/2
    xv[ mmv[[2]] ] <- xv[ mmv[[2]] ] - (1-r)/2

    mmv <- list(
      ya == min( ya ), ya == max( ya )
    )
    yv[ mmv[[1]] ] <- yv[ mmv[[1]] ] - (1-r)/2
    yv[ mmv[[2]] ] <- yv[ mmv[[2]] ] + (1-r)/2

    if (is.null(clr))
      clr = "white"

    if (is.null(brd))
      brd = clr

    if (!slash) {
      polygon( xv, yv, col = clr, border = brd)
    }
    else {
      segments(
        xv[1], yv[1],
        xv[3], yv[2]
      )
    }
  }

  #### 3.2.3) Create figure ####
  if ( status ) message( '  [3]' )

  # If specified create new plotting window
  if (new) {
    x11( height = H, width = W )
  }

  # Create panels for heatmap and
  # for legend
  layout(lyt)

  # Create blank plot as basis for heatmap
  par(mar = mrg)
  blank_plot(xl, yl)

  # Loop over rows
  for (ri in 1:NV) {

    # Loop over columns
    for (ci in 1:NV) {

      # Extract correlation
      cur_R <- omega[ri, ci]

      # If correlation is positive
      if (cur_R > 0) {
        # Determine color for magnitude of correlation
        cur_clr <- color_pos[
          max( which( r_range <= cur_R ) )
        ]
      }

      # If correlation is negative
      if (cur_R < 0) {
        # Determine color for magnitude of correlation
        cur_clr <- color_neg[
          max( which( r_range <= abs( cur_R ) ) )
        ]
      }

      # White boxes
      if (cur_R == 0) {
        cur_clr = "white"
      }
      if (ri == ci) {
        cur_clr = "white"
      }
      if (ri >= ci) {
        cur_clr = "white"
      }
      cur_brd = NULL

      # Add box to figure
      if ( ci > ri ) {
        draw_box(pos, ri, ci,
                 brd = 'grey90' )
      }
      draw_box(pos, ri, ci, clr = cur_clr,
               brd = cur_brd, r = abs( omega[ri, ci] ) )

      # Close 'Loop over columns'
    }

    # Close 'Loop over rows'
  }

  # Loop over rows
  for (ri in 1:NV) {

    # Loop over columns
    for (ci in 1:NV) {

      cur_R <- round(omega[ri, ci], 2)
      cur_p <- p_mat[ri, ci]

      if (is.na(cur_p)) {
        cur_p <- 0
      }

      # If non-significant draw a slash
      if (cur_p > cut_off) {

        # If part of upper triangle
        if (ri < ci) {

          # If correlation is positive
          if (cur_R > 0) {
            # Determine color for magnitude of correlation
            cur_clr <-
              color_pos[round(r_range, 2) == round(cur_R, 2)]
          }
          # If correlation is negative
          if (cur_R < 0) {
            # Determine color for magnitude of correlation
            cur_clr <-
              color_neg[round(r_range, 2) == round( abs(cur_R), 2)]
          }

          # Add black border
          cur_brd = "black"
          draw_box(
            pos, ri, ci, clr = cur_clr,
            brd = cur_brd, slash = T
          )

          # Close 'If part of upper triangle'
        }

        # Close 'If non-significant draw a slash'
      }

      # Close 'Loop over columns'
    }

    # Close 'Loop over rows'
  }

  # Legend for indicator of non-significance

  draw_box(pos, NV, 1, clr = "white", brd = "black")
  draw_box(pos, NV, 1, clr = "white", brd = "black", slash = T)
  text(
    1.25, 0.5,
    paste("Non-significant at p >", cut_off),
    pos = 4, cex = txtSz[1]
  )

  # Default labels for variables
  if (is.null(labels)) {
    labels = colnames(omega)
  }

  # Check if a list was provided for separate row/columns
  if ( is.list(labels) ) {

    labels_row <- labels[[1]]
    labels_col <- labels[[2]]

  } else {

    # Same labels for rows and columns
    labels_row <- labels
    labels_col <- labels
  }

  if ( abbr_labels[1] ) {
    labels_row <- abbreviate( labels_row, minlength = 4 )
  }

  # Add labels to figures
  for (ri in 1:NV) {

    text(
      NV - (ri - 1), ri - 0.5,
      rev(labels_row)[ri], pos = 2,
      xpd = NA, cex = txtSz[2]
    )

    # Close 'Add labels to figures'
  }

  if ( abbr_labels[2] ) {
    labels_col <- abbreviate( labels_col, minlength = 4 )
  }

  axis( 3, 2:NV - .5, labels_col[-1], cex.axis = txtSz[2],
        line = -1.5, tick = FALSE )

  # Title for correlation heatmap
  mtext(ttl, side = 1, line = 1, cex = txtSz[1] )

  #### 3.2.4) Legends ####
  if ( status ) message( '  [4]' )

  # Range of correlation values
  lbl = c( -rev( r_range ), r_range[-1] )
  # Mapping to color gradients
  all_clr <- c( rev( color_neg ), color_pos[-1] )
  # Reverse order so positive values at top and
  # negative values at bottom
  lbl = rev(lbl)
  all_clr <- rev( all_clr )

  # Row positions
  pos = rev(0:(length(lbl)))

  # Create panel for legend

  # Plotting boundaries
  xl = c(0, 2)
  yl = range(pos)

  # Match margin to main panel
  par(mar = c(mrg[1], 0, mrg[3], mrg[4]))

  # Create blank plot
  blank_plot(xl, yl)

  # Loop over rows
  for (ri in 1:length(lbl)) {

    # Draw box with color mapped to given
    # correlation value
    cur_clr <- all_clr[ri]
    draw_box(pos, ri, 1, clr = cur_clr)

    # Close 'Loop over rows'
  }

  # Format values for nice spacing
  string = as.character(lbl)
  string[lbl > 0] = paste(" ", string[lbl > 0])
  string[lbl == 0] = " 0.0"
  string[lbl == 1] = " 1.0"
  string[lbl == -1] = "-1.0"

  # Add correlations values next to
  # corresponding colors
  text(rep(txtSz[1], length(lbl)), pos[-1] + 0.5, string)

  # Add legend title
  mtext("R", side = 1, line = 1, cex = txtSz[1])

}

#### 3) hv_line ####
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

#### 4) add_axes ####
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

#### 5) error_bars ####
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
#'   90 degrees results in a flat bar per standard error bars.
#' @param border The color to draw the border for
#'   \code{\link[graphics]{polygon}}.
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
                       angle = 90, border = NULL, ...) {

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
          border = border,
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
          border = border,
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

#### 6) fill_plot ####
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
#' plot_blank(xl, yl)
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
#' plot_blank(xl, yl)
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

#### 7) save_png_figure ####
#' Create and save PNG file
#'
#' Function to create and save a PNG file
#' given a plotting function to run.
#'
#' @param plotting_fun A function, with the
#'   creation of a figure as a side effect.
#' @param file_name A character string, the
#'   file name for the PNG file - must end
#'   in '.png'.
#' @param figure_dim A numeric vector of two
#'   values, the width and height of the
#'   figure in inches.
#' @param res A integer value, the resolution
#'   for the PNG file.
#' @param ... Additional arguments to pass to
#'   \code{plotting_fun}.
#'
#' @returns Output from \code{plotting_fun}, if any.
#'
#' @export

save_png_figure <- function(
    plotting_fun,
    file_name,
    figure_dim = c( 5, 5 ),
    res = 300,
    ... ) {

  png(
    filename = file_name,
    width = figure_dim[1],
    height = figure_dim[2],
    units = 'in',
    res = res
  )

  obj_output <- plotting_fun(
    ...
  )

  dev.off()

  # Pass through output
  if ( !is.null( obj_output ) ) {

    return( obj_output )

    # Close 'Pass through output'
  }

}

