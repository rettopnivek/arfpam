# Plotting
# Written by Kevin Potter
# email: kevin.w.potter@gmail.com
# Please email me directly if you
# have any questions or comments
# Last updated 2025-10-09

# Table of contents
# 1) Utility functions for plotting
#   1.1) col_to_hex
#   1.2) palettes
#   1.3) specify_positions
#   1.8) draw_by_group
# 2) Functions to draw onto existing plot
#   2.1) draw_axes
#   2.2) draw_borders_and_labels
#   2.3) draw_boxplots
#   2.4) draw_dots
#   2.5) draw_error_bars
#   2.6) draw_hv
#   2.7) draw_legend
#   2.8) draw_lines
# 3) Functions to plot specific types of plots
#   3.1) plot_blank
#   3.2) plot_correlations
#   3.3) plot_forest
#   3.4) plot_histogram
#   3.5) plot_scatter

#### 1) Utility functions for plotting ####

#### 1.1) col_to_hex ####
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
#' plot_blank(c(-4, 4), c(-4, 4))
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


#### 1.2) palettes ####
#' Various Color Palettes
#'
#' Returns a vector of hex color values
#' for a specified color palette.
#'
#' @param index An optional vector of integers or color
#'   names to extract a subset of colors from the
#'   specified palette.
#' @param type The color palette to return. Options
#'   include...
#'   \itemize{
#'     \item 'Colorblind' (a colorblind-friendly palette);
#'     \item 'Grayscale' (4-bit grayscale palette).
#'   }
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

palettes <- function(index = NULL,
                     type = "colorblind",
                     plot = FALSE) {
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
      pink = "#CC79A7",
      blue = "#0072B2",
      yellow = "#F0E442"
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

#### 1.3) specify_positions ####
#' Specify Positions over Grouping Factors
#'
#' Function to create positions for a figure based
#' on the combination of grouping factors in a
#' data frame.
#'
#' @param dtf A data frame with separate columns for
#'   each grouping variable.
#' @param spacing A numeric vector of two values, the
#'   gap to add between groups and at the beginning and
#'   end for plotting limits (given as a proportion).
#' @param as_list Logical; if \code{TRUE} returns a list
#'   with both the positions and the suggested lower and
#'   upper x-axis limits; otherwise, returns the positions
#'   as a vector.
#'
#' @returns A numeric vector of values. If \code{as_list} is
#' \code{TRUE}, a list with two vectors, one for the positions
#' and one with the lower and upper limits for the plotting
#' boundaries.
#'
#' @examples
#' # Example data set
#' data("mtcars")
#' dtf <- stats_by_group( mtcars, 'mpg', c( 'vs', 'am' ) )
#' specify_positions( dtf[, 1:2] )
#'
#' @export

specify_positions <- function( dtf,
                               spacing = c( .25, .25 ),
                               as_list = FALSE ) {

  V <- ncol( dtf )
  dtf_index <- dtf

  for ( v in 1:V ) {
    dtf_index[[ v ]] <- as.numeric(
      as.factor( dtf_index[[ v ]] )
    )
  }

  int_max <- apply(
    dtf_index, 2, max
  )

  num_spacing <-
    max( dtf_index[[V]] ) * spacing[1]
  num_endpoints <-
    max( dtf_index[[V]] ) * spacing[2]

  num_positions <- rep( NA, nrow(dtf_index) )
  num_positions[1] <- 1

  # Loop over remaining rows
  for ( r in 2:nrow(dtf_index) ) {

    num_positions[r] <- num_positions[r-1] + 1

    int_indices_r <- unlist( dtf_index[r, -V] )
    int_indices_rm1 <- unlist( dtf_index[r-1, -V] )

    # If break in nesting structure
    if ( any( int_indices_r != int_indices_rm1) ) {

      num_positions[r] <- num_positions[r] + num_spacing

      # Close 'If break in nesting structure'
    }

    # Close 'Loop over remaining rows'
  }

  lst_output <- list(
    positions = num_positions,
    limits = c(
      min(num_positions) - num_endpoints,
      max(num_positions) + num_endpoints
    )
  )

  if ( as_list ) return( lst_output ) else return( lst_output$positions )
}

#### 1.8) draw_by_group ####
#' Add Elements to Existing Figure by Groups
#'
#' Function that will add elements to an existing
#' plot via different \code{draw} functions per
#' each level of a grouping factor.
#'
#' @param dtf A data frame.
#' @param variable A character string, the column with
#'   the grouping levels.
#' @param groups The grouping levels to plot over.
#' @param draw_fun A function, either [draw_dots], [draw_lines],
#'   or [draw_boxplots].
#' @param ... Additional arguments for the relevant function.
#'   For a given argument, a list with different values per
#'   group can be provided.
#'
#' @returns Adds elements to an existing plot per each group level.
#'
#' @examples
#' # Compute mean, SE, and 95% CI limits for data set
#' dtf <- aggregate(
#'   mtcars$mpg, list( mtcars$cyl ), function(x) c( mean(x), sem(x) )
#' )
#' dtf$X <- 1:3
#' dtf$M <- dtf$x[,1];
#' dtf$LB <- dtf$x[,1] - dtf$x[,2] * 1.96
#' dtf$UB <- dtf$x[,1] + dtf$x[,2] * 1.96
#'
#' # Create blank plot
#' xl <- c( .5, 3.5 )
#' yl <- c( 5, 35 )
#' plot_blank( xl, yl )
#' draw_hv( h = yl, l = xl )
#' draw_hv( v = xl, l = yl )
#'
#' # Add means and error bars by car cylinder type
#' draw_by_group(
#'   dtf, variable = 'Group.1', groups = c( 4, 6, 8 ),
#'   draw_fun = draw_dots,
#'   columns = c( 'X', 'M', 'LB', 'UB' ),
#'   bg = list( 'black', 'blue', 'red' ),
#'   pch = list( 21, 22, 24 )
#' )
#'
#' # Add axes and labels
#' draw_axes( 1:3, dtf$Group.1 )
#' mtext( 'Cylinders', side = 1, line = 2, cex = 1.25 )
#' draw_axes( c( 10, 20, 30 ), side = 2 )
#' mtext( 'MPG', side = 2, line = 2, cex = 1.25 )
#'
#' @export

draw_by_group <- function( dtf, variable, groups, draw_fun, ... ) {

  K <- length( groups )

  arg_list <- list( ... )
  arg_names <- names( list( ... ) )
  J <- length( arg_list )

  # Loop over group levels
  for ( k in 1:K ) {

    index <- dtf[[ variable ]] %in% groups[k]

    list_of_arg <- list(
      x = dtf[ index, ]
    )

    for ( j in 1:J ) {

      cur_arg <- arg_list[[j]]

      if ( is.list( cur_arg ) ) {
        cur_arg <- cur_arg[[k]]
      }

      list_of_arg[[j+1]] <- cur_arg

    }
    names( list_of_arg ) <- c( "x", arg_names )

    do.call( draw_fun, list_of_arg )

    # Close 'Loop over group levels'
  }

}

#### 2) Functions to draw onto existing plot ####

#### 2.1) draw_axes ####
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
#' plot_blank()
#' # Draw boundaries
#' draw_hv(x = 0)
#' draw_hv(y = 0)
#'
#' # Add axes
#' draw_axes(c(0, .5, 1), c("Start", "Middle", "End"))
#' draw_axes(c(0, .5, 1), c("Bottom", "Middle", "Top"), side = 2)
#' draw_axes(.5, "Title", side = 3, cex = 2)
#'
#' # Create blank plot with custom margins
#' par(mar = rep(4, 4))
#' plot_blank()
#' # Draw boundaries
#' draw_hv(x = 0:1)
#' draw_hv(y = 0:1)
#'
#' # Add rotated axes
#' draw_axes(c(0, .5, 1), c("Start", "Middle", "End"),
#'   degrees = 45
#' )
#' draw_axes(c(0, .5, 1), c("Start", "Middle", "End"),
#'   degrees = 45, side = 3
#' )
#' draw_axes(c(0, .5, 1), c("Bottom", "Middle", "Top"),
#'   degrees = 45, side = 2
#' )
#' draw_axes(c(0, .5, 1), c("Bottom", "Middle", "Top"),
#'   degrees = 45, side = 4
#' )
#' @export

draw_axes <- function( at,
                       labels = NULL,
                       side = 1,
                       tick = F,
                       line = NULL,
                       cex = 1.25,
                       degrees = NULL,
                       xpd = NA,
                       adj = NULL,
                       ... ) {

  # Check if rotated axes should be drawn
  if ( is.null(degrees) ) {

    # Default line position to draw axes at
    if ( is.null(line) ) line <- -.5

    axis( at, labels,
          side = side, tick = tick,
          line = line, cex.axis = cex, xpd = xpd,
          ...
    )

    # Close 'Check if rotated axes should be drawn'
  } else {

    # By default labels are values to draw axes at
    if ( is.null(labels) ) {

      labels <- as.character(at)

      # Close 'By default labels are values to draw axes at'
    }

    # Whether rotated axes should be right or left-aligned
    if ( is.null(adj) ) {

      if (side %in% c(1, 3)) adj <- 1 # Right-aligned
      if (side %in% c(2, 4)) adj <- 0 # Left-aligned

      # Close 'Whether rotated axes should be right or left-aligned'
    }

    # Get plot dimensions
    x_limits <- par()$usr[1:2]
    y_limits <- par()$usr[3:4]

    # Determine size of text
    txt_height <- strheight(labels[1], cex = cex)

    # Rotated axes at the bottom
    if (side == 1) {

      # Default line position to draw axes
      if (is.null(line)) {
        line <- y_limits[1] - txt_height / 2

        # Close 'Default line position to draw axes'
      }

      text(
        x = at, y = rep(line, length(at)),
        labels = labels, srt = degrees, xpd = xpd,
        cex = cex, adj = adj, ...
      )

      # Close 'Rotated axes at the bottom'
    }

    # Rotated axes at the top
    if (side == 3) {

      # Default line position to draw axes
      if (is.null(line)) {
        line <- y_limits[2] + txt_height / 2

        # Close 'Default line position to draw axes'
      }

      text(
        x = at, y = rep(line, length(at)),
        labels = labels, srt = degrees, xpd = xpd,
        cex = cex, adj = adj, ...
      )

      # Close 'Rotated axes at the top'
    }

    # Rotated axes at the left
    if (side == 2) {

      # Default line position to draw axes
      if (is.null(line)) {
        line <- x_limits[1] - txt_height / 2

        # Close 'Default line position to draw axes'
      }

      text(
        x = rep(line, length(at)), y = at,
        labels = labels, srt = degrees, xpd = xpd,
        cex = cex, adj = adj, ...
      )

      # Close 'Rotated axes at the left'
    }

    # Rotated axes at the right
    if (side == 4) {

      # Default line position to draw axes
      if (is.null(line)) {
        line <- x_limits[2] + txt_height / 2

        # Close 'Default line position to draw axes'
      }

      text(
        x = rep(line, length(at)), y = at,
        labels = labels, srt = degrees, xpd = xpd,
        cex = cex, adj = adj, ...
      )

      # Close 'Rotated axes at the right'
    }

    # Close else for 'Check if rotated axes should be drawn'
  }

}

#### 2.2) draw_borders_and_labels ####
#' Add Borders and Labels to an Existing Plot
#'
#' Function to add borders and labels to an existing figure.
#'
#' @param xl The lower and upper boundaries for the
#'   x-axis.
#' @param yl The lower and upper boundaries for the
#'   y-axis.
#' @param labels A character vector of up to 4 elements, the
#'   labels for the bottom, left, top, and right sides,
#'   respectively. If fewer than 4 elements are given, the
#'   corresponding labels are set to the empty character string.
#' @param sides An integer vector of up to four values ranging
#'   from 1 to 4, specifying the sides at which to draw a border.
#'   The values 1, 2, 3, and 4 indicate the bottom, left, top,
#'   and right sides, respectively.
#' @param lwd A numeric vector of up to 4 values, the width of the
#'   lines (see \code{\link[graphics]{par}}). Values are recycled
#'   if the length is less than 4.
#' @param cex A numeric vector of up to 4 values, the size of the
#'   text for the labels (see \code{\link[graphics]{par}}). Values
#'   are recycled if the length is less than 4.
#'
#' @returns Adds borders and labels to an existing figure.
#'
#' @examples
#' # Create a blank plot
#' xl <- 0:1; yl <- 0:1
#' plot_blank( xl, yl )
#'
#' # Add borders and labels
#' draw_borders_and_labels( xl, yl )
#'
#' @export

draw_borders_and_labels <- function( xl, yl,
                                     labels =
                                       c( 'X-axis', 'Y-axis', 'Title', '' ),
                                     sides = 1:2,
                                     lwd = 2,
                                     lines = 1.5,
                                     cex = 1.15 ) {

  lines <- rep_len( lines, 4 )
  cex = rep_len( cex, 4 )
  if ( length( labels ) < 4 ) {
    labels <- c(
      labels,
      rep( '', 4 - length(labels) )
    )
  }

  if ( 1 %in% sides ) {
    arfpam::draw_hv( v = xl[1], l = yl, lwd = lwd  )
  }
  if ( 2 %in% sides ) {
    arfpam::draw_hv( h = yl[1], l = xl, lwd = lwd  )
  }
  if ( 3 %in% sides ) {
    arfpam::draw_hv( v = xl[2], l = yl, lwd = lwd  )
  }
  if ( 4 %in% sides ) {
    arfpam::draw_hv( h = yl[2], l = xl, lwd = lwd  )
  }

  arfpam::draw_axes(
    xl[1] + diff(xl)*.5, labels[1],
    line = lines[1], cex = cex[1], side = 1
  )

  arfpam::draw_axes(
    yl[1] + diff(yl)*.5, labels[2],
    line = lines[2], cex = cex[2], side = 2
  )

  arfpam::draw_axes(
    xl[1] + diff(xl)*.5, labels[3],
    line = lines[3], cex = cex[3], side = 3
  )

  arfpam::draw_axes(
    yl[1] + diff(yl)*.5, labels[4],
    line = lines[4], cex = cex[4], side = 4
  )

}

#### 2.3) draw_boxplots ####
#' Add Boxplot to an Existing Plot
#'
#' Function to add a boxplot at a specified x-axis position
#' to an existing figure.
#'
#' @param x Either a single value or a data frame.
#' @param y An optional vector of 5 values giving the cut-offs
#'   for the boxplot (typically the 2.5%, 25%, 50%, 75%, and 97.5%
#'   quantiles).
#' @param columns A character vector, giving the column with the
#'   x-axis value and the 5 columns with the cut-offs for the
#'   boxplot, respectively.
#' @param width The spacing to add to the left and right of the
#'   x-axis value.
#' @param lwd The width of the lines
#'   (see \code{\link[graphics]{par}}).
#' @param col The color of the lines
#'   (see \code{\link[graphics]{par}}).
#' @param col The fill color for the box component.
#'
#' @returns Adds a boxplot to an existing figure.
#'
#' @examples
#' # Compare normal and log-normal distributions
#' dtf_data <- data.frame(
#'   Normal = rnorm( 100, 100, sqrt( 225 ) ),
#'   Log_normal = exp(
#'     rnorm( 100, 4.594045, sqrt( 0.02225061 ) )
#'   )
#' )
#'
#' # Create blank plot
#' xl <- c( .5, 2.5 )
#' yl <- c( 50, 150 )
#' plot_blank( xl, yl )
#'
#' # Add boxplots
#'
#' y <- quantile( dtf_data$Normal, prob = c( .025, .25, .5, .75, .975 ) )
#' draw_boxplots( 1, y )
#'
#' y <- quantile( dtf_data$Log_normal, prob = c( .025, .25, .5, .75, .975 ) )
#' draw_boxplots( 2, y, col = 'blue' )
#'
#' # Compute cut-offs for boxplots for data set
#' dtf <- aggregate(
#'   mtcars$mpg, list( mtcars$cyl ),
#'   quantile, prob = c( .025, .25, .5, .75, .975 )
#' )
#' dtf$X <- 1:3
#' colnames( dtf$x ) <- 'Q' %p% c( '02.5', '25.0', '50.0', '75.0', '97.5' )
#' dtf <- cbind( dtf[,c('Group.1', 'X') ], dtf$x )
#'
#' # Create blank plot
#' xl <- c( .5, 3.5 )
#' yl <- c( 5, 35 )
#' plot_blank( xl, yl )
#' draw_hv( h = yl, l = xl )
#' draw_hv( v = xl, l = yl )
#'
#' # Add boxplots per cylinder type
#' draw_boxplots(
#'   dtf[1,], bg = palettes( index = 1 )
#' )
#' draw_boxplots(
#'   dtf[2,], bg = palettes( index = 2 )
#' )
#' draw_boxplots(
#'   dtf[3,], bg = palettes( index = 3 )
#' )
#'
#' # Add axes and labels
#' draw_axes( 1:3, dtf$Group.1 )
#' mtext( 'Cylinders', side = 1, line = 2, cex = 1.25 )
#' draw_axes( c( 10, 20, 30 ), side = 2 )
#' mtext( 'MPG', side = 2, line = 2, cex = 1.25 )
#'
#' @export

draw_boxplots <- function( x, y = NULL,
                           columns = NULL,
                           width = .25,
                           lwd = 2,
                           col = 'black', bg = NA ) {

  # Check use cases
  current_use <- "Incorrect input"

  # Data frame provided
  if ( is.data.frame(x) & is.null(y) ) {
    current_use <- "Variable 'x' is a data frame"
  }

  # Numeric vectors provided
  if ( is.numeric(x) & !is.null(y) ) {

    if ( is.numeric(y) ) {
      current_use <- "Variable 'x' is a value and 'y' is a numeric vector"
    }

    # Close 'Numeric vectors provided'
  }

  # If no correct inputs found
  if ( current_use == "Incorrect input" ) {

    stop(
      '\n' %p% current_use %p% ' - expected that either...\n' %p%
        "   (1) variable 'x' is a data frame\n" %p%
        "   (2) variable 'x' is a value and 'y' is a numeric vector\n"
    )

    # Close 'If no correct inputs found'
  }

  # Extract variables for data frame
  if ( current_use == "Variable 'x' is a data frame" ) {

    # Save data frame
    dtf <- x

    # Specify separate vectors for 'x' and 'y'

    # Column names are provided
    if ( !is.null( columns ) ) {

      x <- dtf[[ columns[1] ]][1]
      y <- as.numeric( dtf[1, columns[1 + 1:5] ] )

      # Close 'Column names are provided'
    } else {

      # Data frame has X/Q variables
      columns_to_find <- c(
        'X',
        'Q' %p% c( '02.5', '25.0', '50.0', '75.0', '97.5' )
      )

      if ( all( columns_to_find %in% colnames( dtf ) ) ) {

        x <- dtf$X[1]
        y <- as.numeric( dtf[1, columns_to_find[-1] ] )

        # Close 'Data frame has X/Y variables'
      } else {

        # Throw an error
        stop(
          "Must specify column for 'x' and 5 columns for 'y'"
        )

      }

      # Close else for 'Column names are provided'
    }

    # Close 'Extract variables for data frame'
  }


  # Draw box for 25% - 75% interval
  polygon(
    c( x - width, x - width, x + width, x + width ),
    c( y[2], y[4], y[4], y[2] ),
    lwd = lwd, col = bg, border = col
  )

  # Draw line from 2.5% to 25%
  segments( x, y[1], x, y[2], lwd = lwd, col = col )

  # Draw line for median
  segments( x - width, y[3], x + width, y[3], lwd = lwd, col = col )

  # Draw line from 75% to 97.5%
  segments( x, y[4], x, y[5], lwd = lwd, col = col )

}

#### 2.4) draw_dots ####
#' Add Points and Error Bars to an Existing Plot
#'
#' Function to add points and error bars to an
#' existing plot.
#'
#' @param x Either a numeric vector or a data frame.
#' @param y An optional numeric vector matching in length to \code{x}.
#' @param lb An optional numeric vector matching in length to \code{x},
#'   specifying the lower bounds for error bars.
#' @param ub An optional numeric vector matching in length to \code{x},
#'   specifying the upper bounds for error bars.
#' @param columns A character vector of either 2 or 4 elements, the
#'   column names for the x and y-axis values and (optionally) the
#'   column names for the lower and upper bounds of the error bars.
#' @param pch The type of point to draw
#'   (see \code{\link[graphics]{par}}).
#' @param cex The size of the points to draw
#'   (see \code{\link[graphics]{par}}).
#' @param lwd The width of the lines
#'   (see \code{\link[graphics]{par}}).
#' @param length The width of the caps on the error bars.
#' @param col The color of the points
#'   (see \code{\link[graphics]{par}}).
#' @param bg The background color of the points
#'   (see \code{\link[graphics]{par}}).
#' @param col.eb The color of the error bars.
#' @param aes An optional named character vector specifying
#'   column names (if \code{x} is a data frame) with
#'   values for \code{pch}, \code{cex}, \code{lwd},
#'   \code{col}, \code{bg}, and \code{col.eb}.
#'
#' @returns Adds points and error bars to an existing plot.
#'
#' @examples
#' # Add three points
#' plot_blank()
#' draw_dots( c( 0, .5, 1 ), c( 0, .5, 1 ) )
#'
#' # Pass points in via data frame
#' draw_dots(
#'   data.frame( X = c( .1, .2, .3 ), Y = c( .8, .8, .8 ) ),
#'   col = 'blue'
#' )
#'
#' # Compute mean, SE, and 95% CI limits for data set
#' dtf <- aggregate(
#'   mtcars$mpg, list( mtcars$cyl ), function(x) c( mean(x), sem(x) )
#' )
#' dtf$X <- 1:3
#' dtf$M <- dtf$x[,1];
#' dtf$LB <- dtf$x[,1] - dtf$x[,2] * 1.96
#' dtf$UB <- dtf$x[,1] + dtf$x[,2] * 1.96
#'
#' # Create blank plot
#' xl <- c( .5, 3.5 ); yl <- c( 10, 30 )
#' plot_blank( xl, yl )
#' draw_hv( h = yl, l = xl )
#' draw_hv( v = xl, l = yl )
#'
#' draw_dots(
#'   dtf, columns = c( 'X', 'M', 'LB', 'UB' ),
#'   col = palettes( index = 1:3 )
#' )
#'
#' # Add axes and labels
#' draw_axes( 1:3, dtf$Group.1 )
#' mtext( 'Cylinders', side = 1, line = 2, cex = 1.25 )
#' draw_axes( c( 10, 20, 30 ), side = 2 )
#' mtext( 'MPG', side = 2, line = 2, cex = 1.25 )
#'
#' @export

draw_dots <- function( x,
                       y = NULL,
                       lb = NULL,
                       ub = NULL,
                       columns = NULL,
                       pch = 19,
                       cex = 1.25,
                       lwd = 2,
                       length = .05,
                       col = 'black',
                       bg = 'white',
                       col.eb = 'black',
                       aes = NULL ) {

  # Check use cases
  current_use <- "Incorrect input"

  # Data frame provided
  if ( is.data.frame(x) & is.null(y) ) {
    current_use <- "Variable 'x' is a data frame"
  }

  # Numeric vectors provided
  if ( is.numeric(x) & !is.null(y) ) {

    if ( is.numeric(y) ) {
      current_use <- "Variables 'x' and 'y' are numeric vectors"
    }

    # Close 'Numeric vectors provided'
  }

  # If no correct inputs found
  if ( current_use == "Incorrect input" ) {

    stop(
      '\n' %p% current_use %p% ' - expected that either...\n' %p%
        "   (1) variable 'x' is a data frame\n" %p%
        "   (2) variable 'x' and 'y' are numeric vectors\n"
    )

    # Close 'If no correct inputs found'
  }

  # Extract variables for data frame
  if ( current_use == "Variable 'x' is a data frame" ) {

    # Save data frame
    dtf <- x

    # Specify separate vectors for 'x' and 'y'

    # Column names are provided
    if ( !is.null( columns ) ) {

      x <- dtf[[ columns[1] ]]
      y <- dtf[[ columns[2] ]]

      # Lower and upper bounds provided
      if ( length( columns ) == 4 ) {

        lb <- dtf[[ columns[3] ]]
        ub <- dtf[[ columns[4] ]]

        # Close 'Lower and upper bounds provided'
      }

      # If columns for aesthetic variables provided
      if ( !is.null( aes ) ) {

        if ( 'pch' %in% names(aes) ) {
          pch <- dtf[[ aes['pch'] ]]
        }

        if ( 'cex' %in% names(aes) ) {
          cex <- dtf[[ aes['cex'] ]]
        }

        if ( 'lwd' %in% names(aes) ) {
          lwd <- dtf[[ aes['lwd'] ]]
        }

        if ( 'col' %in% names(aes) ) {
          col <- dtf[[ aes['col'] ]]
        }

        if ( 'bg' %in% names(aes) ) {
          bg <- dtf[[ aes['bg'] ]]
        }

        if ( 'col.eb' %in% names(aes) ) {
          col.eb <- dtf[[ aes['col.eb'] ]]
        }

        # Close 'If columns for aesthetic variables provided'
      }

      # Close 'Column names are provided'
    } else {

      # Data frame has X/Y variables
      if ( all( c( 'X', 'Y' ) %in% colnames( dtf ) ) ) {

        x <- dtf$X
        y <- dtf$Y

        # Close 'Data frame has X/Y variables'
      } else {

        # Use first two columns
        x <- dtf[[1]]
        y <- dtf[[2]]

        # Close else for 'Data frame has X/Y variables'
      }

      # Close else for 'Column names are provided'
    }

    # Close 'Extract variables for data frame'
  }

  # Add error bars to figure
  if ( !is.null(lb) & !is.null(ub) ) {

    error_bars(
      x, lb = lb, ub = ub, lwd = lwd, length = length, col = col.eb
    )

    # Close 'Add error bars to figure'
  }

  # Add points to figure
  points( x, y, pch = pch, cex = cex, col = col, bg = bg )

}

#### 2.5) draw_error_bars ####
#' Add Error Bars to a Plot
#'
#' Adds error bars to an existing plot.
#'
#' @param x Either a numeric vector or a data frame.
#'   If a numeric vector, specifies the positions at
#'   which error bars should be drawn.
#' @param lower A numeric vector with the lower limits
#'   for error bars.
#' @param upper A numeric vector with the upper limits
#'   for error bars.
#' @param arrow Logical; if \code{TRUE},
#'   adds error bars using [graphics::arrows],
#'   otherwise adds error bars using
#'   [graphics::polygon] instead.
#' @param flip Logical; if \code{TRUE}, error bars
#'   are drawn at positions on the y-axis with limits
#'   specified over the x-axis; otherwise, error bars
#'   are drawn at positions on the x-axis with limits
#'   specified over the y-axis.
#' @param length The length of the arrowhead for the
#'   call to [graphics::arrows].
#' @param code Integer controlling whether to draw an
#'   arrowhead at the start (1), end (2), or at both ends (3)
#'   of the line for the call to [graphics::arrows].
#' @param angle The angle of the lines creating the arrowhead
#'   for the call to [graphics::arrows]. Using
#'   90 degrees results in a flat bar per typical error bars.
#' @param border The color of the border for calls to
#'   [graphics::polygon].
#' @param columns A character vector, the columns for the
#'   positions and lower/upper limits, respectively, if
#'   \code{x} is a data frame.
#' @param ... Additional plotting parameters for
#'   either the [graphics::arrows] or [graphics::polygon].
#'
#' @export

draw_error_bars <- function( x,
                             lower = NULL,
                             upper = NULL,
                             arrow = TRUE,
                             flip = FALSE,
                             length = .05,
                             code = 3,
                             angle = 90,
                             border = NA,
                             columns = NULL,
                             ... ) {


  current_use <- 'Incorrect inputs'

  # If data frame provided
  if ( is.data.frame(x) ) {

    current_use <- "Variable 'x' is a data frame"

    # Close 'If data frame provided'
  } else {

    # If vectors for limits provided
    if ( !is.null(lower) & !is.null(upper) ) {

      # If vectors are same length
      if ( length(lower) == length(upper) & length(x) == length(lower) ) {

        current_use <-
          "Variables 'x' + 'lower' + 'upper' are numeric vectors"

        # Close 'If vectors are same length'
      }

      # Close 'If vectors for limits provided'
    }

    # Close else for 'If data frame provided'
  }

  # If incorrect inputs provided
  if ( current_use == 'Incorrect inputs' ) {

    stop(
      '\n' %p% current_use %p% ' - expected that either...\n' %p%
        "   (1) variable 'x' is a data frame\n" %p%
        "   (2) variable 'x', 'lower', and 'upper' are numeric vectors\n"
    )

    # Close 'If incorrect inputs provided'
  }

  # If data frame provided
  if ( current_use == "Variable 'x' is a data frame" ) {

    current_use <- 'Data frame'

    # If columns provided
    if ( is.null( columns ) ) {

      columns <- colnames(x)[1:3]

      # Close 'If columns provided'
    }

    position <- x[[ columns[1] ]]
    lower_limit <- x[[ columns[2] ]]
    upper_limit <- x[[ columns[3] ]]

    # Close 'If data frame provided'
  } else {

    position <- x
    lower_limit <- lower
    upper_limit <- upper

    # Close else for 'If data frame provided'
  }

  # If drawing separate bars
  if ( arrow ) {

    # If position is for y-axis
    if ( flip ) {

      arrows(
        lower_limit, position,
        upper_limit, position,
        length = length,
        code = code,
        angle = angle,
        ...
      )

      # Close 'If position is for y-axis'
    } else {

      arrows(
        position, lower_limit,
        position, upper_limit,
        length = length,
        code = code,
        angle = angle,
        ...
      )

      # Close else for 'If position is for y-axis'
    }

    # Close 'If drawing separate bars'
  } else {

    # If position is for y-axis
    if ( flip ) {

      polygon(
        c( lower_limit, rev( upper_limit ) ),
        c( position, rev( position ) ),
        border = border,
        ...
      )

      # Close 'If position is for y-axis'
    } else {

      polygon(
        c( position, rev( position ) ),
        c( lower_limit, rev( upper_limit ) ),
        border = border,
        ...
      )

      # Close else for 'If position is for y-axis'
    }

    # Close else for 'If drawing separate bars'
  }

}

#### 2.6) draw_hv ####
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
#' plot_blank()
#'
#' # Draw horizontal line
#' draw_hv(h = .5, lty = 2)
#' # Draw vertical line
#' draw_hv(v = .5, lwd = 2)
#'
#' # Control width of horizontal line
#' draw_hv(h = .25, l = c(.25, .75), col = "blue")
#' # Control height of vertical line
#' draw_hv(v = .25, l = c(.25, .75), col = "orange")
#'
#' @export

draw_hv <- function( h = NULL,
                     v = NULL,
                     l = NULL,
                     ... ) {

  # If vertical line
  if ( !is.null(v) ) {

    n <- length(v)

    # If limits provided
    if ( is.null(l) ) {

      l <- par("usr")[3:4]

      # Close 'If limits provided'
    }

    segments(
      v, rep(l[1], n),
      v, rep(l[2], n), ...
    )

    # Close 'If vertical line'
  }

  # If horizontal line
  if ( !is.null(h) ) {

    n <- length(h)

    # If limits provided
    if ( is.null(l) ) {

      l <- par("usr")[1:2]

      # Close 'If limits provided'
    }

    segments(
      rep(l[1], n), h,
      rep(l[2], n), h, ...
    )

    # Close 'If horizontal line'
  }

}

#### 2.7) draw_legend ####
#' Add a Legend to an Existing Plot
#'
#' Function to add a legend to an
#' existing plot.
#'
#' @param x The lower and upper boundaries for the
#'   x-axis.
#' @param y The lower and upper boundaries for the
#'   y-axis.
#' @param legend A character vector, the text for the
#'   legend (see [graphics::legend]).
#' @param bty A character string, either \code{"o"} or
#'   \code{"n"} (the default), the type of box to
#'   draw around the legend (see [graphics::legend]).
#' @param xpd A logical value or \code{NA}, determining
#'   how the legend should be clipped relative to the
#'   figure region (see [graphics::par]). Defaults to \code{NA}.
#' @param adj A numeric vector of two values, the
#'   relative x and y-axis position of the legend,
#'   where values are ratios (e.g., values of
#'   1 will place the legend exactly at the top right
#'   corner of the figure boundary).
#' @param ... Additional arguments to pass to [graphics::legend].
#'
#' @returns Adds a legend to an existing plot.
#'
#' @examples
#' # Example figure
#' x <- 0:1; y <- 0:1
#' plot_blank(x, y)
#'
#' # Add legend in middle of figure
#' draw_legend(
#'   x, y, 'Example',
#'   adj = c( .5, .5 )
#' )
#'
#' @export

draw_legend <- function( x,
                         y,
                         legend,
                         bty = 'n',
                         xpd = NA,
                         adj = c( 0, 1.2 ),
                         ... ) {

  legend(
    x = x[1] + diff(x) * adj[1],
    y = y[1] + diff(y) * adj[2],
    legend = legend,
    bty = bty,
    xpd = xpd,
    ...
  )

}

#### 2.8) draw_lines ####
#' Add Lines and Error Bars to an Existing Plot
#'
#' Function to add lines and error bars to an
#' existing plot.
#'
#' @param x Either a numeric vector or a data frame.
#' @param y An optional numeric vector matching in length to \code{x}.
#' @param lb An optional numeric vector matching in length to \code{x},
#'   specifying the lower bounds for error bars.
#' @param ub An optional numeric vector matching in length to \code{x},
#'   specifying the upper bounds for error bars.
#' @param columns A character vector of either 2 or 4 elements, the
#'   column names for the x and y-axis values and (optionally) the
#'   column names for the lower and upper bounds of the error bars.
#' @param pch The type of point to draw
#'   (see \code{\link[graphics]{par}}).
#' @param cex The size of the points to draw
#'   (see \code{\link[graphics]{par}}).
#' @param lwd The width of the lines
#'   (see \code{\link[graphics]{par}}).
#' @param lty The type of line to draw
#'   (see \code{\link[graphics]{par}}).
#' @param arrow Logical; if \code{TRUE} draws individual error bars
#'   while if \code{FALSE} draws a single filled bar.
#' @param length The width of the caps on the error bars.
#' @param col The color of the lines
#'   (see \code{\link[graphics]{par}}).
#' @param col.p The color of the points
#'   (see \code{\link[graphics]{par}}).
#' @param bg The background color of the points
#'   (see \code{\link[graphics]{par}}).
#' @param col.eb The color of the error bars.
#' @param border The color for the border of a single
#'   filled error bar.
#' @param aes An optional named character vector specifying
#'   column names (if \code{x} is a data frame) with
#'   values for \code{pch}, \code{cex}, \code{lwd},
#'   \code{lty}, \code{col}, \code{col.p}, \code{col.eb},
#'   and \code{bg}.
#'
#' @returns Adds lines and error bars to an existing plot.
#'
#' @examples
#' # Draw a line
#' plot_blank()
#' draw_lines( c( 0, .5, 1 ), c( 0, .5, 1 ) )
#'
#' # Pass points in via data frame
#' draw_lines(
#'   data.frame( X = c( .1, .2, .3 ), Y = c( .8, .8, .8 ) ),
#'   lty = 2, lwd = 3, col.p = 'blue', col = 'blue'
#' )
#'
#' # Compute mean, SE, and 95% CI limits for data set
#' dtf <- aggregate(
#'   mtcars$mpg, list( mtcars$cyl, mtcars$am ),
#'   function(x) c( mean(x), sem(x) )
#' )
#' dtf$X <- rep( 1:3, 2 )
#' dtf$M <- dtf$x[,1];
#' dtf$LB <- dtf$x[,1] - dtf$x[,2] * 1.96
#' dtf$UB <- dtf$x[,1] + dtf$x[,2] * 1.96
#'
#' # Create blank plot
#' xl <- c( .5, 3.5 ); yl <- c( 5, 35 )
#' plot_blank( xl, yl )
#' draw_hv( h = yl, l = xl )
#' draw_hv( v = xl, l = yl )
#'
#' # Draw lines for automatic transmission
#' draw_lines(
#'   dtf[1:3,],
#'   columns = c( 'X', 'M', 'LB', 'UB' )
#' )
#'
#' # Draw lines for manual transmission
#' draw_lines(
#'   dtf[1:3 + 3,],
#'   columns = c( 'X', 'M', 'LB', 'UB' ),
#'   col = 'blue', col.p = 'blue', col.eb = col_to_hex( 'blue', .3 )
#' )
#'
#' # Add axes and labels
#' draw_axes( 1:3, dtf$Group.1[1:3] )
#' mtext( 'Cylinders', side = 1, line = 2, cex = 1.25 )
#' draw_axes( c( 10, 20, 30 ), side = 2 )
#' mtext( 'MPG', side = 2, line = 2, cex = 1.25 )
#'
#' # Add legend
#' legend(
#'   2.5, 30,
#'   c( 'Automatic', 'Manual' ),
#'   fill = c( 'black', 'blue' ),
#'   bty = 'n'
#' )
#'
#' @export

draw_lines <- function( x,
                        y = NULL,
                        lb = NULL,
                        ub = NULL,
                        columns = NULL,
                        pch = 19,
                        cex = 1.25,
                        lwd = 2,
                        lty = 1,
                        arrow = FALSE,
                        length = .05,
                        col = 'black',
                        col.p = 'black',
                        col.eb = col_to_hex( 'grey', .5 ),
                        bg = 'white',
                        border = NA,
                        aes = NULL ) {

  # Check use cases
  current_use <- "Incorrect input"

  # Data frame provided
  if ( is.data.frame(x) & is.null(y) ) {
    current_use <- "Variable 'x' is a data frame"
  }

  # Numeric vectors provided
  if ( is.numeric(x) & !is.null(y) ) {

    if ( is.numeric(y) ) {
      current_use <- "Variables 'x' and 'y' are numeric vectors"
    }

    # Close 'Numeric vectors provided'
  }

  # If no correct inputs found
  if ( current_use == "Incorrect input" ) {

    stop(
      '\n' %p% current_use %p% ' - expected that either...\n' %p%
        "   (1) variable 'x' is a data frame\n" %p%
        "   (2) variable 'x' and 'y' are numeric vectors\n"
    )

    # Close 'If no correct inputs found'
  }

  # Extract variables for data frame
  if ( current_use == "Variable 'x' is a data frame" ) {

    # Save data frame
    dtf <- x

    # Specify separate vectors for 'x' and 'y'

    # Column names are provided
    if ( !is.null( columns ) ) {

      x <- dtf[[ columns[1] ]]
      y <- dtf[[ columns[2] ]]

      # Lower and upper bounds provided
      if ( length( columns ) == 4 ) {

        lb <- dtf[[ columns[3] ]]
        ub <- dtf[[ columns[4] ]]

        # Close 'Lower and upper bounds provided'
      }


      # If columns for aesthetic variables provided
      if ( !is.null( aes ) ) {

        if ( 'pch' %in% names(aes) ) {
          pch <- dtf[[ aes['pch'] ]]
        }

        if ( 'cex' %in% names(aes) ) {
          cex <- dtf[[ aes['cex'] ]]
        }

        if ( 'lwd' %in% names(aes) ) {
          lwd <- dtf[[ aes['lwd'] ]]
        }

        if ( 'lty' %in% names(aes) ) {
          lty <- dtf[[ aes['lty'] ]]
        }

        if ( 'col' %in% names(aes) ) {
          col <- dtf[[ aes['col'] ]]
        }

        if ( 'col.p' %in% names(aes) ) {
          col.p <- dtf[[ aes['col.p'] ]]
        }

        if ( 'col.eb' %in% names(aes) ) {
          col.eb <- dtf[[ aes['col.eb'] ]]
        }

        if ( 'bg' %in% names(aes) ) {
          bg <- dtf[[ aes['bg'] ]]
        }

        # Close 'If columns for aesthetic variables provided'
      }

      # Close 'Column names are provided'
    } else {

      # Data frame has X/Y variables
      if ( all( c( 'X', 'Y' ) %in% colnames( dtf ) ) ) {

        x <- dtf$X
        y <- dtf$Y

        # Close 'Data frame has X/Y variables'
      } else {

        # Use first two columns
        x <- dtf[[1]]
        y <- dtf[[2]]

        # Close else for 'Data frame has X/Y variables'
      }

      # Close else for 'Column names are provided'
    }

    # Close 'Extract variables for data frame'
  }

  # Add error bars to figure
  if ( !is.null(lb) & !is.null(ub) ) {

    arfpam::error_bars(
      x, lb = lb, ub = ub,
      lwd = lwd, length = length,
      col = col.eb, arrow = arrow,
      border = border
    )

    # Close 'Add error bars to figure'
  }

  # Add lines to figure
  lines( x, y, lwd = lwd, col = col, lty = lty )

  # Add points to figure
  points( x, y, pch = pch, cex = cex, col = col.p, bg = bg )

}

#### 3) Functions to plot specific types of plots ####

#### 3.1) plot_blank ####
#' Generate a Blank Plot
#'
#' This function generates a completely blank plot.
#'
#' @param x The lower and upper boundaries for the
#'   x-axis.
#' @param y The lower and upper boundaries for the
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
#' plot_blank(guidelines = TRUE)
#'
#' @export

plot_blank <- function( x = c(0, 1),
                        y = c(0, 1),
                        guidelines = FALSE,
                        cex = 0.8 ) {

  plot(
    x, y,
    type = "n", ylab = " ", xlab = " ",
    xaxt = "n", yaxt = "n", bty = "n"
  )

  # Margin info
  if (guidelines) {

    # User specified boundaries for
    # plotting window
    segments(
      x[c(1, 2, 1, 1)],
      y[c(1, 1, 1, 2)],
      x[c(1, 2, 2, 2)],
      y[c(2, 2, 1, 2)],
      col = "black",
      lwd = 2
    )

    # Adjusted boundaries for
    # plotting window

    bnd <- par("usr")

    segments(
      bnd[c(1, 2, 1, 1)],
      bnd[c(3, 3, 3, 4)],
      bnd[c(1, 2, 2, 2)],
      bnd[c(4, 4, 3, 4)],
      col = "grey90",
      lwd = 2
    )

    mrg <- par("mar")

    # Axis lines
    out <- sapply(1:4, function(s) {
      for (i in -1:floor(mrg[s])) {
        pst <- x[1] + diff(x) / 2
        if (s %in% c(2, 4)) {
          pst <- y[1] + diff(y) / 2
        }
        axis(s, pst, i, line = i, tick = F, cex.axis = cex)
      }
    })

    # Percentages of plotting units
    out <- sapply(1:2, function(s) {
      for (i in seq( .05, .3, .05 ) ) {
        pst <- c( x[1] - diff(x) * i, y[2] )
        r <- 90
        if (s %in% 2) {
          pst <- c( x[1], y[2] + diff(y)*i )
          r <- 0
        }
        text( pst[1], pst[2], format( i, nsmall = 2 ),
              cex.axis = cex, xpd = NA, srt = r )
      }
    })

    # Close 'Margin info'
  }

}

#### 3.2) plot_correlations ####
#' General-purpose Function to Plot Correlations
#'
#' Function to plot either a) the upper triangle
#' component of a correlation matrix, or b)
#' a panel of correlations (e.g., between raw and
#' component scores from a PCA, between predictors
#' and different outcomes, etc.).
#'
#' @param dtf A data frame, a set of variables to
#'   compute a correlation matrix over.
#' @param R A matrix of correlations (must be supplied
#'   if \code{dtf} is not provided, otherwise computed
#'   automatically).
#' @param n An integer, the sample size for the correlations
#'   (computed automatically if \code{dtf} is provided).
#' @param p_values A matrix of p-values (must have same dimensions
#'   as \code{R}).
#' @param labels Either a character vector with the
#'   labels for the rows of the correlation matrix, or a
#'   list of character vectors with the labels for the
#'   rows and columns, respectively, for the correlation
#'   matrix.
#' @param label_pos A numeric vector the x-axis adjustment
#'   for row labels and the y-axis adjustment for column
#'   labels, respectively.
#' @param only_upper_tri A logical value, \code{TRUE} if
#'   only the upper triangle of the correlation matrix
#'   should be plotted.
#' @param new A logical value; if \code{TRUE} a new plotting
#'   window is created.
#' @param width An integer value, the width in inches of the plot.
#' @param height An integer value, the height in inches of the plot.
#' @param margin A numeric vector, four values specifying the margins
#'   of the plot in inches, giving the spacing for the bottom, left,
#'   top, and right sides respectively.
#' @param fill A character vector, the colors for negative and
#'   positive correlations respectively.
#' @param opaque A numeric value ranging from 0 to 1, with lower
#'   values indicating greater translucency of the fill color.
#' @param cex A numeric vector of two values, the text size for
#'   the row/column labels and correlation values, respectively.
#' @param digits An integer value, the number of digits to
#'   round correlation values to.
#' @param method A character string, the method to use for
#'   multiple comparison adjustment (see [stats::p.adjust]).
#' @param alpha A numeric value between 0 and 1, the cut-off
#'   for statistical significance (default is 0.05).
#' @param legend_pos A numeric vector of 3 values governing
#'   the y-axis position and x-axis positions, respectively,
#'   for the legend.
#'
#' @returns A plot of the correlations.
#'
#' @examples
#' # Correlation matrix
#' data(mtcars)
#' plot_correlations( dtf = mtcars, new = FALSE )
#'
#' # Example based on PCA
#'
#' # Loading matrix
#' lambda <- cbind(
#'   c( runif( 4, .3, .9 ), rep( 0, 4 ) ),
#'   c( rep( 0, 4 ), runif( 4, .3, .9 ) )
#' )
#' # Communalities
#' D_tau <- diag( runif( 8, .5, 1.5 ) )
#'
#' cov_mat <- lambda %*% t( lambda ) + D_tau
#' cor_mat <- cov2cor( cov_mat )
#'
#' set.seed( 341 ) # For reproducibility
#' x <- MASS::mvrnorm( n = 200, mu = rep( 0, 8 ), Sigma = cor_mat )
#' colnames(x) <- paste0( 'C', 1:8 )
#' PCA <- principal_components_analysis( x )
#'
#' # Correlations between raw variables
#' # and component scores from PCA
#' plot_correlations(
#'   R = PCA$Correlations$Train[, 1:2], n = 200,
#'   labels = list(
#'     paste0( 'Variable ', 1:8 ),
#'     paste0( 'Comp. ', 1:2 )
#'   ),
#'   only_upper_tri = F, margin = c( .25, 2, .25, .25 ),
#'   legend_pos = c( 0, 0, -.25 ),
#'   new = FALSE
#' )
#'
#' @export

plot_correlations <- function( dtf = NULL,
                               R = NULL,
                               n = NULL,
                               p_values = NULL,
                               labels = NULL,
                               label_pos = c( .2, .25 ),
                               only_upper_tri = TRUE,
                               new = TRUE,
                               width = 5,
                               height = 5,
                               margin = NULL,
                               fill = c( "#E69F00", "#56B4E9" ),
                               opaque = .4,
                               cex = c( .8, .8 ),
                               value = TRUE,
                               digits = 2,
                               method = 'BH',
                               alpha = .05,
                               legend_pos = c( 0, .5, 0 ) ) {

  # If data frame is provided
  if ( !is.null( dtf ) ) {

    is_na <- apply(
      dtf, 1, function(x) any( is.na(x) )
    )
    dtf <- dtf[!is_na, ]

    if ( any( is_na ) ) {
      warning(
        paste0( 'Excluded ', sum(is_na), ' rows due to NA values' )
      )
    }

    R <- cor( dtf )
    n <- nrow( dtf )

    # Close 'If data frame is provided'
  }

  # Variable labels
  if ( is.null( labels ) ) {

    lst_labels <- list(
      rows = paste0( 1:nrow(R), ') ', rownames( R ) ),
      cols = 1:ncol( R )
    )

    # Close 'Variable labels'
  } else {

    # If list is provided
    if ( is.list( labels ) ) {

      lst_labels <- list(
        rows = labels[[1]],
        cols = labels[[2]]
      )

      # Close 'If list is provided'
    } else {

      lst_labels <- list(
        rows = labels,
        cols = 1:ncol( R )
      )

      # Close else for 'If list is provided'
    }

    # Close else 'Variable labels'
  }

  # Check if correlation matrix exists
  if ( is.null(R) ) {

    stop( "Must provide data frame 'dtf' or correlation matrix 'R'" )

    # Close 'Check if correlation matrix exists'
  }

  int_rows <- nrow( R )
  int_cols <- ncol( R )

  # Plot upper triangle
  if ( only_upper_tri ) {

    int_row_index <- 1:( int_rows - 1 )

    lst_col_index <- lapply(
      int_row_index, function(j) {
        return( (j+1):int_cols )
      }
    )

    lst_labels[[2]][1] <- ''

    # Close 'Plot upper triangle'
  } else {

    int_row_index <- 1:int_rows

    lst_col_index <- lapply(
      int_row_index, function(j) {
        return( 1:int_cols )
      }
    )

    # Close else for 'Plot upper triangle'
  }

  # User-supplied p-values
  if ( !is.null(p_values) ) {

    mat_p <- p_values

    # Confirm dimensions match
    if ( !all( dim(mat_p) == dim(R) ) ) {

      stop(
        'Matrix of p-values must have same number of rows and columns as R'
      )

      # Close 'Confirm dimensions match'
    }

    # Close 'User-supplied p-values'
  } else {

    # Initialize matrix of p-values
    mat_p <- matrix( NA, nrow(R), ncol(R) )

    # If sample size is provided
    if ( !is.null(n) ) {

      # Loop over rows
      for ( j in seq_along( int_row_index ) ) {

        # Loop over columns
        for ( k in seq_along( lst_col_index[[j]] ) ) {

          cr <- int_row_index[j]
          cc <- lst_col_index[[j]][k]

          r <- R[ cr, cc ]

          z <- atanh(r)
          z.se <- 1/sqrt( n - 3 )

          mat_p[ cr, cc ] <- pt(
            abs( z/z.se ), df = n - 1, lower.tail = F
          )*2

          # Close 'Loop over columns'
        }

        # Close 'Loop over rows'
      }

      # Adjust for multiple comparisons
      if ( method != '' ) {
        mat_p[ upper.tri(mat_p) ] <- p.adjust(
          mat_p[ upper.tri(mat_p) ],
          method = method
        )

        # Close 'Adjust for multiple comparisons'
      }

      # Close 'If sample size is provided'
    }

    # Close else for 'User-supplied p-values'
  }

  # Function to add square with correlation magnitude
  draw_square <- function( r,
                           j,
                           k,
                           value,
                           p ) {

    fill_col <- fill[1]
    if ( r > 0 ) fill_col <- fill[2]

    r <- abs(r)

    x <- c( -1, -1, 0, 0 )
    y <- c( -1, -1 + r, -1 + r, -1 )

    polygon(
      x + k, y + j,
      col = arfpam::col_to_hex( fill_col, opaque ), border = NA
    )

    polygon(
      x + k, c( -1, 0, 0, -1 ) + j, col = NA, border = 'black'
    )

    # If p-value provided
    if ( !is.na(p) ) {

      # If non-significant
      if ( p > alpha ) {

        segments(
          -1 + k,
          -1 + j,
          k,
          j,
          col = 'grey40'
        )

        # Close 'If non-significant'
      }

      # Close 'If p-value provided'
    }

    # If correlation value should be displayed
    if ( value ) {

      text(
        -1 + k + .5, -1 + j + .5,
        round( r, digits ) |> format( nsmall = digits ),
        cex = cex[2]
      )

      # Close 'If correlation value should be displayed'
    }

  }

  # New plotting window
  if ( new ) {

    x11( width = width, height = height )

    # Close 'New plotting window'
  }

  # Default margin
  if ( is.null(margin) ) {

    num_spacing <- c(
      (height * .1)/2, # Bottom/Top
      width - height*.9 - (height * .1)/2  # Left
    )

    margin <- num_spacing[ c( 1, 2, 1, 1 ) ]

    # Close 'Default margin'
  }

  # Plotting dimensions
  xl <- c( 0, int_cols )
  yl <- c( 0, int_rows )

  par( mai = margin )

  # Create a blank plot
  plot(
    xl, yl, type = 'n',
    xaxt = 'n', yaxt = 'n',
    xlab = '', ylab = '',
    bty = 'n'
  )

  # Loop over rows
  for ( j in seq_along( int_row_index ) ) {

    # Loop over columns
    for ( k in seq_along( lst_col_index[[j]] ) ) {

      draw_square(
        R[ int_row_index[j], lst_col_index[[j]][k] ],
        rev( 1:int_rows )[ int_row_index[j] ],
        lst_col_index[[j]][k],
        value,
        mat_p[ int_row_index[j], lst_col_index[[j]][k] ]
      )

      # Close 'Loop over columns'
    }

    # Add labels for rows
    text(
      lst_col_index[[j]][1] - 1 - label_pos[1],
      rev( 1:int_rows )[ int_row_index[j] ] - .5,
      lst_labels[[1]][j],
      cex = cex[1],
      xpd = NA,
      pos = 2
    )

    # Close 'Loop over rows'
  }

  # Add final label for rows
  if ( only_upper_tri ) {

    text(
      int_cols - .5,
      1 - .5,
      lst_labels[[1]][int_rows],
      cex = cex[1]
    )

    # Close 'Add final label for rows'
  }

  # Add labels for columns
  for ( k in 1:ncol( R ) ) {

    text(
      k - .5,
      int_rows + label_pos[2],
      lst_labels[[2]][k],
      cex = cex[1],
      xpd = NA
    )

    # Close 'Add labels for columns'
  }

  legend(
    int_rows*legend_pos[2], int_cols*legend_pos[1],
    c( 'Negative', 'Positive' ),
    fill = c(
      arfpam::col_to_hex( fill[1], opaque ),
      arfpam::col_to_hex( fill[2], opaque )
    ),
    horiz = TRUE,
    cex = cex[1],
    bty = 'n',
    xpd = NA
  )

  # If p-values provided
  if ( any( !is.na( mat_p ) ) ) {

    legend(
      int_rows*legend_pos[3], int_cols*legend_pos[1],
      paste0( '\U2215', ' p > ', alpha ),
      cex = cex[1],
      bty = 'n',
      xpd = NA
    )

    # Close 'If p-values provided'
  }

}


#### 3.3) plot_forest ####
#' Create a Forest Plot
#'
#' Generates a simple forest plot (defined here as
#' a plot of multiple estimates and their associated
#' error bars).
#'
#' @param x Either a vector of estimates or
#'   a data.frame/matrix with three columns
#'   consisting of the estimates, the lower limits
#'   the for error bars, and the upper limits for
#'   the error bars).
#' @param lower A vector matching in length to
#'   \code{values} with the lower limits for
#'   the error bars.
#' @param upper A vector matching in length to
#'   \code{values} with the upper limits for
#'   the error bars.
#' @param point_type An integer vector specifying the
#'   type of point to draw (see [graphics::points]).
#' @param point_color A character vector specifying
#'   the color(s) for the points.
#' @param point_background A character vector specifying
#'   the color(s) for the point backgrounds.
#' @param text_size A numeric vector of three elements
#'   specifying the size of the points to draw, the
#'   size of the x and y-axis labels, and the size
#'   of the x-axis title, respectively. If less than
#'   three elements are given, elements are recycled
#'   to produce the necessary length.
#' @param line_width A numeric vector of three elements,
#'   the line widths for the error bars, the horizontal
#'   grid lines, and the vertical grid lines, respectively.
#' @param line_color A character vector specifying
#'   the color(s) for the error bars.
#' @param margin A numeric vector specifying the margin
#'   sizes (in inches) for the bottom, left, top, and right-hand sides
#'   of the figure.
#' @param labels_y A character vector, the labels for the y-axis.
#' @param labels_x A numeric vector giving the x-axis
#'   labels. If \code{NULL} the function attempts to
#'   automatically create the labels.
#' @param labels_estimates A character vector, the labels to
#'   add next to each estimate within the plot.
#' @param labels_estimates_pos An integer value indicating
#'   the side to place labels for estimates, where 1 = bottom,
#'   2 = left, 3 = top, and 4 = right.
#' @param labels_estimates_limit A numeric value specifying
#'   the highest or lowest value an estimate label can be placed
#'   when placing to the left or right, respectively.
#' @param labels_position A numeric vector of three
#'   elements, the position at which to draw the
#'   y-axis labels, the x-axis labels, and the x-axis
#'   title, respectively.
#' @param title_x A character string, the x-axis title.
#' @param xlim A numeric vector with the lower and
#'   upper limits for the x-axis plotting boundary.
#'   If \code{NULL} the function computes it using
#'   [base::range].
#' @param vert_grid A numeric vector giving the x-axis
#'   positions for vertical grid lines. If \code{NULL}
#'   no grid lines are drawn.
#' @param vert_color A character vector specifying
#'   the color(s) for the vertical grid lines.
#' @param horiz_grid Logical; if \code{TRUE} horizontal
#'   grid lines are added to separate the different
#'   estimates visually.
#' @param horiz_color  A character vector specifying
#'   the color for the horizontal grid lines.
#' @param border An integer vector specifying the sides
#'   to add a border to (1 = bottom, 2 = left, 3 = top, and 4 = right).
#' @param new Logical; if \code{TRUE} a new plotting window
#'   is generated.
#' @param w An integer value, the width of a new plotting window.
#' @param h An integer value, the height of a new plotting window.
#'
#' @returns A forest plot.
#'
#' @examples
#' # Example data set
#' data("ChickWeight")
#' dtf <- ChickWeight[ ChickWeight$Time == 21, ]
#' # Compute uncertainty intervals around means
#' x <- stats_by_group( dtf, 'weight', 'Diet', c( 'N', 'M', 'SE', 'UI' ) )
#'
#' # Basic forest plot
#' plot_forest(
#'   x[, c( 'M', 'UI_LB', 'UI_UB') ], labels_y = 'Diet ' %p% 1:4,
#'   new = FALSE
#' )
#'
#' # Nicely formatted forest plot
#'
#' # T-test against overall mean
#' x$P_value <- pt(
#'   abs( x$M - mean(x$M) ) / x$SE, x$N - 1, lower.tail = FALSE
#' )*2
#' signifcant <- x$P_value < .05
#' # Summary of results
#' results <-
#'   round( x$M ) %p% ' [' %p% round( x$UI_LB ) %p% ', ' %p%
#'   round( x$UI_UB ) %p% ']; p = ' %p%
#'   format( round( x$P_value, 3 ), nsmall = 3 )
#'
#' plot_forest(
#'   x[, c( 'M', 'UI_LB', 'UI_UB') ],
#'   xlim = c( 140, 340 ), labels_x = seq( 140, 340, 40 ),
#'   labels_y = 'Diet ' %p% 1:4, labels_estimates = results,
#'   labels_estimates_limit = 230,
#'   new = FALSE, horiz_grid = TRUE, vert_grid = mean( x$M ),
#'   point_type = replace_cases( signifcant, c( T, F ), c( 21, 19 ) ),
#'   margin = c( .5, .5, .2, 1.75 ), text_size = c( 1.25, .8 )
#' )
#'
#' @export

plot_forest <- function(x,
                        lower = NULL,
                        upper = NULL,
                        point_type = 19,
                        point_color = 'black',
                        point_background = 'white',
                        text_size = 1,
                        line_width = 2,
                        line_color = 'black',
                        margin = NULL,
                        labels_y = NULL,
                        labels_x = NULL,
                        labels_estimates = NULL,
                        labels_estimates_pos = 4,
                        labels_estimates_limit = NULL,
                        labels_position = -1.25,
                        title_x = NULL,
                        xlim = NULL,
                        vert_grid = 0,
                        vert_color = 'black',
                        vert_type = 1,
                        horiz_grid = FALSE,
                        horiz_color = 'grey80',
                        border = c( 1, 2 ),
                        new = FALSE,
                        w = 5,
                        h = 5 ) {

  current_use <- "Incorrect inputs"

  # If a data frame or matrix is provided
  if ( is.data.frame(x) | is.matrix(x) ) {

    # Has three columns
    if ( ncol(x) >= 3 ) {

      current_use <- "'x' is a data frame or matrix"

      # Close 'Has three columns'
    }

    # Close 'If a data frame or matrix is provided'
  } else {

    # If lower and upper limits provided
    if ( !is.null(lower) & !is.null(upper) ) {

      # If inputs match in length
      if ( all( c( length(lower), length(upper) ) %in% length(x) ) ) {

        current_use <- "'x', 'lower', 'upper' are vectors of equal length"

        # Close 'If inputs match in length'
      }

      # Close 'If lower and upper limits provided'
    }

    # Close else for 'If a data frame or matrix is provided'
  }

  # Error message
  if ( current_use == 'Incorrect inputs' ) {

    stop(
      '\n' %p% current_use %p% ' - expected that either...\n' %p%
        "   (1) variable 'x' is a data frame or matrix with 3 columns\n" %p%
        "   (2) variable 'x', 'lower', and 'upper' " %p%
          "are vectors of equal length\n"
    )

    # Close 'Error message'
  }

  # If data frame or matrix provided
  if ( is.data.frame(x) | is.matrix(x) ) {

    values <- x[, 1]
    lower <- x[, 2]
    upper <- x[, 3]

    # Close 'If data frame or matrix provided'
  } else {

    values <- x

    # Close else for 'If data frame or matrix provided'
  }

  # Create new plotting window
  if ( new ) {

    x11( width = w, height = h )

    # Close 'Create new plotting window'
  }

  # Number of data points
  n <- length(values)
  y <- n:1 # 1st data point starts at top

  # Figure limits
  ylim <- c( .5, n + .5 )

  # If no limits for x-axis
  if ( is.null(xlim) ) {

    xlim <- range( c(lower, upper) )
    xlim <- xlim + c( -.05, .05)*diff(xlim)

    # Close 'If no limits for x-axis'
  }

  # Ensure indexing for aesthetics works
  line_width <- rep_len( line_width, 3 )
  text_size <- rep_len( text_size, 3 )
  labels_position <- rep_len( labels_position, 3 )

  if ( is.null( margin ) ) {

    margin <- c( .25, .25, .25, .25 )

    if ( !is.null(labels_y) ) {

      par( mai = margin )
      arfpam::plot_blank( xlim, ylim )

      sn <- sapply(
        labels_y, nchar
      )

      inner_size <- diff( par("usr")[1:2] )

      plot_size_in <- dev.size("in")
      plot_margin_in <- par( "mai" )
      inner_size_in <- plot_size_in[1] - sum( plot_margin_in[c(2,4)] )
      sw <- strwidth( labels_y[ sn == max(sn) ][1] )
      units_per_in <- inner_size / inner_size_in

      margin <- c(
        .5, (sw / units_per_in)*1.1, .2, .2
      )

    }

  }

  # Create blank plot
  par( mai = margin )
  arfpam::plot_blank( xlim, ylim )

  # Add horizontal grid lines
  if ( horiz_grid ) {

    arfpam::draw_hv(
      h = seq( ylim[1], ylim[2], 1 ), l = xlim,
      lwd = line_width[2],
      col = horiz_color
    )

    # Close 'Add vertical grid lines'
  }

  # Add vertical grid lines
  if ( !is.null( vert_grid ) ) {

    arfpam::draw_hv(
      v = vert_grid, l = ylim,
      lwd = line_width[2],
      col = vert_color,
      lty = vert_type
    )

    # Close 'Add vertical grid lines'
  }

  # Add error bars
  segments(
    lower, y,
    upper, y,
    lwd = line_width[1],
    col = line_color
  )

  if ( !is.null( labels_estimates ) ) {

    if ( labels_estimates_pos %in% c( 1, 3 ) ) {

      text(
        values, y,
        labels_estimates,
        pos = labels_estimates_pos,
        cex = text_size[2], xpd = NA
      )

    }

    if ( labels_estimates_pos %in% c( 2, 4 ) ) {

      if ( labels_estimates_pos == 2 ) {

        if ( !is.null( labels_estimates_limit ) ) {

          adj_lower <- sapply(
            lower, function(l) {
              min( l, labels_estimates_limit )
            }
          )

        } else {
          adj_lower <- lower
        }

        text(
          adj_lower, y,
          labels_estimates,
          pos = labels_estimates_pos,
          cex = text_size[2], xpd = NA
        )

      } else {

        if ( !is.null( labels_estimates_limit ) ) {

          adj_upper <- sapply(
            upper, function(u) {
              max( u, labels_estimates_limit )
            }
          )

        } else {

          adj_upper <- upper

        }

        text(
          adj_upper, y,
          labels_estimates,
          pos = labels_estimates_pos,
          cex = text_size[2], xpd = NA
        )

      }

    }

  }

  # Add estimates
  points(
    values, y,
    pch = point_type,
    col = point_color,
    bg = point_background,
    cex = text_size[1]
  )

  if ( 1 %in% border )
    arfpam::draw_hv( h = ylim[1], l = xlim, lwd = line_width[3] )
  if ( 2 %in% border )
    arfpam::draw_hv( v = xlim[1], l = ylim, lwd = line_width[3] )
  if ( 3 %in% border )
    arfpam::draw_hv( h = ylim[2], l = xlim, lwd = line_width[3] )
  if ( 4 %in% border )
    arfpam::draw_hv( v = xlim[2], l = ylim, lwd = line_width[3] )


  # If y-axis labels provided
  if ( !is.null(labels_y) ) {

    # Loop over values
    for ( r in seq_along(y) ) {

      arfpam::draw_axes(
        y[r], labels_y[r],
        side = 2, line = labels_position[1],
        xpd = NA, las = 1, cex = text_size[2]
      )

      # Close 'Loop over values'
    }

    # Close 'If y-axis labels provided'
  }

  # If no values provided for x-axis ticks
  if ( is.null(labels_x) ) {

    labels_x <- arfpam::lin( xlim[1], xlim[2], 6 )

    d <- which( sapply( 0:5, function(d)
      length( unique( round( labels_x, d ) ) ) ) == 6 ) |> min()
    labels_x <- round( labels_x, d )

    # Close 'If no values provided for x-axis ticks'
  }

  arfpam::draw_axes(
    labels_x, side = 1, line = labels_position[2], cex = text_size[2]
  )

  # If x-axis label overlaps with ticks
  if ( labels_position[3] == labels_position[2] ) {

    labels_position[3] <- labels_position[2] + 2.25

    # Close 'If x-axis label overlaps with ticks'
  }
  mtext(
    title_x, side = 1, line = labels_position[3], cex = text_size[3],
    xpd = NA
  )

}

#### 3.4) plot_histogram ####
#' Wrapper for Plotting Histograms
#'
#' A convenience function making a call to
#' \code{\link[graphics]{hist}} with changes to
#' the default options for parameters for pretty
#' plotting.
#'
#' @param x A vector of numeric values.
#' @param breaks The argument controlling breakpoints
#'   passed to the \code{\link[graphics]{hist}} function -
#'   by default uses the Freedman-Diaconis algorithm to
#'   find the optimal number of breakpoints.
#' @param border The color of the border around the bars.
#' @param col The color used to fill the bars.
#' @param main The main title for the figure.
#' @param plot Logical; if \code{TRUE} generates a figure.
#' @param output Logical; if \code{TRUE} returns the
#'   output from the \code{\link[graphics]{hist}} function.
#'   Set to \code{TRUE} if \code{plot} is \code{FALSE}.
#' @param new Logical; if \code{TRUE} generates a new
#'   plotting window via a call to \code{\link[grDevices]{x11}}.
#' @param w The width (in inches) of the new plotting
#'   window.
#' @param h The height (in inches) of the new plotting
#'   window.
#' @param raw_points Logical; if \code{TRUE} adds a bar for
#'   individual data points at the bottom of the figure.
#' @param ... Additional arguments to pass to the
#'   \code{\link[graphics]{hist}} function.
#'
#' @return If \code{output} is \code{TRUE}, returns a
#'   list with the information used to create the
#'   histogram - see the help page for the
#'   \code{\link[graphics]{hist}} function for more
#'   details.
#'
#' @examples
#' x <- rnorm( 100 )
#' plot_histogram( x, new = FALSE )
#'
#' @export

plot_histogram <- function( x,
                            breaks = 'FD',
                            border = 'grey',
                            col = 'grey',
                            main = '',
                            plot = TRUE,
                            output = FALSE,
                            new = TRUE,
                            w = 5, h = 5,
                            raw_points = TRUE,
                            ... ) {

  if ( new & plot ) x11( width = w, height = h )

  out <- hist(
    x, breaks = breaks, main = main,
    col = col, border = border,
    plot = plot, ...
  )

  if (plot & raw_points) {
    text( x, rep( 0, length(x) ),
          labels = rep( '|', length(x) ),
          xpd = NA, pos = 1, col = 'grey60' )
  }

  if ( !plot ) output <- TRUE

  if ( output ) {
    return( out )
  }

}

#### 3.5) plot_scatter ####
#' Create a Scatter Plot
#'
#' Function to create a scatter plot of two variables.
#'
#' @param x Either a data frame or a numeric vector.
#' @param y A numeric vector.
#' @param columns A character string of two elements,
#'   the columns to use if \code{x} is a data frame.
#' @param labels A character string of two elements,
#'   the x and y-axis labels, respectively.
#' @param xlim A numeric vector of two values, the
#'   lower and upper limit for the x-axis (in standard
#'   deviation units).
#' @param ylim A numeric vector of two values, the
#'   lower and upper limit for the y-axis (in standard
#'   deviation units).
#' @param new A logical value; if \code{TRUE} a new
#'   plotting window is generated.
#' @param w A numeric value, the width of the new
#'   plotting window.
#' @param h A numeric value, the height of the new
#'   plotting window.
#' @param cex A numeric vector of three values, the
#'   size of the text for the labels, results, and
#'   axis values, respectively.
#' @param line A numeric vector of three values, the
#'   line position for the labels, results, and
#'   axis values, respectively.
#' @param digits A integer vector of two values, the
#'   number of digits to round to for the x and
#'   y-axis values, respectively.
#'
#' @returns A scatter plot.
#'
#' @examples
#' # Example data
#' data("iris")
#' plot_scatter(
#'   iris, columns = c( 'Petal.Length', 'Sepal.Length' ),
#'   new = F, labels = c( 'Petal length', 'Sepal length' )
#' )
#'
#' @export

plot_scatter <- function( x,
                          y = NULL,
                          columns = NULL,
                          labels = c( 'X', 'Y' ),
                          xlim = NULL,
                          ylim = NULL,
                          new = TRUE,
                          w = 5,
                          h = 5,
                          cex = c( 1, 1, 1 ),
                          line = c( 1, -1, -1 ),
                          digits = c( 2, 2 ),
                          guidelines = TRUE ) {

  # Check use cases
  current_use <- "Incorrect input"

  # Data frame provided
  if ( is.data.frame(x) & is.null(y) ) {
    current_use <- "Variable 'x' is a data frame"
  }

  # Numeric vectors provided
  if ( is.numeric(x) & !is.null(y) ) {

    if ( is.numeric(y) ) {
      current_use <- "Variable 'x' is a value and 'y' is a numeric vector"
    }

    # Close 'Numeric vectors provided'
  }

  # If no correct inputs found
  if ( current_use == "Incorrect input" ) {

    stop(
      '\n' %p% current_use %p% ' - expected that either...\n' %p%
        "   (1) variable 'x' is a data frame\n" %p%
        "   (2) variable 'x' and 'y' are numeric vectors\n"
    )

    # Close 'If no correct inputs found'
  }

  # If data frame provided
  if ( current_use == "Variable 'x' is a data frame" ) {

    # If no columns specified
    if ( is.null( columns ) ) {

      # Take first two columns
      columns <- colnames( x )[1:2]

      # Close 'If no columns specified'
    }

    xv <- x[[ columns[1] ]]
    yv <- x[[ columns[2] ]]

    # Close 'If data frame provided'
  } else {

    xv <- x
    yv <- y

    # Close else for 'If data frame provided'
  }

  is_na <-
    is.na(xv) | is.na(yv)

  xv <- xv[ !is_na ]
  yv <- yv[ !is_na ]

  # Alert if missing values removed
  if ( any( is_na ) ) {

    warning(
      paste0( "There were ", sum(is_na),
              " cases removed with missing values" )
    )

    # Close 'Alert if missing values removed'
  }

  m.x <- mean( xv )
  s.x <- sd( xv )

  m.y <- mean( yv )
  s.y <- sd( yv )

  z.x <- (xv - m.x)/s.x
  z.y <- (yv - m.y)/s.y

  o <- order(z.x)
  z.x <- z.x[o]
  z.y <- z.y[o]

  if ( new ) x11( width = w, height = h )

  limits <- c(
    floor( min( c( z.x, z.y ) ) ),
    ceiling( max( c( z.x, z.y ) ) )
  )
  xl <- limits
  yl <- limits

  if ( !is.null( xlim ) ) xl <- xlim
  if ( !is.null( ylim ) ) yl <- ylim

  dtf <- data.frame(
    z.x = z.x,
    z.y = z.y
  )
  lmf <- lm( z.y ~ 0 + z.x, data = dtf )
  ndtf <- data.frame(
    z.x = seq( xl[1], xl[2], length.out = 100 )
  )
  z.y.hat <- predict( lmf, newdata = ndtf )
  sm <- summary( lmf )

  arfpam::plot_blank( xl, yl )

  # Plot guidelines
  if ( guidelines ) {

    segments(
      limits[1], limits[1],
      limits[2], limits[2],
      lwd = 2, col = 'grey80'
    )

    segments(
      limits[2], limits[1],
      limits[1], limits[2],
      lwd = 2, col = 'grey80'
    )

    segments(
      xl[1], yl[1] + diff(yl)*.5,
      xl[2], yl[1] + diff(yl)*.5,
      lwd = 2, col = 'grey80'
    )

    # Close 'Plot guidelines'
  }

  lines( ndtf$z.x, z.y.hat, lwd = 2, col = arfpam::palettes(2) )

  points( z.x, z.y, pch = 19 )

  arfpam::draw_borders_and_labels(
    xl, yl, labels = labels, sides = 1:4,
    cex = cex[1], lines = line[1]
  )

  results <- paste0(
    'R = ', round( sm$coefficients[1, 1], 2 ),
    '; p = ', format( round( sm$coefficients[1, 4], 3 ), nsmall = 3 ),
    ' (N = ', length(z.x), ')'
  )
  results <- gsub( 'p = 0.000', 'p < 0.001', results, fixed = TRUE )

  arfpam::draw_axes(
    xl[1] + diff(xl)*.5,
    results,
    side = 3, line = line[2], cex = cex[2]
  )

  z.xa <- seq( xl[1], xl[2], length.out = 5 )
  z.ya <- seq( yl[1], yl[2], length.out = 5 )

  xa <- round( z.xa*s.x + m.x, digits[1] )
  ya <- round( z.ya*s.y + m.y, digits[2] )

  z.xa <- ( xa - m.x)/s.x
  z.ya <- ( ya - m.y)/s.y

  arfpam::draw_axes(
    z.xa, xa,
    side = 1, line = line[3], cex = cex[3]
  )

  arfpam::draw_axes(
    z.ya, ya,
    side = 2, line = line[3], cex = cex[3]
  )

}

#### 4) Functions to save figures as files ####

#### 4.1) save_png ####
#' Save PNG file
#'
#' Function that generates a figure and saves
#' as a PNG file using common settings.
#'
#' @param file_name A character string, the
#'   desired path to the PNG file.
#' @param fun_plot A function that generates
#'   a figure as output.
#' @param dmn A numeric vector of two values,
#'   the width and height of the figure
#'   to save (default is in inches).
#' @param units A character string, the
#'   units for figure dimensions (see
#'   [grDevices::png]).
#' @param res An integer value, the
#'   figure resolution (see
#'   [grDevices::png]).
#' @param return_file_name A logical value,
#'   if \code{TRUE} returns the file name
#'   as a character string.
#' @param ... Additional arguments for the
#'   \code{fun_plot} function.
#'
#' @returns A PNG file, and optionally the
#'   file name.
#'
#' @export

save_png <- function(
    file_name,
    fun_plot,
    dmn = c( 5, 5 ),
    units = 'in', res = 300,
    return_file_name = FALSE,
    ... ) {

  png(
    file_name,
    width = dmn[1], height = dmn[2],
    units = units, res = res
  )

  fun_plot( ... )

  dev.off()

  if ( return_file_name ) return( file_name )
}

#### 4.2) save_pdf ####
#' Save PDF file
#'
#' Function that generates a figure and saves
#' as a PDF file using common settings.
#'
#' @param file_name A character string, the
#'   desired path to the PNG file.
#' @param fun_plot A function that generates
#'   a figure as output.
#' @param dmn A numeric vector of two values,
#'   the width and height of the figure
#'   to save (default is in inches).
#' @param return_file_name A logical value,
#'   if \code{TRUE} returns the file name
#'   as a character string.
#' @param ... Additional arguments for the
#'   \code{fun_plot} function.
#'
#' @returns A PDF file, and optionally the
#'   file name.
#'
#' @export

save_pdf <- function(
    file_name,
    fun_plot,
    dmn = c( 5, 5 ),
    return_file_name = FALSE,
    ... ) {

  pdf(
    file_name,
    width = dmn[1], height = dmn[2]
  )

  fun_plot( ... )

  dev.off()

  if ( return_file_name ) return( file_name )
}


