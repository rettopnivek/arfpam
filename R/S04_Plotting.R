# Plotting
# Written by Kevin Potter
# email: kevin.w.potter@gmail.com
# Please email me directly if you
# have any questions or comments
# Last updated 2021-05-06

# Table of contents
# 1) blank_plot
# 2) palettes
# 3) hv_line
# 4) fill_plot
# 5) add_axes

# TO DO
# - Add additional functions
#   (error_bars, etc.)
# - Add additional color palettes
# - Fix issue where website does not draw figures

###
### 1)
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
#' blank_plot( margins = TRUE )
#'
#' @export

blank_plot = function( xDim = c(0,1), yDim = c(0,1),
                      margins = FALSE, cex = 1 ) {

  plot( xDim, yDim, type = 'n', ylab = ' ', xlab = ' ',
        xaxt = 'n', yaxt = 'n', bty = 'n' )


  if ( margins ) {

    # User specified boundaries for
    # plotting window
    segments( xDim[c(1,2,1,1)],
              yDim[c(1,1,1,2)],
              xDim[c(1,2,2,2)],
              yDim[c(2,2,1,2)],
              col = 'black',
              lwd = 2 )

    # Adjusted boundaries for
    # plotting window

    bnd <- par( "usr" )

    segments( bnd[c(1,2,1,1)],
              bnd[c(3,3,3,4)],
              bnd[c(1,2,2,2)],
              bnd[c(4,4,3,4)],
              col = 'grey90',
              lwd = 2 )

    mrg <- par( "mar" )
    out <- sapply( 1:4, function(s) {
      for ( i in -1:floor( mrg[s] ) ) {
        pst = xDim[1] + diff( xDim )/2
        if ( s %in% c( 2, 4 ) ) {
          pst = yDim[1] + diff( yDim )/2
        }
        axis( s, pst, i, line = i, tick = F, cex.axis = cex )
      }
    } )

  }

}

###
### 2)
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
#' palettes( 'Colorblind', plot = TRUE )
#' palettes( 'Grayscale', plot = TRUE )
#'
#' # Example of taking subset of colors
#' palettes( 'Colorblind', 1:2 )
#'
#' @export

palettes <- function( type = 'colorblind', index = NULL, plot = FALSE ) {

  types <- list(
    colorblind = c(
      'Colorblind', 'colorblind',
      'Colourblind', 'colourblind',
      'CB', 'cb',
      '1'
    ),
    grayscale = c(
      'Grayscale', 'grayscale',
      'Greyscale', 'greyscale',
      'Gray', 'gray',
      'Grey', 'grey',
      '2'
    )
  )

  if ( is.null( type ) ) {

    for ( i in 1:length( types ) ) {

      message( paste0( names( types[i] ) ) )
      message( paste0(
        '\t', types[[i]], '\n'
      ) )

    }

  }

  list_of_colors <- NULL

  if ( type %in% types$colorblind ) {

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

  if ( type %in% types$grayscale ) {

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

  if ( !is.null( list_of_colors ) ) {

    if ( is.null( index ) ) {
      index <- 1:length( list_of_colors )
    }

    if ( plot ) {

      n <- length( list_of_colors )

      par( mar = c( 5.1, 5.1, .5, .5 ) )
      plot( 1:n, 1:n,
            xlab = '', ylab = '',
            xaxt = 'n', yaxt = 'n',
            bty = 'n',
            pch = 15, cex = 4,
            col = list_of_colors )
      axis( 1, 1:n, tick = F, cex = 1.25 )
      axis( 2, 1:n, names( list_of_colors ),
            tick = F, cex = 1.25, las = 1 )
      par( mar = c( 5.1, 4.1, 4.1, 2.1 ) )

    }

    return( list_of_colors[ index ] )

  } else {

    stop(
      paste0(
        'Specified palette not found - options are...\n',
        paste(
          sapply( 1:length( types ), function(i) {
            types[[i]][1]
          } ), collapse = '\n'
        )
      )
    )

  }

}

###
### 3)
###

#' Draw Horizontal/Vertical Lines
#'
#' Draws horizontal or vertical lines on an
#' existing figure.
#'
#' @param y A vector with the y-axis positions
#'   for horizontal lines.
#' @param x A vector with the x-axis positions
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
#' hv_line( y = .5, lty = 2 )
#' # Draw vertical line
#' hv_line( x = .5, lwd = 2 )
#'
#' # Control width of horizontal line
#' hv_line( y = .25, l = c( .25, .75 ), col = 'blue' )
#' # Control height of vertical line
#' hv_line( x = .25, l = c( .25, .75 ), col = 'orange' )
#'
#' @export

hv_line <- function( y = NULL, x = NULL, l = NULL, ... ) {

  if ( !is.null( x ) ) {

    n = length( x )

    if ( is.null( l ) ) {

      l = par( 'usr' )[3:4]

    }

    segments( x, rep( l[1], n ),
              x, rep( l[2], n ), ... )

  }

  if ( !is.null( y ) ) {

    n = length( y )

    if ( is.null( l ) ) {

      l = par( 'usr' )[1:2]

    }

    segments( rep( l[1], n ), y,
              rep( l[2], n ), y, ... )

  }

}

###
### 4)
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
#' xl = c(0, 4); yl = c(0,1);
#' blank_plot(xl, yl)
#' # Draw vertical grey bar
#' fill_plot(x = c(1,2), l = yl, col = 'grey90')
#' # Also set height and border of bar
#' fill_plot(x = c(3,4), l = c(.25,.75), col = 'grey80', border = 'black' )
#' # Draw horizontal bar
#' fill_plot(y = c(.9, 1), col = 'black' )
#'
#' # Multiple bars can be draw at once
#' xl = c(0, 4); yl = c(0,1);
#' blank_plot(xl, yl)
#' # Multiple vertical bars
#' fill_plot( x = rbind( c(1,2), c(3,4) ), l = yl, col = 'grey90')
#' # Multiple horizontal bars
#' fill_plot( y = rbind( c(0,.05), c(.95,1) ), l = xl, col = 'black')
#'
#' @export

fill_plot <- function( x = NULL, y = NULL, l = NULL,
                       border = NA, ... ) {

  if ( !is.null(x) ) {

    if ( is.vector( x ) &
         length( x ) == 2 ) {

      xv = rbind( x[ c( 1, 1, 2, 2 ) ] )

    }

    if ( is.matrix( x ) ) {
      if ( ncol(x) == 2 ) {

      xv = cbind( x[,1], x[,1], x[,2], x[,2] )
      }
    }

    if ( is.null( l ) ) {

      l = par( 'usr' )[3:4]

    }
    yv = rbind( l[ c( 1, 2, 2, 1 ) ] )
    if ( nrow( xv ) > nrow( yv ) ) {
      yv = matrix( yv, nrow( xv ), 4, byrow = TRUE )
    }

  }

  if ( !is.null(y) ) {

    if ( is.vector( y ) &
         length( y ) == 2 ) {

      yv = rbind( y[ c( 1, 1, 2, 2 ) ] )

    }

    if ( is.matrix( y ) ) {

      if ( ncol( y ) == 2 ) {

      yv = cbind( y[,1], y[,2], y[,2], y[,1] )

      }
    }

    if ( is.null( l ) ) {

      l = par( 'usr' )[1:2]

    }
    xv = rbind( l[ c( 1, 1, 2, 2 ) ] )
    if ( nrow( yv ) > nrow( xv ) ) {
      xv = matrix( xv, nrow( yv ), 4, byrow = TRUE )
    }

  }

  if ( !is.null(x) | !is.null(y) ) {

    for ( i in 1:nrow( xv ) ) {
      polygon( xv[i,], yv[i,], border = border, ... )
    }

  }

}

###
### 5)
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
#'   current plotting region (use \code{\link[MASS:text]{par()$usr}}
#'   to get x and y-axis coordinates for plot region).
#' @param cex Size of the text.
#' @param degrees Number of degrees to rotate text
#'   (note results in a call to \code{\link[MASS:text]{text()}}
#'   rather than \code{\link[MASS:axis]{axis()}}).
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
#' hv_line( x = 0 ); hv_line( y = 0 )
#'
#' # Add axes
#' add_axes(c(0, .5, 1), c('Start', 'Middle', 'End') )
#' add_axes(c(0, .5, 1), c('Bottom', 'Middle', 'Top'), side = 2 )
#' add_axes( .5, 'Title', side = 3, cex = 2 )
#'
#' # Create blank plot with custom margins
#' par( mar = rep( 4, 4 ) )
#' blank_plot()
#' # Draw boundaries
#' hv_line( x = 0:1 ); hv_line( y = 0:1 )
#'
#' # Add rotated axes
#' add_axes(c(0, .5, 1), c('Start', 'Middle', 'End'),
#'          degrees = 45 )
#' add_axes(c(0, .5, 1), c('Start', 'Middle', 'End'),
#'          degrees = 45, side = 3 )
#' add_axes(c(0, .5, 1), c('Bottom', 'Middle', 'Top'),
#'          degrees = 45, side = 2 )
#' add_axes(c(0, .5, 1), c('Bottom', 'Middle', 'Top'),
#'          degrees = 45, side = 4 )
#' @export

add_axes <- function( at, labels = NULL,
                      side = 1, tick = F,
                      line = NULL, cex = 1.25,
                      degrees = NULL, xpd = NA,
                      adj = NULL, ... ) {

  #< Check if rotated axes should be drawn
  if ( is.null( degrees ) ) {

    # Default line position to draw axes at
    if ( is.null( line ) ) line = -.5

    axis( at, labels,
          side = side, tick = tick,
          line = line, cex.axis = cex, xpd = xpd,
          ... )

    #> Close conditional on no rotated axes
  } else {

    #<| By default labels are values to draw axes at
    if ( is.null( labels ) ) {

      labels <- as.character( at )

      #|> Close conditional
    }

    #<| Whether rotated axes should be right or left-aligned
    if ( is.null( adj ) ) {

      if ( side %in% c(1,3) ) adj = 1 # Right-aligned
      if ( side %in% c(2,4) ) adj = 0 # Left-aligned

      #|> Close conditional
    }

    # Get plot dimensions
    x_limits <- par()$usr[1:2]
    y_limits <- par()$usr[3:4]

    # Determine size of text
    txt_height <- strheight( labels[1], cex = cex )

    #<| Rotated axes at the bottom
    if ( side == 1 ) {

      #<|< Default line position to draw axes
      if ( is.null( line ) ) {

        line <- y_limits[1] - txt_height/2

        #>|> Close conditional
      }

      text( x = at, y = rep( line, length( at ) ),
            labels = labels, srt = degrees, xpd = xpd,
            cex = cex, adj = adj, ... )

      #|> Close conditional
    }

    #<| Rotated axes at the top
    if ( side == 3 ) {

      #<|< Default line position to draw axes
      if ( is.null( line ) ) {

        line <- y_limits[2] + txt_height/2

        #>|> Close conditional
      }

      text( x = at, y = rep( line, length( at ) ),
            labels = labels, srt = degrees, xpd = xpd,
            cex = cex, adj = adj, ... )

      #|> Close conditional
    }

    #<| Rotated axes at the left
    if ( side == 2 ) {

      #<|< Default line position to draw axes
      if ( is.null( line ) ) {

        line <- x_limits[1] - txt_height/2

        #>|> Close conditional
      }

      text( x = rep( line, length( at ) ), y = at,
            labels = labels, srt = degrees, xpd = xpd,
            cex = cex, adj = adj, ... )

      #|> Close conditional
    }

    #<| Rotated axes at the right
    if ( side == 4 ) {

      #<|< Default line position to draw axes
      if ( is.null( line ) ) {

        line <- x_limits[2] + txt_height/2

        #>|> Close conditional
      }

      text( x = rep( line, length( at ) ), y = at,
            labels = labels, srt = degrees, xpd = xpd,
            cex = cex, adj = adj, ... )

      #|> Close conditional
    }

    #> Close conditional on rotated axes
  }

}

