# Plotting
# Written by Kevin Potter
# email: kevin.w.potter@gmail.com
# Please email me directly if you
# have any questions or comments
# Last updated 2021-04-13

# Table of contents
# 1) blank_plot
# 2) palettes

# TO DO
# - Add section for website

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
#'
#' @return An empty plot.
#'
#' @examples
#' # With margin info
#' blank_plot( margins = TRUE )
#'
#' @export

blank_plot = function( xDim = c(0,1), yDim = c(0,1),
                      margins = FALSE ) {

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

    bnd = par( "usr" )

    segments( bnd[c(1,2,1,1)],
              bnd[c(3,3,3,4)],
              bnd[c(1,2,2,2)],
              bnd[c(4,4,3,4)],
              col = 'grey90',
              lwd = 2 )

    mrg = par( "mar" )
    sapply( 1:4, function(s) {
      mtext( -1:floor( mrg[s] ), line = -1:floor( mrg[s] ),
             side = s, cex = .9 )
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

palettes <- function( type = NULL, index = NULL, plot = FALSE ) {

  types <- list(
    colorblind = c(
      'Colorblind', 'colorblind',
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

