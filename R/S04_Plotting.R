# Plotting
# Written by Kevin Potter
# email: kevin.w.potter@gmail.com
# Please email me directly if you
# have any questions or comments
# Last updated 2021-04-07

# Table of contents
# 1) blankPlot

# TO DO
# - Add unit tests for every
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
#' blankPlot( margins = TRUE )
#'
#' @export

blankPlot = function( xDim = c(0,1), yDim = c(0,1),
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
