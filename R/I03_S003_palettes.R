
palettes <- function( type, index = NULL ) {

  types <- list(
    colorblind = c(
      'Colorblind', 'colorblind',
      'CB', 'cb',
      '1'
    )
  )

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

    if ( is.null( index ) ) {
      index <- 1:length( list_of_colors )
    }

    return( list_of_colors[ index ] )
  }

}

