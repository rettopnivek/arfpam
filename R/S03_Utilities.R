# Utilities
# Written by Kevin Potter
# email: kevin.w.potter@gmail.com
# Please email me directly if you
# have any questions or comments
# Last updated 2021-03-30

# Table of contents
# 1) over
# 2) templates
# 3) palettes
# 4) toc

###
### 1)
###

#' A Range Adjusted by an Iterator
#'
#' A function that takes a range and scales it
#' by an iterator. Useful when extracting a
#' set of multiple values within a loop.
#'
#' @param x A range (converted to integers).
#' @param iter An iterator value, typically the
#'   variable defined within a loop.
#' @param per An optional value specifying the
#'   increment by which to adjust the range
#'   (defaults to the maximum value of \code{x}).
#' @param adj An optional value specifying
#'   any adjustments to the iterator.
#'
#' @return A vector of values.
#'
#' @examples
#' # Pull 3 values per iteration
#' y <- 1:9
#' for ( i in 1:3 ) {
#'   print( y[ over( 1:3, i ) ] )
#' }
#'
#' # Pull first 2 of each 3 values per iteration
#' # using the 'per' argument
#' for ( i in 1:3 ) {
#'   print( y[ over( 1:2, i, per = 3 ) ] )
#' }
#'
#' # Pull last of 3 values per iteration
#' # using the 'per' argument
#' for ( i in 1:3 ) {
#'   print( y[ over( 3, i, per = 3 ) ] )
#' }
#'
#' # Pull 2 values for final 2 sets using the
#' # 'adj' argument
#' y <- 1:8
#' for( i in 1:2 ) {
#'   print( y[ over( 1:2, i, adj = 1 ) ] )
#' }
#'
#' @export

over <- function( x, iter,
                  per = NULL,
                  adj = -1 ) {

  # Ensure inputs are integers
  x <- as.integer( round( x ) )

  if ( is.null( per ) ) {
    per = max( x )
  }

  return( x + per * (iter + adj) )
}

###
### 2)
###

#' Create Templates for Annotations and Code
#'
#' ...
#'
#' @param type ...
#' @param author ...
#' @param email ...
#'
#' @return ...
#'
#' @examples
#' # Forthcoming
#'
#' @export

templates <- function( type,
                       author = 'Kevin Potter',
                       email = 'kevin.w.potter@gmail.com' ) {

  types <- list(
    function_documentation = c(
      'Function', 'function',
      'Func', 'func', 'Fun', 'fun',
      'FD', 'fd',
      'Arguments', 'arguments',
      'Arg', 'arg',
      '1'
    ),
    script_header = c(
      'Header', 'header',
      'Head', 'head',
      'Script', 'script',
      'SD', 'sd',
      '2'
    ),
    progress_bar = c(
      'Progress', 'progress',
      'Prog', 'prog',
      '3'
    ),
    code_segment = c(
      'Segment', 'segment',
      'Code', 'code',
      '4'
    ),
    plot_function = c()
  )

  if ( type %in% types$function_documentation ) {

    string <- paste0(
      '# Purpose: \n',
      '# ... \n',
      '# Arguments: \n',
      '# ... \n',
      '# Details: \n',
      '# ... \n',
      '# Returns: \n',
      '# ... \n'
    )

    message( string )

  }

  if ( type %in% types$script_header  ) {

    string <- paste0(
      '# Title\n',
      '# Written by ', author, '\n',
      '# email: ', email, '\n',
      '# Please email me directly if you \n',
      '# have any questions or comments\n',
      '# Last updated ', Sys.Date(), '\n',
      '\n',
      '# Table of contents\n',
      '# 1)'
    )

    message( string )

  }

  if ( type %in% types$progress_bar ) {

    string <- paste0(
      'n_cases <- 10\n',
      '# Create a progress bar using a base R function\n',
      'pb <- txtProgressBar( min = 1, max = n_cases, style = 3 )\n',
      '\n',
      '# Loop over cases\n',
      'for (i in 1:n_cases) {\n',
      '  # Update the progress bar\n',
      '  setTxtProgressBar(pb,i)\n',
      '}\n',
      'close(pb); rm(pb)\n'
    )

    message( string )

  }

  if ( type %in% types$code_segment ) {

    string <- paste0(
      '###\n',
      '### ?) Section label\n',
      '###\n',
      '\n',
      'if (run_code[1]) {\n',
      '\n',
      '}\n'
    )

    message( string )

  }

}

###
### 3)
###

#' Various Color Palettes
#'
#' ...
#'
#' @param type ...
#' @param index ...
#'
#' @return ...
#'
#' @examples
#' # Forthcoming
#'
#' @export

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

###
### 4)
###

#' Forthcoming
#'
#' ...
#'
#' @param type ...
#' @param index ...
#'
#' @return ...
#'
#' @examples
#' # Forthcoming
#'
#' @export

toc <- function( x, run = T ) {

  n_periods <- sum(
    grepl(
      '.',
      strsplit( x, split = '', fixed = T )[[1]],
      fixed = T
    )
  )

  indent <- ''
  if ( n_periods > 0 ) {
    indent <- paste( rep( '  ', n_periods ), collapse = '' )
  }

  message( paste0( indent, x ) )
}

