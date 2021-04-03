# Utilities
# Written by Kevin Potter
# email: kevin.w.potter@gmail.com
# Please email me directly if you
# have any questions or comments
# Last updated 2021-04-03

# Table of contents
# 1) over
# 2) templates
# 3) palettes
# 4) section
# 5) every
#   5.1) every
#   5.2) `every<-`
# 6) findNA
# 7) printTable

# TO DO
# - Add unit tests for every
# - Add section for website

###
### 1)
###

#' A Sequence Adjusted by an Iterator
#'
#' A function that takes a sequence of values
#' and scales it by an iterator. Useful when
#' extracting a set of multiple values within
#' a loop.
#'
#' @param x A sequence of values (converted to integers).
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
#' Outputs a template of common annotations
#' and code segments to the console for
#' easy copying and pasting.
#'
#' @param type The type of template to return,
#'   with options for...
#'   \itemize{
#'     \item 'Function (function documentation);
#'     \item 'Header' (header for a R script);
#'     \item 'Progress' (progress bar for a loop);
#'     \item 'Segment' (code segment to conditionally run).
#'   }
#'
#' @examples
#' # List of possible inputs to argument
#' # 'type' for each template
#' templates()
#'
#' # Function documentation
#' templates( 'Function' )
#'
#' # Header for R script
#' templates( 'Header' )
#'
#' # Progress bar for loop
#' templates( 'Progress' )
#'
#' # Code segment in a script
#' templates( 'Segment' )
#'
#' @export

templates <- function( type ) {

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
      'Section', 'section',
      'Code', 'code',
      '4'
    ),
    plot_function = c()
  )

  if ( is.null( type ) ) {

    for ( i in 1:length( types ) ) {

      message( paste0( names( types[i] ) ) )
      message( paste0(
        '\t', types[[i]], '\n'
      ) )

    }

  }

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

    author = getOption( 'arfpam.author' )
    email = getOption( 'arfpam.email' )

    if ( is.null( author ) )
      author = 'Kevin Potter'
    if ( is.null( email ) )
      email = 'kevin.w.potter@gmail.com'

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
      "  section( '?' )\n",
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

###
### 4)
###

#' Section Numbers for Tracking Progress
#'
#' Output section numbers to the console via
#' a call to \code{\link[base]{message}}.
#' Useful for tracking progress when running
#' a script and for debugging where errors
#' occur.
#'
#' @param x A character string, such as
#'   '1)' or '2.3)' or of a similar form.
#' @param run Logical; if TRUE, a message is
#'   generated.
#' @param spacing A character string, the
#'   character to use to determine the
#'   number of indents to use to
#'   identify section hierarchies.
#' @param end A character string, the
#'   final symbol to attach to the end of
#'   a section header.
#'
#' @examples
#'
#' section( '1' )
#' # Periods indent output by 2 spaces
#' section( '1.1' )
#' section( '1.1.1' )
#'
#' @export

section <- function( x, run = TRUE,
                     spacing = '.',
                     end = ')' ) {

  n_spacers <- sum(
    grepl(
      spacing,
      strsplit( x, split = '', fixed = TRUE )[[1]],
      fixed = TRUE
    )
  )

  if ( grepl( ')', x, fixed = TRUE ) ) end <- ''

  indent <- ''
  if ( n_spacers > 0 ) {
    indent <- paste( rep( '  ', n_spacers ), collapse = '' )
  }

  message( paste0( indent, x, end ) )
}

###
### 5)
###

#' Sequence of Values in Regular Increments
#'
#' Extracts a sequence of values from a
#' vector in regular increments.
#'
#' @param x A vector of values.
#' @param step The size of the increment between
#'   indices in the sequence.
#' @param start The index at which to start the
#'   sequence.
#' @param value A vector of new values to assign
#'   to \code{x} at the sequence of indices.
#'
#' @return A vector of values extracted from \code{x}.
#'
#' @name every
#'
#' @examples
#' # Extract every other value
#' # at odd positions
#' every( 1:10 )
#' # Extract every other value
#' # at even positions
#' every( 1:10,,2) # Note double commas
#'
#' # Extract every 3rd value starting
#' # from 6th position
#' every( 1:12, 3, 6 )
#'
#' # Replace values at even
#' # positions with 0
#' x <- 1:10
#' every( x,,2) <- 0; x
#'
NULL

# 5.1)

#' @rdname every
#' @export

every <- function( x, step = 2, start = 1 ) {

  return( x[ seq( start, length(x), step ) ] )

}

# 5.2)

#' @rdname every
#' @export

`every<-` <- function( x, value, step = 2, start = 1 ) {

  x[ seq( start, length(x), step ) ] <- value

  return( x )
}

###
### 6)
###

#' Identify NA Values by Row
#'
#' Identifies rows in a matrix or data frame
#' that contain any (or all) NA values.
#'
#' @param x A matrix or data frame.
#' @param any Logical; if \code{TRUE} check
#'   if any observation in the row is a \code{NA}
#'   value, else checks if all observations are
#'   \code{NA}.
#'
#' @return A logical vector with values of
#' \code{TRUE} for rows with any \code{NA}
#' values (or rows where all values are \code{NA}
#' when \code{any = FALSE}).
#'
#' @examples
#' x = matrix( rnorm(9), 3, 3 )
#' x[2,3] = NA
#' findNA( x )
#' x = data.frame( A = c( 1, 2, NA ), B = 0 )
#' findNA( x )
#'
#' #' x = matrix( rnorm(9), 3, 3 )
#' x[2,] = NA; x[3,1] = NA
#' findNA( x, any = FALSE )
#'
#' @export

findNA = function( x, any = TRUE ) {

  # Initialize output
  out = NULL

  # If input is matrix or data frame
  if ( is.matrix( x ) |
       is.data.frame( x ) ) {
    # Check whether NA values are present in
    # any of the rows
    if ( any ) {
      out = apply( x, 1, function(r) any( is.na(r) ) )
    } else {
      out = apply( x, 1, function(r) all( is.na(r) ) )
    }
  } else {
    # Throw an error message
    string = "Input should be a matrix or data frame"
    stop( string, call. = FALSE )
  }

  return( out )
}

###
### 7)
###

#' Print a Nicely Formatted Table
#'
#' Given a data frame, prints to console
#' a formatted table of the results.
#'
#' @param tbl A formatted data frame.
#' @param return Logical; if \code{TRUE}, returns
#'   the character vector with each formatted
#'   row.
#'
#' @return If \code{return} is \code{TRUE} returns the
#' vector of character strings with the formatted output
#' for each row of the table.
#'
#' @examples
#' data( 'mtcars' )
#' tbl = aggregate( mtcars$mpg, mtcars[,c('cyl','vs')], mean )
#' tbl$x = round( tbl$x, 1 )
#' colnames( tbl ) = c( "# of cylinders", "Engine type", "Miles per gallon" )
#' printTable( tbl )
#'
#' @export

printTable = function( tbl, return = F ) {

  # Initialize output
  out = matrix( " ", nrow( tbl ) + 1, ncol( tbl ) )

  # Loop over columns of table
  for ( i in 1:ncol( tbl ) ) {

    # Determine maximum number of characters for elements
    # in the table's column (including the column name)
    nc = max( c( sapply( as.character( tbl[[i]] ), nchar ),
                 nchar( colnames(tbl)[i] ) ) )

    # Loop over the table's rows
    for ( j in 1:( nrow( tbl ) + 1 ) ) {

      if ( j > 1 ) {
        # Elements in column
        val = as.character( tbl[[i]] )[j-1]
      } else {
        # Column name
        val = colnames( tbl )[i]
      }
      # Current number of characters
      cur_nc = nchar( val )
      # If necessary pad out characters with empty spaces
      if ( cur_nc < nc ) {
        val = paste( paste( rep( " ", nc - cur_nc ), collapse = "" ),
                     val, sep = "" )
        out[j,i] = val
      } else {
        out[j,i] = val
      }

    }
  }

  # Convert to vector of strings
  output = apply( out, 1, paste, collapse = " | " )
  output = sapply( output, function(x) paste( x, "|" ) )

  if ( !return ) {
    for ( i in 1:length( output ) ) {
      cat( c( output[i], '\n' ) )
    }
  } else {
    return( output )
  }

}

