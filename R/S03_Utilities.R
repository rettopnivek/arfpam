# Utilities
# Written by Kevin Potter
# email: kevin.w.potter@gmail.com
# Please email me directly if you
# have any questions or comments
# Last updated 2021-04-16

# Table of contents
# 1) over
# 2) templates
# 3) section
# 4) every
#   4.1) every
#   4.2) `every<-`
# 5) has_NA
# 6) print_table
# 7) lin
# 8) empty_list
# 9) File name functions
#   9.1) find_file_name
#   9.2) make_file_name

# TO DO
# - Add unit tests ('lin', 'has_NA')
# - Custom tests for 'find_file_name', 'make_file_name'
# - Add documentation for 'make_file_name'

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
    per <- max( x )
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

    author <- getOption( 'arfpam.author' )
    email <- getOption( 'arfpam.email' )

    if ( is.null( author ) )
      author <- 'Kevin Potter'
    if ( is.null( email ) )
      email <- 'kevin.w.potter@gmail.com'

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
### 4)
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

# 4.1)

#' @rdname every
#' @export

every <- function( x, step = 2, start = 1 ) {

  return( x[ seq( start, length(x), step ) ] )

}

# 4.2)

#' @rdname every
#' @export

`every<-` <- function( x, value, step = 2, start = 1 ) {

  x[ seq( start, length(x), step ) ] <- value

  return( x )
}

###
### 5)
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
#' has_NA( x )
#' x = data.frame( A = c( 1, 2, NA ), B = 0 )
#' has_NA( x )
#'
#' #' x = matrix( rnorm(9), 3, 3 )
#' x[2,] = NA; x[3,1] = NA
#' has_NA( x, any = FALSE )
#'
#' @export

has_NA <- function( x, any = TRUE ) {

  # Initialize output
  out <- NULL

  # If input is matrix or data frame
  if ( is.matrix( x ) |
       is.data.frame( x ) ) {
    # Check whether NA values are present in
    # any of the rows
    if ( any ) {
      out <- apply( x, 1, function(r) any( is.na(r) ) )
    } else {
      out <- apply( x, 1, function(r) all( is.na(r) ) )
    }
  } else {
    # Throw an error message
    string <- "Input should be a matrix or data frame"
    stop( string, call. = FALSE )
  }

  return( out )
}

###
### 6)
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
#' print_table( tbl )
#'
#' @export

print_table <- function( tbl, return = F ) {

  # Initialize output
  out <- matrix( " ", nrow( tbl ) + 1, ncol( tbl ) )

  # Loop over columns of table
  for ( i in 1:ncol( tbl ) ) {

    # Determine maximum number of characters for elements
    # in the table's column (including the column name)
    nc <- max( c( sapply( as.character( tbl[[i]] ), nchar ),
                  nchar( colnames(tbl)[i] ) ) )

    # Loop over the table's rows
    for ( j in 1:( nrow( tbl ) + 1 ) ) {

      if ( j > 1 ) {
        # Elements in column
        val <- as.character( tbl[[i]] )[j-1]
      } else {
        # Column name
        val <- colnames( tbl )[i]
      }
      # Current number of characters
      cur_nc <- nchar( val )
      # If necessary pad out characters with empty spaces
      if ( cur_nc < nc ) {
        val <- paste( paste( rep( " ", nc - cur_nc ), collapse = "" ),
                     val, sep = "" )
        out[j,i] <- val
      } else {
        out[j,i] <- val
      }

    }
  }

  # Convert to vector of strings
  output <- apply( out, 1, paste, collapse = " | " )
  output <- sapply( output, function(x) paste( x, "|" ) )

  if ( !return ) {
    for ( i in 1:length( output ) ) {
      cat( c( output[i], '\n' ) )
    }
  } else {
    return( output )
  }

}

###
### 7)
###

#' Create Evenly Spaced Intervals
#'
#' Generates a sequence of evenly spaced
#' intervals between a lower and upper limit.
#'
#' @param start The starting value.
#' @param end The final value.
#' @param n_intervals The number of evenly spaced intervals.
#'
#' @return A numeric vector.
#'
#' @examples
#' # Five evenly spaced intervals from 0 to 1
#' lin( 0, 1, 5 )
#'
#' @export

lin <- function( start, end, n_intervals ) {

  return( seq( start, end, length.out = n_intervals ) )

}

###
### 8)
###

#' Create an Empty List
#'
#' Creates a list with a specified
#' number of empty slots.
#'
#' @param size The number of slots.
#' @param labels An optional character
#'   vector (whose length equals
#'   \code{size}) with labels for the
#'   slots.
#'
#' @return A list.
#'
#' @examples
#' # An empty list with 3 slots
#' empty_list( 3 )
#'
#' # An empty list with labels
#' empty_list( 3, c( 'S01', 'S02', 'S03' ) )
#'
#' @export

empty_list <- function( size, labels = NULL ) {

  lst <- lapply( 1:size, function(x) {
    return( NULL )
  } )

  if ( !is.null( labels ) ) {
    if ( length( labels ) == length( lst ) ) {
      names( lst ) <- labels
    } else {
      warning( 'Vector of labels does not match length of list' )
    }
  }

  return( lst )
}

###
### 9) File name functions
###

# 9.1)

#' Check if a File Name can be Found
#'
#' Checks if a file name can be found
#' in a folder via partial string matching.
#' Multiple types of output are supported.
#'
#' @param string A character string, used
#'   for partial string matching.
#' @param output The type of output to return.
#'   Options include...
#'   \itemize{
#'     \item \code{'logical'};
#'     \item \code{'vector'};
#'     \item \code{'index'};
#'     \item \code{'name'}.
#'   }
#'
#' @return Either...
#'   \itemize{
#'     \item A single logical value, \code{TRUE} if
#'           any matching file names are found
#'           (\code{'logical'}).
#'     \item A logical vector, \code{TRUE} for all
#'           matching files in the vector of file
#'           names (\code{'vector'}).
#'     \item An integer vector giving the position of
#'           any matches in the vector of file names
#'           (\code{'index'}).
#'     \item A character vector with any matching
#'           file names, otherwise \code{NULL}
#'           (\code{'name'}).
#'   }
#'
#' @examples
#' # Go to folder with html files for help pages
#' setwd( find.package( 'arfpam' )[1] )
#' setwd( 'html' )
#'
#' # Find help page for function 'every'
#' find_file_name( 'every' )
#' find_file_name( 'every', output = 'vector' )
#' find_file_name( 'every', output = 'index' )
#' find_file_name( 'every', output = 'name' )
#'
#' @export

find_file_name <- function( string,
                            output = 'logical' ) {

  # All files and folders present
  # in working directory
  all_files <- dir()

  # Determine if (standard) file name is present
  # in list of files/folders
  check <- grepl( string, all_files, fixed = T )

  # Output
  if ( output %in% c( 'Logical', 'logical', 'L', 'l' ) ) {
    return( any( check ) )
  }
  if ( output %in% c( 'Vector', 'vector', 'vec', 'V', 'v' ) ) {
    return( check )
  }
  if ( output %in% c( 'Index', 'index', 'I', 'i' ) ) {
    return( which( check ) )
  }
  if ( output %in% c( 'Name', 'name', 'N', 'n' ) ) {
    if ( any( check ) ) {
      return( all_files[ check ] )
    } else {
      return( NULL )
    }
  }

}

# 9.2)

#' Create Formatted File Name
#'
#' ...
#'
#' @param description ...
#' @param extension ...
#' @param tag ...
#' @param number ...
#' @param file_date ...
#' @param additional ...
#' @param remove ...
#'
#' @return ...
#'
#' @examples
#' # Forthcoming
#'
#' make_file_name( 'Example', 'RData' )
#' make_file_name( 'Example', 'pdf' )
#' make_file_name( 'Example', 'docx' )
#'
#' make_file_name( 'Example', 'RData', tag = 'R', number = '02' )
#'
#' @export

make_file_name <- function( description,
                           extension,
                           tag = NULL,
                           number = NULL,
                           file_date = NULL,
                           additional = NULL,
                           remove = FALSE ) {

  # Determine files in directory
  all_files <- dir()

  # If not specified, auto-generate file tag
  # based on extension
  if ( is.null( tag ) ) {

    # Word document
    if ( extension == 'docx' ) {
      tag <- 'W'
    }
    # Standard figure extentions
    if ( extension %in% c( 'pdf', 'jpg', 'jpeg', 'png' ) ) {
      tag <- 'F'
    }
    # Data files
    if ( extension %in% c( 'RData', 'csv' ) ) {
      tag <- 'D'
    }
    # R script file
    if ( extension %in% c( 'R' ) ) {
      tag <- 'S'
    }
    # Text file
    if ( extension %in% c( 'txt' ) ) {
      tag <- 'T'
    }

  }

  # If not specified, auto-generate file_date
  if ( is.null( file_date ) ) {
    file_date = format(
      Sys.Date(),
      '%m_%d_%Y'
    )
    file_date <- '-' %p% file_date
  } else {
    if ( file_date != '' ) {
      file_date <- '-' %p% file_date
    }
  }

  # Check for matching tags and descriptions for
  # files present in folder
  if ( length( all_files ) > 0 ) {

    only_files_no_placeholder <-
      # Exclude folders
      grepl( '.', all_files, fixed = T ) &
      # Exclude placeholder file
      all_files != 'Placeholder.txt'

    matching_tags <-
      substr( all_files, start = 1, stop = 1 ) == tag &
      only_files_no_placeholder

    matching_description <-
      grepl( '-' %p% description %p% '-', all_files, fixed = T ) &
      only_files_no_placeholder

    matching_extension <-
      grepl( extension %p% '$', all_files ) &
      only_files_no_placeholder

    # Check for existing file
    found_match <-
      matching_description &
      matching_tags &
      matching_extension

    # If needed, increment file number
    if ( is.null( number ) ) {

      if ( any( found_match ) ) {
        number <- substr(
          all_files[ found_match ],
          start = 2, stop = 3
        )
      } else {
        number <- sum( matching_tags ) + 1
      }

      # Make sure number is at least a double-digit and
      # convert to character string
      nc <- nchar( number )
      if ( nc == 1 ) {
        number <- paste0( '0', number )
      } else {
        number <- as.character( number )
      }

    }

    if ( remove ) {

      if ( found_match ) {

        old_file <- all_files[ found_match ]
        file.remove( old_file )

      }

    }


  } else {

    if ( is.null( number ) ) {
      number <- 1

      # Make sure number is at least a double-digit and
      # convert to character string
      nc <- nchar( number )
      if ( nc == 1 ) {
        number <- paste0( '0', number )
      } else {
        number <- as.character( number )
      }

    }

  }

  if ( !is.null( additional ) ) {
    additional <- paste0( '-', additional )
  } else {
    additional <- ''
  }

  # Generate file name
  filename <- paste0(
    tag,
    number,
    '-',
    description,
    file_date,
    additional,
    '.',
    extension
  )

  return( filename )
}

