# Utilities
# Written by Kevin Potter
# email: kevin.w.potter@gmail.com
# Please email me directly if you
# have any questions or comments
# Last updated 2023-12-19

# Table of contents
# 1) Functions for data frames and matrices
#   1.1) column
#   1.2) column_by_other
#   1.3) duplicate_wide_to_long
#   1.4) has_NA
#   1.5) pull_id
#   1.6) take_x_by_y
# 2) Functions for files
#   2.1) date_and_time
#   2.2) find_file_name
#   2.3) make_file_name
#   2.4) load_package
#   2.5) load_R_object
#   2.6) path_to_file
#   2.7) source_R_scripts
#   2.8) copy_from_source
#   2.9) save_png_figure
# 3) Functions for matching and assignment
#   3.1) assign_by_interval
#   3.2) match_and_reorder
#   3.3) match_rows
#   3.4) replace_cases
# 4) Functions for miscellaneous
#   4.1) empty_list
#   4.2) every
#     4.2.1) `every`
#     4.2.2) `every<-`
#   4.3) find_increment
#   4.4) lin
#   4.5) new_limits
#   4.6) over
#   4.7) print_table
#   4.8) runs_in_sequence
#   4.9) data_first
#   4.10) group_index
# 5) Functions for strings
#   5.1) align_strings
#   5.2) format_numbers
#   5.3) replace_string
#   5.4) squish
# 6) Functions for vectors
#   6.1) not_NA
#   6.2) percent_that_sums_to_100
#   6.3) strip_value
# 7) Functions for writing code
#   7.1) create_table_of_contents
#   7.2) create_vector
#   7.3) section
#   7.4) templates

# TO DO
# - Add Custom tests for file/folder functions
# - Add unit tests for functions

#### 1) Functions for data frames and matrices ####

#### 1.1) column ####
#' Extract Column Names Meeting Inclusion/Exclusion Criteria
#'
#' A function that matches or excludes column names in a
#' data frame based on user-supplied sub-strings.
#'
#' @param dtf A data frame.
#' @param ... Character strings with the sub-strings to match
#'   (or exclude) against the column names in \code{dtf}.
#'   If an entry starts with either \code{!}, \code{~}, or
#'   \code{-}, any columns containing the substring will be
#'   excluded. Otherwise, the function will locate
#'   all column names containing all inputted sub-strings.
#'
#' @author Kevin Potter
#'
#' @return A vector of column names meeting the inclusion
#' and exclusion criteria.
#'
#' @examples
#' # Create a data frame
#' dtf <- data.frame(
#'   IDS.INT.Subject = rep( 1:4, each = 2 ),
#'   SSS.CHR.Group = rep( c( 'A', 'A', 'B', 'B' ), each = 2 ),
#'   SSS.INT.Group = rep( c( 1, 1, 2, 2 ), each = 2 ),
#'   SSS.LGC.Group_A = rep( c( T, T, F, F ), each = 2 ),
#'   SSS.CHR.Time_point = rep( c( 'Pre', 'Post' ), 4 ),
#'   SSS.INT.Time_point = rep( 0:1, 4 ),
#'   OUT.DBL.Scores = rnorm( 8 )
#' )
#'
#' #' # All variables containing 'SSS'
#' column( dtf, 'SSS' )
#'
#' # All variables containing both 'SSS' and 'CHR'
#' column( dtf, 'SSS', 'CHR' )
#'
#' # Variables containing 'SSS' but not 'CHR'
#' column( dtf, 'SSS', '~CHR' )
#'
#' @export

column <- function( dtf, ... ) {

  args <- list(...)
  n_args <- length( args )

  include <- rep( '', n_args )
  exclude <- rep( '', n_args )
  inc_i <- 1
  inc_e <- 1
  for ( i in 1:n_args ) {
    txt <- as.character( args[[i]] )
    if ( grepl( '!', txt, fixed = T ) |
         grepl( '~', txt, fixed = T ) |
         grepl( '-', txt, fixed = T ) ) {
      txt <- gsub( '!', '', txt, fixed = T )
      txt <- gsub( '~', '', txt, fixed = T )
      txt <- gsub( '-', '', txt, fixed = T )
      exclude[inc_e] <- txt
      inc_e <- inc_e + 1
    } else {
      include[inc_i] <- txt
      inc_i <- inc_i + 1
    }
  }

  if ( all( include == '' ) ) {
    include = NULL
  } else {
    include <- include[ include != '' ]
  }
  if ( all( exclude == '' ) ) {
    exclude = NULL
  } else {
    exclude <- exclude[ exclude != '' ]
  }

  clm <- colnames( dtf )
  K <- length( clm )

  if ( !is.null( include ) ) {
    each_include <- sapply( include, function(x) {
      grepl( x, clm, fixed = T )
    } )
  } else {
    each_include = cbind( rep( T, K ) )
  }


  if ( !is.null( exclude ) ) {
    each_exclude <- sapply( exclude, function(x) {
      grepl( x, clm, fixed = T )
    } )
  } else {
    each_exclude = cbind( rep( F, K ) )
  }

  entries =
    rowSums( each_include ) == length( include ) &
    !( rowSums( each_exclude ) > 0 )

  return( clm[ entries ] )
}

#### 1.2) column_by_other ####
#' Unique Values of one Column by Another
#'
#' A function that takes two columns in a data frame
#' and reports the unique values of one column
#' associated with the unique values of the other
#' column.
#'
#' @param dtf A data frame.
#' @param col1 The first column (non-standard evaluation
#'   possible).
#' @param col2 The second column (non-standard evaluation
#'   possible).
#'
#' @author Kevin Potter
#'
#' @return A data frame with the unique values
#' of the first column associated with the
#' unique values of the second.
#'
#' @examples
#' # Define a data frame
#' dtf <- data.frame(
#'   A = c( 1, 1, 2, 2, 3, 3 ),
#'   B = c( 'A', 'A', 'B', 'B', 'C', 'D' )
#' )
#'
#' # Values of column 'B' by values of column 'A'
#' column_by_other( dtf, A, B )
#'
#' @export

column_by_other <- function( dtf, col1, col2 ) {

  # Non-standard evaluation
  V <- as.character( substitute( col1 ) )
  L <- as.character( substitute( col2 ) )

  dtf$Cur_values = dtf[[ V ]]
  dtf$Cur_labels = dtf[[ L ]]

  val <- sort( unique( dtf$Cur_values ) )
  val <- val[ !is.na( val ) ]

  lbl <- lapply(
    val,
    function(x) unique( dtf$Cur_labels[ dtf$Cur_values %in% x ] )
  )

  n <- sum( sapply( lbl, length ) )
  out <- data.frame(
    V = rep( NA, n ),
    L = rep( NA, n )
  )
  colnames( out ) <- c( V, L )

  inc <- 0
  for ( i in 1:length( val ) ) {

    index <- 1:length( lbl[[i]] )
    out[ index + inc, 1 ] <- val[i]
    out[ index + inc, 2 ] <- lbl[[i]]

    inc <- max(index + inc)
  }

  return( out )
}

#### 1.3) duplicate_wide_to_long ####
#' Duplicate Values from Wide-Form to Long-Form Data
#'
#' Function to duplicate values in column \code{x}
#' from a wide-form (one row per case) data set \code{wf}
#' based on a shared column \code{y} with a long-form
#' (multiple rows per case) data set \code{lf}.
#'
#' @param wf A wide-form data frame.
#' @param lf A long-form data frame.
#' @param x The column in \code{wf} with values to
#'   duplicate (non-standard evaluation possible).
#' @param y The column in both \code{wf} and \code{lf}
#'   over which to repeat values over (non-standard
#'   evaluation possible).
#' @param default The value to substitute if
#'   no cases for \code{x} based on \code{y}
#'   are found to duplicate.
#'
#' @return A vector matching in length to the number
#' of rows of \code{lf} with the values of \code{x}
#' repeated for each unique case of \code{y}.
#'
#' @examples
#' # Example wide-form data-frame
#' wf <- data.frame(
#'   ID = 1:3,
#'   Value = 4:6
#' )
#'
#' # Example long-form data-frame
#' lf <- data.frame(
#'   ID = rep( 1:3, each = 3),
#'   Value = NA
#' )
#'
#' # Duplicate values from 'wf' based
#' # on shared # column 'ID'
#' lf$Value <- duplicate_wide_to_long( wf, lf, x = Value, y = ID )
#' print( lf )
#'
#' @export

duplicate_wide_to_long <- function( wf, lf, x, y, default = NA ) {

  # Non-standard evaluation
  X <- as.character(substitute(x))
  Y <- as.character(substitute(y))

  N <- nrow( lf )

  out <- sapply( 1:N, function(j) {

    val <- default

    if ( !is.na( lf[[ Y ]][j] ) ) {

      match_to_wf <-
        wf[[ Y ]] %in% lf[[ Y ]][j] &
        !is.na( wf[[ Y ]] )

      if ( any( match_to_wf ) ) {

        val <- wf[[ X ]][ match_to_wf ][1]

      }

    }

    return( val )
  } )

  return( out )
}

#### 1.4) has_NA ####
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
#' x <- matrix(rnorm(9), 3, 3)
#' x[2, 3] <- NA
#' has_NA(x)
#' x <- data.frame(A = c(1, 2, NA), B = 0)
#' has_NA(x)
#'
#' x <- matrix(rnorm(9), 3, 3)
#' x[2, ] <- NA
#' x[3, 1] <- NA
#' has_NA(x, any = FALSE)
#' @export

has_NA <- function(x, any = TRUE) {

  # Initialize output
  out <- NULL

  # If input is matrix or data frame
  if (is.matrix(x) |
      is.data.frame(x)) {
    # Check whether NA values are present in
    # any of the rows
    if (any) {
      out <- apply(x, 1, function(r) any(is.na(r)))
    } else {
      out <- apply(x, 1, function(r) all(is.na(r)))
    }
  } else {
    # Throw an error message
    string <- "Input should be a matrix or data frame"
    stop(string, call. = FALSE)
  }

  return(out)
}

#### 1.5) pull_id ####
#' Extract Subject IDs From a Data-Frame
#'
#' Function to extract the unique values for
#' subject identifiers in a data frame,
#' assuming a common variable name
#' (e.g., \code{IDS.CHR.Subject} or \code{ID}).
#'
#' @param dtf A data frame.
#' @param subject Logical; if \code{TRUE}
#'   looks for common variable names for
#'   subject identifiers (IDS.CHR.Subject,
#'   ID, etc.). If \code{FALSE} looks for
#'   common variable names for screening
#'   identifiers (IDS.CHR.Screen, etc.).
#' @param id A user-defined variable name
#'   if the column with subject identifiers
#'   is not part of the commonly-used labels.
#'
#' @return A vector of values.
#'
#' @examples
#' dtf <- data.frame( ID = rep( 1:3, each = 3 ), X = rnorm(9) )
#' pull_id( dtf )
#'
#' @export

pull_id <- function( dtf, subject = TRUE, id = NULL ) {

  clm <- colnames( dtf )

  if ( is.null( id ) ) {

    if ( subject ) {
      # Check for standard variable names for subject IDs
      standard_id_forms <- c(
        'IDS.CHR.Subject',
        'IDS.INT.Subject',
        'IDS.CHR.AT.Subject',
        'IDS.INT.AT.Subject',
        'ID',
        'study_id',
        'id',
        'studyid'
      )
    } else {
      # Check for standard variable names for subject IDs
      standard_id_forms <- c(
        'IDS.CHR.Screen',
        'IDS.INT.Screen',
        'ID'
      )
    }

    any_match <- standard_id_forms %in% clm

    if ( !any( any_match ) ) {
      stop(
        'No pre-defined variable name for ID found'
      )
    } else {
      id <- standard_id_forms[any_match][1]
    }

  }

  # Extract unique IDs
  out <- unique( dtf[[ id ]] )

  return( out )
}

#### 1.6) take_x_by_y ####
#' Repeat Column Values by Unique Cases in Another Column
#'
#' Function to duplicate values in column \code{x} by
#' the unique cases in column \code{y}. Useful, for
#' example, in extracting and duplicating a subject's
#' baseline values across all time points in a long-form
#' data frame for a longitudinal study.
#'
#' @param lfd A long-form data frame.
#' @param x The column with values to duplicate
#'   (non-standard evaluation possible).
#' @param y The column to repeat values over
#'   (non-standard evaluation possible).
#' @param extra A logical vector matching in
#'   length to the number of rows of \code{lfd}
#'   specifying additional cases to match by
#'   when isolating the values of \code{x} to
#'   repeat.
#' @param default The value to substitute if
#'   no cases for \code{x} based on \code{extra}
#'   are found to duplicate.
#' @param per_row Logical; if \code{FALSE} returns
#'   the associated value of \code{x} for each
#'   unique value of \code{y}, otherwise returns
#'   a value of \code{x} per each frow of \code{lfd}.
#'
#' @return A vector matching in length to the number
#' of rows of \code{lfd} (for \code{per_row = TRUE}) or
#' to the number of unique values of \code{y} with the
#' values of \code{x} repeated for each unique case of
#' \code{y}.
#'
#' @examples
#' # Example long-form data frame
#' lfd <- data.frame(
#'   ID = rep( 1:3, each = 3 ),
#'   Value = 1:9,
#'   Time = rep( 0:2, 3 )
#' )
#'
#' # Repeat first value of Y for each value of X
#' i <- lfd$Time == 0
#' take_x_by_y( lfd, Value, ID, extra = i )
#' # Repeat last value of Y for each value of X
#' i <- lfd$Time == 2
#' take_x_by_y( lfd, Value, ID, extra = i )
#' # Per unique case of ID
#' take_x_by_y( lfd, Value, ID, extra = i, per_row = FALSE )
#'
#' @export

take_x_by_y <- function( lfd, x, y,
                         extra = NULL,
                         default = NA,
                         per_row = TRUE ) {

  # Non-standard evaluation
  X <- as.character(substitute(x))
  Y <- as.character(substitute(y))

  N <- nrow( lfd )

  if ( is.null( extra ) ) {
    extra <- rep( TRUE, N )
  }

  out <- sapply(
    1:N, function(i) {

      specific_index <-
        !is.na( lfd[[ Y ]] ) &
        lfd[[ Y ]] %in% lfd[[ Y ]][i] &
        extra

      if ( sum( specific_index ) > 1 ) {
        warning(
          paste0(
            "Case ", lfd[[ Y ]][i], "\n",
            "   More than one unique value detected"
          )
        )
      }

      if ( any( specific_index ) ) {
        return( lfd[[ X ]][ specific_index ][1] )
      } else {
        return( default )
      }
    } )

  if ( per_row ) {
    return( out )
  } else {

    unq_Y <- unique( lfd[[Y]] )
    unq_Y <- unq_Y[ !is.na(unq_Y) ]
    out_by_y <- sapply( unq_Y, function(y) {
      return( unique( out[ lfd[[Y]] %in% y] ) )
    } )
    names( out_by_y ) <- unq_Y

    return( out_by_y )

  }

}


#### 2) Functions for files ####

#### 2.1) date_and_time ####
#' Formatted Date and Time for File Names
#'
#' Convenience function to generate a
#' nicely formatted character string
#' with the date and time, typically of
#' the form: YYYY_MM_DD-HH_MM to include
#' as part of a file name. Can convert
#' the character string back into a
#' date and time if needed.
#'
#' @param value A character string to convert
#'   back into a date-time object.
#' @param frmt A character string specifying
#'   the format for the date and time object
#'   (see \code{\link[base:strptime]{as.POSIXct}}).
#'
#' @return Either 1) a character string with the
#' date and time, to include in a file name, or
#' 2) a date-time object.
#'
#' @examples
#' string <- date_and_time()
#' string
#' # Convert back to date and time object
#' format( date_and_time( string ), '%Y-%m-%d %H:%M' )
#'
#' @export

date_and_time <- function( value = NULL, frmt = '%Y_%m_%d-%H_%M' ) {

  if ( is.null( value ) ) {
    return( format( Sys.time(), format = frmt ) )
  } else {
    return( as.POSIXct( value, format = frmt ) )
  }

}

#### 2.2) find_file_name ####
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
#'     \item \code{'index'};
#'     \item \code{'name'};
#'     \item \code{'logical'};
#'     \item \code{'vector'}.
#'   }
#' @param full Logical; if \code{TRUE} returns the
#'   full path.
#' @param ... Additional arguments to the
#'   \code{\link[base:list.files]{dir}} function.
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
#' setwd(find.package("arfpam")[1])
#' setwd("html")
#'
#' # Find help page for function 'every'
#' find_file_name("every")
#' find_file_name("every", output = "index")
#' find_file_name("every", output = "logical")
#' find_file_name("every", output = "vector")
#'
#' @export

find_file_name <- function(string,
                           output = "name",
                           full = FALSE,
                           ...) {

  # All files and folders present
  # in working directory
  all_files <- dir(...)

  # Determine if file name is present
  # in list of files/folders
  check <- grepl(string, all_files, fixed = T)

  # Logical output
  if (output %in% c("Logical", "logical", "L", "l")) {

    return(any(check))

    # Close 'Logical output'
  }

  # Vector output
  if (output %in% c("Vector", "vector", "vec", "V", "v")) {

    return(check)

    # Close 'Vector output'
  }

  # Index output
  if (output %in% c("Index", "index", "I", "i")) {

    return(which(check))

    # Close 'Index output'
  }

  # Name output
  if (output %in% c("Name", "name", "N", "n")) {

    # If file found
    if (any(check)) {

      # If returning full path
      if (full) {

        add_path <- ''

        lst_arg <- list(...)

        # Check if path argument provided
        if ( length(lst_arg) > 0 ) {

          # Update additional path
          if ( !is.null(lst_arg$path) ) {

            add_path <- paste0( lst_arg$path, '/' )

            # Close 'Update additional path'
          }

          # Close 'Check if path argument provided'
        }

        full_path <- paste0(
          getwd(), '/',
          add_path,
          all_files[check]
        )
        return(full_path)

        # Close 'If returning full path'
      } else {

        return(all_files[check])

        # Close else for 'If returning full path'
      }

      # Close 'If file found'
    } else {

      return(NULL)

      # Close else for 'If file found'
    }

    # Close 'Name output'
  }

}

#### 2.3) make_file_name ####
#' Create Formatted File Name
#'
#' Creates a file name using the standardized
#' format 'PROJECT-SXX-Description-YYYY_MM_DD-HH_MM.ext'
#' where 'PROJECT' is a abbreviation for a project name,
#' and 'SXX' is a letter followed by a number indicating
#' the script or analysis index. Both variables can
#' be automatically specified from existing
#' environmental variables.
#'
#' @param description A character string, a human-readable
#'   description of the file (with words typically separated
#'   by \code{'_'}).
#' @param extension A character string, the file extension
#'   (e.g., \code{'R'}, \code{'RData'}) - the period is
#'   added automatically.
#' @param project A character string, the abbreviation for the
#'   project name (can be inferred from environmental variables).
#' @param script A character string, typically a one-letter
#'   abbreviation followed by a two-digit number indicating
#'   the script or analysis index.
#' @param date_time A character string giving the date and time
#'   (usually in the format \code{'YYYY_MM_DD-HH_MM'}) - if
#'   not provided uses the current date and time instead.
#' @param remove Logical; if \code{TRUE} removes any existing
#'   files in the working directory with the same
#'   project, script, description, and extension values.
#' @param path A character string specifying the subfolder(s)
#'   for the file.
#' @param env_variables A character vector specifying the
#'   name of the environmental variables with the
#'   project and script abbreviations.
#'
#' @return A character string.
#'
#' @examples
#'
#' make_file_name( 'Example', 'R' )
#' make_file_name( 'Example_2', 'RData', project = 'EX', script = 'S01' )
#'
#' @export

make_file_name <- function(description,
                           extension,
                           project = NULL,
                           script = NULL,
                           date_time = NULL,
                           remove = FALSE,
                           path = '.',
                           env_variables =
                             c( 'ABBR_PROJECT', 'ABBR_SCRIPT' )) {

  if ( is.null(project) ) {

    # Check for environmental variable
    project <- Sys.getenv( env_variables[1] )
    if ( project == "" ) project <- NULL

  }

  if ( is.null(script) ) {

    # Check for environmental variable
    script <- Sys.getenv( env_variables[2] )
    if ( script == "" ) script <- NULL

  }

  if ( !is.null( project ) ) {
    chr_project <- paste0( project, '-' )
  } else {
    chr_project <- ''
  }

  if ( !is.null( script ) ) {
    chr_script <- paste0( script, '-' )
  } else {
    chr_script <- ''
  }

  if ( is.null( date_time ) ) {
    chr_dat <- paste0( '-', arfpam::date_and_time() )
  } else {
    chr_dat <- paste0( '-', date_time )
  }

  chr_desc <- description
  chr_ext <- paste0( '.', extension )

  chr_file <- paste0(
    chr_project,
    chr_script,
    chr_desc,
    chr_dat,
    chr_ext
  )

  if ( remove ) {

    chr_search <- paste0(
      chr_project,
      chr_script,
      chr_desc
    )

    lgc_match <-
      grepl( chr_search, dir( path ), fixed = TRUE ) &
      grepl( chr_ext, dir( path ), fixed = TRUE )

    if ( any(lgc_match) ) {

      lgc_removed <- file.remove(
        paste0(
          path, '/', dir( path )[lgc_match]
        )
      )

    }

  }

  if ( path == '.' ) {
    return(chr_file)
  } else {
    return( paste0( path, '/', chr_file ) )
  }

}

#### 2.4) load_package ####
#' Convenience Function to Load in R Packages
#'
#' Convenience function to load in a set of R packages -
#' can be used to first install missing packages before
#' loading them.
#'
#' @param pkg A character vector of package names to
#'   load - by default loads the current package
#'   in addition to the package
#'   \code{\link[dplyr:dplyr-package]{dplyr}}.
#' @param install Logical; if \code{TRUE} attempts
#'   to install missing packages.
#' @param quietly Logical; if \code{TRUE} loads
#'   in packages with no console messages (including
#'   warnings and/or error messages).
#'
#' @return Loads in a set of R packages (see
#' \code{\link[base]{library}}).
#'
#' @export

load_package <- function( pkg = c( 'dplyr', 'arfpam' ),
                          install = FALSE,
                          quietly = FALSE ) {

  # Packages on Github repo for 'rettopwnivek'
  rettopwnivek_packages <- c(
    'arfpam',
    'extbrms',
    'extofficer',
    'ffcar',
    'pathdiagrams'
  )

  # Number of packages to load in
  K <- length( pkg )

  # Loop over packages
  for ( k in 1:K ) {

    # If specified install missing packages
    if ( install ) {

      already_installed <- installed.packages()

      # If package hs not been installed
      if ( !pkg[k] %in% rownames( already_installed ) ) {

        # If package is on Github repo
        if ( pkg[k] %in% rettopwnivek_packages ) {

          devtools::install_github(
            paste0( 'rettopwnivek/', pkg[k] )
          )

          # Close 'If package is on Github repo'
        } else {

          install.packages( pkg[k] )

          # Close else for 'If package is on Github repo'
        }

        # Close 'If package hs not been installed'
      }

      # Close 'If specified install missing packages'
    }

    library( pkg[k], character.only = TRUE, quietly = quietly )

    # Close 'Loop over packages'
  }

}

#### 2.5) load_R_object ####
#' Load in R Objects From .RData Files
#'
#' A convenience function that, given a
#' path to a .RData file, will load in
#' a specified R object contained within
#' the file.
#'
#' @param path_to_rdata A character string,
#'   a path to a .RData file to be passed
#'   on to the \code{\link[base]{load}}
#' @param which_object The variable name for
#'   the R object to return from the set of
#'   all variables loaded in from the .RData
#'   file. Uses non-standard evaluation.
#'
#' @author Kevin Potter
#'
#' @export

load_R_object <- function( path_to_rdata, which_object ) {

  # Load in .RData file
  load( path_to_rdata )

  # Return specified object loaded in from .RData file
  return( eval( substitute( which_object ) ) )
}

#### 2.6) path_to_file ####
#' Returns File/Folder Paths
#'
#' Returns an absolute file or folder path.
#' Folder paths can be extracted from a
#' pre-specified environmental variable.
#'
#' @param file_name A character string, a
#'   partial match to the file of interest.
#' @param env_var A character string, the name for
#'   the environment variable.
#' @param path A character string, a relative or
#'   absolute path to a folder.
#' @param latest Logical; if \code{TRUE} returns only
#'   the latest version of a file whose name contains
#'   a date.
#'
#' @return A character string.
#'
#' @export

path_to_file <- function( file_name = NULL,
                          env_var = NULL,
                          path = NULL,
                          latest = TRUE ) {

  if ( !is.null( env_var ) ) {
    path = Sys.getenv( env_var )
    if ( path == '' ) {
      stop( 'Environmental variable for path not found' )
    }
  }

  if ( is.null( path ) ) {
    path <- getwd()
  }

  if ( !is.null( file_name ) ) {

    x <- arfpam::find_file_name(
      file_name, output = 'name',
      path = path
    )

    if ( length( x ) == 0 ) {
      stop( 'File not found' )
    }

    if ( latest ) {
      return( paste0( path, '/', sort( x )[ length(x) ] ))
    } else {
      return( paste0( path, '/', sort( x ) ))
    }

  } else {
    return( path )
  }

}

#### 2.7) source_R_scripts ####
#' Source in Multiple R Scripts in a Folder
#'
#' A convenience function that loops through
#' and reads in code in .R files stored in a
#' folder located in the current working directory.
#'
#' @param files_to_include A vector of either...
#'   \itemize{
#'     \item Numeric indices denoting which files
#'       to include;
#'     \item A character string matching the initial
#'        set of letters across all relevant files (e.g., if
#'        all scripts of interest start with the letter 'S');
#'     \item A character vector with the full file names
#'       for the files to include.
#'   }
#' @param path The folder name with the scripts to source.
#'
#' @author Kevin Potter
#'
#' @export

source_R_scripts = function( files_to_include = NULL,
                             path = 'R' ) {

  # Folders to load
  all_files <- dir(
    path = path
  )

  # Identify R scripts

  f <- function( x ) {
    grepl( x, all_files, fixed = T )
  }
  # Files must have extension .R
  r_files <-
    ( f( '.R' ) | f( '.r' ) ) &
    # Exclude R data files
    !( f( '.RData' ) | f( '.rdata' ) |
         f( '.rda' ) |
         # Exclue R markdown files
         f( '.Rmd' ) | f( '.rmd' )
    )

  # Isolate .R files
  if ( any( r_files ) ) {
    all_files <- all_files[ r_files ]
  } else {
    stop( 'No .R files found' )
  }

  # Check if subset of files should be included
  if ( !is.null( files_to_include ) ) {

    # If numeric indices were provided
    if ( is.numeric( files_to_include ) ) {
      files_to_source <- all_files[ files_to_include ]
    }

    # If a character vector was provided
    if ( is.character( files_to_include ) ) {

      # If a single character string with no '.R' extension
      # was provided
      if ( length( files_to_include ) == 1 &
           !any( grepl( '.R', files_to_include, fixed = T ) ) ) {

        n_letters <- nchar( files_to_include )

        to_check <- substr( all_files, start = 1, stop = n_letters )

        files_to_source <- all_files[
          files_to_include %in% to_check
        ]

      } else {
        # Exact matching to file names
        files_to_source <- all_files[ all_files %in% files_to_include ]
      }

    }
  } else {
    # Otherwise take all files in folder
    files_to_source <- all_files
  }

  # Source in all specified R scripts
  if ( length( files_to_source ) > 0 ) {
    sapply( 1:length( files_to_source ), function(i) {
      source( paste0( path, "/", files_to_source[i] ) )
    } )
  } else {
    stop( 'No files found matching given criteria' )
  }

}

#### 2.8) copy_from_source ####
#' Copy Files From Source Folder
#'
#' Function to copy files from a subfolder in a
#' source folder to a new subfolder in a user-defined
#' source folder in the current directory.
#'
#' @param source_path A character string, the
#'   absolute path to the source folder.
#' @param destination_path A character string,
#'   the path to the folder to which files should
#'   be copied - if blank, uses the current
#'   working directory.
#' @param source_subfolder An optional character
#'   string, the full or partial name of a
#'   subfolder in source location with the files
#'   to copy.
#' @param environment_var An optional character
#'   string, the environmental variable with the
#'   path to the source folder.
#'
#' @returns As a side effect copies files to a to
#' the specified destination folder.
#'
#' @export

copy_from_source <- function( source_path = '',
                              destination_path = '',
                              source_subfolder = '',
                              environment_var = 'FOLDER_SOURCE' ) {

  # Path to source folder from environmental variable
  if ( source_path == '' ) {

    source_path <- Sys.getenv(
      environment_var
    )

    # Close 'Path to source folder from environmental variable'
  }

  # If a subfolder is specified
  if ( source_subfolder != '' ) {

    subfolders <- dir(
      path = source_path
    )
    source_subfolder <- subfolders[
      grepl(
        source_subfolder,
        subfolders,
        fixed = TRUE
      )
    ][1]

    source_path <- paste0(
      source_path, '/', source_subfolder
    )

    # Close 'If a subfolder is specified'
  }

  # List files in source folder
  files_and_folders_to_copy <- list.files(
    path = source_path,
    recursive = TRUE,
    include.dirs = TRUE
  )

  # By default use current working directory
  if ( destination_path == '' ) {

    destination_path <- getwd()

    # Close 'By default use current working directory'
  }

  new_path_for_copied_files_and_folders <- paste0(
    destination_path, '/', files_and_folders_to_copy
  )

  # Check if a subfolder is present
  lgc_is_directory <- !grepl(
    '.', files_and_folders_to_copy, fixed = TRUE
  )

  # If subfolder is present
  if ( any( lgc_is_directory ) ) {

    # Create subfolder in new location
    lgc_success <- sapply(
      new_path_for_copied_files_and_folders[lgc_is_directory],
      function( chr_folder ) {
        dir.create(
          chr_folder,
          recursive = TRUE
        )
      }
    )

    # Close 'If subfolder is present'
  }

  # Copy files to local machine
  lgc_success <- file.copy(
    from = paste0(
      source_path, '/',
      files_and_folders_to_copy[!lgc_is_directory]
    ),
    to = new_path_for_copied_files_and_folders[!lgc_is_directory]
  )

  # Error and warning messages
  if ( any(lgc_success) ) {

    if ( !all(lgc_success) ) {
      warning( 'Some files or folders were not copied' )
    }

    # Close 'Error and warning messages'
  } else {

    stop( 'Failed to copy files' )

    # Close else for 'Error and warning messages'
  }

}

#### 2.9) save_png_figure ####
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

#### 3) Functions for matching and assignment ####

#### 3.1) assign_by_interval ####
#' Assign Values by Cases Within Intervals
#'
#' Function that assigns user-specified values
#' for when a numeric variable falls within
#' intervals defined by a set of
#'
#' @param x A numeric vector of values.
#' @param breakpoints A numeric vector of values, the
#'   breakpoints for the intervals. By default, the
#'   lowest and highest breakpoints are set to
#'   \code{-Inf} and \code{Inf}, so only the intervening
#'   points need to be specified (this behavior can be changed).
#' @param values A vector of values to assign for all cases
#'   within a given interval.
#' @param include A character vector with two elements,
#'   either \code{'>'} or \code{'>='} and \code{'<'} or
#'   \code{'<='}.
#' @param ends An optional vector specifying the lowest and
#'   and highest breakpoints. Can be set to \code{NA} to
#'   prevent adding to \code{breakpoints}.
#' @param default The default value to use for cases that
#'   are not within any intervals.
#'
#' @return A vector of values.
#'
#' @examples
#' # Default l > x <= u
#' x <- 1:6
#' assign_by_interval( x, c( 2, 4 ) )
#'
#' # Can quickly define splits
#' x <- c( 1, 1, 1, 1, 2, 2, 10 )
#' # Mean split
#' assign_by_interval( x, mean(x) )
#' # Median split
#' assign_by_interval( x, median(x) )
#' # Custom values
#' assign_by_interval( x, mean(x), values = c( 'Below', 'Above' ) )
#'
#' # Custom conditions and bounds
#' x <- 1:6
#' assign_by_interval(
#'   x, c( 1, 2, 4, 6 ), include = c('>=', '<' ), ends = NULL
#' )
#' # Can change default value for when nothing in range
#' assign_by_interval(x, 6, ends = c( 2, NA ), default = -1 )
#'
#' @export

assign_by_interval <- function(
    x,
    breakpoints,
    values = NULL,
    include = c( '>', '<=' ),
    ends = c( -Inf, Inf ),
    default = NA ) {

  # If limits for breakpoints given
  if ( !is.null( ends ) ) {

    lgc_misspecified <- TRUE

    # If only lower limit given
    if ( length( ends ) == 1 ) {

      if ( !is.na( ends[1] ) ) {
        breakpoints <- c( ends[1], breakpoints )
      }

      lgc_misspecified <- FALSE

      # Close 'If only lower limit given'
    }

    # If lower and upper limit given
    if ( length( ends ) == 2 ) {

      if ( !is.na( ends[1] ) ) {
        breakpoints <- c( ends[1], breakpoints )
      }

      if ( !is.na( ends[2] ) ) {
        breakpoints <- c( breakpoints, ends[2] )
      }

      lgc_misspecified <- FALSE

      # Close 'If lower and upper limit given'
    }

    if ( lgc_misspecified ) {

      chr_error <- paste0(
        "Argument 'ends' must be a vector of one or two values ",
        "giving the lower and upper limits for the breakpoints ",
        "respectively"
      )
      stop( chr_error )

    }

    # Close 'If limits for breakpoints given'
  }

  # Ensure no duplicate cut-offs
  breakpoints <- unique( breakpoints )

  # Number of bins
  bins <- length( breakpoints ) - 1
  # Number of observations
  n <- length(x)

  # By default assign integers for values
  if ( is.null( values ) ) {

    values <- 1:bins

    # Close 'By default assign integers for values'
  }

  # Check number of bins and values
  if ( length(values) != bins ) {

    chr_error <- paste0(
      "Must specify set of replacement values equal to ",
      "number of intervals (i.e., for B breakpoints there are B - 1 values)"
    )
    stop( chr_error )

    # Close 'Check number of bins and values'
  }

  # Shorthand for conditional statements
  if ( length( include ) == 1 ) {

    include <- switch(
      include,
      `<` = c( '>=', '<' ),
      `>` = c( '>', '<=' ),
      `>=` = c( '>=', '<=' ),
      `<=` = c( '>=', '<=' )
    )

    # Close 'Shorthand for conditional statements'
  }

  # Check conditional statements
  if ( !include[1] %in% c( '>', '>=' ) |
       !include[2] %in% c( '<', '<=' ) ) {

    chr_error <- paste0(
      "Argument 'include' must be 2-element vector with either ",
      "'>' or '>=' and '<' or '<='"
    )
    stop( chr_error )

    # Close 'Check conditional statements'
  }

  # Initialize output
  y <- rep( default, n )

  # Loop over bins
  for ( b in 1:bins ) {

    limits <- breakpoints[0:1 + b]

    if ( all( include == c( '>', '<' ) ) ) {
      lgc_inside <-
        !is.na(x) & x > limits[1] & x < limits[2]
    }

    if ( all( include == c( '>=', '<' ) ) ) {
      lgc_inside <-
        !is.na(x) & x >= limits[1] & x < limits[2]
    }

    if ( all( include == c( '>', '<=' ) ) ) {
      lgc_inside <-
        !is.na(x) & x > limits[1] & x <= limits[2]
    }

    if ( all( include == c( '>=', '<=' ) ) ) {
      lgc_inside <-
        !is.na(x) & x >= limits[1] & x <= limits[2]
    }

    y[lgc_inside] <- values[b]

    # Close 'Loop over intervals'
  }

  return(y)
}

#### 3.2) match_and_reorder ####
#' Match and Reorder Vectors
#'
#' Function to match a vector of values against
#' another vector and reorder the original
#' vector based on the matching indices.
#'
#' @param x A vector.
#' @param values A vector containing the values to match
#'   against in \code{x}.
#' @param y An optional vector matching in length to \code{x}
#'   to reorder - otherwise the function returns the indices
#'   for reordering.
#'
#' @return Either a vector of indices for reordering or the
#' reordered output from the input \code{y}.
#'
#' @examples
#' x <- rep( LETTERS[1:3], 3 )
#' values <- c( 'C', 'B', 'A' )
#' y <- rep( 1:3, 3 )
#' match_and_reorder( x, values, y )
#'
#' set.seed( 111 ) # For reproducibility
#' # Example data frame
#' dtf_example <- data.frame(
#'   X = rep( 1:4, each = 2 ),
#'   Y = round( rnorm( 8 ), 2 )
#' )
#' # Resample with replacement from 'X'
#' shuffled_x <- sample( 1:4, size = 4, replace = TRUE )
#' # Create a reordered data frame based on the resampled values
#' dtf_shuffled <- data.frame(
#'   X = match_and_reorder( dtf_example$X, shuffled_x, dtf_example$X ),
#'   Y = match_and_reorder( dtf_example$X, shuffled_x, dtf_example$Y )
#' )
#'
#' @export

match_and_reorder <- function( x, values, y = NULL ) {

  new_index <- lapply(
    values, function(v) {
      which( x %in% v )
    } ) |> unlist()

  if ( is.null( y ) ) {
    return( new_index )
  } else {

    if ( length(y) != length(x) ) {
      stop( "Length of 'x' and 'y' must match" )
    }

    return( y[ new_index ] )
  }

}

#### 3.3) match_rows ####
#' Match Rows Across Two Data Frames
#'
#' Function that returns an index indicating
#' the row in one data frame that matches the
#' row in another data frame.
#'
#' @param dtf_to A data frame with the rows to match over.
#' @param dtf_from A data frame with the rows to compared against.
#'
#' @returns A list equal in length to the number of rows in
#' \code{dtf_to} with the row indices from \code{dtf_from} that
#' matches a given row in \code{dtf_to}. If no matches are found
#' returns \code{NA}.
#'
#' @examples
#' dtf_example_1 <- data.frame( c( LETTERS[3:1], 'Z' ), V2 = c( 3:1, 26 ) )
#' dtf_example_2 <- data.frame( LETTERS[1:6], V2 = 1:6 )
#' match_rows( dtf_example_1, dtf_example_2 )
#'
#' @export

match_rows <- function(
    dtf_to,
    dtf_from ) {

  N_rows_from <- nrow( dtf_from )
  N_rows_to <- nrow( dtf_to )

  lst_index <- lapply(
    1:N_rows_to, function(r_to) {

      lgc_matches <- sapply( 1:N_rows_from, function (r_from) {
        all( all.equal(
          dtf_to[r_to, ],
          dtf_from[r_from, ],
          check.attributes = FALSE
        ) %in% TRUE )
      } )

      if ( any( lgc_matches ) ) {
        return( which( lgc_matches ) )
      } else {
        return( NA )
      }

    }

  )

  return( lst_index )
}


#### 3.4) replace_cases ####
#' Replace Cases
#'
#' Function that matches cases in a vector and
#' replaces them with user-specified values.
#' Robust to NA values.
#'
#' @param x A vector.
#' @param to_replace Either a vector of values to
#'   replace, or a list of vectors for the sets of values
#'   to replace.
#' @param replace_with A vector of values, either a single
#' value or a vector matching in length with \code{to_replace}.
#'
#' @return A vector.
#'
#' @examples
#' # Example vector
#' x <- rep( LETTERS[1:4], each = 3 )
#' # Replace all cases
#' replace_cases( x, c( 'A', 'B', 'C', 'D' ), 1:4 )
#' # Replace some cases and use default value for others
#' replace_cases( x, c( 'A', 'B', 'C' ), 1:3 )
#' # Replace combinations of cases
#' replace_cases( x, list( c( 'A', 'B' ), c( 'C', 'D' ) ), 1:2 )
#'
#' # Robust to NA values
#' x <- c( 1, 1, 2, 2, NA, NA )
#' replace_cases( x, c( 1, 2, NA ), c( 'A', 'B', '' ) )
#'
#' @export

replace_cases <- function( x, to_replace, replace_with, default = NA ) {

  N <- length(x)
  out <- rep( default, N )

  K <- length( to_replace )

  if ( length( replace_with ) == 1 ) {
    replace_with <- rep( replace_with, K )
  }

  if ( length( replace_with ) != length( to_replace ) ) {
    chr_error <- paste0(
      "Argument 'replace_with' must be either single value or ",
      "match in length with argument 'to_replace'."
    )

    stop( chr_error )
  }

  is_list <- is.list( to_replace )

  for ( k in 1:K ) {

    if ( is_list ) {

      if ( any( is.na( to_replace[[k]] ) ) ) {
        cases_that_match <- is.na(x)
      } else {
        cases_that_match <- x %in% to_replace[[k]]
      }

    } else {

      if ( is.na( to_replace[k] ) ) {
        cases_that_match <- is.na(x)
      } else {
        cases_that_match <- x %in% to_replace[k]
      }

    }

    out[ cases_that_match ] <- replace_with[k]

  }

  return( out )
}


#### 4) Functions for miscellaneous ####

#### 4.1) empty_list ####
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
#' empty_list(3)
#'
#' # An empty list with labels
#' empty_list( 3, paste0( "S0", 1:3 ) )
#' @export

empty_list <- function(size, labels = NULL) {

  lst <- rep( list(NULL), size )

  if (!is.null(labels)) {
    if (length(labels) == length(lst)) {
      names(lst) <- labels
    } else {
      warning("Vector of labels does not match length of list")
    }
  }

  return(lst)
}

#### 4.2) every ####
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
#' every(1:10)
#' # Extract every other value
#' # at even positions
#' every(1:10, , 2) # Note double commas
#'
#' # Extract every 3rd value starting
#' # from 6th position
#' every(1:12, 3, 6)
#'
#' # Replace values at even
#' # positions with 0
#' x <- 1:10
#' every(x, , 2) <- 0
#' x
NULL

#### 4.2.1) `every` ####
#' @rdname every
#' @export

every <- function(x, step = 2, start = 1) {
  return(x[seq(start, length(x), step)])
}

#### 4.2.2) `every<-` ####
#' @rdname every
#' @export

`every<-` <- function(x, value, step = 2, start = 1) {
  x[seq(start, length(x), step)] <- value

  return(x)
}

#### 4.3) find_increment ####
#' Find Increment Over Range of Values
#'
#' Given a range of values and a desired divisor,
#' determines the rounded increment to use.
#' Useful, for example, to determine the
#' equally-spaced intervals to use for
#' a figure's axes.
#'
#' @param x A numeric vector of values.
#' @param n An integer, the divisor. If not specified,
#'   uses the number of standard deviations instead.
#'
#' @return A named numeric value, the rounded increment
#'   to iterate over the specified number of times,
#'   with the estimated place to round to as a name.
#'
#' @examples
#' x <- rnorm(100)
#' find_increment(x)
#' find_increment(x, 6)
#'
#' @export

find_increment <- function(x,
                           n = NULL ) {

  r <- range(x, na.rm = TRUE)
  i <- diff(r)

  if ( is.null(n) ) {
    s <- sd(x, na.rm = TRUE)
    n <- round( i/s )
  }

  b10 <- round( log( i/n, base = 10 ) )

  if ( b10 < 0 ) {
    inc <- round( i/n, abs(b10) )
  }

  if ( b10 == 0 ) {
    inc <- round( i/n )
  }

  if ( b10 > 0 ) {
    d <- 10^b10
    inc <- round( (i/n)/d )*d
  }

  names(inc) <- b10

  return(inc)
}


#### 4.4) lin ####
#' Create Linear Sequence of Evenly Spaced Values
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
#' lin(0, 1, 5)
#' @export

lin <- function(start, end, n_intervals) {
  return(seq(start, end, length.out = n_intervals))
}

#### 4.5) new_limits ####
#' Rescale Values to Have New Limits
#'
#' A function that rescales a vector of
#' values to have a new minimum and maximum.
#'
#' @param x A vector of numeric values.
#' @param lower The new lower limit or minimum value.
#' @param upper The new upper limit or maximum value.
#' @param ... Additional parameters for the
#'   \code{\link[base:max]{min}} and
#'   \code{\link[base:max]{max}} functions.
#'
#' @return A vector of rescaled values.
#'
#' @examples
#' x <- 1:3
#' new_limits( x, 0, 1 )
#'
#' @export

new_limits <- function( x, lower, upper, ... ) {

  mn <- min(x, ... )
  mx <- max( x, ... )

  x_unit <- (x - mn) / (mx - mn)

  out <- x_unit*(upper-lower) + lower

  return( out )
}

#### 4.6) over ####
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
#' for (i in 1:3) {
#'   print(y[over(1:3, i)])
#' }
#'
#' # Pull first 2 of each 3 values per iteration
#' # using the 'per' argument
#' for (i in 1:3) {
#'   print(y[over(1:2, i, per = 3)])
#' }
#'
#' # Pull last of 3 values per iteration
#' # using the 'per' argument
#' for (i in 1:3) {
#'   print(y[over(3, i, per = 3)])
#' }
#'
#' # Pull 2 values for final 2 sets using the
#' # 'adj' argument
#' y <- 1:8
#' for (i in 1:2) {
#'   print(y[over(1:2, i, adj = 1)])
#' }
#' @export

over <- function(x, iter,
                 per = NULL,
                 adj = -1) {

  # Ensure inputs are integers
  x <- as.integer(round(x))

  if (is.null(per)) {
    per <- max(x)
  }

  return(x + per * (iter + adj))
}

#### 4.7) print_table ####
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
#' data("mtcars")
#' tbl <- aggregate(mtcars$mpg, mtcars[, c("cyl", "vs")], mean)
#' tbl$x <- round(tbl$x, 1)
#' colnames(tbl) <- c("# of cylinders", "Engine type", "Miles per gallon")
#' print_table(tbl)
#' @export

print_table <- function(tbl, return = F) {

  # Initialize output
  out <- matrix(" ", nrow(tbl) + 1, ncol(tbl))

  column_names <- colnames(tbl)

  to_remove <- c(
    'PRD',
    'OUT',
    'COL'
  )

  # Loop over elements to remove
  for ( i in seq_along( to_remove ) ) {

    column_names <- gsub(
      to_remove[i], '', column_names
    )

    # Close 'Loop over elements to remove'
  }

  mat_find_replace <- rbind(
    c( 'OPP', '(' ), # 1
    c( 'CLP', ')' ), # 2
    c( 'OPB', '[' ), # 3
    c( 'CLB', ']' ), # 4
    c( 'HPH', '-' ), # 5
    c( 'CLN', ':' ), # 6
    c( 'SMC', ';' ), # 7
    c( 'PPE', '|' ), # 8
    c( 'PLS', '+' ), # 9
    c( 'EQL', '=' ), # 10
    c( 'SLS', '/' ), # 11
    c( 'HSH', '#' ), # 12
    c( 'PRC', '%' ), # 13
    c( 'AMP', '&' ), # 14
    c( 'LSS', '<' ), # 15
    c( 'GRT', '>' ), # 16
    c( 'LTE', '\U2264' ), # 17
    c( 'GTE', '\U2265' ), # 18
    c( 'SP2', '\U00B2' ), # 19
    c( 'ZZ', ' ' ) # 20
  )

  for ( i in 1:nrow( mat_find_replace ) ) {
    column_names <- gsub(
      mat_find_replace[i, 1], mat_find_replace[i, 2],
      column_names, fixed = TRUE
    )
  }

  #< Loop over columns
  for (i in 1:ncol(tbl)) {

    # Determine maximum number of characters for elements
    # in the table's column (including the column name)
    nc <- max(c(
      sapply(as.character(tbl[[i]]), nchar),
      nchar(column_names[i])
    ))

    #<| Loop over rows
    for (j in 1:(nrow(tbl) + 1)) {

      if (j > 1) {
        # Elements in column
        val <- as.character(tbl[[i]])[j - 1]
      } else {
        # Column name
        val <- column_names[i]
      }

      # Current number of characters
      cur_nc <- nchar(val)

      #<|< Pad with empty spaces
      if (cur_nc < nc) {
        val <- paste(paste(rep(" ", nc - cur_nc), collapse = ""),
                     val,
                     sep = ""
        )
        out[j, i] <- val
        #>|> Close conditional 'Pad with empty spaces'
      } else {
        out[j, i] <- val
        #>|> Close conditional 'Pad with empty spaces'
      }

      #|> Close loop 'Loop over rows'
    }

    #> Close loop 'Loop over columns'
  }

  # Convert to vector of strings
  output <- apply(out, 1, paste, collapse = " | ")
  output <- sapply(output, function(x) paste(x, "|"))

  if (!return) {
    for (i in 1:length(output)) {
      cat(c(output[i], "\n"))
    }
  } else {
    return(output)
  }
}

#### 4.8) runs_in_sequence ####
#' Determine Runs in a Sequence
#'
#' Given a sequence of values of which
#' a subset are dubbed 'hits', determine
#' the number of runs of hits and the
#' start and end of each run of hits.
#'
#' @param x A vector of values.
#' @param codes_for_run A vector of the values
#'   in \code{x} indicating a hit.
#'
#' @return A list with a) the total number of runs, and
#' b) a matrix with a column for the start position of
#' each run and a column for the end position of each
#' run.
#'
#' @examples
#' # Generate a sequence of zeros and ones
#' x <- rbinom( 10, 1, .5 )
#' print(x)
#' # Determine runs of ones
#' runs_in_sequence( x )
#'
#' @export

runs_in_sequence <- function( x, codes_for_hit = 1 ) {

  # Number of observations
  n <- length( x )

  # Initialize variables to track
  # start and end of each run
  run_start = rep( NA, n )
  run_end = rep( NA, n )

  # Indicator for start of each run
  new_run <- FALSE

  # Convert to FALSE/TRUE
  positive_cases <- x %in% codes_for_hit

  inc <- 0 # Variable for indexing starts of run

  # Loop over observations
  for ( i in 1:n ) {

    # Check if a run is happening
    is_true <- positive_cases[i]

    # If not 1st observation
    if ( i > 1 ) {

      # Check if a new run is occuring
      if ( is_true & (positive_cases[i-1] != is_true) ) {
        new_run <- TRUE
        inc <- inc + 1
      }

    } else {
      if ( is_true ) {
        new_run <- TRUE
        inc <- inc + 1
      }
    }

    if ( new_run ) {
      # If start of new run
      # record position
      run_start[inc] <- i
      run_end[inc] <- i
    } else {
      # if run is continuing
      if ( is_true ) {
        run_end[inc] <- i
      }

    }

    # Reset tracker for new runs
    new_run <- FALSE
  }

  # Remove missing cases
  run_start <- run_start[ !is.na( run_start ) ]
  run_end <- run_end[ !is.na( run_end ) ]

  n_runs <- length( run_start )

  out <- list(
    n_runs = n_runs,
    sequences = cbind( Start = run_start, End = run_end )
  )

  return( out )
}

#### 4.9) data_first ####
#' Adapt Functions to Take Data Argument First
#'
#' A function that ensures the first argument is
#' always for 'data' - allows greater compatibility
#' with the pipe operator.
#'
#' @param data_ob An R object to pass to a given
#'   function's \code{data} argument.
#' @param fun_to_apply An R function that has a
#'   \code{data} argument (e.g., [stats::lm]).
#' @param ... Additional arguments for the given function.
#'
#' @returns The associated output for the \code{fun_to_apply}
#' function.
#'
#' @examples
#' data( 'mtcars' )
#' lm_fit <- data_first( mtcars, lm, formula = mpg ~ cyl )
#' print( lm_fit )
#'
#' @export

data_first <- function( data_obj, fun_to_apply, ... ) {

  return(
    fun_to_apply( ..., data = data_obj )
  )

}

#### 4.10) group_index ####
#' Create Index Over Groupings
#'
#' Create a numeric index over the unique levels of a
#' variable or a set of variables.
#'
#' @param ... Vectors of equal length.
#' @param levels A list with the order of the unique levels for
#'   each input vector (indices assigned from first to last level).
#'
#' @returns An integer vector from 1 to the number of unique levels.
#'
#' @examples
#' # Convert to numeric index
#' group_index( rep( LETTERS[3:1], each = 3 ) )
#'
#' # Can control assignment of indices
#' group_index(
#'   rep( LETTERS[3:1], each = 3 ), levels = list( c( 'C', 'B', 'A' ) )
#' )
#'
#' # Can create single index over all
#' # unique combinations of multiple variables
#' group_index( rep( LETTERS[1:3], each = 3 ), rep( 1:3, 3 ) )
#'
#' @export

group_index <- function( ..., levels = NULL ) {

  # Extract inputs
  lst_arg <- list(...)

  # Number of inputs
  n_arg <- length( lst_arg )

  # Length of each input
  l <- sapply(
    1:n_arg, function(i) length( lst_arg[[i]] )
  )

  # Lengths must be equal
  if ( !all( l %in% l[1] ) ) {

    stop( 'Vectors must be of equal length' )

    # Close 'Lengths must be equal'
  }

  mat_index <- matrix( NA, l[1], n_arg )

  # Loop over inputs
  for ( i in 1:n_arg ) {

    # If levels not specified
    if ( is.null(levels) ) {

      mat_index[, i] <- as.numeric( as.factor( lst_arg[[i]] ) )

      # Close 'If levels not specified'
    } else {

      mat_index[, i] <- as.numeric(
        factor( lst_arg[[i]], levels = levels[[i]] )
      )

      # Close else for 'If levels not specified'
    }

    # Close 'Loop over inputs'
  }

  # Create one collapsed index over all unique combinations
  collapsed_index <- apply(
    mat_index, 1, function(x) {
      paste( x, collapse = '.' )
    }
  )

  return(
    as.numeric( as.factor( collapsed_index ) )
  )

}

#### 5) Functions for strings ####

#### 5.1) align_strings ####
#' Pad Strings to be the Same Length
#'
#' Function that pads strings to be the same
#' length, with padding applied either to the
#' left or right-hand side.
#'
#' @param strings A character vector.
#' @param left Logical; if \code{TRUE}
#'   left-aligns strings, otherwise
#'   right-aligns strings.
#'
#' @return A character vector.
#'
#' @examples
#' # Strings of unequal length
#' x <- c( "A", "BB", "CCC" )
#'
#' # Left-aligned
#' s <- align_strings( x )
#' message( paste0( '|', s, '|\n' ) )
#'
#' # Right-aligned
#' s <- align_strings( x, FALSE )
#' message( paste0( '|', s, '|\n' ) )
#'
#' @export

align_strings <- function( strings, left = TRUE ) {

  nc <- nchar( strings )
  mx <- max( nc )

  padding <- sapply( mx - nc, function(v) {
    if ( v > 0 ) {
      return( squish( rep( " ", v ) ) )
    } else {
      return( "" )
    }
  } )

  if ( left ) {
    return( paste0( strings, padding ) )
  } else {
    return( paste0( padding, strings ) )
  }

}

#### 5.2) format_numbers ####
#' Pad Numeric Values to be the Same Length
#'
#' Function that pads numeric values to be
#' the same length by adding spaces to the
#' left-hand side and trailing zeros after
#' the decimal place.
#'
#' @param x A numeric vector.
#'
#' @return A character vector.
#'
#' @examples
#' # Decimal values
#' message( paste0( format_numbers( round( rnorm( 3 ), 2 ) ), "\n" ) )
#' # Whole numbers
#' message( paste0( format_numbers( rbinom( 3, 100, .1 ) ), "\n" ) )
#'
#' @export

format_numbers <- function( x ) {

  xchr <- as.character( x )

  any_decimals <- any( grepl( ".", xchr, fixed = TRUE ) )

  if ( any_decimals ) {

    lhs <- sapply(
      xchr, function(s) {
        strsplit(
          s, split = ".", fixed = TRUE
        )[[1]][1]
      }
    )

    rhs <- sapply(
      xchr, function(s) {
        strsplit(
          s, split = ".", fixed = TRUE
        )[[1]][2]
      }
    )

    if ( any( is.na( rhs ) ) ) {
      rhs[ is.na( rhs ) ] <- "0"
    }

    nc <- nchar( lhs )
    mx <- max( nc )

    padding <- sapply( mx - nc, function(v) {
      if ( v > 0 ) {
        squish( rep( " ", v ) )
      } else {
        return( "" )
      }
    } )

    lhs <- paste0( padding, lhs )

    nc <- nchar( rhs )
    mx <- max( nc )

    padding <- sapply( mx - nc, function(v) {
      if ( v > 0 ) {
        squish( rep( "0", v ) )
      } else {
        return( "" )
      }
    } )

    rhs <- paste0( rhs, padding )

    return( paste0( lhs, ".", rhs ) )

  } else {

    nc <- nchar( xchr )
    mx <- max( nc )

    padding <- sapply( mx - nc, function(v) {
      if ( v > 0 ) {
        squish( rep( " ", v ) )
      } else {
        return( "" )
      }
    } )

    lhs <- paste0( padding, xchr )

    return( lhs )

  }

}

#### 5.3) replace_string ####
#' Replace String Contents
#'
#' Function that replaces a specified pattern found
#' within a string (or vector of strings) with
#' a user-specified pattern.
#'
#' @param s A character vector.
#' @param to_replace A character vector, the patterns to
#'   match and replace within each string.
#' @param replace_with An optional character vector,
#'   either of length one or of matching length to
#'   \code{to_replace}, the patterns to substitute.
#'
#' @return A character string.
#'
#' @examples
#' # Example string
#' x <- c( 'AA', 'AB', 'AC', 'DD' )
#' # Remove the letter 'A'
#' replace_string( x, 'A' )
#' # Replace the letter 'A' with '1'
#' replace_string( x, 'A', '1' )
#' # Replace multiple letters
#' replace_string( x, c( 'B', 'C' ), c( '1', '2' ) )
#'
#' @export

replace_string <- function( s, to_replace, replace_with = '' ) {

  if ( length( replace_with ) == 1 ) {
    replace_with <- rep( replace_with, length( to_replace ) )
  }


  n_cases <- length( to_replace )
  out <- s

  for ( i in 1:n_cases ) {
    out <- gsub( to_replace[i], replace_with[i], out, fixed = TRUE )
  }

  return( out )
}

#### 5.4) squish ####
#' Collapse a Character Vector
#'
#' Function that combines elements of a
#' character vector into a single string.
#'
#' @param chr_vec A character vector.
#' @param between The value to use as spacing between
#'   each element in \code{chr_vec}.
#'
#' @return A character string.
#'
#' @examples
#' # Collapse a character vector
#' print( squish( c( "A", "B", "C" ) ) )
#'
#' # Collapse a characcter vector with custom spacing
#' print( squish( c( "1", "2", "3" ), " + " ) )
#'
#' @export

squish <- function( chr_vec, between = "" ) {

  return( paste( chr_vec, collapse = between ) )

}

#### 6) Functions for vectors ####

#### 6.1) not_NA ####
#' Return a Logical Vector With NA Values set to FALSE
#'
#' Function that takes a logical vector and ensures
#' any \code{NA} values are set to \code{FALSE}.
#'
#' @param x A logical vector.
#'
#' @return A logical vector with \code{NA} cases
#' set to \code{FALSE}.
#'
#' @examples
#' not_NA( c(TRUE, FALSE, NA) )
#'
#' @export

not_NA <- function( lgc ) {

  return( lgc & !is.na(lgc) )

}

#### 6.2) percent_that_sums_to_100 ####
#' Compute Percentage That Sums to 100%
#'
#' Function that uses the largest remainder method to
#' ensure that a set of percentages sum to 100%
#' even in the presence of rounding error.
#'
#' @param x A vector of proportions or frequencies
#'   that are suppose to sum to 100%.
#' @param digits The number of digits to round to.
#' @param freq Logical; if \code{TRUE} assumes \code{x}
#'   is a vector of frequencies, otherwise assumes
#'   \code{x} is a vector of proportions.
#'
#' @return A vector of percentages that sum to 100%.
#'
#' @examples
#' x <- c( 9990, 5, 5 )
#' # Convert to percentage and round
#' p <- round( 100*x/sum(x), 1 )
#' # No longer sums to 100% due to rounding error
#' print( sum(p) )
#' # Adjust percentages using the
#' # largest remainder method so
#' # they sum to 100%
#' print( percent_that_sums_to_100( p/100 ) )
#' # Works with frequencies as well
#' print( percent_that_sums_to_100( x, freq = TRUE ) )
#'
#' @export

percent_that_sums_to_100 <- function( x, digits = 1, freq = FALSE ) {

  # Convert to proportion
  if ( freq ) {

    p <- x / sum(x)

    # Close 'Convert to proportion'
  } else {

    p <- x

    # Close else for 'Convert to proportion'
  }

  # Number of frequencies
  n <- length(x)

  # Implement largest remainder method
  # so percentages sum to 100
  Nz <- 100 * 10^digits
  lrm <- p * Nz
  r <- Nz - sum( floor( lrm ) )
  a <- rep( 0, n )
  a[ 1:r ] <- 1
  o <- order( lrm - floor( lrm ), decreasing = TRUE )
  lrm[o] <- floor( lrm[o] ) + a[o]

  return( 100*(lrm / Nz) )
}

#### 6.3) strip_value ####
#' Strip Vector of a Value
#'
#' Removes a specified value from a vector.
#'
#' @param x A vector of values.
#' @param value The value to remove (defaults
#'   to \code{NA})
#'
#' @return A vector of values sans the one removed.
#'
#' @examples
#' x <- c( 1, 2, NA, 3 )
#' print( strip_value(x) )
#'
#' x <- c( 'Hello', '', 'world' )
#' print( strip_value(x, '') )
#'
#' @export

strip_value <- function( x, value = NA ) {

  # If removing NA
  if ( is.na( value ) ) {

    out <- x[ !is.na(x) ]

    # Close 'If removing NA'
  } else {

    out <- x[ !( x %in% value ) ]

    # Close else for 'If removing NA'
  }

  return( out )
}

#### 7) Functions for writing code ####

#### 7.1) create_table_of_contents ####
#' Create a Table of Contents
#'
#' Given a path to an R script, creates a
#' table of contents by searching for
#' section headers of the form:
#' \code{#### X) Label ####} where \code{X} is a
#' numbering scheme (e.g.,1, 2.1, 3.2.1)
#' with periods denoting nested sub-sections.
#'
#' @param file_path An absolute path to the
#'   R script.
#'
#' @return Outputs a message to the R console
#' window with the table of contents.
#'
#' @export

create_table_of_contents <- function( file_path ) {

  script_code <- scan(
    file = file_path,
    what = 'character', sep = '\n',
    quiet = TRUE
  )

  four_asterisks <- sapply(
    script_code,
    function(x) grepl( '####', x, fixed = T )
  )

  section_headers <- script_code[ four_asterisks ]

  section_headers <-
    gsub( '#### ', '', section_headers, fixed = TRUE )
  section_headers <-
    gsub( ' ####', '', section_headers, fixed = TRUE )

  pull_num <- function( s ) {
    out <- strsplit( s, split = ')', fixed = T )[[1]][1]
    out <- gsub( " ", "", out, fixed = T )
    out <- paste0( out, ')' )

    n_periods <- sum(
      grepl(
        ".", strsplit( out, split = "" )[[1]], fixed = T
      )
    )
    if ( n_periods > 0 ) {
      out <- paste0(
        paste( rep( "  ", n_periods ), collapse = "" ),
        out
      )
    }

    return( out )
  }
  section_labels <- sapply(
    section_headers,
    function(s) strsplit( s, split = ')', fixed = T )[[1]][2]
  )

  out <- paste0(
    '# ',
    sapply( section_headers, pull_num ),
    section_labels,
    '\n'
  )
  out <- c( '# Table of contents\n', out )

  message( out )
}

#### 7.2) create_vector ####
#' Create Code for Vector
#'
#' Function to extract unique elements of
#' a vector and output example code to the
#' console.
#'
#' @param x A vector of elements.
#' @param inner A character vector with [1] the
#'   string to add before each element and [2]
#'   the string to add after each element.
#' @param outer A character with [1] the
#'   string to add before all elements and
#'   [2] the string to add after all elements.
#'
#' @returns Example code in the console.
#'
#' @examples
#' create_vector( LETTERS[1:3] )
#'
#' @export

create_vector <- function( x,
                           inner = c( '  "', '",\n' ),
                           outer = c( 'c(\n', ')' ),
                           na.rm = TRUE ) {

  if ( na.rm ) {
    x <- x[ !is.na(x) ]
  }

  u <- sort( unique( x ) )

  out <- paste0( inner[1], u, inner[2] )
  if ( inner[2] == '",\n' ) {
    out[ length(out) ] <- gsub(
      '",\n', '"\n', out[ length(out) ], fixed = TRUE
    )
  }
  cat( c( outer[1], out, outer[2] ) )
}

#### 7.3) section ####
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
#' section("1")
#' # Periods indent output by 2 spaces
#' section("1.1")
#' section("1.1.1")
#' @export

section <- function(x, run = TRUE,
                    spacing = ".",
                    end = ")") {
  n_spacers <- sum(
    grepl(
      spacing,
      strsplit(x, split = "", fixed = TRUE)[[1]],
      fixed = TRUE
    )
  )

  if (grepl(")", x, fixed = TRUE)) end <- ""

  indent <- ""
  if (n_spacers > 0) {
    indent <- paste(rep("  ", n_spacers), collapse = "")
  }

  if (run) {
    message(paste0(indent, x, end))
  }
}

#### 7.4) templates ####
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
#'     \item 'Loop' (a \code{for} loop statement);
#'     \item 'Conditional' (a \code{if} statement);
#'     \item 'recode' (values for the 'dplyr'
#'       function \code{\link[dplyr]{recode}}).
#'   }
#' @param val An optional character vector to be used
#'   with \code{type = 'recode'}.
#'
#' @examples
#' # List of possible inputs to argument
#' # 'type' for each template
#' templates()
#'
#' # Function documentation
#' templates("Function")
#'
#' # Header for R script
#' templates("Header")
#'
#' # Progress bar for loop
#' templates("Progress")
#'
#' # Loop
#' templates("Loop")
#'
#' # If ... else statement
#' templates("Conditional")
#'
#' # Roxygen2 function documentation
#' templates("roxygen")
#'
#' # HTML internal links
#' templates("html_links")
#'
#' # Example table for specifying
#' # nomenclature and a glossary
#' # in a .Rmd file
#' templates("rmd_glossary")
#'
#' @export

templates <- function(type = NULL, val = NULL) {

  types <- list(
    function_documentation = c(
      "Function", "function",
      "Func", "func", "Fun", "fun",
      "FD", "fd",
      "Arguments", "arguments",
      "Arg", "arg",
      "1"
    ),
    script_header = c(
      "Header", "header",
      "Head", "head",
      "Script", "script",
      "SD", "sd",
      "2"
    ),
    progress_bar = c(
      "Progress", "progress",
      "Prog", "prog",
      "3"
    ),
    loop = c(
      "Loop", "loop",
      "for", "4"
    ),
    conditional = c(
      "Conditional", "conditional",
      "if", "5"
    ),
    roxygen_documentation = c(
      'Roxygen2', 'Roxygen', 'roxygen', 'roxygen2',
      'roxy',
      '6'
    ),
    recode = c(
      "recode", "7"
    ),
    html_links = c(
      "HTML links", "html links",
      "HTML link", "html link",
      "Internal links", "internal links",
      "Internal link", "internal link",
      "8"
    ),
    rmd_glossary = c(
      "Rmd glossary", "rmd glossary",
      "Nomenclature and glossary",
      "nomenclature and glossary",
      "nom-gloss",
      "9"
    )
    # plot_function = c()
  )

  if (is.null(type)) {

    message( 'Available template options:' )

    for (i in 1:length(types)) {
      message( paste0( '  - ', types[[i]][1] ) )
    }
    type <- ''
  }

  if (type %in% types$function_documentation) {
    string <- paste0(
      "# Title \n",
      "# \n",
      "# ... \n",
      "# \n",
      "# @param 'obj_x' An R object. \n",
      "# \n",
      "# @details \n",
      "# Prerequisites: \n",
      "#   * The R package '?' (version ?) \n",
      "# \n",
      "# @returns ... \n",
      "# \n",
      "# @examples \n",
      "# Forthcoming \n"
    )

    message(string)
  }

  if (type %in% types$script_header) {
    author <- getOption("arfpam.author")
    email <- getOption("arfpam.email")

    if (is.null(author)) {
      author <- "Kevin Potter"
    }
    if (is.null(email)) {
      email <- "kevin.w.potter@gmail.com"
    }

    string <- paste0(
      "# Title\n",
      "# Written by ", author, "\n",
      "# email: ", email, "\n",
      "# Please email me directly if you \n",
      "# have any questions or comments\n",
      "# Last updated ", Sys.Date(), "\n",
      "\n",
      "# Table of contents\n",
      "# 1)"
    )

    message(string)
  }

  if (type %in% types$progress_bar) {
    string <- paste0(
      "n_cases <- 10\n",
      "# Create a progress bar using a base R function\n",
      "pb <- txtProgressBar( min = 1, max = n_cases, style = 3 )\n",
      "\n",
      "# Loop over cases\n",
      "for (i in 1:n_cases) {\n",
      "  # Update the progress bar\n",
      "  setTxtProgressBar(pb,i)\n",
      "  # Close 'Loop over cases'\n",
      "}\n",
      "close(pb)\n"
    )

    message(string)
  }

  if (type %in% types$loop) {
    string <- paste0(
      "# Descriptor\n",
      "for (i in 1:n) {\n",
      "  # Do something\n",
      "  # Close 'Descriptor'\n",
      "}"
    )

    message(string)
  }

  if (type %in% types$conditional) {
    string <- paste0(
      "# Descriptor\n",
      "if (value %in% values) {\n",
      "  # Do something\n",
      "  # Close 'Descriptor'\n",
      "} else {\n",
      "  # Do something else\n",
      "  # Close else for 'Descriptor'\n",
      "}"
    )

    message(string)
  }

  if (type %in% types$roxygen_documentation) {
    string <- paste0(
      "#' Title\n",
      "#' \n",
      "#' Description.\n",
      "#' \n",
      "#' @param x ...\n",
      "#' \n",
      "#' @details\n",
      "#' \n",
      "#' @returns Output.\n",
      "#' \n",
      "#' @examples\n",
      "#' # Examples\n",
      "#' \n",
      "#' @export\n",
      "\n"
    )

    message(string)
  }

  if (type %in% types$html_links) {

    string <- paste0(
      '<a name="SXX"></a>\n',
      '<a href="#SXX">LINK</a>\n',
      '<a name="SXX-PXX"></a>\n',
      '<a href="#SXX-PXX">LINK</a>\n',
      '<a href="#TOC">&#129145;</a> <a href="#END">&#129147;</a>\n'
    )

    message(string)
  }

  if (type %in% types$rmd_glossary) {

    string <- paste0(
      '### 1. Nomenclature and glossary\n\n',
      '```{r S01-nom-gloss,echo=FALSE}\n',
      'nom_gloss <- rbind(\n',
      '  c("Example term", "ET", "Notes on term" )\n',
      ')\n',
      'colnames( nom_gloss ) <- "C" %p% 1:3 \n',
      'nom_gloss <- data.frame( nom_gloss )\n\n',
      'th <- create_header( nom_gloss )\n',
      'th$colA <- c(\n',
      '  "Term",\n',
      '  "Abbreviation",\n',
      '  "Comments"\n',
      ')\n\n',
      'ft <- create_ft( nom_gloss, th, alternate_col = "grey95" )\n',
      'ft\n',
      '```\n'
    )

    message(string)
  }

  if (type %in% types$recode & !is.null( val ) ) {

    string <- paste0(
      "`", val, "` = '',\n"
    )
    string[ length( string ) ] <-
      gsub( ",", "", string[ length( string ) ], fixed = TRUE )
    string <- c(
      "recode(\n  x,\n",
      paste0( "  ", string ),
      ")"
    )
    message( paste( string, collapse = "" ) )
  }

}

