# Utilities
# Written by Kevin Potter
# email: kevin.w.potter@gmail.com
# Please email me directly if you
# have any questions or comments
# Last updated 2022-01-13

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
# 9) Functions for files
#   9.1) find_file_name
#   9.2) make_file_name
#   9.3) path_to_file
#   9.4) source_R_scripts
#   9.5) load_R_object
# 10) Matching and assignment
#   10.1) func_for_list_of_matches
#   10.2) list_of_matches
#   10.3) assign_to_match
# 11) dnr
# 12) create_table_of_contents
# 13) runs_in_sequence
# 14) column
# 15) col_by_other
# 16) date_and_time
# 17) pull_id

# TO DO
# - Add Custom tests for file/folder functions
# - Add unit tests for functions

#### 1) over ####
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

#### 2) templates ####
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
      "# @param 'x' ... \n",
      "# \n",
      "# @details ... \n",
      "# \n",
      "# @return ... \n",
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
      "#' @return Output.\n",
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

#### 3) section ####
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

#### 4) every ####
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

#### 4.1) `every` ####
#' @rdname every
#' @export

every <- function(x, step = 2, start = 1) {
  return(x[seq(start, length(x), step)])
}

#### 4.2) `every<-` ####
#' @rdname every
#' @export

`every<-` <- function(x, value, step = 2, start = 1) {
  x[seq(start, length(x), step)] <- value

  return(x)
}

#### 5) has_NA ####
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

#### 6) print_table ####
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

  #< Loop over columns
  for (i in 1:ncol(tbl)) {

    # Determine maximum number of characters for elements
    # in the table's column (including the column name)
    nc <- max(c(
      sapply(as.character(tbl[[i]]), nchar),
      nchar(colnames(tbl)[i])
    ))

    #<| Loop over rows
    for (j in 1:(nrow(tbl) + 1)) {

      if (j > 1) {
        # Elements in column
        val <- as.character(tbl[[i]])[j - 1]
      } else {
        # Column name
        val <- colnames(tbl)[i]
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

#### 7) lin ####
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

#### 8) empty_list ####
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
#' empty_list(3, c("S01", "S02", "S03"))
#' @export

empty_list <- function(size, labels = NULL) {
  lst <- lapply(1:size, function(x) {
    return(NULL)
  })

  if (!is.null(labels)) {
    if (length(labels) == length(lst)) {
      names(lst) <- labels
    } else {
      warning("Vector of labels does not match length of list")
    }
  }

  return(lst)
}

#### 9) Functions for files ####

#### 9.1) find_file_name ####
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
#' find_file_name("every", output = "vector")
#' find_file_name("every", output = "index")
#' find_file_name("every", output = "name")
#' @export

find_file_name <- function(string,
                           output = "logical",
                           ...) {

  # All files and folders present
  # in working directory
  all_files <- dir(...)

  # Determine if (standard) file name is present
  # in list of files/folders
  check <- grepl(string, all_files, fixed = T)

  # Output
  if (output %in% c("Logical", "logical", "L", "l")) {
    return(any(check))
  }
  if (output %in% c("Vector", "vector", "vec", "V", "v")) {
    return(check)
  }
  if (output %in% c("Index", "index", "I", "i")) {
    return(which(check))
  }
  if (output %in% c("Name", "name", "N", "n")) {
    if (any(check)) {
      return(all_files[check])
    } else {
      return(NULL)
    }
  }
}

#### 9.2) make_file_name ####
#' Create Formatted File Name
#'
#' Create a standardized file name of the
#' form: TXX-Description-MM_DD_YYYY.ext where 'T'
#' is a leading tag, 'XX' is a file number,
#' 'Description' is a human-readable
#' label, and 'ext' is a file extension.
#'
#' @param description A human-readable label, with
#'   words preferably separated by underscores.
#' @param extension A file extension, such as
#'   'RData', 'R', 'txt', 'pdf'.
#' @param tag A leading tag; If no value is provided,
#'   automatically set based on the file extension.
#'   Automatic assignments are...
#'   \itemize{
#'     \item 'S' for extension \code{R};
#'     \item 'D' for extensions \code{RData} and \code{csv};
#'     \item 'T' for extension \code{txt};
#'     \item 'W' for extension \code{docx};
#'     \item 'P' for extension \code{pptx};
#'     \item 'F' for extensions \code{pdf}, \code{jpg},
#'     \code{jpeg}, and \code{png}.
#'   }
#' @param number A file number. If no value is provided,
#'   automatically set based on number of files in current
#'   folder with matching tags.
#' @param file_date The date to include. If no value is
#'   provided, the current date is used.
#' @param additional Additional text to include following
#'   the date. If provided, is preceded by a '-'.
#' @param date_format The format to use for the current
#'   date, defaults to 'MM_DD_YYYY'.
#' @param exclude A vector of file names to exclude when
#' @param remove Logical; if TRUE, attempts to locate
#'   previous versions of the outputted file name
#'   (i.e., same name but earlier dates) and remove
#'   them from the current folder.
#'
#' @return A character string.
#'
#' @examples
#' # Different file types
#' make_file_name("Example", "RData")
#' make_file_name("Example", "pdf")
#' make_file_name("Example", "docx")
#'
#' # User-specified tags and numbers
#' make_file_name("Example", "RData", tag = "R", number = "02")
#' # Additional text
#' make_file_name("Example", "RData", additional = "v.1.0.0")
#' @export

make_file_name <- function(description,
                           extension,
                           tag = NULL,
                           number = NULL,
                           file_date = NULL,
                           additional = NULL,
                           date_format = "%m_%d_%Y",
                           exclude = "",
                           remove = FALSE) {

  # Determine files in directory
  all_files <- dir()

  # If not specified, auto-generate file tag
  # based on extension
  if (is.null(tag)) {

    # Word document
    if (extension == "docx") {
      tag <- "W"
    }
    # PowerPoint
    if (extension == "pptx") {
      tag <- "P"
    }
    # Standard figure extensions
    if (extension %in% c("pdf", "jpg", "jpeg", "png")) {
      tag <- "F"
    }
    # Data files
    if (extension %in% c("RData", "csv")) {
      tag <- "D"
    }
    # R script file
    if (extension %in% c("R")) {
      tag <- "S"
    }
    # Text file
    if (extension %in% c("txt")) {
      tag <- "T"
    }
  }

  # If not specified, auto-generate file_date
  if (is.null(file_date)) {
    file_date <- format(
      Sys.Date(),
      date_format
    )
    file_date <- "-" %p% file_date
  } else {
    if (file_date != "") {
      file_date <- "-" %p% file_date
    }
  }

  # Check for matching tags and descriptions for
  # files present in folder
  if (length(all_files) > 0) {
    only_files_no_placeholder <-
      # Exclude folders
      grepl(".", all_files, fixed = T) &
        # Exclude user-specified files
        !all_files %in% exclude

    matching_tags <-
      substr(all_files, start = 1, stop = 1) == tag &
        only_files_no_placeholder

    matching_description <-
      grepl("-" %p% description %p% "-", all_files, fixed = T) &
        only_files_no_placeholder

    matching_extension <-
      grepl(extension %p% "$", all_files) &
        only_files_no_placeholder

    # Check for existing file
    found_match <-
      matching_description &
        matching_tags &
        matching_extension

    # If needed, increment file number
    if (is.null(number)) {
      if (any(found_match)) {
        number <- substr(
          all_files[found_match],
          start = 2, stop = 3
        )
      } else {
        number <- sum(matching_tags) + 1
      }

      # Make sure number is at least a double-digit and
      # convert to character string
      nc <- nchar(number)
      if (nc == 1) {
        number <- paste0("0", number)
      } else {
        number <- as.character(number)
      }
    }

    if (remove) {
      if (found_match) {
        old_file <- all_files[found_match]
        file.remove(old_file)
      }
    }
  } else {
    if (is.null(number)) {
      number <- 1

      # Make sure number is at least a double-digit and
      # convert to character string
      nc <- nchar(number)
      if (nc == 1) {
        number <- paste0("0", number)
      } else {
        number <- as.character(number)
      }
    }
  }

  if (!is.null(additional)) {
    additional <- paste0("-", additional)
  } else {
    additional <- ""
  }

  # Generate file name
  filename <- paste0(
    tag,
    number,
    "-",
    description,
    file_date,
    additional,
    ".",
    extension
  )

  return(filename)
}

#### 9.3) path_to_file ####
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

#### 9.4) source_R_scripts ####
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

#### 9.5) load_R_object ####
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

#### 10) Matching and assignment ####

#### 10.1) func_for_list_of_matches ####
# Internal Function for 'list_of_matches'
#
# Internal function to be used in 'list_of_matches
# that mirrors `%in%` but can also test for NA
# values.
#
# @param categories
# @param vec
#
# @return A logical vector

func_for_list_of_matches <- function(categories,
                                     vec) {
  #< If any NA values
  if ( any( is.na( categories ) ) ) {

    # Detect NA values in 'vec'
    return( is.na( vec ) )

    #> Close conditional 'If any NA values'
  } else {

    # Determines matches with 'categories' in 'vec'
    return( vec %in% categories )

    #> Close else for 'If any NA values'
  }
}

#### 10.2) list_of_matches ####
#' Create List of Logical Vectors per Match
#'
#' Creates a list of logical vectors, one
#' for each match to an element in a list
#' of categories.
#'
#' @param x A vector or data frame.
#' @param categories A vector or list of values to match
#'   against \code{x}.
#' @param column If \code{x} is a data frame, the column
#'   name for the values to match against.
#'
#' @seealso \code{\link{apply_f_to_plot}}
#'
#' @return A list of logical vectors.
#'
#' @examples
#' # Use 'iris' species for example
#' data( "iris" ); dtf <- iris
#' dtf$Species <- as.character(dtf$Species)
#'
#' # Regroup 3 iris species into 2 categories
#' categories <- list( 'setosa', c( 'versicolor', 'virginica' ) )
#' lst <- list_of_matches( dtf$Species, categories )
#' # New categories versus original species
#' dtf$New <- 1; dtf$New[ lst[[2]] ] <- 2
#' aggregate( dtf$New, list( dtf$Species ), unique )
#'
#' # Alternate specification using 'column' argument
#' lst <- list_of_matches( dtf, categories, 'Species' )
#' dtf$New <- 1; dtf$New[ lst[[2]] ] <- 2
#' aggregate( dtf$New, list( dtf$Species ), unique )
#'
#' # Can match NA values
#' dtf$Species[ c( 1, 51, 101 ) ] <- NA
#' # Add NA to category list
#' categories <- c( categories, NA )
#' lst <- list_of_matches( dtf$Species, categories )
#' dtf$New <- 1; dtf$New[ lst[[2]] ] <- 2; dtf$New[ lst[[3]] ] <- 3
#' dtf$Species[ is.na( dtf$Species ) ] <- 'Was NA'
#' aggregate( dtf$New, list( dtf$Species ), unique )
#'
#' @export

list_of_matches <- function(x, categories, column = NULL) {

  #< If "x" is a vector
  if (is.null(column)) {
    return(lapply(categories, func_for_list_of_matches, vec = x))

    # > Close conditional 'If "x" is a vector'
  } else {
    return(lapply(categories, func_for_list_of_matches, vec = x[[column]]))

    # > Close else for 'If "x" is a vector'
  }
}

#### 10.3) ####
#' Create Vector with Values Assigned Based on a List of Matches
#'
#' This function takes a list of logical vectors equal to
#' \code{TRUE} for different subsets of a vector, and
#' generates a new vector with user-specified values based
#' on each subset from the list.
#'
#' @param values A vector of values equal in length to
#'   \code{matches}.
#' @param matches A list of logical vectors each of size N
#'   (see \code{\link{list_of_matches}}).
#' @param default The default value used to initialize the
#'   output vector.
#'
#' @seealso \code{\link{apply_f_to_plot}}
#'
#' @return A vector of size N.
#'
#' @examples
#' # Use 'iris' species for example
#' data( "iris" ); dtf <- iris
#'
#' # Regroup 3 iris species into 2 categories
#' categories <- list( 'setosa', c( 'versicolor', 'virginica' ) )
#' lst <- list_of_matches( dtf$Species, categories )
#' dtf$Category <- assign_by_match(1:2, lst)
#' aggregate( dtf$Category, list( dtf$Species ), unique )
#'
#' @export

assign_by_match <- function( values, matches, default = NA ) {

  out <- rep( default, length( matches[[1]] ) )

  #< Loop over matches
  for ( i in 1:length( matches ) ) {
    # Assign new value
    out[ matches[[i]] ] <- values[i]
    #> Close loop 'Loop over matches'
  }

  return( out )
}

#### 11) dnr ####
#' Do Not Run Multi-Line Code Segments
#'
#' Allows a user to write out multiple
#' lines of R code that will not be run.
#'
#' @param x Any syntactically valid \code{R} expression.
#'
#' @examples
#' # Will not run
#' dnr({
#'   print('Hello')
#'   print('world')
#' })
#'
#' # Can be used to also create
#' # multi-line commments
#' dnr("
#' Here are multiple
#' lines of text
#' ")
#'
#' @export

dnr <- function(x) {
  if ( FALSE ) {
    substitute(x)
  }
}

#### 12) create_table_of_contents ####
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

#### 13) runs_in_sequence ####
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

#### 14) column ####
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

#### 15) col_by_other ####
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
#' col_by_other( dtf, A, B )
#'
#' @export

col_by_other <- function( dtf, col1, col2 ) {

  # Non-standard evaluation
  V <- as.character( substitute( col1 ) )
  L <- as.character( substitute( col2 ) )

  dtf$Cur_values = dtf[[ V ]]
  dtf$Cur_labels = dtf[[ L ]]

  val <- sort( unique( dtf$Cur_values ) )

  lbl <- lapply(
    val,
    function(x) unique( dtf$Cur_labels[ dtf$Cur_values == x ] )
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

    inc <- inc + 1
  }

  return( out )
}

#### 16) date_and_time ####
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

#### 17) pull_id ####
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

