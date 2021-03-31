
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
