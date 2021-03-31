
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

