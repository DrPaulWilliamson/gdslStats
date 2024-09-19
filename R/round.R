new_round <- function( data_in, dp = 2, as_string = TRUE ) {

  ## as_string - forces results to be reported as strings for tables
  ##             so that displayed dp per column match rounded dp per column
  ##             option ignored for all other data types since numeric output
  ##             displays correct dp per column

  data_in_class <- class(data_in)

  data_length <- NULL

  dp_length <- length( dp )

  # Stop if supplied input is a publish_tab data object
  if ( "publish_tab" %in% class( data_in ) )
    stop( "Apply rounding BEFORE calling publish_tab( )", call. = F )

  # For vector data of any length from 1 to n
  # [Vector data recognised by possession of only one class,
  #  limited to the classes numeric, character, logical and factor]

  # If vector, stop if data supplied as character or factor
  if ( is.null( dim( data_in ) ) ) {

    if ( "character" %in% data_in_class )
      stop( "Supplied value(s) are character, not numeric", call. = F )

    if( is.factor(data_in) )
      stop("Supplied data is a factor. Rounding cannot be applied to factors.",
           call. = F )
  }

  # Given type of input data, calculate appropriate length of data-in
  # (= vector length or number of cols in a matrix/table/data.frame/tibble)
  # and apply appropriate rounding approach

  # Vectors
  if ( is.null( dim( data_in ) ) &
       all( data_in_class %in% c("numeric", "logical") == TRUE) ) {

    data_length <- length( data_in )

    # Apply rounding for vectors
    data_out <- round( data_in, dp[1] )
    if ( dp_length > 1 )
      warning(
        "It is not possible to round each value in a vector to different dp,\n",
        "so first supplied value for dp has been applied to all.",
        call. = F )
  }

  # tables and matrices
  if ( is.table(data_in) | is.matrix(data_in) ) {

    if ( is.numeric(data_in) ) {

      data_length <- ncol( data_in )

      if ( dp_length == 1) {

        data_out <- round( data_in, dp )

      } else {

        data_out <- data_in

        if ( as_string == FALSE | is.matrix( data_in ) ) {

          # Return numeric values that are correctly rounded,
          # but display with same number of dp in each column
          for (i in 1:ncol( data_in )) {
            data_out[ , i ] <- round( data_in[ , i ], dp[i] )
          }

        } else {

          # Return string values that are correctly rounded,
          # and display requested number of dp in each column
          for (i in 1:ncol( data_in )) {
            data_out[ , i ] <- format( round( data_in[ , i ], dp[i] ),
                                       nsmall = dp[i] )
          }
        } # as_string = FALSE/TRUE

        if ( data_length != dp_length )
          warning( "Number of dp values supplied does not match number",
                   " of columns\nin in data being rounded. Therefore results ",
                   "might not be as intended.", call. = F )

      } # dp_length = 1 or 1+


    } else {
      # Not numeric, so provide relevant warning mesage

      if ( is.matrix( data_in) )
        stop("Supplied matrix not numeric, so cannot be rounded.",
             call. = F )

      if ( is.table( data_in) )
        stop("Supplied table not numeric, so cannot be rounded.",
             call. = F )

    } # is.numeric

  } # is table or matrix

  # data.frames
  if ( is.data.frame( data_in ) ) {

    data_length <- ncol(data_in)

    # Apply rounding to data.frames
    if ( dp_length == 1 ) {
      #apply same rounding to all numeric columns in data.frame
      data_out <- data_in |>
        dplyr::mutate( dplyr::across( dplyr::where(is.numeric) ,
                               ~ base::round( .x, dp) ) )
    } else {
      data_out <- data_in
      for ( i in 1:ncol( data_in ) ) {
        if ( is.numeric( data_in[ , i ] ) )
          data_out[ , i ] <- round( data_in[,i], dp[i] )
      }
    }
  } # is.data.frame

  # tibbles
  if ( tibble::is_tibble( data_in ) ) {

    data_length <- ncol(data_in)

    if ( dp_length == 1 ) {
      #apply same rounding to all numeric columns in tibble
      data_out <- data_in |>
        # Change dp data are stored to
        dplyr::mutate( dplyr::across( dplyr::where(is.numeric) ,
                                      ~ base::round( .x, dp) ) ) |>
        # Change dp used to display data
        dplyr::mutate( dplyr::across( dplyr::where(is.numeric) ,
                                      ~ tibble::num( .x, digits = dp) ) )
    } else {
      data_out <- data_in
      for ( i in 1:ncol( data_in ) ) {
        if ( is.numeric( data_in[[i]]) ) {
          # Change dp data are stored to
          data_out[, i] <- round( data_in[ , i], dp[i] )
          # Change dp used to display data
          data_out[, i] <- tibble::num( data_out[[i]], digits = dp[i] )
        }
      }
    }
  } # is.tibble


  # If data_length undefined at this stage, input data is not one of
  # supported data types

  if ( is.null( data_length ) )
    stop( paste0("Supplied data is of type (class) ",
                 class( data_in )," \n",
                 "Supported data types are:\n",
                 "numeric and logial vectors\n",
                 "matrix, table, data.frame and tibble"),
          call. = F )

  return( data_out )

} # new_round
