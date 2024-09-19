#' Round numerical data
#'
#' @param data_in The data to be rounded, supplied as a (vector of) numeric or logical; or as data object of class table, matrix, data.frame or tibble (any of which can contain non-numeric columns)
#' @param dp The number of decimal places to round to (default is 2), supplied as either as single number to be applied to all numeric values, or as a series vector of values (one per column of the input data object, in column order, using NA for factor/string columns)
#' @param as_string If TRUE (default is FALSE), and data_in is of class table, returns a table in which all numeric values have been converted to strings, so that the dp per column used for display matches that used for rounding
#'
#' @return An updated version of the supplied data object containing rounded numeric values
#' @export
#'
#' @examples
#' round( data_in = 1.1234 )
#' round( data_in = rep(1.1234, 4), dp = c(1,2,3,4) )
#' df <- data.frame( var = c("A", "B", "C"), v1 = rep(1.1234, 3), v2 = rep(1.1234, 3) )
#' round( df, dp = c(2, 1) )
#' # Example of table with as_string = FALSE to follow
#' round( tibble::as_tibble( df), dp = c(NA, 2, 1) )
#'
round <- function( data_in, dp = 2, as_string = TRUE ) {

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
    data_out <- base::round( data_in, dp[1] )
    if ( dp_length > 1 )
      warning(
        "It is not possible to round each value in a vector to different dp,\n",
        "so first supplied value for dp has been applied to all.",
        call. = F )
  }

  # For other valid data types, find number of columns in dataset,
  # check that dp_length matches data_length, and take corrective
  # action if required

  if ( is.table(data_in) | is.matrix(data_in) |
       is.data.frame(data_in) | tibble::is_tibble(data_in) ) {

    data_length <- ncol( data_in )

    if ( dp_length !=1 & data_length != dp_length ) {
      warning( "Number of dp values supplied does not match number",
             " of columns\nin data being rounded. Therefore results ",
             "might not be as intended.", call. = F )

      # if dp has too few values, extend as required with default value of 2
      # and update dp_length accordingly
      if ( dp_length < data_length ) {
        dp <- c( dp, rep(2, data_length - dp_length) )
        dp_length <- data_length
      }
    }
  }

  # tables and matrices
  if ( is.table(data_in) | is.matrix(data_in) ) {

    if ( is.numeric(data_in) ) {

      if ( dp_length == 1) {

        data_out <- base::round( data_in, dp )

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
            data_out[ , i ] <- format( base::round( data_in[ , i ], dp[i] ),
                                       nsmall = dp[i] )
          }
        } # as_string = FALSE/TRUE

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
          data_out[ , i ] <- base::round( data_in[,i], dp[i] )
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
          data_out[, i] <- base::round( data_in[ , i], dp[i] )
          # Change dp used to display data
          data_out[, i] <- tibble::num( data_out[[i]], digits = dp[i] )
        }
      }
    }
  } # is_tibble


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

} # round
