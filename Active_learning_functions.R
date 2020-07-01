
#' @title Choose a random sample of messages based on threshold
#' @description Chooses randomly a random sample of messages, based on glmnet's threshold values
#' @param df_predictions_of_satd A df with the messages and threshold values
#' @param min_threshold Minimum threshold, between 0.0 and 1.0
#' @param max_threshold Maximum threshold, between Minimum threshold and 1.0
#' @param threshold_column Column name in the df holding the threshold values
#' @param index_column Column name in the df containing identifier values, e.g. index
#' @param sample_size How many messages should be returned.
#' @param include_evaluated Should we include in the random sample already evaluated messages? Default: "no".
#' @param evaluation_column Name of the column, where the number of times the message has been evaluated is recorded.
#' @return Returns a random sample from the df, with the size of the sample size.
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname choose_random_sample_messages_based_on_threshold
#' @export 
choose_random_sample_messages_based_on_threshold <- function(df_predictions_of_satd, 
                                                             min_threshold,
                                                             max_threshold,
                                                             threshold_column,
                                                             index_column,
                                                             sample_size,
                                                             include_evaluated="no",
                                                             evaluation_column="times_eval"){
  df_predictions_of_satd <- df_predictions_of_satd[df_predictions_of_satd[,threshold_column] > min_threshold & 
                                                     df_predictions_of_satd[,threshold_column] < max_threshold,]
  
  if(tolower(include_evaluated) == "no"){
    df_predictions_of_satd <- df_predictions_of_satd[df_predictions_of_satd[,evaluation_column] < 1,]
  }
  
  if (sample_size > nrow(df_predictions_of_satd)){
    sample_size <- nrow(df_predictions_of_satd)
  }
  satd_sample <- sample(1:nrow(df_predictions_of_satd), sample_size, replace = FALSE)
  index_column_sample <- df_predictions_of_satd[,index_column]
  messages_to_show <- df_predictions_of_satd[df_predictions_of_satd[,index_column] %in% 
                                               index_column_sample[satd_sample],]
  return(messages_to_show)
}