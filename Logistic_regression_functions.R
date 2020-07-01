library(glmnet)
library(caret)
library(text2vec)
library(data.table)
library(tokenizers)
library(ROCR)

create_token_iterator_from_repository <- function(repository,
                                                  message_column="processed",
                                                  identifier_column="index",
                                                  spunct = TRUE,
                                                  snumeric = TRUE,
                                                  tolowercase = TRUE){
  tokens_temp = repository[[message_column]] %>%
    tokenize_words (strip_punct = spunct, strip_numeric = snumeric, lowercase = tolowercase)
  it <- itoken(tokens_temp,
               ids = repository[[identifier_column]],
               progressbar = FALSE)
  return(it)
}

#' @title Create document-term-matrix (dtm) with a vocabulary
#' @description Creates a dtm using a specific vocabulary.
#' @param df_with_message A df containing the messages.
#' @param vocabulary_to_use Vocabulary to be used in the building of dtm.
#' @param identifier_column Name of the unique identifier column in the df, Default: 'index'
#' @param satd_column Name of the satd-column in the df.
#' @param perform_tfidf Whether tf-idf transformation should be performed, Default: TRUE
#' @param print_dtm_info Whether information from the dtm should be printed, Default: TRUE
#' @return A dtm created from the messages in the df using the inserted vocabulary.
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname create_dtm_with_vocabulary
#' @export 
create_dtm_with_vocabulary <- function(df_with_message, 
                                       vocabulary_to_use, 
                                       identifier_column="index", 
                                       #satd_column,
                                       perform_tfidf=TRUE,
                                       print_dtm_info=FALSE){
  setDT(df_with_message)
  setkey(df_with_message[, ..identifier_column])
  train_ids = df_with_message[[identifier_column]]
  train = df_with_message
  it_train = create_token_iterator_from_repository(train)
  vectorizer = vocab_vectorizer(vocabulary_to_use)
  dtm_train = create_dtm(it_train, vectorizer)
  
  if(isTRUE(perform_tfidf)){
  tfidf = TfIdf$new()
  dtm_train = fit_transform(dtm_train, tfidf)
  
  }
  if (isTRUE(print_dtm_info)){
    print(paste("DTM dimensions =",dim(dtm_train)))
    print(paste("No_words:", nrow(vocabulary_to_use)))
    print(paste("Total words:", sum(vocabulary_to_use$term_count)))
  }
  return(dtm_train)
}


#' @title Run Logistic Regression for Messages
#' @description This runs logistic regression for the supplied document-term-matrix
#' @param target_column Column from a df, which is tried to be evaluated
#' @param trained_dtm Document-term-matrix from the target data
#' @param save_classifier Insert 'YES' as an optional parameter to save the classifier, Default: 'NO'
#' @param model_savepath Insert optionally a savepath where the classifier will be saved, Default: '.'
#' @param model_name Insert optionally a name for the saved cv.glmnet classifier, Default: 'glm_classifer'
#' @param weights_vector Column name, or vector where the weights are. If one is not given, reverts to 1 for all occurrences.
#' @return Returns logistic regression classifier
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname run_logistic_regression_for_messages
#' @export 
run_logistic_regression_for_messages <- function(df_to_predict, identifier_column, target_column, 
                                                 trained_dtm, save_classifier="NO", model_savepath=".", 
                                                 model_name="glm_classifer",
                                                 seed_number = 42,
                                                 weights_vector = NULL){
  train = df_to_predict
  setDT(train)
  setkey(train[, ..identifier_column])
  train_ids = train[[identifier_column]]
  
  weights_vector = train[[weights_vector]]
  if(is.null(weights_vector)){
    weights_vector <- rep(1, nrow(train))
  }
  
  train[[target_column]] <- as.factor(train[[target_column]])
  set.seed(seed_number)
  # This is to create a stratified sample based on a certain column
  flds <- createFolds(train[[target_column]], k = 10, list = FALSE, returnTrain = TRUE)
  
  glmnet_classifier = cv.glmnet(x = trained_dtm, y = train[[target_column]], 
                                weights = weights_vector, 
                                family = 'binomial', 
                                alpha = 1,
                                type.measure = "auc",
                                nfolds = 10,
                                foldid = flds,
                                thresh = 1e-3,
                                maxit = 1e3) 
  plot(glmnet_classifier)
  print(paste("max AUC=", round(glmnet_classifier$cvm[glmnet_classifier$lambda == glmnet_classifier$lambda.min], 4)))
  print(paste("AUC with 1se =", round(glmnet_classifier$cvm[glmnet_classifier$lambda == glmnet_classifier$lambda.1se], 4)))
  if (save_classifier=="YES"){
    saveRDS(glmnet_classifier, paste0(model_savepath, "/", model_name, i, ".rds"))
  }
  return(glmnet_classifier)
}


#' @title Get predictor terms from glmnet classifier
#' @description Returns a dataframe with all the predictor terms from glmnet classifier
#' @param glmnet_classifier A Glmnet-classifier object
#' @param lambda_to_use Which lambda value to use, DEFAULT: "lambda.min".
#' @return Dataframe with all the predictor terms from glmnet classifier
#' @details No details
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname get_predictor_terms_from_glmnet_classifier
#' @export 
get_predictor_terms_from_glmnet_classifier <- function(glmnet_classifier, lambda_to_use="lambda.min"){
  best_coef <- coef(glmnet_classifier, s = lambda_to_use)
  best_coef_table <- data.frame(name = best_coef@Dimnames[[1]][best_coef@i + 1], coefficient = best_coef@x)
  best_coef_table$transformed_odds <- exp(best_coef_table$coefficient)
  best_coef_table$name <- as.character(best_coef_table$name)
  return(as.data.frame(best_coef_table))
}

#' @title Find unlabeled SATD code comments
#' @description Make predictions from glmnet-claffier to find unlabeled SATD code comments
#' @param df_with_message A df containing the messages
#' @param identifier_column Unique identifier column
#' @param satd_column A column marking SATD with 1 being yes, and 0 no.
#' @param message_column A column marking the message, which is returned from the df.
#' @param trained_dtm A dtm created from the df containing the messages.
#' @param glmnet_classifier A cv-glmnet classifier
#' @param prediction_cutoff What confidence the predictions should have.
#' @param print_caret If "yes" is given, prints out the confustion matrix for the predictions.
#' @return A df containing the identifier column, message column, and prediction confidence
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso 
#'  \code{\link[caret]{confusionMatrix}}
#' @rdname find_messages_without_keyword
#' @export 
#' @importFrom caret confusionMatrix
find_messages_without_keyword <- function(df_with_message, identifier_column, satd_column, 
                                          message_column, trained_dtm, glmnet_classifier, prediction_cutoff, print_caret){
  train = df_with_message
  setDT(train)
  setkey(train[, ..identifier_column])
  
  train_ids = train[[identifier_column]]
  train[[satd_column]] <- as.factor(train[[satd_column]])
  predicted <- predict.cv.glmnet(glmnet_classifier, s = "lambda.min", newx = trained_dtm, type = "response")
  predicted_todo <- as.factor(ifelse(predicted > prediction_cutoff, "1", "0"))
  
  if (tolower(print_caret) == "yes" ){
    print(caret::confusionMatrix(data = predicted_todo, reference = train[[satd_column]], positive = "1"))
  }
  
  ROCRpred = prediction(predicted, train[[satd_column]])
  ROCRperf = performance(ROCRpred, "tpr", "fpr")
  plot(ROCRperf, print.cutoffs.at=seq(0.5,1,by=0.1), text.adj=c(-0.2,1.7))
  
  predicted_todo <- as.data.frame(predicted_todo)
  predicted_df <- as.data.frame(predicted)
  git_comments_with_prediction <- merge(as.data.frame(train), predicted_df, by.x = identifier_column, by.y = "row.names")
  git_comments_with_prediction<- git_comments_with_prediction[git_comments_with_prediction[,satd_column] == 0 & git_comments_with_prediction$"1" > prediction_cutoff,]
  
  names(git_comments_with_prediction)[names(git_comments_with_prediction)=="1"] <- "threshold"
  return(git_comments_with_prediction)
}