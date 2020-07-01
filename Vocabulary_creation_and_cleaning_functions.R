library(text2vec)
library(NLoN)
library(psych) # Needed for cohen kappa in filter_out_not_NL_messages
library(dplyr)
library(tokenizers)
library(stringr)

#' @title Find a keyword from messages and mark it
#' @description This function creates an extra column to df and marks it whether a specific keyword is present
#' @param list_keyword_to_find List of keywords to be looked for from messages
#' @param df_with_messages A df containing the messages where the keyword is looked from
#' @param message_column The message column in the df, where the keyword is looked from
#' @param identifier_column A column in the df with a unique value for identifing individual rows. Default: 'index'
#' @param combine_keywords If 'yes', then all keywords are combined to a single satd-column. Default: 'no'
#' @param ignore_case A boolean value whether the case should be ignored or not when looking for keywords. Default = TRUE
#' @return A df with extra columns named after the keywords and marked with their presence
#' @details The extra columns are named after the keywords, and marked with 1 if it's present and 0 if it's not.
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname find_and_mark_keyword_messages
#' @export 
find_and_mark_keyword_messages <- function(list_keyword_to_find, df_with_messages, 
                                           message_column="clean_added", 
                                           identifier_column="index",
                                           combine_keywords="no",
                                           ignore_case = TRUE){
  for (keyword_to_find in list_keyword_to_find){
    keyword_code_comments <- df_with_messages[grep(keyword_to_find, df_with_messages[, message_column], ignore.case = ignore_case),]
    df_with_messages[,tolower(keyword_to_find)] <- ifelse(df_with_messages[,identifier_column] %in% keyword_code_comments[,identifier_column], 1, 0)
  }
  if (combine_keywords == "no" | combine_keywords == "n"){
    return(df_with_messages)
  }
  else{
    df_with_messages$satd <- ifelse(rowSums(df_with_messages[,tolower(list_keyword_to_find)], na.rm = TRUE) > 0, 1, 0)
    df_with_messages <- df_with_messages[, !names(df_with_messages) %in% tolower(list_keyword_to_find)]
  }
}

#' @title Delete keywords from messages
#' @description This function deletes keywords passed onto it from messages
#' @param list_keyword_to_find List containing the keywords to be deleted
#' @param df_with_messages Dataframe containing a message column
#' @param message_column Name of the column in the dataframe containing the messages.
#' @param ignore_case A boolean value, whether the case of the word should be ignored. Default: TRUE
#' @return Dataframe from which the keywords in the messages are deleted.
#' @details The message column should be processed before entering, e.g. lower case, removal of special characters etc.
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname delete_keywords_from_messages
#' @export 
delete_keywords_from_messages <- function(list_keyword_to_find, df_with_messages, message_column, ignore_case = TRUE){
  for (keyword_to_find in list_keyword_to_find){
    if(isTRUE(ignore_case)){
      df_with_messages[,message_column] <- gsub(tolower(keyword_to_find), "", df_with_messages[,message_column])
    } else {
      df_with_messages[,message_column] <- gsub(keyword_to_find, "", df_with_messages[,message_column])
    }
    
  }
  return(df_with_messages)
}


#' @title Process messages
#' @description Use several precoessing functions to messages
#' @param message_column Message column to be processed
#' @return Processed message column
#' @details Currently translates to lower case, deletes hashes/emails/git-svn/special charactes/1 character words
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname process_repository_message
#' @export 
process_repository_message <- function(message_column){
  new_message_column <- message_column %>%
    tolower %>%
    str_replace_all("\\b[a-fA-F0-9]{40,}\\b", " ") %>%
    str_replace_all("\\b\\S+@\\S+|\\{(?:\\w+, *)+\\w+\\}@[\\w.-]+\\b", " ") %>%
    str_replace_all("\\b(f|ht)tp(s?)://.*\\b\\s", " ") %>%
    str_replace_all("\\bgit-svn(-id)?\\b", " ") %>%
    str_replace_all("'|`|´", "") %>%
    str_replace_all("[^[:alpha:]]", " ") %>%
    str_replace_all("\\b[a-zA-Z]{1,1}\\b", " ") %>%
    str_replace_all("\\s+", " ")
  return(new_message_column)
}

#' @title Filter out messages that are not natural language
#' @description This function filters out messages that are deemed as code comments
#' @param df_with_messages A df where the messages to be analyzed are
#' @param message_column The column in the df containing the messages
#' @param rater_to_use Which rater to use. Options: "RATER1", "RATER2", "BOTH". Default: 'BOTH'
#' @return A df with only the messages deemed to be natural language
#' @details The message column should not be translated to lower case, as the ratio of capital and non-capital letters is taken into account.
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname filter_out_not_NL_messages
#' @export 
filter_out_not_NL_messages <- function(df_with_messages, message_column, rater_to_use="BOTH"){
  if (rater_to_use=="RATER2"){
    model <- NLoNModel(nlon.data$text, nlon.data$rater2)
    topredict <- df_with_messages[,message_column]
    prediction_results <- NLoNPredict(model, topredict)
    df_with_messages[["NLON"]] <- prediction_results
    return(df_with_messages[df_with_messages[["NLON"]] == "NL",])
  }
  
  else if (rater_to_use=="RATER1"){
    model <- NLoNModel(nlon.data$text, nlon.data$rater1)
    topredict <- df_with_messages[,message_column]
    prediction_results <- NLoNPredict(model, topredict)
    df_with_messages[["NLON"]] <- prediction_results
    return(df_with_messages[df_with_messages[["NLON"]] == "NL",])
  }
  
  else if (rater_to_use=="BOTH"){
    model <- NLoNModel(nlon.data$text, nlon.data$rater1)
    topredict <- df_with_messages[,message_column]
    prediction_results <- NLoNPredict(model, topredict)
    df_with_messages[["NLON1"]] <- prediction_results
    
    model <- NLoNModel(nlon.data$text, nlon.data$rater2)
    topredict <- df_with_messages[,message_column]
    prediction_results <- NLoNPredict(model, topredict)
    df_with_messages[["NLON2"]] <- prediction_results
    print(cohen.kappa(df_with_messages[,c("NLON1", "NLON2")]))
    
    return(df_with_messages[df_with_messages[["NLON1"]] == "NL" & df_with_messages[["NLON2"]] == "NL",])
  }
}

#' @title Function to combine multiple message rows into single rows
#' @description This function merges message rows belonging to same commits as single rows
#' @param df_with_messages A df with the single message rows
#' @param column_to_combine Column in the df with the message rows
#' @param column_to_identify_messages Column which identifies rows which belong to same message. Default: 'index'
#' @return A df with rows belonging to same messages combined to a single row.
#' @details None
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname combine_individual_message_rows_together
#' @export 
combine_individual_message_rows_together <- function(df_with_messages, column_to_combine, column_to_identify_messages){
  df_with_messages2 <- aggregate(as.formula(paste(column_to_combine, "~", column_to_identify_messages)), df_with_messages, paste, collapse = " ")
  # Combining messages sharing an identifying column together
  df_with_messages3 <- merge(x = df_with_messages2, y = df_with_messages, by = column_to_identify_messages)
  # Deleting any residual duplicates
  df_with_messages3 <- df_with_messages3[!duplicated(df_with_messages3[,column_to_identify_messages]),]
  names(df_with_messages3)[names(df_with_messages3)==paste0(column_to_combine ,".x")] <- column_to_combine
  return(df_with_messages3)
}

#' @title Delete automatically created code message contents
#' @description This function deletes comments containing following phrase: "auto generated" 
#' @param df_with_messages A df containing the messages
#' @param message_column The column in the df with the messages
#' @param list_of_words List of words which can't appear in messages
#' @param satd_column Name of the column in the df, where presence of satd is marked
#' @return A df without messages which has any of the words as defined by the user
#' @details None
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname delete_automatically_created_messages
#' @export 
delete_messages_with_following_expressions <- function(df_with_messages, message_column, list_of_words, satd_column
                                                       , keep_satd = FALSE){
  string_start = "^.*("
  string_end = ").*$"
  words_to_find <- paste(list_of_words, collapse ="|")
  words_to_find = paste(string_start, words_to_find, string_end, sep ="")
  messages_to_be_deleted <- df_with_messages[grep(words_to_find, df_with_messages[[message_column]], ignore.case = TRUE, perl = TRUE),]
  # If this is set to true, we keep the SATD messages with the otherwise deleted words.
  if(isTRUE(keep_satd)){
  messages_to_be_deleted <- messages_to_be_deleted[messages_to_be_deleted[,satd_column] == 0,]
  }
  
  df_without_messages <- anti_join(df_with_messages, messages_to_be_deleted)
  return(df_without_messages)
}

#' @title Create vocabularies from several repositories
#' @description This function creates vocabularies from several repositories passed onto it as a df
#' @param repository A df containing all the messages from all the repositories
#' @param individual_repository_column Column which has information about the repository of the message
#' @param processed_column Column where the message itself resides
#' @param index_column Column which has a unique identifier to every message
#' @param minimum_term_percentage Percentage threshold, e.g. how many times the term has to appear to be included
#' @param maximum_document_proportion On how many documents individual word can appear (as a percentage)
#' @param stop_word_vector Vector containing stop word -list
#' @param ngram_length Maximum length for an n-gram
#' @return List containing all the vocabularies created from different repositories
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname create_vocabularies_from_repositories_with_percentage_term_limit
#' @export 
create_vocabularies_from_repositories_with_percentage_term_limit <- function(repository, 
                                                  individual_repository_column, 
                                                  processed_column,
                                                  index_column,
                                                  minimum_term_percentage, 
                                                  maximum_document_proportion, 
                                                  stop_word_vector, 
                                                  ngram_length,
                                                  ngram_min){
  vocabulary_list <- vector(mode = "list", length = length(individual_repository_column))
  i = 1
  for (individual_repository in individual_repository_column){
    individual_repository_messages <- repository[repository$projectID == individual_repository,]
    individual_repository_vocabulary_count<- create_or_count_untrimmed_vocabulary_from_single_repository(repository=individual_repository_messages, 
                                                                 processed_column, 
                                                                 index_column,
                                                                 stop_word_vector,
                                                                 ngram_length,
                                                                 ngram_min,
                                                                 return_value = "count")
    
    minimum_term_count <- floor(minimum_term_percentage * individual_repository_vocabulary_count)
    #print(minimum_term_count)
    tokens_temp = individual_repository_messages[,processed_column] %>%

      tokenize_words (strip_punct = TRUE, strip_numeric = TRUE, stopwords = stop_word_vector)
    it <- itoken(tokens_temp,
                 ids = individual_repository_messages[,index_column],
                 progressbar = FALSE)
    v_temp = create_vocabulary(it, ngram = c(ngram_min = ngram_min, ngram_max = ngram_length), sep_ngram = " ") %>% 
      prune_vocabulary(term_count_min = minimum_term_count, doc_proportion_max = maximum_document_proportion)
    #print(sum(v_temp$term_count))
    vocabulary_list[[i]] <- v_temp
    print(paste("Processed", i, "/", length(individual_repository_column), "repositories so far."))
    i <- i + 1
  }
  return(vocabulary_list)
}

#' @title Create an untrimmed vocabulary from a repository or count word occurrences in a repository
#' @description Creates an untrimmed vocabulary from a repository and returns either thte vocabulary or just the term count
#' @param repository A df containing all the messages from all the repositories
#' @param processed_column Column where the message itself resides
#' @param index_column Column which has a unique identifier to every message
#' @param maximum_document_proportion On how many documents individual word can appear (as a percentage)
#' @param stop_word_vector Vector containing stop word -list
#' @param ngram_length Maximum length for an n-gram
#' @param return_value Value of "count" returns just the sum of terms, otherwise returns the whole vocabulary
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname create_or_count_untrimmed_vocabulary_from_single_repository
#' @export 
create_or_count_untrimmed_vocabulary_from_single_repository <- function(repository, 
                                                               processed_column,
                                                               index_column, 
                                                               stop_word_vector, 
                                                              ngram_length,
                                                              ngram_min,
                                                              return_value="vocabulary"){
  tokens_temp = repository[,processed_column] %>%
    tokenize_words (strip_punct = TRUE, strip_numeric = TRUE, stopwords = stop_word_vector) 
  it <- itoken(tokens_temp, 
               ids = repository[,index_column],
               progressbar = FALSE)
  individual_vocabulary = create_vocabulary(it, ngram = c(ngram_min = ngram_min, ngram_max = ngram_length), sep_ngram = " ") %>% 
    prune_vocabulary(term_count_min = 1, doc_proportion_max = 100)
  vocabulary_sum <- sum(individual_vocabulary$term_count)
  if (return_value == "count"){
    return(vocabulary_sum)
  } else {
    return(individual_vocabulary)
  }
}

#' @title Count in how many repositories each term exists
#' @description This function counts on how many repositores all terms occur
#' @param list_of_vocabularies List containing text2vec-vocabulary items
#' @return Returns list of terms and the number of repositories they appear in
#' @details None
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname count_term_occurrence_in_repositories
#' @export 
count_term_occurrence_in_repositories <- function(list_of_vocabularies){
  for (i in 1:length(list_of_vocabularies)){
    if (i == 1){
      all_vocabulary_terms <- list_of_vocabularies[[i]]$term
    } else {
      all_vocabulary_terms <- append(all_vocabulary_terms, list_of_vocabularies[[i]]$term)
    }
  }
  all_vocabulary_terms <- as.data.frame(table(all_vocabulary_terms))
  all_vocabulary_terms$all_vocabulary_terms <- as.character(all_vocabulary_terms$all_vocabulary_terms)
  return(all_vocabulary_terms)
}

#' @title Combine vocabularies with an repository minimum limit
#' @description Create a combined vocabulary from a list of vocabularies and keep only terms appearing in x repositories
#' @param list_of_vocabularies A list containing text2vec-vocabulary objects
#' @param minimum_repository_count An integer defining on how many repositories a term has to appear
#' @return Returns a vocabulary, which includes only terms that appeared in x amount of repositories
#' @details No details.
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname combine_vocabularies_with_a_repository_minimum_limit
#' @export 
combine_vocabularies_with_a_repository_minimum_limit <- function(list_of_vocabularies, minimum_repository_count){
  all_vocabularies_with_repository_count <- count_term_occurrence_in_repositories(list_of_vocabularies = list_of_vocabularies)
  all_vocabularies_with_repository_count <- all_vocabularies_with_repository_count[all_vocabularies_with_repository_count[,"Freq"] >= minimum_repository_count,]
  combined_vocabulary <- do.call(combine_vocabularies, list_of_vocabularies)
  combined_vocabulary <- combined_vocabulary[combined_vocabulary$term %in% all_vocabularies_with_repository_count$all_vocabulary_terms,]
  return(combined_vocabulary)
}

#' @title Find renaming commits
#' @description Finds commits that were marked as renamed in the TDD dataset by Lenarduzzi and Taibi.
#' @param df_with_messages Df containing the messages
#' @param return_not_renamed Choose , Default: 'not_renamed'
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname find_renaming_commits
#' @export 
find_renaming_commits <- function(df_with_messages, data_path, return_not_renamed=TRUE){
  # We load the changes-file here, as there's no need to keep it stored to the memory otherwise.
  git_changes <- as.data.frame(load_technical_debt_dataset_comments(data_path, "git_changes.csv"))
  git_changes <- git_changes[git_changes$changeType == "ModificationType.RENAME",]
  git_comments_renamed_files <- merge(x = df_with_messages, y = git_changes, by = c("oldPath", "newPath", "commitHash", "projectID"))
  git_comments_renamed_files <- unique(git_comments_renamed_files$index)
  if (isTRUE(return_not_renamed)){
  df_with_messages <- df_with_messages[!(df_with_messages$index %in% git_comments_renamed_files),]
  }
  else{
    df_with_messages <- df_with_messages[(df_with_messages$index %in% git_comments_renamed_files),]
  }
  return(df_with_messages)
}

count_word_occurrences <- function(df_with_messages, message_column, words_to_count){
  library(tidytext)
  
  all_word_counts <- df_with_messages %>%
    unnest_tokens(word, message_column) %>%
    dplyr::count(word, sort = TRUE)
  
  for (individual_word in words_to_count){
    print(paste(individual_word, ":", all_word_counts[all_word_counts$word == individual_word,]))
  }
}
