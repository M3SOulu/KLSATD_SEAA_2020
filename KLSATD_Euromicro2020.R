rm(list = ls())
gc()
source("Data_loading_and_saving_functions.R")
source("Logistic_regression_functions.R")
source("Vocabulary_creation_and_cleaning_functions.R")
source("Commit_information_functions.R")
source("Active_learning_functions.R")
source("Correlation_information_functions.R")
library("sinew")
library("tidyr")

save_path <- "./save/"
original_trained_models_path <- "./original_models/"
data_path <- "./data/"

# Load the comments
tdd_comments_both <- readRDS(paste0(save_path, "tdd_comments_both.RDS"))

# If you loaded the tdd_comments_both above, then skip the steps 0 and 1 from the workflow.
# If you didn't load the presaved comments, you can start here to create them from scratch.
# Please note, that in this way the NLON raters might choose different lines as NLON,
# and the resulting numbers might differ from the ones presented in the paper.

# Workflow:
# 0. Loading technical debt dataset comments
# 1. Run NLoN analysis
# 2. Find and mark commits for keywords (TODO, FIXME ETC.)
# 3. Combine individual code comment rows to single rows
# 4. Filter out auotmatically generated code comments and the ones containing licence or copyright information
# 5. Process the repository messages to lower etc.
# 6. Build a vocabulary from remaining code comments
# 6.1. Apply limits to restrict on how many times and on how many repositories the word has to appear
# For the paper: Run correlation analyses on CLOC, number of programmers, and project age with SATD percentage
# 7. Build a dtm using this restricted vocabulary
# 8. Perform ML training from glmnet
# 9. Evaluate model performance
# 10. Select a sample of messages to be presented to the user for evaluation for TODO inclusion

# 0. Loading technical debt dataset comments
tdd_comments <- as.data.frame(load_technical_debt_dataset_comments(data_path, "git_comments_exploded_add.csv"))
tdd_comments <- tdd_comments[tdd_comments$clean_added != '',]

# 1. Run NLoN analysis
# This can take a long time to run, up to tens of minutes depending on the used machine.
tdd_comments_both <- filter_out_not_NL_messages(tdd_comments, "clean_added", "BOTH")

# 2. Find and mark commits for keywords (TODO, FIXME ETC.). All those recommended by previous literature.
tdd_comments_both <- find_and_mark_keyword_messages(list_keyword_to_find = c("HACK", "FIXME", "TODO", "XXX"), 
                                                    df_with_messages = tdd_comments_both, 
                                                    message_column = "clean_added", 
                                                    identifier_column = "index", 
                                                    combine_keywords = "yes",
                                                    ignore_case = FALSE)

# 3. Combine individual code comment rows to single rows
tdd_comments_combined <- combine_individual_message_rows_together(tdd_comments_both, "clean_added", "index")

# 4. Filter out code comments containing licence or copyright information
tdd_comments_combined <- delete_messages_with_following_expressions(tdd_comments_combined, "clean_added",
                                                                    c("license", "licence", "copyright", "todo auto"), 
                                                                    "satd",
                                                                    keep_satd = FALSE)

# 4.1 Filter out code comments containing any Javadoc tags
tdd_comments_combined <- delete_messages_with_following_expressions(tdd_comments_combined, "clean_added",
                                                                     c("@(?!todo)"), 
                                                                     "satd",
                                                                     keep_satd = FALSE)

# Checking how much of the messages are labeled as satd
table(tdd_comments_combined$satd)
# Total amount of TD comments after above (if loaded the "tdd_comments_both.RDS"):
# Paper: "Preprocessing reduces the totalnumber of comments to 507,254."
# No TD: 493,666
# TD: 13,588
# Total: 507,254

# 4.1 Making the Table 1. to the paper
# Counting total number of comments, and SATD comments
count_satd_comments_and_all_comments_per_project(df_with_commits = tdd_comments_combined,
                                                 project_column = "projectID",
                                                 satd_column = "satd",
                                                 printinfo=TRUE)

# 4.2. For the paper: Run correlation analyses on CLOC, number of programmers, and project age with SATD percentage
count_correlation_with_satd_and_different_features(git_comments =  tdd_comments_combined, return_df = FALSE)

# 5. Process the messages to lower etc.
# 5.1. Delete keywords from the code messages. Otherwise e.g. TODO is too good predictor.
# Hack is a special case in this regard, as it has two meanings depending on how it is written.
# If it's written with capitals, then it's a keyword and should be deleted. If it's written
# normally, it is a descriptive word. E.g. TODO: We should remove this ugly hack.
tdd_comments_combined <- delete_keywords_from_messages(list_keyword_to_find = c("HACK"), 
                                                       df_with_messages = tdd_comments_combined,
                                                       message_column = "clean_added",
                                                       ignore_case = FALSE)

# 5.2. Process the messages to lower etc.
tdd_comments_combined$processed <- process_repository_message(tdd_comments_combined$clean_added)

tdd_comments_combined <- delete_keywords_from_messages(list_keyword_to_find = c("TODO", "FIXME", "XXX"), 
                                                       df_with_messages = tdd_comments_combined,
                                                       message_column = "processed",
                                                       ignore_case = TRUE)

tdd_comments_combined <- tdd_comments_combined[!(tdd_comments_combined$processed=="" | tdd_comments_combined$processed==" "),]

# 5.3. Remove unnecessary columns from the df, and delete the uncombined one.
tdd_comments_combined <- tdd_comments_combined[,c("index", "clean_added", "projectID",
                                                  "satd", "processed")]

# 5.4. Create new columns, so that we can use majority voting on messages if necessary
tdd_comments_combined[,"times_eval"] <- 0
tdd_comments_combined[,"times_eval_0"] <- 0
tdd_comments_combined[,"times_eval_1"] <- 0

# Delete the row based one
tdd_comments_both <- NA

# 6. Build vocabularies from remaining code comments
stop_words <- read.delim(paste0(data_path, "nltk_stop_words.txt"))
stop_words <- as.character(stop_words$i)
stop_words <- append(stop_words, "i")

# 6.1. Apply limits to restrict on how many times and on how many repositories the word has to appear
vocabulary_list <- create_vocabularies_from_repositories_with_percentage_term_limit(repository=tdd_comments_combined,
                                                                                    individual_repository_column=unique(tdd_comments_combined$projectID),
                                                                                    processed_column="processed",
                                                                                    index_column="index",
                                                                                    minimum_term_percentage=0.0001,
                                                                                    maximum_document_proportion=1.0,
                                                                                    stop_word_vector=stop_words,
                                                                                    ngram_length=1L,
                                                                                    ngram_min = 1L)

# make function to mark every word on how many vocabularies it appears
all_terms_and_repo_count <- count_term_occurrence_in_repositories(vocabulary_list)

# Combine vocabularies with repository limit
combined_vocabulary_min <- combine_vocabularies_with_a_repository_minimum_limit(list_of_vocabularies = vocabulary_list,
                                                                                  minimum_repository_count = 5)

# Include only terms with a certain minimum length
combined_vocabulary_min <- combined_vocabulary_min[(nchar(combined_vocabulary_min$term) > 2),]

# OPTIONAL: SAVE THE CREATED VOCABULARY
# save_file_to_disk(file_to_save = combined_vocabulary_min,
#                 file_save_path = save_path,
#                 file_name = "combined_vocabulary.RDS")

# PAPER EXCLUSIVE. Creating comparison wordclouds for Figure 2
# SATD wordcloud, 400 words
# Comparison cloud, 400 words
make_comparison_wordcloud_satd_vs_no_satd(git_comments=tdd_comments_combined, combined_vocabulary=combined_vocabulary_min,
                                          processed_column="processed",
                                          satd_column="satd",
                                          max_words = 400,
                                          create_comparison_clouds=TRUE, create_todo_cloud=FALSE)

# OPTIONAL STEP: WEIGHT ASSIGNMENT:
# Assign weights so that SATD comments equal non-SATD comments
# Done by counting non-satd/satd  for satd comments, and assigning 1 to others.
tdd_comments_combined$weights <- ifelse(tdd_comments_combined$satd == 1,
                                        nrow(tdd_comments_combined[tdd_comments_combined$satd == 0,]) / nrow(tdd_comments_combined[tdd_comments_combined$satd == 1,])
                                        , 1)

# OPTIONAL: SAVE THE ORIGINAL DF
# save_file_to_disk(file_to_save = tdd_comments_combined,
#                   file_save_path = save_path,
#                   file_name = "tdd_comments_combined.RDS")

# 7. Build a dtm using this restricted vocabulary
tdd_comments_combined$index <- as.character(tdd_comments_combined$index)

trained_dtm <- create_dtm_with_vocabulary(df_with_message = tdd_comments_combined,
                                          vocabulary_to_use = combined_vocabulary_min,
                                          identifier_column = c("index"),
                                          perform_tfidf = TRUE)

# OPTIONAL: SAVE THE ORIGINAL DTM
# save_file_to_disk(file_to_save = trained_dtm,
#                  file_save_path = save_path,
#                  file_name = "trained_dtm.RDS")

# 8. Perform ML training from glmnet
glmnet_classifier <- run_logistic_regression_for_messages(df_to_predict = tdd_comments_combined,
                                                          identifier_column = c("index"),
                                                          target_column = "satd",
                                                          trained_dtm = trained_dtm,
                                                          weights_vector = c("weights"),
                                                          seed_number = 28)
# Checking the predictors
predictors <- get_predictor_terms_from_glmnet_classifier(glmnet_classifier, "lambda.min")
View(predictors)

# OPTIONAL: SAVE THE CLASSIFIER AND THE PREDICTORS
# save_file_to_disk(file_to_save = glmnet_classifier,
#                          file_save_path = save_path,
#                          file_name = "cvglmnet.RDS")
# write.csv(predictors, file="SATD_predictors.csv")

# 9. Evaluate model performance by predicting SATD comments missing a keyword
predictions_to_include_satd <- find_messages_without_keyword(df_with_message = tdd_comments_combined,
                                                             identifier_column = c("index"),
                                                             satd_column = "satd",
                                                             message_column = "clean_added",
                                                             trained_dtm = trained_dtm,
                                                             glmnet_classifier = glmnet_classifier,
                                                             prediction_cutoff = 0.7,
                                                             print_caret = "Yes")

# Deleting duplicates for the paper "After deleting the duplicates, we were left with 15,510 comments
predictions_no_duplicates <- predictions_to_include_satd[!duplicated(predictions_to_include_satd[,"processed"]),]

# 10. Select a sample of messages to be presented to the user for evaluation for TODO inclusion
random_message_sample <- choose_random_sample_messages_based_on_threshold(df_predictions_of_satd = predictions_no_duplicates,
                                                                          min_threshold = 0.7,
                                                                          max_threshold = 1.0,
                                                                          threshold_column = "threshold",
                                                                          index_column = "index",
                                                                          sample_size = 10,
                                                                          include_evaluated="no",
                                                                          evaluation_column="times_eval")
