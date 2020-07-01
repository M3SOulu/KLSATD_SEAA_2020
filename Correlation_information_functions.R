library(xtable)
library(tidytext)
library(wordcloud)
library(tm)

############ Making the table 1. to the paper
# Counting how many commits per project have comments
count_correlation_with_satd_and_different_features <- function(git_comments, return_df=FALSE){
  git_commits <- as.data.frame(load_technical_debt_dataset_comments(data_path, "git_commits.csv"))
  
  commits_and_comments_per_project <- count_commits_and_comments_per_project(df_with_commits = git_comments,
                                         project_column = "projectID",
                                         hash_column = "commitHash",
                                         return_df = TRUE)
  
  # 4.2 Making the Table 2. to the paper
  # Counting total number of comments, and SATD comments
  
  satd_comments_per_project <- count_satd_comments_and_all_comments_per_project(df_with_commits = git_comments,
                                                   project_column = "projectID",
                                                   satd_column = "satd",
                                                   return_df = TRUE)
  
  total_commits_and_comment_commits_df <- merge(commits_and_comments_per_project, satd_comments_per_project, by = "projectID")
  
  #### 10.1.2019 Getting the author number for each project from git_commits
  git_commits_cropped <- git_commits[, c("projectID", "author")]
  
  git_author_commits <- git_commits_cropped %>%
    group_by(projectID) %>%
    summarise(number_of_authors = n_distinct(author))
  # Uncomment this if you want to see more detailed summary.
  # summary(git_author_commits$number_of_authors)
  
  total_commits_and_comment_commits_df <- merge(total_commits_and_comment_commits_df, git_author_commits, by = "projectID")
  
  # Create a vector for cloc and analysistime
  total_commits_and_comment_commits_df$CLOC <- 1:33
  total_commits_and_comment_commits_df$ATime <- 1:33
  rownames(total_commits_and_comment_commits_df) <- total_commits_and_comment_commits_df$projectID
  
  ######### Checking the last commit for each repository to count cloc (Paper table 3)
  library(dplyr)
  git_latest_commits <- git_commits
  git_latest_commits <- git_latest_commits[, c("projectID", "commitHash", "committerDate")]
  
  # This sorts the dates on descending, and then takes the latest commit from each project
  git_last_commit_per_project <- git_latest_commits %>%
    arrange(desc(committerDate)) %>% 
    group_by(projectID) %>% slice(1)
  ############
  
  # Batik has a problem, for some reason. Downloaded following from Github and found
  # manually the hash from july/latest. 
  # xmlgraphics-batik 8db1afd7db9c09bdc981985d79d342906c9ebafb
  # cocoon 3625cf736722a4f501111398bfe65aadf534e69c
  # felix a13ea8ea177933e2911b14e1de55fff4e5196927
  # santuario c9718fa9af008a92ade14d649a451658b83aa926
  
  # Setting the values to dataframe one by one. These are counted by getting the last commit from the
  # Technical Debt dataset, and the counted from command line with cloc.
  table(total_commits_and_comment_commits_df$projectID)
  total_commits_and_comment_commits_df["accumulo", "CLOC"] <- 397939
  total_commits_and_comment_commits_df["ambari", "CLOC"] <- 1463948
  total_commits_and_comment_commits_df["atlas", "CLOC"] <- 219674
  total_commits_and_comment_commits_df["aurora", "CLOC"] <- 151156
  total_commits_and_comment_commits_df["batik", "CLOC"] <- 224836
  total_commits_and_comment_commits_df["beam", "CLOC"] <- 703675
  total_commits_and_comment_commits_df["cocoon", "CLOC"] <- 369109
  total_commits_and_comment_commits_df["commons-bcel", "CLOC"] <- 46571
  
  total_commits_and_comment_commits_df["commons-beanutils", "CLOC"] <- 34567
  total_commits_and_comment_commits_df["commons-cli", "CLOC"] <- 9765
  total_commits_and_comment_commits_df["commons-codec", "CLOC"] <- 23348
  total_commits_and_comment_commits_df["commons-collections", "CLOC"] <- 67581
  
  total_commits_and_comment_commits_df["commons-configuration", "CLOC"] <- 88599
  total_commits_and_comment_commits_df["commons-daemon", "CLOC"] <- 21990
  total_commits_and_comment_commits_df["commons-dbcp", "CLOC"] <- 35498
  total_commits_and_comment_commits_df["commons-dbutils", "CLOC"] <- 8766
  
  total_commits_and_comment_commits_df["commons-digester", "CLOC"] <- 26999
  total_commits_and_comment_commits_df["commons-exec", "CLOC"] <- 5327
  total_commits_and_comment_commits_df["commons-fileupload", "CLOC"] <- 7276
  total_commits_and_comment_commits_df["commons-io", "CLOC"] <- 35116
  total_commits_and_comment_commits_df["commons-jelly", "CLOC"] <- 38201
  total_commits_and_comment_commits_df["commons-jexl", "CLOC"] <- 30798
  total_commits_and_comment_commits_df["commons-jxpath", "CLOC"] <- 28869
  
  total_commits_and_comment_commits_df["commons-net", "CLOC"] <- 31284
  total_commits_and_comment_commits_df["commons-ognl", "CLOC"] <- 22598
  total_commits_and_comment_commits_df["commons-validator", "CLOC"] <- 20575
  total_commits_and_comment_commits_df["commons-vfs", "CLOC"] <- 38508
  total_commits_and_comment_commits_df["felix", "CLOC"] <- 542186
  total_commits_and_comment_commits_df["httpcomponents-client", "CLOC"] <- 70500
  total_commits_and_comment_commits_df["httpcomponents-core", "CLOC"] <- 74026
  total_commits_and_comment_commits_df["mina-sshd", "CLOC"] <- 114667
  total_commits_and_comment_commits_df["santuario", "CLOC"] <- 115087
  total_commits_and_comment_commits_df["zookeeper", "CLOC"] <- 158683
  
  #Counting analysis times
  accumulo_time <- length(seq(from=as.Date("2011-10-01"), to=as.Date("2019-07-01"), by='month'))
  ambari_time <- length(seq(from=as.Date("2011-08-01"), to=as.Date("2019-07-01"), by='month'))
  atlas_time <- length(seq(from=as.Date("2014-11-01"), to=as.Date("2019-07-01"), by='month'))
  aurora_time <- length(seq(from=as.Date("2010-04-01"), to=as.Date("2019-07-01"), by='month'))
  batik_time <- length(seq(from=as.Date("2000-10-01"), to=as.Date("2019-07-01"), by='month'))
  beam_time <- length(seq(from=as.Date("2014-12-01"), to=as.Date("2019-07-01"), by='month'))
  cocoon_time <- length(seq(from=as.Date("2003-02-01"), to=as.Date("2019-07-01"), by='month'))
  commons_bcel_time <- length(seq(from=as.Date("2001-10-01"), to=as.Date("2019-07-01"), by='month'))
  commons_beanutils_time <- length(seq(from=as.Date("2001-03-01"), to=as.Date("2019-07-01"), by='month'))
  commons_cli_time <- length(seq(from=as.Date("2002-06-01"), to=as.Date("2019-07-01"), by='month'))
  commons_codec_time <- length(seq(from=as.Date("2003-04-01"), to=as.Date("2019-07-01"), by='month'))
  commons_collections_time <- length(seq(from=as.Date("2001-04-01"), to=as.Date("2019-07-01"), by='month'))
  commons_configuration_time <- length(seq(from=as.Date("2003-12-01"), to=as.Date("2019-07-01"), by='month'))
  commons_daemon_time <- length(seq(from=as.Date("2003-09-01"), to=as.Date("2019-07-01"), by='month'))
  commons_dbcp_time <- length(seq(from=as.Date("2001-04-01"), to=as.Date("2019-07-01"), by='month'))
  commons_dbutils_time <- length(seq(from=as.Date("2003-11-01"), to=as.Date("2019-07-01"), by='month'))
  commons_digester_time <- length(seq(from=as.Date("2001-05-01"), to=as.Date("2019-07-01"), by='month'))
  commons_exec_time <- length(seq(from=as.Date("2005-07-01"), to=as.Date("2019-07-01"), by='month'))
  commons_fileupload_time <- length(seq(from=as.Date("2002-03-01"), to=as.Date("2019-07-01"), by='month'))
  commons_io_time <- length(seq(from=as.Date("2002-01-01"), to=as.Date("2019-07-01"), by='month'))
  commons_jelly_time <- length(seq(from=as.Date("2002-02-01"), to=as.Date("2019-07-01"), by='month'))
  commons_jexl_time <- length(seq(from=as.Date("2002-04-01"), to=as.Date("2019-07-01"), by='month'))
  commons_jxpath_time <- length(seq(from=as.Date("2001-08-01"), to=as.Date("2019-07-01"), by='month'))
  commons_net_time <- length(seq(from=as.Date("2002-04-01"), to=as.Date("2019-07-01"), by='month'))
  commons_ognl_time <- length(seq(from=as.Date("2011-05-01"), to=as.Date("2019-07-01"), by='month'))
  commons_validator_time <- length(seq(from=as.Date("2002-01-01"), to=as.Date("2019-07-01"), by='month'))
  commons_vfs_time <- length(seq(from=as.Date("2002-07-01"), to=as.Date("2019-07-01"), by='month'))
  felix_time <- length(seq(from=as.Date("2005-07-01"), to=as.Date("2019-07-01"), by='month'))
  httpcomponents_client_time <- length(seq(from=as.Date("2005-12-01"), to=as.Date("2019-07-01"), by='month'))
  httpcomponents_core_time <- length(seq(from=as.Date("2005-02-01"), to=as.Date("2019-07-01"), by='month'))
  mina_sshd_time <- length(seq(from=as.Date("2008-12-01"), to=as.Date("2019-07-01"), by='month'))
  santuario_time <- length(seq(from=as.Date("2001-09-01"), to=as.Date("2019-07-01"), by='month'))
  zookeeper_time <- length(seq(from=as.Date("2007-11-01"), to=as.Date("2019-07-01"), by='month'))
  
  # Setting the analysis times
  total_commits_and_comment_commits_df["accumulo", "ATime"] <- accumulo_time
  total_commits_and_comment_commits_df["ambari", "ATime"] <- ambari_time
  total_commits_and_comment_commits_df["atlas", "ATime"] <- atlas_time
  total_commits_and_comment_commits_df["aurora", "ATime"] <- aurora_time
  total_commits_and_comment_commits_df["batik", "ATime"] <- batik_time
  total_commits_and_comment_commits_df["beam", "ATime"] <- beam_time
  total_commits_and_comment_commits_df["cocoon", "ATime"] <- cocoon_time
  total_commits_and_comment_commits_df["commons-bcel", "ATime"] <- commons_bcel_time
  
  total_commits_and_comment_commits_df["commons-beanutils", "ATime"] <- commons_beanutils_time
  total_commits_and_comment_commits_df["commons-cli", "ATime"] <- commons_cli_time
  total_commits_and_comment_commits_df["commons-codec", "ATime"] <- commons_codec_time
  total_commits_and_comment_commits_df["commons-collections", "ATime"] <- commons_collections_time
  
  total_commits_and_comment_commits_df["commons-configuration", "ATime"] <- commons_configuration_time
  total_commits_and_comment_commits_df["commons-daemon", "ATime"] <- commons_daemon_time
  total_commits_and_comment_commits_df["commons-dbcp", "ATime"] <- commons_dbcp_time
  total_commits_and_comment_commits_df["commons-dbutils", "ATime"] <- commons_dbutils_time
  
  total_commits_and_comment_commits_df["commons-digester", "ATime"] <- commons_digester_time
  total_commits_and_comment_commits_df["commons-exec", "ATime"] <- commons_exec_time
  total_commits_and_comment_commits_df["commons-fileupload", "ATime"] <- commons_fileupload_time
  total_commits_and_comment_commits_df["commons-io", "ATime"] <- commons_io_time
  total_commits_and_comment_commits_df["commons-jelly", "ATime"] <- commons_jelly_time
  total_commits_and_comment_commits_df["commons-jexl", "ATime"] <- commons_jexl_time
  total_commits_and_comment_commits_df["commons-jxpath", "ATime"] <- commons_jxpath_time
  
  total_commits_and_comment_commits_df["commons-net", "ATime"] <- commons_net_time
  total_commits_and_comment_commits_df["commons-ognl", "ATime"] <- commons_ognl_time
  total_commits_and_comment_commits_df["commons-validator", "ATime"] <- commons_validator_time
  total_commits_and_comment_commits_df["commons-vfs", "ATime"] <- commons_vfs_time
  total_commits_and_comment_commits_df["felix", "ATime"] <- felix_time
  total_commits_and_comment_commits_df["httpcomponents-client", "ATime"] <- httpcomponents_client_time
  total_commits_and_comment_commits_df["httpcomponents-core", "ATime"] <- httpcomponents_core_time
  total_commits_and_comment_commits_df["mina-sshd", "ATime"] <- mina_sshd_time
  total_commits_and_comment_commits_df["santuario", "ATime"] <- santuario_time
  total_commits_and_comment_commits_df["zookeeper", "ATime"] <- zookeeper_time
  
  # Correlation between analysis time and SATD percentage (RQ 3.1)
  print("Correlation between analysis time and SATD percentage (RQ 3.1)")
  print(cor(total_commits_and_comment_commits_df$ATime, total_commits_and_comment_commits_df$Satd_percentage, method = "spearman"))
  # -0.2527583

  # Correlation between SATD percent and total commits (RQ 3.1)
  print("Correlation between SATD percent and total commits (RQ 3.1)")
  print(cor(total_commits_and_comment_commits_df$Total_Commit_Amount, total_commits_and_comment_commits_df$Satd_percentage, method = "spearman"))
  # 0.1848262
  
  # Correlation between SATD percent and loc (RQ 3.1)
  print("Correlation between SATD percent and loc (RQ 3.1)")
  print(cor(total_commits_and_comment_commits_df$CLOC, total_commits_and_comment_commits_df$Satd_percentage, method = "spearman"))
  # 0.2065508
  
  # Correlation between SATD percent and number of developers (RQ 3.1)
  print("Correlation between SATD percent and number of developers (RQ 3.1)")
  print(cor(total_commits_and_comment_commits_df$number_of_authors, total_commits_and_comment_commits_df$Satd_percentage, method = "spearman"))
  # 0.3822104
  
  if(isTRUE(return_df)){
    return(total_commits_and_comment_commits_df)
  }
}

############ THIS IS JUST FOR KLSATD AND NOT KLSATD

make_comparison_wordcloud_satd_vs_no_satd <- function(git_comments, combined_vocabulary, processed_column, satd_column,
                                                      max_words=500, create_comparison_clouds=FALSE, create_todo_cloud=TRUE){
  
  vocabulary_list <- paste(combined_vocabulary$term)
  todo_list <- git_comments[git_comments[,satd_column] == 1,]
  todo_list <- todo_list[,processed_column]
  todo_list <- paste(todo_list, collapse = '')
  
  todo_list <- unlist(strsplit(unlist(todo_list),split=" "))
  
  todo_list_match <- todo_list[todo_list %in% intersect(todo_list, vocabulary_list)]
  todo_list_match <- paste(todo_list_match, collapse = ' ')
  
  not_todo_list <- git_comments[git_comments[,satd_column] == 0,]
  not_todo_list <- not_todo_list[,processed_column]
  not_todo_list <- paste(not_todo_list, collapse = '')
  
  not_todo_list <- unlist(strsplit(unlist(not_todo_list),split=" "))
  
  not_todo_list_match <- not_todo_list[not_todo_list %in% intersect(not_todo_list, vocabulary_list)]
  not_todo_list_match <- paste(not_todo_list_match, collapse = ' ')
  
  if(isTRUE(create_comparison_clouds)){
    dataframe_list <- list(todo_list_match, not_todo_list_match)
    corpus <- Corpus(VectorSource(dataframe_list))
    tdm <- TermDocumentMatrix(corpus)
    term.matrix <- as.matrix(tdm)
    colnames(term.matrix) <- c("KL-SATD", "not KL-SATD")
    comparison.cloud(term.matrix, scale=c(3,.2), max.words=max_words, random.order=FALSE, title.size = 1.5)
  }
  else{
    if(isTRUE(create_todo_cloud)){
      corpus <- Corpus(VectorSource(todo_list_match))
      tdm <- TermDocumentMatrix(corpus)
      term.matrix <- as.matrix(tdm)
      
      library(wordcloud)
      term.matrix_sums <- sort(rowSums(term.matrix),decreasing=TRUE)
      term.matrix_df <- data.frame(word = names(term.matrix_sums),freq=term.matrix_sums)
      table(term.matrix_df$freq)
      wordcloud(term.matrix_df$word,term.matrix_df$freq, scale=c(2,.2),min.freq=1,
                max.words=max_words, random.order=FALSE, rot.per=.15, colors=brewer.pal(8,"Dark2"))
    }
  }
}