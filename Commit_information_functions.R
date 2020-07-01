#' @title Counts commits and comments per project
#' @description Function to count and print commit and comment information
#' @param df_with_commits DF containing columns for projects and identifiers
#' @param project_column Column where the projects are
#' @param hash_column Column for identifiers, e.g. commit hashes
#' @param return_df A boolean value, whether to return a df created in the function, DEFAULT: FALSE.
#' @return Returns nothing, but prints out information
#' @details NONE
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname count_commits_and_comments_per_project
#' @export 
count_commits_and_comments_per_project <- function(df_with_commits, project_column, hash_column, return_df=FALSE){
  git_comments_unique_hashes <- as.data.frame(df_with_commits[,c(project_column, hash_column)])
  git_comments_unique_hashes <- unique(git_comments_unique_hashes)
  git_unique_commit_comments <- as.data.frame(table(git_comments_unique_hashes[,project_column]))
  colnames(git_unique_commit_comments) <- c(project_column, "Comment_Commit_Amount")
  
  git_commits <- as.data.frame(load_technical_debt_dataset_comments(data_path, "git_commits.csv"))
  commits_per_project_df <- as.data.frame(table(git_commits$projectID))
  colnames(commits_per_project_df) <- c(project_column, "Total_Commit_Amount")
  
  total_commits_and_comment_commits_df <- merge(commits_per_project_df, git_unique_commit_comments, by = project_column)
  total_commits_and_comment_commits_df$Comment_Percentage <- total_commits_and_comment_commits_df$Comment_Commit_Amount / total_commits_and_comment_commits_df$Total_Commit_Amount *100
  
  #total_commits_and_comment_commits_df <- merge(total_commits_and_comment_commits_df, git_author_commits, by = project_column)
  # Print the projects with most and least commits, and the summary
  # print(head(total_commits_and_comment_commits_df[order(total_commits_and_comment_commits_df$Total_Commit_Amount, decreasing = TRUE),]))
  # print(tail(total_commits_and_comment_commits_df[order(total_commits_and_comment_commits_df$Total_Commit_Amount, decreasing = TRUE),]))
  # print(summary(total_commits_and_comment_commits_df[,2:4]))
  # print(paste("Number of Commits in Total: ", sum(total_commits_and_comment_commits_df$Total_Commit_Amount)))
  # print(paste("Comment Commit Total: ", sum(total_commits_and_comment_commits_df$Comment_Commit_Amount)))
  
  if(isTRUE(return_df)){
    return (total_commits_and_comment_commits_df)  
  }
}

count_satd_comments_and_all_comments_per_project <- function (df_with_commits, project_column, satd_column, return_df=FALSE, printinfo=FALSE){
  todo_code_comments <- df_with_commits[df_with_commits[,satd_column] == 1,]
  git_todo_comments_by_project <- as.data.frame(table(todo_code_comments[,project_column]))
  colnames(git_todo_comments_by_project) <- c(project_column, "TODO_Comment_Amount")
  #print(git_todo_comments_by_project)
  
  git_comments_by_project <- as.data.frame(table(df_with_commits[,project_column]))
  colnames(git_comments_by_project) <- c(project_column, "Total_Comment_Amount")
  #print(git_comments_by_project)
  
  git_combined_comments <- merge(git_comments_by_project, git_todo_comments_by_project, by = project_column, all = TRUE)
  git_combined_comments[is.na(git_combined_comments)] <- 0
  colnames(git_combined_comments) <- c(project_column, "Total_Comments", "Satd_Comments")
  git_combined_comments$Satd_percentage <- git_combined_comments$Satd_Comments / git_combined_comments$Total_Comments * 100
  
  if(isTRUE(printinfo)){
    print(head(git_combined_comments[order(git_combined_comments$Satd_percentage, decreasing = TRUE),]))
    print(tail(git_combined_comments[order(git_combined_comments$Satd_percentage, decreasing = TRUE),]))
    print(summary(git_combined_comments[,2:4]))
    print(paste("Number of Comments in Total: ", sum(git_combined_comments$Total_Comments)))
    print(paste("SATD Commit Total: ", sum(git_combined_comments$Satd_Comments)))
  }
  
  if(isTRUE(return_df)){
    return(git_combined_comments)
  }
}

#' @title Add authors to original df
#' @description Adds authors as an extra column to the df
#' @param df_to_add_authors Dataframe where the new column will be inserted
#' @param hash_column Column in the df, which contains hash information
#' @return Returns the inserted df, with an extra column containing the author information
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname add_authors_to_original_df
#' @export 
add_authors_to_original_df <- function(df_to_add_authors, hash_column){
  git_commits <- as.data.frame(load_technical_debt_dataset_comments(data_path, "git_commits.csv"))
  git_commits <- git_commits[,c("commitHash", "author")]
  df_to_add_authors <- merge(df_to_add_authors, git_commits, by.x = hash_column, by.y = "commitHash")
  return(df_to_add_authors)
}
