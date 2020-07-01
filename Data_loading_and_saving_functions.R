library("rjson")
library("data.table")

#' @title Load TDD Comments
#' @description Loads a previously created csv-file with comments
#' @param file_path Path to the directory where the data is
#' @param file_name Name of the file
#' @return Returns a csv-file
#' @details DETAILS
#' @examples 
#' \dontrun{
#' load_technical_debt_dataset_comments("path/to_data", "lots_of_comments.datafile")
#' }
#' @rdname load_technical_debt_dataset_comments
#' @export 
load_technical_debt_dataset_comments <- function(file_path, file_name){
  tdd_comments <- fread(paste0(file_path, file_name), sep = ",", header = TRUE, showProgress = FALSE)
  return(tdd_comments)
}

#' @title Save file to disk
#' @description Saves file to disk
#' @param file_to_save File to save
#' @param file_save_path File path, where to save the file. On default uses the path from main script, Default: save_path
#' @param file_name Name of the file to be saved.
#' @return Nothing.
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname save_file_to_disk
#' @export 
save_file_to_disk <- function(file_to_save, file_save_path=save_path, file_name){
  saveRDS(file_to_save, file=paste0(file_save_path, file_name))
}

#' @title Read file from disk
#' @description Reads file from disk
#' @param file_save_path File path, where the file is saved. On default uses the path from main script, Default: save_path
#' @param file_name Name of the file.
#' @return A file object
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname read_file_from_disk
#' @export 
read_file_from_disk <- function(file_save_path=save_path, file_name){
  saved_file <- readRDS(file=paste0(file_save_path, file_name))
  return(saved_file)
}
