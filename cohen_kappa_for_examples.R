#rm(list = ls())
#gc()
source("Data_loading_and_saving_functions.R")
source("Logistic_regression_functions.R")
source("Vocabulary_creation_and_cleaning_functions.R")
source("Commit_information_functions.R")
source("Active_learning_functions.R")
source("User_related_functions.R")
source("Correlation_information_functions.R")
library("sinew")
library("tidyr")

save_path <- "./save/"
data_path <- "./data/"

library(openxlsx)
library(irr)
csvchanges <- load_technical_debt_dataset_comments(data_path, "git_changes.csv")
csvchanges20 <- csvchanges[1:20,]
write.csv(csvchanges20, file = "csv_input_20_examples.csv")

pred1 <- read.xlsx(paste0(data_path,"100_1.xlsx"))
pred2 <- read.xlsx(paste0(data_path,"100_2.xlsx"))
pred2 <- pred2[, -c(4)]

# Choose here, whether to omit the rows where one rater had a level and the other one didn't
# or if you'd like to replace NA's with 0s
pred1 <-na.omit(pred1)
pred2 <-na.omit(pred2)
table(pred1$SATD)
table(pred2$SATD)
#pred1[is.na(pred1)] <- 9
#pred2[is.na(pred2)] <- 9


pred1$SATD <- as.factor(pred1$SATD)
pred2$SATD <- as.factor(pred2$SATD)

pred3 <- merge(pred1, pred2, by = "index")
kappa2(pred3[,c("SATD.x", "SATD.y")], "squared")
cohen.kappa(pred3[,c("SATD.x", "SATD.y")])

pred2s <- pred3[which(pred3$SATD.x == 2 & pred3$SATD.y == 2),]

# Transform all 1s and 2s to 1 to check kappa differently
table(pred2$SATD)
pred1$SATD <- ifelse(pred1$SATD > 0, 1, 0)
pred2$SATD <- ifelse(pred2$SATD > 0, 1, 0)
pred3 <- merge(pred1, pred2, by = "index")
pred2s <- pred3[which(pred3$SATD.x == 1 & pred3$SATD.y == 1),]

indexes_for_satd <- pred2s$index
tdd_comments_combined[tdd_comments_combined$index == 16430,]
