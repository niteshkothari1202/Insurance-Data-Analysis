source("exploratory_data_analysis.R")
source("correlations.R")
library(rpart)
library(rpart.plot)
library(dplyr)



set.seed(1234)
create_train_test <- function(data, size = 0.2, train = TRUE) {
  n_row = nrow(data)
  total_row = size * n_row
  train_sample <- 1: total_row
  if (train == TRUE) {
    return (data[train_sample, ])
  } else {
    return (data[-train_sample, ])
  }
}
data_train <- create_train_test(model_data, 0.8, train = TRUE)
data_train = data_train %>% select(POL_STATUS, total_coverage, quotemonth_s,Resiliated,P1_MAR_STATUS,P1_SEX,PAYING_GUESTS)
data_test <- create_train_test(model_data, 0.8, train = FALSE)
output_tree <- rpart(as.factor(POL_STATUS) ~.,data = data_train)
         # client_age +as.factor(P1_SEX) + as.factor(CLAIM3YEARS)+as.factor(P1_MAR_STATUS)
jpeg("decision_tree_plot.jpeg")
rpart.plot(output_tree)
dev.off()

predict_model = predict(output_tree,data_test,type="class")
table_p = table(data_test$POL_STATUS,predict_model)
row_table = row.names(table_p)
row_table
precision = c()
recall = c()
f1score = c()
x = 1
for (i in seq(1,length(row_table))) {
  precision[i] = table_p[i,i]/sum(table_p[,i])
  recall[i] = table_p[i,i]/sum(table_p[i,])
  f1score[i] = (2*precision[i]*recall[i])/(precision[i]+recall[i])
  
}
table_r = cbind(precision, recall, f1score)
row.names(table_r) = row_table
table_r = table_r*100

