if(!require(caret)){install.packages('caret', dep=TRUE);require(caret)}
if(!require(data.table)){install.packages('data.table', dep=TRUE);require(data.table)}
if(!require(gbm)){install.packages('gbm', dep=TRUE);require(gbm)}

trainSet <- fread(file="trainSet.csv")

trainSet$result <- as.factor(trainSet$result)

fitControl <- trainControl(
  method = "repeatedcv",
  number = 5,
  repeats = 5
)

#Error in { : task 1 failed - "arguments imply differing number of rows: 0, 336"
model_gbm_caret<-train(result~ +size_delta+inserted_line+deleted_line+size, 
                       data = trainSet, 
                       method='gbm', 
                       trControl = fitControl,
                       verbose=TRUE)

#no error
model_gbm<-gbm(result~+size_delta+inserted_line+deleted_line+size, data=trainSet, cv.folds = 2)


