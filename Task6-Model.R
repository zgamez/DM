setwd("/coursera/task6")


#rm(list = ls())

library(caret)
library(randomForest)
library(parallel)
library(doParallel)
library(MLmetrics)

testfile <- read.csv("formodelingB.csv", header = TRUE, sep = ",")

## Cleaning

# Convert classe to factor

testfile$hygiene <- as.factor(testfile$hygiene)
testfile$zip <- as.factor(testfile$zip)
testfile$stars <- as.factor(testfile$stars)

# Set train-test files

set.seed(123456)
fortrain <- createDataPartition(testfile$hygiene, p = 0.95, list=FALSE)
training <- testfile[fortrain,]
validation <- testfile[-fortrain,]

#rm(fortrain)

## A measure based on regression smoothers
RocImp <- filterVarImp(x = training[, -ncol(training)], y = training$hygiene)
RocImp[order(RocImp$X0, decreasing = TRUE), , drop = FALSE][1:6,]

###########################################################################
# Full Model
#
#
set.seed(123456)
fortrain <- createDataPartition(testfile$hygiene, p = 0.95, list=FALSE)
training <- testfile[fortrain,]
validation <- testfile[-fortrain,]

# Train model - GBM  Accuracy : 0.5648 90:Accuracy : 0.6481 95:Accuracy : 0.6923 
#
#> F1_Score(y_pred = ValPred, y_true = validation$hygiene, positive = "0")
#[1] 0.7142857

cl <- makePSOCKcluster(2)
registerDoParallel(cl)

system.time(
  modFitGBM <- train(hygiene ~., method="gbm", data=training,  
                      trControl=trainControl(method="cv", 10,
                                             allowParallel=TRUE))
)

stopCluster(cl)

save(modFitGBM, file = "modelfull-GBMcp95cv10.rda")
modFitGBM

ValPred <- predict(modFitGBM, validation)  ### Change model here  <- <-
confusionMatrix(ValPred, validation$hygiene)

F1_Score(y_pred = ValPred, y_true = validation$hygiene, positive = "0")


###########################################################################
# LDA no - Sent yes
#
#

# Remove irrelevant columns
myvars <- names(testfile) %in% c("lda")
testfile2 <- testfile[!myvars]


set.seed(123456)
fortrain <- createDataPartition(testfile2$hygiene, p = 0.95, list=FALSE)
training <- testfile2[fortrain,]
validation <- testfile2[-fortrain,]

# Train model - LOGISTICS REGRESSION Accuracy : 0.7308 
#
#> F1_Score(y_pred = ValPred, y_true = validation$hygiene, positive = "0")
#[1] 0.7407407
cl <- makePSOCKcluster(2)
registerDoParallel(cl)

system.time(
  modFitLR2 <- train(hygiene ~., method="multinom", data=training, 
                     trControl=trainControl(method="cv", 10,
                                            allowParallel=TRUE))
)

stopCluster(cl)

save(modFitLR2, file = "modelnoLDA-LRp95cv10.rda")
modFitLR2

ValPred <- predict(modFitLR2, validation)  ### Change model here  <- <-
confusionMatrix(ValPred, validation$hygiene)
F1_Score(y_pred = ValPred, y_true = validation$hygiene, positive = "0")

#modFitLR2$finalModel
exp(coef(modFitLR2$finalModel))


# Train model - GBM Accuracy : 0.7692
#
#> F1_Score(y_pred = ValPred, y_true = validation$hygiene, positive = "0")
#[1] 0.75
cl <- makePSOCKcluster(2)
registerDoParallel(cl)

system.time(
  modFitGBM2 <- train(hygiene ~., method="gbm", data=training,  
                           trControl=trainControl(method="cv", 10,
                                                  allowParallel=TRUE))
)

stopCluster(cl)

save(modFitGBM2, file = "modelnoLDA-GBMcp95cv10.rda")
modFitGBM2

ValPred <- predict(modFitGBM2, validation)  ### Change model here  <- <-
confusionMatrix(ValPred, validation$hygiene)

F1_Score(y_pred = ValPred, y_true = validation$hygiene, positive = "0")


# Model Comparison
#

rValues <- resamples(list(lr=modFitLR2,gbm=modFitGBM2))
rValues$values
summary(rValues)
rValmod1 <- rValues$values$`lr~Accuracy`
rValmod2 <- rValues$values$`gbm~Accuracy`
xyplot(rValmod1 ~ rValmod2)        # scatter plot



# Train model - Boosted Tree C5.0  Accuracy : 0.6538  
#
#> F1_Score(y_pred = ValPred, y_true = validation$hygiene, positive = "0")
#[1] 0.5714286

cl <- makePSOCKcluster(2)
registerDoParallel(cl)

system.time(
  
  modFitC502 <- train(hygiene ~., method="C5.0", data=training, 
                         trControl=trainControl(method="cv", 10,
                                                allowParallel=TRUE),
                         tuneGrid = expand.grid(model = "tree", winnow = FALSE,
                                                trials = c(1:10, (1:5)*10)))
)

stopCluster(cl)
modFitC502

save(modFitC502, file = "modelnoLDA-C502cp95cv10.rda")

ValPred <- predict(modFitC502, validation)  ### Change model here  <- <-
confusionMatrix(ValPred, validation$hygiene)
F1_Score(y_pred = ValPred, y_true = validation$hygiene, positive = "0")

# Train model - RANDOM FOREST Accuracy : 0.6538 
#
#> F1_Score(y_pred = ValPred, y_true = validation$hygiene, positive = "0")
#[1] 0.64

cl <- makePSOCKcluster(2)
registerDoParallel(cl)

system.time(
  modFitRF2 <- train(hygiene ~., method="rf", data=training,  
                     trControl=trainControl(method="cv", 10,
                                            allowParallel=TRUE))
)

stopCluster(cl)

#print(modFitRF2$finalModel)

save(modFitRF2, file = "modelnoLDA-RFp95cv10.rda")

#VI <- varImp(modFitRF2, scale = FALSE)  
#plot(VI, main = "Variable Importance", top = 10)

ValPred <- predict(modFitRF2, validation)  ### Change model here  <- <-
confusionMatrix(ValPred, validation$hygiene)
F1_Score(y_pred = ValPred, y_true = validation$hygiene, positive = "0")

# Train model - SVM   Accuracy : 0.7308 
#
#> F1_Score(y_pred = ValPred, y_true = validation$hygiene, positive = "0")
#[1] 0.6956522

cl <- makePSOCKcluster(2)
registerDoParallel(cl)

system.time(
  modFitSVM2 <- train(hygiene ~., method="svmLinear", data=training, 
                      trControl=trainControl(method="cv", 10,
                                             allowParallel=TRUE))
)

stopCluster(cl)

print(modFitSVM2$finalModel)

save(modFitSVM2, file = "modelnoLDA-SVMp95cv10.rda")


ValPred <- predict(modFitSVM2, validation)  ### Change model here  <- <-
confusionMatrix(ValPred, validation$hygiene)
F1_Score(y_pred = ValPred, y_true = validation$hygiene, positive = "0")




##########################################################################

###########################################################################
# LDA no - Sent no
#
#

# Remove irrelevant columns
myvars <- names(testfile) %in% c("lda","sent")
testfile3 <- testfile[!myvars]


set.seed(123456)
fortrain <- createDataPartition(testfile3$hygiene, p = 0.95, list=FALSE)
training <- testfile3[fortrain,]
validation <- testfile3[-fortrain,]

# Train model - LOGISTICS REGRESSION  Accuracy : 0.7308
#
#> F1_Score(y_pred = ValPred, y_true = validation$hygiene, positive = "0")
#[1] 0.7407407

cl <- makePSOCKcluster(2)
registerDoParallel(cl)

system.time(
  modFitLR3 <- train(hygiene ~., method="multinom", data=training, 
                     trControl=trainControl(method="cv", 10,
                                            allowParallel=TRUE))
)

stopCluster(cl)

save(modFitLR3, file = "modelnoLDAnosent-LRp95cv10.rda")

ValPred <- predict(modFitLR3, validation)  ### Change model here  <- <-
confusionMatrix(ValPred, validation$hygiene)
F1_Score(y_pred = ValPred, y_true = validation$hygiene, positive = "0")

# Train model - GBM  Accuracy : 0.8077
#
#> F1_Score(y_pred = ValPred, y_true = validation$hygiene, positive = "0")
#[1] 0.7826087

cl <- makePSOCKcluster(2)
registerDoParallel(cl)

system.time(
  modFitGBM3 <- train(hygiene ~., method="gbm", data=training,  
                      trControl=trainControl(method="cv", 10,
                                             allowParallel=TRUE))
)

stopCluster(cl)

save(modFitGBM3, file = "modelnoLDAnosent-GBMcp95cv10.rda")

ValPred <- predict(modFitGBM3, validation)  ### Change model here  <- <-
confusionMatrix(ValPred, validation$hygiene)
F1_Score(y_pred = ValPred, y_true = validation$hygiene, positive = "0")

# Train model - Boosted Tree Accuracy : 0.8077 
#
#> F1_Score(y_pred = ValPred, y_true = validation$hygiene, positive = "0")
#[1] 0.7826087

cl <- makePSOCKcluster(2)
registerDoParallel(cl)

system.time(
  
  modFitC503 <- train(hygiene ~., method="C5.0", data=training, 
                      trControl=trainControl(method="cv", 10,
                                             allowParallel=TRUE),
                      tuneGrid = expand.grid(model = "tree", winnow = FALSE,
                                             trials = c(1:10, (1:5)*10)))
)

stopCluster(cl)


save(modFitC503, file = "modelnoLDAnosent-C503cp95cv10.rda")

ValPred <- predict(modFitC503, validation)  ### Change model here  <- <-
confusionMatrix(ValPred, validation$hygiene)
F1_Score(y_pred = ValPred, y_true = validation$hygiene, positive = "0")

# Train model - RANDOM FOREST  Accuracy : 0.5769 
#
#> F1_Score(y_pred = ValPred, y_true = validation$hygiene, positive = "0")
#[1] 0.5217391

cl <- makePSOCKcluster(2)
registerDoParallel(cl)

system.time(
  modFitRF3 <- train(hygiene ~., method="rf", data=training,  
                     trControl=trainControl(method="cv", 10,
                                            allowParallel=TRUE))
)

stopCluster(cl)

print(modFitRF3$finalModel)

save(modFitRF3, file = "modelnoLDAnoSent-RFp95cv10.rda")

#VI <- varImp(modFitRF3, scale = FALSE)  
#plot(VI, main = "Variable Importance", top = 10)

ValPred <- predict(modFitRF3, validation)  ### Change model here  <- <-
confusionMatrix(ValPred, validation$hygiene)
F1_Score(y_pred = ValPred, y_true = validation$hygiene, positive = "0")

# Train model - SVM   Accuracy : 0.7308  
#
#> F1_Score(y_pred = ValPred, y_true = validation$hygiene, positive = "0")
#[1] 0.6956522

cl <- makePSOCKcluster(2)
registerDoParallel(cl)

system.time(
  modFitSVM3 <- train(hygiene ~., method="svmLinear", data=training, 
                      trControl=trainControl(method="cv", 10,
                                             allowParallel=TRUE))
)

stopCluster(cl)

print(modFitSVM3$finalModel)

save(modFitSVM3, file = "modelnoLDAnoSent-SVMp95cv10.rda")

#VI <- varImp(modFitSVM3, scale = FALSE)  
#plot(VI, main = "Variable Importance", top = 10)

ValPred <- predict(modFitSVM3, validation)  ### Change model here  <- <-
confusionMatrix(ValPred, validation$hygiene)
F1_Score(y_pred = ValPred, y_true = validation$hygiene, positive = "0")

