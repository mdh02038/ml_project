library(caret)
library(randomForest)
training = read.csv(file="pml-training.csv",stringsAsFactors=FALSE)
testing = read.csv(file="pml-testing.csv",header=TRUE)

inTrain <- createDataPartition(y=training$classe,p=0.75,list=FALSE)
ptraining1 <- training[inTrain,]
ptesting1 <- training[-inTrain,]

ptraining <- subset(ptraining1, select=-c(user_name,X,raw_timestamp_part_1,raw_timestamp_part_2,cvtd_timestamp,new_window,num_window,
                                       kurtosis_roll_belt,
                                       kurtosis_picth_belt,
                                       kurtosis_yaw_belt,
                                       skewness_roll_belt,
                                       skewness_roll_belt.1,
                                       skewness_yaw_belt,
                                       max_yaw_belt,
                                       min_yaw_belt,
                                       amplitude_yaw_belt,
                                       kurtosis_roll_arm,
                                       kurtosis_picth_arm,
                                       kurtosis_yaw_arm,
                                       skewness_roll_arm,
                                       skewness_pitch_arm,
                                       skewness_yaw_arm,
                                       kurtosis_roll_dumbbell,
                                       kurtosis_picth_dumbbell,
                                       kurtosis_yaw_dumbbell,
                                       skewness_roll_dumbbell,
                                       skewness_pitch_dumbbell,
                                       skewness_yaw_dumbbell,
                                       max_yaw_dumbbell,
                                       min_yaw_dumbbell,
                                       amplitude_yaw_dumbbell,
                                       kurtosis_roll_forearm,
                                       kurtosis_picth_forearm,
                                       kurtosis_roll_forearm,
                                       kurtosis_yaw_forearm,
                                       skewness_roll_forearm,
                                       skewness_pitch_forearm,
                                       skewness_yaw_forearm,
                                       max_yaw_forearm,
                                       min_yaw_forearm,
                                       amplitude_yaw_forearm
                                       ))
ptesting <- subset(ptesting1, select=-c(user_name,X,raw_timestamp_part_1,raw_timestamp_part_2,cvtd_timestamp,new_window,num_window,
                                       kurtosis_roll_belt,
                                       kurtosis_picth_belt,
                                       kurtosis_yaw_belt,
                                       skewness_roll_belt,
                                       skewness_roll_belt.1,
                                       skewness_yaw_belt,
                                       max_yaw_belt,
                                       min_yaw_belt,
                                       amplitude_yaw_belt,
                                       kurtosis_roll_arm,
                                       kurtosis_picth_arm,
                                       kurtosis_yaw_arm,
                                       skewness_roll_arm,
                                       skewness_pitch_arm,
                                       skewness_yaw_arm,
                                       kurtosis_roll_dumbbell,
                                       kurtosis_picth_dumbbell,
                                       kurtosis_yaw_dumbbell,
                                       skewness_roll_dumbbell,
                                       skewness_pitch_dumbbell,
                                       skewness_yaw_dumbbell,
                                       max_yaw_dumbbell,
                                       min_yaw_dumbbell,
                                       amplitude_yaw_dumbbell,
                                       kurtosis_roll_forearm,
                                       kurtosis_picth_forearm,
                                       kurtosis_roll_forearm,
                                       kurtosis_yaw_forearm,
                                       skewness_roll_forearm,
                                       skewness_pitch_forearm,
                                       skewness_yaw_forearm,
                                       max_yaw_forearm,
                                       min_yaw_forearm,
                                       amplitude_yaw_forearm
))
ptraining <- ptraining[,colSums(is.na(ptraining))==0]
ptesting <- ptesting[,colSums(is.na(ptesting))==0]


model <- randomForest(as.factor(classe) ~ ., data=ptraining, mtry=2,importance=TRUE,do.trace=20)
#model2 <- train(as.factor(classe) ~ ., data=ptraining, method="rf",importance=TRUE,do.trace=20)

results <- predict(model,ptraining)
model
sum(results!=ptraining$classe)
sum(results==ptraining$classe)

results <- predict(model,ptesting)
sum(results!=ptesting$classe)
sum(results==ptesting$classe)

out <- as.character(predict(model,testing))

pml_write_files = function(x){
    n = length(x)
    for(i in 1:n){
        filename = paste0("problem_id_",i,".txt")
        write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
    }
}

pml_write_files(out)
