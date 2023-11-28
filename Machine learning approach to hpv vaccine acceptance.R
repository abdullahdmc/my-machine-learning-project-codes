setwd("C:/Users/boss/OneDrive/Folder S/My Research Acitivity/NSU researches/HPV Vaccine acceptance/Dataset")
#loading essential libraries
library(gtsummary)
library(flextable)
library(haven)
library(dplyr)
library(Hmisc)
library(officer)
library(knitr)
library(caTools)



#loading hpv dataset
dt <- read_dta("hpv dataset v4.dta")

#Declaring factor variables
dt$sex <- as_factor(dt$sex)
dt$div <- as_factor(dt$div)
dt$dist <- as_factor(dt$dist)
dt$marital_status <-as_factor(dt$marital_status)
dt$marital2 <-as_factor(dt$marital2)
dt$religion <- as_factor(dt$religion)
dt$residence <- as_factor(dt$residence)
dt$occupation <- as_factor(dt$occupation)
dt$occup2 <- as_factor(dt$occup2)
dt$hcw <- as_factor(dt$hcw)
dt$income_cat <- as_factor(dt$income_cat)
dt$rel_imp <-as_factor(dt$rel_imp)
dt$routine_health3 <-as_factor(dt$routine_health3)
dt$hpv_heard <-as_factor(dt$hpv_heard)
dt$vac_hear <-as_factor(dt$vac_hear)
dt$cancer_hear <-as_factor(dt$cancer_hear)
dt$canvac_hear <-as_factor(dt$canvac_hear)
dt$vul1 <-as_factor(dt$vul1)
dt$vul2 <-as_factor(dt$vul2)
dt$vul3 <-as_factor(dt$vul3)
dt$vul4 <-as_factor(dt$vul4)
dt$vul5 <-as_factor(dt$vul5)
dt$sev1 <-as_factor(dt$sev1)
dt$sev2 <-as_factor(dt$sev2)
dt$sev3 <-as_factor(dt$sev3)
dt$sev4 <-as_factor(dt$sev4)
dt$sev5 <-as_factor(dt$sev5)
dt$ben1 <-as_factor(dt$ben1)
dt$ben2 <-as_factor(dt$ben2)
dt$ben3 <-as_factor(dt$ben3)
dt$ben4 <-as_factor(dt$ben4)
dt$ben5 <-as_factor(dt$ben5)
dt$bar1 <-as_factor(dt$bar1)
dt$bar2 <-as_factor(dt$bar2)
dt$bar3 <-as_factor(dt$bar3)
dt$bar4 <-as_factor(dt$bar4)
dt$bar5 <-as_factor(dt$bar5)
dt$cu1 <-as_factor(dt$cu1)
dt$cu2 <-as_factor(dt$cu2)
dt$cu3 <-as_factor(dt$cu3)
dt$cu4 <-as_factor(dt$cu4)
dt$vul_cat <- as_factor(dt$vul_cat)
dt$sev_cat <- as_factor(dt$sev_cat)
dt$ben_cat <- as_factor(dt$ben_cat)
dt$bar_cat <- as_factor(dt$bar_cat)
dt$cue <- as_factor(dt$cue)
dt$vac_accept <-as_factor(dt$vac_accept)
dt$vac_if_gov <-as_factor(dt$vac_if_gov)
dt$epi <- as_factor(dt$epi)
dt$family_type <- as_factor(dt$family_type)
dt$accept <- as_factor(dt$accept)

#labeling important variables
label(dt$age) <- "Age (years)"
label(dt$sex) <- "Sex"
label(dt$marital_status) <- "Marital status"
label(dt$marital2) <- "Marital Status"
label(dt$religion) <- "Religion"
label(dt$residence) <- "Residence"
label(dt$education_years) <- "Years of Education"
label(dt$occupation) <- "Occupation"
label(dt$occup2) <- "Occupation"
label(dt$hcw) <- "Health Care Worker"
label(dt$income_cat) <- "Monthly household income (BDT)"
label(dt$family_member) <- "Number of family members"
label(dt$family_type) <- "Family Type"
label(dt$children_0) <- "Number of children"
label(dt$daugther_0) <- "Number of daughter"
label(dt$routine_health3) <- "Routine Health Checkup"
label(dt$hpv_heard) <- "Heard about Human Papilloma Virus"
label(dt$vac_hear) <- "Heard about HPV Vaccine"
label(dt$cancer_hear) <- "Heard about Cervical Cancer"
label(dt$canvac_hear) <- "Heard about Cervical Cancer Vaccination"
label(dt$vul1) <- "HPV can cause sexually transmitted disease"
label(dt$vul2) <- "HPV can cause condyloma/genital warts"
label(dt$vul3) <- "There is a risk for young women to contract HPV"
label(dt$vul4) <- "HPV infection is a serious health concern"
label(dt$vul5) <- "I worry that my child might get HPV"
label(dt$sev1) <- "People with HPV might not have symptoms"
label(dt$sev2) <- "HPV-associated warts could be uncomfortable or itchy"
label(dt$sev3) <- "HPV-associated wart is a serious condition"
label(dt$sev4) <- "HPV-associated cervical cancer is a serious condition"
label(dt$sev5) <- "HPV-associated cervical cancer can occur in middle age"
label(dt$ben1) <- "I trust vaccinations as it is getting better all the time because of research"
label(dt$ben2) <- "The HPV vaccine is effective in preventing condyloma/genital wart among my daughter"
label(dt$ben3) <- "HPV vaccine will be effective in preventing cervical cancer in my daughter"
label(dt$ben4) <- "Vaccinated girls are less likely to get HPV than unvaccinated girls"
label(dt$ben5) <- "HPV vaccination increases awareness of sexually transmitted diseases"
label(dt$bar1) <- "I shall not vaccinate my daughter as it is painful"
label(dt$bar2) <- "I shall not vaccinate my daughter as the HPV vaccine can cause an adverse effect"
label(dt$bar3) <- "I shall not vaccinate my daughter as the HPV vaccine needs two injections"
label(dt$bar4) <- "The HPV vaccine is so new that I want to wait a while before deciding if my daughter should get it"
label(dt$bar5) <- "I shall vaccinate my daughter even if it is not free despite knowing that the HPV vaccine cost around 2500 BDT"
label(dt$cu1) <- "Have a personal history of cancer"
label(dt$cu2) <- "Friends or family members have history of cancer"
label(dt$cu3) <- "Have personal history of cervical cancer"
label(dt$cu4) <- "Friends or family members have history of cervical cancer"
label(dt$vul_cat) <- "Percieved Vulnerability"
label(dt$sev_cat) <- "Percieved Severity"
label(dt$ben_cat) <- "Percieved Benefit"
label(dt$bar_cat) <- "Percieved Barrier"
label(dt$cue) <- "Cues to Action"
label(dt$vac_accept) <- "Willing to give HPV vaccine to daughter"
label(dt$vac_if_gov) <- "Willing to give HPV vaccine to daughter if Governemnt provides free of cost"
label(dt$accept) <- "Accepts Vaccine"

##Deleting rows with missing values in factor variables (necessary for RF, KNN, MLP)
dt <- filter(dt, !is.na(dt$income_cat))
dt <- filter(dt, !is.na(dt$family_type))
dt <- filter(dt, !is.na(dt$education_years))
colSums(is.na(dt))


##Preparing data for Multi Layer Perceptron using neural net; it requires creating dummy variables.
dt$marital2 <- recode(dt$marital2, "Living with spouse"="Spouse", "Living without spouse"="NoSpouse", "Others"="Other")
dt$residence <- recode(dt$residence, "Rural" = "Rural", "Semi-urban"= "SemiUrban", "Urban"="Urban")
dt$income_cat <- recode(dt$income_cat, "<=20k" = "LessThenTwenty", "20k-35k"= "Twenty_ThirtyFive", "35k-50k"="ThirtyFive_Fifty", ">50k"="MoreThenFifty")

#Train-test split
train_split_factor <- 0.70
set.seed(299) #for reproducibility
train_cases <- sample(1:nrow(dt), round(nrow(dt)*train_split_factor), replace = FALSE)
train_cases <- train_cases[order(train_cases)]
test_cases <- setdiff(1:nrow(dt), train_cases)
data_train <- dt[train_cases, ]
data_test <- dt[test_cases, ]

# Specify the formula for building all the models
formula <- accept ~ age + sex + marital2 + residence + education_years + occup2 + income_cat + family_type +
  routine_health3 + hpv_heard + vac_hear + cancer_hear + canvac_hear + vul_cat + sev_cat + ben_cat + bar_cat + cue


##Loading libraries for classification
library(caret) #creates confusion matrix and also knn
library(pROC) #creates ROC


##Decision tree model (recursive partitioning and regression trees)
library(rpart)

# Fit the RPART model using the training data
tree_model <- rpart(formula, data = data_train)
plot(tree_model)
summary(tree_model)

# Predict using the test data
predictions <- predict(tree_model, newdata = data_test, type = "class")
predictions2 <- predict(tree_model, newdata = data_test, type = "vector")

# Extracing the acutal values
dt_accept <- data_test$accept

# Create a confusion matrix
conf_matrix <- confusionMatrix(predictions, dt_accept)
print(conf_matrix)

# Calculate other classification metrics
accuracy <- conf_matrix$overall['Accuracy']
precision <- conf_matrix$byClass['Pos Pred Value']
recall <- conf_matrix$byClass['Sensitivity']
f1_score <- conf_matrix$byClass['F1']
roc_auc <- roc(actual_values, predictions2)

cat("Accuracy:", accuracy, "\n")
cat("Precision:", precision, "\n")
cat("Recall:", recall, "\n")
cat("F1-Score:", f1_score, "\n")
cat("ROC-AUC:", auc(roc_auc), "\n")


##Creating Random Forest
library(randomForest)
rf_model <- randomForest(formula, data = data_train)
print(rf_model)
round(importance(rf_model),2)

rf_predictions <- predict(rf_model, newdata = data_test, type = "response")
rf_predictions2 <- ifelse(rf_predictions == "Yes", 1, 0)
# Create a confusion matrix 
cf_rf <- confusionMatrix(rf_predictions, actual_values)
print(cf_rf)

# Calculate other classification metrics
accuracy <- cf_rf$overall['Accuracy']
precision <- cf_rf$byClass['Pos Pred Value']
recall <- cf_rf$byClass['Sensitivity']
f1_score <- cf_rf$byClass['F1']
roc_auc <- roc(actual_values, rf_predictions2)

cat("Accuracy:", accuracy, "\n")
cat("Precision:", precision, "\n")
cat("Recall:", recall, "\n")
cat("F1-Score:", f1_score, "\n")
cat("ROC-AUC:", auc(roc_auc), "\n")



##Creating Support Vector Machine
library(e1071)

svm_model <- svm(formula, data = data_train, kernel = "radial")
print(svm_model)

# Calculate permutation feature importance
svm_imp <- varImp(svm_model)
print(importance)

svm_predictions <- predict(rf_model, newdata = data_test, type = "response")
svm_predictions2 <- ifelse(rf_predictions == "Yes", 1, 0)
# Create a confusion matrix 
cf_svm <- confusionMatrix(svm_predictions, actual_values)
print(cf_svm)

# Calculate other classification metrics
accuracy <- cf_rf$overall['Accuracy']
precision <- cf_rf$byClass['Pos Pred Value']
recall <- cf_rf$byClass['Sensitivity']
f1_score <- cf_rf$byClass['F1']
roc_auc <- roc(actual_values, svm_predictions2)

cat("Accuracy:", accuracy, "\n")
cat("Precision:", precision, "\n")
cat("Recall:", recall, "\n")
cat("F1-Score:", f1_score, "\n")
cat("ROC-AUC:", auc(roc_auc), "\n")

##Creating naiveBayes
library(e1071)

nb_model <- naiveBayes(formula, data = data_train)
print(nb_model)

# predictions using naiveBayes
nb_predictions <- predict(nb_model, newdata = data_test)
nb_predictions2 <- ifelse(rf_predictions == "Yes", 1, 0)
# Create a confusion matrix 
cf_nb <- confusionMatrix(nb_predictions, actual_values)
print(cf_nb)

# Calculate other classification metrics
accuracy <- cf_rf$overall['Accuracy']
precision <- cf_rf$byClass['Pos Pred Value']
recall <- cf_rf$byClass['Sensitivity']
f1_score <- cf_rf$byClass['F1']
roc_auc <- roc(actual_values, svm_predictions2)

cat("Accuracy:", accuracy, "\n")
cat("Precision:", precision, "\n")
cat("Recall:", recall, "\n")
cat("F1-Score:", f1_score, "\n")
cat("ROC-AUC:", auc(roc_auc), "\n")




##Creating K-NN model 
set.seed(256)

table(dt$accept)

dt$accept2 <- ifelse(dt$accept=="No",0,1)
dt$accept2 <- factor(dt$accept2, levels =c(0,1))
trainIndex <- createDataPartition(dt$accept2, 
                                  times=1, 
                                  p = .7, 
                                  list = FALSE)
knn_traindt <- dt[trainIndex,]
knn_testdt <-dt[-trainIndex,]

#K-NN requires all data to be standardized for building a.k.a preprocessing
preProcValues <- preProcess(knn_traindt, method = c("center", "scale"))
trainTransformed <- predict(preProcValues, knn_traindt)
testTransformed <- predict(preProcValues, knn_testdt)



##Determining what is the ideal k

knn_model <- train(
  formula, 
  data = trainTransformed, 
  method = "knn", 
  trControl = trainControl(method = "cv"), 
  tuneGrid = data.frame(k = c(3,5,7))
)

# Access the tuning results
tuning_results <- knn_model$results

# Find the best k-value based on the lowest RMSE (or other metric)
best_k <- tuning_results[which.min(tuning_results$Kappa), "k"]

cat("Optimal k-value:", best_k, "\n")

#creating the best model
best_knn_model<- knn3(
  formula,
  data = trainTransformed,
  k = knn_model$bestTune$k
)

#Making knn predictions

knn_predictions <- predict(best_knn_model, testTransformed,type = "class")
knn_predictions2 <- ifelse(knn_predictions == "Yes", 1, 0)

# Calculate confusion matrix
cf_knn <- confusionMatrix(knn_predictions, as.factor(testTransformed$accept))
cf_knn

# Calculate other classification metrics
accuracy <- cf_knn$overall['Accuracy']
precision <- cf_knn$byClass['Pos Pred Value']
recall <- cf_knn$byClass['Sensitivity']
f1_score <- cf_knn$byClass['F1']
roc_auc <- roc(actual_values, knn_predictions2)

cat("Accuracy:", accuracy, "\n")
cat("Precision:", precision, "\n")
cat("Recall:", recall, "\n")
cat("F1-Score:", f1_score, "\n")
cat("ROC-AUC:", auc(roc_auc), "\n")





train_split_factor <- 0.70
set.seed(299) #for reproducibility
train_cases <- sample(1:nrow(dt), round(nrow(dt)*train_split_factor), replace = FALSE)
train_cases <- train_cases[order(train_cases)]
test_cases <- setdiff(1:nrow(dt), train_cases)
data_train <- dt[train_cases, ]
data_test <- dt[test_cases, ]


##Creating neural network
matrix_mlp <- model.matrix(~accept + age + sex + marital2 + residence + education_years + 
                             occup2 + income_cat + family_type + routine_health3 + hpv_heard + 
                             vac_hear + cancer_hear + canvac_hear + vul_cat + sev_cat + 
                             ben_cat + bar_cat + cue, data=data_train)

head(matrix_mlp)

matrix_mlp_test <-model.matrix(~accept + age + sex + marital2 + residence + education_years + 
                                 occup2 + income_cat + family_type + routine_health3 + hpv_heard + 
                                 vac_hear + cancer_hear + canvac_hear + vul_cat + sev_cat + 
                                 ben_cat + bar_cat + cue, data=data_test)



##Categories within factors need to be renamed for proper model building

mlp_model <-acceptYes~age+sexMale+marital2NoSpouse+marital2Other+residenceSemiUrban+
          residenceUrban+education_years+occup2Business+occup2Housewife+occup2Others+
          income_catTwenty_ThirtyFive+income_catThirtyFive_Fifty+income_catMoreThenFifty+
          family_typeJoint+routine_health3Irregular+routine_health3Never+
          hpv_heardYes+vac_hearYes+cancer_hearYes+canvac_hearYes+vul_catPositive+sev_catPositive+
          ben_catPositive+bar_catPositive+cuePositive 




#calling the library
library(neuralnet)

#Building the model

mlp_model <- neuralnet(
        mlp_model,
        data = matrix_mlp,
        hidden = 3, # Define the number of neurons in hidden layers (you can adjust this)
        linear.output = TRUE, # For binary classification, set to TRUE
        lifesign = "full", # Print progress during training
        stepmax = 1e+6 # Maximum number of iterations (you can adjust this)
        )

summary(mlp_model)

#Making predictions
mlp_predictions <- predict(mlp_model, matrix_mlp_test)
mlp_predictions2 <- ifelse(mlp_predictions>0.500, 1, 0)
mlp_predictions2 <- as.factor(mlp_predictions2)



mlp_accept <- matrix_mlp_test[, 2]
mlp_accept <- as.factor(mlp_accept)


# Calculate confusion matrix
cf_mlp <- confusionMatrix(mlp_predictions2, mlp_accept)
cf_mlp



