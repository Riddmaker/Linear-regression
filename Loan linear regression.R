### Load required libraries ###
library(tidyr)
library(corrplot)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(leaps)
library(boot)


### clean environment ###
rm(list = ls())

### Set working directory
setwd("C:/Git/Loan regression")

### Load data ###
rtl <- read.csv("regression_train_loan.csv")

#Copy the original data
rtl_copy <- rtl

# No NA's in int_rate 
count(filter(rtl_copy, is.na(int_rate)))

View(rtl_copy)

#after checking the Data it was observable that some columns use a string "n/a" or "" as NA.
#These values are changed to a real NA

###remove n/a as strings in column emp_length
rtl_copy$emp_length[rtl_copy$emp_length=="n/a"]<-NA

###remove "n/a"" as strings in column verification_status_joint
rtl_copy[rtl_copy==""]<-NA


# Control of N/A's in Data set and remove variables with more then 70% N/A's

missing_data <- rtl_copy %>% summarise_all(funs(sum(is.na(.))/n()))
missing_data <- gather(missing_data, key = "variables", value = "percent_missing")
ggplot(missing_data, aes(x = reorder(variables, percent_missing), y = percent_missing)) +
  geom_bar(stat = "identity", fill = "blue", aes(color = I('white')), size = 0.1)+coord_flip()+ theme_few()

###remove all rows with more than 50% NA and columns with more than 70% NA
rtl_copy<-rtl_copy[which(rowMeans(!is.na(rtl_copy)) > 0.5), which(colMeans(!is.na(rtl_copy)) > 0.3)]

missing_data <- rtl_copy %>% summarise_all(funs(sum(is.na(.))/n()))
missing_data <- gather(missing_data, key = "variables", value = "percent_missing") 
ggplot(missing_data, aes(x = reorder(variables, percent_missing), y = percent_missing)) +
  geom_bar(stat = "identity", fill = "blue", aes(color = I('white')), size = 0.1)+coord_flip()+ theme_few()

### still too much NA's but it makes sense to change them to 0 because the observations with NA have all 0 delinq_2yrs
rtl_copy$mths_since_last_delinq[is.na(rtl_copy$mths_since_last_delinq)]<-0

### Check all observations with next_pymnt_d as N/A
Check_next_pymnt_d <- filter(rtl_copy, is.na(next_pymnt_d))
as.table(table(Check_next_pymnt_d$loan_status))
### due to the fact the all N/A in next_pymnt_d are fully paid or charged off no next_pymnt_d is coming and therefor we can change it to 0
rtl_copy$next_pymnt_d[is.na(rtl_copy$next_pymnt_d)]<-0

### for now we go on with variables which have less then 10% N/A's
missing_data <- rtl_copy %>% summarise_all(funs(sum(is.na(.))/n()))
missing_data <- gather(missing_data, key = "variables", value = "percent_missing") 
ggplot(missing_data, aes(x = reorder(variables, percent_missing), y = percent_missing)) +
  geom_bar(stat = "identity", fill = "blue", aes(color = I('white')), size = 0.1)+coord_flip()+ theme_few()


### Kick out data for administrative or semantic reasons ###
rtl_copy <- select(rtl_copy, -c(emp_title, member_id, next_pymnt_d, grade, title, url, zip_code, dti, X,id,last_pymnt_d,last_pymnt_amnt))
# Emp_title -> Could be useful but not feasible, use only if model is not sufficient without. 
# Member_id -> Just an ID Number with no meaning. Might confuse the model if left in the analysis 
# Grade -> can be deleted if data quality of sub_grade is sufficient (more or less NA's) 
# Title -> Might be helpful but not feasible. 
# Url -> Remove no meaning 
# Zip_code -> discriminatory
# Dti -> Ratio from other variables
# X -> Just another ID
# Delete id -> No value will just confuse lm
# last_pymnt_d administrative
# last_pymnt_amnt administrative
# next_pymnt_d administrative


### Check and remove for unbalanced Data
as.table(table(rtl_copy$application_type))
as.table(table(rtl_copy$policy_code))
as.table(table(rtl_copy$pymnt_plan))
rtl_copy <- select(rtl_copy, -c(application_type,policy_code,pymnt_plan))

str(rtl_copy)

### change chr to factors, therefore we checked again if the data's are unbalanced. If so we bin them.

### "IA","ID","ND","ME" have very few observations, so we combine them to an "other"
unique(rtl_copy$addr_state)
as.table(table(rtl_copy$addr_state))
rtl_copy$addr_state <- ifelse(rtl_copy$addr_state == "IA","OTHER", rtl_copy$addr_state)
rtl_copy$addr_state <- ifelse(rtl_copy$addr_state == "ID","OTHER", rtl_copy$addr_state)
rtl_copy$addr_state <- ifelse(rtl_copy$addr_state == "ND","OTHER", rtl_copy$addr_state)
rtl_copy$addr_state <- ifelse(rtl_copy$addr_state == "ME","OTHER", rtl_copy$addr_state)
rtl_copy$addr_state <- as.factor(rtl_copy$addr_state)
unique(rtl_copy$addr_state)


unique(rtl_copy$term)
as.table(table(rtl_copy$term))
rtl_copy$term <- as.factor(rtl_copy$term)
unique(rtl_copy$term)

unique(rtl_copy$sub_grade)
as.table(table(rtl_copy$sub_grade))
rtl_copy$sub_grade <- as.factor(rtl_copy$sub_grade)
rtl_copy$sub_grade <- as.ordered(rtl_copy$sub_grade)
unique(rtl_copy$sub_grade)

unique(rtl_copy$emp_length)
rtl_copy$emp_length <- as.factor(rtl_copy$emp_length)
unique(rtl_copy$emp_length)

unique(rtl_copy$home_ownership)
as.table(table(rtl_copy$home_ownership))
rtl_copy$home_ownership <- ifelse(rtl_copy$home_ownership == "NONE","OTHER", rtl_copy$home_ownership)
rtl_copy$home_ownership <- ifelse(rtl_copy$home_ownership == "ANY","OTHER", rtl_copy$home_ownership)
rtl_copy$home_ownership <- as.factor(rtl_copy$home_ownership)
unique(rtl_copy$home_ownership)

unique(rtl_copy$verification_status)
as.table(table(rtl_copy$verification_status))
rtl_copy$verification_status <- as.factor(rtl_copy$verification_status)
unique(rtl_copy$verification_status)


unique(rtl_copy$issue_d)
rtl_copy$issue_dy<-substr(rtl_copy$issue_d,5,8)
rtl_copy$issue_dy<-as.factor(rtl_copy$issue_dy)
unique(rtl_copy$issue_dy)
plot(rtl_copy$issue_dy, rtl_copy$int_rate)


rtl_copy$issue_dm<-substr(rtl_copy$issue_d,0,3)
rtl_copy$issue_dm<-as.factor(rtl_copy$issue_dm)
unique(rtl_copy$issue_dm)
plot(rtl_copy$issue_dm, rtl_copy$int_rate)

### removing the original column issue_d because we separte it into two new columns (issue_dy,issue_dm)
rtl_copy <- select(rtl_copy, -c(issue_d))


unique(rtl_copy$last_credit_pull_d)
rtl_copy$last_credit_pull_d<-substr(rtl_copy$last_credit_pull_d,5,8)
rtl_copy$last_credit_pull_d<-as.factor(rtl_copy$last_credit_pull_d)
unique(rtl_copy$last_credit_pull_d)
plot(rtl_copy$last_credit_pull_d, rtl_copy$int_rate)

### "educational","renewable_energy" very few observations, so we combine them to "other"
unique(rtl_copy$purpose)
as.table(table(rtl_copy$purpose))
rtl_copy$purpose<-ifelse(rtl_copy$purpose=="educational","other",rtl_copy$purpose)
rtl_copy$purpose<-ifelse(rtl_copy$purpose=="renewable_energy","other",rtl_copy$purpose)
rtl_copy$purpose <- as.factor(rtl_copy$purpose)
unique(rtl_copy$purpose)
plot(rtl_copy$purpose, rtl_copy$int_rate)

#reduce the factors by binning into decades
rtl_copy$earliest_cr_line<-rtl$earliest_cr_line
unique(rtl_copy$earliest_cr_line)
rtl_copy$earliest_cr_line<-(as.integer(substr(rtl_copy$earliest_cr_line,5,8)))
as.table(table(rtl_copy$earliest_cr_line))
rtl_copy$earliest_cr_line<-ifelse(rtl_copy$earliest_cr_line<1970,"<1970",rtl_copy$earliest_cr_line)
rtl_copy$earliest_cr_line<-ifelse(rtl_copy$earliest_cr_line %in% c(1970,1971,1972,1973,1974,1975,1976,1977,1978,1979),"1970-1979",rtl_copy$earliest_cr_line)
rtl_copy$earliest_cr_line<-ifelse(rtl_copy$earliest_cr_line %in% c(1980,1981,1982,1983,1984,1985,1986,1987,1988,1989),"1980-1989",rtl_copy$earliest_cr_line)
rtl_copy$earliest_cr_line<-ifelse(rtl_copy$earliest_cr_line %in% c(1990,1991,1992,1993,1994,1995,1996,1997,1998,1999),"1990-1999",rtl_copy$earliest_cr_line)
rtl_copy$earliest_cr_line<-ifelse(rtl_copy$earliest_cr_line %in% c(2000,2001,2002,2003,2004,2005,2006,2007,2008,2009),"2000-2009",rtl_copy$earliest_cr_line)
rtl_copy$earliest_cr_line<-ifelse(rtl_copy$earliest_cr_line>=2010,">2010",rtl_copy$earliest_cr_line)
rtl_copy$earliest_cr_line<-(as.factor(rtl_copy$earliest_cr_line))
plot(rtl_copy$earliest_cr_line, rtl_copy$int_rate)
unique(rtl_copy$earliest_cr_line)

unique(rtl_copy$initial_list_status)
as.table(table(rtl_copy$initial_list_status))
rtl_copy$initial_list_status <- as.factor(rtl_copy$initial_list_status)
unique(rtl_copy$initial_list_status)
plot(rtl_copy$initial_list_status, rtl_copy$int_rate)


unique(rtl_copy$loan_status)
as.table(table(rtl_copy$loan_status))
rtl_copy$loan_status <- as.factor(rtl_copy$loan_status)
unique(rtl_copy$loan_status)
plot(rtl_copy$loan_status, rtl_copy$int_rate)

### check variable --> no chr left 
str(rtl_copy)


### correlation analysis --> reducing the size of the data set to plot the correlation
set.seed(1)
indices = sort(sample(nrow(rtl_copy), nrow(rtl_copy)*.01))
smallrtl_copy <- rtl_copy[indices,]
isNumericSmall <- select_if(smallrtl_copy, is.numeric)

###use na.omit to plot (--> no NA necessary for it) and just numerical variable possible
corrplot(cor(na.omit(isNumericSmall)), type="upper", order="hclust", tl.col="black", tl.srt=45) 


cor(smallrtl_copy$loan_amnt, y = smallrtl_copy$funded_amnt, use = "everything",
    method = c("pearson"))
pairs(smallrtl_copy [, c("loan_amnt", "funded_amnt", "funded_amnt_inv")])
# collinearity detected: we can get rid of funded_amnt and funded_amnt_inv

cor(smallrtl_copy$total_pymnt, y = smallrtl_copy$total_pymnt_inv, use = "everything",
    method = c("pearson"))
pairs(smallrtl_copy [, c("total_pymnt", "total_pymnt_inv")])
# collinearity detected: we can get rid of total_pymnt_inv 

cor(smallrtl_copy$out_prncp, y = smallrtl_copy$out_prncp_inv, use = "everything",
    method = c("pearson"))
pairs(smallrtl_copy [, c("out_prncp", "out_prncp_inv")])
# collinearity detected: we can get rid of total_pymnt_inv 

rtl_copy <- select(rtl_copy, -c(funded_amnt,funded_amnt_inv,total_pymnt_inv,out_prncp_inv))

### Check Correlation again
set.seed(1)
indices = sort(sample(nrow(rtl_copy), nrow(rtl_copy)*.01))
smallrtl_copy <- rtl_copy[indices,]
isNumericSmall <- select_if(smallrtl_copy, is.numeric)
corrplot(cor(na.omit(isNumericSmall)), type="upper", order="hclust", tl.col="black", tl.srt=45)

cor(smallrtl_copy$loan_amnt, y = smallrtl_copy$installment , use = "everything",
    method = c("pearson"))
pairs(smallrtl_copy [, c("loan_amnt", "installment")])
# Correlation existing but not collinearity --> keep in


# to check those with na's we removed all observations with na's in the smallrtl_copy
smallrtl_copy<-na.omit(smallrtl_copy)
cor(smallrtl_copy$revol_bal, y = smallrtl_copy$total_rev_hi_lim , use = "everything",
    method = c("pearson"))
pairs(smallrtl_copy [, c("revol_bal", "total_rev_hi_lim")])
# Correlation existing but not collinearity --> keep in


cor(smallrtl_copy$recoveries, y = smallrtl_copy$collection_recovery_fee , use = "everything",
    method = c("pearson"))
pairs(smallrtl_copy [, c("recoveries", "collection_recovery_fee")])
# Correlation existing but not collinearity --> keep in

cor(smallrtl_copy$total_pymnt, y = smallrtl_copy$total_rec_prncp , use = "everything",
    method = c("pearson"))
pairs(smallrtl_copy [, c("total_pymnt", "total_rec_prncp")])
# Correlation existing but not collinearity --> keep in



### before going on, we decided to remove the outliers of installment, annual_inc, total_rev_hi_lim
###Outliers Removal -> Model got worse with outlier removal -> So we keep the outliers in.
##boxplot(rtl_copy$loan_amnt) #-->ok


#boxplot(rtl_copy$installment)# --> outlier vorhanden -->Removed
#outliers<-boxplot(rtl_copy$installment, plot=FALSE)$out
#rtl_copy <- rtl_copy[-which(rtl_copy$installment %in% outliers),]
#boxplot(rtl_copy$installment)


#boxplot(rtl_copy$annual_inc) # --> outlier vorhanden --> Removed
#outliers<-boxplot(rtl_copy$annual_inc, plot=FALSE)$out
#rtl_copy <- rtl_copy[-which(rtl_copy$annual_inc %in% outliers),]
#boxplot(rtl_copy$annual_inc)

#boxplot(rtl_copy$total_rev_hi_lim) # --> outlier vorhanden --> Removed
#outliers<-boxplot(rtl_copy$total_rev_hi_lim , plot=FALSE)$out
#rtl_copy <- rtl_copy[-which(rtl_copy$total_rev_hi_lim  %in% outliers),]
#boxplot(rtl_copy$total_rev_hi_lim)


#---Preprocessing done so far
#Set seed, generate a sequence of random number, ensure get the same result if start with same seed each time when running the same process 
set.seed(1)


#Resampling, create training set and test set
indices = sort(sample(nrow(rtl_copy), nrow(rtl_copy)*.7))
trainingset <- rtl_copy[indices,]
testset <- rtl_copy[-indices,]

### trainset necessary for backward / forward selection
indices01 = sort(sample(nrow(trainingset), nrow(trainingset)*.25))
trainset <- trainingset[indices01,]


### From here we remove all NA's from the training set
Training <-trainingset
Training <-na.omit(Training)
Test  <-testset
Test <-na.omit(Test)
Train  <-trainset 
Train <-na.omit(Train)

### first model with all existing variable
raw_model <- lm(int_rate~.,data = Training)
options(max.print = 5000)
summary(raw_model)
RSE_raw_model<-summary(raw_model)$r.squared
RSE_raw_model
Adj_RSE_raw_model<-summary(raw_model)$adj.r.squared
Adj_RSE_raw_model
MSE_raw_model<-mean(raw_model$residuals^2)
MSE_raw_model
aic_raw_model <-AIC(raw_model)
aic_raw_model
bic_raw_model <-BIC(raw_model)
bic_raw_model


#testing the raw model with test data checking for overfitting.
predictedInt_rate <- predict.lm(raw_model,Test)
#Check Residual sum of squares, (y-predictedy)^2, 0 the best.
RSS <-sum((predictedInt_rate-Test$int_rate)^2)
#check total sum of squares, see how big the variance.
TSS <-sum((Test$int_rate^2-mean(Test$int_rate))^2)
#calculate the R-Squared, close to 1.
Test_RSQ_raw_model <- 1-RSS/TSS
Test_RSQ_raw_model
Test_MSE_raw_model <- mean((predictedInt_rate-Test$int_rate)^2)
Test_MSE_raw_model

#no overfitting 

###removing all variables without significance in the first raw model
adjusted_rtl_copy <- select(rtl_copy, -c(open_acc,earliest_cr_line,total_pymnt,total_rec_prncp,total_rec_int,total_rec_late_fee,recoveries,tot_coll_amt,delinq_2yrs))

#Resampling, create training set and test set
set.seed(1)
indices = sort(sample(nrow(adjusted_rtl_copy), nrow(adjusted_rtl_copy)*.7))
trainingset <- adjusted_rtl_copy[indices,]
testset <- adjusted_rtl_copy[-indices,]

### trainset necessary for backward / forward selection
indices01 = sort(sample(nrow(trainingset), nrow(trainingset)*.25))
trainset <- trainingset[indices01,]


### From here we remove all NA's from the training set
Training <-trainingset
Training <-na.omit(Training)
Test  <-testset
Test <-na.omit(Test)
Train  <-trainset 
Train <-na.omit(Train)

### first model with all existing variable
adjusted_raw_model <- lm(int_rate~.,data = Training)
options(max.print = 5000)
summary(adjusted_raw_model)
RSE_adjusted_raw_model<-summary(adjusted_raw_model)$r.squared
RSE_adjusted_raw_model
Adj_adjusted_raw_model<-summary(adjusted_raw_model)$adj.r.squared
Adj_adjusted_raw_model
MSE_adjusted_raw_model<-mean(adjusted_raw_model$residuals^2)
MSE_adjusted_raw_model
aic_adjusted_raw_model <-AIC(adjusted_raw_model)
aic_adjusted_raw_model
bic_adjusted_raw_model <-BIC(adjusted_raw_model)
bic_adjusted_raw_model

#testing the raw model with test data checking for overfitting
predictedInt_rate <- predict.lm(adjusted_raw_model,Test)
#Check Residual sum of squares, (y-predictedy)^2, 0 the best 
RSS <-sum((predictedInt_rate-Test$int_rate)^2)
#check total sum of squares, see how big the variance
TSS <-sum((Test$int_rate^2-mean(Test$int_rate))^2)
#calculate the R-Squared, close to 1
Test_RSQ_adjusted_raw_model <- 1-RSS/TSS
Test_RSQ_adjusted_raw_model
Test_MSE_adjusted_raw_model <- mean((predictedInt_rate-Test$int_rate)^2)
Test_MSE_adjusted_raw_model



### due to the fact the the MSE of the model is just declined by 0.0003 we decided to go on with the adjusted_rtl_copy data set

model_data <- adjusted_rtl_copy

set.seed(1)

indices = sort(sample(nrow(model_data), nrow(model_data)*.7))
trainingset <- model_data[indices,]
testset <- model_data[-indices,]

### trainset necessary for backward / forward selection
indices01 = sort(sample(nrow(trainingset), nrow(trainingset)*.25))
trainset <- trainingset[indices01,]


### From here we remove all NA's from the training set
Training <-trainingset
Training <-na.omit(Training)
Test  <-testset
Test <-na.omit(Test)
Train  <-trainset 
Train <-na.omit(Train)

#####SUBSET SELECTION, STEPWISE FORWARD & BACKWARD SLECTION####

TrainingTibble <- as_tibble(Training)

#stepwise backward selection
Model_backward <-regsubsets(int_rate~., data= TrainingTibble, nvmax =188, method = "backward")
summary(Model_backward)
Model_backward_Summary <- summary(Model_backward)
#View all together and looks for the best subsets
data.frame("rsq" = Model_backward_Summary$rsq, "adjrs"=Model_backward_Summary$adjr2, "cp" = Model_backward_Summary$cp, "bic"= Model_backward_Summary$bic)

#and looks for the HIGHEST RSQ, LOWEST RSS, HIGHEST adjusted R^2, LOWEST cp, LOWEST bic
#store the highest value
rsq.max_backward <- which.max(Model_backward_Summary$rsq)
rss.min_backward <- which.min(Model_backward_Summary$rss)
adjR2.max_backward <- which.max(Model_backward_Summary$adjr2)
cp_min_backward <- which.min(Model_backward_Summary$cp)
bic_min_backward <- which.min(Model_backward_Summary$bic)


backward_variables <- summary(Model_backward)$which

write.csv2(backward_variables, file = "backward_variable.csv")

### the tree models with the highest adjr2, lowest BIC and lowest CP are compared
glm101 <-glm(int_rate~sub_grade, data = Train)
glm102 <-glm(int_rate~sub_grade+issue_dy, data = Train)
glm103 <-glm(int_rate~sub_grade+issue_dy+issue_dm, data = Train)
glm104 <-glm(int_rate~sub_grade+issue_dy+issue_dm+loan_amnt, data = Train)
glm105 <-glm(int_rate~sub_grade+issue_dy+issue_dm+loan_amnt+installment, data = Train)
glm106 <-glm(int_rate~sub_grade+issue_dy+issue_dm+loan_amnt+installment+term, data = Train)
glm107 <-glm(int_rate~sub_grade+issue_dy+issue_dm+loan_amnt+installment+term+loan_status, data = Train)
glm108 <-glm(int_rate~sub_grade+issue_dy+issue_dm+loan_amnt+installment+term+loan_status+out_prncp, data = Train)
glm109 <-glm(int_rate~sub_grade+issue_dy+issue_dm+loan_amnt+installment+term+loan_status+out_prncp+purpose, data = Train)
glm110 <-glm(int_rate~sub_grade+issue_dy+issue_dm+loan_amnt+installment+term+loan_status+out_prncp+purpose+verification_status, data = Train)
glm111 <-glm(int_rate~sub_grade+issue_dy+issue_dm+loan_amnt+installment+term+loan_status+out_prncp+purpose+verification_status+last_credit_pull_d, data = Train)
glm112 <-glm(int_rate~sub_grade+issue_dy+issue_dm+loan_amnt+installment+term+loan_status+out_prncp+purpose+verification_status+last_credit_pull_d+addr_state, data = Train)
glm113 <-glm(int_rate~sub_grade+issue_dy+issue_dm+loan_amnt+installment+term+loan_status+out_prncp+purpose+verification_status+last_credit_pull_d+addr_state+inq_last_6mths, data = Train)
glm114 <-glm(int_rate~sub_grade+issue_dy+issue_dm+loan_amnt+installment+term+loan_status+out_prncp+purpose+verification_status+last_credit_pull_d+addr_state+inq_last_6mths+revol_util, data = Train)
glm115 <-glm(int_rate~sub_grade+issue_dy+issue_dm+loan_amnt+installment+term+loan_status+out_prncp+purpose+verification_status+last_credit_pull_d+addr_state+inq_last_6mths+revol_util+initial_list_status, data = Train)
glm116 <-glm(int_rate~sub_grade+issue_dy+issue_dm+loan_amnt+installment+term+loan_status+out_prncp+purpose+verification_status+last_credit_pull_d+addr_state+inq_last_6mths+revol_util+initial_list_status+tot_cur_bal, data = Train)
glm117 <-glm(int_rate~sub_grade+issue_dy+issue_dm+loan_amnt+installment+term+loan_status+out_prncp+purpose+verification_status+last_credit_pull_d+addr_state+inq_last_6mths+revol_util+initial_list_status+tot_cur_bal+home_ownership, data = Train)
glm118 <-glm(int_rate~sub_grade+issue_dy+issue_dm+loan_amnt+installment+term+loan_status+out_prncp+purpose+verification_status+last_credit_pull_d+addr_state+inq_last_6mths+revol_util+initial_list_status+tot_cur_bal+home_ownership+mths_since_last_delinq, data = Train)
glm119 <-glm(int_rate~sub_grade+issue_dy+issue_dm+loan_amnt+installment+term+loan_status+out_prncp+purpose+verification_status+last_credit_pull_d+addr_state+inq_last_6mths+revol_util+initial_list_status+tot_cur_bal+home_ownership+mths_since_last_delinq+annual_inc, data = Train)
glm120 <-glm(int_rate~sub_grade+issue_dy+issue_dm+loan_amnt+installment+term+loan_status+out_prncp+purpose+verification_status+last_credit_pull_d+addr_state+inq_last_6mths+revol_util+initial_list_status+tot_cur_bal+home_ownership+mths_since_last_delinq+annual_inc+revol_bal, data = Train)
glm121 <-glm(int_rate~sub_grade+issue_dy+issue_dm+loan_amnt+installment+term+loan_status+out_prncp+purpose+verification_status+last_credit_pull_d+addr_state+inq_last_6mths+revol_util+initial_list_status+tot_cur_bal+home_ownership+mths_since_last_delinq+annual_inc+revol_bal+total_rev_hi_lim, data = Train)
glm122 <-glm(int_rate~sub_grade+issue_dy+issue_dm+loan_amnt+installment+term+loan_status+out_prncp+purpose+verification_status+last_credit_pull_d+addr_state+inq_last_6mths+revol_util+initial_list_status+tot_cur_bal+home_ownership+mths_since_last_delinq+annual_inc+revol_bal+total_rev_hi_lim+emp_length, data = Train)
glm123 <-glm(int_rate~sub_grade+issue_dy+issue_dm+loan_amnt+installment+term+loan_status+out_prncp+purpose+verification_status+last_credit_pull_d+addr_state+inq_last_6mths+revol_util+initial_list_status+tot_cur_bal+home_ownership+mths_since_last_delinq+annual_inc+revol_bal+total_rev_hi_lim+emp_length+acc_now_delinq, data = Train)
glm124 <-glm(int_rate~sub_grade+issue_dy+issue_dm+loan_amnt+installment+term+loan_status+out_prncp+purpose+verification_status+last_credit_pull_d+addr_state+inq_last_6mths+revol_util+initial_list_status+tot_cur_bal+home_ownership+mths_since_last_delinq+annual_inc+revol_bal+total_rev_hi_lim+emp_length+acc_now_delinq+total_acc, data = Train)
glm125 <-glm(int_rate~sub_grade+issue_dy+issue_dm+loan_amnt+installment+term+loan_status+out_prncp+purpose+verification_status+last_credit_pull_d+addr_state+inq_last_6mths+revol_util+initial_list_status+tot_cur_bal+home_ownership+mths_since_last_delinq+annual_inc+revol_bal+total_rev_hi_lim+emp_length+acc_now_delinq+total_acc+pub_rec, data = Train)
glm126 <-glm(int_rate~sub_grade+issue_dy+issue_dm+loan_amnt+installment+term+loan_status+out_prncp+purpose+verification_status+last_credit_pull_d+addr_state+inq_last_6mths+revol_util+initial_list_status+tot_cur_bal+home_ownership+mths_since_last_delinq+annual_inc+revol_bal+total_rev_hi_lim+emp_length+acc_now_delinq+total_acc+pub_rec+collection_recovery_fee, data = Train)
glm127 <-glm(int_rate~sub_grade+issue_dy+issue_dm+loan_amnt+installment+term+loan_status+out_prncp+purpose+verification_status+last_credit_pull_d+addr_state+inq_last_6mths+revol_util+initial_list_status+tot_cur_bal+home_ownership+mths_since_last_delinq+annual_inc+revol_bal+total_rev_hi_lim+emp_length+acc_now_delinq+total_acc+pub_rec+collection_recovery_fee+collections_12_mths_ex_med, data = Train)

#10-fold CV
cv.err.glm101 <- cv.glm(Train, glm101, K = 10)
cv.err.glm102 <- cv.glm(Train, glm102, K = 10)
cv.err.glm103 <- cv.glm(Train, glm103, K = 10)
cv.err.glm104 <- cv.glm(Train, glm104, K = 10)
cv.err.glm105 <- cv.glm(Train, glm105, K = 10)
cv.err.glm106 <- cv.glm(Train, glm106, K = 10)
cv.err.glm107 <- cv.glm(Train, glm107, K = 10)
cv.err.glm108 <- cv.glm(Train, glm108, K = 10)
cv.err.glm109 <- cv.glm(Train, glm109, K = 10)
cv.err.glm110 <- cv.glm(Train, glm110, K = 10)
cv.err.glm111 <- cv.glm(Train, glm111, K = 10)
cv.err.glm112 <- cv.glm(Train, glm112, K = 10)
cv.err.glm113 <- cv.glm(Train, glm113, K = 10)
cv.err.glm114 <- cv.glm(Train, glm114, K = 10)
cv.err.glm115 <- cv.glm(Train, glm115, K = 10)
cv.err.glm116 <- cv.glm(Train, glm116, K = 10)
cv.err.glm117 <- cv.glm(Train, glm117, K = 10)
cv.err.glm118 <- cv.glm(Train, glm118, K = 10)
cv.err.glm119 <- cv.glm(Train, glm119, K = 10)
cv.err.glm120 <- cv.glm(Train, glm120, K = 10)
cv.err.glm121 <- cv.glm(Train, glm121, K = 10)
cv.err.glm122 <- cv.glm(Train, glm122, K = 10)
cv.err.glm123 <- cv.glm(Train, glm123, K = 10)
cv.err.glm124 <- cv.glm(Train, glm124, K = 10)
cv.err.glm125 <- cv.glm(Train, glm125, K = 10)
cv.err.glm126 <- cv.glm(Train, glm126, K = 10)
cv.err.glm127 <- cv.glm(Train, glm127, K = 10)

#plot the cv error in table as of the number of predictors
x<- 1:27
(cv.err <- c(cv.err.glm101$delta[1], cv.err.glm102$delta[1], cv.err.glm103$delta[1], cv.err.glm104$delta[1], cv.err.glm105$delta[1], cv.err.glm106$delta[1], cv.err.glm107$delta[1], cv.err.glm108$delta[1],
             cv.err.glm109$delta[1], cv.err.glm110$delta[1], cv.err.glm111$delta[1], cv.err.glm112$delta[1], cv.err.glm113$delta[1], cv.err.glm114$delta[1], cv.err.glm115$delta[1], cv.err.glm116$delta[1], cv.err.glm117$delta[1],
             cv.err.glm118$delta[1], cv.err.glm119$delta[1], cv.err.glm120$delta[1], cv.err.glm121$delta[1], cv.err.glm122$delta[1], cv.err.glm123$delta[1], cv.err.glm124$delta[1], cv.err.glm125$delta[1], cv.err.glm126$delta[1],cv.err.glm127$delta[1]))

plot(cv.err ~x)
lines(cv.err ~x)

# Find the minimum of CV Error:
(cv.min <- which.min(cv.err))
cv.err[cv.min]



final_model<-lm(int_rate~sub_grade+issue_dy+issue_dm+loan_amnt+installment+term+loan_status+out_prncp+purpose+verification_status+last_credit_pull_d+addr_state+inq_last_6mths+revol_util+initial_list_status+tot_cur_bal+home_ownership+mths_since_last_delinq+annual_inc+revol_bal+total_rev_hi_lim+emp_length+acc_now_delinq,data = Training)
options(max.print = 5000)
summary(final_model)
RSE_final_model<-summary(final_model)$r.squared
RSE_final_model
Adj_final_raw_model<-summary(final_model)$adj.r.squared
Adj_final_raw_model
MSE_final_model<-mean(final_model$residuals^2)
MSE_final_model
aic_final_model <-AIC(final_model)
aic_final_model
bic_final_model <-BIC(final_model)
bic_final_model

predictedInt_rate <- predict.lm(final_model,Test)
#Check Residual sum of squares, (y-predictedy)^2, 0 the best 
RSS <-sum((predictedInt_rate-Test$int_rate)^2)
#check total sum of squares, see how big the variance
TSS <-sum((Test$int_rate^2-mean(Test$int_rate))^2)
#calculate the R-Squared, close to 1
Test_RSQ_final_model_model <- 1-RSS/TSS
Test_RSQ_final_model_model
Test_MSE_final_model_model <- mean((predictedInt_rate-Test$int_rate)^2)
Test_MSE_final_model_model
