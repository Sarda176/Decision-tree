install.packages('caret')
library(caret)
library(MASS)

bank=read.csv(file = "C:/Users/daljeet/Desktop/Decision tree -R practice/bank_deposit.csv",header=T,sep = 
                ",")
bank
attach(bank)

dim(bank)

head(bank)
tail(bank)

####datatypes###
str(bank)

#####summary q1,mean,std####
summary(bank)


####to find out null values in dataframe####
sum(is.na(bank)/prod(dim(bank)))

colSums(is.na(bank))
colSums(bank == '')
duplicated(bank)
bank[duplicated(bank)]

######Coverting factorlevel 2 into binary format####
bank$deposit=ifelse(bank$deposit=="yes",1,0)
bank

bank$default=ifelse(bank$default=="yes",1,0)
bank$default
bank

bank$housing=ifelse(bank$housing=="yes",1,0)
bank$housing
bank

bank$loan = ifelse(bank$loan=="yes",1,0)
bank 


#####Replacing unknown values with other#####
bank$poutcome <-replace(bank$poutcome, bank$poutcome=="unknown", "other")
bank

####Assinging  -1 values that is customer never contacted with 999 as per business requirement####
bank$pdays <-replace(bank$pdays, bank$pdays==-1, 999)
bank

#####Removing duration as per business requirement#####
bank <- subset( bank, select = -duration )

write.csv(bank,"C:/Users/daljeet/Desktop/Decision tree -R practice/bank02.csv")



####Converting category into factor level more than two using dummies#####
library(dummies)

bank<-dummy.data.frame(bank, names=c("job","marital","education","contact","poutcome","month"), sep="_")
bank

tail(bank)
head(bank)


####Checking outliers using box plot for contiuous variable######
boxplot(age) 
quantile(age, c(.25, .5,.55, .75, .90, .95, .99,1)) 


boxplot(balance)
quantile(balance, c(.25, .5,.55, .75, .90, .95, .99,1)) 



boxplot(bank$age,bank$balance)

dim(bank)
colSums(is.na(bank))


#######Removing outliers##########

boxplot(balance)
quantile(balance, c(.25, .5,.55, .75, .90, .95, .99,1)) 

bank01=bank[-which(bank$balance %in% boxplot.stats(bank$balance)$out), ]
bank01

boxplot(bank01$balance)
quantile(bank01$balance, c(.25, .5,.55, .75, .90, .95, .99,1))




dim(bank01)

#####Scaling the data######
##library(dplyr)
##bank01<-scaleContinuous(bank01)
##bank01<-scale(bank01$age,scale = TRUE)
##bank01

bank01[!binary]

scaleContinuous = function(data) {
  binary = apply(data, 2, function(x) {all(x %in% 0:1)}) 
  data[!binary] = scale(data[!binary])
  return(data)
}

bank01<-scaleContinuous(bank01)
bank01

write.csv(bank01,"C:/Users/daljeet/Desktop/Decision tree -R practice/bank05.csv")



write.csv(bank01,"C:/Users/daljeet/Desktop/Decision tree -R practice/bank7.csv")
tail(bank01)

dim(bank01)

#####Splitting the data into training and testing############

library(caTools)    # split 
set.seed(2)

s = sample.split(bank01$deposit,SplitRatio = 0.70)
train = bank01[s,]
test  = bank01[!s,]
nrow(train)
nrow(test)

train
test

prop.table(table(train$deposit))
prop.table(table(test$deposit))

###Building the model using decision tree####

##install.packages("rpart.plot")
library(rpart)      # decision tree
library(rpart.plot) # decision tree plotting

####Assigning the cp =0 (default cp=.01),that is we are allowing the tree to grow maximum(standard tree)

fit.dtree = rpart(deposit ~ ., method = "class", data=train,control = rpart.control(cp = 0))
fit.dtree


##prp(fit.dtree, type = 2, extra = 104, fallen.leaves = TRUE, main="Decision Tree")


####Predicting the model ########

predict_test <-predict(fit.dtree, test, type = 'class')
predict_test

###Confusion matrix#####
table_mat <- table(test$deposit, predict_test)
table_mat

sum(490+512)/3032######error


accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)
accuracy_Test

misclassificationerror=1-0.6781003
misclassificationerror

sum(diag(table_mat))     ###(1364+850)
sum(table_mat)           ####(1364+737+398+850)


printcp(fit.dtree)
plotcp(fit.dtree)




#####early stoping condition##

# Grow a tree with minsplit of 100 and max depth of 8
bank01_model_preprun <- rpart(deposit ~ ., data = train, method = "class", 
                              control = rpart.control(cp = 0, maxdepth = 8,minsplit = 100))

bank01_model_preprun
# Compute the accuracy of the pruned tree
test$pred <- predict(bank01_model_preprun, test, type = "class")
table02 <- table(test$pred,test$deposit)
table02
accuracy_preprun <- sum(diag(table02)) / sum(table02)
accuracy_preprun


###actual pruning###

bank01_model_pruned <- prune(fit.dtree, cp = 0.0025 )
plot(bank01_model_pruned)
text(bank01_model_pruned,pretty = 0)

# Compute the accuracy of the pruned tree
predict_test01 <-predict(bank01_model_pruned, test, type = 'class')
predict_test01

table_mat01 <- table(test$deposit, predict_test01)
table_mat01
accuracy_postprun <- mean(predict_test01 == test$deposit)
accuracy_postprun
data.frame(accuracy_Test, accuracy_preprun, accuracy_postprun)



