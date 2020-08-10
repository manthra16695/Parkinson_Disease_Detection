##Parkinsons Disease detection using telemonitoring vocal signals recorded from patients
getwd()
setwd('C:\\Users\\manth\\Documents')

##Reading the data
data=read.csv('Parkinsons_data.csv')


##Data Preparation
##Vlidating the variable datatypes
str(data)

##Converting the data types of variables into suitable ones
data$status=as.factor(data$status)

##Dropping unimportant variables
data$name=NULL

dim(data) 
##Totally there are 22 independent variables and 1 target variable(status) and 195 observations recorded from 31 individuals
##of which 23 are healthy

##Checking for NA values
complete.cases(data)
navalues=which(!complete.cases(data))##Gives rows which has NA values
dim(navalues)

##There are no NA values in the data

##Normalizing data since different numeric variables in different ranges

##Subsetting Independent and dependent variables in x and y
x=data[,-c(17)]
y=data[,17]

##Creating a function for Normalization
##WE use Min Max Normalization

normalize=function(x){
  return ((x-min(x,na.rm = TRUE))/(max(x,na.rm = TRUE)-min(x,na.rm = TRUE)))
  
}

##Applying the function to the entire dataset
norm_data=as.data.frame(apply(x, 2, normalize))
norm_data$status=y
str(norm_data)

##Exploratory Data analysis

cols=colnames(data)

##BIvariate Statistics between a Factor and a Numeric variable
boxplot(norm_data$MDVP.Fo.Hz.~norm_data$status,col=c('red','blue'),xlab = 'Parkinson_Status',main='Feature and Parkinson Status')
boxplot(norm_data$MDVP.Fhi.Hz. ~norm_data$status,col=c('red','blue'),xlab = 'Parkinson_Status',main='Feature and Parkinson Status')
boxplot(norm_data$MDVP.Flo.Hz.~norm_data$status,col=c('red','blue'),xlab = 'Parkinson_Status',main='Feature and Parkinson Status')
boxplot(norm_data$MDVP.Jitter...~norm_data$status,col=c('red','blue'),xlab = 'Parkinson_Status',main='Feature and Parkinson Status')
boxplot(norm_data$MDVP.Jitter.Abs.~norm_data$status,col=c('red','blue'),xlab = 'Parkinson_Status',main='Feature and Parkinson Status')
boxplot(norm_data$MDVP.RAP~norm_data$status,col=c('red','blue'),xlab = 'Parkinson_Status',main='Feature and Parkinson Status')
boxplot(norm_data$MDVP.PPQ~norm_data$status,col=c('red','blue'),xlab = 'Parkinson_Status',main='Feature and Parkinson Status')
boxplot(norm_data$Jitter.DDP~norm_data$status,col=c('red','blue'),xlab = 'Parkinson_Status',main='Feature and Parkinson Status')
boxplot(norm_data$MDVP.Shimmer~norm_data$status,col=c('red','blue'),xlab = 'Parkinson_Status',main='Feature and Parkinson Status')
boxplot(norm_data$MDVP.Shimmer.dB.~norm_data$status,col=c('red','blue'),xlab = 'Parkinson_Status',main='Feature and Parkinson Status')
boxplot(norm_data$Shimmer.APQ3~norm_data$status,col=c('red','blue'),xlab = 'Parkinson_Status',main='Feature and Parkinson Status')
boxplot(norm_data$Shimmer.APQ5~norm_data$status,col=c('red','blue'),xlab = 'Parkinson_Status',main='Feature and Parkinson Status')
boxplot(norm_data$MDVP.APQ~norm_data$status,col=c('red','blue'),xlab = 'Parkinson_Status',main='Feature and Parkinson Status')
boxplot(norm_data$Shimmer.DDA~norm_data$status,col=c('red','blue'),xlab = 'Parkinson_Status',main='Feature and Parkinson Status')
boxplot(norm_data$NHR~norm_data$status,col=c('red','blue'),xlab = 'Parkinson_Status',main='Feature and Parkinson Status')
boxplot(norm_data$HNR~norm_data$status,col=c('red','blue'),xlab = 'Parkinson_Status',main='Feature and Parkinson Status')
boxplot(norm_data$DFA~norm_data$status,col=c('red','blue'),xlab = 'Parkinson_Status',main='Feature and Parkinson Status')
boxplot(norm_data$RPDE~norm_data$status,col=c('red','blue'),xlab = 'Parkinson_Status',main='Feature and Parkinson Status')
boxplot(norm_data$spread1~norm_data$status,col=c('red','blue'),xlab = 'Parkinson_Status',main='Feature and Parkinson Status')
boxplot(norm_data$spread2~norm_data$status,col=c('red','blue'),xlab = 'Parkinson_Status',main='Feature and Parkinson Status')
boxplot(norm_data$D2~norm_data$status,col=c('red','blue'),xlab = 'Parkinson_Status',main='Feature and Parkinson Status')
boxplot(norm_data$PPE~norm_data$status,col=c('red','blue'),xlab = 'Parkinson_Status',main='Feature and Parkinson Status')

##Seems like all the features have some significant relationship between the target based on EDA
##We can confirm it by performing Hypothesis testing using t.test

t.test(norm_data$MDVP.Fo.Hz.     ~norm_data$status)  
t.test(norm_data$MDVP.Fhi.Hz.    ~norm_data$status)
t.test(norm_data$MDVP.Flo.Hz.    ~norm_data$status)
t.test(norm_data$MDVP.Jitter...  ~norm_data$status)
t.test(norm_data$MDVP.Jitter.Abs.~norm_data$status)
t.test(norm_data$MDVP.RAP        ~norm_data$status)
t.test(norm_data$MDVP.PPQ        ~norm_data$status)
t.test(norm_data$Jitter.DDP      ~norm_data$status)
t.test(norm_data$MDVP.Shimmer    ~norm_data$status)
t.test(norm_data$MDVP.Shimmer.dB.~norm_data$status)
t.test(norm_data$Shimmer.APQ3    ~norm_data$status)
t.test(norm_data$Shimmer.APQ5    ~norm_data$status)
t.test(norm_data$MDVP.APQ        ~norm_data$status)
t.test(norm_data$Shimmer.DDA     ~norm_data$status)
t.test(norm_data$NHR             ~norm_data$status)
t.test(norm_data$HNR             ~norm_data$status)
t.test(norm_data$DFA             ~norm_data$status)
t.test(norm_data$RPDE            ~norm_data$status)
t.test(norm_data$spread1         ~norm_data$status)
t.test(norm_data$spread2         ~norm_data$status)
t.test(norm_data$D2              ~norm_data$status)
t.test(norm_data$PPE             ~norm_data$status)

## t test also have confirmed that there is a higher significance between all the variables and hence ignoring no variables

##Correlation plot
pairs(x[1:10])
library(corrplot)
corr=cor(x)
corrplot(corr)

##It seems like there are some higher correlations and collinearity problem between independent variables
##It's better to remove the highly collinear variables using VIF else it might weaken our model 


##Selecting significant variables are very essential in developing a string model
##We have the following possible ways to select significant variables
##Regularization (TO penalize the coefficients towards zero for the insgnificant variables)
##Subset Selection
##Exhaustive Regression to look at all possible Linear models but it is inefficient compute wise
##hence we select Stepwise method which selects only restricted possible models which is computatinally efficient

##Claculating VIF
modl=glm(status~.,data=norm_data,family = binomial(link = 'logit'))
library(car)
vif(modl)

##Eliminating variables with VIF > 5 which has higher collinearity

##FInally the selected variables are
##MDVP.Fhi.Hz.
##MDVP.Flo.Hz.
##NHR
##RPDE
##DFA
##spread2
##D2

##Performing Train Test split before Step wise regression
set.seed(1234)
ind=sample(2,nrow(norm_data),replace = TRUE,prob = c(0.75,0.25))
train=norm_data[ind==1,]
test=norm_data[ind==2,]


##Step Wise Regression to select the significant variables step by step from a model with less AIC

interceptmodl=glm(status~1,data=train,family='binomial')
mod=glm(status~MDVP.Fo.Hz.+MDVP.Flo.Hz.+NHR+RPDE+DFA+spread2+D2,data=train,family='binomial')

step(interceptmodl,direction = 'both',scope = formula(mod))

##THe final model obtained after step wise regression is 
##-4.086+(3.243)*spread2+(-3.412)*MDVP.Flo.Hz.+(8.57)*D2+(3.318)*DFA

##Sigificant variables selected using Step wise regression
##spread2
##MDVP.Flo.Hz.
##D2
##DFA

##Checking for Interaction between variables
##Getting all combinations of interaction variables into a dataframe

interactiondata <- as.data.frame(model.matrix(~( MDVP.Flo.Hz. + DFA+ spread2 + D2)^2+status,train))
str(interactiondata)

##Creating model to identify which of the interaction variables are significant and adds value to our model

interactionsinterceptmodel=glm(status1~1,data = interactiondata,family = 'binomial')

  
interactionsmodel=glm(status1~MDVP.Flo.Hz. + DFA+ spread2 + D2+MDVP.Flo.Hz.:DFA+MDVP.Flo.Hz.:spread2+MDVP.Flo.Hz.:spread2+
        MDVP.Flo.Hz.:D2 +DFA:spread2+DFA:D2+spread2:D2 ,data = interactiondata)
##Again we use step wise regression to select significant interactions

step(interactionsinterceptmodel,direction = 'both',formula(interactionsmodel))
##So after running our step wise regression we get the following model and significant interactions and variables

##Model=-9.216+(24.894)*spread2+(-17.566)*MDVP.Flo.Hz.+(-1.793)*D2+(16.485)*DFA+(39.340)*MDVP.Flo.Hz :D2
##+(=31.297)*spread2:DFA
##with FInal AIC as 99.07
##Significant Interactions which can add value to our model are as below
##MDVP.Flo.Hz.:D2
##spread2:DFA 

##Building our final model with all significant variables and Significant variables using the train data

glmodel=glm(status ~ spread2 + MDVP.Flo.Hz. + D2 + DFA + MDVP.Flo.Hz.:D2 + 
      spread2:DFA, family = "binomial", data = train)
pr=plogis(predict(glmodel,test))

pred=ifelse(pr>0.5,1,0)
library(caret)
confusionMatrix(as.factor(pred),test$status,positive = '1')

##ROC Curves and Performance metrics
library(pROC)
plot(roc(train$status , glmodel$fitted.values, direction="<"),
     col="yellow", lwd=3)



##In this case sentivity is 100% that our model is able to accurately predict all Positive values wheras with less Specificity
##But since our ultimate aim is to not miss any positive cases high sensitivity is what we desire

##To solve the low specificity value we do balancing techniques on the data since Negative values are very lesser compared to Positive values
##We do over sampling to scale the negative values to match with the Positive values
table(train$status)
library(ROSE)
over=ovun.sample(status~.,data=train,method='over',N=230)$data
table(over$status)
##Both 1 and 0 values are now equal to 115 values



##Building a Random Forest Model
library(randomForest)

rf=randomForest(status ~ spread2 + MDVP.Flo.Hz. + D2 + DFA + MDVP.Flo.Hz.:D2 + 
               spread2:DFA, data = over,probability=TRUE)
plot(rf)
varImpPlot(rf)
pr=as.numeric(predict(rf,test,type='prob'))
confusionMatrix(pr,test$status,positive = '1')

##ROC Curves and Performance metrics
library(pROC)

plot(roc(test$status,pr , direction="<"),
     col="yellow", lwd=3, main="The turtle finds its way")

library(ROCR)
pr=as.numeric(pr)
prd=prediction(pr,test$status)
perf=performance(prd,"tpr","fpr")


plot(perf)


##Building an SVM Model
library(e1071)
svmmodel=svm(status ~ spread2 + MDVP.Flo.Hz. + D2 + DFA + MDVP.Flo.Hz.:D2+spread2:DFA, data = over,probability=TRUE)
summary(svmmodel)

pr=predict(svmmodel,test)
confusionMatrix(pr,test$status,positive = '1')


##Tuning the svm model with different parameters
svm.tune <- tune(svm, status ~ spread2 + MDVP.Flo.Hz. + D2 + DFA + MDVP.Flo.Hz.:D2+spread2:DFA, data = over,
                 ranges = list(cost=c(0.001,0.01,0.1,1,5,10,100)))




summary(svm.tune$best.model)

svm.tune
require(ROCR)



##Finally SVM radial model has the highest F1 score, accuracy, Sensitivty and Specificity with 83.08, 90.7, 96.88, 72.73



