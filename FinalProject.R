#### Part One Feature Engineering ####
animal<- read.csv(file="/Users/xuan/Desktop/RStatisticalProgramming/Final/shelterdata.csv", header=TRUE, sep=",")
library(ggthemes)
library(ggplot2)
library(tidyverse)
library(caret)
library(cat)
library(Metrics)
library(pROC)
library(glmnet)
library(car)
library(data.table)
library(GGally)
library(mlbench)
library(MASS)
summary(animal)

#separate Sexuponoutcome column into Neutered and Gender
animal <- animal %>% separate(SexuponOutcome, c("Neutered", "Gender"), sep=" ")

#Fill in NA values with "Unknown" in Gender column
animal <- animal %>%
  mutate(Gender = replace(Gender, is.na(Gender), "Unknown"))

#Fill in NA values with "Unknown" in Neutered column
animal <- animal %>%
  mutate(Neutered = replace(Neutered, is.na(Neutered), "Unknown"))
animal$Neutered[animal$Neutered=='']="Unknown"

#Fill in NA values with "Unknown" in Name column
animal$Name <- as.character(animal$Name)
animal$Name[animal$Name==''] <- "Unknown"

#split datetime into month,year,day
animal$DateTime <- as.Date(animal$DateTime,'%m/%d/%Y')
animal <- animal %>%
  mutate(month = format(x = DateTime, "%m"),
         year = format(x = DateTime, "%Y"),
         day = format(x = DateTime, "%d"))

#change Age field to be in units of years
animal <- animal %>% separate(AgeuponOutcome, c("Num", "unit"), sep=" ")

#duplicate column
animal$unitDecimal = animal$unit

#convert column to factor
animal$unitDecimal <- as.factor(animal$unitDecimal)

#convert unitDecimal field to decimal values
animal <- animal %>%
  mutate(unitDecimal = gsub(pattern = "day|days", replacement = "0.002739", unitDecimal))

animal <- animal %>%
  mutate(unitDecimal = gsub(pattern = "week|weeks", replacement = "0.01923", unitDecimal))

animal <- animal %>%
  mutate(unitDecimal = gsub(pattern = "month|months", replacement = "0.0833", unitDecimal))

animal <- animal %>%
  mutate(unitDecimal = gsub(pattern = "year|years", replacement = "1", unitDecimal))

#convert unitDecimal and Num fields to numeric datatypes
animal$unitDecimalnum <- as.numeric(animal$unitDecimal)
animal$Num1 <- as.numeric(animal$Num)

#multiply Num*unitdecimal to get Age column in years
animal$Age = animal$Num1 * animal$unitDecimalnum

#delete unnecessary columns
animal$Num1 <- NULL
animal$Num <- NULL
animal$unitDecimal <- NULL
animal$unitDecimalnum <- NULL
animal$unit <- NULL

### Breed column ###
#detect mix in Breed col ("mix" or "/")
#get their indexes 
mix1 <-grep("Mix", animal$Breed)
mix2<-grep("/",animal$Breed)
mix <- c(mix1,mix2)

#check the ratio of breed and Purebred
Mixtype <- length(mix)
Purebred<-length(animal$Breed)-length(mix)
mixratio <- round(Purebred/Mixtype *100,3)
print(c(Mixtype,Purebred,mixratio))

#seperate the two breed type in a new column: BreedType
animal["BreedType"]<-NA
for(i in 1:length(animal$Breed)){
  if(is.element(i,mix)){
    animal$BreedType[i]<- "Mix"
  }else {
    animal$BreedType[i]<- "Purebred"
  }
  i=i+1
}

#add day of week column
animal$DayofWeek <- weekdays(as.Date(animal$DateTime))

#change intact column
animal["Intact"]<- NA
for(i in 1:length(animal$Neutered)){
  if(animal$Neutered[i] == "Intact"){
    animal$Intact[i]<- "Intact"
  }else{
    animal$Intact[i]<- "Not_Intact"
  }
  i=i+1
}

#Plots
p1 <- ggplot(data =animal, aes(x = AnimalType,fill = OutcomeType)) +
  geom_bar() +
  labs(title = "Count of Outcome by AnimalType") +
  theme(plot.title = element_text(hjust = .5), 
        axis.text.x = element_text(angle = 45, hjust = 1))

p1 + theme_economist() + scale_color_economist()

# seasonality plot
p2 <- ggplot(data = animal, aes(x = month, fill = OutcomeType)) +
  geom_bar() +
  labs(title = " Outcome Type per Month") +
  theme(plot.title = element_text(hjust = .5), 
        axis.text.x = element_text(angle = 45, hjust = 1))

p2 + theme_economist() + scale_color_economist()

# Outcome by DayofWeek
p3 <- ggplot(data = animal, aes(x = DayofWeek, fill = OutcomeType)) +
  geom_bar() +
  labs(title = "                     Outcome by DayofWeek") +
  theme(plot.title = element_text(hjust = .5), 
        axis.text.x = element_text(angle = 45, hjust = 1))

p3 + theme_economist() + scale_color_economist()

#Outcome by Intact/Not Intact
p4 <- ggplot(data = animal, aes(x = Intact, fill = OutcomeType)) +
  geom_bar() +
  labs(title = "                Outcome by Intact/Not Intact") +
  theme(plot.title = element_text(hjust = .5), 
        axis.text.x = element_text(angle = 45, hjust = 1))

p4 + theme_economist() + scale_color_economist()

#Outcome by Gender
p5 <- ggplot(data = animal, aes(x = Gender, fill = OutcomeType)) +
  geom_bar() +
  labs(title = "                Outcome by Gender") +
  theme(plot.title = element_text(hjust = .5), 
        axis.text.x = element_text(angle = 45, hjust = 1))

p5 + theme_economist() + scale_color_economist()

#Outcome by BreedType
p6<-ggplot(data = animal, aes(x = BreedType, fill = OutcomeType)) +
  geom_bar(position = "fill") +
  labs(title = "                Outcome by BreedType") +
  theme(plot.title = element_text(hjust = .5), 
        axis.text.x = element_text(angle = 45, hjust = 1))

p6 + theme_economist() + scale_color_economist()

p7 <- ggplot(data = animal, aes(x=Age,y=OutcomeType)) +
  geom_point()
p7 + theme_economist() + scale_color_economist()


### PART 2 Model Building ###
#split data into training and testing set
animal$Gender <- as.factor(animal$Gender)
animal$month <- as.factor(animal$month)
animal$BreedType <- as.factor(animal$BreedType)
animal$DayofWeek <- as.factor(animal$DayofWeek)
animal$Intact <- as.factor(animal$Intact)

set.seed(1)
data.split <- createDataPartition(animal$OutcomeType, p = .75, list = F)
training <- animal[data.split, ]
training<-na.omit(training)
testing <- animal[-data.split, ]
testing <- na.omit(testing)

#logistic regression
library(e1071)
set.seed(123)
animal.glm.cv <- train(OutcomeType ~ AnimalType + Gender + month + Age + BreedType + DayofWeek + Intact, data = training,
                      method = "glm", family = "binomial",
                      trControl = trainControl(method = "repeatedcv",#repeated k-fold
                                               repeats = 5, number = 3,#k=3
                                               savePredictions = T,
                                               classProbs=T),
                      na.action=na.exclude)

#training accuracy
animal.glm.cv$results

#plot the accuracy distribution
ggplot(data = animal.glm.cv$resample, aes(x = Accuracy)) +
  geom_density(alpha = .2, fill="red")

#extract the final model
animal.glm2 <- animal.glm.cv$finalModel
summary(animal.glm2)

#check multicollinearity
vif(animal.glm2)

#Training Accuracy
class.predictions <- predict(animal.glm.cv, newdata = training)
confusionMatrix(data = class.predictions, reference = training$OutcomeType,
                positive = "Positive")

class.predictions_prob <- predict(animal.glm.cv, newdata = training, type='prob')
roc(response = training$OutcomeType,
    predictor = class.predictions_prob$Positive, plot = T)



#testing confusion matrices
class.predictions <- predict(animal.glm.cv, newdata = testing)
confusionMatrix(data = class.predictions, reference = testing$OutcomeType,
                positive = "Positive")

class.predictions_prob <- predict(animal.glm.cv, newdata = testing, type='prob')
roc(response = testing$OutcomeType,
    predictor = class.predictions_prob$Positive, plot = T)


#Ridge logistic Regresstion
# We need to create a model matrix before inputting data into the model
animal.matrix <- model.matrix(OutcomeType ~ AnimalType + Gender + month + 
                                Age + BreedType + DayofWeek + Intact, 
                              data = training)[, -1] #drop the intercept

# We can now input the data into glmnet
set.seed(123)
animal.ridge <- cv.glmnet(x = animal.matrix, y = training$OutcomeType,
                         alpha = 0, nfolds = 5, family='binomial') #alpha=0 for ridge

# Display regression coefficients
coef(animal.ridge,s="lambda.1se")

logprobabilities <- predict(animal.ridge, s = animal.ridge$lambda.1se,
                            newx = animal.matrix)
predicted.classes <- ifelse(exp(logprobabilities) > 0.5, "Positive", "Negative")
#Training accuracy
observed.classes <- training$OutcomeType
mean(predicted.classes == observed.classes)

roc(response = training$OutcomeType,
    predictor = c(exp(logprobabilities)), plot = T)


# Make predictions on the test data
testing.matrix<-model.matrix(OutcomeType ~ AnimalType + Gender + month + 
                               Age + BreedType + DayofWeek + Intact, 
                             data = testing)[, -1]
logprobabilities <- predict(animal.ridge, s = animal.ridge$lambda.1se,
                            newx = testing.matrix)
predicted.classes <- ifelse(exp(logprobabilities) > 0.5, "Positive", "Negative")
# Model accuracy
observed.classes <- testing$OutcomeType
mean(predicted.classes == observed.classes)

roc(response = testing$OutcomeType,
    predictor = c(exp(logprobabilities)), plot = T)


#Lasso logistic Regresstion
# We can now input the data into glmnet
set.seed(123)
animal.lasso <- cv.glmnet(x = animal.matrix, y = training$OutcomeType,
                          alpha = 1, nfolds = 5, family='binomial') #alpha=1 for lasso

# Display regression coefficients
coef(animal.lasso,s="lambda.1se")

logprobabilities <- predict(animal.lasso, s = animal.lasso$lambda.1se,
                            newx = animal.matrix)
predicted.classes <- ifelse(exp(logprobabilities) > 0.5, "Positive", "Negative")
# Training accuracy
observed.classes <- training$OutcomeType
mean(predicted.classes == observed.classes)

roc(response = training$OutcomeType,
    predictor = c(exp(logprobabilities)), plot = T)


# Make predictions on the test data
logprobabilities <- predict(animal.lasso, s = animal.lasso$lambda.1se,
                            newx = testing.matrix)
predicted.classes <- ifelse(exp(logprobabilities) > 0.5, "Positive", "Negative")
# Model accuracy
observed.classes <- testing$OutcomeType
mean(predicted.classes == observed.classes)

roc(response = testing$OutcomeType,
    predictor = c(exp(logprobabilities)), plot = T)


