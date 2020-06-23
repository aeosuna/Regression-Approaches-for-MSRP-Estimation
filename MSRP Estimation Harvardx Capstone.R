#Installing necessary packages----
#Package and dataset download and 
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret))install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(knitr)) install.packages("knitr", repos = "http://cran.us.r-project.org")
if(!require(kableExtra)) install.packages("kableExtra", repos = "http://cran.us.r-project.org")
if(!require(randomForest)) install.packages("randomForest", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(gam)) install.packages("gam", repos = "http://cran.us.r-project.org")
if(!require(corrplot)) install.packages("corrplot", repos = "http://cran.us.r-project.org")
if(!require(stringr)) install.packages("stringr", repos = "http://cran.us.r-project.org")
if(!require(tidyr)) install.packages("tidyr", repos = "http://cran.us.r-project.org")
if(!require(splines)) install.packages("splines", repos = "http://cran.us.r-project.org")
if(!require(rpart)) install.packages("rpart", repos = "http://cran.us.r-project.org")
if(!require(foreach)) install.packages("foreach", repos = "http://cran.us.r-project.org")
if(!require(nnet)) install.packages("nnet", repos = "http://cran.us.r-project.org")
if(!require(care)) install.packages("care", repos = "http://cran.us.r-project.org")
if(!require(Rborist)) install.packages("Rborist", repos = "http://cran.us.r-project.org")
if(!require(gmodels)) install.packages("gmodels", repos = "http://cran.us.r-project.org")
if(!require(RCurl)) install.packages("RCurl", repos = "http://cran.us.r-project.org")

#Loading critical packages
library(tidyverse)
library(caret)
library(stringr)
library(kableExtra)
library(data.table)
library(tidyr)
library(corrplot)
library(gam)
library(splines)
library(foreach)
library(rpart)
library(gmodels)
library(caret)
library(nnet)
library(randomForest)
library(Rborist)
library(RCurl)

#Download Dataset----

#We load the csv file or from the gitbhub link
url<- getURL("https://raw.githubusercontent.com/aeosuna/Regression-Approaches-for-MSRP-Estimation/master/CarPricesData.csv")
data<- read.csv(text = url)

#If you decide to manually donwload the csv file to your computer
#data<- read.csv("CarPricesData.csv")

#Dataset Structure and Pre-processing----
head(data) #%>% kable %>% kable_styling()

#dataset structure
str(data) #structure of the data. Variable classes are observed
sum(is.na(data))
data<-data %>% drop_na() #drop all NAs
nrow(data) #number of observations
ncol(data) #number of variables

max(data$Year) #this dataset is from 2017
#So if we want to calculate an age variable we can do the following:
data_new<- data %>% mutate(age= max(data$Year)-Year)

#Fuel economy feature can be calculated by a weighted average between city mpg and highway mpg
data_new<- data_new %>% mutate(Fuel_eff= (city.mpg*0.55+ highway.MPG*0.45))

#This shows the two new variables
data_new[,17:18] %>% head()

#Lets remove outliers: 
#Really expensive and rare cars that have small number of observations and therefore are hard to predict.
#Most of the times you need to go directly to the car dealer for this cars (they need a separate model)

#A common way of doing this is by using interquartiles:
#Since we also wan to be able to capture some of this outliers we will use the 75% quantile of the most expensive bracket:
q_3<-quantile(data_new$MSRP, .75)

#we compute the interquartile 
iqr<-IQR(data_new$MSRP)

#The lower bound will be 0 since no prices cant be negative
#The we compute the upper bound of our distribution and we clean out tests from outliers
data_new<- data_new%>% filter(MSRP<= q_3+ 1.5*iqr) 

set.seed(1, sample.kind= "Rounding")
#randomly partitions the dataset into test and training set:
test_index<- createDataPartition(data_new$MSRP, times = 1, p= 0.2, list= FALSE)
test_set<- data_new[test_index,]
train_set<- data_new[-test_index,]

#creates a table to compare size of both sets
data.frame(Set= c("Train", "Test"), 
           Rows= c(nrow(train_set), nrow(test_set)),
           Columns= c(ncol(train_set), ncol(test_set))) #%>% 
  #kable %>% kable_styling()

#creates a summary table of our price target
summary(train_set$MSRP)

#Number of unique brands
paste("There are a total of", n_distinct(data_new$Make), "unique car brands in our dataset")

#Crate table that compute NAs per feature to make sure the data is clean 
NAs_table<- data.frame(Variable=colnames(train_set[1]), NAs= sum(is.na(train_set[,1])))
for (i in 2:ncol(train_set)){
NAs_table<-bind_rows(NAs_table, data.frame(Variable=colnames(train_set[i]), NAs= sum(is.na(train_set[,i]))))
}
NAs_table %>% kable %>% kable_styling()

#Data Visualization Analysis----
#the training set contains cars from all over the years
train_set %>% group_by(Year) %>%
  summarize(Count=n(), .groups= "drop") %>% 
  ggplot(aes(Year, Count))+
  geom_bar(stat= "identity", color= "black", na.rm = TRUE)+
  theme_minimal()+
  theme(axis.text.y = element_text(size=10), title = element_text(size=12))+ggtitle("Car/Year Distribution")+
  ggtitle("Car/Year Distribution")

#the training set also contains all type of cars or vehicle styles
train_set %>% group_by(Vehicle.Style) %>%
  summarize(Count=n(), .groups= "drop") %>% 
  ggplot(aes(Vehicle.Style, Count))+
  geom_bar(stat= "identity", color= "black", na.rm = TRUE)+
  theme_minimal()+
  theme(axis.text.y = element_text(size=10), 
        title = element_text(size=12), 
        axis.text.x = element_text(angle = 90, hjust = 1))+
  xlab("Vehicle Style")

#Some of these cars compete in different market categories:
train_set %>% separate_rows(Market.Category, sep = "\\,") %>% group_by(Market.Category) %>% 
  summarize(Count= n(), .groups= "drop") %>% 
  ggplot(aes(Market.Category, Count, fill= Market.Category))+
  scale_fill_brewer(palette = "Set3")+
  geom_bar(stat = "identity")+
  theme(axis.text.x = element_text(angle=90, hjust=1))+
  xlab("Market")+
  ggtitle("Car Market Segmentation")
#As you can observe there are over 3000 N/A values in the market category data.

#both highway and city mpg does not seem to have a strong effect on MSRP
train_set %>% select(highway.MPG, city.mpg, MSRP) %>% 
  gather(Feature, MPG,-MSRP) %>% 
  ggplot(aes(MPG, MSRP)) +
  geom_violin()+
  facet_wrap(~ Feature, scales= "free")

#After calculating a new variables called Fuel efficiency:
train_set %>% filter(Fuel_eff<=70) %>% #lets filter some of the outliers to better see the trend
  ggplot(aes(Fuel_eff, MSRP))+
  geom_point()+
  ggtitle("Fuel Economy")

#Lets check the relationship between horsepower and MSRP
train_set %>% ggplot(aes(Engine.HP, MSRP))+
  geom_point(na.rm = TRUE)+
  geom_smooth(method= "loess",na.rm = TRUE)

#Let's confirm the hypothesis that the year of the car affects the price
train_set %>% group_by(age) %>% 
  summarize(avg_msrp= mean(MSRP), .groups= "drop") %>% ggplot(aes(age, avg_msrp))+
  geom_line(size= 1.5, color= "blue")+
  theme_bw()
#Old cars have a drop in value because of their age

#What would the relationship between popularity and MSRP be?
paste("There are", n_distinct(train_set$Popularity), "unique popularity classes.",
      "This shows how each brand has its own level of measured popularity")

train_set %>% group_by(Popularity) %>% summarize(avg_msrp= mean(MSRP), .groups= "drop") %>% 
  ggplot(aes(Popularity, avg_msrp))+ 
  geom_line(size= 1, color= "red",na.rm = TRUE)+
  scale_x_log10()+
  theme_bw() #this code made me understand that each brand has its own level of popularity

#This code creates a boxplot facet wrap with 4 graphs to compare MSRP
#within driven_wheels and vehicle size variable
train_set %>% select(Driven_Wheels, Vehicle.Size, MSRP) %>% 
  ggplot(aes(Vehicle.Size, MSRP, fill= Driven_Wheels))+ 
  geom_violin(na.rm = TRUE, outlier.shape = TRUE)+
  stat_summary(fun = mean, geom = "point", size= 2)+
  facet_wrap(~ Driven_Wheels, scales = "free")+
  ylim(c(0,60000))

#This code creates a boxplot to compare the MSRP of all vehicle categories or style
train_set %>% select(Vehicle.Style, Transmission.Type, MSRP) %>%
  ggplot(aes(Vehicle.Style, y= MSRP, fill= Vehicle.Style))+ 
  geom_boxplot(na.rm = TRUE, outlier.shape = TRUE)+
  ylim(c(0,60000))+
  theme(axis.text.x = element_text(angle=75, hjust=1))+
  xlab("Vehicle Style")
#as you can see from the graph the median seems to be slightly increasing as you move to the right

#In the next graph we can compare the boxplots for each vehicle type
train_set %>% ggplot(aes(Vehicle.Size, MSRP, fill= Vehicle.Size)) + 
  geom_boxplot()

#Feature Engineering & Selection----

#Use the corrplot package to create a correlation matrix graphic                   
cor_matrix<-train_set %>% filter(!is.na(Engine.HP)) %>% filter(!is.na(Engine.Cylinders)) %>% 
  filter(!is.na(Number.of.Doors)) %>% select(age,Engine.HP, Engine.Cylinders, 
                     Number.of.Doors, Fuel_eff,Popularity,MSRP) %>% 
  mutate(Engine.HP= as.numeric(Engine.HP), Engine.Cylinders= as.numeric(Engine.Cylinders),
         Number.of.Doors=as.numeric(Number.of.Doors)) %>% cor()
corrplot(cor_matrix, method= "circle", order= "hclust")        


#This process expands the original matrix by separating the category market feature into 10 different new binary variables
names<- unique(train_set %>% separate_rows(Market.Category, sep = "\\,") %>% select(Market.Category))
train_set_clean<-train_set %>% separate(Market.Category, into= c(names$Market.Category), sep = "\\,") %>% 
  bind_cols(train_set$Market.Category)
train_set_clean$...29<- as.character(train_set_clean$...29)

#This loop will assign the binary values to the new variables:
for (i in 1:nrow(train_set_clean)){
  for (j in 10:20){
    string<- data.frame(n= i, string= c(str_split(train_set_clean$...29[i],pattern = ",")))
    colnames(string)<-c("n","string")
    string$string<-as.character(string$string)
    value<-ifelse(colnames(train_set_clean[j]) %in% string$string, 1, 0)
   train_set_clean[i,j]<- value
  }
}

#We need to perform the same for the test set:

#This process expands the original matrix by seprating the category market feature into 10 different new binary variables
names<- unique(test_set %>% separate_rows(Market.Category, sep = "\\,") %>% select(Market.Category))
test_set_clean<-test_set %>% separate(Market.Category, into= c(names$Market.Category), sep = "\\,") %>% 
  bind_cols(test_set$Market.Category)
test_set_clean$...29<- as.character(test_set_clean$...29)

for (i in 1:nrow(test_set_clean)){
  for (j in 10:20){
    string<- data.frame(n= i, string= c(str_split(test_set_clean$...29[i],pattern = ",")))
    colnames(string)<-c("n","string")
    string$string<-as.character(string$string)
    value<-ifelse(colnames(test_set_clean[j]) %in% string$string, 1, 0)
    test_set_clean[i,j]<- value
  }
}

#Lets rename the N/A version of car market category
names(train_set_clean)[names(train_set_clean) == "N/A"] <- "Regular"
names(test_set_clean)[names(test_set_clean) == "N/A"] <- "Regular"

#First we need to covert all new features as numeric for further testing
train_set_clean[,10:20]<-train_set_clean %>% 
  select(c(colnames(train_set_clean[,10:20]))) %>% mutate_if(is.character, as.numeric)
test_set_clean[,10:20]<-test_set_clean %>% 
  select(c(colnames(test_set_clean[,10:20]))) %>% mutate_if(is.character, as.numeric)

#We can check we convert it the columns into numeric type by typing this code:
class(train_set_clean$Performance)
class(test_set_clean$Performance)

#Lets create a violin chart to examine the distribution of these new variables
train_set_clean %>% select(c(colnames(train_set_clean[,10:20])), MSRP) %>% gather(Market, value, -MSRP) %>% 
  ggplot(aes(as.factor(value), MSRP, fill= as.factor(value))) + 
  geom_violin()+
  stat_summary(fun=mean, geom="point", size=2, color="black")+
  facet_wrap(~Market, scales= "free")

#Lets create a correlation matrix to examine them
cor_matrix2<-train_set_clean %>% select(Crossover, Diesel, Exotic, `Factory Tuner`,
                    `Flex Fuel`, Hatchback, Hybrid, `High-Performance`,
                    Luxury,Regular,Performance, MSRP) %>% cor()
#It prints the matrix
print(cor_matrix2)
#Generate one more correlation graphic
corrplot(cor_matrix2, order= "hclust", method= "circle")

#This code creates a heatmap
heatmap(cor_matrix2, symm = TRUE)

#This code will compute the mean, median, number of observations, standard deviation, mean error and the confidence interval
train_set_clean %>% select(c(colnames(train_set_clean[,10:20])), MSRP) %>% 
  gather(Market, value, -MSRP) %>% 
  group_by(Market,value) %>% 
  summarize(mean= mean(MSRP), median= median(MSRP),n=n(),
            sd= sd(MSRP),me= qnorm(.95)*(sd/sqrt(n)),lower= mean-me, upper= mean+me) %>% 
  filter(Market %in% c("Regular", "Exotic", "Luxury", "Factory Tuner", "High-Performance", "Performance"))

#Lets group by transmission type, driven wheels, and compute the count and avg_msrp)
train_set_clean %>% group_by(Transmission.Type, Driven_Wheels) %>% 
  summarize(count=n(), avg_msrp= mean(MSRP),.groups="drop")

#Feature testing with linear regression

#check for linear significance of new categorical variables on MSRP
feature_testing<-train_set_clean %>% select(c(colnames(train_set_clean[,10:20])), MSRP)
fit_lm<- lm(MSRP ~ ., data = feature_testing)
fit_lm$coefficients
summary(fit_lm)

#check for linear significance between HP bins variable and MSRP
features_fit<- lm(MSRP~ age + Engine.HP, data= train_set_clean)
features_fit$coefficients
summary(features_fit)

#Lets drop the old market.category column since we wont need it
train_set_clean<- train_set_clean %>% select(-...29)
test_set_clean<- test_set_clean %>% select(-...29)

#Final Model & Testing----

#Lets create a function that returns the Mean absolute error
MAE<- function(true_msrp, predicted_msrp){
  mean(abs(true_msrp-predicted_msrp))
}

#Calculates Rsquared if needed:
rsq <- function (x, y){
  cor(x, y) ^ 2
} 

#KNN Regression algorithm----

#Since we will be using knn regression it is importanto to standarize the variables 
#so that the algortihm can better compute the distances within the same scale or unit

#This code standarizes different variables within the train and test set
train_set_clean$Engine.HP<- scale(train_set_clean$Engine.HP)
train_set_clean$Fuel_eff<- scale(train_set_clean$Fuel_eff)
test_set_clean$Engine.HP<- scale(test_set_clean$Engine.HP)
test_set_clean$Fuel_eff<- scale(test_set_clean$Fuel_eff)
train_set_clean$Popularity<- scale(train_set_clean$Popularity)
test_set_clean$Popularity<- scale(test_set_clean$Popularity)

#Since Knn can better calculate the distances we can include Number of doors as numeric and scale it
train_set_clean$Number.of.Doors<- scale(as.numeric(train_set_clean$Number.of.Doors))
test_set_clean$Number.of.Doors<- scale(as.numeric(test_set_clean$Number.of.Doors))

#We make sure that the class of our market variables are in numeric format
class(train_set_clean$Number.of.Doors) #it is!

#We create a table so that we can compare and store all RMSE results for different values of k
results_knnreg<-data.frame(k= as.numeric(), mae= as.numeric(), r_squared= as.numeric())

#Now we can run a loop to test for different k values for our knnreg algorithm
for (i in 3:25){
fit_knnreg<- knnreg(x=train_set_clean[,c(5,9,12,25,27,28)],y= train_set_clean$MSRP, k=i)

predictions<- data.frame(fitted= predict(fit_knnreg, train_set_clean[,c(5,9,12,25,27,28)]))
results_knnreg<-bind_rows(results_knnreg, data.frame(k=fit_knnreg$k,
                                                     mae= MAE(train_set_clean$MSRP, predictions$fitted),
                                                     r_squared= rsq(train_set_clean$MSRP, predictions$fitted)))
}

#graph of the mean absolute error was it increases with the k
results_knnreg %>% ggplot(aes(k, mae)) + geom_line() +ggtitle("MAE vs K")
results_knnreg %>% ggplot(aes(k, r_squared))+ geom_line()+ ggtitle("R^2 vs K")
#As you can see from this curve, the minimum MAE is gotten by k=3

#So we run the algorithm again but just for k=3
fit_knnreg<- knnreg(x=train_set_clean[,c(5,9,25,27,28)],y= train_set_clean$MSRP, k=3)

#Run on test set:
test_knnreg_results<-data.frame(Real= test_set_clean$MSRP, 
                                Fitted= predict(fit_knnreg, test_set_clean[,c(5,9,25,27,28)]))

#Calculate %error of calculation and the median error %                               
test_knnreg_results<-test_knnreg_results %>% mutate(percentage_error= abs((Fitted-Real)/Real)*100)
median(test_knnreg_results$percentage_error)

#results on testing set
test_knnreg_results %>% ggplot(aes(Real, Fitted)) +
  geom_point()+
  geom_abline(intercept = 0, slope=1)+
  annotate("text", x = 25000, y = 75000, label = paste("R^2:",round(rsq(test_knnreg_results$Real,test_knnreg_results$Fitted),3)))+
  annotate("text", x = 25000, y = 90000, label = paste("MAE:",round(MAE(test_knnreg_results$Real, test_knnreg_results$Fitted))))+
  ggtitle("R^2 and MAE ($) performance on Test set - KNN Regression")

#Decision Tree Regression----

#for 10-fold cross-validation control
control<- trainControl(method="cv", number= 10, p=0.9)

# Make Valid Column Names for both the test and training set
colnames(train_set_clean) <- make.names(colnames(train_set_clean))
colnames(test_set_clean) <- make.names(colnames(test_set_clean))

#Convert these market category variables into factors
train_set_clean[,10:20]<-train_set_clean %>% select(c(colnames(train_set_clean[,10:20]))) %>% mutate_if(is.numeric, as.factor)
test_set_clean[,10:20]<-test_set_clean %>% select(c(colnames(test_set_clean[,10:20]))) %>% mutate_if(is.numeric, as.factor)

set.seed(1, sample.kind= "Rounding")
#Lets perform a decision regression tree to be able to incorporate categorical variables as well
rpart_fit<- train(MSRP~ Make +Luxury+ Performance +  Regular + Flex.Fuel + Factory.Tuner + 
                    Engine.HP + Transmission.Type+ Driven_Wheels+
                    High.Performance+ Hatchback+ Hybrid+ Diesel+ Vehicle.Style+
                    Exotic+ Crossover+ age+ Fuel_eff,
                  method= "rpart",
                  tuneGrid=data.frame(cp=seq(0,0.05,len=25)),
                 trControl= control,data= train_set_clean)
plot(rpart_fit)
#Analysis of results 
train_rpart_results<-rpart_fit$results
train_rpart_results %>% ggplot(aes(cp, MAE)) + geom_line() + ggtitle("Train set MAE- Regression Tree")

#Testing Results
results_dt<- data.frame(Real= test_set_clean$MSRP, Fitted= predict(rpart_fit, test_set_clean))

#Calculate %error of each observations
results_dt<- results_dt %>% mutate(percentage_error= abs((Fitted-Real)/Real)*100)

#calculating the median %error
median(results_dt$percentage_error)

#results on testing set
results_dt %>% ggplot(aes(Real, Fitted)) +
  geom_point()+
  geom_abline(intercept = 0, slope=1)+
  annotate("text", x = 25000, y = 75000, label = paste("R^2:",round(rsq(results_dt$Real,results_dt$Fitted),3)))+
  annotate("text", x = 25000, y = 90000, label = paste("MAE:",round(MAE(results_dt$Real, results_dt$Fitted))))+
  ggtitle("R^2 and MAE ($) performance on Test set- Decision Tree Regression")

#Random Forest----

modelLookup("rf")
#In here we will use 5k fold for better computational time performance

#for 5-fold cross-validation control
control_rf<- trainControl(method="cv", number= 5, p=0.9)

set.seed(1, sample.kind= "Rounding")
grid<-expand.grid(mtry= seq(10,18))#set tunnig paramater sequence to test randomyl selected variables
rf_fit<- train(MSRP~ Make+ Luxury+ Performance +  Regular + Flex.Fuel + Factory.Tuner + 
                 Engine.HP + Transmission.Type+ Driven_Wheels+
                 High.Performance+ Hatchback+ Hybrid+ Diesel+ Vehicle.Style+
                 Exotic+ Crossover+ age+ Fuel_eff,
               method= "rf", tuneGrid=grid,
               ntree=50, trControl= control_rf, data= train_set_clean)

#plot to see the R^2 behavior
plot(rf_fit$finalModel$rsq,xlab = "Tree Index", ylab = "R^2")
plot(rf_fit)

#This code graphs shows how well did the training set perform
rf_train_results<-data.frame(Real= train_set_clean$MSRP, Fitted= predict(rf_fit, train_set_clean[,c(1,5,7,8,10:20,22,27,28)]))
rf_train_results%>% 
  ggplot(aes(Real, Fitted))+
  geom_point()+
  geom_abline(intercept = 0,  slope=1)+
  annotate("text", x = 25000, y = 75000, label = paste("R^2:",round(rsq(rf_train_results$Real,rf_train_results$Fitted),3)))+
  annotate("text", x = 25000, y = 90000, label = paste("MAE:",round(MAE(rf_train_results$Real, rf_train_results$Fitted))))+
  ggtitle("Train set Performance- Random Forest")

#Run on test set:
rf_results<-data.frame(Real= test_set_clean$MSRP, Fitted= predict(rf_fit, test_set_clean[,c(1,5,7,8,10:20,22,27,28)]))

#Calculating error% of each observation
rf_results<- rf_results %>% mutate(percentage_error= abs((Fitted-Real)/Real)*100)

#Calculating the median %error
median(rf_results$percentage_error)

#results on testing set
rf_results %>% ggplot(aes(Real, Fitted)) +
  geom_point()+
  geom_abline(intercept = 0, slope=1)+
  annotate("text", x = 25000, y = 75000, label = paste("R^2:",round(rsq(rf_results$Real,rf_results$Fitted),3)))+
  annotate("text", x = 25000, y = 90000, label = paste("MAE:",round(MAE(rf_results$Real, rf_results$Fitted))))+
  ggtitle("Test set Performance - Random Forrest")
