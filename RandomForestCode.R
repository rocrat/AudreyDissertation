### Set up environment ----

library(memisc)
library(reshape2)
library(tidyverse)
library(cowplot)
library(randomForest)


### Read in data ----
df <- as.data.set(spss.system.file("./Rcode/Data/College coping data (complete).sav"))
df <- as.data.frame(df)

#Create case ID variable
df$ID <- rownames(df)

# Define variables ----
df$skills <- with(df, 0.64*m143 + 0.59*m144 + 0.57*m145 + 0.55*m146 + 0.53*m147 + 0.53*m148 + 0.53*m149 + 0.51*m150 + 0.47*m151)

df$emot <- with(df, 0.86*m152 + 0.86*m153 + 0.54*m154 + 0.46*m155 + 0.43*m156)

df$part <- with(df, 0.82*m157 + 0.64*m158 + 0.57*m159 + 0.55*m160 + 0.50*m161 + 0.45*m162)

df$perf <- with(df, 0.77*m163 + 0.68*m164 + 0.64*m165) 


#creating the other variables as part of the data field.
df$total.stress <- rowSums(df[, which(grepl("^s\\d{1,2}" , names(df)))], na.rm = TRUE)
df$total.sleep <- rowSums(df[, which(grepl("^b\\d{1,2}" , names(df)))], na.rm = TRUE)
#figure out which one is more appropriate to use, without factor loadings
df$total.eng <-  apply(df[, which(grepl("^m.*", names(df)))], 1, sum,  na.rm = TRUE)
#or with factor loadings - check article and include in methods if using with factor loadings
df$total.eng <- with(df, skills + emot + perf + part)

#Create total.exercise (this is not the weighted sum value - run weighted sum and do relavent analyses again)(rename total.e with something?- total.e.notweighted)
df$total.exercise.notweighted <- df$e195+df$e196+df$e197
### First calculate the total exercise score using weighted sum 
df$total.exercise <- with(df, 9*e195 + 6*e196 + 3*e197)


#Subset data into training and testing sets ----
set.seed(1014)
trainIndex <- sample(1:nrow(df), #Sample the row indices
                     size = floor((2/3)*nrow(df)), #take 2/3rds for the training set
                     replace = FALSE) #ensure the index is unique

dftrain <- df[trainIndex, ]

dftest <- df[-trainIndex, ]

### fit random forest using different numbers of variables in each tree ----
set.seed(2010)

# Create containers for the results
rfError <- data.frame(outBag_error = rep(NA, 6),
                      pred_error = rep(NA, 6),
                      mtry = 1:6)
# Loop thorugh each possible number of predictors in each tree and 
# calculate performance of the model
for(mtry in rfError$mtry){
  rftmp <- randomForest(total.eng ~ total.sleep +
                        total.stress + 
                        total.exercise + 
                        ethnicity + 
                        gender + 
                        age, 
                      mtry = mtry,
                      data = dftrain, 
                      na.action = na.omit)
  
  #Out of bag performance
  rfError$outBag_error[mtry] <- rftmp$mse[500]
  
  # test set performance
  pred <- predict(rftmp, dftest) #Get predicted values
  rfError$pred_error[mtry] <- mean((dftest$total.eng - pred)^2) #calculate MSE
}

## Long form for plotting two lines
rfErrorl <- gather(rfError, Error_type, MSE, -mtry)

ggplot(rfErrorl, aes(x = mtry, y = MSE)) + 
  geom_point(aes(color = Error_type)) +
  geom_line(aes(color = Error_type))
# the fact that the optimal number of vars for each split is 1 means that there are no interactions


#Select number of variables (mtry) based on minimum MSE of prediction and fit on full data ----

rf1 <- randomForest(total.eng ~ total.sleep +
                      total.stress + 
                      total.exercise + 
                      ethnicity + 
                      gender + 
                      age, 
                    mtry = 1,
                    data = df, 
                    na.action = na.omit)

#check for error convergence
plot(rf1) 

### This gives the relative importance of each variable
rf1$importance
