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
# Recode stress variables, set 2 = 0 and 1 = 1 (because they had coded 2=no incidence of the stressor)
df[, which(grepl("^s\\d{1,2}" , names(df)))] <- apply(df[, which(grepl("^s\\d{1,2}" , names(df)))],
                                                      c(1, 2),
                                                      function(x) if(!is.na(x) & x == 2) x = 0 else x)
df$total.sleep <- rowSums(df[, which(grepl("^b\\d{1,2}" , names(df)))], na.rm = TRUE)
#figure out which one is more appropriate to use, without factor loadings
df$total.eng <-  apply(df[, which(grepl("^m.*", names(df)))], 1, sum,  na.rm = TRUE)
#or with factor loadings - check article and include in methods if using with factor loadings
df$total.eng <- with(df, skills + emot + perf + part)

#Create total.exercise (this is not the weighted sum value - run weighted sum and do relavent analyses again)(rename total.e with something?- total.e.notweighted)
df$total.exercise.notweighted <- df$e195+df$e196+df$e197
### First calculate the total exercise score using weighted sum 
df$total.exercise <- with(df, 9*e195 + 6*e196 + 3*e197)
df <- df[-33, ]

ind <- complete.cases(df[, c("total.eng",
                             "total.sleep",
                             "total.stress", 
                             "total.exercise", 
                             "ethnicity",
                             "gender", 
                             "age",
                             "class",
                             "skills",
                             "emot",
                             "part",
                             "perf")])
df <- df[ind, ]

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
rfError <- data.frame(outBag_error = rep(NA, 7),
                      pred_error = rep(NA, 7),
                      mtry = 1:7)

# Loop through each possible number of predictors in each tree and 
# calculate performance of the model
for(mtry in rfError$mtry){
  rftmp <- randomForest(total.eng ~ total.sleep +
                          total.stress + 
                          total.exercise + 
                          ethnicity + 
                          gender + 
                          age +
                          class, 
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


# to avoid instabiliuty of importance measures we fit forest many times and average importances
source("RCode/RandomForestFittingFunction.R")

eng <- skill <- emo <- part <- perf <- vector(mode = "list", length = 500)
for(i in 1:500){
  eng[[i]] <- rforest.imp(x = "total.eng", mtry = 2, iter = i)
  skill[[i]] <- rforest.imp(x = "skills", mtry = 2, iter = i)
  emo[[i]] <- rforest.imp(x = "emot", mtry = 2, iter = i)
  part[[i]] <- rforest.imp(x = "part", mtry = 2, iter = i)
  perf[[i]] <- rforest.imp(x = "perf", mtry = 2, iter = i)
}

Eng <- do.call(rbind, eng) %>%
  group_by(Variable, measure) %>%
  summarize(avg = round(mean(value), 2), 
            SD = round(sd(value), 2))  

Skill  <- do.call(rbind, skill) %>%
  group_by(Variable, measure) %>%
  summarize(avg = round(mean(value), 2), 
            SD = round(sd(value), 2)) 

Emot  <- do.call(rbind, emo) %>%
  group_by(Variable, measure) %>%
  summarize(avg = round(mean(value), 2), 
            SD = round(sd(value), 2)) 

Part  <- do.call(rbind, part) %>%
  group_by(Variable, measure) %>%
  summarize(avg = round(mean(value), 2), 
            SD = round(sd(value), 2)) 

Perf <- do.call(rbind, perf) %>%
  group_by(Variable, measure) %>%
  summarize(avg = round(mean(value), 2), 
            SD = round(sd(value), 2)) 

### Create table of results ----
RFres <- left_join(Eng, Skill, by  = c("Variable", "measure")) %>%
  left_join(Emot, by  = c("Variable", "measure")) %>%
  left_join(Part, by  = c("Variable", "measure")) %>%
  left_join(Perf, by  = c("Variable", "measure")) 

RFres$measure <- ifelse(RFres$measure == "%IncMSE",
                        "% Increase MSE",
                        "Increase in RSS")

RFres$Variable <- gsub("total\\.", "", RFres$Variable)
RFres$Variable <- paste0(toupper(gsub("^([a-z])([a-z]*)", "\\1", RFres$Variable)),
                         gsub("^([a-z])([a-z]*)", "\\2", RFres$Variable))

library(ReporteRs)
RFtbl <- FlexTable(RFres, 
                   header.columns = FALSE, 
                   add.rownames = FALSE,
                   header.cell.props = cellProperties(padding = 2),
                   body.cell.props = cellProperties(padding = 2))
RFtbl <- addHeaderRow(RFtbl, 
                      value = c("",
                                "",
                                "Academic Eng",
                                "Skills",
                                "Emotional",
                                "Participation",
                                "Performance"),
                      colspan = c(1, 1, 2, 2, 2, 2, 2))
RFtbl <- addHeaderRow(RFtbl,
                      value = c("Variable", 
                                "Importance Meas.",
                                rep(c("Mean", "Stdev"), 5)))

RFtbl <- spanFlexTableRows(RFtbl, 
                           j = 1,
                           runs = RFres$Variable)

doc <- docx()
doc <- addFlexTable(doc, RFtbl)
writeDoc(doc, "Random_Forest_Comnined_Result_Table.docx")

library(party)
rf2 <- cforest(total.eng ~ total.sleep +
                      total.stress + 
                      total.exercise + 
                      ethnicity + 
                      gender + 
                      age +
                      class, 
                    controls = cforest_unbiased(mtry = 2, ntree = 1000),
                    data = df)
 rf2
 
 plot(rf2)
 
 varimp(rf2)
 