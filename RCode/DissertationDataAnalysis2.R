library(memisc)
library(reshape2)
library(plyr)
library(dplyr)
library(ggplot2)
library(cowplot)

df <- as.data.set(spss.system.file("./Rcode/Data/College coping data (complete).sav"))
df <- as.data.frame(df)

#reshape to long form
df$ID <- rownames(df)

#remember, you can learn more about a function by typing a ? before the function:
# ?quantile

###################################################################################################

#Creating factors for Academic Engagment variable ----
df$skills <- with(df, 0.64*m143 + 0.59*m144 + 0.57*m145 + 0.55*m146 + 0.53*m147 + 0.53*m148 + 0.53*m149 + 0.51*m150 + 0.47*m151)

df$emot <- with(df, 0.86*m152 + 0.86*m153 + 0.54*m154 + 0.46*m155 + 0.43*m156)

df$part <- with(df, 0.82*m157 + 0.64*m158 + 0.57*m159 + 0.55*m160 + 0.50*m161 + 0.45*m162)

df$perf <- with(df, 0.77*m163 + 0.68*m164 + 0.64*m165) 

#creating the other variables as part of the data field
df$total.s <- rowSums(df[, which(grepl("^s\\d{1,2}" , names(df)))], na.rm = TRUE)
df$total.b <- rowSums(df[, which(grepl("^b\\d{1,2}" , names(df)))], na.rm = TRUE)


#Simple plot of the relationship between stress (s) and skills AE
ggplot(df, aes(x = total.s, y = skills)) + 
  # geom_point() + 
  geom_smooth(method = "lm") +
  geom_text(aes(label = ID)) # ID 33 is an outlier here (shows up in cooks distance too)


# correlation of variables with academic eng factors
cbskills_noOutlier <- cor(df$total.b[-33], df$skills[-33], use = "pairwise.complete.obs")
# the no-outlier version has higher correlation
cbskills <- cor(df$total.b, df$skills, use = "pairwise.complete.obs")


# Factored (academic engagement) model with interaction ----
m4b <- lm(emot ~ total.s + total.b + total.s:total.b, data = df)
#Create summary of the model for evaluation
sm4b <- summary(m4b)
# View the summary
print(sm4b) #nothing significant

m3b <- lm(skills ~ total.s + total.b + total.s:total.b, data = df)
#Create summary of the model for evaluation
sm3b <- summary(m3b)
# View the summary
print(sm3b) ## ID 33 has a lot of leverage (use: plot(m3b))

m5b <- lm(part ~ total.s + total.b + total.s:total.b, data = df)
#Create summary of the model for evaluation
sm5b <- summary(m5b)
# View the summary
print(sm5b) #nothing significant

m6b <- lm(perf ~ total.s + total.b + total.s:total.b, data = df)
#Create summary of the model for evaluation
sm6b <- summary(m6b)
# View the summary
print(sm6b) #nothing significant


write.csv(sm3$coef, file = "Model3Results.csv")

############################################################################################

#creating plot to determine interaction effect on m3: ----

ggplot(df, aes(y = skills, x = total.b)) + #This calls the data and sets the x and y axes
  geom_point(aes(size = total.s)) + #This adds points to the plot
  ylab("Skills") + #These change the labels of the x and y axes (by default they get the name of your variable which may not be very useful to an outside reader)
  xlab("Total.b") + #replace this with something intelligible
  scale_size_continuous("Total.s") #this changes the name of the size legend, make it make sense

#Try again with color instead of size
ggplot(df, aes(y = skills, x = total.b)) + #This calls the data and sets the x and y axes
  geom_point(aes(color = total.s), size = 2) + #This adds points to the plot with the color mapped to total.s
  ylab("Skills") + #These change the labels of the x and y axes (by default they get the name of your variable which may not be very useful to an outside reader)
  xlab("Total.b") + #replace this with something intelligible
  scale_color_continuous("Total.s") #this changes the name of the size legend, make it make sense

#Both of these plots indicate to me that the interaction is slightly attenuating

#lets try reversing the predictors
ggplot(df, aes(y = skills, x = total.s)) + #This calls the data and sets the x and y axes
  geom_point(aes(size = total.b)) + #This adds points to the plot
  ylab("Skills") + #These change the labels of the x and y axes (by default they get the name of your variable which may not be very useful to an outside reader)
  xlab("Total.s") + #replace this with something intelligible
  scale_size_continuous("Total.b") #this changes the name of the size legend, make it make sense

ggplot(df, aes(y = skills, x = total.s)) + #This calls the data and sets the x and y axes
  geom_point(aes(color = total.b), size = 2) + #This adds points to the plot
  ylab("Skills") + #These change the labels of the x and y axes (by default they get the name of your variable which may not be very useful to an outside reader)
  xlab("Total.s") + #replace this with something intelligible
  scale_color_continuous("Total.b") #this changes the name of the size legend, make it make sense


#Sometimes is it helpful to have the response as the third dimension
ggplot(df, aes(y = total.b, x = total.s)) + 
  geom_point(aes(size = skills)) 


ggplot(df, aes(y = total.b, x = total.s)) + 
  geom_point(aes(color = skills), size = 2) 

#Hmmm, there is an outlying value that may be driving the attenuation.  
#I am suspicious that the interaction might dissappear or change signs if this data point is removed.


#refit the model with only skills greater than 5
#First find the outlier
ind <- which(df$skills < 5) #observation 33

#Refit the model after removing the outlier
m3New <- lm(skills ~ total.s + total.b + total.s:total.b, data = df[-ind, ])

summary(m3New)

#As I suspected the removal of this point made the interaction non-significant
#I would suggest re-running the other models without this observation since it 
#appears to be an outlier.  Or indeed verify that it should be included.


#here is a prediction function so you can see how it is done
predictM3 <- function(total.s, total.b){
  skills <- -28.72 + 0.340955*total.s + 1.032546*total.b + -0.007551*total.s*total.b
  return(skills)
} #there is also a predict() function in R that does this for any model

#find the range of values
summary(df[, c("total.s", "total.b", "skills")])


predictM3(total.s = 58, total.b = 22)#with both low

#Looking at the outlier: Observation #33
df[, 33]

############################################################################################
#Interaction effect with exercise ----

#Create total.e (this is not the weighted sum value - run weighted sum and do relavent analyses again)(rename total.e with something?- total.e.notweighted)
df$total.e.notweighted <- df$e195+df$e196+df$e197

### First calculate the total exercise score using weighted sum 
df$total.e <- with(df, 9*e195 + 6*e196 + 3*e197)

# Factored (academic engagement) model with interaction for exercise
m4e <- lm(emot ~ total.s + total.e + total.s:total.e, data = df)
#Create summary of the model for evaluation
sm4e <- summary(m4e)
# View the summary
print(sm4e)

m3e <- lm(skills ~ total.s + total.e + total.s:total.e, data = df)
sm3e <- summary(m3e)
print(sm3e) #obs 33 poses issue here

m5e <- lm(part ~ total.s + total.e + total.s:total.e, data = df)
sm5e <- summary(m5e) # obs 156 poses issue here
print(sm5e)

m6e <- lm(perf ~ total.s + total.e + total.s:total.e, data = df)
sm6e <- summary(m6e) ## 33 and 183 pose issues here
print(sm6e)


df$total.m <-  apply(df[, which(grepl("^m.*", names(df)))], 1, sum,  na.rm = TRUE)

#Simple plot of the relationship between s and e
ggplot(df, aes(x = total.s, y = total.e)) + 
  geom_text(aes(label = ID)) + 
  geom_smooth(method = "lm")

# correlation of s and e
cse <- cor(df$total.s, df$total.e, use = "pairwise.complete.obs")


#Simple plot of the relationship between s and m
ggplot(df, aes(x = total.s, y = total.m)) + 
  geom_text(aes(label = ID)) + 
  geom_smooth(method = "lm")

# correlation of s and m
csm <- cor(df$total.s, df$total.m, use = "pairwise.complete.obs")


#Simple plot of the relationship between e and m
ggplot(df, aes(x = total.e, y = total.m)) + 
  geom_text(aes(label = ID)) + 
  geom_smooth(method = "lm")

# correlation of e and m
cem <- cor(df$total.e, df$total.m, use = "pairwise.complete.obs")
cem_noOutlier <- cor(df$total.e[-33], df$total.m[-33], use = "pairwise.complete.obs")

####################################################################

#Running means of age demographic against AE skills to see if I need to contrast code ----
#model for age -> m age -> mage (name of model), skills=skl
mageskl <- lm(skills ~ -1 + age, data = df)
summary(mageskl)

#Running means of age demographic against AE emot to see if I need to contrast code
mageemot <- lm(emot ~ -1 + age, data = df)
summary(mageemot)

#Running means of age demographic against AE part to see if I need to contrast code
magepart <- lm(part ~ -1 + age, data = df)
summary(magepart)

#Running means of age demographic against AE perf to see if I need to contrast code
mageperf <- lm(perf ~ -1 + age, data = df)
summary(mageperf)

#####################################################################

#Running means of ethnicity demographic against AE skills to see if I need to contrast code
#model for ethnicity -> m ethnicity -> meth (name of model), skills=skl
methskl <- lm(skills ~ -1 + ethnicity, data = df)
summary(methskl)

#Running means of ethnicity demographic against AE emot to see if I need to contrast code
methemot <- lm(emot ~ -1 + ethnicity, data = df)
summary(methemot)

#Running means of ethnicity demographic against AE part to see if I need to contrast code
methpart <- lm(part ~ -1 + ethnicity, data = df)
summary(methpart)

#Running means of ethnicity demographic against AE perf to see if I need to contrast code
methperf <- lm(perf ~ -1 + ethnicity, data = df)
summary(methperf)

######################################################################

#Running means of class demographic against AE skills to see if I need to contrast code----
#model for class -> m class -> mclass (name of model), skills=skl
mclassskl <- lm(skills ~ -1 + class, data = df)
summary(mclassskl)

#Running means of class demographic against AE emot to see if I need to contrast code
mclassemot <- lm(emot ~ -1 + class, data = df)
summary(mclassemot)

#Running means of class demographic against AE part to see if I need to contrast code
mclasspart <- lm(part ~ -1 + class, data = df)
summary(mclasspart)

#Running means of class demographic against AE perf to see if I need to contrast code
mclassperf <- lm(perf ~ -1 + class, data = df)
summary(mclassperf)

########################################################################

#Running means of gender demographic against AE skills to see if I need to contrast code ----
#model for gender -> m gender -> mgend (name of model), skills=skl
mgendskl <- lm(skills ~ -1 + gender, data = df)
summary(mgendskl)

#Running means of gender demographic against AE emot to see if I need to contrast code
mgendemot <- lm(emot ~ -1 + gender, data = df)
summary(mgendemot)

#Running means of gender demographic against AE part to see if I need to contrast code
mgendpart <- lm(part ~ -1 + gender, data = df)
summary(mgendpart)

#Running means of gender demographic against AE perf to see if I need to contrast code
mgendperf <- lm(perf ~ -1 + gender, data = df)
summary(mgendperf)

########################################################################

#Running contrast code/general linear hypothesis testing for age and AE skills ----
library(gmodels)
mageskl <- lm(skills ~ -1 + age, data = df)
summary(mageskl)

### Create a matrices of contrasts with 1 contrast per line
### Here we create linear combinations of the means where 
### we subtract one mean from the other using 1 and -1 and
### 0 drops the mean from the test so we are only looking
### at pairwise comparisons

#compare 18-19 with 20-21
age.1819v2021 <- matrix(c(1, -1, 0, 0),
                        nrow = 1,
                        ncol = 4,
                        byrow = TRUE)

#compare 18-19 with 22-25
age.1819v2225 <- matrix(c(1, 0, -1, 0),
                        nrow = 1,
                        ncol = 4,
                        byrow = TRUE)

#compare 18-19 with average of two middle age categories
age.1819v2025 <- matrix(c(1, -1/2, -1/2, 0),
                        nrow = 1,
                        ncol = 4,
                        byrow = TRUE)

#compare 18-19 with oldest category
age.1819v31 <- matrix(c(1, 0, 0, -1),
                      nrow = 1,
                      ncol = 4,
                      byrow = TRUE)

#Compare average of 20-25 with oldest group 
age.31v2025 <- matrix(c(0, -1/2, -1/2, 1),
                      nrow = 1,
                      ncol = 4,
                      byrow = TRUE)

### Now we use tests of general linear hypotheses (glh) to 
### test for differences in the groups
glh.test(mageskl,
         cm = age.1819v2021)

glh.test(mageskl,
         cm = age.1819v2225)

glh.test(mageskl,
         cm = age.1819v2025)

glh.test(mageskl,
         cm = age.1819v31)

glh.test(mageskl,
         cm = age.31v2025)

###############################################################################

#Running contrast code/general linear hypothesis testing for age and AE emot----
mageemot <- lm(emot ~ -1 + age, data = df)
summary(mageemot)

### Create a matrices of contrasts with 1 contrast per line
### Here we create linear combinations of the means where 
### we subtract one mean from the other using 1 and -1 and
### 0 drops the mean from the test so we are only looking
### at pairwise comparisons

#compare 18-19 with 20-21
age.1819v2021 <- matrix(c(1, -1, 0, 0),
                        nrow = 1,
                        ncol = 4,
                        byrow = TRUE)

#compare 18-19 with 22-25
age.1819v2225 <- matrix(c(1, 0, -1, 0),
                        nrow = 1,
                        ncol = 4,
                        byrow = TRUE)

#compare 18-19 with oldest category
age.1819v31 <- matrix(c(1, 0, 0, -1),
                      nrow = 1,
                      ncol = 4,
                      byrow = TRUE)

#compare average of 18-21 with 22-25
age.1821v2225 <- matrix(c(-1/2, -1/2, 1, 0),
                        nrow = 1,
                        ncol = 4,
                        byrow = TRUE)

#Compare average of 18-21 with oldest group 
age.31v1821 <- matrix(c(-1/2, -1/2, 0, 1),
                      nrow = 1,
                      ncol = 4,
                      byrow = TRUE)

### Now we use tests of general linear hypotheses (glh) to 
### test for differences in the groups
glh.test(mageemot,
         cm = age.1819v2021)

glh.test(mageemot,
         cm = age.1819v2225)

glh.test(mageemot,
         cm = age.1819v31)

glh.test(mageemot,
         cm = age.1821v2225)

glh.test(mageemot,
         cm = age.31v1821)


##########################################################################

#Running contrast code/general linear hypothesis testing for age and AE part ----
magepart <- lm(part ~ -1 + age, data = df)
summary(magepart)

### Create a matrices of contrasts with 1 contrast per line
### Here we create linear combinations of the means where 
### we subtract one mean from the other using 1 and -1 and
### 0 drops the mean from the test so we are only looking
### at pairwise comparisons

#compare 18-19 with 20-21
age.1819v2021 <- matrix(c(1, -1, 0, 0),
                        nrow = 1,
                        ncol = 4,
                        byrow = TRUE)

#compare 18-19 with 22-25
age.1819v2225 <- matrix(c(1, 0, -1, 0),
                        nrow = 1,
                        ncol = 4,
                        byrow = TRUE)

#compare 18-19 with oldest category
age.1819v31 <- matrix(c(1, 0, 0, -1),
                      nrow = 1,
                      ncol = 4,
                      byrow = TRUE)

#compare average of 18-25 with oldest group
age.1825v31 <- matrix(c(-1/3, -1/3, -1/3, 1),
                        nrow = 1,
                        ncol = 4,
                        byrow = TRUE)


### Now we use tests of general linear hypotheses (glh) to 
### test for differences in the groups
glh.test(magepart,
         cm = age.1819v2021)

glh.test(magepart,
         cm = age.1819v2225)

glh.test(magepart,
         cm = age.1819v31)

glh.test(magepart,
         cm = age.1825v31)

###########################################################################

#Running contrast code/general linear hypothesis testing for age and AE perf ----
library(gmodels)
mageperf <- lm(perf ~ -1 + age, data = df)
summary(mageperf)

### Create a matrices of contrasts with 1 contrast per line
### Here we create linear combinations of the means where 
### we subtract one mean from the other using 1 and -1 and
### 0 drops the mean from the test so we are only looking
### at pairwise comparisons

#compare 18-19 with 20-21
age.1819v2021 <- matrix(c(1, -1, 0, 0),
                        nrow = 1,
                        ncol = 4,
                        byrow = TRUE)

#compare 18-19 with 22-25
age.1819v2225 <- matrix(c(1, 0, -1, 0),
                        nrow = 1,
                        ncol = 4,
                        byrow = TRUE)

#compare 18-19 with oldest category
age.1819v31 <- matrix(c(1, 0, 0, -1),
                      nrow = 1,
                      ncol = 4,
                      byrow = TRUE)

#compare 20-21 with 22-25
age.2021v2225 <- matrix(c(0, 1, -1, 0),
                      nrow = 1,
                      ncol = 4,
                      byrow = TRUE)

#compare 20-21 with oldest group
age.2021v31 <- matrix(c(0, 1, 0, -1),
                      nrow = 1,
                      ncol = 4,
                      byrow = TRUE)

#compare 22-25 with oldest group
age.2225v31 <- matrix(c(0, 1, -1, 0),
                      nrow = 1,
                      ncol = 4,
                      byrow = TRUE)

### Now we use tests of general linear hypotheses (glh) to 
### test for differences in the groups
glh.test(mageperf,
         cm = age.1819v2021)

glh.test(mageperf,
         cm = age.1819v2225)

glh.test(mageperf,
         cm = age.1819v31)

glh.test(mageperf,
         cm = age.2021v2225)

glh.test(mageperf,
         cm = age.2021v31)

glh.test(mageperf,
         cm = age.2225v31)

#######################################################################

### Test for relationship between engagement and sleep (total.b) using linear model ----

ggplot(df[-33, ], aes(x = total.b, y = skills)) + 
  geom_point() + 
  geom_smooth(method = "lm")

sleepvskills <- lm(skills ~ total.b, data = df)
plot(sleepvskills) ## 33 is a real issue here


### Definitely should remove 33 as it has high leverage as an outlier
sleepvskills <- lm(skills ~ total.b, data = df[-33, ])
plot(sleepvskills)
summary(sleepvskills)

### Test sleep vs emot
ggplot(df, aes(x = total.b, y = emot)) + 
  geom_point() + 
  geom_smooth(method = "lm")

sleepvemot <- lm(emot ~ total.b, data = df[-33, ])
plot(sleepvemot)
summary(sleepvemot)

### Test sleep vs perf
ggplot(df[-c(33, 183), ], aes(x = total.b, y = perf)) + 
  geom_point() + 
  geom_smooth(method = "lm")

sleepvperf <- lm(perf ~ total.b, data = df[-33, ])
plot(sleepvperf) #183 is an issue here
summary(sleepvperf)

### Test sleep vs part
ggplot(df[-c(33), ], aes(x = total.b, y = part)) + 
  geom_point() + 
  geom_smooth(method = "lm")

sleepvpart <- lm(part ~ total.b, data = df[-33, ])
plot(sleepvpart)
summary(sleepvpart)

#Check if I need this or is it redundant with total.m?
df$total.eng <- with(df, skills + emot + perf + part)

### Test sleep vs total engagement
ggplot(df[-c(33), ], aes(x = total.b, y = total.eng)) + 
  geom_point() + 
  geom_smooth(method = "lm")

sleepveng <- lm(total.eng ~ total.b, data = df[-33, ])
plot(sleepveng)
summary(sleepveng)


##########################################################################

### Test relationship between academic engagement and exercise using linear model (hypothesis 4) ---- 

ggplot(df[-33, ], aes(x = total.e, y = skills)) + 
  geom_point() + 
  geom_smooth(method = "lm")

### Test Exercise vs skills
sleepvskills <- lm(skills ~ total.e, data = df[-33, ])
plot(sleepvskills)
summary(sleepvskills)

### Test exercise vs emot
ggplot(df, aes(x = total.e, y = emot)) + 
  geom_point() + 
  geom_smooth(method = "lm")

exercisevemot <- lm(emot ~ total.e, data = df[-33, ])
plot(exercisevemot)
summary(exercisevemot)

### Test exercise vs perf
ggplot(df[-c(33), ], aes(x = total.e, y = perf)) + 
  geom_point() + 
  geom_smooth(method = "lm")

exercisevperf <- lm(perf ~ total.e, data = df[-33, ])
plot(exercisevperf)
summary(exercisevperf)

### Test exercise vs part
ggplot(df[-c(33), ], aes(x = total.e, y = part)) + 
  geom_point() + 
  geom_smooth(method = "lm")

exercisevpart <- lm(part ~ total.e, data = df[-33, ])
plot(exercisevpart)
summary(exercisevpart)


### Test exercise vs total engagement
ggplot(df[-c(33), ], aes(x = total.e, y = total.eng)) + 
  geom_point() + 
  geom_smooth(method = "lm")

exerciseveng <- lm(total.eng ~ total.e, data = df[-33, ])
plot(exerciseveng)
summary(exerciseveng)

#################################################################


#Simple plot of the relationship between b and e
ggplot(df, aes(x = total.b, y = total.e)) + 
  geom_point() + 
  geom_smooth(method = "lm")

# correlation of b and e
cbe <- cor(df$total.b, df$total.e, use = "pairwise.complete.obs")

#################################################################

# Not needed based on the way the data was collected - making a new total b data point in the data base df in order to reverse the scores ----
### as indicated below and then reversing the sleep hygiene data so a high score 
### indicates good hygiene instead of indicating poor hygiene

####################################################################

#Not needed (see above) - running correlation of sleep hygiene and exercise again with new reverse coded sleep hygiene variable (newTotal.b)

#####################################################################

#Doing a CFA to check factor loadings on Engagement Questionnaire with my data ----

library(lavaan)

HS.model <- ' Skills =~ m143 + m144 + m145 + m146 + m147 + m148 + m149 + m150 + m151
              Emotional =~ m152 + m153 + m154 + m155 + m156
              Part.int =~ m157 + m158 + m159 + m160 + m161 + m162
              Performance =~ m163 + m164 + m165 '

fit <- cfa(HS.model, data=df, std.lv=TRUE)

summary(fit, fit.measures=TRUE, standardized = TRUE)

#######################################################################

### Test for relationship between engagement and sleep using correlation instead of linear model (hypothesis 2) ----

df$total.eng <- with(df, skills + emot + perf + part)

### Should remove 33 as it has high leverage as an outlier: could use: dflim <- df[-33, ] to remove it from the whole data set or eliminate it in each individual analysis(follow each data point with [-33}.  I will eliminate it individually for now and then change previous analysis if I need to. Actually may decide to leave it in since it may theoretically fit the model: low skills, bad sleep hygiene, high stress

#Simple plot of the relationship between sleep and total engagement (total.eng)
ggplot(df[-c(33), ], aes(x = total.b, y = total.eng)) + 
  geom_point() + 
  geom_smooth(method = "lm")

# correlation of b and m (with item #33 removed)
cbm2 <- cor(df$total.b[-33], df$total.eng[-33], use = "pairwise.complete.obs")

# correlation of b and m
cbm <- cor(df$total.b, df$total.eng, use = "pairwise.complete.obs")

### Test sleep vs skills - correlation
ggplot(df[-33, ], aes(x = total.b, y = skills)) + 
  geom_point() + 
  geom_smooth(method = "lm")

cbskills <- cor(df$total.b, df$skills, use = "pairwise.complete.obs")

#with 33 removed
cbskills2 <- cor(df$total.b[-33], df$skills[-33], use = "pairwise.complete.obs")

### Test sleep vs emot
ggplot(df, aes(x = total.b, y = emot)) + 
  geom_point() + 
  geom_smooth(method = "lm")

cbemot <- cor(df$total.b, df$emot, use = "pairwise.complete.obs")

#with item #33 removed
cbemot2 <- cor(df$total.b[-33], df$emot[-33], use = "pairwise.complete.obs")

### Test sleep vs perf
ggplot(df[-c(33), ], aes(x = total.b, y = perf)) + 
  geom_point() + 
  geom_smooth(method = "lm")

# May want to remove observation 183 because it is a low outlier (check again but I don't think so)
cbperf <- cor(df$total.b, df$perf, use = "pairwise.complete.obs")

#with item 33 removed
cbperf2 <- cor(df$total.b[-c(33)], df$perf[-c(33)], use = "pairwise.complete.obs")

### Test sleep vs part
ggplot(df[-c(33), ], aes(x = total.b, y = part)) + 
  geom_point() + 
  geom_smooth(method = "lm")

cbpart <- cor(df$total.b, df$part, use = "pairwise.complete.obs")

#with item #33 removed
cbpart2 <- cor(df$total.b[-33], df$part[-33], use = "pairwise.complete.obs")


##########################################################################

### Test relationship between academic engagement and exercise using correlation versus linear model (hypothesis 4) ----

### Test Exercise vs skills - correlation
ggplot(df[-33, ], aes(x = total.e, y = skills)) + 
  geom_point() + 
  geom_smooth(method = "lm")

ceskills <- cor(df$total.e, df$skills, use = "pairwise.complete.obs")

#with no item33
ceskills2 <- cor(df$total.e[-33], df$skills[-33], use = "pairwise.complete.obs")

### Test exercise vs emot
ggplot(df, aes(x = total.e, y = emot)) + 
  geom_point() + 
  geom_smooth(method = "lm")

ceemot <- cor(df$total.e, df$emot, use = "pairwise.complete.obs")

#with no item 33
ceemot2 <- cor(df$total.e[-33], df$emot[-33], use = "pairwise.complete.obs")

### Test exercise vs perf
ggplot(df[-c(33), ], aes(x = total.e, y = perf)) + 
  geom_point() + 
  geom_smooth(method = "lm")

ceperf <- cor(df$total.e, df$perf, use = "pairwise.complete.obs")

#with no item 33
ceperf2 <- cor(df$total.e[-33], df$perf[-33], use = "pairwise.complete.obs")

### Test exercise vs part ----
ggplot(df[-c(33), ], aes(x = total.e, y = part)) + 
  geom_point() + 
  geom_smooth(method = "lm")

cepart <- cor(df$total.e, df$part, use = "pairwise.complete.obs")

#with no item 33
cepart2 <- cor(df$total.e[-33], df$part[-33], use = "pairwise.complete.obs")

### Test exercise vs total engagement
ggplot(df[-c(33), ], aes(x = total.e, y = total.eng)) + 
  geom_point() + 
  geom_smooth(method = "lm")

ceng <- cor(df$total.e[-33], df$total.eng[-33], use = "pairwise.complete.obs")

#############################################################################
### Test moderation of exercise on relationship between exercise and academic engagement using linear model/multiple regression (equation from article) (hypothesis 5) ----


###Effect of exercise on total engagement (eng)
exerciseveng <- lm(total.eng ~ total.e, data = df[-33, ])
plot(exerciseveng)
summary(exerciseveng)

###Effect of exercise on skills eng
exercisevskills <- lm(skills ~ total.e, data = df[-33, ])
plot(exercisevskills)
summary(exercisevskills)

###Effect of exercise on emot eng
exercisevemot <- lm(emot ~ total.e, data = df[-33, ])
plot(exercisevemot)
summary(exercisevemot)

###Effect of exercise on Part eng
exercisevpart <- lm(part ~ total.e, data = df[-33, ])
plot(exercisevpart)
summary(exercisevpart)

###Effect of exercise on Perf eng
exercisevperf <- lm(perf ~ total.e, data = df[-33, ])
plot(exercisevperf)
summary(exercisevperf)

