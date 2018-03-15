library(memisc)
library(reshape2)
library(plyr)
library(dplyr)
library(ggplot2)

df <- as.data.set(spss.system.file("./Rcode/Data/College coping data (complete).sav"))
df <- as.data.frame(df)

#reshape to long form
df$ID <- rownames(df)
dfl <- melt(df, id = c("ID", "class", "ethnicity", "gender", "age"))

#just s, b and m
dfl.bm <- dfl[which(grepl("s|b|m", dfl$variable)), ]

#Create vartype variable indicating the type of variable by pulling the first letter
#from the variable name
dfl.bm$vartype <- gsub("^(\\w{1})\\d+$", "\\1", dfl.bm$variable)

#create condensed variables by summing over the three variable types respectively
df.bm.sum <- dfl.bm %>%
  group_by(ID, class, ethnicity, gender, age, vartype) %>%
  summarise(total = sum(value, na.rm = TRUE))#Some NA's in the data, you should know what these mean

#reshape to wide format
df.sbm.w <- reshape(as.data.frame(df.bm.sum), 
                   timevar = "vartype", 
                   times = c("s", "b", "m"), 
                   idvar = c("ID", "class", "ethnicity", "gender", "age"), 
                   direction = "wide")

#Simple plot of the relationship between B and M
ggplot(df.sbm.w, aes(x = total.s, y = total.b)) + 
  geom_point() + 
  geom_smooth(method = "lm")

# correlation of B and M
cbm <- cor(df.sbm.w$total.s, df.sbm.w$total.b, use = "pairwise.complete.obs")

# Basic model with interaction
m1 <- lm(total.m ~ total.s + total.b + total.s:total.b, data = df.sbm.w)
#Create summary of the model for evaluation
sm1 <- summary(m1)
# View the summary
print(sm1) #nothing significant

#View histogram of B to see where potential cutpoints might be
hist(df.bm.w$total.b)

#looks pretty normal, can use quartiles to create 4 categories
cuts <- quantile(x = df.sbm.w$total.b, probs = c(0.33, 0.66))

#remember, you can learn more about a function by typing a ? before the function:
?quantile

#Create a new variable with the cuts using logic
df.sbm.w$bCat <- with(df.sbm.w, ifelse(total.b < cuts[1], "Low",
                       ifelse(total.b >= cuts[1] & total.b < cuts[2], "Moderate",
                              ifelse(total.b >= cuts[2], "High", NA))))

#set the factor levels so we can control which category is the "reference"
df.sbm.w$bCat <- factor(df.sbm.w$bCat, levels = c("Low", "Moderate", "High"))

#New model with dichotomized  B variable
m2 <- lm(total.m ~ total.s + bCat + total.s:bCat, data = df.sbm.w)
#Create summary of the model for evaluation
sm2 <- summary(m2)
# View the summary
print(sm2) #the highest category of B has a significant difference in mean S than the lowest
 # the second highest is 'almost' significant ;)

#visualize the results
ggplot(df.bm.w, aes(x = bCat, y = total.s)) + 
  geom_boxplot()

###################################################################################################

#Creating factors for Academic Engagment variable ----
df$skills <- with(df, 0.64*m143 + 0.59*m144 + 0.57*m145 + 0.55*m146 + 0.53*m147 + 0.53*m148 + 0.53*m149 + 0.51*m150 + 0.47*m151)

df$emot <- with(df, 0.86*m152 + 0.86*m153 + 0.54*m154 + 0.46*m155 + 0.43*m156)

df$part <- with(df, 0.82*m157 + 0.64*m158 + 0.57*m159 + 0.55*m160 + 0.50*m161 + 0.45*m162)

df$perf <- with(df, 0.77*m163 + 0.68*m164 + 0.64*m165) 

#creating the other variables as part of the data field
df$total.s <- rowSums(df[, which(grepl("^s\\d{1,2}" , names(df)))], na.rm = TRUE)
df$total.b <- rowSums(df[, which(grepl("^b\\d{1,2}" , names(df)))], na.rm = TRUE)



#Simple plot of the relationship between s and skills
ggplot(df, aes(x = total.b, y = perf)) + 
  geom_point() + 
  geom_smooth(method = "lm")

# correlation of variables with academic eng factors
cbskills <- cor(df$total.b, df$skills, use = "pairwise.complete.obs")

# Factored (academic engagement) model with interaction
m4 <- lm(emot ~ total.s + total.b + total.s:total.b, data = df)
#Create summary of the model for evaluation
sm4 <- summary(m4)
# View the summary
print(sm4) #nothing significant

m3 <- lm(skills ~ total.s + total.b + total.s:total.b, data = df)
#Create summary of the model for evaluation
sm3 <- summary(m3)
# View the summary
print(sm3) #nothing significant

m5 <- lm(part ~ total.s + total.b + total.s:total.b, data = df)
#Create summary of the model for evaluation
sm5 <- summary(m5)
# View the summary
print(sm5) #nothing significant

m6 <- lm(perf ~ total.s + total.b + total.s:total.b, data = df)
#Create summary of the model for evaluation
sm6 <- summary(m6)
# View the summary
print(sm6) #nothing significant


write.csv(sm3$coef, file = "Model3Results.csv")

############################################################################################

#creating plot to determine interaction effect on m3:

ggplot(df, aes(y = skills, x = total.b)) + #This calls the data and sets the x and y axes
  geom_point(aes(size = total.s)) + #This adds points to the plot
  ylab("Skills") + #These change the labels of the x and y axes (by default they get the name of your variable which may not be very useful to an outside reader)
  xlab("Total.b") + #replace this with something intelligible
  scale_size_continuous("Total.s") +#this changes the name of the size legend, make it make sense
  theme_bw() #This adds a simplified theme 

#Try again with color instead of size
ggplot(df, aes(y = skills, x = total.b)) + #This calls the data and sets the x and y axes
  geom_point(aes(color = total.s), size = 2) + #This adds points to the plot with the color mapped to total.s
  ylab("Skills") + #These change the labels of the x and y axes (by default they get the name of your variable which may not be very useful to an outside reader)
  xlab("Total.b") + #replace this with something intelligible
  scale_color_continuous("Total.s") +#this changes the name of the size legend, make it make sense
  theme_bw() #This adds a simplified theme 

#Both of these plots indicate to me that the interaction is slightly attenuating

#lets try reversing the predictors
ggplot(df, aes(y = skills, x = total.s)) + #This calls the data and sets the x and y axes
  geom_point(aes(size = total.b)) + #This adds points to the plot
  ylab("Skills") + #These change the labels of the x and y axes (by default they get the name of your variable which may not be very useful to an outside reader)
  xlab("Total.s") + #replace this with something intelligible
  scale_size_continuous("Total.b") +#this changes the name of the size legend, make it make sense
  theme_bw() #This adds a simplified theme 

ggplot(df, aes(y = skills, x = total.s)) + #This calls the data and sets the x and y axes
  geom_point(aes(color = total.b), size = 2) + #This adds points to the plot
  ylab("Skills") + #These change the labels of the x and y axes (by default they get the name of your variable which may not be very useful to an outside reader)
  xlab("Total.s") + #replace this with something intelligible
  scale_color_continuous("Total.b") +#this changes the name of the size legend, make it make sense
  theme_bw() #This adds a simplified theme 


#Sometimes is it helpful to have the response as the third dimension
ggplot(df, aes(y = total.b, x = total.s)) + 
  geom_point(aes(size = skills)) +
  theme_bw()


ggplot(df, aes(y = total.b, x = total.s)) + 
  geom_point(aes(color = skills), size = 2) +
  theme_bw()

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


#here is a prediciton function so you can see how it is done
predictM3 <- function(total.s, total.b){
  skills <- -28.72 + 0.340955*total.s + 1.032546*total.b + -0.007551*total.s*total.b
  return(skills)
}

#find the range of values
summary(df[, c("total.s", "total.b", "skills")])


predictM3(total.s = 58, total.b = 22)#with both low

#Looking at the outlier: Observation #33
df[, 33]

############################################################################################
#Interaction effect with exercise ----

df$total.e <- df$e195+df$e196+df$e197

# Factored (academic engagement) model with interaction for exercise
m4e <- lm(emot ~ total.s + total.e + total.s:total.e, data = df)
#Create summary of the model for evaluation
sm4e <- summary(m4e)
# View the summary
print(sm4e)

m3e <- lm(skills ~ total.s + total.e + total.s:total.e, data = df)
sm3e <- summary(m3e)
print(sm3e)

m5e <- lm(part ~ total.s + total.e + total.s:total.e, data = df)
sm5e <- summary(m5e)
print(sm5e)

m6e <- lm(perf ~ total.s + total.e + total.s:total.e, data = df)
sm6e <- summary(m6e)
print(sm6e)


df$total.m <-  apply(df[, which(grepl("^m.*", names(df)))], 1, sum,  na.rm = TRUE)

#Simple plot of the relationship between s and e
ggplot(df, aes(x = total.s, y = total.e)) + 
  geom_point() + 
  geom_smooth(method = "lm")

# correlation of s and e
cse <- cor(df$total.s, df$total.e, use = "pairwise.complete.obs")


#Simple plot of the relationship between s and m
ggplot(df, aes(x = total.s, y = total.m)) + 
  geom_point() + 
  geom_smooth(method = "lm")

# correlation of s and m
csm <- cor(df$total.s, df$total.m, use = "pairwise.complete.obs")


#Simple plot of the relationship between e and m
ggplot(df, aes(x = total.e, y = total.m)) + 
  geom_point() + 
  geom_smooth(method = "lm")

# correlation of e and m
cem <- cor(df$total.e, df$total.m, use = "pairwise.complete.obs")

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
library(gmodels)
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
library(gmodels)
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
plot(sleepvskills)

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
plot(sleepvperf)
summary(sleepvperf)

### Test sleep vs part
ggplot(df[-c(33), ], aes(x = total.b, y = part)) + 
  geom_point() + 
  geom_smooth(method = "lm")

sleepvpart <- lm(part ~ total.b, data = df[-33, ])
plot(sleepvpart)
summary(sleepvpart)

df$total.eng <- with(df, skills + emot + perf + part)

### Test sleep vs total engagement
ggplot(df[-c(33), ], aes(x = total.b, y = total.eng)) + 
  geom_point() + 
  geom_smooth(method = "lm")

sleepveng <- lm(total.eng ~ total.b, data = df[-33, ])
plot(sleepveng)
summary(sleepveng)


##########################################################################

### Test relationship between academic engagement and exercise using linear model ----
### First calculate the total exercise score using weighted sum
df$total.e <- with(df, 9*e195 + 6*e196 + 3*e197)

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

### First calculate the total exercise score using weighted sum
df$total.e <- with(df, 9*e195 + 6*e196 + 3*e197)


#Simple plot of the relationship between b and e
ggplot(df, aes(x = total.b, y = total.e)) + 
  geom_point() + 
  geom_smooth(method = "lm")

# correlation of b and e
cbe <- cor(df$total.b, df$total.e, use = "pairwise.complete.obs")

#################################################################

# making a new total b data point in the data base df in order to reverse the scores ----
### as indicated below and then reversing the sleep hygiene data so a high score 
### indicates good hygiene instead of indicating poor hygiene
df$newTotal.b <-  1*(52- df$total.b)

####################################################################

#running correlation of sleep hygiene and exercise again with new reverse coded sleep hygiene variable (newTotal.b)
cbe2 <- cor(df$newTotal.b, df$total.e, use = "pairwise.complete.obs")


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

### Test for relationship between engagement and sleep (after reversing sleep data - newTotal.b) using correlation instead of linear model ----

df$total.eng <- with(df, skills + emot + perf + part)

### Should remove 33 as it has high leverage as an outlier: could use: dflim <- df[-33, ] to remove it from the whole data set or eliminate it in each individual analysis(follow each data point with [-33}.  I will eliminate it individually for now and then change previous analysis if I need to. Actually may decide to leave it in since it may theoretically fit the model: low skills, bad sleep hygiene, high stress

#Simple plot of the relationship between sleep (newTotal.b) and total engagement (total.eng)
ggplot(df[-c(33), ], aes(x = newTotal.b, y = total.eng)) + 
  geom_point() + 
  geom_smooth(method = "lm")

# correlation of b and m (with item #33 removed)
cbm2 <- cor(df$newTotal.b[-33], df$total.eng[-33], use = "pairwise.complete.obs")

# correlation of b and m
cbm <- cor(df$newTotal.b, df$total.eng, use = "pairwise.complete.obs")

### Test sleep vs skills - correlation
ggplot(df[-33, ], aes(x = newTotal.b, y = skills)) + 
  geom_point() + 
  geom_smooth(method = "lm")

cbskills <- cor(df$newTotal.b, df$skills, use = "pairwise.complete.obs")

#with 33 removed
cbskills2 <- cor(df$newTotal.b[-33], df$skills[-33], use = "pairwise.complete.obs")

### Test sleep vs emot
ggplot(df, aes(x = newTotal.b, y = emot)) + 
  geom_point() + 
  geom_smooth(method = "lm")

cbemot <- cor(df$newTotal.b, df$emot, use = "pairwise.complete.obs")

#with item #33 removed
cbemot2 <- cor(df$newTotal.b[-33], df$emot[-33], use = "pairwise.complete.obs")

### Test sleep vs perf
ggplot(df[-c(33), ], aes(x = newTotal.b, y = perf)) + 
  geom_point() + 
  geom_smooth(method = "lm")

# May want to remove observation 183 because it is a low outlier
cbperf <- cor(df$newTotal.b, df$perf, use = "pairwise.complete.obs")

#with item 33 removed
cbperf2 <- cor(df$newTotal.b[-c(33)], df$perf[-c(33)], use = "pairwise.complete.obs")

### Test sleep vs part
ggplot(df[-c(33), ], aes(x = newTotal.b, y = part)) + 
  geom_point() + 
  geom_smooth(method = "lm")

cbpart <- cor(df$newTotal.b, df$part, use = "pairwise.complete.obs")

#with item #33 removed
cbpart2 <- cor(df$newTotal.b[-33], df$part[-33], use = "pairwise.complete.obs")


##########################################################################

### Test relationship between academic engagement and exercise using correlation versus linear model ----
### First calculate the total exercise score using weighted sum
df$total.e <- with(df, 9*e195 + 6*e196 + 3*e197)

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

cem <- cor(df$total.e[-33], df$total.eng[-33], use = "pairwise.complete.obs")

#################################################################