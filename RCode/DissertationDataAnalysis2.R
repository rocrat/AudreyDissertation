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

##########################################################################

# A reverse-coded SH total was not needed based on the way the data was collected - making a new total b data point in the data base df in order to reverse the scores ----
### as indicated below and then reversing the sleep hygiene data so a high score 
### indicates good hygiene instead of indicating poor hygiene
#Not needed (see above) - running correlation of sleep hygiene and exercise again with new reverse coded sleep hygiene variable (newTotal.b)

###################################################################################################

#In the data:
#m=academic engagement 
#b=sleep hygiene
#s=stress
#e=exercise

#Creating factors for Academic Engagement (Eng) variable ----
df$skills <- with(df, 0.64*m143 + 0.59*m144 + 0.57*m145 + 0.55*m146 + 0.53*m147 + 0.53*m148 + 0.53*m149 + 0.51*m150 + 0.47*m151)

df$emot <- with(df, 0.86*m152 + 0.86*m153 + 0.54*m154 + 0.46*m155 + 0.43*m156)

df$part <- with(df, 0.82*m157 + 0.64*m158 + 0.57*m159 + 0.55*m160 + 0.50*m161 + 0.45*m162)

df$perf <- with(df, 0.77*m163 + 0.68*m164 + 0.64*m165) 


#creating the other variables as part of the data field.
# Recode stress variables, set 2 = 0 and 1 = 1 (because they had coded 2=no incidence of the stressor)
df[, which(grepl("^s\\d{1,2}" , names(df)))] <- apply(df[, which(grepl("^s\\d{1,2}" , names(df)))],
                                                      c(1, 2),
                                                      function(x) if(!is.na(x) & x == 2) x = 0 else x)

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

#Mean scores of the variables
mean(df$total.stress)
sd(df$total.stress)

mean(df$total.sleep)
sd(df$total.sleep)

mean(df$total.eng)
sd(df$total.eng)
mean(df$total.eng, na.rm = TRUE)
sd(df$total.eng, na.rm = TRUE)

mean(df$skills)
sd(df$skills)

mean(df$emot)
sd(df$emot)

mean(df$part)
sd(df$part)
mean(df$part, na.rm = TRUE)
sd(df$part, na.rm = TRUE)

mean(df$perf)
sd(df$perf)

mean(df$total.exercise)
sd(df$total.exercise)


# Example equation of correlation of variables with academic eng factors (correlation of sleep hygiene and skills eng)
#csleep-skills_noOutlier <- cor(df$total.sleep[-33], df$skills[-33], use = "pairwise.complete.obs")
# the no-outlier version has higher correlation
#csleep-skills <- cor(df$total.sleep, df$skills, use = "pairwise.complete.obs")

##############################################################################################

#Demographic Analysis - done

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

################################################################################

#Check relationship between sleep hygiene and exercise to see if they should be included as one self-care variable (do so if significantly correlated)

#Simple plot of the relationship between sleep and exercise
ggplot(df, aes(x = total.sleep, y = total.exercise)) + 
  geom_point() + 
  geom_smooth(method = "lm")

# correlation of sleep and exercise
cbe <- cor(df$total.sleep, df$total.exercise, use = "pairwise.complete.obs")
cbe2 <- cor(df$total.sleep[-33], df$total.exercise[-33], use = "pairwise.complete.obs")
# add cor.test to get p-values
cbe2 <- cor.test(df$total.sleep[-33], df$total.exercise[-33], use = "pairwise.complete.obs")

#######################################################################################

#Hypothesis 1 - done

#Correlations between Stress and Academic Engagement (AE)/factors of AE

#Simple plot of the relationship between stress and engagement
ggplot(df, aes(x = total.stress, y = total.eng)) + 
  geom_text(aes(label = ID)) + 
  geom_smooth(method = "lm")

# correlation of stress and eng
cstressveng <- cor(df$total.stress, df$total.eng, use = "pairwise.complete.obs")
cstressveng2 <- cor.test(df$total.stress[-33], df$total.eng[-33], use = "pairwise.complete.obs")

#Simple plot of the relationship between stress (s) and skills AE
ggplot(df, aes(x = total.stress, y = skills)) + 
  # geom_point() + 
  geom_smooth(method = "lm") +
  geom_text(aes(label = ID)) # ID 33 is an outlier here (shows up in cooks distance too)

# correlation of stress and skills
cstressvskills <- cor(df$total.stress, df$skills, use = "pairwise.complete.obs")
cstressvskills2 <- cor.test(df$total.stress[-33], df$skills[-33], use = "pairwise.complete.obs")

#Simple plot of the relationship between stress (s) and emot AE
ggplot(df, aes(x = total.stress, y = emot)) + 
  # geom_point() + 
  geom_smooth(method = "lm") +
  geom_text(aes(label = ID)) 

# correlation of stress and emot
cstressvemot <- cor(df$total.stress, df$emot, use = "pairwise.complete.obs")
cstressvemot2 <- cor.test(df$total.stress[-33], df$emot[-33], use = "pairwise.complete.obs")

#Simple plot of the relationship between stress (s) and part AE
ggplot(df, aes(x = total.stress, y = part)) + 
  # geom_point() + 
  geom_smooth(method = "lm") +
  geom_text(aes(label = ID)) 

# correlation of stress and part
cstressvpart <- cor(df$total.stress, df$part, use = "pairwise.complete.obs")
cstressvpart2 <- cor.test(df$total.stress[-33], df$part[-33], use = "pairwise.complete.obs")

#Simple plot of the relationship between stress (s) and perf AE
ggplot(df, aes(x = total.stress, y = perf)) + 
  # geom_point() + 
  geom_smooth(method = "lm") +
  geom_text(aes(label = ID)) 

# correlation of stress and perf
cstressvperf <- cor(df$total.stress, df$perf, use = "pairwise.complete.obs")
cstressvperf2 <- cor.test(df$total.stress[-33], df$perf[-33], use = "pairwise.complete.obs")

#Correlations of stress with academic engagement show small significance for skills eng. So use skills for this analysis. 0.233
#Part and Perf were -.126 and .112, emot and total.eng were lower.

##############################################################################################

# Hypothesis 2 - done

# Correlation of sleep hygiene and academic engagement

### Test for relationship between engagement and sleep using correlation instead of linear model ----

### Should remove 33 as it has high leverage as an outlier: could use: dflim <- df[-33, ] to remove it from the whole data set or eliminate it in each individual analysis(follow each data point with [-33}.  I will eliminate it individually for now and then change previous analysis if I need to. Actually may decide to leave it in since it may theoretically fit the model: low skills, bad sleep hygiene, high stress

#Simple plot of the relationship between sleep and total engagement (total.eng)
ggplot(df[-c(33), ], aes(x = total.sleep, y = total.eng)) + 
  geom_point() + 
  geom_smooth(method = "lm")

# correlation of sleep and eng (with item #33 removed)
csleepveng2 <- cor.test(df$total.sleep[-33], df$total.eng[-33], use = "pairwise.complete.obs")

# correlation of sleep and eng
csleepveng <- cor(df$total.sleep, df$total.eng, use = "pairwise.complete.obs")

### Test sleep vs skills - correlation
ggplot(df[-33, ], aes(x = total.sleep, y = skills)) + 
  geom_point() + 
  geom_smooth(method = "lm")

csleepvskills <- cor(df$total.sleep, df$skills, use = "pairwise.complete.obs")

#with 33 removed
csleepvskills2 <- cor.test(df$total.sleep[-33], df$skills[-33], use = "pairwise.complete.obs")

### Test sleep vs emot
ggplot(df, aes(x = total.sleep, y = emot)) + 
  geom_point() + 
  geom_smooth(method = "lm")

csleepvemot <- cor(df$total.sleep, df$emot, use = "pairwise.complete.obs")

#with item #33 removed
csleepvemot2 <- cor.test(df$total.sleep[-33], df$emot[-33], use = "pairwise.complete.obs")

### Test sleep vs perf
ggplot(df[-c(33), ], aes(x = total.sleep, y = perf)) + 
  geom_point() + 
  geom_smooth(method = "lm")

# May want to remove observation 183 because it is a low outlier (check again but I don't think so)
csleepvperf <- cor(df$total.sleep, df$perf, use = "pairwise.complete.obs")

#with item 33 removed
csleepvperf2 <- cor.test(df$total.sleep[-c(33)], df$perf[-c(33)], use = "pairwise.complete.obs")

### Test sleep vs part
ggplot(df[-c(33), ], aes(x = total.sleep, y = part)) + 
  geom_point() + 
  geom_smooth(method = "lm")

csleepvpart <- cor(df$total.sleep, df$part, use = "pairwise.complete.obs")

#with item #33 removed
csleepvpart2 <- cor.test(df$total.sleep[-33], df$part[-33], use = "pairwise.complete.obs")


#######################################################################

#Hypothesis 2 - Alternative to correlation can use linear model but I will use correlation above - done
### Test for relationship between engagement and sleep (total.sleep) using linear model ----

### Test sleep vs total engagement
ggplot(df[-33, ], aes(x = total.sleep, y = total.eng)) + 
  geom_point() + 
  geom_smooth(method = "lm")

sleepveng <- lm(total.eng ~ total.sleep, data = df[-33, ])
plot(sleepveng)
summary(sleepveng)
#Significant effect p-value=.000266

###Test sleep vs skills
ggplot(df[-33, ], aes(x = total.sleep, y = skills)) + 
  geom_point() + 
  geom_smooth(method = "lm")

sleepvskills <- lm(skills ~ total.sleep, data = df[-33, ])
plot(sleepvskills) 
summary(sleepvskills)
#highly significant p-value=.000000349

### Test sleep vs emot
ggplot(df[-33, ], aes(x = total.sleep, y = emot)) + 
  geom_point() + 
  geom_smooth(method = "lm")

sleepvemot <- lm(emot ~ total.sleep, data = df[-33, ])
plot(sleepvemot)
summary(sleepvemot)

### Test sleep vs perf
ggplot(df[-33, ], aes(x = total.sleep, y = perf)) + 
  geom_point() + 
  geom_smooth(method = "lm")

sleepvperf <- lm(perf ~ total.sleep, data = df[-33, ])
plot(sleepvperf)
summary(sleepvperf)
# somewhat significant p-value=.00246

### Test sleep vs part
ggplot(df[-33, ], aes(x = total.sleep, y = part)) + 
  geom_point() + 
  geom_smooth(method = "lm")

sleepvpart <- lm(part ~ total.sleep, data = df[-33, ])
plot(sleepvpart)
summary(sleepvpart)


#Just a repetition of above because I accidentally repeated the analysis but above looks cleaner
ggplot(df[-33, ], aes(x = total.sleep, y = skills)) + 
  geom_point() + 
  geom_smooth(method = "lm")

sleepvskills <- lm(skills ~ total.sleep, data = df)
plot(sleepvskills) ## 33 is a real issue here


### Definitely should remove 33 as it has high leverage as an outlier
sleepvskills <- lm(skills ~ total.sleep, data = df[-33, ])
plot(sleepvskills)
summary(sleepvskills)

### Test sleep vs emot
ggplot(df, aes(x = total.sleep, y = emot)) + 
  geom_point() + 
  geom_smooth(method = "lm")

sleepvemot <- lm(emot ~ total.sleep, data = df[-33, ])
plot(sleepvemot)
summary(sleepvemot)

### Test sleep vs perf
ggplot(df[-c(33, 183), ], aes(x = total.sleep, y = perf)) + 
  geom_point() + 
  geom_smooth(method = "lm")

sleepvperf <- lm(perf ~ total.sleep, data = df[-33, ])
plot(sleepvperf) #183 is an issue here
summary(sleepvperf)

### Test sleep vs part
ggplot(df[-c(33), ], aes(x = total.sleep, y = part)) + 
  geom_point() + 
  geom_smooth(method = "lm")

sleepvpart <- lm(part ~ total.sleep, data = df[-33, ])
plot(sleepvpart)
summary(sleepvpart)

### Test sleep vs total engagement
ggplot(df[-c(33), ], aes(x = total.sleep, y = total.eng)) + 
  geom_point() + 
  geom_smooth(method = "lm")

sleepveng <- lm(total.eng ~ total.sleep, data = df[-33, ])
plot(sleepveng)
summary(sleepveng)


################################################################################################

# Hypothesis 3: when looked at as moderation/interaction effect. 

#Factored (academic engagement) model with interaction ----
# Will need to redo this as mediation later*
# Checked about seperating out SH into levels but was not useful in explaining data - don't do

#eng3=Skills
#eng4=Emotional
#eng5=Participation/Interaction
#eng6=Performance

eng3sleep <- lm(skills ~ total.stress + total.sleep + total.stress:total.sleep, data = df)
#Create summary of the model for evaluation
seng3sleep <- summary(eng3sleep)
# View the summary
print(seng3sleep) ## ID 33 has a lot of leverage (use: plot(m3b))

eng4sleep <- lm(emot ~ total.stress + total.sleep + total.stress:total.sleep, data = df)
#Create summary of the model for evaluation
seng4sleep <- summary(eng4sleep)
# View the summary
print(seng4sleep) #nothing significant

eng5sleep <- lm(part ~ total.stress + total.sleep + total.stress:total.sleep, data = df)
#Create summary of the model for evaluation
seng5sleep <- summary(eng5sleep)
# View the summary
print(seng5sleep) #nothing significant

eng6sleep <- lm(perf ~ total.stress + total.sleep + total.stress:total.sleep, data = df)
#Create summary of the model for evaluation
seng6sleep <- summary(eng6sleep)
# View the summary
print(seng6sleep) #nothing significant

#found effect for eng3 and possible outlier - examine further

write.csv(seng3$coef, file = "Model3Results.csv")

############################################################################################

#creating plot to determine interaction effect on eng3: ----

ggplot(df, aes(y = skills, x = total.sleep)) + #This calls the data and sets the x and y axes
  geom_point(aes(size = total.stress)) + #This adds points to the plot
  ylab("Skills") + #These change the labels of the x and y axes (by default they get the name of your variable which may not be very useful to an outside reader)
  xlab("Total.sleep") + #replace this with something intelligible
  scale_size_continuous("Total.stress") #this changes the name of the size legend, make it make sense

#Try again with color instead of size
ggplot(df, aes(y = skills, x = total.sleep)) + #This calls the data and sets the x and y axes
  geom_point(aes(color = total.s), size = 2) + #This adds points to the plot with the color mapped to total.s
  ylab("Skills") + #These change the labels of the x and y axes (by default they get the name of your variable which may not be very useful to an outside reader)
  xlab("Total.sleep") + #replace this with something intelligible
  scale_color_continuous("Total.stress") #this changes the name of the size legend, make it make sense

#Both of these plots indicate to me that the interaction is slightly attenuating

#lets try reversing the predictors
ggplot(df, aes(y = skills, x = total.stress)) + #This calls the data and sets the x and y axes
  geom_point(aes(size = total.sleep)) + #This adds points to the plot
  ylab("Skills") + #These change the labels of the x and y axes (by default they get the name of your variable which may not be very useful to an outside reader)
  xlab("Total.stress") + #replace this with something intelligible
  scale_size_continuous("Total.sleep") #this changes the name of the size legend, make it make sense

ggplot(df, aes(y = skills, x = total.stress)) + #This calls the data and sets the x and y axes
  geom_point(aes(color = total.b), size = 2) + #This adds points to the plot
  ylab("Skills") + #These change the labels of the x and y axes (by default they get the name of your variable which may not be very useful to an outside reader)
  xlab("Total.stress") + #replace this with something intelligible
  scale_color_continuous("Total.sleep") #this changes the name of the size legend, make it make sense


#Sometimes is it helpful to have the response as the third dimension
ggplot(df, aes(y = total.sleep, x = total.stress)) + 
  geom_point(aes(size = skills)) 


ggplot(df, aes(y = total.sleep, x = total.stress)) + 
  geom_point(aes(color = skills), size = 2) 

#Hmmm, there is an outlying value that may be driving the attenuation.  
#I am suspicious that the interaction might dissappear or change signs if this data point is removed.


#refit the model with only skills greater than 5
#First find the outlier
ind <- which(df$skills < 5) #observation 33

#Refit the model after removing the outlier
eng3New <- lm(skills ~ total.stress + total.sleep + total.stress:total.sleep, data = df[-ind, ])

summary(eng3New)

#As I suspected the removal of this point made the interaction non-significant
#I would suggest re-running the other models without this observation since it 
#appears to be an outlier.  Or indeed verify that it should be included.


#here is a prediction function so you can see how it is done
predicteng3 <- function(total.stress, total.sleep){
  skills <- -28.72 + 0.340955*total.stress + 1.032546*total.sleep + -0.007551*total.stress*total.sleep
  return(skills)
} #there is also a predict() function in R that does this for any model

#find the range of values
summary(df[, c("total.stress", "total.sleep", "skills")])


predicteng3(total.stress = 58, total.sleep = 22)#with both low

#Looking at the outlier: Observation #33
df[, 33]


########################################################################
#Hypothesis 3 - Mediational model -done

#Independent variable = Stress
#Dependent variable = AE/factors
#Mediator = Sleep Hygiene

#First step - Regress the dependent variable on the independent variable. Use linear model.

### Test stress vs total engagement
ggplot(df[-33, ], aes(x = total.stress, y = total.eng)) + 
  geom_point() + 
  geom_smooth(method = "lm")

stressveng <- lm(total.eng ~ total.stress, data = df[-33, ])
plot(stressveng)
summary(stressveng)

###Test stress vs skills
ggplot(df[-33, ], aes(x = total.stress, y = skills)) + 
  geom_point() + 
  geom_smooth(method = "lm")

stressvskills <- lm(skills ~ total.stress, data = df[-33, ])
plot(stressvskills) 
summary(stressvskills)
#Significant effect 

### Test stress vs emot
ggplot(df[-33, ], aes(x = total.stress, y = emot)) + 
  geom_point() + 
  geom_smooth(method = "lm")

stressvemot <- lm(emot ~ total.stress, data = df[-33, ])
plot(stressvemot)
summary(stressvemot)

### Test stress vs perf
ggplot(df[-33, ], aes(x = total.stress, y = perf)) + 
  geom_point() + 
  geom_smooth(method = "lm")

stressvperf <- lm(perf ~ total.stress, data = df[-33, ])
plot(stressvperf)
summary(stressvperf)

### Test stress vs part
ggplot(df[-33, ], aes(x = total.stress, y = part)) + 
  geom_point() + 
  geom_smooth(method = "lm")

stressvpart <- lm(part ~ total.stress, data = df[-33, ])
plot(stressvpart)
summary(stressvpart)

#Only significant regression is on stress vs skills


#Second step - Regress the mediator (sleep hygiene) on the independent variable (stress)

### Test sleep vs stress
ggplot(df[-33, ], aes(x = total.sleep, y = total.stress)) + 
  geom_point() + 
  geom_smooth(method = "lm")

sleepvstress <- lm(total.stress ~ total.sleep, data = df[-33, ])
plot(sleepvstress)
summary(sleepvstress)
#highly significant p-value=.00000271


#Don't need the correlations of stress and sleep hygiene done here because I did linear model above
#Will be used for correlation table
#Simple plot of the relationship between stress and sleep
ggplot(df, aes(x = total.stress, y = total.sleep)) + 
  geom_text(aes(label = ID)) + 
  geom_smooth(method = "lm")
# correlation of stress and sleep hygiene
cstressvsleep <- cor(df$total.stress, df$total.sleep, use = "pairwise.complete.obs")
#remove outlier 33
cstressvsleep2 <- cor.test(df$total.stress[-33], df$total.sleep[-33], use = "pairwise.complete.obs")
#0.323


#Third step - Regress the dependent variable on the mediator and independent variable

engstresssleep <- lm(total.eng ~ total.stress + total.sleep, data = df[-33, ])
#Create summary of the model for evaluation
sengstresssleep <- summary(engstresssleep)
# View the summary
print(sengstresssleep)
#The effect size of .312 for mediator is signif at p-value=0.000157
#The effect size for stress decreased from  to 0.044

skillsstresssleep <- lm(skills ~ total.stress + total.sleep, data = df[-33, ])
skillsstresssleep <- summary(skillsstresssleep)
print(skillsstresssleep)
##The effect size of  for mediator is signif at p-value=
#The effect size for stress decreased from  to  (sign to nonsign)

emotstresssleep <- lm(emot ~ total.stress + total.sleep, data = df[-33, ])
emotstresssleep <- summary(emotstresssleep)
print(emotstresssleep)

partstresssleep <- lm(part ~ total.stress + total.sleep, data = df[-33, ])
partstresssleep <- summary(partstresssleep)
print(partstresssleep)
##The effect size of  for mediator is slightly signif at p-value=
#The effect size for stress decreased from  to - (further from zero)

perfstresssleep <- lm(perf ~ total.stress + total.sleep, data = df[-33, ])
perfstresssleep <- summary(perfstresssleep)
print(perfstresssleep)
##The effect size of  for mediator is signif at p-value=
#The effect size for stress decreased from  to  (sign. to nonsign.)


##########################################################################

#Hypothesis 4 -done ----

### Test relationship between academic engagement and exercise using correlation ----

#Create total.exercise (this is not the weighted sum value - run weighted sum and do relavent analyses again)(rename total.e with something?- total.e.notweighted)
df$total.exercise.notweighted <- df$e195+df$e196+df$e197

### First calculate the total exercise score using weighted sum 
df$total.exercise <- with(df, 9*e195 + 6*e196 + 3*e197)

#Check relationship between stress and exercise first - just for informational purposes
#Simple plot of the relationship between stress and exercise
ggplot(df, aes(x = total.stress, y = total.exercise)) + 
  geom_text(aes(label = ID)) + 
  geom_smooth(method = "lm")

# correlation of stress and exercise
cstressvexercise <- cor(df$total.stress, df$total.exercise, use = "pairwise.complete.obs")
#remove outlier 33
cstressvexercise2 <- cor.test(df$total.stress[-33], df$total.exercise[-33], use = "pairwise.complete.obs")


#Simple plot of the relationship between exercise and total engagement
ggplot(df, aes(x = total.exercise, y = total.eng)) + 
  geom_text(aes(label = ID)) + 
  geom_smooth(method = "lm")

# correlation of exercise and total eng
cexerciseveng <- cor(df$total.exercise, df$total.eng, use = "pairwise.complete.obs")
cexerciseveng2 <- cor.test(df$total.exercise[-33], df$total.eng[-33], use = "pairwise.complete.obs")

### Test Exercise vs skills - correlation
ggplot(df[-33, ], aes(x = total.exercise, y = skills)) + 
  geom_point() + 
  geom_smooth(method = "lm")

cexercisevskills <- cor(df$total.exercise, df$skills, use = "pairwise.complete.obs")

#with no item33
cexercisevskills2 <- cor.test(df$total.exercise[-33], df$skills[-33], use = "pairwise.complete.obs")

### Test exercise vs emot
ggplot(df, aes(x = total.exercise, y = emot)) + 
  geom_point() + 
  geom_smooth(method = "lm")

cexercisevemot <- cor(df$total.exercise, df$emot, use = "pairwise.complete.obs")

#with no item 33
cexercisevemot2 <- cor.test(df$total.exercise[-33], df$emot[-33], use = "pairwise.complete.obs")

### Test exercise vs perf
ggplot(df[-c(33), ], aes(x = total.exercise, y = perf)) + 
  geom_point() + 
  geom_smooth(method = "lm")

cexercisevperf <- cor(df$total.exercise, df$perf, use = "pairwise.complete.obs")

#with no item 33
cexercisevperf2 <- cor.test(df$total.exercise[-33], df$perf[-33], use = "pairwise.complete.obs")

### Test exercise vs part ----
ggplot(df[-c(33), ], aes(x = total.exercise, y = part)) + 
  geom_point() + 
  geom_smooth(method = "lm")

cexercisevpart <- cor(df$total.exercise, df$part, use = "pairwise.complete.obs")

#with no item 33
cexercisevpart2 <- cor.test(df$total.exercise[-33], df$part[-33], use = "pairwise.complete.obs")


################################################################################################

#Hypothesis 4 - Alternative to correlation can use linear model but I will use correlation above - done

### Test relationship between academic engagement and exercise using linear model, with item 33 omitted (hypothesis 4) ---- 

### Test exercise vs total engagement
ggplot(df[-c(33), ], aes(x = total.e, y = total.eng)) + 
  geom_point() + 
  geom_smooth(method = "lm")

exerciseveng <- lm(total.eng ~ total.e, data = df[-33, ])
plot(exerciseveng)
summary(exerciseveng)

### Test Exercise vs skills
ggplot(df[-33, ], aes(x = total.e, y = skills)) + 
  geom_point() + 
  geom_smooth(method = "lm")

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


############################################################################################

#Hypothesis 5: Interaction effect with exercise  - done ----

### Test moderation of exercise on relationship between exercise and academic engagement using multiple regression (equation from article)

# Factored (total academic engagement) model with interaction for exercise
engexercise <- lm(total.eng ~ total.stress + total.exercise + total.stress:total.exercise, data = df)
#Create summary of the model for evaluation
sengexercise <- summary(engexercise)
# View the summary
print(sengexercise)

eng3exercise <- lm(skills ~ total.stress + total.exercise + total.stress:total.exercise, data = df)
seng3exercise <- summary(eng3exercise)
print(seng3exercise) #obs 33 poses issue here

eng4exercise <- lm(emot ~ total.stress + total.exercise + total.stress:total.exercise, data = df)
seng4exercise <- summary(eng4exercise)
print(seng4exercise)

eng5exercise <- lm(part ~ total.stress + total.exercise + total.stress:total.exercise, data = df)
seng5exercise <- summary(eng5exercise) # obs 156 poses issue here
print(seng5exercise)

eng6exercise <- lm(perf ~ total.stress + total.exercise + total.stress:total.exercise, data = df)
seng6exercise <- summary(eng6exercise) ## 33 and 183 pose issues here
print(seng6exercise)

#Do above analysis with 33 omitted:

engexercise <- lm(total.eng ~ total.stress + total.exercise + total.stress:total.exercise, data = df[-33,])
sengexercise <- summary(engexercise)
print(sengexercise)

eng3exercise <- lm(skills ~ total.stress + total.exercise + total.stress:total.exercise, data = df[-33,])
seng3exercise <- summary(eng3exercise)
print(seng3exercise)

eng4exercise <- lm(emot ~ total.stress + total.exercise + total.stress:total.exercise, data = df[-33,])
seng4exercise <- summary(eng4exercise)
print(seng4exercise)

eng5exercise <- lm(part ~ total.stress + total.exercise + total.stress:total.exercise, data = df[-33,])
seng5exercise <- summary(eng5exercise)
print(seng5exercise)

eng6exercise <- lm(perf ~ total.stress + total.exercise + total.stress:total.exercise, data = df[-31,])
seng6exercise <- summary(eng6exercise)
print(seng6exercise)


####################################################################

# Hypothesis 6 - Predictive model of influence of variables on AE

#See equation from notes
predicteng <- lm(total.eng ~ total.stress*total.sleep*total.exercise, data = df[-33,])
spredicteng <- summary(predicteng)
print(spredicteng)

# with demographic variable of age
#You may want to set the reference category though, by default it is the name of the category which comes first alpha-numerically.
#df$age <- factor(df$age, levels = c("level1", :"level2", etc.))
predicteng <- lm(total.eng ~ age*total.stress*total.sleep*total.exercise, data = df[-33,])
spredicteng <- summary(predicteng)
print(spredicteng)

predictskills <- lm(total.skills ~ total.stress + total. sleep + total.exercise + total.stress:total.sleep + total.stress:total.exercise + total.stress:total.sleep:total.exercise, data = df[-33,])
spredictskills <- summary(predictskills)
print(spredictskills)

###Random Forest Code to look at importance of each variable
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


################################################################################

#Doing a CFA to check factor loadings on Engagement Questionnaire with my data ----

library(lavaan)


HS.model <- ' Skills =~ m143 + m144 + m145 + m146 + m147 + m148 + m149 + m150 + m151
Emotional =~ m152 + m153 + m154 + m155 + m156
Part.int =~ m157 + m158 + m159 + m160 + m161 + m162
Performance =~ m163 + m164 + m165 '

fit <- cfa(HS.model, data=df, std.lv=TRUE)

summary(fit, fit.measures=TRUE, standardized = TRUE)

coef(fit)

######################################################################################

