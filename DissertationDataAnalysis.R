library(memisc)
library(reshape2)
library(plyr)
library(dplyr)
library(ggplot2)

df <- as.data.set(spss.system.file("./Data/College coping data (complete).sav"))
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
df.bm.w <- reshape(as.data.frame(df.bm.sum), 
                   timevar = "vartype", 
                   times = c("s", "b", "m"), 
                   idvar = c("ID", "class", "ethnicity", "gender", "age"), 
                   direction = "wide")

#Simple plot of the relationship between B and M
ggplot(df.bm.w, aes(x = total.b, y = total.m)) + 
  geom_point() + 
  geom_smooth(method = "lm")

# correlation of B and M
cbm <- cor(df.bm.w$total.b, df.bm.w$total.m, use = "pairwise.complete.obs")

# Basic model with interaction
m1 <- lm(total.s ~ total.m + total.b + total.m:total.b, data = df.bm.w)
#Create summary of the model for evaluation
sm1 <- summary(m1)
# View the summary
print(sm1) #nothing significant

#View histogram of B to see where potential cutpoints might be
hist(df.bm.w$total.b)

#looks pretty normal, can use quartiles to create 4 categories
cuts <- quantile(x = df.bm.w$total.b, probs = c(0.25, 0.5, 0.75))

#remember, you can learn more about a function by typing a ? before the function:
?quantile

#Create a new variable with the cuts using logic
df.bm.w$bCat <- with(df.bm.w, ifelse(total.b < cuts[1], "Low",
                       ifelse(total.b >= cuts[1] & total.b < cuts[2], "Mid_low",
                              ifelse(total.b >= cuts[2] & total.b < cuts[3], "Mid_high", "High"))))

#set the factor levels so we can control which category is the "reference"
df.bm.w$bCat <- factor(df.bm.w$bCat, levels = c("Low", "Mid_low", "Mid_high", "High"))

#New model with dichotomized  B variable
m2 <- lm(total.s ~ total.m + bCat + total.m:bCat, data = df.bm.w)
#Create summary of the model for evaluation
sm2 <- summary(m2)
# View the summary
print(sm2) #the highest category of B has a significant difference in mean S than the lowest
 # the second highest is 'almost' significant ;)

#visualize the results
ggplot(df.bm.w, aes(x = bCat, y = total.s)) + 
  geom_boxplot()
