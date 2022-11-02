library(ggplot2)
library(ggpubr)
library(tidyverse)
library(broom)
library(AICcmodavg)

#INSTRUMENT USAGE
#IMPORTING CSV FILE
a<-read.csv("rtable.csv" , header = TRUE , colClasses = c("factor" , "factor" , "numeric" , "numeric" , "numeric" , "factor" , "numeric"  , "numeric"  , "numeric"  , "numeric" ))
summary(a)

#ONE - WAY ANOVA FOR INSTRUMENT USAGE
one.way <- aov(instruments.usage ~ gender, data = a)
summary(one.way)

#TWO - WAY ANOVA FOR INSTRUMENT USAGE
two.way <- aov(instruments.usage ~ gender + age, data = a)
summary(two.way)

#INTERACTION FOR INSTRUMENT USAGE
interaction <- aov(instruments.usage ~ gender*age, data = a)
summary(interaction)

#BLOCKING FOR INSTRUMENT USAGE
blocking <- aov(instruments.usage ~ gender + age + block, data = a)
summary(blocking)

#AIC -TEST (LOW AIC VALUE BEST METHOD) FOR INSTRUMENT USAGE
library(AICcmodavg)
model.set <- list(one.way, two.way, interaction, blocking)
model.names <- c("one.way", "two.way", "interaction", "blocking")
aictab(model.set, modnames = model.names)
#AIC WEIGHT - TOTAL VARIATION IN THE DEPENDENT VARIABILES

#Tukey's Honestly Significant Difference TEST - pairwise comparisons FOR INSTRUMENT USAGE
#statictical difference of  p <0.05
tukey.two.way<-TukeyHSD(two.way)
tukey.two.way

#FIND THE GROUPWISE DIFFERENCES FOR INSTRUMENT USAGE
#FROM THE TUKEY'S TEST - GENDER:AGE - ANOTHER WAY TO SHOW THE P-VALUE
tukey.plot.aov<-aov(instruments.usage ~ gender:age, data=a)
tukey.plot.test<-TukeyHSD(tukey.plot.aov)
plot(tukey.plot.test, las = 1)

#MAKING DATA FRAME WITH THE GROUP LABELS
#SUMMARY OF THE ORIGINAL DATA FOR INSTRUMENT USAGE
mean.instruments.usage.data <- a %>%
  group_by(gender , age) %>%
  summarise(
    instruments.usage = mean(instruments.usage)
  )
#LABELING PART FOR INSTRUMENT USAGE
mean.instruments.usage.data$group <- c("a","b","b","b","b","c")
mean.instruments.usage.data
#EASY TO PLOT

#PLOT OF THE RAW DATA FOR INSTRUMENT USAGE
two.way.plot <- ggplot(a, aes(x = age, y = instruments.usage, group=gender)) +
  geom_point(cex = 1.5, pch = 1.0,position = position_jitter(w = 0.1, h = 0))
two.way.plot

#ADDING MEANS AND STANDARD ERRORS TO THE GRAPH FOR INSTRUMENT USAGE
two.way.plot <- two.way.plot +
  stat_summary(fun.data = 'mean_se', geom = 'errorbar', width = 0.2) +
  stat_summary(fun.data = 'mean_se', geom = 'pointrange') +
  geom_point(data=mean.instruments.usage.data, aes(x=age, y=instruments.usage))
two.way.plot

#SPLIT UP THE DATA FOR INSTRUMENT USAGE
#LINE REPRESENT THE UNCERTAINTY IN A REPORTED MEASUREMENT
two.way.plot <- two.way.plot +
  geom_text(data=mean.instruments.usage.data, label=mean.instruments.usage.data$group, vjust = -8, size = 5) +
  facet_wrap(~ gender)
two.way.plot

#FINAL GRAPH FOR INSTRUMENT USAGE
two.way.plot <- two.way.plot +
  theme_classic2() + #BACKGROUND
  labs(title = "Cyberbullying with the instruments usage in response to gender mix and age",
       #TITLE OF THE GRAPH       
       x = "Age (1=15 to 20, 2=21 to 25 , 3=26 to 30)",#X - AXIS NAME
       y = "Instruments usage")#Y - AXIS NAME
two.way.plot



#FREQUENTLY WITNESS TO CYBERBULLYING
#IMPORTING CSV FILE
a<-read.csv("rtable.csv" , header = TRUE , colClasses = c("factor" , "factor" , "numeric" , "numeric" , "numeric" , "factor" , "numeric"  , "numeric"  , "numeric"  , "numeric" ))
summary(a)

#ONE - WAY ANOVA FOR FREQUENTLY WITNESS TO CYBERBULLYING
one.way <- aov(frequently.witness.to.cyberbullying.inculsion ~ gender, data = a)
summary(one.way)

#TWO - WAY ANOVA FOR FREQUENTLY WITNESS TO CYBERBULLYING
two.way <- aov(frequently.witness.to.cyberbullying.inculsion ~ gender + age, data = a)
summary(two.way)


#INTERACTION FOR FREQUENTLY WITNESS TO CYBERBULLYING
interaction <- aov(frequently.witness.to.cyberbullying.inculsion ~ gender*age, data = a)
summary(interaction)

#BLOCKING FOR FREQUENTLY WITNESS TO CYBERBULLYING
blocking <- aov(frequently.witness.to.cyberbullying.inculsion ~ gender + age + block, data = a)
summary(blocking)

#AIC -TEST (LOW AIC VALUE BEST METHOD) FOR FREQUENTLY WITNESS TO CYBERBULLYING
library(AICcmodavg)
model.set <- list(one.way, two.way, interaction, blocking)
model.names <- c("one.way", "two.way", "interaction", "blocking")
aictab(model.set, modnames = model.names)
#AIC WEIGHT - TOTAL VARIATION IN THE DEPENDENT VARIABILES

#Tukey's Honestly Significant Difference TEST - pairwise comparisons FOR FREQUENTLY WITNESS TO CYBERBULLYING
#statictical difference of  p <0.05
tukey.two.way<-TukeyHSD(two.way)
tukey.two.way

#FIND THE GROUPWISE DIFFERENCES FOR FREQUENTLY WITNESS TO CYBERBULLYING
#FROM THE TUKEY'S TEST - GENDER:AGE - ANOTHER WAY TO SHOW THE P-VALUE
tukey.plot.aov<-aov(frequently.witness.to.cyberbullying.inculsion ~ gender:age, data=a)
tukey.plot.test<-TukeyHSD(tukey.plot.aov)
plot(tukey.plot.test, las = 1)

#MAKING DATA FRAME WITH THE GROUP LABELS
#SUMMARY OF THE ORIGINAL DATA FOR FREQUENTLY WITNESS TO CYBERBULLYING
mean.frequently.witness.to.cyberbullying.inculsion.data <- a %>%
  group_by(gender , age) %>%
  summarise(
    frequently.witness.to.cyberbullying.inculsion = mean(frequently.witness.to.cyberbullying.inculsion)
  )
#LABELING PART FOR FREQUENTLY WITNESS TO CYBERBULLYING
mean.frequently.witness.to.cyberbullying.inculsion.data$group <- c("a","b","b","b","b","c")
mean.frequently.witness.to.cyberbullying.inculsion.data
#EASY TO PLOT

#PLOT OF THE RAW DATA FOR FREQUENTLY WITNESS TO CYBERBULLYING
two.way.plot <- ggplot(a, aes(x = age, y = frequently.witness.to.cyberbullying.inculsion, group=gender)) +
  geom_point(cex = 1.5, pch = 1.0,position = position_jitter(w = 0.1, h = 0))
two.way.plot

#ADDING MEANS AND STANDARD ERRORS TO THE GRAPH FOR FREQUENTLY WITNESS TO CYBERBULLYING
two.way.plot <- two.way.plot +
  stat_summary(fun.data = 'mean_se', geom = 'errorbar', width = 0.2) +
  stat_summary(fun.data = 'mean_se', geom = 'pointrange') +
  geom_point(data=mean.frequently.witness.to.cyberbullying.inculsion.data, aes(x=age, y=frequently.witness.to.cyberbullying.inculsion))
two.way.plot

#SPLIT UP THE DATA FOR FREQUENTLY WITNESS TO CYBERBULLYING
#LINE REPRESENT THE UNCERTAINTY IN A REPORTED MEASUREMENT
two.way.plot <- two.way.plot +
  geom_text(data=mean.frequently.witness.to.cyberbullying.inculsion.data, label=mean.frequently.witness.to.cyberbullying.inculsion.data$group, vjust = -8, size = 5) +
  facet_wrap(~ gender)
two.way.plot

#FINAL GRAPH FOR FREQUENTLY WITNESS TO CYBERBULLYING
two.way.plot <- two.way.plot +
  theme_classic2() + #BACKGROUND
  labs(title = "Cyberbullying with the frequently witness to cyberbullying in response to gender mix and age",
       #TITLE OF THE GRAPH       
       x = "Age (1=15 to 20, 2=21 to 25 , 3=26 to 30)",#X - AXIS NAME
       y = "Frequently witness to cyberbullying")#Y - AXIS NAME
two.way.plot



#APPROACH WHEN YOU ARE A CYBERBULLYING WITNESS
#ONE - WAY ANOVA FOR APPROACH WHEN YOU ARE A CYBERBULLYING WITNESS
one.way <- aov(approach.when.you.are.a.cyberbullying.witness ~ gender, data = a)
summary(one.way)

#TWO - WAY ANOVA FOR APPROACH WHEN YOU ARE A CYBERBULLYING WITNESS
two.way <- aov(approach.when.you.are.a.cyberbullying.witness ~ gender + age, data = a)
summary(two.way)

#INTERACTION FOR APPROACH WHEN YOU ARE A CYBERBULLYING WITNESS
interaction <- aov(approach.when.you.are.a.cyberbullying.witness ~ gender*age, data = a)
summary(interaction)

#BLOCKING FOR APPROACH WHEN YOU ARE A CYBERBULLYING WITNESS
blocking <- aov(approach.when.you.are.a.cyberbullying.witness ~ gender + age + block, data = a)
summary(blocking)

#AIC -TEST (LOW AIC VALUE BEST METHOD) FOR APPROACH WHEN YOU ARE A CYBERBULLYING WITNESS
library(AICcmodavg)
model.set <- list(one.way, two.way, interaction, blocking)
model.names <- c("one.way", "two.way", "interaction", "blocking")
aictab(model.set, modnames = model.names)
#AIC WEIGHT - TOTAL VARIATION IN THE DEPENDENT VARIABILES

#Tukey's Honestly Significant Difference TEST - pairwise comparisons FOR APPROACH WHEN YOU ARE A CYBERBULLYING WITNESS
#statictical difference of  p <0.05
tukey.two.way<-TukeyHSD(two.way)
tukey.two.way

#FIND THE GROUPWISE DIFFERENCES FOR APPROACH WHEN YOU ARE A CYBERBULLYING WITNESS
#FROM THE TUKEY'S TEST - GENDER:AGE - ANOTHER WAY TO SHOW THE P-VALUE
tukey.plot.aov<-aov(approach.when.you.are.a.cyberbullying.witness ~ gender:age, data=a)
tukey.plot.test<-TukeyHSD(tukey.plot.aov)
plot(tukey.plot.test, las = 1)

#MAKING DATA FRAME WITH THE GROUP LABELS
#SUMMARY OF THE ORIGINAL DATA FOR APPROACH WHEN YOU ARE A CYBERBULLYING WITNESS
mean.approach.when.you.are.a.cyberbullying.witness.data <- a %>%
  group_by(gender , age) %>%
  summarise(
    approach.when.you.are.a.cyberbullying.witness = mean(approach.when.you.are.a.cyberbullying.witness)
  )
#LABELING PART FOR APPROACH WHEN YOU ARE A CYBERBULLYING WITNESS
mean.approach.when.you.are.a.cyberbullying.witness.data$group <- c("a","b","b","b","b","c")
mean.approach.when.you.are.a.cyberbullying.witness.data
#EASY TO PLOT

#PLOT OF THE RAW DATA FOR APPROACH WHEN YOU ARE A CYBERBULLYING WITNESS
two.way.plot <- ggplot(a, aes(x = age, y = approach.when.you.are.a.cyberbullying.witness, group=gender)) +
  geom_point(cex = 1.5, pch = 1.0,position = position_jitter(w = 0.1, h = 0))
two.way.plot

#ADDING MEANS AND STANDARD ERRORS TO THE GRAPH FOR APPROACH WHEN YOU ARE A CYBERBULLYING WITNESS
two.way.plot <- two.way.plot +
  stat_summary(fun.data = 'mean_se', geom = 'errorbar', width = 0.2) +
  stat_summary(fun.data = 'mean_se', geom = 'pointrange') +
  geom_point(data=mean.approach.when.you.are.a.cyberbullying.witness.data, aes(x=age, y=approach.when.you.are.a.cyberbullying.witness))
two.way.plot

#SPLIT UP THE DATA FOR APPROACH WHEN YOU ARE A CYBERBULLYING WITNESS
#LINE REPRESENT THE UNCERTAINTY IN A REPORTED MEASUREMENT
two.way.plot <- two.way.plot +
  geom_text(data=mean.approach.when.you.are.a.cyberbullying.witness.data, label=mean.approach.when.you.are.a.cyberbullying.witness.data$group, vjust = -8, size = 5) +
  facet_wrap(~ gender)
two.way.plot

#FINAL GRAPH FOR APPROACH WHEN YOU ARE A CYBERBULLYING WITNESS
two.way.plot <- two.way.plot +
  theme_classic2() + #BACKGROUND
  labs(title = "Cyberbullying with the approach when you are a cyberbullying witness in response to gender mix and age",
       #TITLE OF THE GRAPH       
       x = "Age (1=15 to 20, 2=21 to 25 , 3=26 to 30)",#X - AXIS NAME
       y = "Approach when you are a cyberbullying witness")#Y - AXIS NAME
two.way.plot



#FLAMING
#IMPORTING CSV FILE
a<-read.csv("rtable.csv" , header = TRUE , colClasses = c("factor" , "factor" , "numeric" , "numeric" , "numeric" , "factor" , "numeric"  , "numeric"  , "numeric"  , "numeric" ))
summary(a)

#ONE - WAY ANOVA FOR FLAMING
one.way <- aov(tfbp ~ gender, data = a)
summary(one.way)

#TWO - WAY ANOVA FOR FLAMING
two.way <- aov(tfbp ~ gender + age, data = a)
summary(two.way)

#INTERACTION FOR FLAMING
interaction <- aov(tfbp ~ gender*age, data = a)
summary(interaction)

#BLOCKING FOR FLAMING
blocking <- aov(tfbp ~ gender + age + block, data = a)
summary(blocking)

#AIC -TEST (LOW AIC VALUE BEST METHOD) FOR FLAMING
library(AICcmodavg)
model.set <- list(one.way, two.way, interaction, blocking)
model.names <- c("one.way", "two.way", "interaction", "blocking")
aictab(model.set, modnames = model.names)
#AIC WEIGHT - TOTAL VARIATION IN THE DEPENDENT VARIABILES

#Tukey's Honestly Significant Difference TEST - pairwise comparisons FOR FLAMING
#statictical difference of  p <0.05
tukey.two.way<-TukeyHSD(two.way)
tukey.two.way

#FIND THE GROUPWISE DIFFERENCES FOR FLAMING
#FROM THE TUKEY'S TEST - GENDER:AGE - ANOTHER WAY TO SHOW THE P-VALUE
tukey.plot.aov<-aov(tfbp ~ gender:age, data=a)
tukey.plot.test<-TukeyHSD(tukey.plot.aov)
plot(tukey.plot.test, las = 1)

#MAKING DATA FRAME WITH THE GROUP LABELS
#SUMMARY OF THE ORIGINAL DATA FOR FLAMING
mean.tfbp.data <- a %>%
  group_by(gender , age) %>%
  summarise(
    tfbp = mean(tfbp)
  )
#LABELING PART FOR FLAMING
mean.tfbp.data$group <- c("a","b","b","b","b","c")
mean.tfbp.data
#EASY TO PLOT

#PLOT OF THE RAW DATA FOR FLAMING
two.way.plot <- ggplot(a, aes(x = age, y = tfbp, group=gender)) +
  geom_point(cex = 1.5, pch = 1.0,position = position_jitter(w = 0.1, h = 0))
two.way.plot

#ADDING MEANS AND STANDARD ERRORS TO THE GRAPH FOR FLAMING
two.way.plot <- two.way.plot +
  stat_summary(fun.data = 'mean_se', geom = 'errorbar', width = 0.2) +
  stat_summary(fun.data = 'mean_se', geom = 'pointrange') +
  geom_point(data=mean.tfbp.data, aes(x=age, y=tfbp))
two.way.plot

#SPLIT UP THE DATA FOR FLAMING
#LINE REPRESENT THE UNCERTAINTY IN A REPORTED MEASUREMENT
two.way.plot <- two.way.plot +
  geom_text(data=mean.tfbp.data, label=mean.tfbp.data$group, vjust = -8, size = 5) +
  facet_wrap(~ gender)
two.way.plot

#FINAL GRAPH FOR FLAMING
two.way.plot <- two.way.plot +
  theme_classic2() + #BACKGROUND
  labs(title = "Cyberbullying with the total faming behavioural pattern in response to gender mix and age",
       #TITLE OF THE GRAPH       
       x = "Age (1=15 to 20, 2=21 to 25 , 3=26 to 30)",#X - AXIS NAME
       y = "Flaming")#Y - AXIS NAME
two.way.plot



#ONLINE HARESSMENT
#IMPORTING CSV FILE
a<-read.csv("rtable.csv" , header = TRUE , colClasses = c("factor" , "factor" , "numeric" , "numeric" , "numeric" , "factor" , "numeric"  , "numeric"  , "numeric"  , "numeric" ))
summary(a)

#ONE - WAY ANOVA FOR ONLINE HARESSMENT
one.way <- aov(tohbp ~ gender, data = a)
summary(one.way)

#TWO - WAY ANOVA FOR ONLINE HARESSMENT 
two.way <- aov(tohbp ~ gender + age, data = a)
summary(two.way)

#INTERACTION FOR ONLINE HARESSMENT
interaction <- aov(tohbp ~ gender*age, data = a)
summary(interaction)

#BLOCKING FOR ONLINE HARESSMENT
blocking <- aov(tohbp ~ gender + age + block, data = a)
summary(blocking)

#AIC -TEST (LOW AIC VALUE BEST METHOD) FOR ONLINE HARESSMENT
library(AICcmodavg)
model.set <- list(one.way, two.way, interaction, blocking)
model.names <- c("one.way", "two.way", "interaction", "blocking")
aictab(model.set, modnames = model.names)
#AIC WEIGHT - TOTAL VARIATION IN THE DEPENDENT VARIABILES

#Tukey's Honestly Significant Difference TEST - pairwise comparisons FOR ONLINE HARESSMENT
#statictical difference of  p <0.05
tukey.two.way<-TukeyHSD(two.way)
tukey.two.way

#FIND THE GROUPWISE DIFFERENCES FOR ONLINE HARESSMENT
#FROM THE TUKEY'S TEST - GENDER:AGE - ANOTHER WAY TO SHOW THE P-VALUE
tukey.plot.aov<-aov(tohbp ~ gender:age, data=a)
tukey.plot.test<-TukeyHSD(tukey.plot.aov)
plot(tukey.plot.test, las = 1)

#MAKING DATA FRAME WITH THE GROUP LABELS
#SUMMARY OF THE ORIGINAL DATA FOR ONLINE HARESSMENT
mean.tohbp.data <- a %>%
  group_by(gender , age) %>%
  summarise(
    tohbp = mean(tohbp)
  )
#LABELING PART FOR ONLINE HARESSMENT
mean.tohbp.data$group <- c("a","b","b","b","b","c")
mean.tohbp.data
#EASY TO PLOT

#PLOT OF THE RAW DATA FOR ONLINE HARESSMENT
two.way.plot <- ggplot(a, aes(x = age, y = tohbp, group=gender)) +
  geom_point(cex = 1.5, pch = 1.0,position = position_jitter(w = 0.1, h = 0))
two.way.plot

#ADDING MEANS AND STANDARD ERRORS TO THE GRAPH FOR ONLINE HARESSMENT
two.way.plot <- two.way.plot +
  stat_summary(fun.data = 'mean_se', geom = 'errorbar', width = 0.2) +
  stat_summary(fun.data = 'mean_se', geom = 'pointrange') +
  geom_point(data=mean.tohbp.data, aes(x=age, y=tohbp))
two.way.plot

#SPLIT UP THE DATA FOR ONLINE HARESSMENT
#LINE REPRESENT THE UNCERTAINTY IN A REPORTED MEASUREMENT
two.way.plot <- two.way.plot +
  geom_text(data=mean.tohbp.data, label=mean.tohbp.data$group, vjust = -8, size = 5) +
  facet_wrap(~ gender)
two.way.plot

#FINAL GRAPH FOR ONLINE HARESSMENT
two.way.plot <- two.way.plot +
  theme_classic2() + #BACKGROUND
  labs(title = "Cyberbullying with the total online haressment behavioural pattern in response to gender mix and age",
       #TITLE OF THE GRAPH       
       x = "Age (1=15 to 20, 2=21 to 25 , 3=26 to 30)",#X - AXIS NAME
       y = "Online Haressment")#Y - AXIS NAME
two.way.plot



#DEFAME
#IMPORTING CSV FILE
a<-read.csv("rtable.csv" , header = TRUE , colClasses = c("factor" , "factor" , "numeric" , "numeric" , "numeric" , "factor" , "numeric"  , "numeric"  , "numeric"  , "numeric" ))
summary(a)

#ONE - WAY ANOVA FOR DEFAME
one.way <- aov(tdbp ~ gender, data = a)
summary(one.way)

#TWO - WAY ANOVA FOR DEFAME
two.way <- aov(tdbp ~ gender + age, data = a)
summary(two.way)

#INTERACTION FOR DEFAME
interaction <- aov(tdbp ~ gender*age, data = a)
summary(interaction)

#BLOCKING FOR DEFAME
blocking <- aov(tdbp ~ gender + age + block, data = a)
summary(blocking)

#AIC -TEST (LOW AIC VALUE BEST METHOD) FOR DEFAME
library(AICcmodavg)
model.set <- list(one.way, two.way, interaction, blocking)
model.names <- c("one.way", "two.way", "interaction", "blocking")
aictab(model.set, modnames = model.names)
#AIC WEIGHT - TOTAL VARIATION IN THE DEPENDENT VARIABILES

#Tukey's Honestly Significant Difference TEST - pairwise comparisons FOR DEFAME
#statictical difference of  p <0.05
tukey.two.way<-TukeyHSD(two.way)
tukey.two.way

#FIND THE GROUPWISE DIFFERENCES FOR DEFAME
#FROM THE TUKEY'S TEST - GENDER:AGE - ANOTHER WAY TO SHOW THE P-VALUE
tukey.plot.aov<-aov(tdbp ~ gender:age, data=a)
tukey.plot.test<-TukeyHSD(tukey.plot.aov)
plot(tukey.plot.test, las = 1)

#MAKING DATA FRAME WITH THE GROUP LABELS
#SUMMARY OF THE ORIGINAL DATA FOR DEFAME
mean.tdbp.data <- a %>%
  group_by(gender , age) %>%
  summarise(
    tdbp = mean(tdbp)
  )
#LABELING PART FOR DEFAME
mean.tdbp.data$group <- c("a","b","b","b","b","c")
mean.tdbp.data
#EASY TO PLOT

#PLOT OF THE RAW DATA FOR DEFAME
two.way.plot <- ggplot(a, aes(x = age, y = tdbp, group=gender)) +
  geom_point(cex = 1.5, pch = 1.0,position = position_jitter(w = 0.1, h = 0))
two.way.plot

#ADDING MEANS AND STANDARD ERRORS TO THE GRAPH FOR DEFAME
two.way.plot <- two.way.plot +
  stat_summary(fun.data = 'mean_se', geom = 'errorbar', width = 0.2) +
  stat_summary(fun.data = 'mean_se', geom = 'pointrange') +
  geom_point(data=mean.tdbp.data, aes(x=age, y=tdbp))
two.way.plot

#SPLIT UP THE DATA FOR DEFAME
#LINE REPRESENT THE UNCERTAINTY IN A REPORTED MEASUREMENT
two.way.plot <- two.way.plot +
  geom_text(data=mean.tdbp.data, label=mean.tdbp.data$group, vjust = -8, size = 5) +
  facet_wrap(~ gender)
two.way.plot

#FINAL GRAPH FOR DEFAME
two.way.plot <- two.way.plot +
  theme_classic2() + #BACKGROUND
  labs(title = "Cyberbullying with the total defaming behavioural pattern in response to gender mix and age",
       #TITLE OF THE GRAPH       
       x = "Age (1=15 to 20, 2=21 to 25 , 3=26 to 30)",#X - AXIS NAME
       y = "Defame")#Y - AXIS NAME
two.way.plot


#EXCLUSION
#IMPORTING CSV FILE
a<-read.csv("rtable.csv" , header = TRUE , colClasses = c("factor" , "factor" , "numeric" , "numeric" , "numeric" , "factor" , "numeric"  , "numeric"  , "numeric"  , "numeric" ))
summary(a)

#ONE - WAY ANOVA FOR EXCLUSION
one.way <- aov(tebp ~ gender, data = a)
summary(one.way)

#TWO - WAY ANOVA FOR EXCLUSION
two.way <- aov(tebp ~ gender + age, data = a)
summary(two.way)

#INTERACTION FOR EXCLUSION
interaction <- aov(tebp ~ gender*age, data = a)
summary(interaction)

#BLOCKING FOR EXCLUSION
blocking <- aov(tebp ~ gender + age + block, data = a)
summary(blocking)

#AIC -TEST (LOW AIC VALUE BEST METHOD) FOR EXCLUSION
library(AICcmodavg)
model.set <- list(one.way, two.way, interaction, blocking)
model.names <- c("one.way", "two.way", "interaction", "blocking")
aictab(model.set, modnames = model.names)
#AIC WEIGHT - TOTAL VARIATION IN THE DEPENDENT VARIABILES

#Tukey's Honestly Significant Difference TEST - pairwise comparisons FOR EXCLUSION
#statictical difference of  p <0.05
tukey.two.way<-TukeyHSD(two.way)
tukey.two.way

#FIND THE GROUPWISE DIFFERENCES FOR EXCLUSION
#FROM THE TUKEY'S TEST - GENDER:AGE - ANOTHER WAY TO SHOW THE P-VALUE
tukey.plot.aov<-aov(tebp ~ gender:age, data=a)
tukey.plot.test<-TukeyHSD(tukey.plot.aov)
plot(tukey.plot.test, las = 1)

#MAKING DATA FRAME WITH THE GROUP LABELS
#SUMMARY OF THE ORIGINAL DATA FOR EXCLUSION
mean.tebp.data <- a %>%
  group_by(gender , age) %>%
  summarise(
    tebp = mean(tfbp)
  )
#LABELING PART FOR EXCLUSION
mean.tebp.data$group <- c("a","b","b","b","b","c")
mean.tebp.data
#EASY TO PLOT

#PLOT OF THE RAW DATA FOR EXCLUSION
two.way.plot <- ggplot(a, aes(x = age, y = tebp, group=gender)) +
  geom_point(cex = 1.5, pch = 1.0,position = position_jitter(w = 0.1, h = 0))
two.way.plot

#ADDING MEANS AND STANDARD ERRORS TO THE GRAPH FOR EXCLUSION
two.way.plot <- two.way.plot +
  stat_summary(fun.data = 'mean_se', geom = 'errorbar', width = 0.2) +
  stat_summary(fun.data = 'mean_se', geom = 'pointrange') +
  geom_point(data=mean.tebp.data, aes(x=age, y=tebp))
two.way.plot

#SPLIT UP THE DATA FOR EXCLUSION
#LINE REPRESENT THE UNCERTAINTY IN A REPORTED MEASUREMENT
two.way.plot <- two.way.plot +
  geom_text(data=mean.tebp.data, label=mean.tebp.data$group, vjust = -8, size = 5) +
  facet_wrap(~ gender)
two.way.plot

#FINAL GRAPH FOR EXCLUSION
two.way.plot <- two.way.plot +
  theme_classic2() + #BACKGROUND
  labs(title = "Cyberbullying with the total exclusion behavioural pattern in response to gender mix and age",
       #TITLE OF THE GRAPH       
       x = "Age (1=15 to 20, 2=21 to 25 , 3=26 to 30)",#X - AXIS NAME
       y = "Exclusion")#Y - AXIS NAME
two.way.plot
