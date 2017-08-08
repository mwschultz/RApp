require(tidyverse)
require(haven)
require(dplyr)
require(Lahman)
require(magrittr)

Batting %>% select(X2B:HR)

c<-Batting %>% transmute(ExtraBaseHits = X2B + X3B + HR)

#mutate -- add newly created column to data frame 
#transmute -- create new variable 

Batting %>% summarise(AvgX2B = mean(X2B, na.rm=TRUE)) 
#NA is missing 

Batting %>% group_by (teamID)

Batting <- tbl_df(Batting)
Batting

Batting %>% group_by(teamID) #Once group by something, the tag sticks
#with the data set until you ungroup it. 

summarise(AvgX2B = mean(X2B, na.rm = TRUE))


# create two simple data frames
# underscore functions are from the tidyverse package 
a <- data_frame(color = c("green", "yellow", "red"), num = 1:3)
b <- data_frame(color = c("green", "yellow", "pink"), size = c("S", "M", "L"))
a
b
inner_join(a, b)

inner_join(a, b)
full_join(a, b)
left_join(a, b)
right_join(a, b)
left_join(b, a)
semi_join(a, b) #joining between a and b, and filtering a where there is a match
anti_join(a, b) #joining between a and b, and filthering where there is NOT a match 






b <- b %>% rename(col = color)
a
b
inner_join(a, b, by = c("color" = "col"))




titanicData <- read_csv("https://raw.githubusercontent.com/jbpost2/DataScienceR/master/datasets/titanic.csv")
titanicData


table(titanicData$embarked)

table(titanicData$survived)

table(titanicData$sex)

help(table)

table(titanicData$survived, titanicData$sex)
tab <- table(titanicData$survived, titanicData$embarked, titanicData$sex)
tab
tab[1, ,]
tabl[]

require(ggplot2)
#filled bar plot
g <- ggplot(data = titanicData %>% drop_na(embarked),
            aes(x = as.factor(embarked)))
g + geom_bar(aes(fill = as.factor(survived)))

g <- ggplot(data = titanicData %>% drop_na(embarked),
            aes(x = as.factor(embarked)))
g + geom_bar(aes(fill = as.factor(survived))) +
  labs(x = "City Embarked", 
       title = "Bar Plot of Embarked City for Titanic Passengers") + 
  scale_x_discrete(labels = c("Cherbourg", "Queenstown", "Southampton")) + 
  scale_fill_discrete(name = "Surived", labels = c("No","Yes"))


####

CO2 <- tbl_df(CO2)
CO2

mean(CO2$uptake, trim = 0.05)
median(CO2$uptake)

summary(CO2$uptake)

quantile(CO2$uptake, probs = c(0.1,0.2))

stats <- c(summary(CO2$uptake), var(CO2$uptake),
           sd(CO2$uptake), quantile(CO2$uptake, probs = c(0.1, 0.2)))

stats
str(stats)

attributes(stats)

names(stats)[7:10] <-c("Var", "SD", "10thP", "20thP")

CO2 %>% group_by(Treatment) %>% summarise(avg=mean(uptake))
CO2 %>% group_by(Treatment) %>% summarise(median=median(uptake))


CO2 %>% group_by(Treatment, Type) %>% summarise(avg=mean(uptake))

g <- ggplot(CO2, aes(x= uptake)) + geom_dotplot()
g


g <- ggplot(CO2, aes(x= uptake)) + geom_dotplot(aes(color=Treatment))


g<-ggplot(CO2, aes(x=uptake)) + geom_histogram(color="blue", fill="red", linetype = "dashed")
g

g <- ggplot(CO2, aes(x = uptake))+
  geom_histogram(aes(y = ..density.., fill = Treatment))+
  geom_density(adjust = 0.25, alpha = 0.5, aes(fill = Treatment)) 


g <- ggplot(CO2, aes(x=uptake, color=Treatment)) + stat_ecdf(geom="step")

g


scoresFull <- read_csv("https://raw.githubusercontent.com/jbpost2/DataScienceR/master/datasets/scoresFull.csv")
scoresFull

g <- ggplot(scoresFull, aes(x = homeRushYds, y = HFinal)) + geom_point() + geom_smooth() + 
  geom_smooth(method = lm, col = "Red") #linear regression line 
g


g <- ggplot(scoresFull, aes(x = homeRushYds, y = HFinal)) +
  geom_point()

g

g <- ggplot(scoresFull, aes(x = homeRushYds, y = HFinal)) +
  geom_point() +
  geom_smooth() +
  geom_smooth(method = lm, col = "Red") 

paste("Hi", "What", "Is", "Going", "On", "?", sep = " ")
paste("Hi", "What", "Is", "Going", "On", "?", sep = ".")

g <- ggplot(scoresFull, aes(x = homeRushYds,y = HFinal)) +
  geom_point() +
  geom_smooth() +
  geom_smooth(method = lm, col = "Red") + 
  geom_text(x = 315, y = 10, size = 5, label = paste0("Correlation = ", round(correlation, 2)))

g



g <- ggplot(scoresFull, aes(x = homeRushYds, y = HFinal)) +
  geom_point()+
  facet_grid(roof ~ surface)

g

g <- ggplot(scoresFull, aes(x = homeRushYds,y = HFinal)) +
  geom_point(aes(col = homeSpread), alpha = 0.3, size = 0.5) +  
  facet_grid(roof ~ surface)

g

pairs(select(scoresFull, Hturnovers, homeRushYds,
             homePassYds, HFinal), cex = 0.3)


Correlation <- cor(select(scoresFull, Hturnovers, homeRushYds,
                          homePassYds, HFinal), method = "spearman")

require(corrplot)
corrplot(Correlation, type = "upper",
         title = "Figure 2: Correlation matrix of variables.",
         tl.pos = "lt")
corrplot(Correlation, type = "lower", method = "number",
         add = TRUE, diag = FALSE, tl.pos = "n")


g <- ggplot(scoresFull, aes(x = surface, y = homePassYds)) +
  geom_boxplot(fill = "grey")
g


g <- ggplot(scoresFull, aes(x = surface, y = homePassYds)) +
  geom_boxplot(fill = "grey") +
  geom_jitter(aes(col = roof), alpha = 0.3, size = 0.3) +
  stat_summary(fun.y = mean, geom = "line", 
               lwd = 1.5, aes(group = roof, col = roof))
g

g <- ggplot(scoresFull, aes(x = surface, y = homePassYds))+
  geom_violin(fill = "blue") + geom_boxplot(fill="grey", alpha = 0.3)
g

oneDate<-paste(scoresFull$date[1], scoresFull$season[1], sep = "-")
oneDate

library(lubridate)
as.Date(oneDate, "%d-%b-%Y")

as.Date(oneDate, "%d-%b-%Y") + 1

scoresFull$date <- paste(scoresFull$date, scoresFull$season, sep = "-") %>% 
  as.Date("%d-%b-%Y")


subScores <- scoresFull %>% 
  filter(homeTeam %in% c("Pittsburgh Steelers", "Cleveland Browns",
                         "Baltimore Ravens", "Cincinnati Bengals")) %>% 
  group_by(season, homeTeam) %>%
  summarise(homeAvgYds = mean(homePassYds + homeRushYds))

subScores

g <- ggplot(subScores, aes(x = season, y = homeAvgYds, color = homeTeam)) +
  geom_line(lwd = 2)
g

install.packages("plot3Drgl")
library(plot3Drgl)

scatter3D(x = scoresFull$homeRushYds, y = scoresFull$awayRushYds,
          z = scoresFull$HFinal)

plotrgl()

voting <- read.csv("https://raw.githubusercontent.com/jbpost2/DataScienceR/master/datasets/counties.csv", header = TRUE)
voting


votePlot <- ggplot(voting, aes(x = college, y = income))
votePlot + 
  geom_point()+
  geom_text(x = 40, y = 15000, label = round(cor(voting$college, voting$income), 2))

votePlot

lm(income ~ college, data = voting)


fit <- lm(income ~ college, data = voting)
attributes(fit)

anova(fit)
summary(fit)
plot(fit)

predict(fit, newdata = data.frame(college = c(40, 10)))

predict(fit, newdata = data.frame(college = c(40, 10)), se.fit = TRUE)

predict(fit, newdata = data.frame(college = c(40, 10)),
        se.fit = TRUE, interval = "confidence")

predict(fit, newdata = data.frame(college = c(40, 10)),
        se.fit = TRUE, interval = "prediction")



votePlot +
  geom_point(aes(col = region)) +
  geom_smooth(method = "lm", aes(col = region))


fits <- voting %>% group_by(region) %>% 
  do(model = lm(income ~ college, data = .))
names(fits)













fit2<-lm(income ~ college + Perot, data = voting)
anova(fit2)
summary(fit2)
coef(fit2)
fit2$rank
plot(fit2)

predict(fit2, newdata = data.frame(college = 40, Perot = 20))

