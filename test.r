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
