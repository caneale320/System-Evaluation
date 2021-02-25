
# import data


library(RODBC)
db <- "C:\\Users\\canea\\Documents\\UVA\\Spring 2021\\Systems Evaluation\\Birdstrikes Case\\wildlife.accdb"
con <- odbcConnectAccess2007(db)

df <- sqlFetch(con, "STRIKE_REPORTS")

library(dplyr)

removeNA <- df %>% filter(!is.na(DAMAGE_LEVEL)) 
removeNA2 <- df %>% filter(!is.na(INDICATED_DAMAGE))
removeNA2 <- filter(removeNA2, !(AIRCRAFT == "UNKNOWN"))

subsetAircraft <- removeNA2 %>% count(vars= AIRCRAFT) %>% filter(n>250)

over250Strikes <- df %>% filter(df$AIRCRAFT %in% subsetAircraft$vars)
over250Strikes$INDICATED_DAMAGE <- as.factor(over250Strikes$INDICATED_DAMAGE)

twoWayTable = table(over250Strikes$AIRCRAFT, over250Strikes$INDICATED_DAMAGE)
library(vcd)
mosaicplot(twoWayTable, shade = TRUE, las=2,
           main = "Aircraft", legend= TRUE)

twoWayTableDamage = table(df$TIME_OF_DAY, df$INDICATED_DAMAGE)
mosaicplot(twoWayTableDamage, shade = TRUE, las=2,
           main = "Time of Day")

twoWayTableMonth = table(df$INCIDENT_MONTH, df$INDICATED_DAMAGE)
mosaicplot(twoWayTableMonth, shade = TRUE, las=2,
           main = "Months vs Indicated Damage")


numFligths <- read.csv("C:\\Users\\canea\\Documents\\UVA\\Spring 2021\\Systems Evaluation\\Birdstrikes Case\\USCarrier_Traffic_20210224185428.csv")
df %>% group_by(INCIDENT_YEAR) %>% count(vars= INCIDENT_MONTH) %>% write.csv("StrikesByMonth.csv")

library(ggplot2)

ggplot(over250Strikes) +
  aes(x = AIRCRAFT, y=1, fill = INDICATED_DAMAGE) +
  geom_col(position = "fill") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+ ylab("Proportion of Birdstrikes with ot without Damage")
