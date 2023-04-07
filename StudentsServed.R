library(googlesheets4)
library(dplyr)
library(tidyr)
library(tidyverse)
library(formattable)

removeallNA <- function(data){
  
  a <- (!is.na(data)) %>% colSums %>% data.frame
  ind <- c(1:nrow(a))
  
  final <- data[a$. != 0]
  return(final)
}

process <- function(ogData, moduleName){
 # ogData$module <- moduleName
  Data <- mutate_at(ogData, "School District", ~replace(., ogData$`School District` == "OTHER", ogData$`Other School District`[!is.na(ogData$`Other School District`)])) %>%
    mutate_at( "School Name", ~replace(., ogData$`School Name`== "OTHER", ogData$`Other School Name`[!is.na(ogData$`Other School Name`)]))
  
  b <- duplicated (Data$`Email Address`)
  finalData <- Data[!b,]
  finalData <- finalData[c(2:6,8,14,19,24,29,34,39,44,49,54)]
  numStudentsData <- gather(finalData, class, numberofStudents, 7:15)
  numStudentsData <- numStudentsData[!is.na(numStudentsData$`numberofStudents`),]
  numStudentsData$module <- moduleName
  return(numStudentsData)
}

inLA <- function(districts, dat){
  
  l <- lapply(dat$`School District`, grepl, districts, ignore.case = TRUE, fixed = TRUE) %>% as.logical
  
  return(dat[l,])
  #return(l)
  
}



fm1Link <- "https://docs.google.com/spreadsheets/d/1Q96D8o3HnRYw0qoNHgq0jR9PRN3gwCbtpLBMaj5Z14k/edit#gid=448675731"
fm2Link <- "https://docs.google.com/spreadsheets/d/1eWQqjtDQSpv2oKeEOo0QVL3sR5o0jw03nps-KichCbU/edit#gid=1290383217"
fm3Link <- "https://docs.google.com/spreadsheets/d/1-iwWp5hy89-uLDfFhzS-iCNUda11gF-OnE-WVChUYsE/edit#gid=832850661"
fm4Link <- "https://docs.google.com/spreadsheets/d/1PX9hF-Q206WEvx57maTIRrTPHbrZRlsjayo3-cgWDu0/edit#gid=258031725"
  
wm1Link <- "https://docs.google.com/spreadsheets/d/1zrU_pe5sFbKRbQtO6ep-bnnAUHRDMZYFac3uYWq8gaw/edit#gid=1692958468"
wm2Link <- "https://docs.google.com/spreadsheets/d/1nTI5Ow-fswjrZiArEUZo1UII9pqNKk5zV5N6Ww-kkCQ/edit#gid=208466648"
wm3Link <- "https://docs.google.com/spreadsheets/d/1SeFAheHz2w8jg5FfngnWi9PkMwtTcycTezkzlqK28PA/edit#gid=2034811853"
wm4Link <- "https://docs.google.com/spreadsheets/d/1Jjke-fWCViD5GwvqUBqwZI1cEqdzZCxJ5oV_2wiW1_c/edit#gid=1977870379"

districtList <- read_csv("LACountyDistricts.csv", col_names = FALSE)

fm1Data <- range_read(fm1Link) #%>% removeallNA
fm1Data <- process(fm1Data, "Everyone is an Observer")
fm1LA <- inLA(districtList, fm1Data)
fm1percentFromLA <- (sum(fm1LA$numberofStudents)/ sum(fm1Data$numberofStudents))*100

  
fm2Data <- range_read(fm2Link) #%>% removeallNA
fm2Data <- process(fm2Data, "Clues from Comets")
fm2LA <- inLA(districtList, fm2Data)
fm2percentFromLA <- (sum(fm2LA$numberofStudents)/ sum(fm2Data$numberofStudents))*100

fm3Data <- range_read(fm3Link) #%>% removeallNA
fm3Data <- process(fm3Data, "The Search for Water")
fm3LA <- inLA(districtList, fm3Data)
fm3percentFromLA <- (sum(fm3LA$numberofStudents)/ sum(fm3Data$numberofStudents))*100

fm4Data <- range_read(fm4Link) #%>% removeallNA
fm4Data <- process(fm4Data, "Exoplanets are Everywhere")
fm4LA <- inLA(districtList, fm4Data)
fm4percentFromLA <- (sum(fm4LA$numberofStudents)/ sum(fm4Data$numberofStudents))*100

#------------------------------------------------------------------------------------------------------------------

wm1Data <- range_read(wm1Link) #%>% removeallNA
wm1Data <- process(wm1Data, "Everyone is an Observer")
wm1LA <- inLA(districtList, wm1Data)
wm1percentFromLA <- (sum(wm1LA$numberofStudents)/ sum(wm1Data$numberofStudents))*100

wm2Data <- range_read(wm2Link) #%>% removeallNA
wm2Data <- process(wm2Data, "Clues from Comets")
wm2LA <- inLA(districtList, wm2Data)
wm2percentFromLA <- (sum(as.integer(wm2LA$numberofStudents))/ sum(as.integer(wm2Data$numberofStudents)))*100

wm3Data <- range_read(wm3Link) #%>% removeallNA
wm3Data <- process(wm3Data, "The Search for Water")
wm3LA <- inLA(districtList, wm3Data)
wm3percentFromLA <- (sum(wm3LA$numberofStudents)/ sum(wm3Data$numberofStudents))*100


wm4Data <- range_read(wm4Link) #%>% removeallNA
wm4Data <- process(wm4Data, "Exoplanets are Everywhere")
wm4LA <- inLA(districtList, wm4Data)
wm4percentFromLA <- (sum(wm4LA$numberofStudents)/ sum(wm4Data$numberofStudents))*100

#-------------------------------The Data Added----------------------------------

# Module 1 totals
m1All <- rbind(fm1Data,wm1Data)
m1LA <- rbind(fm1LA,wm1LA)
m1PercentLA <- (sum(m1LA$numberofStudents) / sum(m1All$numberofStudents)) %>% format(digits = 3)

m2All <- rbind(fm2Data, wm2Data)
m2LA <- rbind(fm2LA, wm2LA)
m2PercentLA <- (sum(m2LA$numberofStudents) / sum(m2All$numberofStudents)) %>% format(digits = 3)

m3All <- rbind(fm3Data, wm3Data)
m3LA <- rbind(fm3LA, wm3LA)
m3PercentLA <- (sum(m3LA$numberofStudents) / sum(m3All$numberofStudents)) %>% format(digits = 3)

m4All <- rbind(fm4Data, wm4Data)
m4LA <- rbind(fm4LA, wm4LA)
m4PercentLA <- (sum(m4LA$numberofStudents) / sum(m4All$numberofStudents)) %>% format(digits = 3)

#-------------------------------------------------------------------------------

#------------- Table -----------------------------------------------------------

# allData <- cbind("Module" <-c("Module 1: Everyone is an Observer", "Module 2: Clues from Comets", 
#              "Module 3: The Search for Water", "Module 4: Exoplanets are Everywhere"),
# "% of LA City Students" <- c( m1PercentLA, m2PercentLA,m3PercentLA,m4PercentLA),
# "Total Number of LA Students" <- c(sum(m1LA$numberofStudents), sum(m2LA$numberofStudents), sum(m3LA$numberofStudents), sum(m4LA$numberofStudents)),
# "Total Number of Students" <- c(sum(m1All$numberofStudents), sum(m2All$numberofStudents), sum(m3All$numberofStudents), sum(m4All$numberofStudents))
# )

p <- percent(c( m1PercentLA, m2PercentLA,m3PercentLA,m4PercentLA))
allData <-cbind(c("Module 1: Everyone is an Observer", "Module 2: Clues from Comets",
                  "Module 3: The Search for Water", "Module 4: Exoplanets are Everywhere"),
                p,
                c(sum(m1LA$numberofStudents), sum(m2LA$numberofStudents), sum(m3LA$numberofStudents), sum(m4LA$numberofStudents)),
                c(sum(m1All$numberofStudents), sum(m2All$numberofStudents), sum(m3All$numberofStudents), sum(m4All$numberofStudents))) %>% 
                as.data.frame

colnames(allData) <- c("Module", "% of LA County Students", "Total Number of LA Students", "Total Number of Students")
allData$`% of LA City Students` <- p
write.csv(allData, "percentagesTable.csv", row.names = FALSE )

#-------------------------------------------------------------------------------

#----------- all participating schools and their school districts --------------
lallSchools <- c(fm1Data$`School Name`,fm2Data$`School Name`,fm3Data$`School Name`,fm4Data$`School Name`,wm1Data$`School Name`,wm2Data$`School Name`,wm3Data$`School Name`,wm4Data$`School Name`)
lallDistricts <- c(fm1Data$`School District`,fm2Data$`School District`,fm3Data$`School District`,fm4Data$`School District`,wm1Data$`School District`,wm2Data$`School District`,wm3Data$`School District`,wm4Data$`School District`)
allSchoolsDF <- data_frame(lallSchools,lallDistricts)
b <- !(duplicated(allSchoolsDF$lallSchools))
allSchoolsDF <- allSchoolsDF[b,] %>% as.data.frame

write.csv(allSchoolsDF, "ListofSchools.csv")
#-------------------------------------------------------------------------------
