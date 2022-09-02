library(dplyr)
library(tidyverse)
library(effects)
library(stringr)
library(rebus)
library(modelsummary)
library(ggalluvial)
library(gt)
library(lme4)
library(ggiraph)
library(ggiraphExtra)
library(panelr)
library(kableExtra)
library(nnet)
library(tidyclm)
library(gtsummary)
library(tidyverse)
library(plyr)
library(MASS)
library(ordinal)
library(ggeffects)
library(stargazer)
library(ggpubr)
library(gridExtra)
library(sjPlot)
library(lubridate)
library(emmeans)

setwd("C:/Users/bengo/OneDrive - Nexus365/Documents/Children's Care Homes Project")
options(scipen=999)


rm(list=setdiff(ls(), c("")))

####Provider Data Downloading and Cleaning####

##Download Data##

ProviderData2022 <-  read.csv("Data/providerdata22.csv")
ProviderData2020 <- read.csv("Data/All_SocCare_inspec_1920.csv")
ProviderData2019 <- read.csv("Data/All_SocCare_inspec_1819.csv")
ProviderData2018 <- read.csv("Data/All_SocCare_inspec_1718.csv")
ProviderData2017 <- read.csv("Data/All_SocCare_inspec_1617.csv")
ProviderData2016 <- read.csv("Data/All_SocCare_inspec_1516.csv")
ProviderData2015 <- read.csv("Data/All_SocCare_inspec_1415.csv")
ProviderData2014excch <- read.csv("Data/All_SocCare_exccarehomes_inspec_1314.csv")
ProviderData2014c1 <- read.csv("Data/Carehomes_inspec_14_1.csv")
ProviderData2014c2 <- read.csv("Data/Carehomes_inspec_14_2.csv")

ProviderData2014ch <- rbind(ProviderData2014c1, ProviderData2014c2)

#setdiff(ProviderData2014, ProviderData2014ch)


##Clean Data##
#make column titles and numbers the same

#2014 excluding care homes
ProviderData2014excch$Registration.date <- NA
ProviderData2014excch$Sector <- NA

ProviderData2014excch$Web.link <- NA
ProviderData2014excch$Linked.Education.URN <- NA
names(ProviderData2014excch)[names(ProviderData2014excch)=="Provider.URN"] <- "URN"
names(ProviderData2014excch)[names(ProviderData2014excch)=="Provider.name"] <- "Name"
names(ProviderData2014excch)[names(ProviderData2014excch)=="Provider.address.1"] <- "Address.1"
names(ProviderData2014excch)[names(ProviderData2014excch)=="Provider.address.2"] <- "Address.2"
names(ProviderData2014excch)[names(ProviderData2014excch)=="Provider.town"] <- "Town"
ProviderData2014excch$County <- NA
names(ProviderData2014excch)[names(ProviderData2014excch)=="Provider.postcode"] <- "Postcode"
ProviderData2014excch$Ofsted.region <- NA
ProviderData2014excch$Parliamentary.constituency <- NA
ProviderData2014excch$Places <- NA
names(ProviderData2014excch)[names(ProviderData2014excch)=="Organisation"] <- "Organisation.which.owns.the.provider"
names(ProviderData2014excch)[names(ProviderData2014excch)=="Event.Type.Group"] <- "Event.type"
ProviderData2014excch$Government.Office.Region <- NA

names(ProviderData2014excch)[names(ProviderData2014excch)=="Event.date"] <- "Inspection.date"
ProviderData2014excch$Publication.date <- NA
ProviderData2014excch$Outcomes.in.education.and.related.learning.activities <- NA
ProviderData2014excch$Health.services <- NA
names(ProviderData2014excch)[names(ProviderData2014excch)=="Leadership.and.management"] <- "The.effectiveness.of.leaders.and.managers"
ProviderData2014excch$Short.Breaks.Only <- NA

names(ProviderData2014excch)[names(ProviderData2014excch)=="Overall.effectiveness"] <- "Overall.experiences.and.progress.of.children.and.young.people"
names(ProviderData2014excch)[names(ProviderData2014excch)=="Safeguarding.children.and.young.people"] <- "How.well.children.and.young.people.are.helped.and.protected"
ProviderData2014excch <- ProviderData2014excch[-c(18:22, 24:35)]

names(ProviderData2014excch)[names(ProviderData2014excch)=="Quality.of.service"] <- "Quality.of.care"
names(ProviderData2014excch)[names(ProviderData2014excch)=="Experiences.and.progress.of..and.outcomes.for..children.and.young.people"] <- "Outcomes.for.children.and.young.people"


#2014

ProviderData2014ch$Web.link <- NA
names(ProviderData2014ch)[names(ProviderData2014ch)=="Provider.URN"] <- "URN"
ProviderData2014ch$Linked.Education.URN <- NA
names(ProviderData2014ch)[names(ProviderData2014ch)=="Provider.type"] <- "Provision.type"
ProviderData2014ch$Registration.date <- NA
names(ProviderData2014ch)[names(ProviderData2014ch)=="Provider.status"] <- "Registration.status"
names(ProviderData2014ch)[names(ProviderData2014ch)=="Provider.name"] <- "Name"
names(ProviderData2014ch)[names(ProviderData2014ch)=="Provider.address.1"] <- "Address.1"
names(ProviderData2014ch)[names(ProviderData2014ch)=="Provider.address.2"] <- "Address.2"
names(ProviderData2014ch)[names(ProviderData2014ch)=="Provider.town"] <- "Town"
ProviderData2014ch$County <- NA
names(ProviderData2014ch)[names(ProviderData2014ch)=="Provider.postcode"] <- "Postcode"
ProviderData2014ch$Ofsted.region <- NA
names(ProviderData2014ch)[names(ProviderData2014ch)=="Local.Authority"] <- "Local.authority"
ProviderData2014ch$Parliamentary.constituency <- NA
ProviderData2014ch$Places <- NA
ProviderData2014ch$Organisation.which.owns.the.provider <- NA
names(ProviderData2014ch)[names(ProviderData2014ch)=="Event.type.group"] <- "Event.type"
names(ProviderData2014ch)[names(ProviderData2014ch)=="government_office_region_name"] <- "Government.Office.Region"

names(ProviderData2014ch)[names(ProviderData2014ch)=="Event.date"] <- "Inspection.date"
ProviderData2014ch$Publication.date <- NA
ProviderData2014ch$Outcomes.in.education.and.related.learning.activities <- NA
ProviderData2014ch$Health.services <- NA
names(ProviderData2014ch)[names(ProviderData2014ch)=="Leadership.and.management"] <- "The.effectiveness.of.leaders.and.managers"
ProviderData2014ch$Short.Breaks.Only <- NA

names(ProviderData2014ch)[names(ProviderData2014ch)=="Overall.effectiveness"] <- "Overall.experiences.and.progress.of.children.and.young.people"
names(ProviderData2014ch)[names(ProviderData2014ch)=="Safeguarding.children.and.young.people"] <- "How.well.children.and.young.people.are.helped.and.protected"

ProviderData2020$Outcomes.for.children.and.young.people <- NA
ProviderData2020$Quality.of.care <- NA

#setdiff(ProviderData2020, ProviderData2014ch)

#2015
names(ProviderData2015)[names(ProviderData2015)=="Linked.ED.URN"] <- "Linked.Education.URN"
names(ProviderData2015)[names(ProviderData2015)=="Reg.Status"] <- "Registration.status"
ProviderData2015$Registration.date <- NA
names(ProviderData2015)[names(ProviderData2015)=="Setting.Name"] <- "Name"
names(ProviderData2015)[names(ProviderData2015)=="Inspection.type"] <- "Event.type"
names(ProviderData2015)[names(ProviderData2015)=="Short.breaks.only"] <- "Short.Breaks.Only"

ProviderData2015$Ofsted.region <- NA
ProviderData2015$Places <- NA
ProviderData2015$Organisation.which.owns.the.provider <- NA
ProviderData2015$Publication.date <- NA


names(ProviderData2015)[names(ProviderData2015)=="Region"] <- "Government.Office.Region"
names(ProviderData2015)[names(ProviderData2015)=="Inspection.Type"] <- "Event.type"
names(ProviderData2015)[names(ProviderData2015)=="Inspection.Date"] <- "Inspection.date"
names(ProviderData2015)[names(ProviderData2015)=="Inspection.ID"] <- "Event.number"
ProviderData2014$Short.Breaks.Only <- NA
ProviderData2020$Short.Breaks.Only <- NA
names(ProviderData2015)[names(ProviderData2015)=="Leadership.and.management"] <- "The.effectiveness.of.leaders.and.managers"
ProviderData2015$Health.services <- NA
ProviderData2015$Parliamentary.constituency <- NA

names(ProviderData2015)[names(ProviderData2015)=="Overall.effectiveness"] <- "Overall.experiences.and.progress.of.children.and.young.people"
names(ProviderData2015)[names(ProviderData2015)=="ï..Web.link"] <- "Web.link"
names(ProviderData2015)[names(ProviderData2015)=="The.safety.of.children.and.young.people"] <- "How.well.children.and.young.people.are.helped.and.protected"


#setdiff(ProviderData2020, ProviderData2015)

#2016
names(ProviderData2016)[names(ProviderData2016)=="Linked.education.URN"] <- "Linked.Education.URN"
names(ProviderData2016)[names(ProviderData2016)=="Registration.Status"] <- "Registration.status"

ProviderData2016$Registration.date <- NA
ProviderData2016$Ofsted.region <- NA
ProviderData2016$Parliamentary.constituency <- NA
ProviderData2016$Organisation.which.owns.the.provider <- NA
ProviderData2016$Publication.date <- NA
ProviderData2016$Short.Breaks.Only <- NA
names(ProviderData2016)[names(ProviderData2016)=="Leadership.and.management"] <- "The.effectiveness.of.leaders.and.managers"
ProviderData2016$Outcomes.in.education.and.related.learning.activities <- NA
ProviderData2016$Health.services <- NA

names(ProviderData2016)[names(ProviderData2016)=="Overall.effectiveness"] <- "Overall.experiences.and.progress.of.children.and.young.people"
names(ProviderData2016)[names(ProviderData2016)=="Safeguarding.children.and.young.people"] <- "How.well.children.and.young.people.are.helped.and.protected"


#2017
names(ProviderData2017)[names(ProviderData2017)=="Short.breaks.only"] <- "Short.Breaks.Only"
names(ProviderData2017)[names(ProviderData2017)=="Ofsted.Region"] <- "Ofsted.region"

ProviderData2017$Name <- NA
ProviderData2017$Parliamentary.constituency <- NA
ProviderData2017$Organisation.which.owns.the.provider <- NA
ProviderData2017$Publication.date <- NA
ProviderData2017$Outcomes.in.education.and.related.learning.activities <- NA


ProviderData2017$Health.services <- NA
ProviderData2017$Registration.date <- NA
names(ProviderData2017)[names(ProviderData2017)=="Organisation.which.owns.the.children.s.home"] <- "Organisation.which.owns.the.provider"
names(ProviderData2017)[names(ProviderData2017)=="Registration.Status"] <- "Registration.status"

names(ProviderData2017)[names(ProviderData2017)=="Overall.effectiveness"] <- "Overall.experiences.and.progress.of.children.and.young.people"
names(ProviderData2017)[names(ProviderData2017)=="Safeguarding.children.and.young.people"] <- "How.well.children.and.young.people.are.helped.and.protected"
names(ProviderData2017)[names(ProviderData2017)=="Leadership.and.management"] <- "The.effectiveness.of.leaders.and.managers"

#2018
names(ProviderData2018)[names(ProviderData2018)=="Registration.Status"] <- "Registration.status"
ProviderData2018$Outcomes.in.education.and.related.learning.activities <- NA

names(ProviderData2018)[names(ProviderData2018)=="Overall.effectiveness"] <- "Overall.experiences.and.progress.of.children.and.young.people"
names(ProviderData2018)[names(ProviderData2018)=="Safeguarding.children.and.young.people"] <- "How.well.children.and.young.people.are.helped.and.protected"
names(ProviderData2018)[names(ProviderData2018)=="Leadership.and.management"] <- "The.effectiveness.of.leaders.and.managers"
names(ProviderData2018)[names(ProviderData2018)=="Ofsted.Region"] <- "Ofsted.region"


ProviderData2018$Short.Breaks.Only <- NA
ProviderData2018$Health.services <- NA
ProviderData2018$Registration.date <- NA
names(ProviderData2018)[names(ProviderData2018)=="Organisation"] <- "Organisation.which.owns.the.provider"


#2019

names(ProviderData2019)[names(ProviderData2019)=="ï..Web.link"] <- "Web.link"

ProviderData2019$Short.Breaks.Only <- NA
ProviderData2019$Outcomes.for.children.and.young.people <- NA
ProviderData2019$Health.services <- NA
ProviderData2019$Quality.of.care <- NA

ProviderData2022$Outcomes.for.children.and.young.people <- NA
ProviderData2022$Health.services <- NA
ProviderData2022$Quality.of.care <- NA

names(ProviderData2022)[names(ProviderData2022)=="Short.break.only.children.s.home"] <- "Short.Breaks.Only"


#check for any different column names
setdiff(names(ProviderData2014excch), names(ProviderData2022))


##Spelling differences corrected##


ProviderData2018$Organisation.which.owns.the.provider[ProviderData2018$Organisation.which.owns.the.provider== "Acorn Homes (uk) Ltd"] <- "Acorn Homes (UK) Limited"
ProviderData2018$Organisation.which.owns.the.provider[ProviderData2018$Organisation.which.owns.the.provider== "Broadwood Residential Ltd"] <- "Broadwood Residential Limited"
ProviderData2018$Organisation.which.owns.the.provider[ProviderData2018$Organisation.which.owns.the.provider== "Headway Adolescent Resources Ltd"] <- "Headway Adolescent Resources Limited"
ProviderData2018$Organisation.which.owns.the.provider[ProviderData2018$Organisation.which.owns.the.provider== "Hillcrest Children's Services Ltd"] <- "Hillcrest Childrens Services Limited"
ProviderData2018$Organisation.which.owns.the.provider[ProviderData2018$Organisation.which.owns.the.provider== "Kisimul Group Ltd"] <- "Kisimul Group Limited"
ProviderData2018$Organisation.which.owns.the.provider[ProviderData2018$Organisation.which.owns.the.provider== "Priory Education Services Limited 06244880"] <- "Priory Education Services Limited"
ProviderData2018$Organisation.which.owns.the.provider[ProviderData2018$Organisation.which.owns.the.provider== "The Adolescent And Children's Trust (tact)"] <- "The Adolescent and Children's Trust"
ProviderData2018$Organisation.which.owns.the.provider[ProviderData2018$Organisation.which.owns.the.provider== "The SENAD Group Limited"] <- "The Senad Group Limited"
ProviderData2018$Organisation.which.owns.the.provider[ProviderData2018$Organisation.which.owns.the.provider== "Time-Out Children's Homes Limited"] <- "Time-Out Childrens Homes Limited"
ProviderData2018$Organisation.which.owns.the.provider[ProviderData2018$Organisation.which.owns.the.provider== "Cambian Childcare Ltd"] <- "Cambian Childcare Limited"

ProviderData2019$Organisation.which.owns.the.provider[ProviderData2019$Organisation.which.owns.the.provider== "Acorn Children's Homes (Branston) Ltd"] <- "Acorn Children's Home (Branston) Ltd"
ProviderData2019$Organisation.which.owns.the.provider[ProviderData2019$Organisation.which.owns.the.provider== "Action For Children"] <- "Action for Children"
ProviderData2019$Organisation.which.owns.the.provider[ProviderData2019$Organisation.which.owns.the.provider== "Inspire Social Care Services Ltd"] <- "Inspire Social Care Services Limited"
ProviderData2019$Organisation.which.owns.the.provider[ProviderData2019$Organisation.which.owns.the.provider== "Pathway Care Solutions Ltd 04004053"] <- "Pathway Care Solutions Ltd"
ProviderData2019$Organisation.which.owns.the.provider[ProviderData2019$Organisation.which.owns.the.provider== "The Partnership Of Care Today Children's Services"] <- "The Partnership of Care Today Children's Services"
ProviderData2019$Organisation.which.owns.the.provider[ProviderData2019$Organisation.which.owns.the.provider== "The SENAD Group Limited"] <- "The Senad Group Limited"
ProviderData2019$Organisation.which.owns.the.provider[ProviderData2019$Organisation.which.owns.the.provider== "Cambian Childcare Ltd"] <- "Cambian Childcare Limited"

ProviderData2020$Organisation.which.owns.the.provider[ProviderData2020$Organisation.which.owns.the.provider== "Pathway Care Solutions Ltd 04004053"] <- "Pathway Care Solutions Ltd"
ProviderData2020$Organisation.which.owns.the.provider[ProviderData2020$Organisation.which.owns.the.provider== "Social Care Services Ltd"] <- "Social Care Services Limited"
ProviderData2020$Organisation.which.owns.the.provider[ProviderData2020$Organisation.which.owns.the.provider== "The SENAD Group Limited"] <- "The Senad Group Limited"
ProviderData2020$Organisation.which.owns.the.provider[ProviderData2020$Organisation.which.owns.the.provider== "Cambian Childcare Ltd"] <- "Cambian Childcare Limited"

ProviderData2022$Organisation.which.owns.the.provider[ProviderData2022$Organisation.which.owns.the.provider== "Pathway Care Solutions Ltd 04004053"] <- "Pathway Care Solutions Ltd"
ProviderData2022$Organisation.which.owns.the.provider[ProviderData2022$Organisation.which.owns.the.provider== "Social Care Services Ltd"] <- "Social Care Services Limited"
ProviderData2022$Organisation.which.owns.the.provider[ProviderData2022$Organisation.which.owns.the.provider== "The SENAD Group Limited"] <- "The Senad Group Limited"
ProviderData2022$Organisation.which.owns.the.provider[ProviderData2022$Organisation.which.owns.the.provider== "Cambian Childcare Ltd"] <- "Cambian Childcare Limited"

#work out annual chain sizes


chainsizedata18 <- ProviderData2018 %>% group_by_("URN") %>% mutate(keep = row_number())
chainsizedata18 <-chainsizedata18[which(chainsizedata18$keep==1),]
chainsizedata19 <- ProviderData2019 %>% group_by_("URN") %>% mutate(keep = row_number())
chainsizedata19 <-chainsizedata19[which(chainsizedata19$keep==1),]
chainsizedata20 <- ProviderData2020 %>% group_by_("URN") %>% mutate(keep = row_number())
chainsizedata20 <-chainsizedata20[which(chainsizedata20$keep==1),]
chainsizedata22 <- ProviderData2022 %>% group_by_("URN") %>% mutate(keep = row_number())
chainsizedata22 <-chainsizedata22[which(chainsizedata22$keep==1),]



chainsize18 <- chainsizedata18[c(18)]
chainsize19 <- chainsizedata19[c(19)]
chainsize20 <- chainsizedata20[c(19)]
chainsize22 <- chainsizedata22[c(20)]




chainsize18 <- chainsize18 %>% mutate(chainsize= ifelse(chainsize18$Organisation.which.owns.the.provider=="", 0,1 ))
chainsize19 <- chainsize19 %>% mutate(chainsize= ifelse(chainsize19$Organisation.which.owns.the.provider=="", 0,1 ))
chainsize20 <- chainsize20 %>% mutate(chainsize= ifelse(chainsize20$Organisation.which.owns.the.provider=="", 0,1 ))
chainsize22 <- chainsize22 %>% mutate(chainsize= ifelse(chainsize22$Organisation.which.owns.the.provider=="", 0,1 ))

chainsize18 <- aggregate(. ~Organisation.which.owns.the.provider, data=chainsize18, sum)
chainsize19 <- aggregate(. ~Organisation.which.owns.the.provider, data=chainsize19, sum)
chainsize20 <- aggregate(. ~Organisation.which.owns.the.provider, data=chainsize20, sum)
chainsize22 <- aggregate(. ~Organisation.which.owns.the.provider, data=chainsize22, sum)

ProviderData2018 <- merge(ProviderData2018, chainsize18, by="Organisation.which.owns.the.provider", all.x=T)
ProviderData2019 <- merge(ProviderData2019, chainsize19, by="Organisation.which.owns.the.provider", all.x=T)
ProviderData2020 <- merge(ProviderData2020, chainsize20, by="Organisation.which.owns.the.provider", all.x=T)
ProviderData2022 <- merge(ProviderData2022, chainsize22, by="Organisation.which.owns.the.provider", all.x=T)

ProviderData2014ch$chainsize <- NA
ProviderData2014excch$chainsize <- NA
ProviderData2015$chainsize <- NA
ProviderData2016$chainsize <- NA
ProviderData2017$chainsize <- NA

imputechain18 <- ProviderData2018[c(3,32)]
imputechain18$chainsize_18 <- imputechain18$chainsize

# imputechain19 <- ProviderData2019[c(3,32)]
# imputechain19$chainsize_imputed <- imputechain19$chainsize
# 
# imputechain20 <- ProviderData2020[c(3,32)]
# imputechain20$chainsize_imputed <- imputechain20$chainsize
# 
# 
imputechain18 <- imputechain18[-c(2)]
# imputechain19 <- imputechain19[-c(2)]
# imputechain20 <- imputechain20[-c(2)]
# 
# imputechain <- rbind(imputechain18, imputechain19, imputechain20)
# 
# imputechain <- unique(imputechain$URN)


#bind together the datasets
ProviderData <- rbind(ProviderData2020,ProviderData2022, ProviderData2014ch,ProviderData2014excch, ProviderData2015, ProviderData2016, ProviderData2017, ProviderData2018, ProviderData2019)

ProviderData <- merge(ProviderData, imputechain18, by="URN", all.x=T)

ProviderData$chainsize_imputed <- ProviderData$chainsize
ProviderData$chainsize_imputed <- ifelse(is.na(ProviderData$chainsize_imputed), ProviderData$chainsize_18, ProviderData$chainsize_imputed)

ProviderData$chainsize[ProviderData$chainsize== 0] <- NA
ProviderData$chainsize_imputed[ProviderData$chainsize_imputed== 0] <- NA


ProviderData$Overall.experiences.and.progress.of.children.and.young.people[ProviderData$Overall.experiences.and.progress.of.children.and.young.people == "Requires improvement"] <- "Requires improvement to be good"
ProviderData$Outcomes.in.education.and.related.learning.activities[ProviderData$Outcomes.in.education.and.related.learning.activities == "Requires improvement"] <- "Requires improvement to be good"
ProviderData$How.well.children.and.young.people.are.helped.and.protected[ProviderData$How.well.children.and.young.people.are.helped.and.protected == "Requires improvement"] <- "Requires improvement to be good"
ProviderData$The.effectiveness.of.leaders.and.managers[ProviderData$The.effectiveness.of.leaders.and.managers == "Requires improvement"] <- "Requires improvement to be good"

ProviderData$Overall.experiences.and.progress.of.children.and.young.people[ProviderData$Overall.experiences.and.progress.of.children.and.young.people == "Adequate"] <- "Requires improvement to be good"
ProviderData$Outcomes.in.education.and.related.learning.activities[ProviderData$Outcomes.in.education.and.related.learning.activities == "Adequate"] <- "Requires improvement to be good"
ProviderData$How.well.children.and.young.people.are.helped.and.protected[ProviderData$How.well.children.and.young.people.are.helped.and.protected == "Adequate"] <- "Requires improvement to be good"
ProviderData$The.effectiveness.of.leaders.and.managers[ProviderData$The.effectiveness.of.leaders.and.managers == "Adequate"] <- "Requires improvement to be good"


ProviderData$Sector[ProviderData$Sector == "Voluntary "] <- "Voluntary"
ProviderData$Sector[ProviderData$Sector == "Local authority"] <- "Local Authority"
ProviderData$Sector[ProviderData$Sector == "Health authority"] <- "Health Authority"

ProviderData$Provision.type[ProviderData$Provision.type == "Independent Fostering Provider"] <- "Independent Fostering Agency"
ProviderData$Provision.type[ProviderData$Provision.type == "Children's Home"] <- "Children's home"


#Identify inspections that are included in several annual data releases
n_occur <- data.frame(table(ProviderData$Event.number))

yep <- n_occur[n_occur$Freq > 1,]

#Remove repeated inspections
ProviderData <- ProviderData[!duplicated(ProviderData$Event.number), ]


#add requirement data - merging by event number

CHrequirements <- read.csv("Data/Requirements_ch_ifa.csv")
CHrecs <- read.csv("Data/Recommendations_ch_ifa.csv")

CHrequirements <- unique(CHrequirements)
CHrecs <- unique(CHrecs)

names(CHrequirements)[names(CHrequirements)=="Event"] <- "Event.number"
names(CHrecs)[names(CHrecs)=="Event"] <- "Event.number"

Reqbin <-CHrequirements

Reqbin <- Reqbin[c(3)]
Reqbin <- unique(Reqbin)
Reqbin$Requirement.binary <- 1

reqcount <- data.frame(table(CHrequirements$Event.number))
names(reqcount)[names(reqcount)=="Var1"] <- "Event.number"
names(reqcount)[names(reqcount)=="Freq"] <- "Number.of.Requirements"


library(dplyr)
library(tidyverse)
library(plyr)

CHrequirements <- CHrequirements[c(3,4)]


CHrequirements <- CHrequirements %>%                           
  group_by(Event.number) %>% 
  dplyr::mutate(ReqNo = row_number())


CHrequirements <- CHrequirements %>% 
  pivot_wider(names_from = ReqNo, values_from = Requirement,
              names_prefix = "Requirement_Number_")

ProviderData <- merge(ProviderData, Reqbin, by= "Event.number", all.x=T)
ProviderData <- merge(ProviderData, reqcount, by= "Event.number", all.x=T)
ProviderData <- merge(ProviderData, CHrequirements, by= "Event.number", all.x=T)



Reqbin <-CHrecs

Reqbin <- Reqbin[c(5)]
Reqbin <- unique(Reqbin)
Reqbin$Recommendation.binary <- 1

reqcount <- data.frame(table(CHrecs$Event.number))
names(reqcount)[names(reqcount)=="Var1"] <- "Event.number"
names(reqcount)[names(reqcount)=="Freq"] <- "Number.of.Reccomendations"


library(dplyr)
library(tidyverse)
library(plyr)

CHrecs <- CHrecs[c(4,5)]


CHrecs <- CHrecs %>%                           
  group_by(Event.number) %>% 
  dplyr::mutate(RecNo = row_number())


CHrecs <- CHrecs %>% 
  pivot_wider(names_from = RecNo, values_from = Description,
              names_prefix = "Recommendation_Number_")

ProviderData <- merge(ProviderData, Reqbin, by= "Event.number", all.x=T)
ProviderData <- merge(ProviderData, reqcount, by= "Event.number", all.x=T)
ProviderData <- merge(ProviderData, CHrecs, by= "Event.number", all.x=T)




#write the data to csv
#write.csv(ProviderData, "Data/ProviderData_final.csv")

#write.csv(paneldata, "Data/LaData_final.csv")

#remove unwanted objects
rm(list=setdiff(ls(), c("ProviderData")))



###LA Data Downloading and Cleaning##### 

#Download data
LAData2020 <- read.csv("Data/LAs_alldates.csv")

under18popasof2019 <- read.csv("Data/under18la.csv")
#IDACI2019 <- read.csv("Data/IDACI2019.csv")
idacilarge <- read.csv("Data/idacilarge.csv")
#IMD2019 <- read.csv("Data/IMD2019.csv")
imdlarge <- read.csv("Data/imdlarge.csv")
#IMD <- rbind(IMD2019, imdlarge)
#IDACI <- rbind(IDACI2019, idacilarge)

#remove duplicated authorities in LA and UA IMD files
IDACI <-idacilarge
IMD <- imdlarge


ExpenditureData <- read.csv("Data/LA_Care_Expenditure_By_Ownership_OverTime.csv")
ExpenditureData <- ExpenditureData[which(ExpenditureData$geographic_level =="Local authority"),]
expenditure2020 <- ExpenditureData[which(ExpenditureData$ï..time_period=="201920"),]
expenditure2019 <- ExpenditureData[which(ExpenditureData$ï..time_period=="201819"),]
expenditure2018 <- ExpenditureData[which(ExpenditureData$ï..time_period=="201718"),]
expenditure2017 <- ExpenditureData[which(ExpenditureData$ï..time_period=="201617"),]
expenditure2016 <- ExpenditureData[which(ExpenditureData$ï..time_period=="201516"),]
expenditure2015 <- ExpenditureData[which(ExpenditureData$ï..time_period=="201415"),]


expenditure2020 <- expenditure2020[which(expenditure2020$Description=="3.1.1 Residential care"),]
expenditure2020 <- expenditure2020[c(10,13:19)]
expenditure2018 <- expenditure2018[which(expenditure2018$Description=="3.1.1 Residential care"),]
expenditure2018 <- expenditure2018[c(10,13:19)]
expenditure2019 <- expenditure2019[which(expenditure2019$Description=="3.1.1 Residential care"),]
expenditure2019 <- expenditure2019[c(10,13:19)]
expenditure2017 <- expenditure2017[which(expenditure2017$Description=="3.1.1 Residential care"),]
expenditure2017 <- expenditure2017[c(10,13:19)]
expenditure2016 <- expenditure2016[which(expenditure2016$Description=="3.1.1 Residential care"),]
expenditure2016 <- expenditure2016[c(10,13:19)]
expenditure2015 <- expenditure2015[which(expenditure2015$Description=="3.1.1 Residential care"),]
expenditure2015 <- expenditure2015[c(10,13:19)]

names(expenditure2020)[names(expenditure2020)=="OwnProvision"] <- "OwnProvision_spend_1000s_2020"

names(expenditure2020)[names(expenditure2020)=="PrivateProvision"] <- "PrivateProvision_spend_1000s_2020"
names(expenditure2020)[names(expenditure2020)=="OtherPublic"] <- "OtherPublic_spend_1000s_2020"
names(expenditure2020)[names(expenditure2020)=="Voluntary"] <- "Voluntary_spend_1000s_2020"
names(expenditure2020)[names(expenditure2020)=="TotalExpenditure"] <- "TotalExpenditure_spend_1000s_2020"


names(expenditure2019)[names(expenditure2019)=="OwnProvision"] <- "OwnProvision_spend_1000s_2019"

names(expenditure2019)[names(expenditure2019)=="PrivateProvision"] <- "PrivateProvision_spend_1000s_2019"
names(expenditure2019)[names(expenditure2019)=="OtherPublic"] <- "OtherPublic_spend_1000s_2019"
names(expenditure2019)[names(expenditure2019)=="Voluntary"] <- "Voluntary_spend_1000s_2019"
names(expenditure2019)[names(expenditure2019)=="TotalExpenditure"] <- "TotalExpenditure_spend_1000s_2019"

names(expenditure2018)[names(expenditure2018)=="OwnProvision"] <- "OwnProvision_spend_1000s_2018"

names(expenditure2018)[names(expenditure2018)=="PrivateProvision"] <- "PrivateProvision_spend_1000s_2018"
names(expenditure2018)[names(expenditure2018)=="OtherPublic"] <- "OtherPublic_spend_1000s_2018"
names(expenditure2018)[names(expenditure2018)=="Voluntary"] <- "Voluntary_spend_1000s_2018"
names(expenditure2018)[names(expenditure2018)=="TotalExpenditure"] <- "TotalExpenditure_spend_1000s_2018"

names(expenditure2017)[names(expenditure2017)=="OwnProvision"] <- "OwnProvision_spend_1000s_2017"

names(expenditure2017)[names(expenditure2017)=="PrivateProvision"] <- "PrivateProvision_spend_1000s_2017"
names(expenditure2017)[names(expenditure2017)=="OtherPublic"] <- "OtherPublic_spend_1000s_2017"
names(expenditure2017)[names(expenditure2017)=="Voluntary"] <- "Voluntary_spend_1000s_2017"
names(expenditure2017)[names(expenditure2017)=="TotalExpenditure"] <- "TotalExpenditure_spend_1000s_2017"


names(expenditure2016)[names(expenditure2016)=="OwnProvision"] <- "OwnProvision_spend_1000s_2016"

names(expenditure2016)[names(expenditure2016)=="PrivateProvision"] <- "PrivateProvision_spend_1000s_2016"
names(expenditure2016)[names(expenditure2016)=="OtherPublic"] <- "OtherPublic_spend_1000s_2016"
names(expenditure2016)[names(expenditure2016)=="Voluntary"] <- "Voluntary_spend_1000s_2016"
names(expenditure2016)[names(expenditure2016)=="TotalExpenditure"] <- "TotalExpenditure_spend_1000s_2016"

names(expenditure2015)[names(expenditure2015)=="OwnProvision"] <- "OwnProvision_spend_1000s_2015"

names(expenditure2015)[names(expenditure2015)=="PrivateProvision"] <- "PrivateProvision_spend_1000s_2015"
names(expenditure2015)[names(expenditure2015)=="OtherPublic"] <- "OtherPublic_spend_1000s_2015"
names(expenditure2015)[names(expenditure2015)=="Voluntary"] <- "Voluntary_spend_1000s_2015"
names(expenditure2015)[names(expenditure2015)=="TotalExpenditure"] <- "TotalExpenditure_spend_1000s_2015"


expenditure2020 <- expenditure2020[c(1:6)]
expenditure2019 <- expenditure2019[c(1:6)]
expenditure2018 <- expenditure2018[c(1:6)]
expenditure2017 <- expenditure2017[c(1:6)]
expenditure2016 <- expenditure2016[c(1:6)]
expenditure2015 <- expenditure2015[c(1:6)]



unitcostData <- read.csv("Data/unit_costs_sept_csv.csv")
unitcostres <- read.csv("Data/unit_cost_residential.csv")
unitcostfost <- read.csv("Data/unit_cost_fostering.csv")
unitcostadopt <- read.csv("Data/unit_cost_adoption.csv")
unitcostsen <- read.csv("Data/unit_cost_sen.csv")
unitcostsocwork <- read.csv("Data/unit_cost_soc_work.csv")


unitcostres <- unitcostres[-c(1)]
unitcostfost <- unitcostfost[-c(1)]
unitcostadopt <- unitcostadopt[-c(1)]
unitcostsen <- unitcostsen[-c(1)]
unitcostsocwork <- unitcostsocwork[-c(1)]


childrenstats <- read.csv("Data/Children characteristics by LA.csv")

#Clean data#
childrenstats <- childrenstats[which(childrenstats$time_period==2020),]
childrenstats <- childrenstats[which(childrenstats$LAD19CD!=""),]

childrenstatsno <- childrenstats[c(10,12,13)]
childrenstatsper <- childrenstats[c(10,12,14)]
childrenstatsno <- unique(childrenstatsno)
childrenstatsper <- unique(childrenstatsper)

childrenstatsper <- childrenstatsper[which(childrenstatsper$Characteristic!= "Total children"),]
childrenstatsno <- childrenstatsno[which(childrenstatsno$Characteristic!= "Total children"),]



library(tidyr)

#Turn data from panel to wide
childrenwideno <- childrenstatsno %>% pivot_wider(names_from = Characteristic, values_from = Number, names_prefix = "Count")
childrenwideper <- childrenstatsper %>% pivot_wider(names_from = Characteristic, values_from = Percentage, names_prefix = "CHARACTERISTICS (%) ")

childrenmissing <- read.csv("Data/Missing children by LA.csv")
childrenmissing <- childrenmissing[which(childrenmissing$ï..time_period==2020),]
childrenmissing <- childrenmissing[which(childrenmissing$LAD19CD!=""),]

childrenmissing <- childrenmissing[c(10:12)]
childrenmissing <- unique(childrenmissing)
childrenmisswide <- childrenmissing %>% pivot_wider(names_from = CLA_group, values_from = Number, names_prefix = "MISSING_STATS ")


childrenhealth <- read.csv("Data/Conviction and health outcomes by LA.csv")
childrenhealth <- childrenhealth[which(childrenhealth$ï..time_period==2020),]
childrenhealth <- childrenhealth[which(childrenhealth$LAD19CD!=""),]

childrenhealth <- childrenhealth[c(10, 12, 14)]
childrenhealth <- unique(childrenhealth)
childrenhealthwide <- childrenhealth %>% pivot_wider(names_from = characteristic, values_from = percent, names_prefix = "CONVICTION STATS (%) ")

childrenintake <- read.csv("Data/Intake rates by LA.csv")
childrenintake <- childrenintake[which(childrenintake$time_period==2020),]
childrenintake <- childrenintake[which(childrenintake$LAD19CD!=""),]

childrenintake <- childrenintake[c(10, 11, 14)]
childrenintake <- unique(childrenintake)
childrenintakewide <- childrenintake %>% pivot_wider(names_from = population_count, values_from = rate_per_10000, names_prefix = "INTAKE STATS (rate per 10,000) ")

shortterm <- read_csv("Data/children_looked_after_exclusively_under_a_series_of_short_term_placements.csv")




#merge data

LAData<- merge(LAData2020,under18popasof2019, by="LAD19NM", all.x=T) 
# LAData <- merge(LAData, IDACI, by="LAD19CD", all=T)
# LAData <- merge(LAData, IMD, by="LAD19CD", all=T)
# LAData <- merge(LAData, unitcostData, by="LAD19NM", all=T)
# LAData <- merge(LAData, unitcostres, by="LAD19NM", all=T)
# LAData <- merge(LAData, unitcostfost, by="LAD19NM", all=T)
# LAData <- merge(LAData, unitcostadopt, by="LAD19NM", all=T)
# LAData <- merge(LAData, unitcostsen, by="LAD19NM", all=T)
# LAData <- merge(LAData, unitcostsocwork, by="LAD19NM", all=T)
# 
# LAData <- merge(LAData, expenditure2019, by="LAD19CD", all=T)
# LAData <- merge(LAData, expenditure2020, by="LAD19CD", all=T)
# LAData <- merge(LAData, expenditure2018, by="LAD19CD", all=T)
# LAData <- merge(LAData, expenditure2017, by="LAD19CD", all=T)
# LAData <- merge(LAData, expenditure2016, by="LAD19CD", all=T)
# LAData <- merge(LAData, expenditure2015, by="LAD19CD", all=T)
# LAData <- merge(LAData, childrenwideper, by="LAD19CD", all=T)
# LAData <- merge(LAData, childrenmisswide, by="LAD19CD", all=T)
# LAData <- merge(LAData, childrenhealthwide, by="LAD19CD", all=T)
# LAData <- merge(LAData, childrenintakewide, by="LAD19CD", all=T)
# LAData <- merge(LAData, shortterm, by="LAD19CD", all.x=T)
# LAData <- merge(LAData, schlexlusion, by="LAD19CD", all.x=T)
# LAData <- merge(LAData, ks4, by="LAD19CD", all.x=T)
# LAData <- merge(LAData, ks2achieving, by="LAD19CD", all.x=T)
# LAData <- merge(LAData, ks2number, by="LAD19CD", all.x=T)
# LAData <- merge(LAData, ks2percentachieving, by="LAD19CD", all.x=T)
# LAData <- merge(LAData, ks2progressaverage, by="LAD19CD", all.x=T)
# LAData <- merge(LAData, ks2progresselig, by="LAD19CD", all.x=T)
# LAData <- merge(LAData, ks2progressscore, by="LAD19CD", all.x=T)
# LAData <- merge(LAData, schlabsence, by="LAD19CD", all.x=T)
# 
# LAData <- as.data.frame(LAData)
# 



####Expenditure data pre2014####

outturn0809 <- read.csv("Data/outturn_0809.csv")
outturn0910 <- read.csv("Data/outturn_0910.csv")
outturn1011 <- read.csv("Data/outturn_1011.csv")
outturn1112 <- read.csv("Data/outturn_1112.csv")
outturn1213 <- read.csv("Data/outturn_1213.csv")
outturn1314 <- read.csv("Data/outturn_1314.csv")




#keep just residential care spend
outturn0809CLA <- outturn0809[which(outturn0809$S52.Line.Reference.=="14 Total Children Looked After"),]
outturn0809 <- outturn0809[which(outturn0809$S52.Line.Reference.=="5 Residential care"),]

outturn0910CLA <- outturn0910[which(outturn0910$S52.Line.Reference =="Total Children Looked After"),]
outturn0910 <- outturn0910[which(outturn0910$S52.Line.Reference =="Residential care"),]

outturn1011CLA <- outturn1011[which(outturn1011$S52.Line.Reference =="Total Children Looked After"),]
outturn1011 <- outturn1011[which(outturn1011$S52.Line.Reference =="Residential care"),]

outturn1112CLA <- outturn1112[which(outturn1112$LineNumber =="12"),]
outturn1112 <- outturn1112[which(outturn1112$LineNumber =="4"),]

outturn1213CLA <- outturn1213[which(outturn1213$LineNumber =="15"),]
outturn1213 <- outturn1213[which(outturn1213$LineNumber =="5"),]

outturn1314CLA <- outturn1314[which(outturn1314$LineNumber =="16"),]
outturn1314 <- outturn1314[which(outturn1314$LineNumber =="6"),]

#Standardise variable names

names(outturn0809)[names(outturn0809)=="PRIVATE..x."] <- "PrivateProvision_spend_1000s_2009"
names(outturn0809)[names(outturn0809)=="VOLUNTARY..y."] <- "Voluntary_spend_1000s_2009"
names(outturn0809)[names(outturn0809)=="PUBLIC..z."] <- "OwnProvision_spend_1000s_2009"
names(outturn0809)[names(outturn0809)=="TOTAL.EXPENDITURE..k."] <- "TotalExpenditure_spend_1000s_2009"

names(outturn0809CLA)[names(outturn0809CLA)=="PRIVATE..x."] <- "TotalCLA_PrivateProvision_spend_1000s_2009"
names(outturn0809CLA)[names(outturn0809CLA)=="VOLUNTARY..y."] <- "TotalCLA_Voluntary_spend_1000s_2009"
names(outturn0809CLA)[names(outturn0809CLA)=="PUBLIC..z."] <- "TotalCLA_OwnProvision_spend_1000s_2009"
names(outturn0809CLA)[names(outturn0809CLA)=="TOTAL.EXPENDITURE..k."] <- "TotalCLA_TotalExpenditure_spend_1000s_2009"


names(outturn0910)[names(outturn0910)=="Private..z.i.."] <- "PrivateProvision_spend_1000s_2010"
names(outturn0910)[names(outturn0910)=="Voluntary..z.iii.."] <- "Voluntary_spend_1000s_2010"
names(outturn0910)[names(outturn0910)=="Own.Provision..y."] <- "OwnProvision_spend_1000s_2010"
names(outturn0910)[names(outturn0910)=="Total.Expenditure..k."] <- "TotalExpenditure_spend_1000s_2010"
names(outturn0910)[names(outturn0910)=="Other.Public..z.ii.."] <- "OtherPublic_spend_1000s_2010"

names(outturn0910CLA)[names(outturn0910CLA)=="Private..z.i.."] <- "TotalCLA_PrivateProvision_spend_1000s_2010"
names(outturn0910CLA)[names(outturn0910CLA)=="Voluntary..z.iii.."] <- "TotalCLA_Voluntary_spend_1000s_2010"
names(outturn0910CLA)[names(outturn0910CLA)=="Own.Provision..y."] <- "TotalCLA_OwnProvision_spend_1000s_2010"
names(outturn0910CLA)[names(outturn0910CLA)=="Total.Expenditure..k."] <- "TotalCLA_TotalExpenditure_spend_1000s_2010"
names(outturn0910CLA)[names(outturn0910CLA)=="Other.Public..z.ii.."] <- "TotalCLA_OtherPublic_spend_1000s_2010"

names(outturn1011)[names(outturn1011)=="Private..z.i.."] <- "PrivateProvision_spend_1000s_2011"
names(outturn1011)[names(outturn1011)=="Voluntary..z.iii.."] <- "Voluntary_spend_1000s_2011"
names(outturn1011)[names(outturn1011)=="Own.Provision..y."] <- "OwnProvision_spend_1000s_2011"
names(outturn1011)[names(outturn1011)=="Total.Expenditure..k."] <- "TotalExpenditure_spend_1000s_2011"
names(outturn1011)[names(outturn1011)=="Other.Public..z.ii.."] <- "OtherPublic_spend_1000s_2011"

names(outturn1011CLA)[names(outturn1011CLA)=="Private..z.i.."] <- "TotalCLA_PrivateProvision_spend_1000s_2011"
names(outturn1011CLA)[names(outturn1011CLA)=="Voluntary..z.iii.."] <- "TotalCLA_Voluntary_spend_1000s_2011"
names(outturn1011CLA)[names(outturn1011CLA)=="Own.Provision..y."] <- "TotalCLA_OwnProvision_spend_1000s_2011"
names(outturn1011CLA)[names(outturn1011CLA)=="Total.Expenditure..k."] <- "TotalCLA_TotalExpenditure_spend_1000s_2011"
names(outturn1011CLA)[names(outturn1011CLA)=="Other.Public..z.ii.."] <- "TotalCLA_OtherPublic_spend_1000s_2011"


names(outturn1112)[names(outturn1112)=="PrivateProvision"] <- "PrivateProvision_spend_1000s_2012"
names(outturn1112)[names(outturn1112)=="Voluntary"] <- "Voluntary_spend_1000s_2012"
names(outturn1112)[names(outturn1112)=="OwnProvision"] <- "OwnProvision_spend_1000s_2012"
names(outturn1112)[names(outturn1112)=="TotalExpenditure"] <- "TotalExpenditure_spend_1000s_2012"
names(outturn1112)[names(outturn1112)=="OtherPublic"] <- "OtherPublic_spend_1000s_2012"

names(outturn1112CLA)[names(outturn1112CLA)=="PrivateProvision"] <- "TotalCLA_PrivateProvision_spend_1000s_2012"
names(outturn1112CLA)[names(outturn1112CLA)=="Voluntary"] <- "TotalCLA_Voluntary_spend_1000s_2012"
names(outturn1112CLA)[names(outturn1112CLA)=="OwnProvision"] <- "TotalCLA_OwnProvision_spend_1000s_2012"
names(outturn1112CLA)[names(outturn1112CLA)=="TotalExpenditure"] <- "TotalCLA_TotalExpenditure_spend_1000s_2012"
names(outturn1112CLA)[names(outturn1112CLA)=="OtherPublic"] <- "TotalCLA_OtherPublic_spend_1000s_2012"

names(outturn1213)[names(outturn1213)=="PrivateProvision"] <- "PrivateProvision_spend_1000s_2013"
names(outturn1213)[names(outturn1213)=="Voluntary"] <- "Voluntary_spend_1000s_2013"
names(outturn1213)[names(outturn1213)=="OwnProvision"] <- "OwnProvision_spend_1000s_2013"
names(outturn1213)[names(outturn1213)=="TotalExpenditure"] <- "TotalExpenditure_spend_1000s_2013"
names(outturn1213)[names(outturn1213)=="OtherPublic"] <- "OtherPublic_spend_1000s_2013"

names(outturn1213CLA)[names(outturn1213CLA)=="PrivateProvision"] <- "TotalCLA_PrivateProvision_spend_1000s_2013"
names(outturn1213CLA)[names(outturn1213CLA)=="Voluntary"] <- "TotalCLA_Voluntary_spend_1000s_2013"
names(outturn1213CLA)[names(outturn1213CLA)=="OwnProvision"] <- "TotalCLA_OwnProvision_spend_1000s_2013"
names(outturn1213CLA)[names(outturn1213CLA)=="TotalExpenditure"] <- "TotalCLA_TotalExpenditure_spend_1000s_2013"
names(outturn1213CLA)[names(outturn1213CLA)=="OtherPublic"] <- "TotalCLA_OtherPublic_spend_1000s_2013"

names(outturn1314)[names(outturn1314)=="PrivateProvision"] <- "PrivateProvision_spend_1000s_2014"
names(outturn1314)[names(outturn1314)=="Voluntary"] <- "Voluntary_spend_1000s_2014"
names(outturn1314)[names(outturn1314)=="OwnProvision"] <- "OwnProvision_spend_1000s_2014"
names(outturn1314)[names(outturn1314)=="TotalExpenditure"] <- "TotalExpenditure_spend_1000s_2014"
names(outturn1314)[names(outturn1314)=="OtherPublic"] <- "OtherPublic_spend_1000s_2014"

names(outturn1314CLA)[names(outturn1314CLA)=="PrivateProvision"] <- "TotalCLA_PrivateProvision_spend_1000s_2014"
names(outturn1314CLA)[names(outturn1314CLA)=="Voluntary"] <- "TotalCLA_Voluntary_spend_1000s_2014"
names(outturn1314CLA)[names(outturn1314CLA)=="OwnProvision"] <- "TotalCLA_OwnProvision_spend_1000s_2014"
names(outturn1314CLA)[names(outturn1314CLA)=="TotalExpenditure"] <- "TotalCLA_TotalExpenditure_spend_1000s_2014"
names(outturn1314CLA)[names(outturn1314CLA)=="OtherPublic"] <- "TotalCLA_OtherPublic_spend_1000s_2014"

#keep important vars

outturn0809 <- outturn0809[c(1,4,5,6,7)]
outturn0910 <- outturn0910[c(1,5,6,7,8,9)]
outturn1011 <- outturn1011[c(1,6,7,8,9, 10)]
outturn1112 <- outturn1112[c(1,5,6,7,8,9)]
outturn1213 <- outturn1213[c(1,6,7,8,9, 10)]
outturn1314 <- outturn1314[c(1,6,7,8,9, 10)]

outturn0809CLA <- outturn0809CLA[c(1,4,5,6,7)]
outturn0910CLA <- outturn0910CLA[c(1,5,6,7,8,9)]
outturn1011CLA <- outturn1011CLA[c(1,6,7,8,9, 10)]
outturn1112CLA <- outturn1112CLA[c(1,5,6,7,8,9)]
outturn1213CLA <- outturn1213CLA[c(1,6,7,8,9, 10)]
outturn1314CLA <- outturn1314CLA[c(1,6,7,8,9, 10)]


outturn0809$OtherPublic_spend_1000s_2009 <- NA
outturn0809CLA$OtherPublic_spend_1000s_2009 <- NA

oldlalookup <- read.csv("Data/oldlalookup.csv")

names(outturn0809)[names(outturn0809)=="LEA.Number."] <- "LA"
names(outturn0910)[names(outturn0910)=="LEA"] <- "LA"
names(outturn0809CLA)[names(outturn0809CLA)=="LEA.Number."] <- "LA"
names(outturn0910CLA)[names(outturn0910CLA)=="LEA"] <- "LA"


outturnla <- merge(oldlalookup, outturn0809, by="LA", all=T)
outturnla <- merge(outturnla, outturn0910, by="LA", all=T)
outturnla <- merge(outturnla, outturn1011, by="LA", all=T)
outturnla <- merge(outturnla, outturn1112, by="LA", all=T)
outturnla <- merge(outturnla, outturn1213, by="LA", all=T)
outturnla <- merge(outturnla, outturn1314, by="LA", all=T)

outturnla <- merge(outturnla, outturn0809CLA, by="LA", all=T)
outturnla <- merge(outturnla, outturn0910CLA, by="LA", all=T)
outturnla <- merge(outturnla, outturn1011CLA, by="LA", all=T)
outturnla <- merge(outturnla, outturn1112CLA, by="LA", all=T)
outturnla <- merge(outturnla, outturn1213CLA, by="LA", all=T)
outturnla <- merge(outturnla, outturn1314CLA, by="LA", all=T)




LAData <- merge(LAData, outturnla, by="LAD19CD", all=T)
LAData <- LAData[complete.cases(LAData$LAD19CD),] # Gets rid of Cheshire and Bedfordshire pre2009 LGR reforms
#which contains one year of expenditure in 2008-2009
#see LAs 820 and 875 here https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/915524/RFC_1066___changes_-_LA_codeset_updated.pdf

####Create provider variables at the LA scale####

ProviderData2020 <- read.csv("Data/Carehomes_alldates.csv")

ProviderData2020 <- ProviderData2020[!duplicated(ProviderData2020$Event.number), ]

ProviderData2020 <- ProviderData2020[!duplicated(ProviderData2020$URN), ]
ProviderData2020 <- ProviderData2020[which(ProviderData2020$Registration.status=="Active"), ]
ProviderData2020$Local.authority[ProviderData2020$Local.authority == "Kingston upon Hull"] <- "Kingston upon Hull, City of"
ProviderData2020$Local.authority[ProviderData2020$Local.authority == "Herefordshire"] <- "Herefordshire, County of"
ProviderData2020$Local.authority[ProviderData2020$Local.authority == "Bristol"] <- "Bristol, City of"
ProviderData2020$Local.authority[ProviderData2020$Local.authority == "Southend on Sea"] <- "Southend-on-Sea"
ProviderData2020$Local.authority[ProviderData2020$Local.authority == "Durham"] <- "County Durham"
ProviderData2020$Local.authority[ProviderData2020$Local.authority == "Bournemouth, Christchurch & Poole"] <- "Bournemouth, Christchurch and Poole"
ProviderData2020$Local.authority[ProviderData2020$Local.authority == "St Helens"] <- "St. Helens"

laprovidersoutcomes <- ProviderData2020[c(15,24)]

#create variable for percent of homes that underperform

laprovidersoutcomes$Overall.experiences.and.progress.of.children.and.young.people <- factor(laprovidersoutcomes$Overall.experiences.and.progress.of.children.and.young.people , levels = c("Inadequate","Requires improvement to be good","Good","Outstanding" ))
laprovidersoutcomes$Overall.experiences.and.progress.of.children.and.young.people <-revalue(laprovidersoutcomes$Overall.experiences.and.progress.of.children.and.young.people, c("Inadequate"=1, "Requires improvement to be good"=2,"Good"=3,"Outstanding"=4 ))
laprovidersoutcomes$Overall.experiences.and.progress.of.children.and.young.people <- as.double(laprovidersoutcomes$Overall.experiences.and.progress.of.children.and.young.people)

laprovidersoutcomes <- laprovidersoutcomes %>% mutate(poor = ifelse(laprovidersoutcomes$Overall.experiences.and.progress.of.children.and.young.people<3, 1,0))
laprovidersoutcomes$all_homes_n <- 1

laprovidersoutcomes <- aggregate(. ~Local.authority, data=laprovidersoutcomes, sum)

laprovidersoutcomes$Homes_worse_than_good_percent <- laprovidersoutcomes$poor/laprovidersoutcomes$all*100

names(laprovidersoutcomes)[names(laprovidersoutcomes)=="Local.authority"] <- "lad19nm"


#create variable for number of places in private and public ownership
laproviders <- ProviderData2020[c(15,17,18)]

laproviders$Homes <- 1

laproviders <- aggregate(. ~Local.authority+Sector, data=laproviders, sum)
latotalplace <- aggregate(. ~Local.authority, data=laproviders[c(1,3, 4)], sum)


names(latotalplace)[names(latotalplace)=="Places"] <- "Total_places"
names(latotalplace)[names(latotalplace)=="Homes"] <- "Total_homes"

laprovidersprivate <- laproviders[which(laproviders$Sector=="Private"),]

names(laprovidersprivate)[names(laprovidersprivate)=="Places"] <- "Private_places"
names(laprovidersprivate)[names(laprovidersprivate)=="Homes"] <- "Private_homes"


laprovidersLA <- laproviders[which(laproviders$Sector=="Local Authority"),]


names(laprovidersLA)[names(laprovidersLA)=="Places"] <- "LA_places"
names(laprovidersLA)[names(laprovidersLA)=="Homes"] <- "LA_homes"

laproviders  <- merge(laprovidersprivate, latotalplace, by="Local.authority", all.y=T)
laproviders  <- merge(laprovidersLA, laproviders, by="Local.authority", all.y=T)

laproviders$Sector[is.na(laproviders$Sector)] <- "Private"

laproviders$Private_places[is.na(laproviders$Private_places)] <- 0
laproviders$Private_homes[is.na(laproviders$Private_homes)] <- 0
laproviders$LA_places[is.na(laproviders$LA_places)] <- 0
laproviders$LA_homes[is.na(laproviders$LA_homes)] <- 0


laproviders$Percent_privateplaces <- laproviders$Private_places/laproviders$Total_places*100
laproviders$Percent_privatehomes <- laproviders$Private_homes/laproviders$Total_homes*100
laproviders$Percent_LAhomes <- laproviders$LA_homes/laproviders$Total_homes*100
laproviders$Percent_LAplaces <- laproviders$LA_places/laproviders$Total_places*100




names(laproviders)[names(laproviders)=="Local.authority"] <- "lad19nm"


laproviding <- merge(laproviders, laprovidersoutcomes, by="lad19nm")


laprovided <- laproviding

names(laprovided)[names(laprovided)=="lad19nm"] <- "LAD19NM"



LAData <- merge(LAData, laprovided, by="LAD19NM", all=T)

laprovided <- laprovided[-c(2)]
laprovided$year <- 2020
#create_population_vars

LAData$population_2019 <- LAData$All.ages

LAData$Under18_percent <- as.double(LAData$Under18)/ as.double(LAData$All.ages)*100


#create writable dataframe
er <- apply(ProviderData,2,as.character)

#write data to csv
#write.csv(er, "Data/provider_data_full_Oct_21.csv")




#remove unwanted objects
#rm(list=setdiff(ls(), c("ProviderData", "LAData", "ProviderData2020")))

####Key variables dataset####

keyvariables <- LAData[c(1,2,4,5,6:9,13,16,28,31,33,36,38,41,43,46,48,51,53,56,95,96,102,108,117,136,148:150,152:156)]

#create writable dataframe
er2 <- apply(keyvariables,2,as.character)

#write data to csv
#write.csv(er2, "Data/LA_Data_key_variables.csv")


####panel data set####

ExpenditureData <- read.csv("Data/LA_Care_Expenditure_By_Ownership_OverTime.csv")
ExpenditureData2 <- ExpenditureData[which(ExpenditureData$Description=="3.1.11 Total Children Looked After"),]

ExpenditureData <- ExpenditureData[which(ExpenditureData$Description=="3.1.1 Residential care"),]

ExpenditureData <- ExpenditureData[c(1,10,13:19)]
ExpenditureData2<- ExpenditureData2[c(1,10,13:19)]

names(ExpenditureData)[names(ExpenditureData)=="OwnProvision"] <- "OwnProvision_spend"

names(ExpenditureData)[names(ExpenditureData)=="PrivateProvision"] <- "PrivateProvision_spend"
names(ExpenditureData)[names(ExpenditureData)=="OtherPublic"] <- "OtherPublic_spend"
names(ExpenditureData)[names(ExpenditureData)=="Voluntary"] <- "Voluntary_spend"
names(ExpenditureData)[names(ExpenditureData)=="TotalExpenditure"] <- "TotalExpenditure_spend"

ExpenditureData <- ExpenditureData %>% mutate(year= ifelse(ExpenditureData$ï..time_period=="201920", 2020, ifelse(ExpenditureData$ï..time_period=="201819", 2019, ifelse(ExpenditureData$ï..time_period=="201718", 2018,ifelse(ExpenditureData$ï..time_period=="201617", 2017, ifelse(ExpenditureData$ï..time_period=="201516", 2016, ifelse(ExpenditureData$ï..time_period=="201415", 2015,ifelse(ExpenditureData$ï..time_period=="201314", 2014, 0))))))))


names(ExpenditureData2)[names(ExpenditureData2)=="OwnProvision"] <- "totalCLA_OwnProvision_spend"

names(ExpenditureData2)[names(ExpenditureData2)=="PrivateProvision"] <- "totalCLA_PrivateProvision_spend"
names(ExpenditureData2)[names(ExpenditureData2)=="OtherPublic"] <- "totalCLA_OtherPublic_spend"
names(ExpenditureData2)[names(ExpenditureData2)=="Voluntary"] <- "totalCLA_Voluntary_spend"
names(ExpenditureData2)[names(ExpenditureData2)=="TotalExpenditure"] <- "totalCLA_TotalExpenditure_spend"

ExpenditureData2 <- ExpenditureData2 %>% mutate(year= ifelse(ExpenditureData2$ï..time_period=="201920", 2020, ifelse(ExpenditureData2$ï..time_period=="201819", 2019, ifelse(ExpenditureData2$ï..time_period=="201718", 2018,ifelse(ExpenditureData2$ï..time_period=="201617", 2017, ifelse(ExpenditureData2$ï..time_period=="201516", 2016, ifelse(ExpenditureData2$ï..time_period=="201415", 2015,ifelse(ExpenditureData2$ï..time_period=="201314", 2014, 0))))))))


CLAexpend2020 <- ExpenditureData2[which(ExpenditureData2$year==2020),]
CLAexpend2020 <- CLAexpend2020[-c(1)]

LAData <- merge(LAData, CLAexpend2020, by="LAD19CD", all.x = T)


paneldata <- ExpenditureData[c(2:7, 10)]

ExpenditureData2 <- ExpenditureData2[c(2:7, 10)]

paneldata <- merge(paneldata, ExpenditureData2, by=c("LAD19CD", "year"), all=T)


#2021expenditure#

expend21 <- read.csv("Data/2021_s251outturn.csv")
expend21 <- expend21[which(expend21$ï..time_period==202021),]

expend212 <- expend21[which(expend21$description_of_expenditure=="3.1.11 Total Children Looked After"),]

expend21 <- expend21[which(expend21$description_of_expenditure=="3.1.1 Residential care"),]

expend21 <- expend21[c(8,13:17)]
expend212<- expend212[c(8,13:17)]

names(expend21)[names(expend21)=="own_provision"] <- "OwnProvision_spend"

names(expend21)[names(expend21)=="private_provision"] <- "PrivateProvision_spend"
names(expend21)[names(expend21)=="other_public_sector_provision"] <- "OtherPublic_spend"
names(expend21)[names(expend21)=="voluntary_provision"] <- "Voluntary_spend"
names(expend21)[names(expend21)=="total_expenditure"] <- "TotalExpenditure_spend"
names(expend21)[names(expend21)=="new_la_code"] <- "LAD19CD"

expend21$year <- 2021

names(expend212)[names(expend212)=="own_provision"] <- "totalCLA_OwnProvision_spend"

names(expend212)[names(expend212)=="private_provision"] <- "totalCLA_PrivateProvision_spend"
names(expend212)[names(expend212)=="other_public_sector_provision"] <- "totalCLA_OtherPublic_spend"
names(expend212)[names(expend212)=="voluntary_provision"] <- "totalCLA_Voluntary_spend"
names(expend212)[names(expend212)=="total_expenditure"] <- "totalCLA_TotalExpenditure_spend"
names(expend212)[names(expend212)=="new_la_code"] <- "LAD19CD"

expend212$year <-2021 

expend212 <- merge(expend21, expend212, by=c("LAD19CD", "year"), all=T)

paneldata <- rbind(paneldata, expend212)




LAnames <- LAData[c(1,2)]



paneldata <- merge(paneldata, LAnames, by="LAD19CD", all.x=T)






unitcostData <- read.csv("Data/unit_costs_sept_csv.csv")
unitcostres <- read.csv("Data/unit_cost_residential.csv")
unitcostfost <- read.csv("Data/unit_cost_fostering.csv")
unitcostadopt <- read.csv("Data/unit_cost_adoption.csv")
unitcostsen <- read.csv("Data/unit_cost_sen.csv")
unitcostsocwork <- read.csv("Data/unit_cost_soc_work.csv")

colnames(unitcostData) <- c("c", "LAD19NM", "2014", "2015", "2016", "2017", "2018", "2019", "2020")
colnames(unitcostres) <- c("c", "LAD19NM", "2014", "2015", "2016", "2017", "2018", "2019", "2020")
colnames(unitcostfost) <- c("c", "LAD19NM", "2014", "2015", "2016", "2017", "2018", "2019", "2020")
colnames(unitcostadopt) <- c("c", "LAD19NM", "2014", "2015", "2016", "2017", "2018", "2019", "2020")
colnames(unitcostsen) <- c("c", "LAD19NM", "2014", "2015", "2016", "2017", "2018", "2019", "2020")
colnames(unitcostsocwork) <- c("c", "LAD19NM", "2014", "2015", "2016", "2017", "2018", "2019", "2020")

unitcostData <- unitcostData[-c(1)]
unitcostres <- unitcostres[-c(1)]
unitcostfost <- unitcostfost[-c(1)]
unitcostadopt <- unitcostadopt[-c(1)]
unitcostsen <- unitcostsen[-c(1)]
unitcostsocwork <- unitcostsocwork[-c(1)]

library("reshape2")

unitcostData <- melt(unitcostData,id =  "LAD19NM", variable.name = "year")
unitcostres <- melt(unitcostres,id =  "LAD19NM", variable.name = "year")
unitcostfost <- melt(unitcostfost,id =  "LAD19NM", variable.name = "year")
unitcostadopt <- melt(unitcostadopt,id =  "LAD19NM", variable.name = "year")
unitcostsen <- melt(unitcostsen,id =  "LAD19NM", variable.name = "year")
unitcostsocwork <- melt(unitcostsocwork,id =  "LAD19NM", variable.name = "year")

names(unitcostData)[names(unitcostData)=="value"] <- "unit_cost"
names(unitcostres)[names(unitcostres)=="value"] <- "unit_cost_residential"
names(unitcostfost)[names(unitcostfost)=="value"] <- "unit_cost_fostering"
names(unitcostadopt)[names(unitcostadopt)=="value"] <- "unit_cost_adoption"
names(unitcostsen)[names(unitcostsen)=="value"] <- "unit_cost_sen"
names(unitcostsocwork)[names(unitcostsocwork)=="value"] <- "unit_cost_socwork"

paneldata <- merge(paneldata, unitcostData, by=c("LAD19NM", "year"), all.x=T)
paneldata <- merge(paneldata, unitcostres, by=c("LAD19NM", "year"), all.x=T)
paneldata <- merge(paneldata, unitcostfost, by=c("LAD19NM", "year"), all.x=T)
paneldata <- merge(paneldata, unitcostadopt, by=c("LAD19NM", "year"), all.x=T)
paneldata <- merge(paneldata, unitcostsen, by=c("LAD19NM", "year"), all.x=T)
paneldata <- merge(paneldata, unitcostsocwork, by=c("LAD19NM", "year"), all.x=T)

outturn0809 <- read.csv("Data/outturn_0809.csv")
outturn0910 <- read.csv("Data/outturn_0910.csv")
outturn1011 <- read.csv("Data/outturn_1011.csv")
outturn1112 <- read.csv("Data/outturn_1112.csv")
outturn1213 <- read.csv("Data/outturn_1213.csv")
outturn1314 <- read.csv("Data/outturn_1314.csv")

#keep just residential care spend

outturn0809CLA <- outturn0809[which(outturn0809$S52.Line.Reference.=="14 Total Children Looked After"),]
outturn0809 <- outturn0809[which(outturn0809$S52.Line.Reference.=="5 Residential care"),]

outturn0910CLA <- outturn0910[which(outturn0910$S52.Line.Reference =="Total Children Looked After"),]
outturn0910 <- outturn0910[which(outturn0910$S52.Line.Reference =="Residential care"),]

outturn1011CLA <- outturn1011[which(outturn1011$S52.Line.Reference =="Total Children Looked After"),]
outturn1011 <- outturn1011[which(outturn1011$S52.Line.Reference =="Residential care"),]

outturn1112CLA <- outturn1112[which(outturn1112$LineNumber =="12"),]
outturn1112 <- outturn1112[which(outturn1112$LineNumber =="4"),]

outturn1213CLA <- outturn1213[which(outturn1213$LineNumber =="15"),]
outturn1213 <- outturn1213[which(outturn1213$LineNumber =="5"),]

outturn1314CLA <- outturn1314[which(outturn1314$LineNumber =="16"),]
outturn1314 <- outturn1314[which(outturn1314$LineNumber =="6"),]



#Standardise variable names

names(outturn0809)[names(outturn0809)=="PRIVATE..x."] <- "PrivateProvision_spend"
names(outturn0809)[names(outturn0809)=="VOLUNTARY..y."] <- "Voluntary_spend"
names(outturn0809)[names(outturn0809)=="PUBLIC..z."] <- "OwnProvision_spend"
names(outturn0809)[names(outturn0809)=="TOTAL.EXPENDITURE..k."] <- "TotalExpenditure_spend"

names(outturn0809CLA)[names(outturn0809CLA)=="PRIVATE..x."] <- "totalCLA_PrivateProvision_spend"
names(outturn0809CLA)[names(outturn0809CLA)=="VOLUNTARY..y."] <- "totalCLA_Voluntary_spend"
names(outturn0809CLA)[names(outturn0809CLA)=="PUBLIC..z."] <- "totalCLA_OwnProvision_spend"
names(outturn0809CLA)[names(outturn0809CLA)=="TOTAL.EXPENDITURE..k."] <- "totalCLA_TotalExpenditure_spend"


names(outturn0910)[names(outturn0910)=="Private..z.i.."] <- "PrivateProvision_spend"
names(outturn0910)[names(outturn0910)=="Voluntary..z.iii.."] <- "Voluntary_spend"
names(outturn0910)[names(outturn0910)=="Own.Provision..y."] <- "OwnProvision_spend"
names(outturn0910)[names(outturn0910)=="Total.Expenditure..k."] <- "TotalExpenditure_spend"
names(outturn0910)[names(outturn0910)=="Other.Public..z.ii.."] <- "OtherPublic_spend"

names(outturn0910CLA)[names(outturn0910CLA)=="Private..z.i.."] <- "totalCLA_PrivateProvision_spend"
names(outturn0910CLA)[names(outturn0910CLA)=="Voluntary..z.iii.."] <- "totalCLA_Voluntary_spend"
names(outturn0910CLA)[names(outturn0910CLA)=="Own.Provision..y."] <- "totalCLA_OwnProvision_spend"
names(outturn0910CLA)[names(outturn0910CLA)=="Total.Expenditure..k."] <- "totalCLA_TotalExpenditure_spend"
names(outturn0910CLA)[names(outturn0910CLA)=="Other.Public..z.ii.."] <- "totalCLA_OtherPublic_spend"

names(outturn1011)[names(outturn1011)=="Private..z.i.."] <- "PrivateProvision_spend"
names(outturn1011)[names(outturn1011)=="Voluntary..z.iii.."] <- "Voluntary_spend"
names(outturn1011)[names(outturn1011)=="Own.Provision..y."] <- "OwnProvision_spend"
names(outturn1011)[names(outturn1011)=="Total.Expenditure..k."] <- "TotalExpenditure_spend"
names(outturn1011)[names(outturn1011)=="Other.Public..z.ii.."] <- "OtherPublic_spend"

names(outturn1011CLA)[names(outturn1011CLA)=="Private..z.i.."] <- "totalCLA_PrivateProvision_spend"
names(outturn1011CLA)[names(outturn1011CLA)=="Voluntary..z.iii.."] <- "totalCLA_Voluntary_spend"
names(outturn1011CLA)[names(outturn1011CLA)=="Own.Provision..y."] <- "totalCLA_OwnProvision_spend"
names(outturn1011CLA)[names(outturn1011CLA)=="Total.Expenditure..k."] <- "totalCLA_TotalExpenditure_spend"
names(outturn1011CLA)[names(outturn1011CLA)=="Other.Public..z.ii.."] <- "totalCLA_OtherPublic_spend"

names(outturn1112)[names(outturn1112)=="PrivateProvision"] <- "PrivateProvision_spend"
names(outturn1112)[names(outturn1112)=="Voluntary"] <- "Voluntary_spend"
names(outturn1112)[names(outturn1112)=="OwnProvision"] <- "OwnProvision_spend"
names(outturn1112)[names(outturn1112)=="TotalExpenditure"] <- "TotalExpenditure_spend"
names(outturn1112)[names(outturn1112)=="OtherPublic"] <- "OtherPublic_spend"

names(outturn1112CLA)[names(outturn1112CLA)=="PrivateProvision"] <- "totalCLA_PrivateProvision_spend"
names(outturn1112CLA)[names(outturn1112CLA)=="Voluntary"] <- "totalCLA_Voluntary_spend"
names(outturn1112CLA)[names(outturn1112CLA)=="OwnProvision"] <- "totalCLA_OwnProvision_spend"
names(outturn1112CLA)[names(outturn1112CLA)=="TotalExpenditure"] <- "totalCLA_TotalExpenditure_spend"
names(outturn1112CLA)[names(outturn1112CLA)=="OtherPublic"] <- "totalCLA_OtherPublic_spend"

names(outturn1213)[names(outturn1213)=="PrivateProvision"] <- "PrivateProvision_spend"
names(outturn1213)[names(outturn1213)=="Voluntary"] <- "Voluntary_spend"
names(outturn1213)[names(outturn1213)=="OwnProvision"] <- "OwnProvision_spend"
names(outturn1213)[names(outturn1213)=="TotalExpenditure"] <- "TotalExpenditure_spend"
names(outturn1213)[names(outturn1213)=="OtherPublic"] <- "OtherPublic_spend"

names(outturn1213CLA)[names(outturn1213CLA)=="PrivateProvision"] <- "totalCLA_PrivateProvision_spend"
names(outturn1213CLA)[names(outturn1213CLA)=="Voluntary"] <- "totalCLA_Voluntary_spend"
names(outturn1213CLA)[names(outturn1213CLA)=="OwnProvision"] <- "totalCLA_OwnProvision_spend"
names(outturn1213CLA)[names(outturn1213CLA)=="TotalExpenditure"] <- "totalCLA_TotalExpenditure_spend"
names(outturn1213CLA)[names(outturn1213CLA)=="OtherPublic"] <- "totalCLA_OtherPublic_spend"

names(outturn1314)[names(outturn1314)=="PrivateProvision"] <- "PrivateProvision_spend"
names(outturn1314)[names(outturn1314)=="Voluntary"] <- "Voluntary_spend"
names(outturn1314)[names(outturn1314)=="OwnProvision"] <- "OwnProvision_spend"
names(outturn1314)[names(outturn1314)=="TotalExpenditure"] <- "TotalExpenditure_spend"
names(outturn1314)[names(outturn1314)=="OtherPublic"] <- "OtherPublic_spend"

names(outturn1314CLA)[names(outturn1314CLA)=="PrivateProvision"] <- "totalCLA_PrivateProvision_spend"
names(outturn1314CLA)[names(outturn1314CLA)=="Voluntary"] <- "totalCLA_Voluntary_spend"
names(outturn1314CLA)[names(outturn1314CLA)=="OwnProvision"] <- "totalCLA_OwnProvision_spend"
names(outturn1314CLA)[names(outturn1314CLA)=="TotalExpenditure"] <- "totalCLA_TotalExpenditure_spend"
names(outturn1314CLA)[names(outturn1314CLA)=="OtherPublic"] <- "totalCLA_OtherPublic_spend"

#keep important vars

outturn0809 <- outturn0809[c(1,4,5,6,7)]
outturn0910 <- outturn0910[c(1,5,6,7,8,9)]
outturn1011 <- outturn1011[c(1,6,7,8,9, 10)]
outturn1112 <- outturn1112[c(1,5,6,7,8,9)]
outturn1213 <- outturn1213[c(1,6,7,8,9, 10)]
outturn1314 <- outturn1314[c(1,6,7,8,9, 10)]



outturn0809CLA <- outturn0809CLA[c(1,4,5,6,7)]
outturn0910CLA <- outturn0910CLA[c(1,5,6,7,8,9)]
outturn1011CLA <- outturn1011CLA[c(1,6,7,8,9, 10)]
outturn1112CLA <- outturn1112CLA[c(1,5,6,7,8,9)]
outturn1213CLA <- outturn1213CLA[c(1,6,7,8,9, 10)]
outturn1314CLA <- outturn1314CLA[c(1,6,7,8,9, 10)]


outturn0809$OtherPublic_spend <- NA
outturn0809CLA$totalCLA_OtherPublic_spend <- NA

oldlalookup <- read.csv("Data/oldlalookup.csv")

names(outturn0809)[names(outturn0809)=="LEA.Number."] <- "LA"
names(outturn0910)[names(outturn0910)=="LEA"] <- "LA"
names(outturn0809CLA)[names(outturn0809CLA)=="LEA.Number."] <- "LA"
names(outturn0910CLA)[names(outturn0910CLA)=="LEA"] <- "LA"


outturn0809$year <- 2009
outturn0910$year <- 2010
outturn1011$year <- 2011
outturn1112$year <- 2012
outturn1213$year <- 2013
outturn1314$year <- 2014

outturn0809CLA$year <- 2009
outturn0910CLA$year <- 2010
outturn1011CLA$year <- 2011
outturn1112CLA$year <- 2012
outturn1213CLA$year <- 2013
outturn1314CLA$year <- 2014


oldlalookup <- read.csv("Data/oldlalookup.csv")

outturnlapan <- rbind(outturn0910, outturn0809, outturn1011, outturn1112, outturn1213, outturn1314)

outturnlapan <- merge(outturnlapan, oldlalookup, by="LA", all.x=T)
outturnlapan <- merge(outturnlapan, LAnames, by="LAD19CD", all.x=T)
outturnlapan <- outturnlapan[-c(2)]


outturnlapanCLA <- rbind(outturn0910CLA, outturn0809CLA, outturn1011CLA, outturn1112CLA, outturn1213CLA, outturn1314CLA)

outturnlapanCLA <- merge(outturnlapanCLA, oldlalookup, by="LA", all.x=T)
outturnlapanCLA <- merge(outturnlapanCLA, LAnames, by="LAD19CD", all.x=T)
outturnlapanCLA <- outturnlapanCLA[-c(2)]

outturnlapanCLA <- merge(outturnlapanCLA, outturnlapan, by=c("LAD19CD", "year", "LAD19NM"))
outturnlapanCLA$unit_cost <- NA
outturnlapanCLA$unit_cost_residential <- NA
outturnlapanCLA$unit_cost_fostering <- NA
outturnlapanCLA$unit_cost_adoption <- NA
outturnlapanCLA$unit_cost_sen <- NA
outturnlapanCLA$unit_cost_socwork <- NA


paneldata <- rbind(paneldata, outturnlapanCLA)
paneldata <- merge(paneldata, laprovided, by=c("LAD19NM", "year"), all.x=T)

Placement_stabilitys <- read.csv("Data/la_cla_at_31_march_placement_stability_indicator.csv")

Placement_stability <- read.csv("Data/new_stability.csv")
names(Placement_stability)[names(Placement_stability)=="ï..time_period"] <- "year"
names(Placement_stability)[names(Placement_stability)=="new_la_code"] <- "LAD19CD"

Placement_stability <- Placement_stability[which(Placement_stability$LAD19CD!=""),]


Placement_stability <- Placement_stability[which(Placement_stability$characteristic!= "Total children"),]

Placement_stabilityno <- Placement_stability[c(1,10,12,13)]
Placement_stabilityper <- Placement_stability[c(1,10,12,14)]
Placement_stabilityno <- unique(Placement_stabilityno)
Placement_stabilityper <- unique(Placement_stabilityper)


Placement_stabilityno <- Placement_stabilityno %>% pivot_wider(names_from = characteristic, values_from = number, names_prefix = "Stability (Count) ")
Placement_stabilityper <- Placement_stabilityper %>% pivot_wider(names_from = characteristic, values_from = percentage, names_prefix = "Stability (%) ")
names(Placement_stabilityper)[names(Placement_stabilityper)=="ï..time_period"] <- "year"
names(Placement_stabilityno)[names(Placement_stabilityno)=="ï..time_period"] <- "year"


childrenstats <- read.csv("Data/la_new.csv")
placementstats <-  read.csv("Data/la_cla_during_year_new_placements.csv")

#Clean data#
names(childrenstats)[names(childrenstats)=="time_period"] <- "year"
names(childrenstats)[names(childrenstats)=="new_la_code"] <- "LAD19CD"
names(placementstats)[names(placementstats)=="time_period"] <- "year"
names(placementstats)[names(placementstats)=="new_la_code"] <- "LAD19CD"

childrenstats <- childrenstats[which(childrenstats$LAD19CD!=""),]
placementstats <- placementstats[which(placementstats$LAD19CD!=""),]

#BuckreplaceLAD19CD

childrenstats[which(childrenstats$LAD19CD=="E06000060"),]$LAD19CD <- "E10000002"
placementstats[which(placementstats$LAD19CD=="E06000060"),]$LAD19CD <- "E10000002"

placementstatsno <- placementstats[c(1,10,12,13)]
placementstatsper <- placementstats[c(1,10,12,14)]
placementstatsno <- unique(placementstatsno)
placementstatsper <- unique(placementstatsper)

placementstatsper <- placementstatsper[which(placementstatsper$characteristic!= "Total"),]
placementstatsno <- placementstatsno[which(placementstatsno$characteristic!= "Total"),]




library(tidyr)

#Turn data from panel to wide
placementwideno <- placementstatsno %>% pivot_wider(names_from = characteristic, values_from = number, names_prefix = "Count")
placementwideper <- placementstatsper %>% pivot_wider(names_from = characteristic, values_from = percentage, names_prefix = "CHARACTERISTICS (%) ")


childrenstatsno <- childrenstats[c(1,10,12,13)]
childrenstatsper <- childrenstats[c(1,10,12,14)]
childrenstatsno <- unique(childrenstatsno)
childrenstatsper <- unique(childrenstatsper)

childrenstatsper <- childrenstatsper[which(childrenstatsper$characteristic!= "Total"),]
childrenstatsno <- childrenstatsno[which(childrenstatsno$characteristic!= "Total"),]




library(tidyr)

#Turn data from panel to wide
childrenwideno <- childrenstatsno %>% pivot_wider(names_from = characteristic, values_from = number, names_prefix = "Count", values_fn = length)
childrenwideper <- childrenstatsper %>% pivot_wider(names_from = characteristic, values_from = percentage, names_prefix = "CHARACTERISTICS (%) ")

childrenmissing <- read.csv("Data/new_missing.csv")
names(childrenmissing)[names(childrenmissing)=="ï..time_period"] <- "year"
names(childrenmissing)[names(childrenmissing)=="new_la_code"] <- "LAD19CD"

childrenmissing <- childrenmissing[which(childrenmissing$LAD19CD!=""),]

childrenmissing <- childrenmissing[c(1,10:12)]
childrenmissing <- unique(childrenmissing)
childrenmisswide <- childrenmissing %>% pivot_wider(names_from = cla_group, values_from = number, names_prefix = "MISSING_STATS ")


childrenhealth <- read.csv("Data/Conviction and health outcomes by LA.csv")
names(childrenhealth)[names(childrenhealth)=="ï..time_period"] <- "year"
childrenhealth <- childrenhealth[which(childrenhealth$LAD19CD!=""),]

childrenhealth <- childrenhealth[c(1,10, 12, 14)]
childrenhealth <- unique(childrenhealth)
childrenhealth <- childrenhealth[which(childrenhealth$characteristic!="Total all ages"),]
childrenhealthwide <- childrenhealth %>% pivot_wider(values_fn = length,names_from = characteristic, values_from = percent, names_prefix = "CONVICTION STATS (%) ")

childrenintake <- read.csv("Data/Intake rates by LA.csv")
childrenintake <- childrenintake[which(childrenintake$time_period==2020),]
names(childrenintake)[names(childrenintake)=="time_period"] <- "year"

childrenintake <- childrenintake[which(childrenintake$LAD19CD!=""),]

childrenintake <- childrenintake[c(1,10, 11, 14)]
childrenintake <- unique(childrenintake)
childrenintakewide <- childrenintake %>% pivot_wider(names_from = population_count, values_from = rate_per_10000, names_prefix = "INTAKE STATS (rate per 10,000) ")

paneldata <- merge(paneldata, childrenwideper, by=c("LAD19CD", "year"), all=T)
paneldata <- merge(paneldata, placementwideno, by=c("LAD19CD", "year"), all=T)
paneldata <- merge(paneldata, placementwideper, by=c("LAD19CD", "year"), all=T)

paneldata <- merge(paneldata, childrenmisswide, by=c("LAD19CD", "year"), all=T)
paneldata <- merge(paneldata, childrenhealthwide, by=c("LAD19CD", "year"), all=T)
paneldata <- merge(paneldata, childrenintakewide, by=c("LAD19CD", "year"), all=T)
paneldata <- merge(paneldata, Placement_stabilityper, by=c("LAD19CD", "year"), all=T)

idacilarge <- read.csv("Data/idacilarge.csv")
#IMD2019 <- read.csv("Data/IMD2019.csv")
imdlarge <- read.csv("Data/imdlarge.csv")
#IMD <- rbind(IMD2019, imdlarge)
#IDACI <- rbind(IDACI2019, idacilarge)

#remove duplicated authorities in LA and UA IMD files
IDACI <-idacilarge
IMD <- imdlarge

paneldata <- merge(paneldata, IDACI, by=c("LAD19CD"), all.x =T)
paneldata <- merge(paneldata, IMD, by=c("LAD19CD"), all.x =T)

#age#

panelage <- read.csv("Data/age_over_time_la.csv")
names(panelage)[names(panelage)=="ï..LAD19CD"] <- "LAD19CD"

latierlookup <- read.csv("Data/la_tier_lookup.csv")
names(latierlookup)[names(latierlookup)=="LTLA19CD"] <- "LAD19CD"

panelage <- merge(panelage, latierlookup, by="LAD19CD", all.x=T)

panelage <- panelage[-c(1,3,24,25,27)]
names(panelage)[names(panelage)=="UTLA19CD"] <- "LAD19CD"



panelage <- aggregate(. ~LAD19CD+age, data=panelage, sum)

panelage2010 <- panelage[c(1,2,12)]
panelage2011 <- panelage[c(1,2,13)]
panelage2012 <- panelage[c(1,2,14)]
panelage2013 <- panelage[c(1,2,15)]
panelage2014 <- panelage[c(1,2,16)]
panelage2015 <- panelage[c(1,2,17)]
panelage2016 <- panelage[c(1,2,18)]
panelage2017 <- panelage[c(1,2,19)]
panelage2018 <- panelage[c(1,2,20)]
panelage2019 <- panelage[c(1,2,21)]
panelage2020 <- panelage[c(1,2,22)]


panelage2010under18 <- panelage2010[which(panelage2010$age<18),]
panelage2011under18 <- panelage2011[which(panelage2011$age<18),]
panelage2012under18 <- panelage2012[which(panelage2012$age<18),]
panelage2013under18 <- panelage2013[which(panelage2013$age<18),]
panelage2014under18 <- panelage2014[which(panelage2014$age<18),]
panelage2015under18 <- panelage2015[which(panelage2015$age<18),]
panelage2016under18 <- panelage2016[which(panelage2016$age<18),]
panelage2017under18 <- panelage2017[which(panelage2017$age<18),]
panelage2018under18 <- panelage2018[which(panelage2018$age<18),]
panelage2019under18 <- panelage2019[which(panelage2019$age<18),]
panelage2020under18 <- panelage2020[which(panelage2020$age<18),]


panelage2010total <- aggregate(. ~LAD19CD, data=panelage2010[-c(2)], sum)
panelage2011total <- aggregate(. ~LAD19CD, data=panelage2011[-c(2)], sum)
panelage2012total <- aggregate(. ~LAD19CD, data=panelage2012[-c(2)], sum)
panelage2013total <- aggregate(. ~LAD19CD, data=panelage2013[-c(2)], sum)
panelage2014total <- aggregate(. ~LAD19CD, data=panelage2014[-c(2)], sum)
panelage2015total <- aggregate(. ~LAD19CD, data=panelage2015[-c(2)], sum)
panelage2016total <- aggregate(. ~LAD19CD, data=panelage2016[-c(2)], sum)
panelage2017total <- aggregate(. ~LAD19CD, data=panelage2017[-c(2)], sum)
panelage2018total <- aggregate(. ~LAD19CD, data=panelage2018[-c(2)], sum)
panelage2019total <- aggregate(. ~LAD19CD, data=panelage2019[-c(2)], sum)
panelage2020total <- aggregate(. ~LAD19CD, data=panelage2020[-c(2)], sum)

panelage2010under18 <- aggregate(. ~LAD19CD, data=panelage2010under18[-c(2)], sum)
panelage2011under18 <- aggregate(. ~LAD19CD, data=panelage2011under18[-c(2)], sum)
panelage2012under18 <- aggregate(. ~LAD19CD, data=panelage2012under18[-c(2)], sum)
panelage2013under18 <- aggregate(. ~LAD19CD, data=panelage2013under18[-c(2)], sum)
panelage2014under18 <- aggregate(. ~LAD19CD, data=panelage2014under18[-c(2)], sum)
panelage2015under18 <- aggregate(. ~LAD19CD, data=panelage2015under18[-c(2)], sum)
panelage2016under18 <- aggregate(. ~LAD19CD, data=panelage2016under18[-c(2)], sum)
panelage2017under18 <- aggregate(. ~LAD19CD, data=panelage2017under18[-c(2)], sum)
panelage2018under18 <- aggregate(. ~LAD19CD, data=panelage2018under18[-c(2)], sum)
panelage2019under18 <- aggregate(. ~LAD19CD, data=panelage2019under18[-c(2)], sum)
panelage2020under18 <- aggregate(. ~LAD19CD, data=panelage2020under18[-c(2)], sum)

panelage2010total$year <- 2010
panelage2011total$year <- 2011
panelage2012total$year <- 2012
panelage2013total$year <- 2013
panelage2014total$year <- 2014
panelage2015total$year <- 2015
panelage2016total$year <- 2016
panelage2017total$year <- 2017
panelage2018total$year <- 2018
panelage2019total$year <- 2019
panelage2020total$year <- 2020

panelagetog2010 <- merge(panelage2010total, panelage2010under18, by="LAD19CD")
panelagetog2011 <- merge(panelage2011total, panelage2011under18, by="LAD19CD")
panelagetog2012 <- merge(panelage2012total, panelage2012under18, by="LAD19CD")
panelagetog2013 <- merge(panelage2013total, panelage2013under18, by="LAD19CD")
panelagetog2014 <- merge(panelage2014total, panelage2014under18, by="LAD19CD")
panelagetog2015 <- merge(panelage2015total, panelage2015under18, by="LAD19CD")
panelagetog2016 <- merge(panelage2016total, panelage2016under18, by="LAD19CD")
panelagetog2017 <- merge(panelage2017total, panelage2017under18, by="LAD19CD")
panelagetog2018 <- merge(panelage2018total, panelage2018under18, by="LAD19CD")
panelagetog2019 <- merge(panelage2019total, panelage2019under18, by="LAD19CD")
panelagetog2020 <- merge(panelage2020total, panelage2020under18, by="LAD19CD")

panelagetog2010$under18 <-(panelagetog2010$X2010.y/panelagetog2010$X2010.x)*100
panelagetog2011$under18 <-(panelagetog2011$X2011.y/panelagetog2011$X2011.x)*100
panelagetog2012$under18 <-(panelagetog2012$X2012.y/panelagetog2012$X2012.x)*100
panelagetog2013$under18 <-(panelagetog2013$X2013.y/panelagetog2013$X2013.x)*100
panelagetog2014$under18 <-(panelagetog2014$X2014.y/panelagetog2014$X2014.x)*100
panelagetog2015$under18 <-(panelagetog2015$X2015.y/panelagetog2015$X2015.x)*100
panelagetog2016$under18 <-(panelagetog2016$X2016.y/panelagetog2016$X2016.x)*100
panelagetog2017$under18 <-(panelagetog2017$X2017.y/panelagetog2017$X2017.x)*100
panelagetog2018$under18 <-(panelagetog2018$X2018.y/panelagetog2018$X2018.x)*100
panelagetog2019$under18 <-(panelagetog2019$X2019.y/panelagetog2019$X2019.x)*100
panelagetog2020$under18 <-(panelagetog2020$X2020.y/panelagetog2020$X2020.x)*100

panelagetog2010 <- panelagetog2010[c(1,3,5)]
panelagetog2011 <- panelagetog2011[c(1,3,5)]
panelagetog2012 <- panelagetog2012[c(1,3,5)]
panelagetog2013 <- panelagetog2013[c(1,3,5)]
panelagetog2014 <- panelagetog2014[c(1,3,5)]
panelagetog2015 <- panelagetog2015[c(1,3,5)]
panelagetog2016 <- panelagetog2016[c(1,3,5)]
panelagetog2017 <- panelagetog2017[c(1,3,5)]
panelagetog2018 <- panelagetog2018[c(1,3,5)]
panelagetog2019 <- panelagetog2019[c(1,3,5)]
panelagetog2020 <- panelagetog2020[c(1,3,5)]

panelageclean <- rbind(panelagetog2010, panelagetog2011, panelagetog2012, panelagetog2013, panelagetog2014, panelagetog2015, panelagetog2016, panelagetog2017, panelagetog2018, panelagetog2019, panelagetog2020 )
paneldata <- merge(paneldata, panelageclean, by=c("LAD19CD", "year"), all.x =T)


region <- read.csv("Data/LA_Care_Expenditure_By_Ownership_OverTime.csv")

region <- region[c(6,10)]
region <- unique(region)
region <- region[which(region$LAD19CD!=""),]

paneldata <- merge(paneldata, region, by="LAD19CD", all.x=T)

paneldata <- paneldata[which(paneldata$LAD19CD!=""),]

####Children SC workforce####

workforcechars <- read.csv("Data/data-children-s-social-work-workforce_21.csv")
workforcelook <- read.csv("Data/workforce lookup.csv")
workforcelook <- unique(workforcelook)

#Richmond and Kingston submit joint responses to this data - so need to merge
Richmonddf<-workforcechars[which(workforcechars$ï..LAD19CD=="E09000021"), ]

Richmonddf$ï..LAD19CD <- "E09000027"

workforcechars <- rbind(workforcechars, Richmonddf)
names(workforcechars)[names(workforcechars)=="ï..LAD19CD"] <- "LAD19CD"


workforcetype <- read.csv("Data/data-children-s-social-work-workforce_212.csv")
workforcetype <- merge(workforcetype, workforcelook, by="ï..location", all.x=T)
workforcetype <-workforcetype[-c(1)]


Richmonddf<-workforcetype[which(workforcetype$LAD19CD=="E09000021"), ]

Richmonddf$LAD19CD <- "E09000027"

workforcetype <- rbind(workforcetype, Richmonddf)

workforcetype <- workforcetype[complete.cases(workforcetype$LAD19CD),]
workforcechars <- workforcechars[complete.cases(workforcechars$LAD19CD),]

paneldata <- merge(paneldata, workforcechars, by=c("LAD19CD", "year"), all=T)
paneldata <- merge(paneldata, workforcetype, by=c("LAD19CD", "year"), all=T)

workforcetype20 <- workforcetype[which(workforcetype$year=="2020"),]
workforcechars20 <- workforcechars[which(workforcechars$year=="2020"),]

workforcetype20 <- workforcetype20[-c(2)]
workforcechars20 <- workforcechars20[-c(1)]

LAData <- merge(LAData, workforcechars20, by="LAD19CD", all.x=T)
LAData <- merge(LAData, workforcetype20, by="LAD19CD", all.x=T)


yearonly <-paneldata[c(2,4:9)]

yearonly$OwnProvision_spend <- as.double(yearonly$OwnProvision_spend)
yearonly$PrivateProvision_spend <- as.double(yearonly$PrivateProvision_spend)
yearonly$OtherPublic_spend <- as.double(yearonly$OtherPublic_spend)
yearonly$Voluntary_spend <- as.double(yearonly$Voluntary_spend)
yearonly$TotalExpenditure_spend <- as.double(yearonly$TotalExpenditure_spend)
yearonly$unit_cost <- as.double(yearonly$unit_cost)

yearonly$year <- factor(yearonly$year)

yearonly <- aggregate(. ~year, data=yearonly, sum,  na.action = na.pass)

yearonly$private_spend_per <- yearonly$PrivateProvision_spend/yearonly$TotalExpenditure_spend*100
LAData$private_spend_per <- as.double(LAData$PrivateProvision_spend_1000s_2020)/as.double(LAData$TotalExpenditure_spend_1000s_2020)*100

####LA Spend vs S251 documents####

LA_expend2020 <- read.csv("Data/LA_expenditure_socialcare.csv")

ExpenditureData <- read.csv("Data/LA_Care_Expenditure_By_Ownership_OverTime.csv")
expenditure2020 <- ExpenditureData[which(ExpenditureData$ï..time_period=="201920"),]

expenditure2020totallac <- expenditure2020[which(expenditure2020$Description=="3.1.11 Total Children Looked After"),]
expenditure2020totalsurestart <- expenditure2020[which(expenditure2020$Description=="Total Sure Start children's centres and other spend on children under 5"),]
expenditure2020totalyouthjustice <- expenditure2020[which(expenditure2020$Description=="Total Youth Justice"),]

expenditure2020totallac <- expenditure2020totallac[c(10,17,19)]
expenditure2020totalsurestart <- expenditure2020totalsurestart[c(10,17,19)]
expenditure2020totalyouthjustice <- expenditure2020totalyouthjustice[c(10,17,19)]

names(expenditure2020totallac)[names(expenditure2020totallac)=="TotalExpenditure"] <- "TotalExpenditure_LAC"
names(expenditure2020totallac)[names(expenditure2020totallac)=="NetCurrentExpenditure"] <- "NetExpenditure_LAC"

names(expenditure2020totalsurestart)[names(expenditure2020totalsurestart)=="TotalExpenditure"] <- "TotalExpenditure_surestart"
names(expenditure2020totalsurestart)[names(expenditure2020totalsurestart)=="NetCurrentExpenditure"] <- "NetExpenditure_surestart"

names(expenditure2020totalyouthjustice)[names(expenditure2020totalyouthjustice)=="TotalExpenditure"] <- "TotalExpenditure_youthjustice"
names(expenditure2020totalyouthjustice)[names(expenditure2020totalyouthjustice)=="NetCurrentExpenditure"] <- "NetExpenditure_youthjustice"

names(LA_expend2020)[names(LA_expend2020)=="ONS.Code"] <- "LAD19CD"

Expnditurecheck <- merge(LA_expend2020, expenditure2020totallac, by="LAD19CD")
Expnditurecheck <- merge(Expnditurecheck, expenditure2020totalyouthjustice, by="LAD19CD")
Expnditurecheck <- merge(Expnditurecheck, expenditure2020totalsurestart, by="LAD19CD")

Expnditurecheck$Children.Looked.After_Total.Expenditure <- as.double(Expnditurecheck$Children.Looked.After_Total.Expenditure)

Expnditurecheck <- Expnditurecheck[complete.cases(Expnditurecheck$Children.Looked.After_Total.Expenditure),]

#### Age and Provider Chains####
providerage <- ProviderData[c(2,7)]
providerage <- unique(providerage)
providerage <- providerage[complete.cases(providerage$Registration.date),]

providerage$age <- as.Date(providerage$Registration.date, format = "%d/%m/%Y")

providerage$month <- format(providerage$age,"%m/%y")
providerage$year <- format(providerage$age,"%Y")

providerage$age_months <- time_length(difftime(as.Date("2021-06-01"), providerage$age), "months")
providerage$age_years <- time_length(difftime(as.Date("2021-06-01"), providerage$age), "years")

providerage <- providerage[c(1,6,7)]

ProviderData <- merge(ProviderData, providerage, by="URN", all.x=T)

####lagged and leads####
paneldata$percent_private <- (as.double(paneldata$PrivateProvision_spend)/as.double(paneldata$TotalExpenditure_spend))*100

paneldatalagged <- paneldata[c("LAD19CD","year","CHARACTERISTICS (%) Private provision","percent_private", "agency_worker_rate_fte_.")]
paneldatalagged$year <- paneldatalagged$year+1
names(paneldatalagged)[names(paneldatalagged)=="CHARACTERISTICS (%) Private provision"] <- "lagged_private_provision"
names(paneldatalagged)[names(paneldatalagged)=="percent_private"] <- "lagged_private_spend"
names(paneldatalagged)[names(paneldatalagged)=="agency_worker_rate_fte_."] <- "lagged_agency_worker_rate"

paneldata <- merge(paneldata,paneldatalagged, by=c("LAD19CD", "year"), all.x=T)
paneldatalead <- paneldata[c("LAD19CD","year","CHARACTERISTICS (%) Private provision","percent_private", "agency_worker_rate_fte_.")]
paneldatalead$year <- paneldatalead$year-1
names(paneldatalead)[names(paneldatalead)=="CHARACTERISTICS (%) Private provision"] <- "lead_private_provision"
names(paneldatalead)[names(paneldatalead)=="percent_private"] <- "lead_private_spend"
names(paneldatalead)[names(paneldatalead)=="agency_worker_rate_fte_."] <- "lead_agency_worker_rate"

paneldata <- merge(paneldata,paneldatalead, by=c("LAD19CD", "year"), all.x=T)

paneldatanm <- unique(paneldata[c("LAD19NM", "LAD19CD")])
paneldatanm <- paneldatanm[complete.cases(paneldatanm),]
paneldata <- paneldata[-c(3)]
paneldata <- merge(paneldata, paneldatanm, by="LAD19CD", all.x=T)

####LA POLITICAL AFFILIATION####

councils2021 <- read.csv("Data/opencouncildata_councils.csv")
councils2021 <- councils2021[which(councils2021$model..C.county..D.district..1.all.up..3.thirds..etc..!="D1"&
                                     councils2021$model..C.county..D.district..1.all.up..3.thirds..etc..!="D2"&
                                     councils2021$model..C.county..D.district..1.all.up..3.thirds..etc..!="D3"),]

councils2018_19 <- read.csv("Data/history1973_2019.csv")
councils2018_19 <- councils2018_19[which(councils2018_19$Year=="2018"|councils2018_19$Year=="2019"),]

councils2018_19 <- councils2018_19[c("Authority", "Year", "Council_size", "Con", "Lab", "LD", "Other", "Control")]

councils2021$Other <- councils2021$total-(councils2021$lab+councils2021$con+councils2021$ld)

councils2021 <- councils2021[c("name", "total", "majority", "con", "lab", "ld","Other" ,"ons.code")]

names(councils2018_19) <- c("LAD19NM", "year", "Council_size", "Con", "Lab", "Ld", "Other", "Control")
names(councils2021) <- c("LAD19NM", "Council_size","Control" ,"Con" ,"Lab", "Ld","Other" ,"LAD19CD")
eng <- councils2021[c("LAD19NM")]
eng$eng <- 1
councils2018_19 <- merge(councils2018_19, eng,by="LAD19NM",all=T)
councils2018_19 <- councils2018_19[which(councils2018_19$eng==1|
                                           councils2018_19$LAD19NM=="Bournemouth"|
                                           councils2018_19$LAD19NM=="Buckinghamshire"|
                                           councils2018_19$LAD19NM=="Christchurch"|
                                           councils2018_19$LAD19NM=="East Dorset"|
                                           councils2018_19$LAD19NM=="Forest Heath"|
                                           councils2018_19$LAD19NM=="North Dorset"|
                                           councils2018_19$LAD19NM=="Poole"|
                                           councils2018_19$LAD19NM=="Purbeck"|
                                           councils2018_19$LAD19NM=="St Edmundsbury"|
                                           councils2018_19$LAD19NM=="Suffolk Coastal"|
                                           councils2018_19$LAD19NM=="Taunton Deane"|
                                           councils2018_19$LAD19NM=="Waveney"|
                                           councils2018_19$LAD19NM=="West Dorset"|
                                           councils2018_19$LAD19NM=="West Somerset"|
                                           councils2018_19$LAD19NM=="Weymouth and Portland"),]


councils2018_19 <- councils2018_19[complete.cases(councils2018_19$year),]

councils2018_19$LAD19CD <- NA
councils2021$year <- 2021
councils2020 <- councils2018_19[which(councils2018_19$year==2019),]
councils2020$year <- 2020

councils2021$eng <- 1

councils <- rbind(councils2018_19, councils2021, councils2020)

councils$LAD19NM[councils$LAD19NM=="Bournemouth"] <- "Bournemouth (Pre LG organisation)"
councils$LAD19NM[councils$LAD19NM=="Buckinghamshire CC"] <- "Buckinghamshire"
councils$LAD19NM[councils$LAD19NM=="Dorset"&councils$year==2018] <- "Dorset (Pre LG organisation)"
councils$LAD19NM[councils$LAD19NM=="Dorset"&councils$year==2019]<- "Dorset (Pre LG organisation)"
councils$LAD19NM[councils$LAD19NM=="Poole"] <- "Poole (Pre LG organisation)"
councils$LAD19NM[councils$LAD19NM=="Richmond upon Thames"] <- "Richmond Upon Thames"
councils$LAD19NM[councils$LAD19NM=="Southend-on-Sea"] <- "Southend on Sea"
councils$LAD19NM[councils$LAD19NM=="St. Helens"] <- "St Helens"

councils <- councils[-c(10)]


paneldata <- merge(paneldata, councils, by=c("LAD19NM", "year"),all=T)

er <- paneldata[c("LAD19CD", "LAD19NM", "year", "Lab")]

####Variable Table####

variabletable <- LAData
variabletable$private_spend <- as.double(variabletable$PrivateProvision_spend_1000s_2020)/as.double(variabletable$TotalExpenditure_spend_1000s_2020)*100

variabletable <- variabletable[c(1,2,4,5,6:9,13,16,26,28,31,33,36,38,41,43,46,48,51,53,56,95,96,102,108,117,136,148:150,152:156, 157)]

variabletable$Overall.effectiveness <- factor(variabletable$Overall.effectiveness , levels = c("Inadequate","Requires improvement to be good","Good","Outstanding" ))
variabletable$Overall.effectiveness <-revalue(variabletable$Overall.effectiveness, c("Inadequate"=1, "Requires improvement to be good"=2,"Good"=3,"Outstanding"=4 ))
variabletable$Overall.effectiveness <- as.double(variabletable$Overall.effectiveness)

variabletable$Impact.of.leaders <- factor(variabletable$Impact.of.leaders , levels = c("Inadequate","Requires improvement to be good","Good","Outstanding" ))
variabletable$Impact.of.leaders <-revalue(variabletable$Impact.of.leaders, c("Inadequate"=1, "Requires improvement to be good"=2,"Good"=3,"Outstanding"=4 ))
variabletable$Impact.of.leaders <- as.double(variabletable$Impact.of.leaders)

variabletable$Experiences.and.progress.of.children.who.need.help.and.protection <- factor(variabletable$Experiences.and.progress.of.children.who.need.help.and.protection , levels = c("Inadequate","Requires improvement to be good","Good","Outstanding" ))
variabletable$Experiences.and.progress.of.children.who.need.help.and.protection <-revalue(variabletable$Experiences.and.progress.of.children.who.need.help.and.protection, c("Inadequate"=1, "Requires improvement to be good"=2,"Good"=3,"Outstanding"=4 ))
variabletable$Experiences.and.progress.of.children.who.need.help.and.protection <- as.double(variabletable$Experiences.and.progress.of.children.who.need.help.and.protection)

variabletable$Experiences.and.progress.of.children.in.care.and.care.leavers <- factor(variabletable$Experiences.and.progress.of.children.in.care.and.care.leavers , levels = c("Inadequate","Requires improvement to be good","Good","Outstanding" ))
variabletable$Experiences.and.progress.of.children.in.care.and.care.leavers <-revalue(variabletable$Experiences.and.progress.of.children.in.care.and.care.leavers, c("Inadequate"=1, "Requires improvement to be good"=2,"Good"=3,"Outstanding"=4 ))
variabletable$Experiences.and.progress.of.children.in.care.and.care.leavers <- as.double(variabletable$Experiences.and.progress.of.children.in.care.and.care.leavers)

variabletable$`CHARACTERISTICS (%) Children who are the responsibility of this LA placed outside the LA boundary` <- as.double(variabletable$`CHARACTERISTICS (%) Children who are the responsibility of this LA placed outside the LA boundary`)
variabletable$`CHARACTERISTICS (%) 5. Secure units, children homes and semi-independent living accommodation` <- as.double(variabletable$`CHARACTERISTICS (%) 5. Secure units, children homes and semi-independent living accommodation`)
variabletable$`MISSING_STATS Children who had a missing incident during the year` <-as.double(variabletable$`MISSING_STATS Children who had a missing incident during the year`)
variabletable$`CONVICTION STATS (%) SDQ score is a cause for concern` <- as.double(variabletable$`CONVICTION STATS (%) SDQ score is a cause for concern`)

library(pastecs)


# paneldata <- paneldata[which(paneldata$LAD19CD!=""),]
# 
# panelnames <- unique(paneldata[c("LAD19CD", "LAD19NM")])
# panelnames <- panelnames[complete.cases(panelnames),]
# 
# 
# paneldata <- paneldata[-c(1)]
# paneldata <- merge(paneldata, panelnames, by="LAD19CD", all.x=T)
# paneldata <- unique(paneldata)


descriptivestats <- stat.desc(variabletable)
rm(list=setdiff(ls(), c("ProviderData", "LAData", "ProviderData2020", "yearonly", "paneldata")))

covid_data <- read.csv("Data/covid_visits_sept21tomay_22.csv")

# LAnames <- LAData[c(1,2)]
# 
# 
# 
# paneldata <- merge(paneldata, LAnames, by="LAD19CD", all.x=T)
# paneldata <- merge(paneldata, LAnames, by="LAD19CD", all.x=T)
# paneldata <- paneldata[which(paneldata$LAD19NM.x!="Isles of Scilly"),]
LAData <- LAData[which(LAData$LAD19NM!="Isles of Scilly"),]


ProviderData <- ProviderData[which(ProviderData$Event.type=="Full inspection"),]

ProviderData$Overall.experiences.and.progress.of.children.and.young.people <- factor(ProviderData$Overall.experiences.and.progress.of.children.and.young.people, levels = c("Inadequate","Requires improvement to be good", "Good","Outstanding" ), ordered = T)
ProviderData$Quality.of.care <- factor(ProviderData$Quality.of.care, levels = c("Inadequate","Requires improvement to be good", "Good","Outstanding" ), ordered = T)
ProviderData$The.effectiveness.of.leaders.and.managers <- factor(ProviderData$The.effectiveness.of.leaders.and.managers, levels = c("Inadequate","Requires improvement to be good", "Good","Outstanding" ), ordered = T)
ProviderData$How.well.children.and.young.people.are.helped.and.protected <- factor(ProviderData$How.well.children.and.young.people.are.helped.and.protected, levels = c("Inadequate","Requires improvement to be good", "Good","Outstanding" ), ordered = T)
ProviderData$Outcomes.for.children.and.young.people <- factor(ProviderData$Outcomes.for.children.and.young.people, levels = c("Inadequate","Requires improvement to be good", "Good","Outstanding" ), ordered = T)

ProviderData <- ProviderData[which(ProviderData$Provision.type!="Secure Training Centre"),]
ProviderData <- ProviderData[which(ProviderData$Provision.type!="Residential special school (>295 days/year)"),]
ProviderData <- ProviderData[which(ProviderData$Provision.type!="Residential Special School"),]
ProviderData <- ProviderData[which(ProviderData$Provision.type!="Boarding School"),]
ProviderData <- ProviderData[which(ProviderData$Provision.type!="Boarding school"),]
ProviderData <- ProviderData[which(ProviderData$Provision.type!="Secure training centre"),]
ProviderData <- ProviderData[which(ProviderData$Provision.type!="Further Education College with Residential Accommodation"),]
ProviderData <- ProviderData[which(ProviderData$Provision.type!="Voluntary Adoption Agency"),]
ProviderData <- ProviderData[which(ProviderData$Provision.type!="Secure children's home"),]
ProviderData <- ProviderData[which(ProviderData$Provision.type!="Residential special school (registered as a children's home)"),]
ProviderData <- ProviderData[which(ProviderData$Provision.type!="Residential Holiday Scheme for Disabled Children"),]
ProviderData <- ProviderData[which(ProviderData$Provision.type!="Residential Family Centre"),]
ProviderData <- ProviderData[which(ProviderData$Provision.type!="Further Education College with Residential Accommodation"),]
ProviderData <- ProviderData[which(ProviderData$Provision.type!="Adoption Support Agency"),]


ProviderData[which(ProviderData$Sector == "Health Authority"),]$Sector <- "Local Authority"
ProviderData[which(ProviderData$URN == "SC474543"),]$Sector <- "Voluntary"
ProviderData$date <- as.Date(ProviderData$Inspection.date, format =  "%d/%m/%Y")
ProviderData$year <- format(ProviderData$date,"%Y")
#write.csv(ProviderData, "Data/ProviderData_final.csv")


####Clean panel####
#cla2010 <- read.csv("Data/SSDA_2010.csv")
cla2011 <- read.csv("Data/SSDA_2011.csv")
cla2012 <- read.csv("Data/SSDA_2012.csv")
cla2013 <- read.csv("Data/SSDA_2013.csv")
cla2014 <- read.csv("Data/SSDA_2014.csv")
cla2015 <- read.csv("Data/SSDA_2015.csv")
cla2016 <- read.csv("Data/SSDA_2016.csv")
cla2017 <- read.csv("Data/SSDA_2017.csv")
cla2021 <- read.csv("Data/SSDA_2021.csv")
cla2021stab <- read.csv("Data/la_cla_at_31_march_placement_stability_indicator21.csv")

cla2011$year <- "2011"
cla2012$year <- "2012"
cla2013$year <- "2013"
cla2014$year <- "2014"
cla2015$year <- "2015"
cla2016$year <- "2016"
cla2017$year <- "2017"

cla2011 <-cla2011[which(cla2011$geog_l=="LA"),]
cla2012 <-cla2012[which(cla2012$geog_l=="LA"),]
cla2013 <-cla2013[which(cla2013$geog_l=="LA"),]
cla2014 <-cla2014[which(cla2014$geog_l=="LA"),]
cla2015 <-cla2015[which(cla2015$geog_l=="LA"),]
cla2016 <-cla2016[which(cla2016$geog_l=="LA"),]
cla2017 <-cla2017[which(cla2017$geog_l=="LA"),]

names(cla2011)[names(cla2011)=="CLA_Mar2011"] <- "CLA_Mar"
names(cla2012)[names(cla2012)=="CLA_Mar2012"] <- "CLA_Mar"
names(cla2013)[names(cla2013)=="CLA_Mar2013"] <- "CLA_Mar"
names(cla2014)[names(cla2014)=="CLA_Mar2014"] <- "CLA_Mar"
names(cla2015)[names(cla2015)=="CLA_Mar2015"] <- "CLA_Mar"
names(cla2016)[names(cla2016)=="CLA_Mar2016"] <- "CLA_Mar"
names(cla2017)[names(cla2017)=="CLA_Mar2017"] <- "CLA_Mar"

names(cla2011)[names(cla2011)=="CLA_stp2011"] <- "CLA_stp"
names(cla2012)[names(cla2012)=="CLA_stp2012"] <- "CLA_stp"
names(cla2013)[names(cla2013)=="CLA_stp2013"] <- "CLA_stp"
names(cla2014)[names(cla2014)=="CLA_stp2014"] <- "CLA_stp"
names(cla2015)[names(cla2015)=="CLA_stp2015"] <- "CLA_stp"
names(cla2016)[names(cla2016)=="CLA_stp2016"] <- "CLA_stp"
names(cla2017)[names(cla2017)=="CLA_stp2017"] <- "CLA_stp"

names(cla2011)[names(cla2011)=="CLA_2011"] <- "CLA"
names(cla2012)[names(cla2012)=="CLA_2012"] <- "CLA"
names(cla2013)[names(cla2013)=="CLA_2013"] <- "CLA"
names(cla2014)[names(cla2014)=="CLA_2014"] <- "CLA"
names(cla2015)[names(cla2015)=="CLA_2015"] <- "CLA"
names(cla2016)[names(cla2016)=="CLA_2016"] <- "CLA"
names(cla2017)[names(cla2017)=="CLA_2017"] <- "CLA"


names(cla2011)[names(cla2011)=="CLAA_InBound"] <- "CLA_InBound"
names(cla2012)[names(cla2012)=="CLAA_InBound"] <- "CLA_InBound"


#missing at 31 mar
#outsourcing
#demographics
#inside LA


cla2015$CLA_Miss <- NA
cla2016$CLA_Miss <- NA
cla2017$CLA_Miss <- NA

cla2015$CLA_Moth <- NA
cla2016$CLA_Moth <- NA
cla2017$CLA_Moth <- NA

cla2015$CLA_1Pla <- NA
cla2016$CLA_1Pla <- NA
cla2017$CLA_1Pla <- NA

cla2015$CLA_2PLa <- NA
cla2016$CLA_2PLa <- NA
cla2017$CLA_2PLa <- NA

cla2015$CLA_3Pla <- NA
cla2016$CLA_3Pla <- NA
cla2017$CLA_3Pla <- NA

cla2015$CLA_P2yrs <- NA
cla2016$CLA_P2yrs <- NA
cla2017$CLA_P2yrs <- NA

cla2016$CLA_OthPl <- NA

cla2017 <- cla2017[c(names(cla2011))]


CLA <- rbind(cla2011, cla2012, cla2013, cla2014, cla2015, cla2016, cla2017)


setdiff(names(cla2011), names(cla2017))



cla2021 <- cla2021[which(cla2021$geographic_level=="Local authority"),]
cla2021stab <- cla2021stab[which(cla2021stab$geographic_level=="Local authority"),]
cla2021 <- cla2021[which(cla2021$characteristic!="Total"|(cla2021$characteristic=="Total"&cla2021$cla_group=="Category of need")),]
cla2021stab <- cla2021stab[which(cla2021stab$characteristic!="Total children"),]
cla2021 <- cla2021[c(1,8,9,10,12,13)] %>% pivot_wider(names_from = characteristic, values_from = number, names_prefix = "CLA_")
cla2021stab <- cla2021stab[c(1,8,9,10,12,13)] %>% pivot_wider(names_from = characteristic, values_from = number, names_prefix = "CLA_")


names(cla2021)[names(cla2021)=="time_period"] <- "year"
names(cla2021)[names(cla2021)=="old_la_code"] <- "geog_c"
names(cla2021)[names(cla2021)=="la_name"] <- "geog_n"
names(cla2021)[names(cla2021)=="new_la_code"] <- "New_geog_code"
names(cla2021)[names(cla2021)=="CLA_Total"] <- "CLA_Mar"
names(cla2021)[names(cla2021)=="CLA_1 to 4 years"] <- "CLA_1to4"
names(cla2021)[names(cla2021)=="CLA_5 to 9 years"] <- "CLA_5to9"
names(cla2021)[names(cla2021)=="CLA_15 to 15 years"] <- "CLA_10to15"
names(cla2021)[names(cla2021)=="CLA_Under 1 year"] <- "CLA_U1"
names(cla2021)[names(cla2021)=="CLA_16 years and over"] <- "CLA_16over"
names(cla2021)[names(cla2021)=="CLA_Asian or Asian British"] <- "CLA_Asian"
names(cla2021)[names(cla2021)=="CLA_Black, African, Caribbean or Black British"] <- "CLA_Black"
names(cla2021)[names(cla2021)=="CLA_Mixed or Multiple ethnic groups"] <- "CLA_Mixed"
names(cla2021)[names(cla2021)=="CLA_Other ethnic group"] <- "CLA_EOTH"
names(cla2021)[names(cla2021)=="CLA_Refused or information not yet available"] <- "CLA_OTH"
names(cla2021)[names(cla2021)=="CLA_Female"] <- "CLA_female"
names(cla2021)[names(cla2021)=="CLA_Male"] <- "CLA_male"
names(cla2021)[names(cla2021)=="CLA_Other placements"] <- "CLA_OthPl"
names(cla2021)[names(cla2021)=="CLA_Foster placements"] <- "CLA_Fost"
names(cla2021)[names(cla2021)=="CLA_Placed for adoption"] <- "CLA_Adopt"
names(cla2021)[names(cla2021)=="CLA_Parents or other person with parental responsibility"] <- "CLA_Parent"
names(cla2021)[names(cla2021)=="CLA_Other placements in the community"] <- "CLA_Ocom"
names(cla2021)[names(cla2021)=="CLA_Other residential settings"] <- "CLA_Ores"
names(cla2021)[names(cla2021)=="CLA_Residential schools"] <- "CLA_RSch"
names(cla2021)[names(cla2021)=="CLA_Secure units, children's homes and semi-independent living accommodation"] <- "CLA_Secure"
names(cla2021)[names(cla2021)=="CLA_unaccompanied asylum-seeking children"] <- "CLA_UASC"
names(cla2021)[names(cla2021)=="CLA_Private provision"] <- "CLA_Priv"
names(cla2021)[names(cla2021)=="CLA_Voluntary/third sector provision"] <- "CLA_Vol"


names(cla2021stab)[names(cla2021stab)=="CLA_Living in the same placement for at least 2 years, or are placed for adoption and their adoption and their adoptive placement together with their previous placement, last for at least 2 years"] <- "CLA_P2yrs"
names(cla2021stab)[names(cla2021stab)=="ï..time_period"] <- "year"
names(cla2021stab)[names(cla2021stab)=="new_la_code"] <- "New_geog_code"
names(cla2021stab)[names(cla2021stab)=="old_la_code"] <- "geog_c"
names(cla2021stab)[names(cla2021stab)=="la_name"] <- "geog_n"

cla2021 <- merge(cla2021, cla2021stab, by=c("year", "New_geog_code", "geog_c", "geog_n"))




names(cla2021)[names(cla2021)=="CLA_Placed inside the local authority boundary"] <- "CLA_InBound"
names(cla2021)[names(cla2021)=="time_period"] <- "year"

out <- rbind(cla2021[c("New_geog_code","year" ,"CLA_Priv", "CLA_InBound","CLA_Mar", "CLA_Vol")], CLA[c("New_geog_code","year" ,"CLA_Priv","CLA_InBound" ,"CLA_Mar", "CLA_Vol")])
out$out <- as.double(out$CLA_Priv)/as.double(out$CLA_Mar)*100
out$vol <- as.double(out$CLA_Vol)/as.double(out$CLA_Mar)*100
out$lain <- as.double(out$CLA_InBound)/as.double(out$CLA_Mar)*100
out$outplus <- (as.double(out$CLA_Priv)+as.double(out$CLA_Vol))/as.double(out$CLA_Mar)*100

outmerge <- rbind(cla2021[c("New_geog_code","year" ,"CLA_Priv","CLA_Mar", "CLA_Vol","CLA_female", "CLA_White")], CLA[c("New_geog_code","year" ,"CLA_Priv" ,"CLA_Mar", "CLA_Vol", "CLA_female", "CLA_White")])
names(outmerge)[names(outmerge)=="New_geog_code"]<- "LAD19CD"
names(outmerge)[names(outmerge)=="CLA_Mar"]<- "Cic_n_march"
outmerge$for_profit_percent <- as.double(outmerge$CLA_Priv)/as.double(outmerge$Cic_n_march)*100
outmerge$third_sector_percent <- as.double(outmerge$CLA_Vol)/as.double(outmerge$Cic_n_march)*100
outmerge$CLA_white_percent <- as.double(outmerge$CLA_White)/as.double(outmerge$Cic_n_march)*100
outmerge$CLA_female_percent <- as.double(outmerge$CLA_female)/as.double(outmerge$Cic_n_march)*100

LAData2021 <- read.csv("Data/la_new_of.csv")
LAData2021$date <- as.Date(LAData2021$Inspection.date, format =  "%d/%m/%Y")
LAData2021$year <- format(LAData2021$date,"%Y")

# LAData2021[which(LAData2021$year=="2016"),]$year <- 2018
# LAData2021[which(LAData2021$year=="2017"),]$year <- 2018

names(LAData2021)[names(LAData2021)=="Overall.effectiveness"] <- "over"
names(LAData2021)[names(LAData2021)=="Impact.of.leaders"] <- "lead"
names(LAData2021)[names(LAData2021)=="Experiences.and.progress.of.children.who.need.help.and.protection"] <- "safe"
names(LAData2021)[names(LAData2021)=="Experiences.and.progress.of.children.in.care.and.care.leavers"] <- "exper"

paneldata <- merge(paneldata, LAData2021[c("LAD19NM", "year", "over", "lead", "safe", "exper")], by=c("LAD19NM", "year"), all.x=T )

paneldata$over <- factor(paneldata$over , levels = c("Inadequate","Requires improvement to be good","Good","Outstanding" ))
paneldata$lead <- factor(paneldata$lead , levels = c("Inadequate","Requires improvement to be good","Good","Outstanding" ))
paneldata$safe <- factor(paneldata$safe , levels = c("Inadequate","Requires improvement to be good","Good","Outstanding" ))
paneldata$exper <- factor(paneldata$exper , levels = c("Inadequate","Requires improvement to be good","Good","Outstanding" ))

mergedata <- paneldata[complete.cases(paneldata$over),]
mergedata2 <- merge(mergedata[c("LAD19NM","LAD19CD", "year", "over", "lead", "safe", "exper", "IMD.2019...Extent", "under18", "region_name")], outmerge[c("LAD19CD", "year", "for_profit_percent", "third_sector_percent", "Cic_n_march", "CLA_female_percent", "CLA_white_percent")], by=c("LAD19CD", "year"),all=T)

mergedata2 <- mergedata2[complete.cases(mergedata2$over),]
mergedata2$imd <- mergedata2$IMD.2019...Extent
mergedata2$Cic_n_march <- as.double(mergedata2$Cic_n_march)


mergedata2$region <- mergedata2$region_name
mergedata2$profit <- mergedata2$for_profit_percent
mergedata2$white <- mergedata2$CLA_white_percent
mergedata2$female <- mergedata2$CLA_female_percent
mergedata2$n <- mergedata2$Cic_n_march/100
mergedata2$third <- as.double(mergedata2$third_sector_percent)


mergedata2$over <- factor(mergedata2$over)
mergedata2$lead <- factor(mergedata2$lead)
mergedata2$safe <- factor(mergedata2$safe)
mergedata2$exper <- factor(mergedata2$exper)


ProviderData <- ProviderData[which(ProviderData$Event.type=="Full inspection"),]

ProviderData$Overall.experiences.and.progress.of.children.and.young.people <- factor(ProviderData$Overall.experiences.and.progress.of.children.and.young.people, levels = c("Inadequate","Requires improvement to be good", "Good","Outstanding" ), ordered = T)
ProviderData$Quality.of.care <- factor(ProviderData$Quality.of.care, levels = c("Inadequate","Requires improvement to be good", "Good","Outstanding" ), ordered = T)
ProviderData$The.effectiveness.of.leaders.and.managers <- factor(ProviderData$The.effectiveness.of.leaders.and.managers, levels = c("Inadequate","Requires improvement to be good", "Good","Outstanding" ), ordered = T)
ProviderData$How.well.children.and.young.people.are.helped.and.protected <- factor(ProviderData$How.well.children.and.young.people.are.helped.and.protected, levels = c("Inadequate","Requires improvement to be good", "Good","Outstanding" ), ordered = T)
ProviderData$Outcomes.for.children.and.young.people <- factor(ProviderData$Outcomes.for.children.and.young.people, levels = c("Inadequate","Requires improvement to be good", "Good","Outstanding" ), ordered = T)

ProviderData <- ProviderData[which(ProviderData$Provision.type!="Secure Training Centre"),]
ProviderData <- ProviderData[which(ProviderData$Provision.type!="Residential special school (>295 days/year)"),]
ProviderData <- ProviderData[which(ProviderData$Provision.type!="Residential Special School"),]
ProviderData <- ProviderData[which(ProviderData$Provision.type!="Boarding school"),]
ProviderData <- ProviderData[which(ProviderData$Provision.type!="Boarding School"),]
ProviderData <- ProviderData[which(ProviderData$Provision.type!="Further Education College with Residential Accommodation"),]

ProviderData[which(ProviderData$Sector == "Health authority")]$Sector <- "Local Authority"
ProviderData[which(ProviderData$Organisation.which.owns.the.provider == "Bromley Healthcare Community Interest Company")]$Sector <- "Voluntary"




ProviderData$Sector <- factor(ProviderData$Sector, levels = c("Local Authority", "Private", "Voluntary"))

ProviderData$Places <- as.double(ProviderData$Places)
ProviderData$chainsize <- as.double(ProviderData$chainsize)
ProviderData$chainsize_imputed <- as.double(ProviderData$chainsize_imputed)
ProviderData$age_months <- as.double(ProviderData$age_months)
ProviderData$date <- as.Date(ProviderData$Inspection.date, format =  "%d/%m/%Y")
ProviderData$year <- format(ProviderData$date,"%Y")

ProviderData <- ProviderData[which(ProviderData$Provision.type=="Children's home"),]
ProviderData$Recommendation.binary[is.na(ProviderData$Recommendation.binary)] <- 0
ProviderData$Requirement.binary[is.na(ProviderData$Requirement.binary)] <- 0
ProviderData$Number.of.Requirements[is.na(ProviderData$Number.of.Requirements)] <- 0
ProviderData$Number.of.Reccomendations[is.na(ProviderData$Number.of.Reccomendations)] <- 0
ProviderData$Requirement.binary[is.na(ProviderData$Requirement.binary)] <- 0
names(ProviderData)[names(ProviderData)=="Number.of.Reccomendations"] <- "Number.of.Recommendations"

ProviderData <- ProviderData %>% select(-contains("Rec"))%>% select(-contains("Req"))

