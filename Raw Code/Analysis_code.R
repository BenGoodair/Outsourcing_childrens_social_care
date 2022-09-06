if (!require("pacman")) install.packages("pacman")

pacman::p_load(devtools,knitr, dplyr, dotwhisker, tidyverse, pastecs, stringr, rebus, cobalt,lubridate ,pracma, zoo, future.apply, runner, plm, extrafont,texreg, CBPS, sp, maptools, sf, gt, gtsummary,modelsummary, regclass, stargazer, pbkrtest, sjPlot, lme4, clubSandwich, lmerTest, sf, sp, spdep, rgdal, rgeos, tmap, tmaptools,  spgwr, grid, gridExtra,curl, kableExtra,  MuMIn, parallel, tinytex, cowplot, ggforce, ggthemes, parallel, zen4R, ordinal, plyr)


#Download Data


ProviderData <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/Outsourcing_childrens_social_care/main/Data/ProviderData_submitted.csv"))
LAData <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/Outsourcing_childrens_social_care/main/Data/ladata_submitted.csv"))

####Table 1####

ProviderData$Overall.experiences.and.progress.of.children.and.young.people <- factor(ProviderData$Overall.experiences.and.progress.of.children.and.young.people, levels = c( "Inadequate", "Requires improvement to be good","Good", "Outstanding"))
ProviderData$How.well.children.and.young.people.are.helped.and.protected <- factor(ProviderData$How.well.children.and.young.people.are.helped.and.protected, levels = c( "Inadequate", "Requires improvement to be good","Good", "Outstanding"))
ProviderData$The.effectiveness.of.leaders.and.managers <- factor(ProviderData$The.effectiveness.of.leaders.and.managers, levels = c( "Inadequate", "Requires improvement to be good","Good", "Outstanding"))

ProviderData %>% 
  select(Overall.experiences.and.progress.of.children.and.young.people, The.effectiveness.of.leaders.and.managers ,How.well.children.and.young.people.are.helped.and.protected , Sector, age_months, Places, chainsize )%>%
  tbl_summary(
    by = c(Sector), # split table by group
    missing = "no",
    type = all_continuous() ~ "continuous2",
    statistic = all_continuous() ~ c("{N_nonmiss}",
                                     "{mean} ({median})", 
                                     "{min}, {max} ({sd})")# don't list missing data separately
  ) %>%
  add_n() %>% # add column with total number of non-missing observations
  #add_overall() %>%
  add_p() %>% # test for a difference between groups
  modify_spanning_header(c("stat_1", "stat_2") ~ "**Provider Sector**") %>%
  modify_caption("**Table. Provider Outcomes by Sector**") %>%
  modify_header(label = "**Variable**")%>%
  bold_labels()


####Figure 1#####


ProviderData$date <- as.Date(ProviderData$Inspection.date, format =  "%d/%m/%Y")
ProviderData$month <- format(ProviderData$date,"%m/%y")
ProviderData$time <- as.integer(time_length(difftime( as.Date(ProviderData$date), as.Date("2021-09-01")), "months"))
Providernobs <- unique(ProviderData[c("time", "Sector", "URN")][which(ProviderData$Provision.type=="Children's home"),])
nobsByIdih <- Providernobs %>% dplyr::group_by(time, Sector) %>% dplyr::summarize(nobs = n())
nobsprive <- nobsByIdih[which(nobsByIdih$Sector=="Private"),]
nobsvol <- nobsByIdih[which(nobsByIdih$Sector=="Voluntary"),]
nobsla <- nobsByIdih[which(nobsByIdih$Sector=="Local Authority"),]
cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

all <- unique(nobsprive[c("Sector")])
all<-all[rep(seq_len(nrow(all)), each = 92), ]  # Base R
all$time <- seq(from =-91, to=0)
all$er <- 1
nobsprive <- merge(nobsprive, all,by=c("Sector", "time"), all=T)
nobsprive[is.na(nobsprive$nobs),]$nobs <- 0
nobsprive$cumulative <- cumsum(nobsprive$nobs)

all <- unique(nobsla[c("Sector")])
all<-all[rep(seq_len(nrow(all)), each = 92), ]  # Base R
all$time <- seq(from =-91, to=0)
all$er <- 1
nobsla <- merge(nobsla, all,by=c("Sector", "time"), all=T)
nobsla[is.na(nobsla$nobs),]$nobs <- 0
nobsla$cumulative <- cumsum(nobsla$nobs)

all <- unique(nobsvol[c("Sector")])
all<-all[rep(seq_len(nrow(all)), each = 92), ]  # Base R
all$time <- seq(from =-91, to=0)
all$er <- 1
nobsvol <- merge(nobsvol, all,by=c("Sector", "time"), all=T)
nobsvol[is.na(nobsvol$nobs),]$nobs <- 0
nobsvol$cumulative <- cumsum(nobsvol$nobs)



nobs <- rbind(nobsla, nobsvol,nobsprive)

nobsvol$vol <- nobsvol$nobs
nobsla$la <- nobsla$nobs
nobsprive$prive <- nobsprive$nobs

nobsper <- merge(nobsla, nobsprive, by="time", all=T)
nobsper <-merge(nobsper, nobsvol, by="time", all=T)
nobsper$priveper <- nobsper$prive/(nobsper$la+nobsper$vol+nobsper$prive)*100

nobs$Sector <-  revalue(as.character(nobs$Sector), c("Private"="Private", "Local Authority"="Local Authority", "Voluntary"="Third Sector"))

nobs$Sector <- factor(nobs$Sector, levels = c("Private", "Local Authority", "Third Sector"))
levels(nobs$Sector) <- c("For-profit", "Local Authority", "Third Sector")

a <- ggplot(nobs, aes(x=time, y=cumulative, group=Sector,fill=Sector,  colour = Sector))+
  geom_point()+
  #geom_smooth(method="loess", span = 0.3)+
  theme_minimal()+
  scale_colour_manual(values = cbPalette[1:3])+
  annotate("rect", xmin = -17, xmax = -5, ymin = 0, ymax = 10700, alpha = .35)+
  labs(x="Year", y="Number of Ofsted Inspections\n(Cumulative)", title = "Ofsted inspections by ownership", fill="Ownership", color="Ownership")+
  scale_x_continuous(breaks=c(-7,-19,-31,-43,-55,-67,-79, -91),
                     labels=c("2021", "2020", "2019", "2018", "2017", "2016", "2015", "2014"))

ProviderData$date <- as.Date(ProviderData$Registration.date, format =  "%d/%m/%Y")
ProviderData$month <- format(ProviderData$date,"%m/%y")
ProviderData$time <- as.integer(time_length(difftime( as.Date(ProviderData$date), as.Date("2021-09-01")), "months"))
Providernobs <- unique(ProviderData[c("time", "Sector", "URN")][which(ProviderData$Provision.type=="Children's home"),])
nobsByIdih <- Providernobs %>% dplyr::group_by(time, Sector) %>% dplyr::summarize(nobs = n())
nobsprive <- nobsByIdih[which(nobsByIdih$Sector=="Private"),]
nobsvol <- nobsByIdih[which(nobsByIdih$Sector=="Voluntary"),]
nobsla <- nobsByIdih[which(nobsByIdih$Sector=="Local Authority"),]

all <- unique(nobsprive[c("Sector")])
all<-all[rep(seq_len(nrow(all)), each = 212), ]  # Base R
all$time <- seq(from =-211, to=0)
all$er <- 1
nobsprive <- merge(nobsprive, all,by=c("Sector", "time"), all=T)
nobsprive[is.na(nobsprive$nobs),]$nobs <- 0
nobsprive$cumulative <- cumsum(nobsprive$nobs)

all <- unique(nobsla[c("Sector")])
all<-all[rep(seq_len(nrow(all)), each = 212), ]  # Base R
all$time <- seq(from =-211, to=0)
all$er <- 1
nobsla <- merge(nobsla, all,by=c("Sector", "time"), all=T)
nobsla[is.na(nobsla$nobs),]$nobs <- 0
nobsla$cumulative <- cumsum(nobsla$nobs)

all <- unique(nobsvol[c("Sector")])
all<-all[rep(seq_len(nrow(all)), each = 212), ]  # Base R
all$time <- seq(from =-211, to=0)
all$er <- 1
nobsvol <- merge(nobsvol, all,by=c("Sector", "time"), all=T)
nobsvol[is.na(nobsvol$nobs),]$nobs <- 0
nobsvol$cumulative <- cumsum(nobsvol$nobs)



nobs <- rbind(nobsla, nobsvol,nobsprive)

nobs$Sector <-  revalue(as.character(nobs$Sector), c("Private"="Private", "Local Authority"="Local Authority", "Voluntary"="Third Sector"))

nobs$Sector <- factor(nobs$Sector, levels = c("Private", "Local Authority", "Third Sector"))
levels(nobs$Sector) <- c("For-profit", "Local Authority", "Third Sector")

Providernobs$Sector <-  revalue(as.character(Providernobs$Sector), c("Private"="Private", "Local Authority"="Local Authority", "Voluntary"="Third Sector"))
Providernobs$Sector <- factor(Providernobs$Sector, levels = c("Private", "Local Authority", "Third Sector"))
levels(Providernobs$Sector) <- c("For-profit", "Local Authority", "Third Sector")

d <- ggplot(nobs[which(nobs$time>-211),], aes(x=time, y=cumulative, group=Sector,fill=Sector,  colour = Sector))+
  geom_point()+
  #geom_smooth(method="loess", span = 0.3)+
  theme_minimal()+
  scale_colour_manual(values = cbPalette[1:3])+
  # theme(legend.position="left")+
  labs(x="Year", y="Number of Children's homes", title = "Number of active children's homes", fill="Ownership", color="Ownership")+
  scale_x_continuous(breaks=c(-7,-31,-55,-79,-103,-127,-151,-175, -199),
                     labels=c("2021","2019",  "2017", "2015", "2013","2011", "2009", "2007", "2005"))


cowplot::plot_grid(a,d, ncol=2 ,labels="AUTO")


####Table 2####
ProviderData$Overall.experiences.and.progress.of.children.and.young.people <- factor(ProviderData$Overall.experiences.and.progress.of.children.and.young.people, levels = c( "Inadequate", "Requires improvement to be good","Good", "Outstanding"))
ProviderData$How.well.children.and.young.people.are.helped.and.protected <- factor(ProviderData$How.well.children.and.young.people.are.helped.and.protected, levels = c( "Inadequate", "Requires improvement to be good","Good", "Outstanding"))
ProviderData$The.effectiveness.of.leaders.and.managers <- factor(ProviderData$The.effectiveness.of.leaders.and.managers, levels = c( "Inadequate", "Requires improvement to be good","Good", "Outstanding"))


randommodel = clmm(Overall.experiences.and.progress.of.children.and.young.people ~  Sector + factor(year)+ (1|URN),
                   data = ProviderData)

randommodel5 = clmm(Overall.experiences.and.progress.of.children.and.young.people ~  Sector + age_months+chainsize +Places+factor(year)+ (1|URN),
                    data = ProviderData)

randommodel2 = clmm(The.effectiveness.of.leaders.and.managers ~  Sector +factor(year) + (1|URN),
                    data = ProviderData)

# 
randommodel25 = clmm(The.effectiveness.of.leaders.and.managers ~  Sector + age_months+chainsize+Places+factor(year) + (1|URN),
                     data = ProviderData)

randommodel3 = clmm(How.well.children.and.young.people.are.helped.and.protected ~ Sector +factor(year) + (1|URN),
                    data = ProviderData)

# 
randommodel35 = clmm(How.well.children.and.young.people.are.helped.and.protected ~ Sector + age_months+chainsize+Places+factor(year) + (1|URN),
                     data = ProviderData)
# 
options("modelsummary_get" = "easystats")

cm <- c('SectorPrivate' = 'For-Profit [ref: LA]',
        'SectorVoluntary' = 'Third Sector [ref: LA]',
        'age_months' = 'Age (Months)',
        'chainsize' = 'Chain size (n)',
        'Places' = 'Places (n)')

rows <- tribble(~term,          ~`(1)`,  ~`(2)`, ~`(3)`, ~`(4)`, ~`(5)`, ~`(6)`, 
                'Provder Random Effects', 'Yes',  'Yes','Yes','Yes',  'Yes','Yes',
                'Time Fixed Effects', 'Yes',  'Yes','Yes','Yes',  'Yes','Yes',
                'Clustered Standard Errors', 'Yes',  'Yes','Yes','Yes',  'Yes','Yes')



tidy_custom.clmm <- function(x, ...) {
  broom::tidy(x)%>%
    dplyr::mutate(std.error = broom::tidy(x, cluster = "URN")$std.error)%>%
    dplyr::mutate(p.value = broom::tidy(x, cluster = "URN")$p.value)%>%
    dplyr::mutate(estimate = exp(broom::tidy(x, cluster = "URN")$estimate))
}

table <- modelsummary(list("(1)"=randommodel,"(2)"=randommodel5,"(3)"=randommodel2,"(4)"=randommodel25,"(5)"=randommodel3,"(6)"=randommodel35),
                      exponentiate = T,add_rows = rows,stars = T ,
                      coef_omit = "Intercept|year", coef_map = cm, output = "gt") %>%
  tab_spanner(label = 'Overall Experiences', columns = 2:3) %>%
  tab_spanner(label = 'Effectiveness of Leadership ', columns = 4:5) %>%
  tab_spanner(label = 'Help and Protection for Children', columns = 6:7)

table

####Figure 2####

pred1 <-emmeans(randommodel5, ~ Overall.experiences.and.progress.of.children.and.young.people | Sector, mode = "prob")
pred2 <-emmeans(randommodel25, ~ The.effectiveness.of.leaders.and.managers | Sector, mode = "prob")
pred3 <-emmeans(randommodel35, ~ How.well.children.and.young.people.are.helped.and.protected | Sector, mode = "prob")

pred1 <- as.data.frame(pred1)
pred2 <- as.data.frame(pred2)
pred3 <- as.data.frame(pred3)

names(pred1)[names(pred1)=="Overall.experiences.and.progress.of.children.and.young.people"] <- "Ratings"
names(pred2)[names(pred2)=="The.effectiveness.of.leaders.and.managers"] <- "Ratings"
names(pred3)[names(pred3)=="How.well.children.and.young.people.are.helped.and.protected"] <- "Ratings"

pred1$outcome <- "Overall experience"
pred2$outcome <- "Leadership and Management"
pred3$outcome <- "Help and Support"

pred1$Sector <- as.character(pred1$Sector)
pred2$Sector <- as.character(pred2$Sector)
pred3$Sector <- as.character(pred3$Sector)

pred1$Sector[pred1$Sector=="Private"] <- "For-Profit"
pred2$Sector[pred2$Sector=="Private"] <- "For-Profit"
pred3$Sector[pred3$Sector=="Private"] <- "For-Profit"

pred1$Sector[pred1$Sector=="Voluntary"] <- "Third Sector"
pred2$Sector[pred2$Sector=="Voluntary"] <- "Third Sector"
pred3$Sector[pred3$Sector=="Voluntary"] <- "Third Sector"


predall <- rbind(pred1, pred2, pred3)

predall$Ratings <-  revalue(as.character(predall$Ratings), c("4"="Outstanding", "3"="Good", "2"="Requires improvement","1"= "Inadequate"))
predall$Ratings <- factor(predall$Ratings, levels = c("Outstanding","Good","Requires improvement","Inadequate"))



pred1$Ratings <-  revalue(as.character(pred1$Ratings), c("4"="Outstanding", "3"="Good", "2"="Requires improvement","1"= "Inadequate"))
pred2$Ratings <-  revalue(as.character(pred2$Ratings), c("4"="Outstanding", "3"="Good", "2"="Requires improvement","1"= "Inadequate"))
pred3$Ratings <-  revalue(as.character(pred3$Ratings), c("4"="Outstanding", "3"="Good", "2"="Requires improvement","1"= "Inadequate"))

#reorder variable
pred1$Ratings <- factor(pred1$Ratings, levels = c("Outstanding","Good","Requires improvement","Inadequate"))
pred2$Ratings <- factor(pred2$Ratings, levels = c("Outstanding","Good","Requires improvement","Inadequate"))
pred3$Ratings <- factor(pred3$Ratings, levels = c("Outstanding","Good","Requires improvement","Inadequate"))

pred1$Sector <- factor(pred1$Sector, levels = c("Local Authority","For-Profit","Third Sector"))
pred2$Sector <- factor(pred2$Sector, levels = c("Local Authority","For-Profit","Third Sector"))
pred3$Sector <- factor(pred3$Sector, levels = c("Local Authority","For-Profit","Third Sector"))

cbPalette <- c( "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

plot<- ggplot(pred1, aes(x = Sector, y = prob)) + 
  geom_point(aes(color = Sector), position =position_dodge(width = 0.2), size=2) +
  ylim(0,0.81)+
  labs(title="Overall Experience", y="Probability",x="" )+
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL, color = Sector), position = position_dodge(width = 0.5),  width=0.5,size=0.6) + 
  theme_minimal() + scale_color_manual(values = cbPalette[1:3])+theme(legend.position = "None", 
                                                                      strip.background = element_blank(),
                                                                      strip.placement = "outside", axis.text.x = element_text(size=7), strip.text = element_text(face="bold"),panel.spacing = unit(2, "lines"))+
  facet_wrap(~Ratings, strip.position = "bottom", scales = "free_x", ncol = 4) 


plot2<- ggplot(pred2, aes(x = Sector, y = prob)) + 
  geom_point(aes(color = Sector), position =position_dodge(width = 0.2), size=2) +
  ylim(0,0.81)+
  labs(title="Leadership & Management", y="Probability",x="" )+
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL, color = Sector), position = position_dodge(width = 0.5), width = 0.5,size=0.6) + 
  theme_minimal() + scale_color_manual(values = cbPalette[1:3])+theme(legend.position = "None",
                                                                      strip.background = element_blank(),
                                                                      strip.placement = "outside", axis.text.x = element_text(size=7), strip.text = element_text(face="bold"),panel.spacing = unit(2, "lines"))+
  facet_wrap(~Ratings, strip.position = "bottom", scales = "free_x", ncol = 4) 

plot3<- ggplot(pred3, aes(x = Sector, y = prob)) + 
  geom_point(aes(color = Sector), position =position_dodge(width = 0.2), size=2) +
  labs(title="Help & support", y="Probability",x=""  ,color="Ownership")+
  ylim(0,0.81)+
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL, color = Sector), position = position_dodge(width = 0.5), width = 0.5,size=0.6) + 
  theme_minimal() + scale_color_manual(values = cbPalette[1:3])+theme(legend.position = "bottom",
                                                                      strip.background = element_blank(),
                                                                      strip.placement = "outside", axis.text.x = element_text(size=7), strip.text = element_text(face="bold"),panel.spacing = unit(2, "lines"))+
  facet_wrap(~Ratings, strip.position = "bottom", scales = "free_x", ncol = 4) 

library(cowplot)
once <- cowplot::plot_grid(plot, plot2, ncol=2 ,labels="AUTO")
twice <- cowplot::plot_grid(NULL, plot3, NULL ,ncol=3, rel_widths = c(0.4, 1.2, 0.4) ,labels=c("", "C", ""))
all <- cowplot::plot_grid(once, twice, ncol=1 )
cowplot::plot_grid(plot, plot2, plot3, ncol=1 ,rel_widths = c(1, 1, 1.65) ,labels="AUTO")


####Table 3####
# olr <- lm(Number.of.Requirements~Sector+factor(year) ,data = finalrecdata)
# olr2 <- lm(Number.of.Requirements~Sector+age_months+chainsize+Places+factor(year),data = finalrecdata)
# olr3 <- glm(Requirement.binary~Sector+factor(year) ,data = finalrecdata,  family = "binomial")
# olr4 <- glm(Requirement.binary~Sector+age_months+chainsize+Places+factor(year) ,data = finalrecdata, family = "binomial")
# 
# 
# 
# olrrec <- lm(Number.of.Reccomendations~Sector+factor(year) ,data = finalrecdata)
# olrrec2 <- lm(Number.of.Reccomendations~Sector+age_months+chainsize+Places+factor(year),data = finalrecdata)
# olrrec3 <- glm(Recommendation.binary~Sector+factor(year) ,data = finalrecdata,  family = "binomial")
# olrrec4 <- glm(Recommendation.binary~Sector+age_months+chainsize+Places+factor(year) ,data = finalrecdata, family = "binomial")
# 
# #confint(olr4,parm="beta_",method="Wald")
# 
# 
# 
# options("modelsummary_get" = "easystats")
# rows <- tribble(~term,          ~ `(1)`, ~ `(2)`, ~ `(3)`, ~ `(4)`, ~ `(5)`, ~ `(6)`, ~ `(7)`, ~ `(8)`,
#                 'Provder Random Effects', 'Yes',  'Yes','Yes', 'Yes',  'Yes','Yes', 'Yes',  'Yes',
#                 'Time Fixed Effects', 'Yes',  'Yes','Yes', 'Yes',  'Yes','Yes', 'Yes','Yes',
#                 'Clustered Standard Errors', 'Yes',  'Yes','Yes', 'Yes',  'Yes','Yes', 'Yes','Yes',)
# 
# cm <- c('SectorPrivate' = 'For-Profit [ref: LA]',
#         'SectorVoluntary' = 'Third Sector [ref: LA]',
#         'age_months' = 'Age (Months)',
#         'chainsize' = 'Chain size (n)',
#         'Places' = 'Places (n)')
# 
# tidy_custom.glm <- function(x, ...) {
#   broom::tidy(x)%>%
#     dplyr::mutate(std.error = broom::tidy(x, cluster = "URN")$std.error)%>%
#     dplyr::mutate(p.value = broom::tidy(x, cluster = "URN")$p.value)%>%
#     dplyr::mutate(estimate = exp(broom::tidy(x, cluster = "URN")$estimate))
# }
# tidy_custom.lm <- function(x, ...) {
#   broom::tidy(x)%>%
#     dplyr::mutate(std.error = broom::tidy(x, cluster = "URN")$std.error)%>%
#     dplyr::mutate(p.value = broom::tidy(x, cluster = "URN")$p.value)%>%
#     dplyr::mutate(estimate = broom::tidy(x, cluster = "URN")$estimate)
# }
# 
# modelsummary(list( "(1)"=olr, "(2)"=olr2, "(3)"=olr3, "(4)"=olr4,"(5)"=olrrec,"(6)"= olrrec2, "(7)"=olrrec3, "(8)"=olrrec4),add_rows = rows,stars = T ,
#              coef_omit = "Intercept|year", coef_map = cm , output = "gt") %>%
#   tab_spanner(label = 'Number of Requirements', columns = 2:3) %>%
#   tab_spanner(label = 'Requirement Binary', columns = 4:5) %>%
#   tab_spanner(label = 'Number of Recommendations', columns = 6:7)%>%
#   tab_spanner(label = 'Recommendation Binary', columns = 8:9)%>%
#   tab_options(table.width=pct(100))


####Table 4####

LAData$over <- factor(LAData$over, levels = c( "Inadequate", "Requires improvement to be good","Good", "Outstanding"))
LAData$safe <- factor(LAData$safe, levels = c( "Inadequate", "Requires improvement to be good","Good", "Outstanding"))
LAData$lead <- factor(LAData$lead, levels = c( "Inadequate", "Requires improvement to be good","Good", "Outstanding"))
LAData$exper <- factor(LAData$exper, levels = c( "Inadequate", "Requires improvement to be good","Good", "Outstanding"))


# 
# model1 <-  clm(over~profit+white+imd+female+under18+n+factor(region),Hess = T, link="logit" , data = mergedata2)
# model2 <-  clm(lead~profit+white+imd+female+under18+n+factor(region),Hess = T, link="logit" , data = mergedata2)
# model3 <-  clm(safe~profit+white+imd+female+under18+n+factor(region),Hess = T, link="logit" , data = mergedata2)
# model4 <-  clm(exper~profit+white+imd+female+under18+n+factor(region),Hess = T, link="logit" , data = mergedata2)
# 
# nominal_test1la <- nominal_test(model1)
# nominal_test2la <- nominal_test(model2)
# nominal_test3la <- nominal_test(model3)
# nominal_test4la <- nominal_test(model4)
# 
# 
# 
# scale_test1la <- scale_test(model1)
# scale_test2la <- scale_test(model2)
# scale_test3la <- scale_test(model3)
# scale_test4la <- scale_test(model4)


model1 <-  clm(over~profit+imd+female+n+factor(region), scale =~white ,Hess = T, link="logit" , data = LAData)
model2 <-  clm(lead~profit+white+female+under18+n+factor(region), scale =~imd,Hess = T, link="logit" , data = LAData)
model3 <-  clm(safe~profit+white+imd+female+under18+n+factor(region), nominal = ~n,Hess = T, link="logit" , data = LAData)
model4 <-  clm(exper~profit+white+imd+female+under18+n+factor(region),Hess = T, link="logit" , data = LAData)




options("modelsummary_get" = "easystats")

# cm <- c('private' = 'For-profit Outsourcing (%)')
# rows <- tribble(~term,          ~`(1)`,  ~`(2)`, ~`(3)`, ~`(4)`,
#                 'Controls', 'Yes',  'Yes','Yes','Yes')

library(gtsummary)
table <- modelsummary(list("(1)"=model1,"(2)"=model2,"(3)"=model3,"(4)"=model4),stars = T ,
                      exponentiate = TRUE ,coef_omit = "Intercept|year",  
                      notes = list('N for CiC measured in hundreds.'),output = "gt") %>%
  tab_spanner(label = 'Overall Effectiveness', columns = 2) %>%
  tab_spanner(label = 'Effectiveness of Leaders', columns = 3) %>%
  tab_spanner(label = 'Experiences of children who need protection', columns = 4)%>%
  tab_spanner(label = 'Experiences and progress of children', columns = 5)

table










