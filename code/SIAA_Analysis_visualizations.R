title: "SIAA_Analysis"
author: "Sameer Desai & Srividya Dasaraju"
date: "11/7/2020"
output:
  pdf_document: default
html_document: default

#check working directory
setwd("~/Documents/Documents - Srividya’s MacBook Air/SIAA/RDatasets")
getwd()

#load in data
SIAAData <- read.csv("matched_SIAA_Final.csv")


#install.packages(c("survey", "ggthemes", "ggplot2", "dplyr", 
                   #"ggpubr", "gridExtra", "ggalt"))
library(ggplot2)
library(ggpubr)
library(grid)
library(gridExtra)
library(dplyr)
library(survey)
library(ggthemes)
library(readr)
library(ggplot2)
library(leaflet)
library(dplyr)
library(ggmap)
library(data.table)
library(Hmisc)
library(summarytools)
library(ggmap)
library(ggalt)
#############
#by student and parent
#number of students who strongly approve number of parent

#######################
#Harris Enthusiasm Voting 

#select student column 
a <- SIAAData %>%
  select(harris_voting_students)
#group answers, count how many of each answer, rename the column to be called answer 
a1 <- a %>%
  group_by(harris_voting_students) %>% tally() %>% rename (Answer = harris_voting_students) %>%
  rename(Count1 = n)
#create new column to indicate respondent type = student, and populate with student in cells
a1$RespType1 <- 'student'
a1$student_pct = ((a1$Count1 / sum(a1$Count1))*100)

#select parent column, group answers, count how many of each answer, rename column to be called answer
b <-  SIAAData %>% select(harris_voting_parents)  %>% 
      group_by(harris_voting_parents) %>% tally() %>% rename (Answer = harris_voting_parents) %>% 
      rename(Count2 = n)
#create new column to indicate respondent type = parent, and populate with parent in cells
b$RespType2 <- 'parent'
b$parent_pct = ((b$Count2 / sum(b$Count2))*100)


#join to create new df 
c <- left_join(a1,b, by=c("Answer"))
c <- c %>% slice(1:3)

c$student_pct <- round(c$student_pct)
c$parent_pct <- round(c$parent_pct)

c$difference <- abs(c$Count1 - c$Count2)
c$difference_pct <- abs(c$student_pct - c$parent_pct)
#c$difference_pct <- round(c$difference_pct, 1)

#student
#green <- "DarkGreen"
green <- "indianred3"
#parent
#blue <- "darkblue"
blue <- "steelblue"


fig1 <- ggplot() +
  geom_segment(data=c, aes(y=Answer, yend=Answer, x=0, xend=90), 
               color="#b2b2b2", size=0.15) +
  geom_dumbbell(data=c, aes(y=Answer, x=student_pct, xend=parent_pct),
                size=1, color="#b2b2b2", size_x=5, size_xend = 5, colour_x = green, 
                colour_xend = blue) +
  geom_text(data=filter(c, Answer=="More likely"),
          aes(x=student_pct, y=Answer, label="Students"),
          color=green, size=3, vjust=-1.5, fontface="bold") +
  geom_text(data=filter(c, Answer=="More likely"),
            aes(x=parent_pct, y=Answer, label="Parents"),
            color=blue, size=3, vjust=-1.5, fontface="bold") +
  geom_text(data=c, aes(x=student_pct, y=Answer, label=round(student_pct,digits=0)),
            color=green, size=2.75, vjust=2.5) +
  geom_text(data=c, color=blue, size=2.75, vjust=2.5,
            aes(x=parent_pct, y=Answer, label=round(parent_pct,digits=0))) +
  geom_rect(data=c, aes(xmin=80, xmax=90, ymin=-Inf, ymax=Inf), fill="gray") +
  geom_text(data=c, aes(label=difference_pct, y=Answer, x=85), size=3) +
  geom_text(data=filter(c,  Answer=="More likely"), 
            aes(x=85, y=Answer, label="Difference"),
            color="black", size=3.1, vjust=-1.5) +
  scale_y_discrete(name ="Answer", limits=c("Less likely", "Makes no difference","More likely")) +
  labs(x=NULL, y=NULL, 
      title="Figure 9
       
Impact of Harris VP Choice on Voting, by Generation",
       subtitle="Percent of respondents who...", x = "Percent",
       caption="N = 51 Pairs of Students and Parents
       Source: 2020 Survey of Indian-American Attitudes") + theme_light()
fig1 + theme(plot.subtitle = element_text(face = "italic")) +
  theme(plot.background = element_blank(),panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) 


#make scale for p & s

#ggplotly makes it interactive 
#library(plotly)
#fig1a <- ggplotly(fig1)
#fig1a

########################################

##############################################
#political Ideology categorization 

d <- SIAAData %>%
  select(ideological_scale_students) %>% group_by(ideological_scale_students) %>% tally() %>%  
  rename (Answer = ideological_scale_students) %>% rename(Student = n)
#create new column to indicate respondent type = student, and populate with student in cells
d$RespType1 <- 'student'
d$student_pct = ((d$Student / sum(d$Student))*100)

  
#select parent column, group answers, count how many of each answer, rename column to be called answer
e <- SIAAData %>%
  select(ideological_scale_parents) %>% group_by(ideological_scale_parents) %>% tally() %>% 
  rename (Answer = ideological_scale_parents) %>% rename(Parent = n)
#create new column to indicate respondent type = parent, and populate with parent in cells
e$RespType2 <- 'parent'
e$parent_pct = ((e$Parent / sum(e$Parent))*100)

#join to create new df, put e first so we retain the broader parent answer
f <- left_join(e,d, by=c("Answer"))

f$RespType1[is.na(f$RespType1)] <- "student"
f <- f %>% slice(1:7)
f[is.na(f)] <- 0


f$difference <- abs(f$Student - f$Parent)
f$student_pct <- round(f$student_pct)
f$parent_pct <- round(f$parent_pct)
f$difference_pct <- abs(f$student_pct - f$parent_pct)

green <- "indianred3"
blue <- "steelblue"

(f$Answer[f$Answer == "Moderate; middle of the road"] <- "Moderate")

#e1 <- e %>% slice(3,6,8)
#e1$parent_pct <- round(e1$parent_pct)




#fig2 <-
#  ggplot() +
 # geom_segment(data=f, aes(y=Answer, yend=Answer, x=0, xend=50), 
          #     color="#b2b2b2", size=0.15) +
 # geom_dumbbell(data=f, aes(y=Answer, x=student_pct, xend=parent_pct),
           #     size=1, color="#b2b2b2", size_x=5, size_xend = 5, colour_x = green, 
            #    colour_xend = blue) +
 # geom_point(data=e1, aes(y=Answer, x=parent_pct), size=5, color=blue) +
#  geom_text(data=filter(f, Answer=="Extremely liberal"),
       #     aes(x=student_pct, y=Answer, label="Students"),
   #         color=green, size=3, vjust=-1.5, fontface="bold") +
#  geom_text(data=filter(f, Answer=="Extremely liberal"),
    #        aes(x=parent_pct, y=Answer, label="Parents"),
    #        color=blue, size=3, vjust=-1.5, fontface="bold") +
#  geom_text(data=f, aes(x=student_pct, y=Answer, label=round(student_pct,digits=0)),
#            color=green, size=2.75, vjust=2.5) +
 # geom_text(data=f, color=blue, size=2.75, vjust=2.5,
    #        aes(x=parent_pct, y=Answer, label=round(parent_pct,digits=0))) +
 # geom_rect(data=f, aes(xmin=45, xmax=55, ymin=-Inf, ymax=Inf), fill="gray") +
 # geom_text(data=f, aes(label=difference_pct, y=Answer, x=50), size=3) +
 # geom_text(data=e1, aes(label=parent_pct, y=Answer, x=50), size=3) +
 # geom_text(data=filter(f,  Answer=="Extremely liberal"), 
          #  aes(x=50, y=Answer, label="Difference"),
          #  color="black", size=3.1, vjust=-1.5) +
 # scale_y_discrete(name ="Answer", limits=c("Haven't thought much about this", "Conservative", 
   #  "Slightly conservative","Moderate; middle of the road", "Slightly liberal",
   #  "Liberal", "Extremely liberal")) +
 # labs(x=NULL, y=NULL, title="Figure 7

#Ideological Scale by Generation",
     #  subtitle="Percent of respondents who identify as...",
     #  caption="N = 51 Pairs of Students and Parents
     #  Source: 2020 Survey of Indian-American Attitudes") + theme_light()
#fig2 + theme(plot.subtitle = element_text(face = "italic")) +
#  theme(plot.background = element_blank(),panel.grid.major = element_blank(),
  #      panel.grid.minor = element_blank()) 



fig2 <-
  ggplot() +
  geom_segment(data=f, aes(y=Answer, yend=Answer, x=0, xend=50), 
               color="#b2b2b2", size=0.15) +
  geom_dumbbell(data=f, aes(y=Answer, x=student_pct, xend=parent_pct),
                size=1, color="#b2b2b2", size_x=5, size_xend = 5, colour_x = green, 
                colour_xend = blue) +
  geom_text(data=filter(f, Answer=="Extremely liberal"),
            aes(x=student_pct, y=Answer, label="Students"),
            color=green, size=3, vjust=-1.5, fontface="bold") +
  geom_text(data=filter(f, Answer=="Extremely liberal"),
            aes(x=parent_pct, y=Answer, label="Parents"),
            color=blue, size=3, vjust=-1.5, fontface="bold") +
  geom_text(data=f, aes(x=student_pct, y=Answer, label=round(student_pct,digits=0)),
            color=green, size=2.75, vjust=2.5) +
  geom_text(data=f, color=blue, size=2.75, vjust=2.5,
            aes(x=parent_pct, y=Answer, label=round(parent_pct,digits=0))) +
  geom_rect(data=f, aes(xmin=45, xmax=55, ymin=-Inf, ymax=Inf), fill="gray") +
  geom_text(data=f, aes(label=difference_pct, y=Answer, x=50), size=3) +
  geom_text(data=filter(f,  Answer=="Extremely liberal"), 
            aes(x=50, y=Answer, label="Difference"),
            color="black", size=3.1, vjust=-1.5) +
  scale_y_discrete(name ="Answer", limits=c("Haven't thought much about this", "Conservative", 
                                            "Slightly conservative","Moderate", "Slightly liberal",
                                            "Liberal", "Extremely liberal")) +
  labs(x=NULL, y=NULL, title="Figure 6

Ideological Scale by Generation",
       subtitle="Percent of respondents who identify as...",
       caption="N = 51 Pairs of Students and Parents
       Source: 2020 Survey of Indian-American Attitudes") + theme_light()
fig2 + theme(plot.subtitle = element_text(face = "italic")) +
  theme(plot.background = element_blank(),panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) 


#############################Figure 3##############
#Positions on Contemporary Debates - 11 Carnegie Endowment 

#Reshaping Data
g <- SIAAData %>%
  select(immigration_opinion_students, immigration_opinion_parents, affirmative_action_students, affirmative_action_parents,
religious_tolerance_students, religious_tolerance_parents, police_brutality_students, police_brutality_parents, 
media_suppression_students, media_suppression_parents, immigrant_deportation_students, immigrant_deportation_parents, 
travel_ban_students, travel_ban_parents)

g2 <- subset(g, immigration_opinion_parents == "Strongly oppose" | immigration_opinion_parents == "Somewhat oppose",
             select = c("immigration_opinion_students")) 


(g2[g2 == "Somewhat oppose"] <- "Oppose Pathway to Citizenship for Undocumented Immigrants")
(g2[g2 == "Strongly oppose"] <- "Oppose Pathway to Citizenship for Undocumented Immigrants")

g2 <- g2 %>% group_by(immigration_opinion_parents) %>% tally() %>% 
  rename(Answer = immigration_opinion_parents) %>% rename(Parent = n)




g1 <- subset(g, immigration_opinion_students == "Strongly oppose" | immigration_opinion_students == "Somewhat oppose",
             select = c("immigration_opinion_students")) 
(g1[g1 == "Somewhat oppose"] <- "Oppose Pathway to Citizenship for Undocumented Immigrants")
(g1[g1 == "Strongly oppose"] <- "Oppose Pathway to Citizenship for Undocumented Immigrants")

g1 <- g1 %>% group_by(immigration_opinion_students) %>% tally() %>% 
  rename(Answer = immigration_opinion_students) %>% rename(Student = n)

ga <- left_join(g1,g2, by=c("Answer"))




g3 <- subset(g, affirmative_action_students == "Strongly oppose" | affirmative_action_students == "Somewhat oppose",
             select = c("affirmative_action_students")) 
(g3[g3 == "Somewhat oppose"] <- "Oppose Race-based Affirmative Action")
(g3[g3 == "Strongly oppose"] <- "Oppose Race-based Affirmative Action")

g3 <- g3 %>% group_by(affirmative_action_students) %>% tally() %>% 
  rename(Answer = affirmative_action_students) %>% rename(Student = n)

g4 <- subset(g, affirmative_action_parents == "Strongly oppose" | affirmative_action_parents == "Somewhat oppose",
             select = c("affirmative_action_parents")) 
(g4[g4 == "Somewhat oppose"] <- "Oppose Race-based Affirmative Action")
(g4[g4 == "Strongly oppose"] <- "Oppose Race-based Affirmative Action")

g4 <- g4 %>% group_by(affirmative_action_parents) %>% tally() %>% 
  rename(Answer = affirmative_action_parents) %>% rename(Parent = n)

gb <- left_join(g3,g4, by=c("Answer"))

z <- rbind(ga,gb)

g5 <- subset(g, religious_tolerance_students == "Strongly support" | religious_tolerance_students == "Somewhat support",
             select = c("religious_tolerance_students")) 
(g5[g5 == "Strongly support"] <- "Support Equal Treatment of Religious Minorities")
(g5[g5 == "Somewhat support"] <- "Support Equal Treatment of Religious Minorities")

g5 <- g5 %>% group_by(religious_tolerance_students) %>% tally() %>% 
  rename(Answer = religious_tolerance_students) %>% rename(Student = n)

g6 <- subset(g, religious_tolerance_parents == "Strongly support" | religious_tolerance_parents == "Somewhat support",
             select = c("religious_tolerance_parents")) 
(g6[g6 == "Strongly support"] <- "Support Equal Treatment of Religious Minorities")
(g6[g6 == "Somewhat support"] <- "Support Equal Treatment of Religious Minorities")

g6 <- g6 %>% group_by(religious_tolerance_parents) %>% tally() %>% 
  rename(Answer = religious_tolerance_parents) %>% rename(Parent = n)

gc <- left_join(g5,g6, by=c("Answer"))

z <- rbind(z, gc)



g7 <- subset(g, police_brutality_students == "Strongly oppose" | police_brutality_students == "Somewhat oppose",
             select = c("police_brutality_students")) 
(g7[g7 == "Somewhat oppose"] <- "Oppose police using physical force against peaceful BLM protesters")
(g7[g7 == "Strongly oppose"] <- "Oppose police using physical force against peaceful BLM protesters")

g7 <- g7 %>% group_by(police_brutality_students) %>% tally() %>% 
  rename(Answer = police_brutality_students) %>% rename(Student = n)

g8 <- subset(g, police_brutality_parents == "Strongly oppose" | police_brutality_parents == "Somewhat oppose",
             select = c("police_brutality_parents")) 
(g8[g8 == "Somewhat oppose"] <- "Oppose police using physical force against peaceful BLM protesters")
(g8[g8 == "Strongly oppose"] <- "Oppose police using physical force against peaceful BLM protesters")

g8 <- g8 %>% group_by(police_brutality_parents) %>% tally() %>% 
  rename(Answer = police_brutality_parents) %>% rename(Parent = n)

gd <- left_join(g7,g8, by=c("Answer"))
z <- rbind(z, gd)


g9 <- subset(g, media_suppression_students == "Strongly oppose" | media_suppression_students == "Somewhat oppose",
             select = c("media_suppression_students")) 
(g9[g9 == "Somewhat oppose"] <- "Oppose White House revoking press credentials of reporters")
(g9[g9 == "Strongly oppose"] <- "Oppose White House revoking press credentials of reporters")

g9 <- g9 %>% group_by(media_suppression_students) %>% tally() %>% 
  rename(Answer = media_suppression_students) %>% rename(Student = n)

g10 <- subset(g, media_suppression_parents == "Strongly oppose" | media_suppression_parents == "Somewhat oppose",
             select = c("media_suppression_parents")) 
(g10[g10 == "Somewhat oppose"] <- "Oppose White House revoking press credentials of reporters")
(g10[g10 == "Strongly oppose"] <- "Oppose White House revoking press credentials of reporters")

g10 <- g10 %>% group_by(media_suppression_parents) %>% tally() %>% 
  rename(Answer = media_suppression_parents) %>% rename(Parent = n)


ge <- left_join(g9,g10, by=c("Answer"))
z <- rbind(z, ge)


g11 <- subset(g, immigrant_deportation_students == "Strongly oppose" | immigrant_deportation_students == "Somewhat oppose",
             select = c("immigrant_deportation_students")) 
(g11[g11 == "Somewhat oppose"] <- "Oppose enhanced ICE efforts to deport illegal immigrants")
(g11[g11 == "Strongly oppose"] <- "Oppose enhanced ICE efforts to deport illegal immigrants")

g11 <- g11 %>% group_by(immigrant_deportation_students) %>% tally() %>% 
  rename(Answer = immigrant_deportation_students) %>% rename(Student = n)

g12 <- subset(g, immigrant_deportation_parents == "Strongly oppose" |immigrant_deportation_parents == "Somewhat oppose",
              select = c("immigrant_deportation_parents")) 
(g12[g12 == "Somewhat oppose"] <- "Oppose enhanced ICE efforts to deport illegal immigrants")
(g12[g12 == "Strongly oppose"] <- "Oppose enhanced ICE efforts to deport illegal immigrants")

g12 <- g12 %>% group_by(immigrant_deportation_parents) %>% tally() %>% 
  rename(Answer = immigrant_deportation_parents) %>% rename(Parent = n)

gf <- left_join(g11,g12, by=c("Answer"))
z <- rbind(z, gf)



g13 <- subset(g, travel_ban_students == "Strongly oppose" | travel_ban_students == "Somewhat oppose",
              select = c("travel_ban_students")) 
(g13[g13 == "Somewhat oppose"] <- "Oppose the travel ban for citizens from Muslim countries")
(g13[g13 == "Strongly oppose"] <- "Oppose the travel ban for citizens from Muslim countries")

g13 <- g13 %>% group_by(travel_ban_students) %>% tally() %>% 
  rename(Answer = travel_ban_students) %>% rename(Student = n)

g14 <- subset(g, travel_ban_parents == "Strongly oppose" |travel_ban_parents == "Somewhat oppose",
              select = c("travel_ban_parents")) 
(g14[g14 == "Somewhat oppose"] <- "Oppose the travel ban for citizens from Muslim countries")
(g14[g14 == "Strongly oppose"] <- "Oppose the travel ban for citizens from Muslim countries")

g14 <- g14 %>% group_by(travel_ban_parents) %>% tally() %>% 
  rename(Answer = travel_ban_parents) %>% rename(Parent = n)

ge <- left_join(g13,g14, by=c("Answer"))
z <- rbind(z, ge)


z$difference <- abs(z$Student - z$Parent)
################Figure 3 in percent form #######


#Reshaping Data
g <- SIAAData %>%
  select(immigration_opinion_students, immigration_opinion_parents, affirmative_action_students, affirmative_action_parents,
         religious_tolerance_students, religious_tolerance_parents, police_brutality_students, police_brutality_parents, 
         media_suppression_students, media_suppression_parents, immigrant_deportation_students, immigrant_deportation_parents, 
         travel_ban_students, travel_ban_parents)


g2 <- g %>% select(immigration_opinion_parents)
(g2[g2 == "Somewhat oppose"] <- "Oppose Pathway to Citizenship for Undocumented Immigrants")
(g2[g2 == "Strongly oppose"] <- "Oppose Pathway to Citizenship for Undocumented Immigrants")

g2 <- g2 %>% group_by(immigration_opinion_parents) %>% tally() %>%
  rename(Answer = immigration_opinion_parents) %>% rename(Parent = n)
g2$parent_pct = ((g2$Parent / sum(g2$Parent))*100)
g2 <- g2 %>% slice(1)



g1 <- g %>% select(immigration_opinion_students)
(g1[g1 == "Somewhat oppose"] <- "Oppose Pathway to Citizenship for Undocumented Immigrants")
(g1[g1 == "Strongly oppose"] <- "Oppose Pathway to Citizenship for Undocumented Immigrants")

g1 <- g1 %>% group_by(immigration_opinion_students) %>% tally() %>%
  rename(Answer = immigration_opinion_students) %>% rename(Student = n)
g1$student_pct = ((g1$Student / sum(g1$Student))*100)
g1 <- g1 %>% slice(1)

ga <- left_join(g1,g2, by=c("Answer"))



g3 <- g %>% select(affirmative_action_students)
(g3[g3 == "Somewhat oppose"] <- "Oppose Race-based Affirmative Action")
(g3[g3 == "Strongly oppose"] <- "Oppose Race-based Affirmative Action")

g3 <- g3 %>% group_by(affirmative_action_students) %>% tally() %>%
  rename(Answer = affirmative_action_students) %>% rename(Student = n)
g3$student_pct = ((g3$Student / sum(g3$Student))*100)
g3 <- g3 %>% slice(1)


g4 <- g %>% select(affirmative_action_parents)
(g4[g4 == "Somewhat oppose"] <- "Oppose Race-based Affirmative Action")
(g4[g4 == "Strongly oppose"] <- "Oppose Race-based Affirmative Action")

g4 <- g4 %>% group_by(affirmative_action_parents) %>% tally() %>% 
  rename(Answer = affirmative_action_parents) %>% rename(Parent = n)
g4$parent_pct = ((g4$Parent / sum(g4$Parent))*100)
g4 <- g4 %>% slice(1)

gb <- left_join(g3,g4, by=c("Answer"))

z <- rbind(ga,gb)


g5 <- g %>% select(religious_tolerance_students)
(g5[g5 == "Strongly support"] <- "Support Equal Treatment of Religious Minorities")
(g5[g5 == "Somewhat support"] <- "Support Equal Treatment of Religious Minorities")

g5 <- g5 %>% group_by(religious_tolerance_students) %>% tally() %>% 
  rename(Answer = religious_tolerance_students) %>% rename(Student = n)
g5$student_pct = ((g5$Student / sum(g5$Student))*100)
g5 <- g5 %>% slice(2)


g6 <- g %>% select(religious_tolerance_parents)
(g6[g6 == "Strongly support"] <- "Support Equal Treatment of Religious Minorities")
(g6[g6 == "Somewhat support"] <- "Support Equal Treatment of Religious Minorities")
g6 <- g6 %>% group_by(religious_tolerance_parents) %>% tally() %>% 
  rename(Answer = religious_tolerance_parents) %>% rename(Parent = n)
g6$parent_pct = ((g6$Parent / sum(g6$Parent))*100)
g6 <- g6 %>% slice(2)

gc <- left_join(g5,g6, by=c("Answer"))
z <- rbind(z, gc)



g7 <- g %>% select(police_brutality_students)
(g7[g7 == "Somewhat oppose"] <- "Oppose police using physical force against peaceful BLM protesters")
(g7[g7 == "Strongly oppose"] <- "Oppose police using physical force against peaceful BLM protesters")
g7 <- g7 %>% group_by(police_brutality_students) %>% tally() %>% 
  rename(Answer = police_brutality_students) %>% rename(Student = n)
g7$student_pct = ((g7$Student / sum(g7$Student))*100)
g7 <- g7 %>% slice(1)


g8 <- g %>% select(police_brutality_parents)
(g8[g8 == "Somewhat oppose"] <- "Oppose police using physical force against peaceful BLM protesters")
(g8[g8 == "Strongly oppose"] <- "Oppose police using physical force against peaceful BLM protesters")
g8 <- g8 %>% group_by(police_brutality_parents) %>% tally() %>% 
  rename(Answer = police_brutality_parents) %>% rename(Parent = n)
g8$parent_pct = ((g8$Parent / sum(g8$Parent))*100)
g8 <- g8 %>% slice(1)
gd <- left_join(g7,g8, by=c("Answer"))
z <- rbind(z, gd)



g9 <- g %>% select(media_suppression_students)
(g9[g9 == "Somewhat oppose"] <- "Oppose White House revoking press credentials of reporters")
(g9[g9 == "Strongly oppose"] <- "Oppose White House revoking press credentials of reporters")
g9 <- g9 %>% group_by(media_suppression_students) %>% tally() %>% 
  rename(Answer = media_suppression_students) %>% rename(Student = n)
g9$student_pct = ((g9$Student / sum(g9$Student))*100)
g9 <- g9 %>% slice(1)


g10 <- g %>% select(media_suppression_parents)
(g10[g10 == "Somewhat oppose"] <- "Oppose White House revoking press credentials of reporters")
(g10[g10 == "Strongly oppose"] <- "Oppose White House revoking press credentials of reporters")
g10 <- g10 %>% group_by(media_suppression_parents) %>% tally() %>% 
  rename(Answer = media_suppression_parents) %>% rename(Parent = n)
g10$parent_pct = ((g10$Parent / sum(g10$Parent))*100)
g10 <- g10 %>% slice(1)


ge <- left_join(g9,g10, by=c("Answer"))
z <- rbind(z, ge)




g11 <- g %>% select(immigrant_deportation_students)
(g11[g11 == "Somewhat oppose"] <- "Oppose enhanced ICE efforts to deport illegal immigrants")
(g11[g11 == "Strongly oppose"] <- "Oppose enhanced ICE efforts to deport illegal immigrants")
g11 <- g11 %>% group_by(immigrant_deportation_students) %>% tally() %>% 
  rename(Answer = immigrant_deportation_students) %>% rename(Student = n)
g11$student_pct = ((g11$Student / sum(g11$Student))*100)
g11 <- g11 %>% slice(1)



g12 <- g %>% select(immigrant_deportation_parents)
(g12[g12 == "Somewhat oppose"] <- "Oppose enhanced ICE efforts to deport illegal immigrants")
(g12[g12 == "Strongly oppose"] <- "Oppose enhanced ICE efforts to deport illegal immigrants")
g12 <- g12 %>% group_by(immigrant_deportation_parents) %>% tally() %>% 
  rename(Answer = immigrant_deportation_parents) %>% rename(Parent = n)
g12$parent_pct = ((g12$Parent / sum(g12$Parent))*100)
g12 <- g12 %>% slice(1)

gf <- left_join(g11,g12, by=c("Answer"))
z <- rbind(z, gf)


g13 <- g %>% select(travel_ban_students)
(g13[g13 == "Somewhat oppose"] <- "Oppose the travel ban for citizens from Muslim countries")
(g13[g13 == "Strongly oppose"] <- "Oppose the travel ban for citizens from Muslim countries")
g13 <- g13 %>% group_by(travel_ban_students) %>% tally() %>% 
  rename(Answer = travel_ban_students) %>% rename(Student = n)
g13$student_pct = ((g13$Student / sum(g13$Student))*100)
g13 <- g13 %>% slice(1)


g14 <- g %>% select(travel_ban_parents)
(g14[g14 == "Somewhat oppose"] <- "Oppose the travel ban for citizens from Muslim countries")
(g14[g14 == "Strongly oppose"] <- "Oppose the travel ban for citizens from Muslim countries")
g14 <- g14 %>% group_by(travel_ban_parents) %>% tally() %>% 
  rename(Answer = travel_ban_parents) %>% rename(Parent = n)
g14$parent_pct = ((g14$Parent / sum(g14$Parent))*100)
g14 <- g14 %>% slice(1)


ge <- left_join(g13,g14, by=c("Answer"))
z <- rbind(z, ge)


z$difference <- abs(z$Student - z$Parent)
z$student_pct <- round(z$student_pct)
z$parent_pct <- round(z$parent_pct)
z$difference_pct <- abs(z$student_pct - z$parent_pct)



ediff <- read.csv("combined_SIAA.csv")

ediff1 <- ediff %>%
  select(immigration_opinion_encoded_diff,
         affirmative_action_encoded_diff,
         religious_tolerance_encoded_diff,
         police_brutality_encoded_diff,
         media_suppression_encoded_diff,
         immigrant_deportation_encoded_diff,
         travel_ban_encoded_diff)

ediff2 <- colMeans(x=ediff1, na.rm = TRUE)
ediff2 <- data.frame(ediff2)
ediff2 <- round(ediff2$ediff2, 1)

z1 <- cbind(z, ediff2)



#######Plotting Figure 3######
green <- "DarkGreen"
#parent
blue <- "deepskyblue4"

fig3 <-
  ggplot() +
  geom_segment(data=z, aes(y=Answer, yend=Answer, x=0, xend=115), 
               color="#b2b2b2", size=0.15) +
  geom_dumbbell(data=z, aes(y=Answer, x=student_pct, xend=parent_pct),
                size=1, color="#b2b2b2", size_x=5, size_xend = 5, colour_x = green, 
                colour_xend = blue) +
  geom_text(data=filter(z, Answer=="Oppose enhanced ICE efforts to deport illegal immigrants"),
            aes(x=student_pct, y=Answer, label="Students"),
            color=green, size=3, vjust=-1.5, fontface="bold") +
  geom_text(data=filter(z, Answer=="Oppose enhanced ICE efforts to deport illegal immigrants"),
            aes(x=parent_pct, y=Answer, label="Parents"),
            color=blue, size=3, vjust=-1.5, fontface="bold") +
  geom_text(data=z, aes(x=student_pct, y=Answer, label=round(student_pct, digits=0)),
            color=green, size=2.75, vjust=2.5) +
  geom_text(data=z, color=blue, size=2.75, vjust=2.5,
            aes(x=parent_pct, y=Answer, label=round(parent_pct,digits=0))) +
  geom_rect(data=z, aes(xmin=101, xmax=115, ymin=-Inf, ymax=Inf), fill="gray") +
  geom_text(data=z, aes(label=difference_pct, y=Answer, x=108), size=3) +
  #geom_text(data=z1, aes(label=ediff2, y=Answer, x = 135), size=3) +
  #geom_text(data=filter(z1,  Answer=="Oppose enhanced ICE efforts to deport illegal immigrants"), 
          #  aes(x=135, y=Answer, label="Average Difference"), 
           # color="black", size=3.1, vjust=-1.5) +
  geom_text(data=filter(z,  Answer=="Oppose enhanced ICE efforts to deport illegal immigrants"), 
            aes(x=108, y=Answer, label="Difference"),
            color="black", size=3.1, vjust=-1.5) +
  scale_y_discrete(name ="Answer", limits=c("Oppose Pathway to Citizenship for Undocumented Immigrants",
    "Oppose Race-based Affirmative Action","Oppose police using physical force against peaceful BLM protesters",
   "Support Equal Treatment of Religious Minorities",                                         
  "Oppose White House revoking press credentials of reporters",
  "Oppose the travel ban for citizens from Muslim countries", 
  "Oppose enhanced ICE efforts to deport illegal immigrants")) +
  labs(x=NULL, y=NULL, title="Figure 10

Positions on Contemporary Debates, by Generation",
       subtitle="Percent of respondents who...",
       caption="N = 51 Pairs of Students and Parents
       Source: 2020 Survey of Indian-American Attitudes") + theme_light()
fig3 + theme(plot.subtitle = element_text(face = "italic")) +
  theme(plot.background = element_blank(),panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) 


#####################Figure 4#######################
#2020 Vote Choice by Respondent Type

h <- SIAAData %>%
  select(president_choice_students) %>% group_by(president_choice_students) %>% tally() %>% 
  rename (Answer = president_choice_students) %>% rename(Student = n)
#create new column to indicate respondent type = student, and populate with student in cells
h$RespType1 <- 'student'
h$student_pct = ((h$Student / sum(h$Student))*100)




#select parent column, group answers, count how many of each answer, rename column to be called answer
i <- SIAAData %>%
  select(president_choice_parents) %>% group_by(president_choice_parents) %>% tally() %>% 
  rename (Answer = president_choice_parents) %>% rename(Parent = n)
#create new column to indicate respondent type = parent, and populate with parent in cells
i$RespType2 <- 'parent'
i$parent_pct = ((i$Parent / sum(i$Parent))*100)

#join to create new df, put e first so we retain the broader parent answer
j <- left_join(h,i, by=c("Answer"))
j$difference <- abs(j$Student - j$Parent)
j$student_pct <- round(j$student_pct)
j$parent_pct <- round(j$parent_pct)
j$difference_pct <- abs(j$student_pct - j$parent_pct)


h1 <- h %>% slice(3)
h1$student_pct <- round(h1$student_pct)
j <- j %>% slice(1:3)
(j[j == "Other [specify]"] <- "Other")
(h1[h1 == "Other [specify]"] <- "Other")
(j[j == "I do not intend to vote in the election for President this year"] <- "Not voting for President")

fig4 <-
  ggplot() +
  geom_segment(data=j, aes(y=Answer, yend=Answer, x=0, xend=110), 
               color="#b2b2b2", size=0.15) +
  geom_dumbbell(data=j, aes(y=Answer, x=student_pct, xend=parent_pct),
                size=1, color="#b2b2b2", size_x=5, size_xend = 5, colour_x = green, 
                colour_xend = blue) +
  geom_point(data=h1, aes(y=Answer, x=student_pct), size=5, color=blue) +
  geom_text(data=filter(j, Answer=="Joe Biden"),
            aes(x=student_pct, y=Answer, label="Students"),
            color=green, size=3, vjust=-1.5, fontface="bold") +
  geom_text(data=filter(j, Answer=="Joe Biden"),
            aes(x=parent_pct, y=Answer, label="Parents"),
            color=blue, size=3, vjust=-1.5, fontface="bold") +
  geom_text(data=j, aes(x=student_pct, y=Answer, label=round(student_pct,digits=0)),
            color=green, size=2.75, vjust=2.5) +
  geom_text(data=j, color=blue, size=2.75, vjust=2.5,
            aes(x=parent_pct, y=Answer, label=round(parent_pct,digits=0))) +
  geom_rect(data=j, aes(xmin=100, xmax=110, ymin=-Inf, ymax=Inf), fill="gray") +
  geom_text(data=j, aes(label=difference_pct, y=Answer, x=105), size=3) +
  geom_text(data=h1, aes(label=student_pct, y=Answer, x=105), size=3) +
  geom_text(data=filter(j,  Answer=="Joe Biden"), 
            aes(x=105, y=Answer, label="Difference"),
            color="black", size=3.1, vjust=-1.5) +
  scale_y_discrete(name ="Answer", limits=c("Other", "Not voting for President", "Joe Biden" )) +
  labs(x=NULL, y=NULL, title="Figure 4: 2020 Presidential Vote Choice",
       subtitle="Percent of respondents who are voting for...",
       caption="N = 51 Pairs of Students and Parents
       
       Source: 2020 Survey of Indian-American Attitudes") + theme_light()
fig4 + theme(plot.subtitle = element_text(face = "italic")) +
  theme(plot.background = element_blank(),panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) 


##################Figure 5#################
#Trump U.S. India Relations
k <- SIAAData %>%
  select(trump_india_approval_students) %>% group_by(trump_india_approval_students) %>% tally() %>% 
  rename (Answer = trump_india_approval_students) %>% rename(Student = n)
#create new column to indicate respondent type = student, and populate with student in cells
k$RespType1 <- 'student'


#select parent column, group answers, count how many of each answer, rename column to be called answer
l <- SIAAData %>%
  select(trump_india_approval_parents) %>% group_by(trump_india_approval_parents) %>% tally() %>% 
  rename (Answer = trump_india_approval_parents) %>% rename(Parent = n)
#create new column to indicate respondent type = parent, and populate with parent in cells
l$RespType2 <- 'parent'

#join to create new df, put e first so we retain the broader parent answer
m <- left_join(k,l, by=c("Answer"))
m$difference <- abs(m$Student - m$Parent)


fig5 <- ggplot() +
  geom_segment(data=m, aes(y=Answer, yend=Answer, x=0, xend=60), 
               color="#b2b2b2", size=0.15) +
  geom_dumbbell(data=m, aes(y=Answer, x=Student, xend=Parent),
                size=1, color="#b2b2b2", size_x=5, size_xend = 5, colour_x = green, 
                colour_xend = blue) +
  geom_text(data=filter(m, Answer=="Disapprove"),
            aes(x=Student, y=Answer, label="Students"),
            color=green, size=3, vjust=-1.5, fontface="bold") +
  geom_text(data=filter(m, Answer=="Disapprove"),
            aes(x=Parent, y=Answer, label="Parents"),
            color=blue, size=3, vjust=-1.5, fontface="bold") +
  geom_text(data=m, aes(x=Student, y=Answer, label=round(Student,digits=0)),
            color=green, size=2.75, vjust=2.5) +
  geom_text(data=m, color=blue, size=2.75, vjust=2.5,
            aes(x=Parent, y=Answer, label=round(Parent,digits=0))) +
  geom_rect(data=m, aes(xmin=50, xmax=60, ymin=-Inf, ymax=Inf), fill="gray") +
  geom_text(data=m, aes(label=difference, y=Answer, x=55), size=3) +
  geom_text(data=filter(m,  Answer=="Disapprove"), 
            aes(x=55, y=Answer, label="Difference"),
            color="black", size=3.1, vjust=-1.5) +
  labs(x=NULL, y=NULL, title="Figure 5: Trump Approval on U.S.-India Relations",
       subtitle="Do you approve or disapprove of the way Trump is handling relations with India...",
       caption="N = 51 Pairs of Students and Parents
       
       Source: 2020 Survey of Indian-American Attitudes") + theme_light()
fig5 + theme(plot.subtitle = element_text(face = "italic")) +
  theme(plot.background = element_blank(),panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) 


##################Figure 6######################
#Modi Approval Student

n <- SIAAData %>%
  select(modi_approval_students) %>% group_by(modi_approval_students) %>% tally() %>% 
  rename (Answer = modi_approval_students) %>% rename(Student = n)
#create new column to indicate respondent type = student, and populate with student in cells
n$RespType1 <- 'student'


#select parent column, group answers, count how many of each answer, rename column to be called answer
o <- SIAAData %>%
  select(modi_approval_parents) %>% group_by(modi_approval_parents) %>% tally() %>% 
  rename (Answer = modi_approval_parents) %>% rename(Parent = n)
#create new column to indicate respondent type = parent, and populate with parent in cells
o$RespType2 <- 'parent'

#join to create new df, put e first so we retain the broader parent answer
p <- left_join(n,o, by=c("Answer"))
p$difference <- abs(p$Student - p$Parent)
p <- p %>% slice(1:2)

fig6 <- ggplot() +
  geom_segment(data=p, aes(y=Answer, yend=Answer, x=0, xend=50), 
               color="#b2b2b2", size=0.15) +
  geom_dumbbell(data=p, aes(y=Answer, x=Student, xend=Parent),
                size=1, color="#b2b2b2", size_x=5, size_xend = 5, colour_x = green, 
                colour_xend = blue) +
  geom_text(data=filter(p, Answer=="Disapprove"),
            aes(x=Student, y=Answer, label="Students"),
            color=green, size=3, vjust=-1.5, fontface="bold") +
  geom_text(data=filter(p, Answer=="Disapprove"),
            aes(x=Parent, y=Answer, label="Parents"),
            color=blue, size=3, vjust=-1.5, fontface="bold") +
  geom_text(data=p, aes(x=Student, y=Answer, label=round(Student,digits=0)),
            color=green, size=2.75, vjust=2.5) +
  geom_text(data=p, color=blue, size=2.75, vjust=2.5,
            aes(x=Parent, y=Answer, label=round(Parent,digits=0))) +
  geom_rect(data=p, aes(xmin=43, xmax=50, ymin=-Inf, ymax=Inf), fill="gray") +
  geom_text(data=p, aes(label=difference, y=Answer, x=46), size=3) +
  geom_text(data=filter(p,  Answer=="Disapprove"), 
            aes(x=46, y=Answer, label="Difference"),
            color="black", size=3.1, vjust=-1.5) +
  scale_y_discrete(name ="Answer", limits=c("Approve", "Disapprove" )) +
  labs(x=NULL, y=NULL, title="Figure 6: Modi Approval as Prime Minister",
       subtitle="Do you approve or disapprove of the way Narendra Modi is handling his job as Prime Minister?",
       caption="N = 51 Pairs of Students and Parents
       
       Source: 2020 Survey of Indian-American Attitudes") + theme_light()
fig6 + theme(plot.subtitle = element_text(face = "italic")) +
  theme(plot.background = element_blank(),panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) 

##############################################################
##Figure 7 India Contemporary Debates#####

indiasubset <- SIAAData %>%
  select(media_suppression_india_students, media_suppression_india_parents,immigration_deportation_india_students,
         immigration_deportation_india_parents, citizenship_india_students, citizenship_india_parents,
         affirmative_action_india_students, affirmative_action_india_parents, police_brutality_india_students, 
         police_brutality_india_parents, hindutwa_threat_students, hindutwa_threat_parents)


mss <- indiasubset %>%
  select(media_suppression_india_students) 
(mss[mss == "Somewhat oppose"] <- "Oppose Media Suppression by the Indian Government")
(mss[mss == "Strongly oppose"] <- "Oppose Media Suppression by the Indian Government")

mss <- mss %>% group_by(media_suppression_india_students) %>% tally() %>% 
  rename (Answer = media_suppression_india_students) %>% rename(Student = n)

mss$student_pct = ((mss$Student / sum(mss$Student))*100)
mss <- mss %>% slice(1)


msp <- indiasubset %>%
  select(media_suppression_india_parents) 
(msp[msp == "Somewhat oppose"] <- "Oppose Media Suppression by the Indian Government")
(msp[msp == "Strongly oppose"] <- "Oppose Media Suppression by the Indian Government")

msp <- msp %>% group_by(media_suppression_india_parents) %>% tally() %>% 
  rename (Answer = media_suppression_india_parents) %>% rename(Parent = n)

msp$parent_pct = ((msp$Parent / sum(msp$Parent))*100)
msp <- msp %>% slice(1)

msi <- left_join(mss, msp, by=c("Answer"))


#####
ids <- indiasubset %>%
  select(immigration_deportation_india_students) 
(ids[ids == "Somewhat oppose"] <- "Oppose an all-India National Register of Citizens (NRC)")
(ids[ids == "Strongly oppose"] <- "Oppose an all-India National Register of Citizens (NRC)")

ids <- ids %>% group_by(immigration_deportation_india_students) %>% tally() %>% 
  rename (Answer = immigration_deportation_india_students) %>% rename(Student = n)

ids$student_pct = ((ids$Student / sum(ids$Student))*100)
ids <- ids %>% slice(1)


idp <- indiasubset %>%
  select(immigration_deportation_india_parents) 
(idp[idp == "Somewhat oppose"] <- "Oppose an all-India National Register of Citizens (NRC)")
(idp[idp == "Strongly oppose"] <- "Oppose an all-India National Register of Citizens (NRC)")

idp <- idp %>% group_by(immigration_deportation_india_parents) %>% tally() %>% 
  rename (Answer = immigration_deportation_india_parents) %>% rename(Parent = n)

idp$parent_pct = ((idp$Parent / sum(idp$Parent))*100)
idp <- idp %>% slice(1)

idi <- left_join(ids, idp, by=c("Answer"))

#####
cis <- indiasubset %>%
  select(citizenship_india_students) 
(cis[cis == "Somewhat oppose"] <- "Oppose the Citizenship Amendment Act")
(cis[cis == "Strongly oppose"] <- "Oppose the Citizenship Amendment Act")

cis <- cis %>% group_by(citizenship_india_students) %>% tally() %>% 
  rename (Answer = citizenship_india_students) %>% rename(Student = n)

cis$student_pct = ((cis$Student / sum(cis$Student))*100)
cis <- cis %>% slice(1)


cip <- indiasubset %>%
  select(citizenship_india_parents) 
(cip[cip == "Somewhat oppose"] <- "Oppose the Citizenship Amendment Act")
(cip[cip == "Strongly oppose"] <- "Oppose the Citizenship Amendment Act")

cip <- cip %>% group_by(citizenship_india_parents) %>% tally() %>% 
  rename (Answer = citizenship_india_parents) %>% rename(Parent = n)

cip$parent_pct = ((cip$Parent / sum(cip$Parent))*100)
cip <- cip %>% slice(1)

cii <- left_join(cis, cip, by=c("Answer"))
######

aas <- indiasubset %>%
  select(affirmative_action_india_students) 
(aas[aas == "Somewhat support"] <- "Support Caste-Based Affirmative Action")
(aas[aas == "Strongly support"] <- "Support Caste-Based Affirmative Action")

aas <- aas %>% group_by(affirmative_action_india_students) %>% tally() %>% 
  rename (Answer = affirmative_action_india_students) %>% rename(Student = n)

aas$student_pct = ((aas$Student / sum(aas$Student))*100)
aas <- aas %>% slice(3)


aap <- indiasubset %>%
  select(affirmative_action_india_parents) 
(aap[aap == "Somewhat support"] <- "Support Caste-Based Affirmative Action")
(aap[aap == "Strongly support"] <- "Support Caste-Based Affirmative Action")

aap <- aap %>% group_by(affirmative_action_india_parents) %>% tally() %>% 
  rename (Answer = affirmative_action_india_parents) %>% rename(Parent = n)

aap$parent_pct = ((aap$Parent / sum(aap$Parent))*100)
aap <- aap %>% slice(3)

aai<- left_join(aas, aap, by=c("Answer"))

#####
pbs <- indiasubset %>%
  select(police_brutality_india_students) 
(pbs[pbs == "Somewhat oppose"] <- "Oppose Police Brutality against Peaceful Protesters")
(pbs[pbs == "Strongly oppose"] <- "Oppose Police Brutality against Peaceful Protesters")

pbs <- pbs %>% group_by(police_brutality_india_students) %>% tally() %>% 
  rename (Answer = police_brutality_india_students) %>% rename(Student = n)

pbs$student_pct = ((pbs$Student / sum(pbs$Student))*100)
pbs <- pbs %>% slice(1)


pbp <- indiasubset %>%
  select(police_brutality_india_parents) 
(pbp[pbp == "Somewhat oppose"] <- "Oppose Police Brutality against Peaceful Protesters")
(pbp[pbp == "Strongly oppose"] <- "Oppose Police Brutality against Peaceful Protesters")

pbp <- pbp %>% group_by(police_brutality_india_parents) %>% tally() %>% 
  rename (Answer = police_brutality_india_parents) %>% rename(Parent = n)

pbp$parent_pct = ((pbp$Parent / sum(pbp$Parent))*100)
pbp <- pbp %>% slice(1)

pbi<- left_join(pbs, pbp, by=c("Answer"))

icd <- rbind(msi,idi)
icd <- rbind(icd, cii)
icd <- rbind(icd, aai)
icd <- rbind(icd, pbi)
icd

icd$student_pct <- round(icd$student_pct)
icd$parent_pct <- round(icd$parent_pct)

icd$difference <- abs(icd$Student - icd$Parent)
icd$difference_pct <- abs(icd$student_pct - icd$parent_pct)



fig7 <-
  ggplot() +
  geom_segment(data=icd, aes(y=Answer, yend=Answer, x=0, xend=110), 
               color="#b2b2b2", size=0.15) +
  geom_dumbbell(data=icd, aes(y=Answer, x=student_pct, xend=parent_pct),
                size=1, color="#b2b2b2", size_x=5, size_xend = 5, colour_x = green, 
                colour_xend = blue) +
  geom_text(data=filter(icd, Answer=="Support Caste-Based Affirmative Action"),
            aes(x=student_pct, y=Answer, label="Students"),
            color=green, size=3, vjust=-1.5, fontface="bold") +
  geom_text(data=filter(icd, Answer=="Support Caste-Based Affirmative Action"),
            aes(x=parent_pct, y=Answer, label="Parents"),
            color=blue, size=3, vjust=-1.5, fontface="bold") +
  geom_text(data=icd, aes(x=student_pct, y=Answer, label=round(student_pct,digits=0)),
            color=green, size=2.75, vjust=2.5) +
  geom_text(data=icd, color=blue, size=2.75, vjust=2.5,
            aes(x=parent_pct, y=Answer, label=round(parent_pct,digits=0))) +
  geom_rect(data=icd, aes(xmin=100, xmax=110, ymin=-Inf, ymax=Inf), fill="gray") +
  geom_text(data=icd, aes(label=difference_pct, y=Answer, x=105), size=3) +
  geom_text(data=filter(icd,  Answer=="Support Caste-Based Affirmative Action"), 
            aes(x=105, y=Answer, label="Difference"),
            color="black", size=3.1, vjust=-1.5) +
  labs(x=NULL, y=NULL, title="Figure 7: Positions on Contemporary Debates in India, by Respondent Type",
       subtitle="Percent of respondents who...",
       caption="N = 51 Pairs of Students and Parents
       
       Source: 2020 Survey of Indian-American Attitudes") + theme_light()
fig7 + theme(plot.subtitle = element_text(face = "italic")) +
  theme(plot.background = element_blank(),panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) 



###############Figure 8#########################
feeling <- SIAAData %>% 
  select(republican_feeling_students ,democrat_feeling_students, trump_feeling_students,
 harris_feeling_students, biden_feeling_students, modi_feeling_students, 
 rahul_gandhi_feeling_students, rss_feeling_students, bjp_feeling_students, congress_party_feeling_students, republican_feeling_parents,
 democrat_feeling_parents, trump_feeling_parents, harris_feeling_parents, biden_feeling_parents,
 modi_feeling_parents, rahul_gandhi_feeling_parents, rss_feeling_parents, bjp_feeling_parents, congress_party_feeling_parents)

#feeling1 <- data.frame(lapply(mean(feeling[1:16], as.numeric)))


feeling1 <- data.frame(lapply(feeling[1:20], as.numeric))
for (i in colnames(feeling1)){feeling1[i] = mean(feeling1[[i]], na.rm = TRUE)}
#for each index (column or row, column in this case) in our dataframe, 
#replace the index of that column with the mean of the column 
feeling1 <- feeling1 %>% slice(1:20)

feeling1$Question <- factor(colnames(feeling1))
fs <- feeling1 %>%
  select(1:10, 21) %>% slice(1:10) 
fs <- fs[, c(11, 1:10)]
vec <- c(13.92, 59.76, 4.76, 61.36, 58.2, 23.53061, 37.625, 15.70588, 21.25532, 46.19048)  
vec
fs$Student <- vec 
fs <- fs %>% 
  select(Question, Student)


fp <- feeling1 %>% select(11:21) %>% slice(11:20)
fp <- fp[, c(11, 1:10)]  
vec <- c(25.97826, 69.30435, 15.68889, 66.17778, 68.08889, 54.4186, 23.39474, 34, 52.57143, 33.4878)  
vec
fp$Parent <- vec 
fp <- fp %>% 
  select(Question, Parent)

feeling2 <- cbind(fs,fp)
feeling2 <- feeling2 %>% select (1:2, 4)
vec1 <- c("Republican", "Democrat", "Trump",
         "Harris", "Biden", "Modi", 
         "Rahul Gandhi", "RSS", "BJP", "Congress Party")
vec1
feeling2$Questions <- vec1
feeling2 <- feeling2 %>% select(2:4) 
feeling2 <- feeling2[, c(3, 1:2)]
feeling2$Student <- round(feeling2$Student)
feeling2$Parent <- round(feeling2$Parent)
feeling2$difference <- abs(feeling2$Student - feeling2$Parent)

feeling3 <- feeling2 %>% slice (6:10)
feeling2 <- feeling2 %>% slice(1:5)


fig8 <-
  ggplot() +
  geom_segment(data=feeling2, aes(y=Questions, yend=Questions, x=0, xend=85), 
               color="#b2b2b2", size=0.15) +
  geom_dumbbell(data=feeling2, aes(y=Questions, x=Student, xend=Parent),
                size=1, color="#b2b2b2", size_x=5, size_xend = 5, colour_x = green, 
                colour_xend = blue) +
  geom_text(data=filter(feeling2, Questions=="Democrat"),
            aes(x=Student, y=Questions, label="Students"),
            color=green, size=3, vjust=-1.5, fontface="bold") +
  geom_text(data=filter(feeling2, Questions=="Democrat"),
            aes(x=Parent, y=Questions, label="Parents"),
            color=blue, size=3, vjust=-1.5, fontface="bold") +
  geom_text(data=feeling2, aes(x=Student, y=Questions, label=round(Student,digits=0)),
            color=green, size=2.75, vjust=2.5) +
  geom_text(data=feeling2, color=blue, size=2.75, vjust=2.5,
            aes(x=Parent, y=Questions, label=round(Parent,digits=0))) +
  geom_rect(data=feeling2, aes(xmin=75, xmax=85, ymin=-Inf, ymax=Inf), fill="gray") +
  geom_text(data=feeling2, aes(label=difference, y=Questions, x=80), size=3) +
  geom_text(data=filter(feeling2, Questions=="Democrat"), 
            aes(x=80, y=Questions, label="Difference"),
            color="black", size=3.1, vjust=-1.5) +
  scale_y_discrete(name ="Answer", limits=c("Trump", "Harris", "Biden", "Republican", "Democrat")) +
  labs(x=NULL, y=NULL, title="Figure 8

Polarization among Indian-Americans, by Generation",
       subtitle="Average feeling thermometer ratings...",
       caption="N = 51 Pairs of Students and Parents
       Source: 2020 Survey of Indian-American Attitudes") + theme_light()
fig8 + theme(plot.subtitle = element_text(face = "italic")) +
  theme(plot.background = element_blank(),panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) 


fig8b <-
  ggplot() +
  geom_segment(data=feeling3, aes(y=Questions, yend=Questions, x=0, xend=75), 
               color="#b2b2b2", size=0.15) +
  geom_dumbbell(data=feeling3, aes(y=Questions, x=Student, xend=Parent),
                size=1, color="#b2b2b2", size_x=5, size_xend = 5, colour_x = green, 
                colour_xend = blue) +
  geom_text(data=filter(feeling3, Questions=="Modi"),
            aes(x=Student, y=Questions, label="Students"),
            color=green, size=3, vjust=-1.5, fontface="bold") +
  geom_text(data=filter(feeling3, Questions=="Modi"),
            aes(x=Parent, y=Questions, label="Parents"),
            color=blue, size=3, vjust=-1.5, fontface="bold") +
  geom_text(data=feeling3, aes(x=Student, y=Questions, label=round(Student,digits=0)),
            color=green, size=2.75, vjust=2.5) +
  geom_text(data=feeling3, color=blue, size=2.75, vjust=2.5,
            aes(x=Parent, y=Questions, label=round(Parent,digits=0))) +
  geom_rect(data=feeling3, aes(xmin=65, xmax=75, ymin=-Inf, ymax=Inf), fill="gray") +
  geom_text(data=feeling3, aes(label=difference, y=Questions, x=70), size=3) +
  geom_text(data=filter(feeling3, Questions=="Modi"), 
            aes(x=70, y=Questions, label="Difference"),
            color="black", size=3.1, vjust=-1.5) +
  scale_y_discrete(name ="Answer", limits=c("Congress Party","RSS", "BJP", "Rahul Gandhi", "Modi")) +
  labs(x=NULL, y=NULL, title="Figure 9

Polarization among Indian-Americans, by Generation",
       subtitle="Average feeling thermometer ratings...",
       caption="N = 51 Pairs of Students and Parents
       Source: 2020 Survey of Indian-American Attitudes") + theme_light()
fig8b + theme(plot.subtitle = element_text(face = "italic")) +
  theme(plot.background = element_blank(),panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) 





####################Figure 9 Religion######################

rels <- SIAAData %>%
  select(religion_students)
#group answers, count how many of each answer, rename the column to be called answer 
rels <- rels %>%
  group_by(religion_students) %>% tally() %>% rename (Answer = religion_students) %>%
  rename(Student = n)
#create new column to indicate respondent type = student, and populate with student in cells
rels$RespType1 <- 'student'

#select parent column, group answers, count how many of each answer, rename column to be called answer
relp <-  SIAAData %>% select(religion_parents)  %>% 
  group_by(religion_parents) %>% tally() %>% rename (Answer = religion_parents) %>% 
  rename(Parent = n)
#create new column to indicate respondent type = parent, and populate with parent in cells
relp$RespType2 <- 'parent'

#join to create new df 
religion <- left_join(rels,relp, by=c("Answer"))
religion$difference <- abs(religion$Student - religion$Parent)

#student
green <- "DarkGreen"
#parent
blue <- "deepskyblue4"

religion1 <- rels %>% slice(2)


fig9 <- 
  ggplot() + geom_segment(data=religion, aes(y=Answer, yend=Answer, x=0, xend=40), 
               color="#b2b2b2", size=0.15) +
  geom_dumbbell(data=religion, aes(y=Answer, x=Student, xend=Parent),
                size=1, color="#b2b2b2", size_x=5, size_xend = 5, colour_x = green, 
                colour_xend = blue) +
  geom_point(data=religion1, aes(y=Answer, x=Student), size=5, color=blue) +
  geom_text(data=filter(religion, Answer=="Agnostic"),
            aes(x=Student, y=Answer, label="Students"),
            color=green, size=3, vjust=-1.5, fontface="bold") +
  geom_text(data=filter(religion, Answer=="Agnostic"),
            aes(x=Parent, y=Answer, label="Parents"),
            color=blue, size=3, vjust=-1.5, fontface="bold") +
  geom_text(data=religion, aes(x=Student, y=Answer, label=round(Student,digits=0)),
            color=green, size=2.75, vjust=2.5) +
  geom_text(data=religion, color=blue, size=2.75, vjust=2.5,
            aes(x=Parent, y=Answer, label=round(Parent,digits=0))) +
  geom_rect(data=religion, aes(xmin=40, xmax=50, ymin=-Inf, ymax=Inf), fill="gray") +
  geom_text(data=religion, aes(label=difference, y=Answer, x=45), size=3) +
  geom_text(data=religion1, aes(label=Student, y=Answer, x=45), size=3) +
  geom_text(data=filter(religion,  Answer=="Agnostic"), 
            aes(x=45, y=Answer, label="Difference"),
            color="black", size=3.1, vjust=-1.5) +
  scale_y_discrete(name ="Answer", limits=c("Something else", "Sikh", "Roman Catholic",
    "Protestant", "Nothing in particular", "Muslim", "Jewish", "Hindu", "Eastern or Greek Orthodox",
    "Atheist", "Agnostic")) +
    labs(x=NULL, y=NULL, title="Figure 9: Religious Identity by Generation",
       subtitle="Number of respondents who...",
       caption="N = 51 Pairs of Students and Parents
       
       Source: 2020 Survey of Indian-American Attitudes") + theme_light()
fig9 + theme(plot.subtitle = element_text(face = "italic")) +
  theme(plot.background = element_blank(),panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) 



#################Figure 10###########################


news <- SIAAData %>% 
  select(nyt_trust_parents, fox_trust_parents, npr_trust_parents, wsj_trust_parents, 
         msnbc_trust_parents, cnn_trust_parents, bbc_trust_parents, govt_trust_parents, 
         social_media_trust_parents, whatsapp_trust_parents, nyt_trust_students,
         fox_trust_students,	npr_trust_students,	wsj_trust_students,	msnbc_trust_students,
         cnn_trust_students,	bbc_trust_students,	govt_trust_students,	social_media_trust_students,
         whatsapp_trust_students)
#feeling1 <- data.frame(lapply(mean(feeling[1:16], as.numeric)))


news_a <- data.frame(lapply(news[1:20], as.numeric))
for (i in colnames(news_a)){news_a[i] = mean(news_a[[i]], na.rm = TRUE)}
#for each index (column or row, column in this case) in our dataframe, 
#replace the index of that column with the mean of the column 
news_a <- news_a %>% slice(1:20)

news_a$Question <- factor(colnames(news_a))

news_p <- news_a %>%
  select(1:10, 21) %>% slice(1:10) 
news_p  <- news_p [, c(11, 1:10)]
vec <- c(7.90625, 2.315789, 8.62963, 7.095238, 7.052632, 7.108108, 8.428571, 
         3.386364, 3.090909, 2.777778)  
vec
news_p$Parent <- vec 
news_p <- news_p %>% 
  select(Question, Parent)


news_s <- news_a %>% select(11:21) %>% slice(11:20)
news_s <- news_s[, c(11, 1:10)]  
vec <- c(7.52, 2.026316, 8.292683, 6.375, 5.9, 6.47619, 7.137931, 3.02, 3.877551, 1.78)  
vec
news_s$Student <- vec 
news_s <- news_s %>% 
  select(Question, Student)


news_2 <- cbind(news_s,news_p)
news_2 <- news_2 %>% select (1:2, 4)
vec1 <- c("New York Times", "Fox News", "NPR",
          "Wall Street Journal", "MSNBC", "CNN", 
          "BBC", "Government Media", "Social Media", "Whatsapp")
vec1
news_2$Questions <- vec1
news_2 <- news_2 %>% select(2:4) 
news_2 <- news_2[, c(3, 1:2)]
news_2$difference <- abs(news_2$Student - news_2$Parent)
news_2$difference <- round(news_2$difference)

fig10 <-
  ggplot() +
  geom_segment(data=news_2, aes(y=Questions, yend=Questions, x=0, xend=12), 
               color="#b2b2b2", size=0.15) +
  geom_dumbbell(data=news_2, aes(y=Questions, x=Student, xend=Parent),
                size=1, color="#b2b2b2", size_x=5, size_xend = 5, colour_x = green, 
                colour_xend = blue) +
  geom_text(data=filter(news_2, Questions=="BBC"),
            aes(x=Student, y=Questions, label="Students"),
            color=green, size=3, vjust=-1.5, fontface="bold") +
  geom_text(data=filter(news_2, Questions=="BBC"),
            aes(x=Parent, y=Questions, label="Parents"),
            color=blue, size=3, vjust=-1.5, fontface="bold") +
  geom_text(data=news_2, aes(x=Student, y=Questions, label=round(Student,digits=0)),
            color=green, size=2.75, vjust=2.5) +
  geom_text(data=news_2, color=blue, size=2.75, vjust=2.5,
            aes(x=Parent, y=Questions, label=round(Parent,digits=0))) +
  geom_rect(data=news_2, aes(xmin=10, xmax=12, ymin=-Inf, ymax=Inf), fill="gray") +
  geom_text(data=news_2, aes(label=difference, y=Questions, x=11), size=3) +
  geom_text(data=filter(news_2, Questions=="BBC"), 
            aes(x=11, y=Questions, label="Difference"),
            color="black", size=3.1, vjust=-1.5) +
  scale_y_discrete(name ="Responses", limits=c("New York Times", "Fox News", "NPR",
    "Wall Street Journal", "MSNBC", "CNN", 
    "Government Media", "Social Media", "Whatsapp", "BBC")) +
  labs(x=NULL, y=NULL, title="Figure 10: Trust in U.S. Affairs News Sources, by Generation",
       subtitle="Average feeling thermometer ratings...",
       caption="N = 51 Pairs of Students and Parents
       
       Source: 2020 Survey of Indian-American Attitudes") + theme_light()
fig10 + theme(plot.subtitle = element_text(face = "italic")) +
  theme(plot.background = element_blank(),panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) 
##################Figure 11####################
indianews <- SIAAData %>% 
  select(nyt_trust_india_students,	bbc_trust_india_students, cnn_trust_india_students,
 economist_trust_students, toi_trust_students, ndtv_trust_students,
 republic_tv_trust_students,	aaj_tak_trust_students,	govt_trust_india_students,	
social_media_trust_india_students,	whatsapp_trust_india_students, 
nyt_trust_india_parents,	bbc_trust_india_parents,	cnn_trust_india_parents,
economist_trust_parents,	toi_trust_parents,	ndtv_trust_parents,	
republic_tv_trust_parents,	aaj_tak_trust_parents,	govt_trust_india_parents,	
social_media_trust_india_parents,	whatsapp_trust_india_parents)        
         
        
#feeling1 <- data.frame(lapply(mean(feeling[1:16], as.numeric)))
news_i <- data.frame(lapply(indianews[1:22], as.numeric))
for (i in colnames(news_i)){news_i[i] = mean(news_i[[i]], na.rm = TRUE)}
#for each index (column or row, column in this case) in our dataframe, 
#replace the index of that column with the mean of the column 
news_i <- news_i %>% slice(1:22)

news_i$Question <- factor(colnames(news_i))

news_is <- news_i %>%
  select(1:11, 23) %>% slice(1:11) 
news_is  <- news_is [, c(12, 1:11)]
vec <- c(6.897436, 6.878788, 5.878788, 6.827586, 5.358974, 5.318182, 2.545455, 
         4.2, 2.729167, 3.428571, 1.653061)  
vec
news_is$Student <- vec 
news_is <- news_is %>% 
  select(Question, Student)


news_ip <- news_i %>% select(12:23) %>% slice(12:22)
news_ip <- news_ip[, c(12, 1:11)]  
vec <- c(6.375, 7.076923, 6.416667, 7.35,
 6.84375, 6, 5.090909, 6.6, 4.302326, 3.325581, 2.933333)  
vec
news_ip$Parent <- vec 
news_ip <- news_ip %>% 
  select(Question, Parent)


news_i2 <- cbind(news_is,news_ip)
news_i2 <- news_i2 %>% select (1:2, 4)
vec2 <- c("New York Times", "BBC", "CNN", "The Economist", "TOI", "NDTV",
          "Republic TV", "Aaj Tak News", "Indian Government Media", 
          "Social Media", "Whatsapp")
vec2
news_i2$Questions <- vec2
news_i2 <- news_i2 %>% select(2:4) 
news_i2 <- news_i2[, c(3, 1:2)]
news_i2$difference <- abs(news_i2$Student - news_i2$Parent)
news_i2$difference <- round(news_i2$difference)

fig11 <-
  ggplot() +
  geom_segment(data=news_i2, aes(y=Questions, yend=Questions, x=0, xend=12), 
               color="#b2b2b2", size=0.15) +
  geom_dumbbell(data=news_i2, aes(y=Questions, x=Student, xend=Parent),
                size=1, color="#b2b2b2", size_x=5, size_xend = 5, colour_x = green, 
                colour_xend = blue) +
  geom_text(data=filter(news_i2, Questions=="Whatsapp"),
            aes(x=Student, y=Questions, label="Students"),
            color=green, size=3, vjust=-1.5, fontface="bold") +
  geom_text(data=filter(news_i2, Questions=="Whatsapp"),
            aes(x=Parent, y=Questions, label="Parents"),
            color=blue, size=3, vjust=-1.5, fontface="bold") +
  geom_text(data=news_i2, aes(x=Student, y=Questions, label=round(Student,digits=0)),
            color=green, size=2.75, vjust=2.5) +
  geom_text(data=news_i2, color=blue, size=2.75, vjust=2.5,
            aes(x=Parent, y=Questions, label=round(Parent,digits=0))) +
  geom_rect(data=news_i2, aes(xmin=10, xmax=12, ymin=-Inf, ymax=Inf), fill="gray") +
  geom_text(data=news_i2, aes(label=difference, y=Questions, x=11), size=3) +
  geom_text(data=filter(news_i2, Questions=="Whatsapp"), 
            aes(x=11, y=Questions, label="Difference"),
            color="black", size=3.1, vjust=-1.5) +
  scale_y_discrete(name ="Responses", limits=c("New York Times", "BBC", "CNN", "The Economist", 
       "TOI", "NDTV", "Republic TV", "Aaj Tak News", "Indian Government Media", 
          "Social Media", "Whatsapp")) +
  labs(x=NULL, y=NULL, title="Figure 11: Trust in Indian Affairs News Sources, by Generation",
       subtitle="Average feeling thermometer ratings...",
       caption="N = 51 Pairs of Students and Parents
       
       Source: 2020 Survey of Indian-American Attitudes") + theme_light()
fig11 + theme(plot.subtitle = element_text(face = "italic")) +
  theme(plot.background = element_blank(),panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) 
label(SIAA_Data$vote_choice_misc)

######################Gigure 12##########################
#select student column 
pvotes <- SIAAData %>%
  select(vote_choice_students) %>%
  group_by(vote_choice_students) %>% tally() %>% rename (Answer = vote_choice_students) %>%
  rename(Student = n)
#group answers, count how many of each answer, rename the column to be called answer 
#create new column to indicate respondent type = student, and populate with student in cells
pvotes$student_pct = ((pvotes$Student / sum(pvotes$Student))*100)

#select parent column, group answers, count how many of each answer, rename column to be called answer
pvotep <- SIAAData %>%
  select(vote_choice_parents) %>%
  group_by(vote_choice_parents) %>% tally() %>% rename (Answer = vote_choice_parents) %>%
  rename(Parent = n)
#group answers, count how many of each answer, rename the column to be called answer 
#create new column to indicate respondent type = student, and populate with student in cells
pvotep$parent_pct = ((pvotep$Parent / sum(pvotep$Parent))*100)


#join to create new df 
pvote <- left_join(pvotes, pvotep, by=c("Answer"))
pvote1 <- pvotep %>% slice(3)

pvote <- pvote %>% slice(1:4)
pvote [is.na(pvote )] <- 0

pvote$student_pct <- round(pvote$student_pct)
pvote$parent_pct <- round(pvote$parent_pct)

pvote$difference <- abs(pvote$Student - pvote$Parent)
pvote$difference_pct <- abs(pvote$student_pct - pvote$parent_pct)
#c$difference_pct <- round(c$difference_pct, 1)

#student
green <- "DarkGreen"
#parent
blue <- "darkblue"

(pvote[pvote == "Other [specify]"] <- "Other")




fig12 <- ggplot() +
  geom_segment(data=pvote, aes(y=Answer, yend=Answer, x=0, xend=90), 
               color="#b2b2b2", size=0.15) +
  geom_dumbbell(data=pvote, aes(y=Answer, x=student_pct, xend=parent_pct),
                size=1, color="#b2b2b2", size_x=5, size_xend = 5, colour_x = green, 
                colour_xend = blue) +
  geom_text(data=filter(pvote, Answer=="Joe Biden"),
            aes(x=student_pct, y=Answer, label="Students"),
            color=green, size=3, vjust=-1.5, fontface="bold") +
  geom_text(data=filter(pvote, Answer=="Joe Biden"),
            aes(x=parent_pct, y=Answer, label="Parents"),
            color=blue, size=3, vjust=-1.5, fontface="bold") +
  geom_text(data=pvote, aes(x=student_pct, y=Answer, label=round(student_pct,digits=0)),
            color=green, size=2.75, vjust=2.5) +
  geom_text(data=pvote, color=blue, size=2.75, vjust=2.5,
            aes(x=parent_pct, y=Answer, label=round(parent_pct,digits=0))) +
  geom_rect(data=pvote, aes(xmin=80, xmax=90, ymin=-Inf, ymax=Inf), fill="gray") +
  geom_text(data=pvote, aes(label=difference_pct, y=Answer, x=85), size=3) +
  geom_text(data=filter(pvote,  Answer=="Joe Biden"), 
            aes(x=85, y=Answer, label="Difference"),
            color="black", size=3.1, vjust=-1.5) +
  scale_y_discrete(name ="Answer", limits=c("Bernie Sanders", 
                                            "Elizabeth Warren", "Kamala Harris", "Joe Biden")) +
  labs(x=NULL, y=NULL, title="Figure 5
       
Primary Vote Choice, by Generation",
       subtitle="Percent of respondents who voted for...",
       caption="N = 51 Pairs of Students and Parents
       Source: 2020 Survey of Indian-American Attitudes") + theme_light()
fig12 + theme(plot.subtitle = element_text(face = "italic")) +
  theme(plot.background = element_blank(),panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) 
###############################################


#######Plotting Figure 13#######################

pol_ediff <- read.csv("combined_SIAA.csv")


pol_ediff  <- pol_ediff  %>%
  select(immigration_opinion_encoded_diff,
         affirmative_action_encoded_diff,
         religious_tolerance_encoded_diff,
         police_brutality_encoded_diff,
         media_suppression_encoded_diff,
         immigrant_deportation_encoded_diff,
         travel_ban_encoded_diff)


pol_ediff2 <- colMeans(x=pol_ediff, na.rm = TRUE)
pol_ediff2 <- data.frame(pol_ediff2)
pol_ediff2 <- round(pol_ediff2$pol_ediff2, 2)
pol_ediff2 <- data.frame(pol_ediff2)


pol_ediff2$Question  <- c('Citizenship for Undocumented Immigrants',
                         'Affirmative Action', 'Religious Tolerance', 'Police Brutality',
                         'Media Suppression', 'Immigrant Deportation', 'Travel Ban')


pol_ediff2 <- pol_ediff2 %>% rename(Average_Difference = pol_ediff2)



fig13 <-
  ggplot(data=pol_ediff2, aes(x=Question, y=Average_Difference, fill=Question)) +
  geom_bar(stat = 'identity', width = .45)  + 
  labs(title = "Average Difference for U.S. Policy Questions between Generations" ,
       x = "Policy Questions",
       y = "Differences", 
       colour = "") + 
  scale_fill_manual(values = c("Citizenship for Undocumented Immigrants" = "midnightblue", 
                               "Affirmative Action" = "brown3", 
                               "Religious Tolerance" = "goldenrod1", "Police Brutality" = "cadetblue3", "Media Suppression" = "gray76", 
                               "Immigrant Deportation" = "darkgreen", "Travel Ban" =  "lightsteelblue3")) + 
                  theme_light() 
  fig13 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) 
 #black and white theme in the background
###################################

#media_suppression_india_students, media_suppression_india_parents,immigration_deportation_india_students,
         #immigration_deportation_india_parents, citizenship_india_students, citizenship_india_parents,
        # affirmative_action_india_students, affirmative_action_india_parents, police_brutality_india_students, 
         #police_brutality_india_parents, hindutwa_threat_students, hindutwa_threat_parents)

Ind_ediff <- read.csv("combined_SIAA.csv")


Ind_ediff  <- Ind_ediff   %>%
  select(media_suppression_encoded_diff, 
         immigration_deportation_india_encoded_diff, 
         citizenship_india_encoded_diff, 
         affirmative_action_india_encoded_diff, 
         police_brutality_india_encoded_diff, 
         hindutwa_threat_encoded_diff)
         
         

Ind_ediff2 <- colMeans(x=Ind_ediff, na.rm = TRUE)
Ind_ediff2 <- data.frame(Ind_ediff2)
Ind_ediff2 <- round(Ind_ediff2$Ind_ediff2, 2)
Ind_ediff2 <- data.frame(Ind_ediff2)


Ind_ediff2$Question  <- c('Media Suppression',
                          'Immigrant Deportation', 'Citizenship for Muslim Immigrants', 
                          'Affirmative Action', 'Police Brutality', 'Hindu Nationalist Threat')


Ind_ediff2 <- Ind_ediff2 %>% rename(Average_Difference = Ind_ediff2)


fig14 <-
  ggplot(data=Ind_ediff2 , aes(x=Question, y=Average_Difference, fill=Question)) +
  geom_bar(stat = 'identity', width = .45)  + 
  labs(title = "Average Difference for India Policy Questions between Generations" ,
       x = "Policy Questions",
       y = "Differences", 
       colour = "") + 
  scale_fill_manual(values = c("Media Suppression" = "midnightblue", 
                               "Immigrant Deportation" = "brown3", 
                               "Citizenship for Muslim Immigrants" = "goldenrod1", "Affirmative Action" = "cadetblue3", "Media Suppression" = "gray76", 
                               "Police Brutality" = "darkgreen", "Hindu Nationalist Threat" =  "lightsteelblue3")) + 
  theme_light() 
  fig14 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) 
#black and white theme in the background













 

###########################################
l = mget(plots)

MyPlots = list(fig1, fig2, fig5, fig6, fig8, fig9, 
               fig10, fig11, fig12)
pdf("graphs.pdf")
MyPlots
dev.off()


#x axis = different question 
#y axis = absolute difference within that question 
#% 5 categories = % = 5-absolute difference / 5 (x100)




