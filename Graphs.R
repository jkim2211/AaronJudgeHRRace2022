library(tidyverse)

#Graph 1: Time series graph for Home Runs
fangraphsLogs$Date <- as.Date(fangraphsLogs$Date)
CumHrDF <- fangraphsLogs %>%
  filter(PlayerName!="Barry Bonds")%>%
  group_by(PlayerName)%>%
  arrange(Date) %>%
  mutate(CumHR=cumsum(HR))%>%
  select(Date,CumHR)

ggplot(data=CumHrDF, aes(x=Date,y=CumHR,color=PlayerName)) +
  geom_line()+
  labs(title="MLB's Top 4 Home Run Leaders in 2022")+
  xlab("Date")+
  ylab("Homeruns")+
  theme_classic()+
  scale_color_manual(
    name="2022 Home Run Leaders",
    values=c("Aaron Judge"="navyblue",
             "Kyle Schwarber"="Red",
             "Pete Alonso"="lightblue",
             "Mike Trout"="firebrick3"),
    label=c("Aaron Judge (62)","Kyle Schwarber (46)","Pete Alonso (40) ","Mike Trout (40)"))

CumHrDFBarry <- fangraphsLogs %>% #add 2001 Barry Bonds
  group_by(PlayerName)%>%
  arrange(gameNum) %>%
  mutate(CumHR=cumsum(HR))%>%
  select(gameNum,CumHR)

ggplot(data=CumHrDFBarry, aes(x=gameNum,y=CumHR,color=PlayerName)) +
  geom_line()+
  labs(title="2022 Home Run Leaders and 2001 Barry Bonds")+
  xlab("Games Played")+
  ylab("Homeruns")+
  theme_classic()+
  scale_color_manual(
    name="Home Run Leaders",
    values=c("Barry Bonds" = "darkorange",
             "Aaron Judge"="navyblue",
             "Kyle Schwarber"="Red",
             "Pete Alonso"="lightblue",
             "Mike Trout"="firebrick3"),
    label=c("2001 Barry Bonds (73)","Aaron Judge (62)","Kyle Schwarber (46)","Pete Alonso (40) ","Mike Trout (40)"))

#Graph 2 - at-bats per home run
baseref <- baseref %>% mutate(ABperHR=round(AB/HR,2))
ggplot(data=baseref,aes(x=Name,y=ABperHR,fill=Name))+
  geom_bar(stat="identity")+
  ggtitle("At-Bats per Home Run")+
  theme_classic()+
  scale_fill_discrete(name="Players",guide="none")+
  xlab("Player")+
  ylab("AB/HR")

#Graph 3 - scatterplot of exit velo and launch angle
HRsavantStats <- savantStats %>% filter(events=="home_run")
ggplot(data=HRsavantStats,aes(x=launch_angle,y=launch_speed,color=player_name))+
  geom_point()+
  geom_smooth(method="lm",level=0)+
  labs(title="Relationships between Launch Angle (x) and Exit Velocity (y)",
       x="Launch angle",
       y="Exit velocity")+
  scale_color_discrete(name="Player")

HRsavantStatsAvg <- HRsavantStats %>%
  group_by(player_name) %>%
  summarise(avgLA = mean(launch_angle),
             avgEV = mean(launch_speed))

ggplot(data=HRsavantStatsAvg,aes(x=avgLA,y=avgEV,color=player_name))+
  geom_point(size=6)+
  geom_smooth(method="lm",level=0)+
  labs(title="Relationships between Avg. Launch Angle (x) and Avg. Exit Velocity (y)",
       x="Average launch angle",
       y="Average exit velocity")+
  scale_color_discrete(name="Player")+
  xlim(24,31)+
  ylim(105,110)
  

#Graph 4 - proportion of hits for each player (bar chart)
baserefHits <- baseref %>%
  select(Name,H,X1B,X2B,X3B,HR)
baserefHits <- baserefHits %>%
  pivot_longer(cols=c("X1B","X2B","X3B","HR"),
               names_to ="hit type",
               values_to ="hit amount")

ggplot(data=baserefHits,aes(x=Name,y=`hit amount`/H,fill=`hit type`))+
  geom_bar(stat="identity")+
  labs(title="2022 HR leaders: Proportions of Hit Types",x="Player", y="Proportion")+
  scale_fill_discrete(labels=c("homerun","single","double","triple"))


#Graph 5 - distance traveled for home runs
hrDist <- savantStats %>%
  filter(events== "home_run") %>%
  select(player_name,hit_distance_sc) %>%
  group_by(player_name)

avgDist <- hrDist %>%
  group_by(player_name)%>%
  summarise(dist=round(mean(hit_distance_sc),0))

ggplot(data=hrDist,aes(x=player_name,y=hit_distance_sc,color=player_name))+
  geom_boxplot()+
  theme_classic()+
  scale_color_brewer(palette="Set1",guide="none")+
  labs(title="Homerun Distances Travelled",
       subtitle="Mean distances labelled",
       x="Player",y="Distance Traveled (ft)")+
  coord_flip()+
  geom_text(data=avgDist,aes(label=dist,y=dist),vjust=-3,hjust=.3)

#Graph 6 - home run rate by pitch type
savantStats$pitch_type <- gsub("FS","SI",savantStats$pitch_type)
savantHRPitches <- savantStats %>%
  filter(events=="home_run") %>%
  group_by(pitch_type,player_name)%>%
  summarise(HRPitch=n())


savantBIP <- savantStats %>%
  filter(description=="hit_into_play") %>%
  group_by(pitch_type, player_name) %>%
  summarise(totalBIP=n())

savantPitches <- left_join(savantHRPitches,savantBIP,by=c("player_name","pitch_type"))

totalABs <- baseref %>% select(Name,AB)
savantPitches <- left_join(savantPitches,totalABs,by=c("player_name"="Name"))


savantPitches <- savantPitches %>% filter(pitch_type!="FA" & pitch_type!="FS" & pitch_type!="KC") #removing 'FA', 'FC', 'FS' (low sample)
savantPitches$pitch_type <- gsub("CH","Changeup",
                            gsub("FC","Cutter",
                            gsub("CU","Curveball",
                            gsub("FF","Four-seam Fastball",
                            gsub("SI","Sinker (sinking-fastball)",
                            gsub("SL","Slider",savantPitches$pitch_type))))))

ggplot(data=baseref,aes(x=Name,y=HR/AB,fill=Name))+ # home run rate
  labs(title="Homerun Rate per At-Bat",x="Player",y="Homerun Rate")+
  geom_bar(stat="identity")+
  scale_fill_discrete(guide="none")+
  theme_classic()

ggplot(data=savantPitches,aes(x=player_name,y=HRPitch/totalBIP,fill=player_name))+ #Home run rate per at-bat, facet_wrap by pitch
  geom_bar(stat="identity")+
  labs(title="Homerun Rate for Balls in Play by Pitch Type",x=element_blank(),y="HR % for Balls in Play")+
  scale_fill_discrete(name="Player")+
  theme_classic()+
  facet_wrap(~ pitch_type)+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())



