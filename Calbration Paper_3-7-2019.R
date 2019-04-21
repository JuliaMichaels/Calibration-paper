# Install and Load Packages --------------------------------------------------------

install.packages('tidyverse'); install.packages('vegan')
install.packages('MASS');install.packages("devtools"); devtools::install_github("gavinsimpson/ggvegan")
install.packages("ggrepel"); install.packages('ggplot2');install.packages('dplyr'); install.packages('scales')
install.packages('viridis'); install.packages('lme4')
install.packages('lmerTest'); install.packages('gridExtra');install.packages('ggplot2');library(lmerTest); install.packages('ggpubr')

install.packages('colorspace'); install.packages('ggpubr')
library('ggvegan'); library('tidyverse'); library('ggplot2')
library('MASS');  library('scales')
library('viridis'); library('lme4'); library('dplyr')
library('ggpubr')
library('gridExtra')


setwd("C:\\Users\\Julia Michaels\\Google Drive\\Dissertation Chapter 2\\Chapter_2_Analysis")



# Load data ---------------------------------------------------------------
#calibration<-read.csv("2018 Inundation_Stopping 2018-03-19.csv")
data_loggers_2018<-read.csv('2018_Levelloggers.csv')
staff_gauge_2018<-read.csv('2017-2018 Staff Gauges All.csv')
precip_2018<-read.csv('precipitation data_2018.csv')#in mm?
#days_2018<-read.csv("2018_Master_List.csv")
#Calibrated_2018<-read.csv("2018_Calibrated.csv")
#all_years_combined<-read.csv("All_Years_Master_List.csv") 
#qdata<-read.csv("2017-2018 Vegetation Quadrats.csv",fileEncoding="UTF-8-BOM")

# Calculate total precipitation -------------------------------------------------

#2018
precip_2018_by_day<-precip_2018 %>%
  group_by(Date) %>% 
  summarize(rainfall=sum(Precip, na.rm = TRUE)) %>% 
  slice(2:n()) %>% 
  mutate(rainfall=rainfall/10)

# Calculate total days of inundation from data loggers --------------------

DL2018<-data_loggers_2018%>%              #Average hourly datalogger data by day
  group_by(Date) %>% 
  summarise_all(funs(median)) 


dl_days_2018<-c()
for(i in 2:ncol(DL2018)){
  dl_days_2018[i]<-sum(DL2018[,i]>0, na.rm=TRUE)#took out na.rm=TRUE to look at ones where DL stops early, way after pools had dried
}

dl_days_2018<-tibble(dl_days_2018, Pool.ID=colnames(DL2018))

dl_days_2018<-dl_days_2018[-15,]

#calculate max depth from data loggers

DL2018_2<-DL2018<-data.frame(DL2018[, 2:ncol(DL2018)])
DL2018_2[DL2018_2 < 0] = 0
DL2018_2<-DL2018_2 %>% 
  summarize_all(funs(max), na.rm=TRUE)
DL2018_2<-gather(DL2018_2, "Pool.ID", "Level") 
mean(DL2018_2$Level)

# Calculate total days inundation from staff gauges -----------------------
#are we going to subset or keep all sg dates?

sg_subset_2018<-data.frame(staff_gauge_2018[, 2:ncol(staff_gauge_2018)])
sg_subset_2018[sg_subset_2018 < 0] = 0
sg_subset_depth<-sg_subset_2018
sg_subset_2018$Date<-staff_gauge_2018$Date
sg_subset_2018[is.na(sg_subset_2018)] = 1 #make all na vals (days we didnt check staff gauge) 1 because we cant assume it was dry
sg_hydrograph_weekly<-sg_subset_2018 #maybe set aside for hydrograph?
sg_subset_2018[sg_subset_2018 > 0] = 1
sg_subset_weekly<-sg_subset_2018
interval_days<-c(1,14,10,7,8,7,7,9,7,7,8,7,8,6,7,7,7,7,4,21,9,7,7)
sg_subset_weekly$Interval_Days<-interval_days
sg_subset_temp<-sg_subset_weekly[,1:56] * sg_subset_weekly[,58]

total_sg_days<-c()
for(i in 1:ncol(sg_subset_temp)){
  total_sg_days[i]<-sum(sg_subset_temp[,i], na.rm=TRUE)
}


sg_subset<-tibble(Pool.ID=colnames(sg_subset_temp), total_sg_days)

# calculate max depth
sg_depth<-as.data.frame(sg_subset_depth[,colnames(sg_subset_depth) %in% DL2018_2$Pool.ID])

sg_depth<-sg_subset_depth %>%
  summarize_all(funs(max), na.rm=TRUE)
sg_depth<-gather(sg_depth, "Pool.ID", "Level") 
mean(sg_depth$Level)



#compare staff Gauge depth to data logger depth
sg_depth<- sg_depth %>% 
  mutate(Method="SG")
DL2018_2<-DL2018_2%>% 
  mutate(Method="DL")
y<-full_join(sg_depth, DL2018_2, by="Pool.ID", "Method") %>% 
  mutate(difference=Level.x-Level.y) %>% 
  filter(!is.na(difference))

depth<-full_join(sg_depth, DL2018_2, by=c("Pool.ID", "Method", "Level"))%>% 
  filter(Pool.ID %in% y$Pool.ID)

ggplot(depth, aes(x=Method, y=Level))+
  geom_boxplot()+
  theme(plot.title=element_text(size=20, hjust=.5))+
  theme(axis.title.x =element_text(size=20))+
  theme(axis.title.y =element_text(size=20))+
  theme(axis.title.x =element_text(size=20))+
  theme(axis.title.y =element_text(size=20))+
  theme(axis.text.x =element_text(size=20))+
  theme(axis.text.y =element_text(size=20))+
  labs(title="Maximum vernal pool depth by method", y="Maximum depth (cm)", x="Method")+
  stat_compare_means(hjust=.5)


t.test(Level~Method, data=depth)

# Compare data loggers to weekly staff gauge ------------------------------

compare<-full_join(dl_days_2018, sg_subset)%>% 
  mutate(difference=dl_days_2018-total_sg_days) %>% 
  filter(!is.na(total_sg_days)) 


#average difference
mean((abs(compare$difference)), na.rm=TRUE)
mean((compare$difference), na.rm=TRUE)
#off by -2.3 days
#SG are off by an average of 7.74 days
#worst was 30, best was 0

mean((compare$dl_days_2018), na.rm=TRUE)
#67.90
mean((compare$total_sg_days), na.rm=TRUE)
#72.05357



compare_weekly<-compare %>% 
  filter(!is.na(difference)) %>% 
  filter(! Pool.ID=="D5.13") %>% 
  mutate(Method ="Weekly")


fit_weekly<-lm(dl_days_2018 ~ total_sg_days, data = compare_weekly)

weekly<-ggplot(data=compare_weekly, aes(x=dl_days_2018,y=total_sg_days))+
  geom_point()+
  stat_smooth(method='lm', se=FALSE, fullrange = TRUE)+
  geom_label(aes(x = 40, y = 70), hjust = 0, 
             label = paste("",
                           "\nAdj R2 =" ,signif(summary(fit_weekly)$adj.r.squared, 5),
                           "\nIntercept =",signif(fit_weekly$coef[[1]],5 ),
                           " \nSlope =",signif(fit_weekly$coef[[2]], 5),
                           " \nP =",signif(summary(fit_weekly)$coef[2,4], 5)))

weekly





# Compare data loggers to every 2 weeks staff gauge ------------------------------

# 2018 Pull out subset of every other date from staff gauges----------------------------

sg_subset_biweekly<-sg_subset_2018 %>% 
  filter(Date %in% c('2017-11-06','2017-11-30', '2017-12-15',  '2017-12-29', '2018-01-14',  '2018-01-29',  '2018-02-13','2018-02-26',  '2018-03-12', '2018-03-23', '2018-04-22','2018-05-01'))
interval_days<-c(1,24,15,14,15,15,15,13,14,11,30,14)
sg_subset_biweekly$Interval_Days<-interval_days
sg_subset_temp<-sg_subset_biweekly[,1:56] * sg_subset_biweekly[,58]

total_sg_days<-c()
for(i in 1:ncol(sg_subset_temp)){
  total_sg_days[i]<-sum(sg_subset_temp[,i], na.rm=TRUE)
}


sg_subset_biweekly<-tibble(Pool.ID=colnames(sg_subset_temp), total_sg_days)


compare_biweekly<-full_join(dl_days_2018, sg_subset_biweekly)%>% 
  mutate(difference=dl_days_2018-total_sg_days) %>% 
  filter(!is.na(total_sg_days)) 


#average difference
mean((abs(compare_biweekly$difference)), na.rm=TRUE)
#10.03
mean((compare_biweekly$difference), na.rm=TRUE)
#-.35 days

mean((compare_biweekly$dl_days_2018), na.rm=TRUE)
#67.90
mean((compare_biweekly$total_sg_days), na.rm=TRUE)
#70.09
#biweekly overestimated days

#SG are off by an average of 10.03 days
#worst was 37, best was 1


compare_biweekly<-compare_biweekly %>% 
  filter(!is.na(difference)) %>% 
  filter(! Pool.ID=="D5.13") %>% 
  mutate(Method ="Every Two Weeks")

fit_biweekly<-lm(dl_days_2018 ~ total_sg_days, data = compare_biweekly)

biweekly<-ggplot(data=compare_biweekly, aes(x=dl_days_2018,y=total_sg_days))+
  geom_point()+
  stat_smooth(method='lm', se=FALSE, fullrange = TRUE)+
  geom_label(aes(x = 40, y = 70), hjust = 0, 
             label = paste("",
                           "\nAdj R2 =" ,signif(summary(fit_biweekly)$adj.r.squared, 5),
                           "\nIntercept =",signif(fit_biweekly$coef[[1]],5 ),
                           " \nSlope =",signif(fit_biweekly$coef[[2]], 5),
                           " \nP =",signif(summary(fit_biweekly)$coef[2,4], 5)))

biweekly



# Compare data loggers to every 3 weeks staff gauge ------------------------------

# 2018 Pull out subset of every third date from staff gauges----------------------------

sg_subset_triweekly<-sg_subset_2018 %>% 
  filter(Date %in% c('2017-11-06', '2017-12-07', '2017-12-29',  '2018-01-21',  '2018-02-13', '2018-03-05', '2018-03-23', '2018-04-29'))
interval_days<-c(1,31,22,23,23,20,18,37)
sg_subset_triweekly$Interval_Days<-interval_days
sg_subset_temp<-sg_subset_triweekly[,1:56] * sg_subset_triweekly[,58]

total_sg_days<-c()
for(i in 1:ncol(sg_subset_temp)){
  total_sg_days[i]<-sum(sg_subset_temp[,i], na.rm=TRUE)
}

sg_subset_triweekly<-tibble(Pool.ID=colnames(sg_subset_temp), total_sg_days)

compare_triweekly<-full_join(dl_days_2018, sg_subset_triweekly)%>% 
  mutate(difference=dl_days_2018-total_sg_days) %>% 
  filter(!is.na(total_sg_days)) 


mean((compare_triweekly$dl_days_2018), na.rm=TRUE)
#67.90
mean((compare_triweekly$total_sg_days), na.rm=TRUE)
#70.09

mean((abs(compare_triweekly$difference)), na.rm=TRUE)
#SG are off by an average of 12.13 days
#worst was 28, best was 0
mean((compare_triweekly$difference), na.rm=TRUE)

compare_triweekly<-compare_triweekly %>% 
  filter(!is.na(difference)) %>% 
  filter(! Pool.ID=="D5.13") %>% 
  mutate(Method ="Every Three Weeks")

fit_triweekly<-lm(dl_days_2018 ~ total_sg_days, data = compare_triweekly)

triweekly<-ggplot(data=compare_triweekly, aes(x=dl_days_2018,y=total_sg_days))+
  geom_point()+
  stat_smooth(method='lm', se=FALSE, fullrange = TRUE)+
  geom_label(aes(x = 40, y = 70), hjust = 0, 
             label = paste("",
                           "\nAdj R2 =" ,signif(summary(fit_triweekly)$adj.r.squared, 5),
                           "\nIntercept =",signif(fit_triweekly$coef[[1]],5 ),
                           " \nSlope =",signif(fit_triweekly$coef[[2]], 5),
                           " \nP =",signif(summary(fit_triweekly)$coef[2,4], 5)))

triweekly



# Compare data loggers to monthly staff gauge ------------------------------

# 2018 Pull out subset of every fourth date from staff gauges----------------------------

sg_subset_monthly<-sg_subset_2018 %>% 
  filter(Date %in% c('2017-11-06','2017-12-07', '2018-01-07',  '2018-02-05',  '2018-03-05', '2018-04-13', '2018-05-01'))
interval_days<-c(1,31,31,29,28,39,18)
sg_subset_monthly$Interval_Days<-interval_days
sg_subset_temp<-sg_subset_monthly[,1:56] * sg_subset_monthly[,58]

total_sg_days<-c()
for(i in 1:ncol(sg_subset_temp)){
  total_sg_days[i]<-sum(sg_subset_temp[,i], na.rm=TRUE)
}

sg_subset_monthly<-tibble(Pool.ID=colnames(sg_subset_temp), total_sg_days)

compare_monthly<-full_join(dl_days_2018, sg_subset_monthly)%>% 
  mutate(difference=dl_days_2018-total_sg_days) %>% 
  filter(!is.na(total_sg_days)) 

mean(compare_monthly$difference, na.rm=TRUE)
mean((abs(compare_monthly$difference)), na.rm=TRUE)
#SG are off by an average of 23.84 days
#worst was 46, best was 4


compare_monthly<-compare_monthly %>% 
  filter(!is.na(difference)) %>% 
  mutate(Method ="Monthly")

fit_monthly<-lm(dl_days_2018 ~ total_sg_days, data = compare_monthly)

monthly<-ggplot(data=compare_monthly, aes(x=dl_days_2018,y=total_sg_days))+
  geom_point()+
  stat_smooth(method='lm', se=FALSE, fullrange = TRUE)+
  geom_label(aes(x = 40, y = 70), hjust = 0, 
             label = paste("",
                           "\nAdj R2 =" ,signif(summary(fit_monthly)$adj.r.squared, 5),
                           "\nIntercept =",signif(fit_monthly$coef[[1]],5 ),
                           " \nSlope =",signif(fit_monthly$coef[[2]], 5),
                           " \nP =",signif(summary(fit_monthly)$coef[2,4], 5)))
monthly




all_methods<-full_join(compare_weekly, compare_biweekly, by=c("dl_days_2018", "Pool.ID", "Method", "difference", "total_sg_days"))
all_methods<-full_join(all_methods, compare_triweekly, by=c("dl_days_2018", "Pool.ID", "Method", "total_sg_days","difference"))
all_methods<-full_join(all_methods, compare_monthly, by=c("dl_days_2018", "Pool.ID", "Method", "total_sg_days","difference"))

all_methods$Method <- factor(all_methods$Method, levels=c("Weekly", "Every Two Weeks", "Every Three Weeks", "Monthly"))

#Graph
ggplot(data=all_methods, aes(x=dl_days_2018,y=total_sg_days))+
  geom_point(aes(color = Method), size=3)+
  stat_smooth(method='lm', se=FALSE, fullrange = TRUE, aes(color=Method))+
  labs(title="Comparing Staff Gauge Sampling Frequency to Data Loggers", y="Staff Gauge", x="Data Logger")+
  theme(plot.title=element_text(size=30, hjust=.5))+
  theme(axis.title.x =element_text(size=30))+
  theme(axis.title.y =element_text(size=30))+
  theme(legend.title =element_text(size=30))+
  theme(legend.text =element_text(size=30))+
  theme(axis.text =element_text(size=20))+
  theme(strip.text =element_text(size=20))


# Hydrographs -------------------------------------------------------------
DL<-DL2018 %>% 
  group_by(Date) %>% 
  summarise_all(funs(median)) 
DL_hydrograph<-gather(DL, "Pool.ID", "Level", 2:34)

 
DL_hydrograph<- DL_hydrograph%>% 
  group_by(Date, Pool.ID)%>% 
  summarize(Level=mean(Level))%>% 
  mutate(Method="Hourly datalogger") 

DL_hydrograph[DL_hydrograph < 0] = 0




sg_hydrograph_weekly_temp<-as.data.frame(sg_hydrograph_weekly[,colnames(sg_hydrograph_weekly) %in% DL_hydrograph$Pool.ID])

sg_hydrograph_weekly<-sg_hydrograph_weekly_temp %>% 
  mutate(Date=sg_hydrograph_weekly$Date)

sg_hydrograph_weekly<-gather(sg_hydrograph_weekly, "Pool.ID", "Level", 1:32)
weekly_hydrograph<-sg_hydrograph_weekly %>% 
  group_by(Pool.ID, Date) %>% 
  summarize(Level=mean(Level))%>% 
  mutate(Method="Weekly")



sg_hydrograph_biweekly<-sg_hydrograph_weekly %>% 
  filter(Date %in% c('2017-11-06','2017-11-30', '2017-12-15',  '2017-12-29', '2018-01-14',  '2018-01-29',  '2018-02-13','2018-02-26',  '2018-03-12', '2018-03-23', '2018-04-22','2018-05-01'))
biweekly_hydrograph<-sg_hydrograph_biweekly %>% 
  group_by(Pool.ID, Date) %>% 
  summarize(Level=mean(Level))%>% 
  mutate(Method="Every Two Weeks")


sg_hydrograph_triweekly<-sg_hydrograph_weekly %>% 
  filter(Date %in% c('2017-11-06', '2017-12-07', '2017-12-29',  '2018-01-21',  '2018-02-13', '2018-03-05', '2018-03-23', '2018-04-29'))
triweekly_hydrograph<-sg_hydrograph_triweekly %>% 
  group_by(Pool.ID, Date) %>% 
  summarize(Level=mean(Level)) %>% 
  mutate(Method="Every Three Weeks")


sg_hydrograph_monthly<-sg_hydrograph_weekly %>% 
  filter(Date %in% c('2017-11-06','2017-12-07', '2018-01-07',  '2018-02-05',  '2018-03-05', '2018-04-13', '2018-05-01'))
monthly_hydrograph<-sg_hydrograph_monthly %>% 
  group_by(Pool.ID, Date) %>% 
  summarize(Level=mean(Level)) %>% 
  mutate(Method="Monthly")


hydrograph_all<-full_join(weekly_hydrograph, biweekly_hydrograph, by=c("Pool.ID", "Method", "Date", "Level"))
hydrograph_all<-full_join(hydrograph_all, triweekly_hydrograph, by=c("Pool.ID", "Method", "Date", "Level"))
hydrograph_all<-full_join(hydrograph_all, monthly_hydrograph, by=c("Pool.ID", "Method", "Date", "Level"))
hydrograph_all<-full_join(hydrograph_all, DL_hydrograph, by=c("Method", "Date", "Level"))

hydrograph_averaged<-hydrograph_all %>% 
  group_by(Method, Date) %>% 
  summarize(Level=mean(Level))



#graph
hydrograph_averaged$Method <- factor(hydrograph_averaged$Method, levels=c("Hourly datalogger", "Weekly", "Every Two Weeks", "Every Three Weeks", "Monthly"),
                                     labels=c("Daily", "Weekly", "Every Two Weeks", "Every Three Weeks", "Monthly"))

ggplot(data=hydrograph_averaged, mapping=aes(x=Date, y=Level, group=Method))+
  geom_line(aes(color=Method), size=1.5)+
  geom_point(color='black', size=1)+
  geom_line(data=precip_2018_by_day, mapping=aes(x=Date, y=rainfall, linetype="Precipitation",group=1), color='grey35')+
  scale_linetype_manual(name="", 
                        values=c("Precipitation"="dashed"))+
  #geom_vline(xintercept=138, linetype="dashed", color="black", size=1)+
  theme(plot.title=element_text(hjust=.5))+
  theme(axis.text.y=element_text(size=10))+
  scale_y_continuous(name="Level (cm)")+
  theme(axis.text.x = element_text(angle=90, size=.5))+
  #scale_x_discrete(breaks=c('2017-11-06','2017-11-20', '2017-11-30', '2017-12-07','2017-12-07', '2017-12-15', '2017-12-22', '2017-12-29', '2018-01-07', '2018-01-14', '2018-01-21', '2018-01-29', '2018-02-05', '2018-02-13', '2018-02-19','2018-02-26', '2018-03-05', '2018-03-12', '2018-03-19','2018-03-23', '2018-04-13','2018-04-22', '2018-04-29', '2018-05-01', '2018-05-8', '2018-05-15', '2018-05-25', '2018-6-02'))+
  #scale_x_discrete(breaks = hydrograph_averaged$Date[seq(2, length(DL_hydrograph$Date), by =30)])+
  labs(title="Average Vernal Pool Depth by Measuement Method, 2017-2018", y="Pool Depth", x="Date")+
  theme(plot.title=element_text(size=30, hjust=.5))+
  theme(axis.title.x =element_text(size=30))+
  theme(axis.title.y =element_text(size=30))+
  theme(strip.text = element_text(size=20))+
  theme(legend.title =element_text(size=30))+
  theme(legend.text =element_text(size=25))+
  theme(axis.text.y =element_text(size=10))+
  theme(axis.text.x =element_blank())+
  theme(axis.ticks.x =element_blank())+
  facet_wrap(~Method)
  

# Graph all modelled pools ------------------------------------------------

SG<-staff_gauge_2018                   #Load staff gauge data

joined<-full_join(DL, SG, "Date") #Join data logger and staff gaugue
joined[joined < 0] = 0

S1<-ggplot(joined, mapping=aes(joined$Date, joined$C2.19, group=1, linetype='solid'))+
  geom_line()+
  geom_point(mapping=aes(joined$Date, joined$B3.91, color="red"))+
  geom_hline(yintercept=0, linetype="dashed", color="grey")+
  labs(y="Depth (cm)", x="Date")+
  theme(plot.title=element_text(size=20, hjust=.5))+
  theme(axis.title.x =element_text(size=20))+
  theme(axis.title.y =element_text(size=20))+
  theme(legend.title =element_text(size=20))+
  theme(legend.text =element_text(size=20))+
  scale_linetype_manual(name="", labels = "C2-19", values ="solid")+
  scale_color_manual(name="", labels = "B3-91", values ="red")

S2<-ggplot(joined, mapping=aes(joined$Date, joined$D5.02.x, group=1, linetype='solid'))+
  geom_line()+ 
  geom_point(mapping=aes(joined$Date, joined$B3.92, color="red"))+
  geom_hline(yintercept=0, linetype="dashed", color="grey")+
  labs(y="Depth (cm)", x="Date")+
  theme(plot.title=element_text(size=20, hjust=.5))+
  theme(axis.title.x =element_text(size=20))+
  theme(axis.title.y =element_text(size=20))+
  theme(legend.title =element_text(size=20))+
  theme(legend.text =element_text(size=20))+
  scale_linetype_manual(name="", labels = "D5-02", values ="solid")+
  scale_color_manual(name="", labels = "B3-92", values ="red")

S3<-ggplot(joined, mapping=aes(joined$Date, joined$D5.03.x, group=1, linetype='solid'))+
  geom_line()+
  geom_point(mapping=aes(joined$Date, joined$B4.27, color="red"))+
  geom_hline(yintercept=0, linetype="dashed", color="grey")+
  labs(y="Depth (cm)", x="Date")+
  theme(plot.title=element_text(size=20, hjust=.5))+
  theme(axis.title.x =element_text(size=20))+
  theme(axis.title.y =element_text(size=20))+
  theme(legend.title =element_text(size=20))+
  theme(legend.text =element_text(size=20))+
  scale_linetype_manual(name="", labels = "D5-03", values ="solid")+
  scale_color_manual(name="", labels = "B4-27", values ="red")


S4<-ggplot(joined, mapping=aes(joined$Date, joined$D5.15.x, group=1, linetype='solid'))+
  geom_line()+
  geom_point(mapping=aes(joined$Date, joined$B4.28, color="red"))+
  geom_hline(yintercept=0, linetype="dashed", color="grey")+
  labs(y="Depth (cm)", x="Date")+
  theme(plot.title=element_text(size=20, hjust=.5))+
  theme(axis.title.x =element_text(size=20))+
  theme(axis.title.y =element_text(size=20))+
  theme(legend.title =element_text(size=20))+
  theme(legend.text =element_text(size=20))+
  scale_linetype_manual(name="", labels = "D5-15", values ="solid")+
  scale_color_manual(name="", labels = "B4-28", values ="red")

S5<-ggplot(joined, mapping=aes(joined$Date, joined$E5.27.x, group=1, linetype='solid'))+
  geom_line()+
  geom_point(mapping=aes(joined$Date, joined$C2.11, color="red"))+
  geom_hline(yintercept=0, linetype="dashed", color="grey")+
  labs(y="Depth (cm)", x="Date")+
  theme(plot.title=element_text(size=20, hjust=.5))+
  theme(axis.title.x =element_text(size=20))+
  theme(axis.title.y =element_text(size=20))+
  theme(legend.title =element_text(size=20))+
  theme(legend.text =element_text(size=20))+
  scale_linetype_manual(name="", labels = "E5.27", values ="solid")+
  scale_color_manual(name="", labels = "C2-11", values ="red")

S6<-ggplot(joined, mapping=aes(joined$Date, joined$D5.39.x, group=1, linetype='solid'))+
  geom_line()+
  geom_point(mapping=aes(joined$Date, joined$C2.16, color="red"))+
  geom_hline(yintercept=0, linetype="dashed", color="grey")+
  labs(y="Depth (cm)", x="Date")+
  theme(plot.title=element_text(size=20, hjust=.5))+
  theme(axis.title.x =element_text(size=20))+
  theme(axis.title.y =element_text(size=20))+
  theme(legend.title =element_text(size=20))+
  theme(legend.text =element_text(size=20))+
  scale_linetype_manual(name="", labels = "D5-39", values ="solid")+
  scale_color_manual(name="", labels = "C2-16", values ="red")

S7<-ggplot(joined, mapping=aes(joined$Date, joined$D6.61.x, group=1, linetype='solid'))+
  geom_line()+
  geom_point(mapping=aes(joined$Date, joined$C3.13.y, color="red"))+
  geom_hline(yintercept=0, linetype="dashed", color="grey")+
  labs(y="Depth (cm)", x="Date")+
  theme(plot.title=element_text(size=20, hjust=.5))+
  theme(axis.title.x =element_text(size=20))+
  theme(axis.title.y =element_text(size=20))+
  theme(legend.title =element_text(size=20))+
  theme(legend.text =element_text(size=20))+
  scale_linetype_manual(name="", labels = "D6-61", values ="solid")+
  scale_color_manual(name="", labels = "C3-13", values ="red")


S8<-ggplot(joined, mapping=aes(joined$Date, joined$D6.61.x, group=1, linetype='solid'))+
  geom_line()+
  geom_point(mapping=aes(joined$Date, joined$C3.17, color="red"))+
  geom_hline(yintercept=0, linetype="dashed", color="grey")+
  labs(y="Depth (cm)", x="Date")+
  theme(plot.title=element_text(size=20, hjust=.5))+
  theme(axis.title.x =element_text(size=20))+
  theme(axis.title.y =element_text(size=20))+
  theme(legend.title =element_text(size=20))+
  theme(legend.text =element_text(size=20))+
  scale_linetype_manual(name="", labels = "D6-61", values ="solid")+
  scale_color_manual(name="", labels = "C3-17", values ="red")


S9<-ggplot(joined, mapping=aes(joined$Date, joined$C2.17.x, group=1, linetype='solid'))+
  geom_line()+
  geom_point(mapping=aes(joined$Date, joined$C3.19, color="red"))+
  geom_hline(yintercept=0, linetype="dashed", color="grey")+
  labs(y="Depth (cm)", x="Date")+
  theme(plot.title=element_text(size=20, hjust=.5))+
  theme(axis.title.x =element_text(size=20))+
  theme(axis.title.y =element_text(size=20))+
  theme(legend.title =element_text(size=20))+
  theme(legend.text =element_text(size=20))+
  scale_linetype_manual(name="", labels = "C2-17", values ="solid")+
  scale_color_manual(name="", labels = "C3-19", values ="red")


S10<-ggplot(joined, mapping=aes(joined$Date, joined$E5.31.x, group=1, linetype='solid'))+
  geom_line()+
  geom_point(mapping=aes(joined$Date, joined$D5.18, color="red"))+
  geom_hline(yintercept=0, linetype="dashed", color="grey")+
  labs(y="Depth (cm)", x="Date")+
  theme(plot.title=element_text(size=20, hjust=.5))+
  theme(axis.title.x =element_text(size=20))+
  theme(axis.title.y =element_text(size=20))+
  theme(legend.title =element_text(size=20))+
  theme(legend.text =element_text(size=20))+
  scale_linetype_manual(name="", labels = "E5-31", values ="solid")+
  scale_color_manual(name="", labels = "D5-18", values ="red")

S11<-ggplot(joined, mapping=aes(joined$Date, joined$C2.19, group=1, linetype='solid'))+
  geom_line()+
  geom_point(mapping=aes(joined$Date, joined$D5.20, color="red"))+
  geom_hline(yintercept=0, linetype="dashed", color="grey")+
  labs(y="Depth (cm)", x="Date")+
  theme(plot.title=element_text(size=20, hjust=.5))+
  theme(axis.title.x =element_text(size=20))+
  theme(axis.title.y =element_text(size=20))+
  theme(legend.title =element_text(size=20))+
  theme(legend.text =element_text(size=20))+
  scale_linetype_manual(name="", labels = "C2-19", values ="solid")+
  scale_color_manual(name="", labels = "D5-20", values ="red")

S12<-ggplot(joined, mapping=aes(joined$Date, joined$D5.15.x, group=1, linetype='solid'))+
  geom_line()+
  geom_point(mapping=aes(joined$Date, joined$D5.21, color="red"))+
  geom_hline(yintercept=0, linetype="dashed", color="grey")+
  labs(y="Depth (cm)", x="Date")+
  theme(plot.title=element_text(size=20, hjust=.5))+
  theme(axis.title.x =element_text(size=20))+
  theme(axis.title.y =element_text(size=20))+
  theme(legend.title =element_text(size=20))+
  theme(legend.text =element_text(size=20))+
  scale_linetype_manual(name="", labels = "D5-15", values ="solid")+
  scale_color_manual(name="", labels = "D5-21", values ="red")


S13<-ggplot(joined, mapping=aes(joined$Date, joined$E5.31.x, group=1, linetype='solid'))+
  geom_line()+
  geom_point(mapping=aes(joined$Date, joined$D5.28, color="red"))+
  geom_hline(yintercept=0, linetype="dashed", color="grey")+
  labs(y="Depth (cm)", x="Date")+
  theme(plot.title=element_text(size=20, hjust=.5))+
  theme(axis.title.x =element_text(size=20))+
  theme(axis.title.y =element_text(size=20))+
  theme(legend.title =element_text(size=20))+
  theme(legend.text =element_text(size=20))+
  scale_linetype_manual(name="", labels = "E5-31", values ="solid")+
  scale_color_manual(name="", labels = "D5-28", values ="red")



S14<-ggplot(joined, mapping=aes(joined$Date, joined$C2.19, group=1, linetype='solid'))+
  geom_line()+
  geom_point(mapping=aes(joined$Date, joined$D5.35, color="red"))+
  geom_hline(yintercept=0, linetype="dashed", color="grey")+
  labs(y="Depth (cm)", x="Date")+
  theme(plot.title=element_text(size=20, hjust=.5))+
  theme(axis.title.x =element_text(size=20))+
  theme(axis.title.y =element_text(size=20))+
  theme(legend.title =element_text(size=20))+
  theme(legend.text =element_text(size=20))+
  scale_linetype_manual(name="", labels = "C2-19", values ="solid")+
  scale_color_manual(name="", labels = "D5-35", values ="red")

S15<-ggplot(joined, mapping=aes(joined$Date, joined$C2.19, group=1, linetype='solid'))+
  geom_line()+
  geom_point(mapping=aes(joined$Date, joined$D6.58, color="red"))+
  geom_hline(yintercept=0, linetype="dashed", color="grey")+
  labs(y="Depth (cm)", x="Date")+
  theme(plot.title=element_text(size=20, hjust=.5))+
  theme(axis.title.x =element_text(size=20))+
  theme(axis.title.y =element_text(size=20))+
  theme(legend.title =element_text(size=20))+
  theme(legend.text =element_text(size=20))+
  scale_linetype_manual(name="", labels = "C2-19", values ="solid")+
  scale_color_manual(name="", labels = "D6-58", values ="red")


S17<-ggplot(joined, mapping=aes(joined$Date, joined$D5.15.x, group=1, linetype='solid'))+
  geom_line()+
  geom_point(mapping=aes(joined$Date, joined$D6.60, color="red"))+
  geom_hline(yintercept=0, linetype="dashed", color="grey")+
  labs(y="Depth (cm)", x="Date")+
  theme(plot.title=element_text(size=20, hjust=.5))+
  theme(axis.title.x =element_text(size=20))+
  theme(axis.title.y =element_text(size=20))+
  theme(legend.title =element_text(size=20))+
  theme(legend.text =element_text(size=20))+
  scale_linetype_manual(name="", labels = "D5-15", values ="solid")+
  scale_color_manual(name="", labels = "D6-60", values ="red")

S18<-ggplot(joined, mapping=aes(joined$Date, joined$E5.30.x, group=1, linetype='solid'))+
  geom_line(size=1)+
  geom_point(mapping=aes(joined$Date, joined$D6.63, color="red"))+
  geom_vline(xintercept=138, linetype="dashed", color="black")+
  labs(y="Depth (cm)", x="Date")+
  theme(plot.title=element_text(size=20, hjust=.5))+
  theme(axis.title.x =element_text(size=20))+
  theme(axis.title.y =element_text(size=20))+
  theme(legend.title =element_text(size=20))+
  theme(legend.text =element_text(size=20))+
  scale_linetype_manual(name="", labels = "E5-30", values ="solid")+
  scale_color_manual(name="", labels = "D6-63", values ="red")



S19<-ggplot(joined, mapping=aes(joined$Date, joined$E5.04.x, group=1, linetype='solid'))+
  geom_line()+
  geom_point(mapping=aes(joined$Date, joined$E5.02, color="red"))+
  geom_hline(yintercept=0, linetype="dashed", color="grey")+
  labs(y="Depth (cm)", x="Date")+
  theme(plot.title=element_text(size=20, hjust=.5))+
  theme(axis.title.x =element_text(size=20))+
  theme(axis.title.y =element_text(size=20))+
  theme(legend.title =element_text(size=20))+
  theme(legend.text =element_text(size=20))+
  scale_linetype_manual(name="", labels = "E5-04", values ="solid")+
  scale_color_manual(name="", labels = "E5-02", values ="red")




S21<-ggplot(joined, mapping=aes(joined$Date, joined$D5.02.x, group=1, linetype='solid'))+
  geom_line()+
  geom_point(mapping=aes(joined$Date, joined$E5.03, color="red"))+
  geom_hline(yintercept=0, linetype="dashed", color="grey")+
  labs(y="Depth (cm)", x="Date")+
  theme(plot.title=element_text(size=20, hjust=.5))+
  theme(axis.title.x =element_text(size=20))+
  theme(axis.title.y =element_text(size=20))+
  theme(legend.title =element_text(size=20))+
  theme(legend.text =element_text(size=20))+
  scale_linetype_manual(name="", labels = "D5.02", values ="solid")+
  scale_color_manual(name="", labels = "E5-03", values ="red")



S22<-ggplot(joined, mapping=aes(joined$Date, joined$D5.30.x, group=1, linetype='solid'))+
  geom_line()+
  geom_point(mapping=aes(joined$Date, joined$E5.23, color="red"))+
  labs(y="Depth (cm)", x="Date")+
  theme(plot.title=element_text(size=20, hjust=.5))+
  theme(axis.title.x =element_text(size=20))+
  theme(axis.title.y =element_text(size=20))+
  theme(legend.title =element_text(size=20))+
  theme(legend.text =element_text(size=20))+
  scale_linetype_manual(name="", labels = "D5-30", values ="solid")+
  scale_color_manual(name="", labels = "E5-23", values ="red")


S23<-ggplot(joined, mapping=aes(joined$Date, joined$D5.08.x, group=1, linetype='solid'))+
  geom_line()+
  geom_point(mapping=aes(joined$Date, joined$E5.26, color="red"))+
  geom_hline(yintercept=0, linetype="dashed", color="grey")+
  labs(y="Depth (cm)", x="Date")+
  theme(plot.title=element_text(size=20, hjust=.5))+
  theme(axis.title.x =element_text(size=20))+
  theme(axis.title.y =element_text(size=20))+
  theme(legend.title =element_text(size=20))+
  theme(legend.text =element_text(size=20))+
  scale_linetype_manual(name="", labels = "D5-08", values ="solid")+
  scale_color_manual(name="", labels = "E5-26", values ="red")


S24<-ggplot(joined, mapping=aes(joined$Date, joined$D5.08.x, group=1, linetype='solid'))+
  geom_line()+
  geom_point(mapping=aes(joined$Date, joined$C2.10.y, color="red"))+
  geom_hline(yintercept=0, linetype="dashed", color="grey")+
  labs(y="Depth (cm)", x="Date")+
  theme(plot.title=element_text(size=20, hjust=.5))+
  theme(axis.title.x =element_text(size=20))+
  theme(axis.title.y =element_text(size=20))+
  theme(legend.title =element_text(size=20))+
  theme(legend.text =element_text(size=20))+
  scale_linetype_manual(name="", labels = "C2-10", values ="solid")+
  scale_color_manual(name="", labels = "E5-29", values ="red")


S25<-ggplot(joined, mapping=aes(joined$Date, joined$C2.17.x, group=1, linetype='solid'))+
  geom_line()+
  geom_point(mapping=aes(joined$Date, joined$E5.38, color="red"))+
  geom_hline(yintercept=0, linetype="dashed", color="grey")+
  labs(y="Depth (cm)", x="Date")+
  theme(plot.title=element_text(size=20, hjust=.5))+
  theme(axis.title.x =element_text(size=20))+
  theme(axis.title.y =element_text(size=20))+
  theme(legend.title =element_text(size=20))+
  theme(legend.text =element_text(size=20))+
  scale_linetype_manual(name="", labels = "C2-17", values ="solid")+
  scale_color_manual(name="", labels = "E5-38", values ="red")



r1<-grid.arrange(S1, S2, S3, S4, S5, nrow=1) #3000x466
r2<-grid.arrange(S6, S7, S8, S9, S10, nrow=1)
r3<-grid.arrange(S11, S12, S13, S14, S15, nrow=1)
r4<-grid.arrange(S17, S18, S19, S21, S22, nrow=1)
r5<-grid.arrange(S23, S25, nrow=1)



# remove all DLs from their own pools, re-calibrate -------------------------------------------

############THERE IS A 

#Variable inundation


DV1<-ggplot(joined, mapping=aes(joined$Date, joined$D5.02.x, group=1, linetype='D5-02 Datalogger'))+
  geom_line()+
  geom_line(mapping=aes(joined$Date, joined$D5.14.x, group=1, linetype='D5-14 Datalogger'))+
  geom_point(mapping=aes(joined$Date, joined$D5.02.y, color="red"))+
  geom_hline(yintercept=0, linetype="dashed", color="grey")+
  labs(y="Depth (cm)", x="Date", size=3)+
  theme(plot.title=element_text(size=20, hjust=.5))+
  theme(axis.title.x =element_text(size=20))+
  theme(legend.title = element_blank())+
  theme(axis.title.y =element_text(size=20))+
  theme(legend.text =element_text(size=20))+
  scale_color_manual(name="", labels = "D5-02 Staff Gauge", values ="red")



DV2<-ggplot(joined, mapping=aes(joined$Date, joined$D5.08.x, group=1, linetype='D5-08 Datalogger'))+
  geom_line()+
  geom_line(mapping=aes(joined$Date, joined$E5.05.x, group=1, linetype='E5-05 Datalogger'))+
  geom_point(mapping=aes(joined$Date, joined$D5.08.y, color="red"))+
  geom_hline(yintercept=0, linetype="dashed", color="grey")+
  labs(y="Depth (cm)", x="Date", size=3)+
  theme(plot.title=element_text(size=20, hjust=.5))+
  theme(axis.title.x =element_text(size=20))+
  theme(legend.title = element_blank())+
  theme(axis.title.y =element_text(size=20))+
  theme(legend.text =element_text(size=20))+
  scale_color_manual(name="", labels = "D5-08 Staff Gauge", values ="red")

DV3<-ggplot(joined, mapping=aes(joined$Date, joined$D5.11.x, group=1, linetype='D5-11 Datalogger'))+
  geom_line()+
  geom_line(mapping=aes(joined$Date, joined$D6.61.x, group=1, linetype='D6-61 Datalogger'))+
  geom_point(mapping=aes(joined$Date, joined$D5.11.y, color="red"))+
  geom_hline(yintercept=0, linetype="dashed", color="grey")+
  labs(y="Depth (cm)", x="Date", size=3)+
  theme(plot.title=element_text(size=20, hjust=.5))+
  theme(axis.title.x =element_text(size=20))+
  theme(legend.title = element_blank())+
  theme(axis.title.y =element_text(size=20))+
  theme(legend.text =element_text(size=20))+
  scale_color_manual(name="", labels = "D5-11 Staff Gauge", values ="red")


DV5<-ggplot(joined, mapping=aes(joined$Date, joined$D5.14.x, group=1, linetype='D5-14 Datalogger'))+
  geom_line()+
  geom_line(mapping=aes(joined$Date, joined$D5.02.x, group=1, linetype='D5-02 Datalogger'))+
  geom_point(mapping=aes(joined$Date, joined$D5.14.y, color="red"))+
  geom_hline(yintercept=0, linetype="dashed", color="grey")+
  labs(y="Depth (cm)", x="Date", size=3)+
  theme(plot.title=element_text(size=20, hjust=.5))+
  theme(axis.title.x =element_text(size=20))+
  theme(legend.title = element_blank())+
  theme(axis.title.y =element_text(size=20))+
  theme(legend.text =element_text(size=20))+
  scale_color_manual(name="", labels = "D5-14 Staff Gauge", values ="red")

DV6<-ggplot(joined, mapping=aes(joined$Date, joined$D5.16.x, group=1, linetype='D5-16 Datalogger'))+
  geom_line()+
  geom_line(mapping=aes(joined$Date, joined$D5.15.x, group=1, linetype='D5-15 Datalogger'))+
  geom_point(mapping=aes(joined$Date, joined$D5.16.y, color="red"))+
  geom_hline(yintercept=0, linetype="dashed", color="grey")+
  labs(y="Depth (cm)", x="Date", size=3)+
  theme(plot.title=element_text(size=20, hjust=.5))+
  theme(axis.title.x =element_text(size=20))+
  theme(legend.title = element_blank())+
  theme(axis.title.y =element_text(size=20))+
  theme(legend.text =element_text(size=20))+
  scale_color_manual(name="", labels = "D5-16 Staff Gauge", values ="red")

DV7<-ggplot(joined, mapping=aes(joined$Date, joined$D5.17.x, group=1, linetype='D5-17 Datalogger'))+
  geom_line()+
  geom_line(mapping=aes(joined$Date, joined$D5.30.x, group=1, linetype='D5-30 Datalogger'))+
  geom_point(mapping=aes(joined$Date, joined$D5.17.y, color="red"))+
  geom_hline(yintercept=0, linetype="dashed", color="grey")+
  labs(y="Depth (cm)", x="Date", size=3)+
  theme(plot.title=element_text(size=20, hjust=.5))+
  theme(axis.title.x =element_text(size=20))+
  theme(legend.title = element_blank())+
  theme(axis.title.y =element_text(size=20))+
  theme(legend.text =element_text(size=20))+
  scale_color_manual(name="", labels = "D5-17 Staff Gauge", values ="red")


DV8<-ggplot(joined, mapping=aes(joined$Date, joined$D5.29.x, group=1, linetype='D5-29 Datalogger'))+
  geom_line()+
  geom_line(mapping=aes(joined$Date, joined$D5.30.x, group=1, linetype='D5-30 Datalogger'))+
  geom_point(mapping=aes(joined$Date, joined$D5.29.y, color="red"))+
  geom_hline(yintercept=0, linetype="dashed", color="grey")+
  labs(y="Depth (cm)", x="Date", size=3)+
  theme(plot.title=element_text(size=20, hjust=.5))+
  theme(axis.title.x =element_text(size=20))+
  theme(legend.title = element_blank())+
  theme(axis.title.y =element_text(size=20))+
  theme(legend.text =element_text(size=20))+
  scale_color_manual(name="", labels = "D5-29 Staff Gauge", values ="red")

DV9<-ggplot(joined, mapping=aes(joined$Date, joined$D5.30.x, group=1, linetype='D5-30 Datalogger'))+
  geom_line()+
  geom_line(mapping=aes(joined$Date, joined$D5.29.x, group=1, linetype='D5-29 Datalogger'))+
  geom_point(mapping=aes(joined$Date, joined$D5.30.y, color="red"))+
  geom_hline(yintercept=0, linetype="dashed", color="grey")+
  labs(y="Depth (cm)", x="Date", size=3)+
  theme(plot.title=element_text(size=20, hjust=.5))+
  theme(axis.title.x =element_text(size=20))+
  theme(legend.title = element_blank())+
  theme(axis.title.y =element_text(size=20))+
  theme(legend.text =element_text(size=20))+
  scale_color_manual(name="", labels = "D5-30 Staff Gauge", values ="red")


DV10<-ggplot(joined, mapping=aes(joined$Date, joined$D6.61.x, group=1, linetype='D6-61 Datalogger'))+
  geom_line()+
  geom_line(mapping=aes(joined$Date, joined$D6.37.x, group=1, linetype='D6-37 Datalogger'))+
  geom_point(mapping=aes(joined$Date, joined$D6.61.y, color="red"))+
  geom_hline(yintercept=0, linetype="dashed", color="grey")+
  labs(y="Depth (cm)", x="Date", size=3)+
  theme(plot.title=element_text(size=20, hjust=.5))+
  theme(axis.title.x =element_text(size=20))+
  theme(legend.title = element_blank())+
  theme(axis.title.y =element_text(size=20))+
  theme(legend.text =element_text(size=20))+
  scale_color_manual(name="", labels = "D6-61 Staff Gauge", values ="red")

DV11<-ggplot(joined, mapping=aes(joined$Date, joined$E5.04.x, group=1, linetype='E5.04 Datalogger'))+
  geom_line()+
  geom_line(mapping=aes(joined$Date, joined$D5.14.x, group=1, linetype='D5-14 Datalogger'))+
  geom_point(mapping=aes(joined$Date, joined$E5.04.y, color="red"))+
  geom_hline(yintercept=0, linetype="dashed", color="grey")+
  labs(y="Depth (cm)", x="Date", size=3)+
  theme(plot.title=element_text(size=20, hjust=.5))+
  theme(axis.title.x =element_text(size=20))+
  theme(legend.title = element_blank())+
  theme(axis.title.y =element_text(size=20))+
  theme(legend.text =element_text(size=20))+
  scale_color_manual(name="", labels = "E5-04 Staff Gauge", values ="red")

DV12<-ggplot(joined, mapping=aes(joined$Date, joined$E5.05.x, group=1, linetype='E5-05 Datalogger'))+
  geom_line()+
  geom_line(mapping=aes(joined$Date, joined$D6.61.x, group=1, linetype='D6-61 Datalogger'))+
  geom_point(mapping=aes(joined$Date, joined$E5.05.y, color="red"))+
  geom_hline(yintercept=0, linetype="dashed", color="grey")+
  labs(y="Depth (cm)", x="Date", size=3)+
  theme(plot.title=element_text(size=20, hjust=.5))+
  theme(axis.title.x =element_text(size=20))+
  theme(legend.title = element_blank())+
  theme(axis.title.y =element_text(size=20))+
  theme(legend.text =element_text(size=20))+
  scale_color_manual(name="", labels = "E5.05 Staff Gauge", values ="red")


DV13<-ggplot(joined, mapping=aes(joined$Date, joined$E5.30.x, group=1, linetype='E5-30 Datalogger'))+
  geom_line()+
  geom_line(mapping=aes(joined$Date, joined$D5.08.x, group=1, linetype='D5-08 Datalogger'))+
  geom_point(mapping=aes(joined$Date, joined$E5.30.y, color="red"))+
  geom_hline(yintercept=0, linetype="dashed", color="grey")+
  labs(y="Depth (cm)", x="Date", size=3)+
  theme(plot.title=element_text(size=20, hjust=.5))+
  theme(axis.title.x =element_text(size=20))+
  theme(legend.title = element_blank())+
  theme(axis.title.y =element_text(size=20))+
  theme(legend.text =element_text(size=20))+
  scale_color_manual(name="", labels = "E5-30 Staff Gauge", values ="red")


DV14<-ggplot(joined, mapping=aes(joined$Date, joined$D5.13.x, group=1, linetype='D5-13 Datalogger'))+
  geom_line()+
  geom_line(mapping=aes(joined$Date, joined$D5.03.x, group=1, linetype='D5-03 Datalogger'))+
  geom_point(mapping=aes(joined$Date, joined$D5.13.y, color="red"))+
  geom_hline(yintercept=0, linetype="dashed", color="grey")+
  labs(y="Depth (cm)", x="Date", size=3)+
  theme(plot.title=element_text(size=20, hjust=.5))+
  theme(axis.title.x =element_text(size=20))+
  theme(legend.title = element_blank())+
  theme(axis.title.y =element_text(size=20))+
  theme(legend.text =element_text(size=20))+
  scale_color_manual(name="", labels = "D5-13 Staff Gauge", values ="red")


DV15<-ggplot(joined, mapping=aes(joined$Date, joined$D5.25.x, group=1, linetype='D5-25 Datalogger'))+
  geom_line()+
  geom_line(mapping=aes(joined$Date, joined$D5.25.x, group=1, linetype='D5-25 Datalogger'))+
  geom_point(mapping=aes(joined$Date, joined$D5.25.y, color="red"))+
  geom_hline(yintercept=0, linetype="dashed", color="grey")+
  labs(y="Depth (cm)", x="Date", size=3)+
  theme(plot.title=element_text(size=20, hjust=.5))+
  theme(axis.title.x =element_text(size=20))+
  theme(legend.title = element_blank())+
  theme(axis.title.y =element_text(size=20))+
  theme(legend.text =element_text(size=20))+
  scale_color_manual(name="", labels = "D5-25 Staff Gauge", values ="red")


DV16<-ggplot(joined, mapping=aes(joined$Date, joined$E5.29.x, group=1, linetype='E5-29 Datalogger'))+
  geom_line()+
  geom_line(mapping=aes(joined$Date, joined$E5.29.x, group=1, linetype='E5-29 Datalogger'))+
  geom_point(mapping=aes(joined$Date, joined$E5.29.y, color="red"))+
  geom_hline(yintercept=0, linetype="dashed", color="grey")+
  labs(y="Depth (cm)", x="Date", size=3)+
  theme(plot.title=element_text(size=20, hjust=.5))+
  theme(axis.title.x =element_text(size=20))+
  theme(legend.title = element_blank())+
  theme(axis.title.y =element_text(size=20))+
  theme(legend.text =element_text(size=20))+
  scale_color_manual(name="", labels = "E5-29 Staff Gauge", values ="red")

DV17<-ggplot(joined, mapping=aes(joined$Date, joined$D6.37.x, group=1, linetype='D6-37 Datalogger'))+
  geom_line()+
  geom_line(mapping=aes(joined$Date, joined$C2.18.x, group=1, linetype='C2-18 Datalogger'))+
  geom_point(mapping=aes(joined$Date, joined$E5.29.y, color="red"))+
  geom_hline(yintercept=0, linetype="dashed", color="grey")+
  labs(y="Depth (cm)", x="Date", size=3)+
  theme(plot.title=element_text(size=20, hjust=.5))+
  theme(axis.title.x =element_text(size=20))+
  theme(legend.title = element_blank())+
  theme(axis.title.y =element_text(size=20))+
  theme(legend.text =element_text(size=20))+
  scale_color_manual(name="", labels = "D6-37 Staff Gauge", values ="red")

DV18<-ggplot(joined, mapping=aes(joined$Date, joined$C3.16.x, group=1, linetype='C3-16 Datalogger'))+
  geom_line()+
  geom_line(mapping=aes(joined$Date, joined$E5.29.x, group=1, linetype='E5-29 Datalogger'))+
  geom_point(mapping=aes(joined$Date, joined$C3.16.y, color="red"))+
  geom_hline(yintercept=0, linetype="dashed", color="grey")+
  labs(y="Depth (cm)", x="Date", size=3)+
  theme(plot.title=element_text(size=20, hjust=.5))+
  theme(axis.title.x =element_text(size=20))+
  theme(legend.title = element_blank())+
  theme(axis.title.y =element_text(size=20))+
  theme(legend.text =element_text(size=20))+
  scale_color_manual(name="", labels = "C3-16 Staff Gauge", values ="red")




DV_1<-grid.arrange(DV1, DV2, DV3, DV5, nrow=1) #3000x466
DV_2<-grid.arrange(DV6, DV7, DV8, DV9, nrow=1) #3000x466
DV_3<-grid.arrange(DV10, DV11, DV12, DV13, nrow=1) #3000x466
DV_4<-grid.arrange(DV17, DV18, DV15, DV16, nrow=1) #3000x466


#Stable inundation


DS1<-ggplot(joined, mapping=aes(joined$Date, joined$C2.14.x, group=1, linetype='C2-14 Datalogger'))+
  geom_line()+
  geom_line(mapping=aes(joined$Date, joined$D5.22.x, group=1, linetype='D5-22 Datalogger'))+
  geom_point(mapping=aes(joined$Date, joined$C2.14.y, color="red"))+
  geom_hline(yintercept=0, linetype="dashed", color="grey")+
  labs(y="Depth (cm)", x="Date", size=3)+
  theme(plot.title=element_text(size=20, hjust=.5))+
  theme(axis.title.x =element_text(size=20))+
  theme(legend.title = element_blank())+
  theme(axis.title.y =element_text(size=20))+
  theme(legend.text =element_text(size=20))+
  scale_color_manual(name="", labels = "C2-14 Staff Gauge", values ="red")


DS2<-ggplot(joined, mapping=aes(joined$Date, joined$C2.17.x, group=1, linetype='C2-17 Datalogger'))+
  geom_line()+
  geom_line(mapping=aes(joined$Date, joined$E5.27.x, group=1, linetype='E5-27 Datalogger'))+
  geom_point(mapping=aes(joined$Date, joined$C2.17.y, color="red"))+
  geom_hline(yintercept=0, linetype="dashed", color="grey")+
  labs(y="Depth (cm)", x="Date", size=3)+
  theme(plot.title=element_text(size=20, hjust=.5))+
  theme(axis.title.x =element_text(size=20))+
  theme(legend.title = element_blank())+
  theme(axis.title.y =element_text(size=20))+
  theme(legend.text =element_text(size=20))+
  scale_color_manual(name="", labels = "C2-17 Staff Gauge", values ="red")


DS3<-ggplot(joined, mapping=aes(joined$Date, joined$C2.18.x, group=1, linetype='C2-18 Datalogger'))+
  geom_line()+
  geom_line(mapping=aes(joined$Date, joined$D5.14.x, group=1, linetype='D5-14 Datalogger'))+
  geom_point(mapping=aes(joined$Date, joined$C2.18.y, color="red"))+
  geom_hline(yintercept=0, linetype="dashed", color="grey")+
  labs(y="Depth (cm)", x="Date", size=3)+
  theme(plot.title=element_text(size=20, hjust=.5))+
  theme(axis.title.x =element_text(size=20))+
  theme(legend.title = element_blank())+
  theme(axis.title.y =element_text(size=20))+
  theme(legend.text =element_text(size=20))+
  scale_color_manual(name="", labels = "C2-18 Staff Gauge", values ="red")



DS4<-ggplot(joined, mapping=aes(joined$Date, joined$D5.01.x, group=1, linetype='D5-01 Datalogger'))+
  geom_line()+
  geom_line(mapping=aes(joined$Date, joined$D5.22.x, group=1, linetype='D5-22 Datalogger'))+
  geom_point(mapping=aes(joined$Date, joined$D5.01.y, color="red"))+
  geom_hline(yintercept=0, linetype="dashed", color="grey")+
  labs(y="Depth (cm)", x="Date", size=3)+
  theme(plot.title=element_text(size=20, hjust=.5))+
  theme(axis.title.x =element_text(size=20))+
  theme(legend.title = element_blank())+
  theme(axis.title.y =element_text(size=20))+
  theme(legend.text =element_text(size=20))+
  scale_color_manual(name="", labels = "D5-01 Staff Gauge", values ="red")


DS5<-ggplot(joined, mapping=aes(joined$Date, joined$D5.13.x, group=1, linetype='D5-13 Datalogger'))+
  geom_line()+
  geom_line(mapping=aes(joined$Date, joined$D5.03.x, group=1, linetype='D5-03 Datalogger'))+
  geom_point(mapping=aes(joined$Date, joined$D5.13.y, color="red"))+
  geom_hline(yintercept=0, linetype="dashed", color="grey")+
  labs(y="Depth (cm)", x="Date", size=3)+
  theme(plot.title=element_text(size=20, hjust=.5))+
  theme(axis.title.x =element_text(size=20))+
  theme(legend.title = element_blank())+
  theme(axis.title.y =element_text(size=20))+
  theme(legend.text =element_text(size=20))+
  scale_color_manual(name="", labels = "D5-03 Staff Gauge", values ="red")

DS6<-ggplot(joined, mapping=aes(joined$Date, joined$D5.10.x, group=1, linetype='D5-10 Datalogger'))+
  geom_line()+
  geom_line(mapping=aes(joined$Date, joined$D5.03.x, group=1, linetype='D5-03 Datalogger'))+
  geom_point(mapping=aes(joined$Date, joined$D5.10.y, color="red"))+
  geom_hline(yintercept=0, linetype="dashed", color="grey")+
  labs(y="Depth (cm)", x="Date", size=3)+
  theme(plot.title=element_text(size=20, hjust=.5))+
  theme(axis.title.x =element_text(size=20))+
  theme(legend.title = element_blank())+
  theme(axis.title.y =element_text(size=20))+
  theme(legend.text =element_text(size=20))+
  scale_color_manual(name="", labels = "D5-10 Staff Gauge", values ="red")+
  scale_linetype_manual(labels=c("D5-10 Datalogger","D5-03 Datalogger"), values=c("solid", "dashed"))

DS7<-ggplot(joined, mapping=aes(joined$Date, joined$D5.15.x, group=1, linetype='D5-15 Datalogger'))+
  geom_line()+
  geom_line(mapping=aes(joined$Date, joined$D5.39.x, group=1, linetype='D5-39 Datalogger'))+
  geom_point(mapping=aes(joined$Date, joined$D5.15.y, color="red"))+
  geom_hline(yintercept=0, linetype="dashed", color="grey")+
  labs(y="Depth (cm)", x="Date", size=3)+
  theme(plot.title=element_text(size=20, hjust=.5))+
  theme(axis.title.x =element_text(size=20))+
  theme(legend.title = element_blank())+
  theme(axis.title.y =element_text(size=20))+
  theme(legend.text =element_text(size=20))+
  scale_color_manual(name="", labels = "D5-15 Staff Gauge", values ="red")


DS8<-ggplot(joined, mapping=aes(joined$Date, joined$D5.22.x, group=1, linetype='D5-22 Datalogger'))+
  geom_line()+
  geom_line(mapping=aes(joined$Date, joined$D5.03.x, group=1, linetype='D5-03 Datalogger'))+
  geom_point(mapping=aes(joined$Date, joined$D5.22.y, color="red"))+
  geom_hline(yintercept=0, linetype="dashed", color="grey")+
  labs(y="Depth (cm)", x="Date", size=3)+
  theme(plot.title=element_text(size=20, hjust=.5))+
  theme(axis.title.x =element_text(size=20))+
  theme(legend.title = element_blank())+
  theme(axis.title.y =element_text(size=20))+
  theme(legend.text =element_text(size=20))+
  scale_color_manual(name="", labels = "D5-22 Staff Gauge", values ="red")+
  scale_linetype_manual(labels=c("D5-22 Datalogger","D5-03 Datalogger"), values=c("solid", "dashed"))



DS10<-ggplot(joined, mapping=aes(joined$Date, joined$C2.10.x, group=1, linetype='C2-10 Datalogger'))+
  geom_line()+
  geom_line(mapping=aes(joined$Date, joined$C2.17.x, group=1, linetype='C2-17 Datalogger'))+
  geom_point(mapping=aes(joined$Date, joined$C2.10.y, color="red"))+
  geom_hline(yintercept=0, linetype="dashed", color="grey")+
  labs(y="Depth (cm)", x="Date", size=3)+
  theme(plot.title=element_text(size=20, hjust=.5))+
  theme(axis.title.x =element_text(size=20))+
  theme(legend.title = element_blank())+
  theme(axis.title.y =element_text(size=20))+
  theme(legend.text =element_text(size=20))+
  scale_color_manual(name="", labels = "C2-10 Staff Gauge", values ="red")


DS11<-ggplot(joined, mapping=aes(joined$Date, joined$D6.40.x, group=1, linetype='D6-40 Datalogger'))+
  geom_line()+
  geom_line(mapping=aes(joined$Date, joined$C2.17.x, group=1, linetype='C2-17 Datalogger'))+
  geom_point(mapping=aes(joined$Date, joined$D6.40.y, color="red"))+
  geom_hline(yintercept=0, linetype="dashed", color="grey")+
  labs(y="Depth (cm)", x="Date", size=3)+
  theme(plot.title=element_text(size=20, hjust=.5))+
  theme(axis.title.x =element_text(size=20))+
  theme(legend.title = element_blank())+
  theme(axis.title.y =element_text(size=20))+
  theme(legend.text =element_text(size=20))+
  scale_color_manual(name="", labels = "D6-40 Staff Gauge", values ="red")+
  scale_linetype_manual(labels=c("D6-40 Datalogger","C2-17 Datalogger"), values=c("solid", "dashed"))

DS12<-ggplot(joined, mapping=aes(joined$Date, joined$E5.27.x, group=1, linetype='E5-27 Datalogger'))+
  geom_line()+
  geom_line(mapping=aes(joined$Date, joined$D5.39.x, group=1, linetype='D5-39 Datalogger'))+
  geom_point(mapping=aes(joined$Date, joined$E5.27.y, color="red"))+
  geom_hline(yintercept=0, linetype="dashed", color="grey")+
  labs(y="Depth (cm)", x="Date", size=3)+
  theme(plot.title=element_text(size=20, hjust=.5))+
  theme(axis.title.x =element_text(size=20))+
  theme(legend.title = element_blank())+
  theme(axis.title.y =element_text(size=20))+
  theme(legend.text =element_text(size=20))+
  scale_color_manual(name="", labels = "E5-27 Staff Gauge", values ="red")+
  scale_linetype_manual(labels=c("E5-27 Datalogger","D5-39 Datalogger"), values=c("solid", "dashed"))


DS13<-ggplot(joined, mapping=aes(joined$Date, joined$E5.31.x, group=1, linetype='E5-31 Datalogger'))+
  geom_line()+
  geom_line(mapping=aes(joined$Date, joined$C2.19, group=1, linetype='C2-19 Datalogger'))+
  geom_point(mapping=aes(joined$Date, joined$E5.31.y, color="red"))+
  geom_hline(yintercept=0, linetype="dashed", color="grey")+
  labs(y="Depth (cm)", x="Date", size=3)+
  theme(plot.title=element_text(size=20, hjust=.5))+
  theme(axis.title.x =element_text(size=20))+
  theme(legend.title = element_blank())+
  theme(axis.title.y =element_text(size=20))+
  theme(legend.text =element_text(size=20))+
  scale_color_manual(name="", labels = "E5-31 Staff Gauge", values ="red")+
  scale_linetype_manual(labels=c("E5-31 Datalogger","C2-19 Datalogger"), values=c("solid", "dashed"))



DS14<-ggplot(joined, mapping=aes(joined$Date, joined$D5.39.x, group=1, linetype='D5-39 Datalogger'))+
  geom_line()+
  geom_line(mapping=aes(joined$Date, joined$D5.15.x, group=1, linetype='D5-15 Datalogger'))+
  geom_point(mapping=aes(joined$Date, joined$D5.39.y, color="red"))+
  labs(y="Depth (cm)", x="Date", size=3)+
  theme(plot.title=element_text(size=20, hjust=.5))+
  theme(axis.title.x =element_text(size=20))+
  theme(legend.title = element_blank())+
  theme(axis.title.y =element_text(size=20))+
  theme(legend.text =element_text(size=20))+
  scale_color_manual(name="", labels = "D5-39 Staff Gauge", values ="red")+
  scale_linetype_manual(labels=c("D5-39 Datalogger","D5-15 Datalogger"), values=c("solid", "dashed"))



DS_1<-grid.arrange(DS1, DS2, DS3, DS4, nrow=1) #3000x466
DS_2<-grid.arrange(DS5, DS6, DS7, DS8, nrow=1)
DS_3<-grid.arrange(DS10, DS11, DS12, DS13, nrow=1)
DS_4<-grid.arrange(DS14, nrow=1)





# Counting drydown/wetup events -------------------------------------------
Pool.IDs<-dl_days_2018$Pool.ID
Pool.IDs<-Pool.IDs[2:32]
Pool.IDs<-Pool.IDs[-5]
drydowns<-c(1,0,1,0,2,2,0,0,0,4,0,2,0,0,0,0,0,5,0,0,0,0,0,2,0,1,1,2,4,0)
drydown_count<-tibble(Pool.ID=Pool.IDs, drydown=drydowns)

drydown<-ggplot(drydown_count)+
geom_histogram(aes(x=drydown), binwidth=1)+
  labs(y="Count", x="# Wet-up/Dry-down events missed by weekly staff gauge sampling")+
  theme(plot.title=element_text(size=30, hjust=.5))+
  theme(axis.title.x =element_text(size=30))+
  theme(axis.title.y =element_text(size=30))+
  theme(axis.text.x =element_text(size=30))+
  theme(axis.text.y =element_text(size=30))+
  theme(legend.title =element_text(size=30))+
  theme(legend.text =element_text(size=30))





