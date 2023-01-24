rm(list=ls())
setwd('/Users/fabianyii/Desktop/ZOC-BHVI/')

# install.packages("ggpubr")
# install.packages('lm.beta') # compute standardised beta coefficient
# install.packages('lme4')
# install.packages('lmerTest')
# install.packages('report')
# install.packages('viridis')

library('lm.beta')
library('lme4')
library('nlme')
library('lmerTest')
library('ggplot2')
library('car')
library('report')
library('viridis')
library('gridExtra')
library('lmtest')
library('sjPlot') #for plotting lmer and glmer mods
library('effects')
library('hrbrthemes')
library('grid')

## Wide format: OD & OS
OD <- read.csv('data/OD.csv'); OS <- read.csv('data/OS.csv'); d_wide <- read.csv('data/OD.csv')
# Check correlation between both eyes
ind <- c(9:38) # indices for data of interest
for(i in ind){
  print(paste(names(OD)[i], ': ', 
        round(as.numeric(cor.test(OD[,i], OS[,i])$estimate),2))) }
# Replace missing data on one day with data from the fellow eye
for(i in ind){
  print(paste('Replace OD with OS: ', names(OD)[i]))
  OD[,i] <- ifelse(is.na(OD[,i]), OS[,i], OD[,i] )
  print(paste('Replace OS with OD: ', names(OD)[i]))
  OS[,i] <- ifelse(is.na(OS[,i]), OD[,i], OS[,i]) }
# Average data from both eyes
d_wide[,ind] <- (OD[,ind]+OS[,ind])/2

# Convert to long format
y0 <- d_wide[c('id', 'sex', 'y0_age', 'y0_iop', 'y0_al', 'y0_se', 'y0_cct', 'y0_acd', 'y0_lt', 'age_group')]
y2 <- d_wide[c('id', 'sex', 'y2_age', 'y2_iop', 'y2_al', 'y2_se', 'y2_cct', 'y2_acd', 'y2_lt', 'age_group')]
y4 <- d_wide[c('id', 'sex', 'y4_age', 'y4_iop', 'y4_al', 'y4_se', 'y4_cct', 'y4_acd', 'y4_lt', 'age_group')]
y6 <- d_wide[c('id', 'sex', 'y6_age', 'y6_iop', 'y6_al', 'y6_se', 'y6_cct', 'y6_acd', 'y6_lt', 'age_group')]
y8 <- d_wide[c('id', 'sex', 'y8_age', 'y8_iop', 'y8_al', 'y8_se', 'y8_cct', 'y8_acd', 'y8_lt', 'age_group')]
names(y0)[3:9] <- c('age', 'iop', 'al', 'se', 'cct', 'acd', 'lt')
names(y2)[3:9] <- c('age', 'iop', 'al', 'se', 'cct', 'acd', 'lt')
names(y4)[3:9] <- c('age', 'iop', 'al', 'se', 'cct', 'acd', 'lt')
names(y6)[3:9] <- c('age', 'iop', 'al', 'se', 'cct', 'acd', 'lt')
names(y8)[3:9] <- c('age', 'iop', 'al', 'se', 'cct', 'acd', 'lt')
y0$tp <- 0
y2$tp <- 2
y4$tp <- 4
y6$tp <- 6
y8$tp <- 8
y0$b_iop <- y0$iop
y2$b_iop <- y0$iop
y4$b_iop <- y0$iop
y6$b_iop <- y0$iop
y8$b_iop <- y0$iop
y0$b_age <- y0$age
y2$b_age <- y0$age
y4$b_age <- y0$age
y6$b_age <- y0$age
y8$b_age <- y0$age
y0$b_al <- y0$al
y2$b_al <- y0$al
y4$b_al <- y0$al
y6$b_al <- y0$al
y8$b_al <- y0$al
# long format
d_long <- rbind(y0, y2, y4, y6, y8)
d_long <- d_long[order(d_long$id),]
# function to median centre all continuous variables of interest
med <- function(var){var - median(var, na.rm=TRUE)}

# Study sample: baseline age between 7 and 12, and follow-up age censored at 16
data <- subset(d_long, age_group==1 & age<=15)

# Annual rate of change in AL computed over 2 years
data$al_next_2y <- NA
for(i in unique(data$id)){
  d <- data[which(data$id==i),]
  if(nrow(d)!=1){ data[which(data$id==i),]$al_next_2y[1:nrow(d)-1] <- d$al[2:nrow(d)] } }
data$rate_over_2y <- (data$al_next_2y - data$al)/2 # compute annual rate of change

## data cleaning ##
# ID "MYP-000337" a large negative annual rate (which is implausible; let's probe further)
hist(data$rate_over_2y) # distribution of annual rates
data[which(data$rate_over_2y < -0.2),]$id
# change Al at TP 2 to NA because the shrinking AL is highly likely to be data entry error
data[which(data$id=='MYP-000337' & data$tp==2),]$al <- NA
for(i in unique(data$id)){
  d <- data[which(data$id==i),]
  if(nrow(d)!=1){ data[which(data$id==i),]$al_next_2y[1:nrow(d)-1] <- d$al[2:nrow(d)] } }
data$rate_over_2y <- (data$al_next_2y - data$al)/2 # compute annual rate of change
hist(data$rate_over_2y) # viualise distribution of annual rates after data cleaning
# IDs where annual rate > 0.6mm/y were also checked; data entry error not suspected

### Annual rate of change in AL computed over 4 years ###
data$al_next_4y <- NA
for(i in unique(data$id)){
  d <- data[which(data$id==i),]
  if(nrow(d)==3){ data[which(data$id==i),]$al_next_2y[1] <- d$al[3] }
  if(nrow(d)!=1 & nrow(d)!=2 & nrow(d)!=3){ data[which(data$id==i),]$al_next_4y[1:(nrow(d)-2)] <- d$al[3:nrow(d)] } }
data$rate_over_4y <- (data$al_next_4y - data$al)/4 # compute annual rate of change
hist(data$rate_over_4y)


# multivariable model 1: contemporaneous (cross-sectional) effect of IOP on AL, adjusting for potential confounders 
m1 <- lme(al~med(iop)*med(age) + med(cct) + med(acd) + med(lt) + factor(sex), random=~1+med(age)|id, na.action=na.omit, data=data)
tab_model(m1, pred.labels=c('Intercept', 'IOP', 'Age', 'CCT', 'ACD', 'LT', 'Female', 'IOP x Age'), dv.labels='AL')
# multivariable model 2: annual rate of change in AL (computed over 2 years) vs IOP at the preceding timepoint, adjusting for potential confounders
m2 <- lme(rate_over_2y~med(iop)*med(age) + med(al) + med(cct) + med(acd) + med(lt) + factor(sex), random=~1+med(age)|id, na.action=na.omit, data=data)
tab_model(m2, pred.labels=c('Intercept', 'IOP', 'AL', 'Age', 'CCT', 'ACD', 'LT', 'Female', 'IOP x Age'), dv.labels='Rate (mm/y)')
# multivariable model 3: annual rate of change in AL (computed over 4 years) vs IOP at the preceding timepoint, adjusting for potential confounders
m3 <- lme(rate_over_4y~med(iop)*med(age) + med(al) + med(cct) + med(acd) + med(lt) + factor(sex), random=~1|id, na.action=na.omit, data=data)
tab_model(m3, pred.labels=c('Intercept', 'IOP', 'AL', 'Age', 'CCT', 'ACD', 'LT', 'Female', 'IOP x Age'), dv.labels='Rate (mm/y)')

### Individual and average AL trajectories ###
# group individuals into 3 baseline age groups
data$age_group<-'9 to 10'
data[which(round(data$b_age,0)<=8),]$age_group<-'7 to 8'
data[which(round(data$b_age,0)>=11),]$age_group<-'11 to 12'
data$age_group <- factor(data$age_group, levels=c('7 to 8', '9 to 10', '11 to 12'))
m1 <- lme(al~med(iop)*med(age) + med(cct) + med(acd) + med(lt) + sex, random=~1+med(age)|id, na.action=na.omit, data=data)
age_effects <- as.data.frame(effect(term="med(age)", mod=m1))
# plot AL vs age
ggplot(age_effects, aes(x=age, y=fit)) + 
  # geom_ribbon(aes(min=lower, max=upper), alpha=0.3, colour=NA) + 
  # geom_line(size=1.5, colour='black') +
  geom_point(data=data, aes(x=age, y=al, group=id, colour=age_group), size=3, alpha=0.3) + 
  geom_smooth(data=data, aes(x=age, y=al, group=id, colour=age_group), method='lm', se=FALSE, size=0.15) +
  scale_color_discrete('Baseline Age (y)') + labs(x='Age (y)', y='AL (mm)', title='Individual AL trajectories') + 
  theme_ipsum(ticks=TRUE) +
  scale_x_continuous(breaks=seq(7,15,1), labels=seq(7,15,1)) +
  scale_y_continuous(limits=c(24,32), breaks=seq(24,32,1), labels=seq(24,32,1)) +
  theme(legend.position='bottom', 
        plot.title=element_text(hjust=0.5),
        plot.background=element_rect(fill="#fbf9f4", color="#fbf9f4"),
        panel.background=element_rect(fill="#fbf9f4", color="#fbf9f4"),
        panel.grid.minor=element_blank(), 
        panel.grid.major=element_blank())
# Plot annual rate of change vs age at preceding timepoint
m2 <- lme(rate_over_2y~med(iop)*med(age) + med(al) + med(cct) + med(acd) + med(lt) + sex, random=~1+med(age)|id, na.action=na.omit, data=data)
age_effects <- as.data.frame(effect(term="med(age)", mod=m2))
ggplot(age_effects, aes(x=age, y=fit)) + 
  geom_ribbon(aes(min=lower, max=upper), alpha=0.3, colour=NA) + 
  geom_line(size=1.5, colour='black') +
  geom_point(data=data, aes(y=rate_over_2y, ), size=2, alpha=0.5, colour='black') + 
  theme_ipsum(ticks=TRUE) +
  ylim(c(-0.1,0.8)) +
  xlim(c(7,13)) +
  theme(plot.background=element_rect(fill="#fbf9f4", color="#fbf9f4"),
        panel.background=element_rect(fill="#fbf9f4", color="#fbf9f4")) +
  labs(x='Age (y)', y='Rate (mm/y)', title='Annual rate of change in AL vs age')
  
### Contemperaneous effect of IOP on AL while adjusting for all other covariates ###
iop_effects <- as.data.frame(effect(term= "med(iop)", mod= m1))
p1 <- ggplot(iop_effects, aes(iop)) +
  geom_ribbon(aes(min=lower, max=upper), alpha=0.1) +
  geom_line(aes(y=fit), size=1.3) +
  # geom_point(data=data, aes(y=al), size=3, alpha=0.2) +
  labs(x='', y='', subtitle='Contemperaneous effect of IOP on AL') + 
  theme_ipsum(ticks=TRUE) +
  scale_y_continuous(limits=c(27,28), breaks=seq(27,28,0.2), labels=seq(27,28,0.2)) +
  theme(legend.position="right", 
        panel.grid.major = element_blank(),
        legend.background=element_blank(),
        plot.background=element_rect(fill='#fbf9f4', color='#fbf9f4'),
        panel.background=element_rect(fill='#fbf9f4', color='#fbf9f4'),
        plot.margin = unit(c(5.5, 5.5, 3, 5.5), 'pt'))
# Interaction between IOP and age on AL
iop_effects <- as.data.frame(effect(term= "med(iop)*med(age)", mod= m1))
plot_grid <- function(rounded_age, col){
  ggplot(subset(iop_effects, age==rounded_age), aes(iop)) +
    geom_point(data=subset(data, round(age)==rounded_age), aes(y=al), size=4, alpha=0.2, colour=col) +
    geom_line(aes(y=fit), size=1.8, colour=col) +
    theme_ipsum(ticks=TRUE) +
    geom_ribbon(aes(min=lower, max=upper), alpha=0.3) +
    labs(y='', x='', subtitle=paste('Age', rounded_age)) +
    scale_y_continuous(limits=c(24,32), breaks=seq(24,32,2), labels=seq(24,32,2)) +
    scale_x_continuous(limits=c(10,23), breaks=seq(10,23,3), labels=seq(10,23,3)) +
    theme(panel.grid.minor = element_blank(),
          plot.background=element_rect(fill='#fbf9f4', color='#fbf9f4'),
          panel.background=element_rect(fill='#fbf9f4', color='#fbf9f4'),
          plot.subtitle=element_text(colour=col, face='bold'),
          plot.margin = unit(c(10, 3, 5.5, 3), 'pt'))  }
p9 <- plot_grid(9, col='#F8766D')
p11 <- plot_grid(11, col='#00BA38') + theme(axis.text.y=element_blank(), axis.ticks.y=element_blank())
p13 <- plot_grid(13, col='#A58AFF') + theme(axis.text.y=element_blank(), axis.ticks.y=element_blank())
p15 <- plot_grid(15, col='#00A9FF') + theme(axis.text.y=element_blank(), axis.ticks.y=element_blank())
bottom <- textGrob('IOP (mmHg)', gp=gpar(fontsize=10, fontface='bold'))
left <- textGrob('AL (mm)', gp=gpar(fontsize=10, fontface='bold'), rot=90)
grid.arrange(p1, p9, p11, p13, p15, bottom=bottom, left=left, heights=c(1,1.3), layout_matrix=rbind( c(1,1,1,1), 
                                                                                          c(2,3,4,5) ))



### rate of change in AL vs IOP adjusting for age, AL, CCT, LT, etc. ###
iop_effects <- as.data.frame(effect(term= "med(iop)", mod= m2))
p2 <- ggplot(data=iop_effects, aes(x=iop, y=fit)) + 
  labs(x='IOP (mmHg)', y='Rate (mm/y)', subtitle='Annual rate of change in AL vs IOP at preceding timepoint') +
  theme_ipsum(ticks=TRUE) +
  geom_point(data=data, aes(y=rate_over_2y), alpha=0.2, size=3) +
  geom_line(aes(y = fit), size=1.2) +
  geom_ribbon(aes(min=lower, max=upper), alpha=0.1) +
  # geom_smooth(data=data, aes(y=rate, group=id), method='lm', se=FALSE, size=0.8) +
  theme(plot.background=element_rect(fill="#fbf9f4", color="#fbf9f4"),
        panel.background=element_rect(fill="#fbf9f4", color="#fbf9f4"),
        panel.grid.minor = element_blank())

iop_effects <- as.data.frame(effect(term= "med(iop)*med(age)", mod= m2))
plot_grid <- function(rounded_age, col){
  ggplot(subset(iop_effects, round(age,0)==rounded_age), aes(iop)) +
    geom_point(data=subset(data, round(age)==rounded_age), aes(y=rate_over_2y), size=4, alpha=0.2, colour=col) +
    geom_line(aes(y=fit), size=1.8, colour=col) +
    theme_ipsum(ticks=TRUE) +
    geom_ribbon(aes(min=lower, max=upper), alpha=0.3) +
    labs(y='', x='', subtitle=paste('Age', rounded_age)) +
    # scale_y_continuous(limits=c(24,32), breaks=seq(24,32,2), labels=seq(24,32,2)) +
    scale_x_continuous(limits=c(10,23), breaks=seq(10,23,3), labels=seq(10,23,3)) +
    theme(panel.grid.minor = element_blank(),
          plot.background=element_rect(fill='#fbf9f4', color='#fbf9f4'),
          panel.background=element_rect(fill='#fbf9f4', color='#fbf9f4'),
          plot.subtitle=element_text(colour=col, face='bold'),
          plot.margin = unit(c(10, 3, 5.5, 3), 'pt'))  }
p9 <- plot_grid(9, col='#F8766D')
p13 <- plot_grid(13, col='#A58AFF') + theme(axis.text.y=element_blank(), axis.ticks.y=element_blank())
bottom <- textGrob('IOP (mmHg)', gp=gpar(fontsize=10, fontface='bold'))
left <- textGrob('Rate (mm/y)', gp=gpar(fontsize=10, fontface='bold'), rot=90)
grid.arrange(p2, p9, p13, bottom=bottom, left=left, heights=c(1,1.3), layout_matrix=rbind( c(1,1), 
                                                                                           c(2,3) ))







# al <- function(iop, age, sex){
#   21.360643996 + 0.019072*iop + 0.255930*round(age,0) - 0.376298*(sex==2) - 0.001893218*median(data$cct, na.rm=TRUE) + 0.435260767*median(data$acd, na.rm=TRUE) + 0.850721348*median(data$lt, na.rm=TRUE)}
# plot_dat <- function(rounded_age, xlab=''){
#   sub <- subset(data, round(age,0)==rounded_age)
#   min_iop <- min(sub$iop,na.rm=TRUE)
#   max_iop <- max(sub$iop,na.rm=TRUE)
#   ggplot(sub) + 
#     geom_point(aes(x=iop, y=al, colour=factor(sex)), size=3, alpha=0.5) +
#     labs(x=xlab, y='', subtitle=paste(rounded_age,'y')) +
#     ylim(c(24,32)) + 
#     theme_ipsum(ticks=TRUE) +
#     scale_x_continuous(breaks=seq(11,21,2), labels=seq(11,21,2), 
#                        limits=c(11, 21, 2) ) +
#     geom_line(aes(x=iop, y=al(iop, rounded_age, 1) ), colour='red', size=1.2) +
#     geom_line(aes(x=iop, y=al(iop, rounded_age, 2) ), colour='blue', size=1.2) +
#     theme(plot.margin = unit(c(3, 5.5, 3, 5.5), 'pt'),
#           legend.position='none') }
# p8 <- plot_dat(8)
# p9 <- plot_dat(9)
# p10 <- plot_dat(10)
# p11 <- plot_dat(11)
# p12 <- plot_dat(12)
# p13 <- plot_dat(13)
# p14 <- plot_dat(14)
# p15 <- plot_dat(15)
# grid.arrange(p8, p9, p10, p11, p12, p13, p14, p15, nrow=2, ncol=4)
# 
# ggplot(subset(data, round(age,0)==12 & sex==2)) + 
#   geom_point(aes(x=iop, y=al)) + geom_line(aes(x=iop, y=al(iop, age, sex)))
# d=subset(sub, round(age,0)==11 & Sex=='Male')
# plot(d$iop, al(d$iop, d$age, d$Sex))


# # multivariable model 2: time-lagged effect of IOP (at TP) on AL (at TP+1) and the rate of change in AL, 
# # adjusting for other covariates (at TP) and their interaction with time
# m2 <- lmer(al_next_2y~med(iop)*tp + med(b_age)*tp + med(cct)*tp + med(acd)*tp + med(lt)*tp + factor(sex)*tp + (1+tp|id), data )
# tab_model(m2,
#           pred.labels=c('Intercept', 'IOP', 'Year (timepoint)', 'Baseline age', 'CCT',
#                         'ACD', 'LT', 'Female', 'IOP x Year', 'Baseline age x Year', 
#                         'CCT x Year', 'ACD x Year', 'LT x Year', 'Female x Year'),
#           dv.labels='AL at the next timepoint')
# # multivariable model 3: same as model 2 BUT adjust for AL at the same TP as IOP 
# m3 <- lmer(al_next_2y~med(iop)*tp + med(al)*tp + med(b_age)*tp + med(cct)*tp + med(acd)*tp + med(lt)*tp + factor(sex)*tp + (1+tp|id), data )
# tab_model(m3,
#           pred.labels=c('Intercept', 'IOP', 'Year (timepoint)', 'AL', 'Baseline age', 'CCT',
#                         'ACD', 'LT', 'Female', 'IOP x Year', 'AL x Year', 'Baseline age x Year', 
#                         'CCT x Year', 'ACD x Year', 'LT x Year', 'Female x Year'),
#           dv.labels='AL at the next timepoint')
# 
# m3=lmer(rate~iop + age + al + cct + acd + lt + sex + (1+age|id), data,
#         control = lmerControl(optimizer ="Nelder_Mead"))
# tab_model(m3)







# # Wide format
# OD <- read.csv('data/OD.csv'); OS <- read.csv('data/OS.csv'); d_wide <- read.csv('data/OD.csv')
# # Convert to long format
# y0_OD <- OD[c('id', 'sex', 'y0_age', 'y0_iop', 'y0_al', 'y0_se', 'y0_cct', 'y0_acd', 'y0_lt', 'age_group')]
# y2_OD <- OD[c('id', 'sex', 'y2_age', 'y2_iop', 'y2_al', 'y2_se', 'y2_cct', 'y2_acd', 'y2_lt', 'age_group')]
# y4_OD <- OD[c('id', 'sex', 'y4_age', 'y4_iop', 'y4_al', 'y4_se', 'y4_cct', 'y4_acd', 'y4_lt', 'age_group')]
# y6_OD <- OD[c('id', 'sex', 'y6_age', 'y6_iop', 'y6_al', 'y6_se', 'y6_cct', 'y6_acd', 'y6_lt', 'age_group')]
# y8_OD <- OD[c('id', 'sex', 'y8_age', 'y8_iop', 'y8_al', 'y8_se', 'y8_cct', 'y8_acd', 'y8_lt', 'age_group')]
# y0_OS <- OS[c('id', 'sex', 'y0_age', 'y0_iop', 'y0_al', 'y0_se', 'y0_cct', 'y0_acd', 'y0_lt', 'age_group')]
# y2_OS <- OS[c('id', 'sex', 'y2_age', 'y2_iop', 'y2_al', 'y2_se', 'y2_cct', 'y2_acd', 'y2_lt', 'age_group')]
# y4_OS <- OS[c('id', 'sex', 'y4_age', 'y4_iop', 'y4_al', 'y4_se', 'y4_cct', 'y4_acd', 'y4_lt', 'age_group')]
# y6_OS <- OS[c('id', 'sex', 'y6_age', 'y6_iop', 'y6_al', 'y6_se', 'y6_cct', 'y6_acd', 'y6_lt', 'age_group')]
# y8_OS <- OS[c('id', 'sex', 'y8_age', 'y8_iop', 'y8_al', 'y8_se', 'y8_cct', 'y8_acd', 'y8_lt', 'age_group')]
# names(y0_OD)[3:9] <- c('age', 'iop', 'al', 'se', 'cct', 'acd', 'lt'); names(y0_OS)[3:9] <- c('age', 'iop', 'al', 'se', 'cct', 'acd', 'lt')
# names(y2_OD)[3:9] <- c('age', 'iop', 'al', 'se', 'cct', 'acd', 'lt'); names(y2_OS)[3:9] <- c('age', 'iop', 'al', 'se', 'cct', 'acd', 'lt')
# names(y4_OD)[3:9] <- c('age', 'iop', 'al', 'se', 'cct', 'acd', 'lt'); names(y4_OS)[3:9] <- c('age', 'iop', 'al', 'se', 'cct', 'acd', 'lt')
# names(y6_OD)[3:9] <- c('age', 'iop', 'al', 'se', 'cct', 'acd', 'lt'); names(y6_OS)[3:9] <- c('age', 'iop', 'al', 'se', 'cct', 'acd', 'lt')
# names(y8_OD)[3:9] <- c('age', 'iop', 'al', 'se', 'cct', 'acd', 'lt'); names(y8_OS)[3:9] <- c('age', 'iop', 'al', 'se', 'cct', 'acd', 'lt')
# y0_OD$tp <- 0; y0_OS$tp <- 0
# y2_OD$tp <- 2; y2_OS$tp <- 2
# y4_OD$tp <- 4; y4_OS$tp <- 4
# y6_OD$tp <- 6; y6_OS$tp <- 6
# y8_OD$tp <- 8; y8_OS$tp <- 8
# y0_OD$b_iop <- y0_OD$iop; y0_OS$b_iop <- y0_OS$iop
# y2_OD$b_iop <- y0_OD$iop; y2_OS$b_iop <- y0_OS$iop
# y4_OD$b_iop <- y0_OD$iop; y4_OS$b_iop <- y0_OS$iop
# y6_OD$b_iop <- y0_OD$iop; y6_OS$b_iop <- y0_OS$iop
# y8_OD$b_iop <- y0_OD$iop; y8_OS$b_iop <- y0_OS$iop
# y0_OD$b_age <- y0_OD$age; y0_OS$b_age <- y0_OS$age
# y2_OD$b_age <- y0_OD$age; y2_OS$b_age <- y0_OS$age
# y4_OD$b_age <- y0_OD$age; y4_OS$b_age <- y0_OS$age
# y6_OD$b_age <- y0_OD$age; y6_OS$b_age <- y0_OS$age
# y8_OD$b_age <- y0_OD$age; y8_OS$b_age <- y0_OS$age
# y0_OD$b_al <- y0_OD$al; y0_OS$b_al <- y0_OS$al
# y2_OD$b_al <- y0_OD$al; y2_OS$b_al <- y0_OS$al
# y4_OD$b_al <- y0_OD$al; y4_OS$b_al <- y0_OS$al
# y6_OD$b_al <- y0_OD$al; y6_OS$b_al <- y0_OS$al
# y8_OD$b_al <- y0_OD$al; y8_OS$b_al <- y0_OS$al
# y0_OD$eye <- 1; y0_OS$eye <- 2
# y2_OD$eye <- 1; y2_OS$eye <- 2
# y4_OD$eye <- 1; y4_OS$eye <- 2
# y6_OD$eye <- 1; y6_OS$eye <- 2
# y8_OD$eye <- 1; y8_OS$eye <- 2
# # long format
# d_long <- rbind(y0_OD, y2_OD, y4_OD, y6_OD, y8_OD, y0_OS, y2_OS, y4_OS, y6_OS, y8_OS)
# d_long <- d_long[order(d_long$id),]
# 
# # Study sample: baseline age between 7 and 12, and follow-up age censored at 16
# data <- subset(d_long, age_group==1 & age<=15)
# 
# # Include time-lagged AL (lag-one effect of IOP on AL: effect of IOP at TP1 on AL at TP2) 
# data$al_next_2y <- NA
# for(i in unique(data$id)){
#   OD <- data[which(data$id==i & data$eye==1),]
#   OS <- data[which(data$id==i & data$eye==2),]
#   if(nrow(OD)!=1){ data[which(data$id==i & data$eye==1),]$al_next_2y[1:nrow(OD)-1] <- OD$al[2:nrow(OD)] }
#   if(nrow(OS)!=1){ data[which(data$id==i & data$eye==2),]$al_next_2y[1:nrow(OS)-1] <- OS$al[2:nrow(OS)] }
#   }
# data$rate <- (data$al_next_2y - data$al)/2 # compute annual rate of change
# 
# ## data cleaning ##
# # ID "MYP-000337" an annual rate of around -0.5mm (which is implausible; let's probe further)
# hist(data$rate) # distribution of annual rates
# data[which(data$rate <= -0.2),]$id
# # change Al at TP 2 to NA because the shrinking AL is highly likely to be data entry error at timepoint 2 (in BE)
# data[which(data$id=='MYP-000337' & data$tp==2),]$al <- NA
# for(i in unique(data$id)){
#   OD <- data[which(data$id==i & data$eye==1),]
#   OS <- data[which(data$id==i & data$eye==2),]
#   if(nrow(OD)!=1){ data[which(data$id==i & data$eye==1),]$al_next_2y[1:nrow(OD)-1] <- OD$al[2:nrow(OD)] }
#   if(nrow(OS)!=1){ data[which(data$id==i & data$eye==2),]$al_next_2y[1:nrow(OS)-1] <- OS$al[2:nrow(OS)] }
# }
# data$rate <- (data$al_next_2y - data$al)/2 # compute annual rate of change
# hist(data$rate) # viualise distribution of annual rates after data cleaning
# # IDs where annual rate > 0.6mm/y were also checked; data entry error not suspected
# 
# # multivariable model 1: contemporaneous (cross-sectional) effect of IOP on AL, adjusting for potential confounders 
# m1 <- lme(al~med(iop) + med(age) + med(cct) + med(acd) + med(lt) + factor(sex), random=~1+med(age)|id/eye, na.action=na.omit, data=data)
# tab_model(m1, pred.labels=c('Intercept', 'IOP', 'Age', 'CCT', 'ACD', 'LT', 'Female'), dv.labels='AL')
# # multivariable model 2: annual rate of change in AL vs IOP at the preceding timepoint, adjusting for potential confounders
# m2 <- lme(rate~med(iop) + med(al) + med(age) + med(cct) + med(acd) + med(lt) + factor(sex), random=~1+med(age)|id/eye, na.action=na.omit, data=data)
# tab_model(m2, pred.labels=c('Intercept', 'IOP', 'AL', 'Age', 'CCT', 'ACD', 'LT', 'Female'), dv.labels='Rate (mm/y)')















