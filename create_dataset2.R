rm(list=ls())
setwd('/Users/fabianyii/Desktop/ZOC-BHVI/')

# Install and load relevant packagaes
# install.packages('haven')
# install.packages('blandr')
library('haven')
library('blandr')
library('ggplot2')

### Read and clean all datasets ### 
y0 <- read_dta('data/BHVI_ZOC high myopia cohort_y0 data_ww20220807.dta')
paste('y0 has', sum(duplicated(y0$id)), 'duplicated IDs' ) # no entries with duplicated IDs

y2 <- read_dta('data/BHVI_ZOC high myopia cohort_y2 data_ww20220807.dta') # 911 rows
paste('y2 has', sum(duplicated(y2$id)), 'duplicated IDs' ) # 26 entries with duplicated IDs
IDs <- y2[duplicated(y2$id),]$id  # Extract duplicated IDs
y2[y2$id %in% IDs,] # Isolate all entries with duplicated entries, Examine them. We can remove them because all pertinent measurements, i.e. AL and refraction are indeed duplicated.
y2 <- y2[!duplicated(y2$id),] # Resultant number of rows=885

y4 <- read_dta('data/BHVI_ZOC high myopia cohort_y4 data_ww20220807.dta') # 524 rows
paste('y4 has', sum(duplicated(y4$id)), 'duplicated IDs' ) # 2 entries with duplicated IDs. Remove them
y4 <- y4[!duplicated(y4$id),] # Resultant number of rows=522

y6 <- read_dta('data/BHVI_ZOC high myopia cohort_y6 data_ww20220807.dta') # 416 rows
paste('y6 has', sum(duplicated(y6$id)), 'duplicated IDs' ) # 1 entry with duplicated ID. Remove them
y6 <- y6[!duplicated(y6$id),] # Resultant number of rows=415

y8 <- read_dta('data/BHVI_ZOC high myopia cohort_y8 data_ww20220807.dta') # 323 rows
paste('y8 has', sum(duplicated(y8$id)), 'duplicated IDs' ) # 7 entries with duplicated IDs. Remove them
y8 <- y8[!duplicated(y8$id),] # Resultant number of rows=316

### Merge all datasets, setting all.x to TRUE so all IDs in y0 (baseline) are included
merged <- merge(y0, y2, by='id', all.x=TRUE)
merged <- merge(merged, y4, by='id', all.x=TRUE)
merged <- merge(merged, y6, by='id', all.x=TRUE)
merged <- merge(merged, y8, by='id', all.x=TRUE)
# only include those aged 17y or younger at baseline
merged <- subset(merged, y0_age<=17)
merged$age_group <- 1
merged[which(merged$y0_age>=12),]$age_group <- 2
merged$age_group <- factor(merged$age_group)

OD <- data.frame('id'=merged$id, 
                 'sex'=as.factor(merged$sex), # sex (1=male; 2=female)
                 'y0_age'=round(merged$y0_age,2), 
                 'y2_age'=round(merged$y2_age,2), 
                 'y4_age'=as.numeric(round((as.Date(merged$y4_examdate, '%Y/%m/%d') - merged$dob)/365,2)), 
                 'y6_age'=as.numeric(round((as.Date(merged$y6_examdate, '%Y/%m/%d')-merged$dob)/365,2)), 
                 'y8_age'=as.numeric(round((as.Date(merged$y8_examdate, '%Y/%m/%d')-merged$dob)/365,2)), 
                 'y0_iop'=merged$y0_iopm_od, 
                 'y2_iop'=merged$y2_iopm_od, 
                 'y4_iop'=merged$y4_iopm_od, 
                 'y6_iop'=merged$y6_iopm_od, 
                 'y8_iop'=merged$y8_iopm_od, 
                 'y0_al'=merged$y0_Lenstar_al_OD,
                 'y2_al'=merged$y2_al_od, 
                 'y4_al'=merged$y4_al_od, 
                 'y6_al'=merged$y6_al_od, 
                 'y8_al'=merged$y8_al_od, 
                 'y0_se'=round(merged$y0_subj_sphereod-merged$y0_subj_cylinderod/2,2), 
                 'y2_se'=round(merged$y2_subj_sphereod-merged$y2_subj_cylinderod/2,2), 
                 'y4_se'=round(merged$y4_subj_sphereod-merged$y4_subj_cylinderod/2,2), 
                 'y6_se'=round(merged$y6_subj_sphereod-merged$y6_subj_cylinderod/2,2), 
                 'y8_se'=round(merged$y8_subj_sphereod-merged$y8_subj_cylinderod/2,2),
                 'y0_cct'=as.numeric(merged$y0_Lenstar_cct_OD),
                 'y2_cct'=as.numeric(merged$y2_cct_od),
                 'y4_cct'=as.numeric(merged$y4_cct_od),
                 'y6_cct'=as.numeric(merged$y6_cct_od),
                 'y8_cct'=as.numeric(merged$y8_cct_od),
                 'y0_acd'=as.numeric(merged$y0_Lenstar_ad_OD),
                 'y2_acd'=as.numeric(merged$y2_acd_od),
                 'y4_acd'=as.numeric(merged$y4_acd_od),
                 'y6_acd'=as.numeric(merged$y6_acd_od),
                 'y8_acd'=as.numeric(merged$y8_acd_od),
                 'y0_lt'=as.numeric(merged$y0_Lenstar_lt_OD),
                 'y2_lt'=as.numeric(merged$y2_lt_od),
                 'y4_lt'=as.numeric(merged$y4_lt_od),
                 'y6_lt'=as.numeric(merged$y6_lt_od),
                 'y8_lt'=as.numeric(merged$y8_lt_od),
                 'age_group'=merged$age_group)
OS <- data.frame('id'=merged$id, 
                 'sex'=as.factor(merged$sex), # sex (1=male; 2=female)
                 'y0_age'=round(merged$y0_age,2), 
                 'y2_age'=round(merged$y2_age,2), 
                 'y4_age'=as.numeric(round((as.Date(merged$y4_examdate, '%Y/%m/%d')-merged$dob)/365,2)), 
                 'y6_age'=as.numeric(round((as.Date(merged$y6_examdate, '%Y/%m/%d')-merged$dob)/365,2)), 
                 'y8_age'=as.numeric(round((as.Date(merged$y8_examdate, '%Y/%m/%d')-merged$dob)/365,2)), 
                 'y0_iop'=merged$y0_iopm_os, 
                 'y2_iop'=merged$y2_iopm_os, 
                 'y4_iop'=merged$y4_iopm_os, 
                 'y6_iop'=merged$y6_iopm_os, 
                 'y8_iop'=merged$y8_iopm_os, 
                 'y0_al'=merged$y0_Lenstar_al_OS,
                 'y2_al'=merged$y2_al_os, 
                 'y4_al'=merged$y4_al_os, 
                 'y6_al'=merged$y6_al_os, 
                 'y8_al'=merged$y8_al_os, 
                 'y0_se'=round(merged$y0_subj_sphereos-merged$y0_subj_cylinderos/2, 2), 
                 'y2_se'=round(merged$y2_subj_sphereos-merged$y2_subj_cylinderos/2, 2),
                 'y4_se'=round(merged$y4_subj_sphereos-merged$y4_subj_cylinderos/2, 2),
                 'y6_se'=round(merged$y6_subj_sphereos-merged$y6_subj_cylinderos/2, 2),
                 'y8_se'=round(merged$y8_subj_sphereos-merged$y8_subj_cylinderos/2,2),
                 'y0_cct'=as.numeric(merged$y0_Lenstar_cct_OS),
                 'y2_cct'=as.numeric(merged$y2_cct_os),
                 'y4_cct'=as.numeric(merged$y4_cct_os),
                 'y6_cct'=as.numeric(merged$y6_cct_os),
                 'y8_cct'=as.numeric(merged$y8_cct_os),
                 'y0_acd'=as.numeric(merged$y0_Lenstar_ad_OS),
                 'y2_acd'=as.numeric(merged$y2_acd_os),
                 'y4_acd'=as.numeric(merged$y4_acd_os),
                 'y6_acd'=as.numeric(merged$y6_acd_os),
                 'y8_acd'=as.numeric(merged$y8_acd_os),
                 'y0_lt'=as.numeric(merged$y0_Lenstar_lt_OS),
                 'y2_lt'=as.numeric(merged$y2_lt_os),
                 'y4_lt'=as.numeric(merged$y4_lt_os),
                 'y6_lt'=as.numeric(merged$y6_lt_os),
                 'y8_lt'=as.numeric(merged$y8_lt_os),
                 'age_group'=merged$age_group)
# Replace positive SE with NA
OD[which(OD$y2_se >= 0),]$y2_se <- NA # 3 instances replaced with NA
OD[which(OD$y8_se >= 0),]$y8_se <- NA # 1 instance replaced with NA
OS[which(OS$y2_se >= 0),]$y2_se <- NA # 3 instances replaced with NA
OS[which(OS$y6_se >= 0),]$y6_se <- NA # 1 instance replaced with NA
OS[which(OS$y8_se >= 0),]$y8_se <- NA # 1 instance replaced with NA

### Add additional variables (potential covariates) to the cleaned dataset (OD) ###
# Check CCT, ACD and LT distributions at each timepoint
for(i in 23:37){ hist(OD[,i], main=names(OD[i])); hist(OS[,i], main=names(OS[i])) }
# Y8 OD & OS CCT unusually small value (<400), but does not seem to be data 
# entry error as the value is consistent with a low IOP reading (11mmHg) in BE
OD[which(OD$y8_cct<400),]; OS[which(OD$y8_cct<400),]
## ID MYP-000943
# Y2 OS LT unusually large value (>4.5)
# Y4 OD & OS LT unusually large value (>4.5)
# Y6 OD & OS LT unusually large value (>4.5)
# don't bother as this px wont get included anyway (baseline age 16y/o)


### Data cleaning ###
# remove implausibly large IOP (>50)
OD[which(OD$y0_iop > 50),]$y0_iop <- NA # 3 instances where IOP=99 replaced w NA
OD[which(OD$y4_iop > 50),]$y4_iop <- NA # 2 instances where IOP=99 replaced w NA
OD[which(OD$y6_iop > 50),]$y6_iop <- NA # 1 instance where IOP=51 replaced w NA
OD[which(OD$y8_iop > 50),]$y8_iop <- NA # 1 instance where IOP=51 replaced w NA
OS[which(OS$y0_iop > 50),]$y0_iop <- NA # 3 instances where IOP=99 replaced w NA
OS[which(OS$y4_iop > 50),]$y4_iop <- NA # 2 instances and 1 instance where IOP=99 and IOP=51 replaced w NA
OS[which(OS$y6_iop > 50),]$y6_iop <- NA # 1 instance where IOP=61 replaced w NA
# visualise OD and OS IOP distributions
for(i in 8:12){ hist(OD[,i]); hist(OS[,i]) } 
# visualise OD and OS AL distributions at each visit
for(i in 13:17){ hist(OD[,i], main=paste('TP',i-12)); hist(OS[,i], main=paste('TP',i-12)) }
# visualise OD and OS age distributions at each visit
for(i in 3:7){ hist(OD[,i], main=paste('TP',i-2)); hist(OS[,i], main=paste('TP',i-2)) }
ind <- which(OD$y6_age < 10)
OD[ind,]$y6_age <- NA; OS[ind,]$y6_age <- NA # 2 instances where age=0
ind <- which(OD$y8_age < 10)
OD[ind,]$y8_age <- NA; OS[ind,]$y8_age <- NA # 7 instances where age=0

### Write the final datasets (OD & OS) ###
write.csv(OD, 'data/OD.csv')
write.csv(OS, 'data/OS.csv')









