library(dplyr)


## processing 1987 data

d87c <- read.table("Data/87/87CONSUMP.txt", header = T, sep=",",quote = "",fill = TRUE) # consumption
d87d <- read.table("Data/87/87DEMOGRAPbod.txt", header = F, sep="\t",quote = "",fill = TRUE) #demographic 
d87head <- read.table("Data/87/87DEMOGRAP.txt", header = T, sep=",",quote = "",fill = TRUE) #demographic 
names(d87d) <- names(d87head)
drops <- c("NWEIGHT", "REGIONC", "DIVISION", "NWEIGHT", "REGIONC", "DIVISION")
d87d <- d87d[,!(names(d87d) %in% drops)]
d87e <- read.table("Data/87/ENANDEQU.txt", header = T, sep=",",quote = "",fill = TRUE) #equipments
d87e <- d87e[,!(names(d87e) %in% drops)]

d87s <- read.table("Data/87/87STRUCTUR.txt", header = T, sep=",",quote = "",fill = TRUE) # structure of home
d87s <- d87s[,!(names(d87s) %in% drops)]


d87 <- merge(d87c,d87d, by = "HHID" )
d87 <- merge(d87, d87s, by = "HHID")
d87 <- merge(d87, d87e, by = "HHID")
rm(d87head,d87s,d87e,d87d,d87c)
names(d87)

d87 <- d87 %>%
  select(HHID,REGIONC,DIVISION,YEARMADE,TYPEHU,HOMEAREA,CDD65,HDD65,HHSEX,HHAGE,
         YEARS02,YEARS03,YEARS04,YEARS05,YEARS06,YEARS07,YEARS08,YEARS09,YEARS10,YEARS11,
         YEARS12,NHSLDMEM,ONEYPY,NWEIGHT,BTUKER,BTUEL,BTUNG,BTUFO,BTUNG,BTULP)



        # BTUEL  = BTUELSPH + BTUELWTH + BTUELAPL + BTUELCOL + BTUELRFG
        # BTUNG  = BTUNGSPH + BTUNGWTH + BTUNGAPL + BTUNGCOL
        # BTUFO  = BTUFOSPH + BTUFOWTH + BTUFOAPL
        # BTULP  = BTULPSPH + BTULPWTH + BTULPAPL
        # BTUKER = BTUKRSPH + BTUKRWTH + BTUKRAPL

## replacing 9999999 and 99 with 0
ll <- length(d87)
for (i in 1:ll) {
  col <- names(d87[i])
  d87[,col] <- ifelse(d87[,col] == 9999999, 0,d87[,col])
  d87[,col] <- ifelse(d87[,col] == 99, 0,d87[,col])
  
}

#calculating total energy use
d87$BTUTOT <- BTUKER+BTUEL+BTUNG+BTUFO+BTUNG+BTULP


##Create count columns
d87$age5 <- rowSums(d87[,10:21]<5) #<5 years olds
d87$age5to9 <- rowSums(d87[,10:21]>4 & d87[,10:21]<10) #5-9 years olds
d87$age10to14 <- rowSums(d87[,10:21]>9 & d87[,10:21]<15) #10-14 years olds
d87$age15to19 <- rowSums(d87[,10:21]>14 & d87[,10:21]<20) #15-19 years olds
d87$age20to24 <- rowSums(d87[,10:21]>19 & d87[,10:21]<25) #20-24 years olds
d87$age25to29 <- rowSums(d87[,10:21]>24 & d87[,10:21]<30) #25-29 years olds
d87$age30to34 <- rowSums(d87[,10:21]>29 & d87[,10:21]<35) #30-34 years olds
d87$age35to39 <- rowSums(d87[,10:21]>34 & d87[,10:21]<40) #35-39 years olds
d87$age40to44 <- rowSums(d87[,10:21]>39 & d87[,10:21]<45) #40-44 years olds
d87$age45to49 <- rowSums(d87[,10:21]>44 & d87[,10:21]<50) #45-49 years olds
d87$age50to54 <- rowSums(d87[,10:21]>49 & d87[,10:21]<55) #50-54 years olds
d87$age55to59 <- rowSums(d87[,10:21]>54 & d87[,10:21]<60) #55-59 years olds
d87$age60to64 <- rowSums(d87[,10:21]>59 & d87[,10:21]<65) #60-64 years olds
d87$age65to69 <- rowSums(d87[,10:21]>64 & d87[,10:21]<70) #65-69 years olds
d87$age70to74 <- rowSums(d87[,10:21]>69 & d87[,10:21]<75) #70-74 years olds
d87$age75to79 <- rowSums(d87[,10:21]>74 & d87[,10:21]<80) #75-79 years olds
d87$age80to84 <- rowSums(d87[,10:21]>79 & d87[,10:21]<85) #80-84 years olds
d87$age85 <- rowSums(d87[,10:21]>84 ) #>85 years olds




#####
####
####
####

# processing 1990 data

d901 <- read.table("Data/90/90File1_structure_data.txt", header = T, sep=",",quote = "",fill = TRUE) # general + TYPEHUQ and YEARMADE
d909 <- read.table("Data/90/90File9_demograp_data.txt", header = T, sep=",",quote = "",fill = TRUE) #demographic 
drops <- c("NWEIGHT", "REGIONC", "DIVISION", "NWEIGHT","TYPEHUQ","CDD65","HDD65","YEARMADE","HEATED","TYPEHOME")
d909 <- d909[,!(names(d909) %in% drops)]
d9012 <- read.table("Data/90/90File12_conelng_data.txt", header = T, sep=",",quote = "",fill = TRUE) #energy data
drops1 <- c("MONEYPY","AREA1980","NWEIGHT", "REGIONC", "DIVISION", "NWEIGHT","TYPEHOME","HEATED","YEARMADE","FUELH2O","COOLMAIN","COOLUNIT","FUELFOOD","FUELHEAT","EQUIPM")

d9014 <- read.table("Data/90/90File14_conoth_data.txt", header = T, sep=",",quote = "",fill = TRUE) #energy data
drops2 <- c("MONEYPY","AREA1980","NWEIGHT", "REGIONC", "DIVISION", "NWEIGHT","TYPEHOME","HEATED","YEARMADE")
d9014 <- d9014[,!(names(d9014) %in% drops2)]
d9012 <- d9012[,!(names(d9012) %in% drops1)]




d90 <- merge(d901,d909, by = "HHID" )
d90 <- merge(d90, d9014, by = "HHID")
d90 <- merge(d90, d9012, by = "HHID")

rm(d901,d9012,d909,d9014)
names(d90)

d90 <- d90 %>%
  select(HHID,NWEIGHT,REGIONC,DIVISION,TYPEHUQ,YEARMADE,HHSEX,HHAGE,YEARS02,
         YEARS03,YEARS04,YEARS05,YEARS06,YEARS07,YEARS08,YEARS09,
         YEARS10,YEARS11,YEARS12,NHSLDMEM,MONEYPY,CDD65,HDD65,BTUKER,BTUEL,BTUNG,BTUFO,BTUNG,BTULP)

## replacing 9999999 and 99 with 0
ll2 <- length(d90)
for (i in 1:ll2) {
  col <- names(d90[i])
  d90[,col] <- ifelse(d90[,col] == 9999999, 0,d90[,col])
  d90[,col] <- ifelse(d90[,col] == 99, 0,d90[,col])
  
}

#calculating total energy use
d90$BTUTOT <- d90$BTUKER+d90$BTUEL+d90$BTUNG+d90$BTUFO+d90$BTUNG+d90$BTULP
d90 <- subset(d90, d90$HHID != 1)


##Create count columns
d90$age5 <- rowSums(d90[,10:21]<5) #<5 years olds
d90$age5to9 <- rowSums(d90[,10:21]>4 & d90[,10:21]<10) #5-9 years olds
d90$age10to14 <- rowSums(d90[,10:21]>9 & d90[,10:21]<15) #10-14 years olds
d90$age15to19 <- rowSums(d90[,10:21]>14 & d90[,10:21]<20) #15-19 years olds
d90$age20to24 <- rowSums(d90[,10:21]>19 & d90[,10:21]<25) #20-24 years olds
d90$age25to29 <- rowSums(d90[,10:21]>24 & d90[,10:21]<30) #25-29 years olds
d90$age30to34 <- rowSums(d90[,10:21]>29 & d90[,10:21]<35) #30-34 years olds
d90$age35to39 <- rowSums(d90[,10:21]>34 & d90[,10:21]<40) #35-39 years olds
d90$age40to44 <- rowSums(d90[,10:21]>39 & d90[,10:21]<45) #40-44 years olds
d90$age45to49 <- rowSums(d90[,10:21]>44 & d90[,10:21]<50) #45-49 years olds
d90$age50to54 <- rowSums(d90[,10:21]>49 & d90[,10:21]<55) #50-54 years olds
d90$age55to59 <- rowSums(d90[,10:21]>54 & d90[,10:21]<60) #55-59 years olds
d90$age60to64 <- rowSums(d90[,10:21]>59 & d90[,10:21]<65) #60-64 years olds
d90$age65to69 <- rowSums(d90[,10:21]>64 & d90[,10:21]<70) #65-69 years olds
d90$age70to74 <- rowSums(d90[,10:21]>69 & d90[,10:21]<75) #70-74 years olds
d90$age75to79 <- rowSums(d90[,10:21]>74 & d90[,10:21]<80) #75-79 years olds
d90$age80to84 <- rowSums(d90[,10:21]>79 & d90[,10:21]<85) #80-84 years olds
d90$age85 <- rowSums(d90[,10:21]>84 ) #>85 years olds




#####
###
##
####

#### processing 2009 data

d09 <- read.table("Data/09/09_public.csv", header=T, sep=',')

## Trim to necessary fields

necFields <- c("DOEID",                              #id variable
               "HDD65","CDD65","HDD30YR", "CDD30YR", #these are local climate variables
               "DIVISION","REGIONC",                 #census divisions and regions
               "HHAGE",                              #age of householder
               "AGEHHMEMCAT2", "AGEHHMEMCAT3","AGEHHMEMCAT4","AGEHHMEMCAT5","AGEHHMEMCAT6",
               "AGEHHMEMCAT7","AGEHHMEMCAT8","AGEHHMEMCAT9","AGEHHMEMCAT10","AGEHHMEMCAT11",
               "AGEHHMEMCAT12","AGEHHMEMCAT13","AGEHHMEMCAT14", #age of householder members 2-14 (categorical)
               "NWEIGHT",                            #weight variable
               "TOTALBTU","TOTALDOL",                #total energy consumption and total energy expenditure
               "YEARMADE","TYPEHUQ",                 #year the building was built and building type
               "EDUCATION",                          #highest education of householder
               "NHSLDMEM",                           #household size   
               "MONEYPY",                           #income
               "TOTSQFT")                            #house size

d09 <- d09[ ,necFields]

###converting YEARMADE to actual age###

d09$YEARMADE <- (2009-d09$YEARMADE)

## transforming hhage variable into categorical (just like other household members)

d09$HHAGE <- ifelse(d09$HHAGE > 15 & d09$HHAGE < 20, 4,d09$HHAGE )
d09$HHAGE <- ifelse(d09$HHAGE > 19 & d09$HHAGE < 25, 5,d09$HHAGE )
d09$HHAGE <- ifelse(d09$HHAGE > 24 & d09$HHAGE < 30, 6,d09$HHAGE )
d09$HHAGE <- ifelse(d09$HHAGE > 29 & d09$HHAGE < 35, 7,d09$HHAGE )
d09$HHAGE <- ifelse(d09$HHAGE > 34 & d09$HHAGE < 40, 8,d09$HHAGE )
d09$HHAGE <- ifelse(d09$HHAGE > 39 & d09$HHAGE < 45, 9,d09$HHAGE )
d09$HHAGE <- ifelse(d09$HHAGE > 44 & d09$HHAGE < 50, 10,d09$HHAGE )
d09$HHAGE <- ifelse(d09$HHAGE > 49 & d09$HHAGE < 55, 11,d09$HHAGE )
d09$HHAGE <- ifelse(d09$HHAGE > 54 & d09$HHAGE < 60, 12,d09$HHAGE )
d09$HHAGE <- ifelse(d09$HHAGE > 59 & d09$HHAGE < 65, 13,d09$HHAGE )
d09$HHAGE <- ifelse(d09$HHAGE > 64 & d09$HHAGE < 70, 14,d09$HHAGE )
d09$HHAGE <- ifelse(d09$HHAGE > 69 & d09$HHAGE < 75, 15,d09$HHAGE )
d09$HHAGE <- ifelse(d09$HHAGE > 74 & d09$HHAGE < 80, 16,d09$HHAGE )
d09$HHAGE <- ifelse(d09$HHAGE > 79 & d09$HHAGE < 85, 17,d09$HHAGE )
d09$HHAGE <- ifelse(d09$HHAGE > 84 , 18,d09$HHAGE )


##Create count columns

d09$age5 <- rowSums(d09[,8:21]==1) #<5 years olds
d09$age5to9 <- rowSums(d09[,8:21]==2) #5-9 years olds
d09$age10to14 <- rowSums(d09[,8:21]==3) #10-14 years olds
d09$age15to19 <- rowSums(d09[,8:21]==4) #15-19 years olds
d09$age20to24 <- rowSums(d09[,8:21]==5) #20-24 years olds
d09$age25to29 <- rowSums(d09[,8:21]==6) #25-29 years olds
d09$age30to34 <- rowSums(d09[,8:21]==7) #30-34 years olds
d09$age35to39 <- rowSums(d09[,8:21]==8) #35-39 years olds
d09$age40to44 <- rowSums(d09[,8:21]==9) #40-44 years olds
d09$age45to49 <- rowSums(d09[,8:21]==10) #45-49 years olds
d09$age50to54 <- rowSums(d09[,8:21]==11) #50-54 years olds
d09$age55to59 <- rowSums(d09[,8:21]==12) #55-59 years olds
d09$age60to64 <- rowSums(d09[,8:21]==13) #60-64 years olds
d09$age65to69 <- rowSums(d09[,8:21]==14) #65-69 years olds
d09$age70to74 <- rowSums(d09[,8:21]==15) #70-74 years olds
d09$age75to79 <- rowSums(d09[,8:21]==16) #75-79 years olds
d09$age80to84 <- rowSums(d09[,8:21]==17) #80-84 years olds
d09$age85 <- rowSums(d09[,8:21]==18) #>85 years olds



