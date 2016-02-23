source("READ.R")

##LMs

##1987
fit87 <- lm(d87$BTUTOT~d87$age5+d87$age5to9+d87$age10to14+d87$age15to19+d87$age20to24+d87$age25to29+d87$age30to34+d87$age35to39+d87$age40to44+d87$age45to49+
              d87$age50to54+d87$age55to59+d87$age60to64+d87$age65to69+d87$age70to74+d87$age75to79+d87$age80to84+d87$age85+d87$HDD65+d87$CDD65+d87$YEARMADE+
              d87$ONEYPY+d87$TYPEHU+d87$HOMEAREA+d87$NHSLDMEM)
summary(fit87)
ages87<- fit87$coefficients[2:19]
barplot(ages87)


##1990
fit90 <- lm(d90$BTUTOT~d90$age5+d90$age5to9+d90$age10to14+d90$age15to19+d90$age20to24+d90$age25to29+d90$age30to34+d90$age35to39+d90$age40to44+d90$age45to49+
              d90$age50to54+d90$age55to59+d90$age60to64+d90$age65to69+d90$age70to74+d90$age75to79+d90$age80to84+d90$age85+d90$HDD65+d90$CDD65+d90$YEARMADE+
              d90$MONEYPY+d90$TYPEHUQ+d90$YEARMADE+d90$NHSLDMEM)
summary(fit90)
ages90<- fit90$coefficients[2:19]
barplot(ages90)


##2009
fit09 <- lm(d09$TOTALBTU~d09$age5+d09$age5to9+d09$age10to14+d09$age15to19+d09$age20to24+d09$age25to29+d09$age30to34+d09$age35to39+d09$age40to44+d09$age45to49+
              d09$age50to54+d09$age55to59+d09$age60to64+d09$age65to69+d09$age70to74+d09$age75to79+d09$age80to84+d09$age85+d09$HDD65+d09$CDD65+d09$YEARMADE+
              d09$MONEYPY+d09$TOTSQFT+d09$NHSLDMEM)
summary(fit09)
ages09<- fit09$coefficients[2:19]
barplot(ages09)
