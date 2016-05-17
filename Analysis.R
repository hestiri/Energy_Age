source("READ.R")

#Building formulas
age <- as.formula("BTUTOT~age05+age05to09+age10to14+age15to19+age20to24+age25to29+
                  age30to34+age35to39+age40to44+age45to49+age50to54+age55to59+
                  age60to64+age65to69+age70to74+age75to79+age80p"
)
age_climate <- as.formula("BTUTOT~age05+age05to09+age10to14+age15to19+age20to24+age25to29+
                          age30to34+age35to39+age40to44+age45to49+age50to54+age55to59+
                          age60to64+age65to69+age70to74+age75to79+age80p+
                          HDD65+CDD65"
)
age_climate_income <- as.formula("BTUTOT~age05+age05to09+age10to14+age15to19+age20to24+age25to29+
                               age30to34+age35to39+age40to44+age45to49+age50to54+age55to59+
                               age60to64+age65to69+age70to74+age75to79+age80p+
                               HDD65+CDD65+
                               inc10"
)


age_climate_housingType <- as.formula("BTUTOT~age05+age05to09+age10to14+age15to19+age20to24+age25to29+
                                       age30to34+age35to39+age40to44+age45to49+age50to54+age55to59+
                                       age60to64+age65to69+age70to74+age75to79+age80p+
                                       HDD65+CDD65+
                                       TYPEHUQ"
)

age_climate_housingSize <- as.formula("BTUTOT~age05+age05to09+age10to14+age15to19+age20to24+age25to29+
                                       age30to34+age35to39+age40to44+age45to49+age50to54+age55to59+
                                  age60to64+age65to69+age70to74+age75to79+age80p+
                                  HDD65+CDD65+
                                  HOMEAREA"
)
age_climate_housingAge <- as.formula("BTUTOT~age05+age05to09+age10to14+age15to19+age20to24+age25to29+
                                       age30to34+age35to39+age40to44+age45to49+age50to54+age55to59+
                                  age60to64+age65to69+age70to74+age75to79+age80p+
                                  HDD65+CDD65+
                                  YEARMADE10"
)
age_climate_housingAll <- as.formula("BTUTOT~age05+age05to09+age10to14+age15to19+age20to24+age25to29+
                                  age30to34+age35to39+age40to44+age45to49+age50to54+age55to59+
                                  age60to64+age65to69+age70to74+age75to79+age80p+
                                  HDD65+CDD65+
                                  TYPEHUQ+HOMEAREA+YEARMADE10"
)

age_climate_income_housingAll <- as.formula("BTUTOT~age05+age05to09+age10to14+age15to19+age20to24+age25to29+
                                           age30to34+age35to39+age40to44+age45to49+age50to54+age55to59+
                                           age60to64+age65to69+age70to74+age75to79+age80p+
                                           HDD65+CDD65+
                                           inc10+
                                           TYPEHUQ+HOMEAREA+YEARMADE10"
)

## a function for running the models and storing the results
model <- function(x, y) {
fit <- lm(x) 
N<- dim(summary(fit)$coefficients)[1]-1
RES <- c(1:N)
RES <- as.data.frame(RES)
RES$Var <- names(fit$coefficients[-1])
RES$Coef <- summary(fit)$coefficients[-1,1]
RES$upper <- RES$Coef+summary(fit)$coefficients[-1,2]
RES$lower <- RES$Coef-summary(fit)$coefficients[-1,2]
RES$sig0.05 <- ifelse(summary(fit)$coefficients[-1,4] < 0.0499, "Sig.", "Not Sig.")
RES$sig0.1 <- ifelse(summary(fit)$coefficients[-1,4] < 0.100001, "Sig.", "Not Sig.")
RES$YEAR <- y
RES$MDL <- x 
RES
}



## running the models and storing the results
attach(d87)
out1 <- model("age",1987)
out2 <- model("age_climate",1987)
out3 <- model("age_climate_income",1987)
out4 <- model("age_climate_housingType",1987)
out5 <- model("age_climate_housingSize",1987)
out6 <- model("age_climate_housingAge",1987)
out7 <- model("age_climate_housingAll",1987)
out8 <- model("age_climate_income_housingAll",1987)
detach(d87)
attach(d90)
out9 <- model("age",1990)
out10 <- model("age_climate",1990)
out11 <- model("age_climate_income",1990)
out12 <- model("age_climate_housingType",1990)
out13 <- model("age_climate_housingSize",1990)
out14 <- model("age_climate_housingAge",1990)
out15 <- model("age_climate_housingAll",1990)
out16 <- model("age_climate_income_housingAll",1990)
detach(d90)
attach(d05)
out17 <- model("age",2005)
out18 <- model("age_climate",2005)
out19 <- model("age_climate_income",2005)
out20 <- model("age_climate_housingType",2005)
out21 <- model("age_climate_housingSize",2005)
out22 <- model("age_climate_housingAge",2005)
out23 <- model("age_climate_housingAll",2005)
out24 <- model("age_climate_income_housingAll",2005)
detach(d05)
attach(d09)
out25 <- model("age",2009)
out26 <- model("age_climate",2009)
out27 <- model("age_climate_income",2009)
out28 <- model("age_climate_housingType",2009)
out29 <- model("age_climate_housingSize",2009)
out30 <- model("age_climate_housingAge",2009)
out31 <- model("age_climate_housingAll",2009)
out32 <- model("age_climate_income_housingAll",2009)
detach(d09)

## joining the results
OUTPUT <- rbind(out1,out2,out3,out4,out5,out6,out7,out8,out9,out10,out11,out12,out13,out14,out15,out16,out17,out18,out19,out20,out21,out22,out23,out24,out25,out26,out27,out28,out29,out30,out31,out32)
rm(out1,out2,out3,out4,out5,out6,out7,out8,out9,out10,out11,out12,out13,out14,out15,out16,out17,out18,out19,out20,out21,out22,out23,out24,out25,out26,out27,out28,out29,out30,out31,out32)











