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
age_climate_econ <- as.formula("BTUTOT~age05+age05to09+age10to14+age15to19+age20to24+age25to29+
                               age30to34+age35to39+age40to44+age45to49+age50to54+age55to59+
                               age60to64+age65to69+age70to74+age75to79+age80p+
                               HDD65+CDD65+
                               inc10"
)
age_climate_econ_housing <- as.formula("BTUTOT~age05+age05to09+age10to14+age15to19+age20to24+age25to29+
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
RES$YEAR <- y
RES$MDL <- x 
RES
}

## running the models and storing the results
attach(d87)
out1 <- model("age",1987)
out2 <- model("age_climate",1987)
out3 <- model("age_climate_econ",1987)
out4 <- model("age_climate_econ_housing",1987)
detach(d87)
attach(d90)
out5 <- model("age",1990)
out6 <- model("age_climate",1990)
out7 <- model("age_climate_econ",1990)
out8 <- model("age_climate_econ_housing",1990)
detach(d90)
attach(d09)
out9 <- model("age",2009)
out10 <- model("age_climate",2009)
out11 <- model("age_climate_econ",2009)
out12 <- model("age_climate_econ_housing",2009)
detach(d09)

## joining the results
OUTPUT <- rbind(out1,out2,out3,out4,out5,out6,out7,out8,out9,out10,out11,out12)
rm(out1,out2,out3,out4,out5,out6,out7,out8,out9,out10,out11,out12)











