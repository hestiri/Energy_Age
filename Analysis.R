source("READ.R")

#Building formulas
Model1 <- as.formula("BTUTOT~age05+age05to09+age10to14+age15to19+age20to24+age25to29+
                  age30to34+age35to39+age40to44+age45to49+age50to54+age55to59+
                  age60to64+age65to69+age70to74+age75to79+age80p"
)
Model2 <- as.formula("BTUTOT~age05+age05to09+age10to14+age15to19+age20to24+age25to29+
                          age30to34+age35to39+age40to44+age45to49+age50to54+age55to59+
                          age60to64+age65to69+age70to74+age75to79+age80p+
                          HDD65+CDD65"
)
Model3 <- as.formula("BTUTOT~age05+age05to09+age10to14+age15to19+age20to24+age25to29+
                               age30to34+age35to39+age40to44+age45to49+age50to54+age55to59+
                               age60to64+age65to69+age70to74+age75to79+age80p+
                               HDD65+CDD65+
                               inc10"
)


Model4 <- as.formula("BTUTOT~age05+age05to09+age10to14+age15to19+age20to24+age25to29+
                                       age30to34+age35to39+age40to44+age45to49+age50to54+age55to59+
                                       age60to64+age65to69+age70to74+age75to79+age80p+
                                       HDD65+CDD65+
                                       TYPEHUQ"
)

Model6 <- as.formula("BTUTOT~age05+age05to09+age10to14+age15to19+age20to24+age25to29+
                                       age30to34+age35to39+age40to44+age45to49+age50to54+age55to59+
                                  age60to64+age65to69+age70to74+age75to79+age80p+
                                  HDD65+CDD65+
                                  HOMEAREA.percap"
)
Model5 <- as.formula("BTUTOT~age05+age05to09+age10to14+age15to19+age20to24+age25to29+
                                       age30to34+age35to39+age40to44+age45to49+age50to54+age55to59+
                                  age60to64+age65to69+age70to74+age75to79+age80p+
                                  HDD65+CDD65+
                                  YEARMADE10"
)
Model7 <- as.formula("BTUTOT~age05+age05to09+age10to14+age15to19+age20to24+age25to29+
                                  age30to34+age35to39+age40to44+age45to49+age50to54+age55to59+
                                  age60to64+age65to69+age70to74+age75to79+age80p+
                                  HDD65+CDD65+
                                  TYPEHUQ+HOMEAREA.percap+YEARMADE10"
)

Model8 <- as.formula("BTUTOT~age05+age05to09+age10to14+age15to19+age20to24+age25to29+
                                           age30to34+age35to39+age40to44+age45to49+age50to54+age55to59+
                                           age60to64+age65to69+age70to74+age75to79+age80p+
                                           HDD65+CDD65+
                                           inc10+
                                           TYPEHUQ+HOMEAREA.percap+YEARMADE10"
)

## helper function for running the models and storing the results
model <- function(x,vars, y, z) {
fit <- lm(x) 
N<- dim(summary(fit)$coefficients)[1]-1
RES <- c(1:N)
RES <- as.data.frame(RES)
RES$Var <- names(fit$coefficients[-1])
RES$Coef <- summary(fit)$coefficients[-1,1]
RES$upper <- RES$Coef+summary(fit)$coefficients[-1,2]
RES$lower <- RES$Coef-summary(fit)$coefficients[-1,2]
RES$sig0.05 <- ifelse(summary(fit)$coefficients[-1,4] < 0.0499, "Significant", "Not Significant")
RES$sig0.1 <- ifelse(summary(fit)$coefficients[-1,4] < 0.100001, "Significant", "Not Significant")
RES$YEAR <- y
RES$MDL <- x 
RES$MDLX <- z ## ordering the models
RES$varnams <- vars
RES$R2 = summary(fit)$r.squared#adj.r.squared
return(
RES
)
}



## running the models and storing the results
attach(d87)
out1 <- model("Model1","Model 1: age",1987,1)
out2 <- model("Model2","Model 2: age+climate",1987,2)
out3 <- model("Model3","Model 3: age+climate+income",1987,3)
out4 <- model("Model4","Model 4: age+climate+housing type",1987,4)
out5 <- model("Model6","Model 6: age+climate+housing size",1987,6)
out6 <- model("Model5","Model 5: age+climate+housing age",1987,5)
out7 <- model("Model7","Model 7: age+climate+housing (all)",1987,7)
out8 <- model("Model8","Model 8: age+climate+income+housing (all)",1987,8)
detach(d87)
attach(d90)
out9 <- model("Model1","Model 1: age",1990,1)
out10 <- model("Model2","Model 2: age+climate",1990,2)
out11 <- model("Model3","Model 3: age+climate+income",1990,3)
out12 <- model("Model4","Model 4: age+climate+housing type",1990,4)
out13 <- model("Model6","Model 6: age+climate+housing size",1990,6)
out14 <- model("Model5","Model 5: age+climate+housing age",1990,5)
out15 <- model("Model7","Model 7: age+climate+housing (all)",1990,7)
out16 <- model("Model8","Model 8: age+climate+income+housing (all)",1990,8)
detach(d90)
attach(d05)
out17 <- model("Model1","Model 1: age",2005,1)
out18 <- model("Model2","Model 2: age+climate",2005,2)
out19 <- model("Model3","Model 3: age+climate+income",2005,3)
out20 <- model("Model4","Model 4: age+climate+housing type",2005,4)
out21 <- model("Model6","Model 6: age+climate+housing size",2005,6)
out22 <- model("Model5","Model 5: age+climate+housing age",2005,5)
out23 <- model("Model7","Model 7: age+climate+housing (all)",2005,7)
out24 <- model("Model8","Model 8: age+climate+income+housing (all)",2005,8)
detach(d05)
attach(d09)
out25 <- model("Model1","Model 1: age",2009,1)
out26 <- model("Model2","Model 2: age+climate",2009,2)
out27 <- model("Model3","Model 3: age+climate+income",2009,3)
out28 <- model("Model4","Model 4: age+climate+housing type",2009,4)
out29 <- model("Model6","Model 6: age+climate+housing size",2009,6)
out30 <- model("Model5","Model 5: age+climate+housing age",2009,5)
out31 <- model("Model7","Model 7: age+climate+housing (all)",2009,7)
out32 <- model("Model8","Model 8: age+climate+income+housing (all)",2009,8)
detach(d09)

## joining the results
OUTPUT <- rbind(out1,out2,out3,out4,out5,out6,out7,out8,out9,out10,out11,out12,out13,out14,out15,out16,out17,out18,out19,out20,out21,out22,out23,out24,out25,out26,out27,out28,out29,out30,out31,out32)
rm(out1,out2,out3,out4,out5,out6,out7,out8,out9,out10,out11,out12,out13,out14,out15,out16,out17,out18,out19,out20,out21,out22,out23,out24,out25,out26,out27,out28,out29,out30,out31,out32)

OUTPUT.bundle = subset(OUTPUT, OUTPUT$MDLX %in% c(2, 7,8))
OUTPUT.bundle$MODEL= as.character("Model C")
OUTPUT.bundle$MODEL = ifelse(OUTPUT.bundle$MDLX == 2, as.character("Model A"), OUTPUT.bundle$MODEL)
OUTPUT.bundle$MODEL = ifelse(OUTPUT.bundle$MDLX == 7, as.character("Model B"), OUTPUT.bundle$MODEL)

OUTPUT.bundle$varnams = ifelse (OUTPUT.bundle$MDLX == 2,"Model A: age + climate",OUTPUT.bundle$varnams) 
OUTPUT.bundle$varnams = ifelse (OUTPUT.bundle$MDLX == 7, "Model B: age + climate + housing",OUTPUT.bundle$varnams) 
OUTPUT.bundle$varnams = ifelse (OUTPUT.bundle$MDLX == 8, "Model C: age + climate + housing + income",OUTPUT.bundle$varnams)

OUTPUT$MDL2 = ""
OUTPUT$MDL2 = ifelse (OUTPUT$MDL == "Model1", "Model 0",OUTPUT$MDL2)
OUTPUT$MDL2 = ifelse (OUTPUT$MDL == "Model2", "Model A",OUTPUT$MDL2)
OUTPUT$MDL2 = ifelse (OUTPUT$MDL == "Model3", "Model A+income",OUTPUT$MDL2)
OUTPUT$MDL2 = ifelse (OUTPUT$MDL == "Model4", "Model A+housing.type",OUTPUT$MDL2)
OUTPUT$MDL2 = ifelse (OUTPUT$MDL == "Model5", "Model A+housing.age",OUTPUT$MDL2)
OUTPUT$MDL2 = ifelse (OUTPUT$MDL == "Model6", "Model A+housing.size",OUTPUT$MDL2)
OUTPUT$MDL2 = ifelse (OUTPUT$MDL == "Model7", "Model B",OUTPUT$MDL2)
OUTPUT$MDL2 = ifelse (OUTPUT$MDL == "Model8", "Model C",OUTPUT$MDL2)

