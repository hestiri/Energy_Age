source("Analysis.R")

##LEt's plot'em out!

##First lookin at only age variables:

Ages <- subset(OUTPUT, OUTPUT$Var %in% c("age05","age05to09","age10to14","age15to19","age20to24","age25to29",
                                         "age30to34","age35to39","age40to44","age45to49","age50to54","age55to59",
                                         "age60to64","age65to69","age70to74","age75to79","age80p"))



########
# SMOOTHING *****
###############

Ages$Coef <- as.integer(Ages$Coef)


# creating a variable for x axis to run smoothing regression

Ages$REGX <- 0
r <- length(unique(Ages$Var))
for (i in 1:r) {
  var <- unique(Ages$Var)[i]
  num <- rank(unique(Ages$Var))[i]
  Ages$REGX <- ifelse(Ages$Var == var, rank(unique(Ages$Var))[i], Ages$REGX)
}



##########
######
######## AGE PLOTS
ggplot(Ages, aes(x = Var, y = Coef)) + theme_bw() + geom_bar(stat = "identity",aes(fill=Coef)) + 
  facet_grid(YEAR~ MDL)  + labs(title ="Energy Demand Across Age groups by year and model", x = "Age Group", y = "Estimated Coefficient")

ggplot(Ages, aes(x = Var, y = Coef)) + theme_bw() + geom_bar(stat = "identity") + facet_grid(YEAR~ MDL) +
  labs(title ="Energy Demand Across Age groups by year and model", x = "Age Group", y = "Estimated Coefficient") 

ggplot(Ages, aes(x = Var, y = Coef)) + theme_bw() + geom_bar(stat = "identity",aes(fill=Var)) + 
  facet_grid(YEAR~ MDL)  + labs(title =" Energy Demand Across Age groups by year and model", x = "Age Group", y = "Estimated Coefficient") + 
  coord_polar(theta = "x", direction=1 ) + theme(legend.position='none')

ggplot(Ages, aes(x = Var, y = Coef)) + theme_bw() + geom_bar(stat = "identity") + facet_grid(YEAR~ MDL) +
  labs(title =" New title", x = "Age Group", y = "Estimated Coefficient") + coord_polar(theta = "x", direction=1 )



ggplot(Ages, aes(x = Var, y = Coef)) + 
  geom_point(aes(x= Var, y=Coef),alpha = 0.5) + 
  geom_boxplot(aes(fill=Coef), alpha = 0.5) +
  scale_fill_continuous(low="gold", high="red",guide = FALSE) +
  geom_smooth(aes(x= REGX, y=Coef),col ="black",se = T,span = 0.8, alpha=0.5) +
  facet_wrap(~MDL)  + 
  labs(title ="Coefficient estimate ranges by model", x = "Age Group", y = "Estimated Coefficient") 

ggplot(Ages, aes(x = Var, y = Coef)) + theme_bw() + 
  geom_point(aes(x= Var, y=Coef),alpha = 0.5) + 
  geom_boxplot(aes(fill=Coef), alpha = 0.5) +
  geom_smooth(aes(x= REGX, y=Coef),col = "black", se = T,span = 0.8) +
  facet_wrap(~YEAR)  + 
  labs(title ="Coefficient estimate ranges by year", x = "Age Group", y = "Estimated Coefficient") 



ggplot(Ages, aes(x = Var, y = Coef, ymin = lower, ymax = upper)) + theme_bw() + geom_errorbar() +
  facet_grid(YEAR~ MDL)   + labs(title ="Coefficient error bars by year and model", x = "Age Group", y = "Estimated Coefficient") 


ggplot(Ages, aes(x = Var, y = Coef, ymin = lower, ymax = upper)) + theme_bw() + geom_pointrange() +
  geom_point(aes(x= REGX, y=Coef, color=as.factor(sig0.05))) +
  geom_smooth(aes(x= REGX, y=Coef, color=as.factor(YEAR)),se = T,span = 0.8) +
  facet_grid(YEAR~ MDL)  + labs(title ="Coefficient estimate ranges by year and model", x = "Age Group", y = "Estimated Coefficient") 

ggplot(Ages, aes(x = Var, y = Coef, ymin = lower, ymax = upper)) + theme_bw() + geom_pointrange() +
  geom_point(aes(x= REGX, y=Coef, color=as.factor(YEAR))) +
  geom_smooth(aes(x= REGX, y=Coef, color=as.factor(YEAR)),se = T,span = 0.8) +
  facet_wrap(~MDL)  + labs(title ="Coefficient estimate ranges by model", x = "Age Group", y = "Estimated Coefficient") 

plot <- ggplot(Ages, aes(x = Var, y = Coef, ymin = lower, ymax = upper)) + theme_bw() + geom_pointrange() +
  geom_point(aes(x= REGX, y=Coef, color=as.factor(YEAR))) +
  geom_smooth(aes(x= REGX, y=Coef, color=as.factor(YEAR)),se = T,span = 0.8) +
  facet_wrap(~YEAR)  + labs(title ="Coefficient estimate ranges by year", x = "Age Group", y = "Estimated Coefficient") 


##now lookin at only non-age variables:

NAges <- subset(OUTPUT, OUTPUT$Var %in% c("HDD65","CDD65","inc10","TYPEHUQ","HOMEAREA","YEARMADE10"))


### NAges PLOTS
ggplot(NAges, aes(x = Var, y = Coef)) + theme_bw() + geom_bar(stat = "identity") + facet_grid(YEAR~ MDL) +
  labs(title ="Energy Demand Across Age groups by year and model", x = "Age Group", y = "Estimated Coefficient") 



## loess smoothing
ggplot(Ages, aes(x= REGX, y=Coef, color=as.factor(YEAR))) +
  geom_point() +
  geom_smooth(se = T,span = 0.8) +
  facet_wrap(~MDL) +
  labs(title ="Coefficient estimate trends by year and model -- loess smoothing", x = "Age Group", y = "Estimated Coefficient") 

## fitting a linear line
ggplot(Ages, aes(x= REGX, y=Coef, color=as.factor(YEAR))) +
  geom_point() +
  geom_smooth(se = FALSE,method='lm') +
  facet_wrap(~MDL) +
  labs(title ="Coefficient estimate trends by year and model -- linear line", x = "Age Group", y = "Estimated Coefficient") 

## fitting a spline degree 3
ggplot(Ages, aes(x= REGX, y=Coef, color=as.factor(YEAR))) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ splines::bs(x, 3), se = FALSE) +
  facet_wrap(~MDL) +
  labs(title ="Coefficient estimate trends by year and model -- linear spline degree 3", x = "Age Group", y = "Estimated Coefficient") 


