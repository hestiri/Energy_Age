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
#plot1_Alt
ggplot(Ages, aes(x = Var, y = Coef)) + theme_bw() + geom_bar(stat = "identity",aes()) + 
  facet_grid(YEAR~ MDL)  + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + labs(title ="Energy Demand Across Age groups by year and model", x = "Age Group", y = "Estimated Coefficient")

ggplot(Ages, aes(x = Var, y = Coef)) + theme_bw() + geom_bar(stat = "identity") + facet_grid(YEAR~ MDL) +
  labs(title ="Energy Demand Across Age groups by year and model", x = "Age Group", y = "Estimated Coefficient") + theme(axis.text.x = element_text(angle = 90, hjust = 1))

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
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  facet_wrap(~MDL)  + 
  labs(title ="Coefficient estimate ranges by model", x = "Age Group", y = "Estimated Coefficient") 

ggplot(Ages, aes(x = Var, y = Coef)) + theme_bw() + 
  geom_point(aes(x= Var, y=Coef),alpha = 0.5) + 
  geom_boxplot(aes(fill=Coef), alpha = 0.5) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  geom_smooth(aes(x= REGX, y=Coef),col = "black", se = T,span = 0.8) +
  facet_wrap(~YEAR)  + 
  labs(title ="Coefficient estimate ranges by year", x = "Age Group", y = "Estimated Coefficient") 



ggplot(Ages, aes(x = Var, y = Coef, ymin = lower, ymax = upper)) + theme_bw() + geom_errorbar() +   theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  facet_grid(YEAR~ MDL)   + labs(title ="Coefficient error bars by year and model", x = "Age Group", y = "Estimated Coefficient") 

##plot 1
plot1 <- ggplot(Ages, aes(x = Var, y = Coef, ymin = lower, ymax = upper)) + theme_bw() + geom_linerange(alpha=0.7) +
  geom_smooth(aes(x= REGX, y=Coef),se = T,span = 0.8, color="white") +
  geom_point(aes(x= REGX, y=Coef, color=as.factor(sig0.05)),alpha=0.8, shape = 21, size = 2, stroke = 1.5) +
  scale_colour_manual(values = c("gray50", "black"),guide = guide_legend(title = "Significance at p<0.05")) +
  theme(legend.position = "bottom") +
  geom_smooth(aes(x= REGX, y=Coef),se = F,span = 0.8, color="salmon", size=1, alpha=0.3) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme(strip.text.x = element_text(size=9, face="bold"),
        strip.text.y = element_text(size=9, face="bold")) +
  facet_grid(YEAR~ MDL)  + labs(title ="Coefficient Estimate Ranges by Year and Model", x = "Age Group", y = "Estimated Coefficient") 
 
ggsave("graphics/plot1.pdf", plot1, dpi = 500, width = 24, height = 12)

##plot 2 
plot2 <- ggplot(Ages, aes(x = Var, y = Coef, ymin = lower, ymax = upper)) + theme_bw() + geom_linerange(alpha=0.3) +
  geom_smooth(aes(x= REGX, y=Coef, color=as.factor(YEAR)),se = T,span = 0.8) +
  guides(colour = guide_legend("Year")) +
  geom_point(aes(x= REGX, y=Coef),alpha=0.4,shape=19) +
  geom_smooth(aes(x= REGX, y=Coef, color=as.factor(YEAR)),se = F,span = 0.8) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme(strip.text.x = element_text(size=9, face="bold"),
        strip.text.y = element_text(size=9, face="bold")) +
  theme(legend.position = "bottom") +
  facet_wrap(~MDL, ncol=8)  + labs(title ="Coefficient estimate ranges by model", x = "Age Group", y = "Estimated Coefficient")  

ggsave("graphics/plot2.png", plot2, dpi = 500, width = 24, height = 5)


##plot 3
plot3 <- ggplot(Ages, aes(x = Var, y = Coef, ymin = lower, ymax = upper)) + theme_bw() + geom_linerange(alpha=0.3) +
  geom_smooth(aes(x= REGX, y=Coef, color=as.factor(MDL)),se = T,span = 0.8) +
  geom_point(aes(x= REGX, y=Coef),alpha=0.4,shape=19) +
  geom_smooth(aes(x= REGX, y=Coef, color=as.factor(MDL)),se = F,span = 0.8) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme(legend.position = "bottom") +
  theme(strip.text.x = element_text(size=9, face="bold"),
        strip.text.y = element_text(size=9, face="bold")) +
  guides(col = guide_legend(nrow = 4,"Model",title.position = "top")) +
  facet_wrap(~YEAR, ncol=1)  + labs(title ="Coefficient estimate ranges by year", x = "Age Group", y = "Estimated Coefficient")  

ggsave("graphics/plot3.png", plot3, dpi = 500, width = 4, height = 12)


plot4 <- ggplot(subset(Ages, Ages$MDL == "B_age_climate" | Ages$MDL == "F_age_climate_housingSize"), aes(x = Var, y = Coef)) + geom_bar(stat = "identity",aes(fill=Var)) + 
  facet_grid(YEAR~ MDL)  + labs(title ="", x = "", y = "") + 
  coord_polar(theta = "x", direction=1 ) + 
  theme(axis.text.x = element_text(hjust = 1)) +
  theme_bw() +
  theme(axis.text.x = element_text(size=6)) +
  theme(axis.text.y = element_text(size=6)) +
  theme(legend.position='none')


ggsave("graphics/plot4.png", plot4, dpi = 500, width = 7, height = 12)


ggplot(Ages, aes(x = Var, y = Coef, ymin = lower, ymax = upper)) + theme_bw() + geom_pointrange() +
  geom_point(aes(x= REGX, y=Coef, color=as.factor(YEAR))) +
  geom_smooth(aes(x= REGX, y=Coef, color=as.factor(YEAR)),se = T,span = 0.8) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
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


