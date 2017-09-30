source("Analysis.R")

##LEt's plot'em out!

##First lookin at only age variables:
##for all 8 models
Ages <- subset(OUTPUT, OUTPUT$Var %in% c("age05","age05to09","age10to14","age15to19","age20to24","age25to29",
                                         "age30to34","age35to39","age40to44","age45to49","age50to54","age55to59",
                                         "age60to64","age65to69","age70to74","age75to79","age80p"))

##for the 3 model bundles
Ages.bundle <- subset(OUTPUT.bundle, OUTPUT.bundle$Var %in% c("age05","age05to09","age10to14","age15to19","age20to24","age25to29",
                                         "age30to34","age35to39","age40to44","age45to49","age50to54","age55to59",
                                         "age60to64","age65to69","age70to74","age75to79","age80p"))

write.csv(Ages.bundle[,-c(1,9:10,13)],"graphics/table2.csv")

########
# SMOOTHING *****
###############

Ages$Coef <- as.integer(Ages$Coef)
Ages.bundle$Coef <- as.integer(Ages.bundle$Coef)

# creating a variable for x axis to run smoothing regression

Ages$REGX <- 0
r <- length(unique(Ages$Var))
for (i in 1:r) {
  var <- unique(Ages$Var)[i]
  num <- rank(unique(Ages$Var))[i]
  Ages$REGX <- ifelse(Ages$Var == var, rank(unique(Ages$Var))[i], Ages$REGX)
}

Ages.bundle$REGX <- 0
r <- length(unique(Ages.bundle$Var))
for (i in 1:r) {
  var <- unique(Ages.bundle$Var)[i]
  num <- rank(unique(Ages.bundle$Var))[i]
  Ages.bundle$REGX <- ifelse(Ages.bundle$Var == var, rank(unique(Ages.bundle$Var))[i], Ages.bundle$REGX)
}

# 
# labelModels <- c(Model1="Model 1: age", Model2="Model 2: age+climate",
#                  Model3= "Model 3: age+climate+income", Model4 = "Model 4: age+climate+housing type",
#                  Model5="Model 5: age+climate+housing age", Model6 = "Model 6: age+climate+housing size",
#                  Model7="Model 7: age+climate+housing (All)", Model8="Model 8: age+climate+income+housing (All)")

##########
######
######## AGE PLOTS --alternative plots not in the paper
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


      # # Helper function for string wrapping. 
      # # Default 20 character target width.
      # swr = function(string, nwrap=20) {
      #   paste(strwrap(string, width=nwrap), collapse="\n")
      # }
      # swr = Vectorize(swr)
      # 
      # # Create line breaks in Year
      # Ages$varnams == swr(Ages$varnams)



##plot 1 for all models
plot1 <- ggplot(Ages, aes(x = Var, y = Coef, ymin = lower, ymax = upper)) + theme_bw() + geom_linerange(alpha=0.7) +
  geom_smooth(aes(x= REGX, y=Coef),se = T,span = 0.8, color="white") +
  geom_point(aes(x= REGX, y=Coef, color=as.factor(sig0.05)),alpha=0.8, shape = 21, size = 2, stroke = 1.5) +
  scale_colour_manual(values = c("red", "black"),
                      guide = guide_legend(title = "Significance at p<0.05", face = "bold",size=12)) +
  theme(legend.position = "bottom") +
  geom_smooth(aes(x= REGX, y=Coef),se = F,span = 0.8, color="salmon", size=1, alpha=0.3) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme(strip.text.x = element_text(size=14, face="bold", vjust = 1),
        strip.text.y = element_text(size=14, face="bold")) +
  geom_text(aes(x = "age20to24", y = 39000,label=paste0("R^2: ",round(R2,3))), parse = TRUE, inherit.aes=FALSE) +
  facet_grid(YEAR~ MDL2)  +
  theme(legend.text=element_text(size=14)) +
  labs(title ="", 
       x = "Age Group", y = "Estimated Coefficient") 
 
ggsave("graphics/plot1_new.pdf", plot1, dpi = 300, width = 24, height = 12)

##plot 1 for all bundled models
plot1.bundle <- ggplot(Ages.bundle, aes(x = Var, y = Coef, ymin = lower, ymax = upper)) + theme_bw() + geom_linerange(alpha=0.7) +
  geom_smooth(aes(x= REGX, y=Coef),se = T,span = 0.8, color="white") +
  geom_point(aes(x= REGX, y=Coef, color=as.factor(sig0.05)),alpha=0.8, shape = 21, size = 2, stroke = 1.5) +
  scale_colour_manual(values = c("red", "black"),
                      guide = guide_legend(title = "Significance at p<0.05", face = "bold",size=14)) +
  theme(legend.position = "bottom") +
  geom_smooth(aes(x= REGX, y=Coef),se = F,span = 0.8, color="salmon", size=1, alpha=0.3) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme(strip.text.x = element_text(size=14, face="bold", vjust = 1),
        strip.text.y = element_text(size=14, face="bold")) +
  # geom_text(aes(x = "age20to24", y = 39000,label=paste0("R^2: ",round(R2,3))), parse = TRUE, inherit.aes=FALSE) +
  facet_grid(YEAR~ MODEL)  +
  theme(legend.text=element_text(size=14)) +
  labs(title ="", 
       x = "Age Group", y = "Estimated Coefficient") 

ggsave("graphics/figure2.pdf", plot1.bundle, dpi = 300, width = 9, height = 12)



##plot 2 for all plots
plot2 <- ggplot(Ages, aes(x = Var, y = Coef, ymin = lower, ymax = upper)) + theme_bw() + geom_linerange(alpha=0.3) +
  geom_smooth(aes(x= REGX, y=Coef, color=as.factor(YEAR)),se = T,span = 0.8) +
  guides(colour = guide_legend("Year")) +
  geom_point(aes(x= REGX, y=Coef),alpha=0.4,shape=19) +
  geom_smooth(aes(x= REGX, y=Coef, color=as.factor(YEAR)),se = F,span = 0.8) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme(strip.text.x = element_text(size=14, face="bold"),
        strip.text.y = element_text(size=14, face="bold")) +
  theme(legend.position = "bottom") +
  theme(legend.text=element_text(size=14)) +
  facet_wrap(~varnams, ncol=2)  + labs(title ="Coefficient estimate ranges by model", x = "Age Group", y = "Estimated Coefficient")  

ggsave("graphics/plot2.pdf", plot2, dpi = 300, width = 9, height = 18)


##plot 2 for bundled models
plot2.bundel <- ggplot(Ages.bundle, aes(x = Var, y = Coef, ymin = lower, ymax = upper)) + theme_bw() + geom_linerange(alpha=0.3) +
  geom_smooth(aes(x= REGX, y=Coef, color=as.factor(YEAR)),se = T,span = 0.8) +
  guides(colour = guide_legend("Year")) +
  geom_point(aes(x= REGX, y=Coef),alpha=0.4,shape=19) +
  geom_smooth(aes(x= REGX, y=Coef, color=as.factor(YEAR)),se = F,span = 0.8) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme(strip.text.x = element_text(size=14, face="bold"),
        strip.text.y = element_text(size=14, face="bold")) +
  theme(legend.position = c(0.7,.1)) +
  guides(col = guide_legend(nrow = 1,"Year")) +
  theme(legend.text=element_text(size=14)) +
  facet_wrap(~varnams, ncol=2)  + 
  labs(title ="", x = "Age Group", y = "Estimated Coefficient")  

ggsave("graphics/figure2.pdf", plot2.bundel, dpi = 300, width = 10, height = 8)



##plot 3 for the 8 models
plot3 <- ggplot(Ages, aes(x = Var, y = Coef, ymin = lower, ymax = upper)) + theme_bw() + geom_linerange(alpha=0.3) +
  geom_smooth(aes(x= REGX, y=Coef, color=as.factor(varnams)),se = T,span = 0.8) +
  geom_point(aes(x= REGX, y=Coef),alpha=0.4,shape=19) +
  geom_smooth(aes(x= REGX, y=Coef, color=as.factor(varnams)),se = F,span = 0.8) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme(legend.position = "bottom") +
  theme(strip.text.x = element_text(size=14, face="bold"),
        strip.text.y = element_text(size=14, face="bold")) +
  theme(legend.text=element_text(size=12)) +
  guides(col = guide_legend(nrow = 4,"Model",title.position = "top"))+#, labeller = labeller(MDL = varnams)) +
  facet_wrap(~YEAR, ncol=2)  + labs(title ="Coefficient estimate ranges by year", x = "Age Group", y = "Estimated Coefficient")  

ggsave("graphics/plot3-2.pdf", plot3, dpi = 300, width = 8, height = 10)


##plot 3 for the bundled models
plot3.bundle <- ggplot(Ages.bundle, aes(x = Var, y = Coef, ymin = lower, ymax = upper)) + theme_bw() + geom_linerange(alpha=0.3) +
  geom_smooth(aes(x= REGX, y=Coef, color=as.factor(varnams)),se = T,span = 0.8) +
  geom_point(aes(x= REGX, y=Coef),alpha=0.4,shape=19) +
  geom_smooth(aes(x= REGX, y=Coef, color=as.factor(varnams)),se = F,span = 0.8) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme(legend.position = "bottom") +
  theme(strip.text.x = element_text(size=14, face="bold"),
        strip.text.y = element_text(size=14, face="bold")) +
  theme(legend.text=element_text(size=12)) +
  guides(col = guide_legend(nrow = 4,"Model",title.position = "top"))+#, labeller = labeller(MDL = varnams)) +
  facet_wrap(~YEAR, ncol=2)  + labs(title ="Coefficient estimate ranges by year", x = "Age Group", y = "Estimated Coefficient")  

ggsave("graphics/plot3.bundle.pdf", plot3.bundle, dpi = 300, width = 8, height = 10)


##plot 3 for the bundled models in the substantive model
plot3.bundle.2 <- ggplot(subset(Ages.bundle, Ages.bundle$MODEL == "Model C"), aes(x = Var, y = Coef, ymin = lower, ymax = upper)) + theme_bw() + #geom_linerange(alpha=0.3) +
  # geom_point(aes(x= Var, y=Coef),alpha=0.3,color="red") +
  geom_point(aes(x= Var, y=Coef, color=as.factor(sig0.05)),alpha=0.4) +
  scale_colour_manual(values = c("red", "black"),
                      guide = guide_legend(title = "Significance at p<0.05", face = "bold",size=10)) +
  geom_smooth(aes(x= REGX, y=Coef),size=0.2,se = F, method = "lm", color="black", alpha=0.1) +
  geom_smooth(aes(x= REGX, y=Coef, group=as.factor(YEAR)),se = T,span = 0.5,alpha=0.1, color = NA) +
  geom_smooth(aes(x= REGX, y=Coef),size=2,se = F,span = 0.5) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme(legend.position = "bottom") +
  theme(strip.text.x = element_text(size=14, face="bold"),
        strip.text.y = element_text(size=14, face="bold")) +
  theme(legend.text=element_text(size=12)) +
  # guides(col = guide_legend(nrow = 4,"Model",title.position = "top"))+#, labeller = labeller(MDL = varnams)) +
  # facet_wrap(~YEAR, ncol=2)  + 
  labs(title ="", x = "Age Group", y = "Estimated Coefficient")  

ggsave("graphics/figure1.pdf", plot3.bundle.2, dpi = 300, width = 12, height = 8)





#alternative plots
          plot4 <- ggplot(subset(Ages, Ages$MDL == "B_age_climate" | Ages$MDL == "F_age_climate_housingSize"), aes(x = Var, y = Coef)) + geom_bar(stat = "identity",aes(fill=Var)) + 
            facet_grid(YEAR~ MDL)  + labs(title ="", x = "", y = "") + 
            coord_polar(theta = "x", direction=1 ) + 
            theme(axis.text.x = element_text(hjust = 1)) +
            theme_bw() +
            theme(axis.text.x = element_text(size=6)) +
            theme(axis.text.y = element_text(size=6)) +
            theme(legend.position='none')
          
          
          ggsave("graphics/plot4-2.png", plot4, dpi = 500, width = 7, height = 12)
          
          
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
          
          
