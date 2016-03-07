source("Analysis.R")

##LEt's plot'em out!

##First lookin at only age variables:

Ages <- subset(OUTPUT, OUTPUT$Var %in% c("age05","age05to09","age10to14","age15to19","age20to24","age25to29",
                                         "age30to34","age35to39","age40to44","age45to49","age50to54","age55to59",
                                         "age60to64","age65to69","age70to74","age75to79","age80p"))



ggplot()+geom_point(data=Ages, aes(x=Var, y=Coef, fill = MDL))+
  facet_wrap(~ YEAR)



ggplot(Ages, aes(x = Var, y = Coef)) + theme_bw() + geom_bar(stat = "identity",aes(fill=Coef)) + facet_grid(YEAR~ MDL)  + labs(title =" New title", x = "New x", y = "New y") 
ggplot(Ages, aes(x = Var, y = Coef)) + theme_bw() + geom_bar(stat = "identity") + facet_grid(YEAR~ MDL)   + labs(title =" New title", x = "New x", y = "New y") 

ggplot(Ages, aes(x = Var, y = Coef)) + theme_bw() + geom_bar(stat = "identity",aes(fill=Var)) + facet_grid(YEAR~ MDL)  + labs(title =" New title", x = "New x", y = "New y") + coord_polar(theta = "x", direction=1 )



ggplot(Ages, aes(x = Var, y = Coef)) + theme_bw() + geom_boxplot() + facet_wrap(~ MDL) + labs(title =" New title", x = "New x", y = "New y") 
ggplot(Ages, aes(x = Var, y = Coef)) + theme_bw() + geom_boxplot(fill = "gray") + facet_wrap(~ YEAR) + labs(title =" New title", x = "New x", y = "New y") 


ggplot(Ages, aes(x = Var, y = Coef, ymin = lower, ymax = upper)) + theme_bw() + geom_errorbar() + facet_grid(YEAR~ MDL)   + labs(title =" New title", x = "New x", y = "New y") 


ggplot(Ages, aes(x = Var, y = Coef, ymin = lower, ymax = upper)) + theme_bw() + geom_pointrange() + facet_grid(YEAR~ MDL)  + labs(title =" New title", x = "New x", y = "New y") 

