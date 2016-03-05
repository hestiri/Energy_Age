source("Analysis.R")

##LEt's plot'em out!
p1 <- ggplot()+geom_point(data=OUTPUT, aes(x=Var, y=Coef, fill = MDL))+
  facet_wrap(~ YEAR)

