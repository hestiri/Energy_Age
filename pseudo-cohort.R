##GEN Ys (87 cohorts) --the generation born in the 1980s and 1990s, 
# comprising primarily the children of the baby boomers and typically 
# perceived as increasingly familiar with digital and electronic technology.

A_Age5sIN87 <- subset(OUTPUT, (
  (OUTPUT$Var == "age05" & OUTPUT$YEAR == 1987) | 
    (OUTPUT$Var == "age05to09" & OUTPUT$YEAR == 1990) |
    (OUTPUT$Var == "age20to24" & OUTPUT$YEAR == 2005) |
    (OUTPUT$Var == "age25to29" & OUTPUT$YEAR == 2009)
)
)

A_Age5sIN87$cohort <- "Cohort 1"
A_Age5sIN87$cohortXL <- "Cohort 01: Age <5 in 1987"


# unique(OUTPUT$Var)
B_Age9sIN87 <- subset(OUTPUT, (
  (OUTPUT$Var == "age05to09" & OUTPUT$YEAR == 1987) | 
    (OUTPUT$Var == "age10to14" & OUTPUT$YEAR == 1990) |
    (OUTPUT$Var == "age25to29" & OUTPUT$YEAR == 2005) |
    (OUTPUT$Var == "age30to34" & OUTPUT$YEAR == 2009)
)
) 

B_Age9sIN87$cohort <- "Cohort 2"
B_Age9sIN87$cohortXL <- "Cohort 02: Age 5-9 in 1987"


C_Age14sIN87 <- subset(OUTPUT, (
  (OUTPUT$Var == "age10to14" & OUTPUT$YEAR == 1987) | 
    (OUTPUT$Var == "age15to19" & OUTPUT$YEAR == 1990) |
    (OUTPUT$Var == "age30to34" & OUTPUT$YEAR == 2005) |
    (OUTPUT$Var == "age35to39" & OUTPUT$YEAR == 2009)
)
) 

C_Age14sIN87$cohort <- "Cohort 3"
C_Age14sIN87$cohortXL <- "Cohort 03: Age 10-14 in 1987"

D_Age19sIN87 <- subset(OUTPUT, (
  (OUTPUT$Var == "age15to19" & OUTPUT$YEAR == 1987) | 
    (OUTPUT$Var == "age20to24" & OUTPUT$YEAR == 1990) |
    (OUTPUT$Var == "age35to39" & OUTPUT$YEAR == 2005) |
    (OUTPUT$Var == "age40to44" & OUTPUT$YEAR == 2009)
)
)

D_Age19sIN87$cohort <- "Cohort 4"
D_Age19sIN87$cohortXL <- "Cohort 04: Age 15-19 in 1987"


E_Age24sIN87 <- subset(OUTPUT, (
  (OUTPUT$Var == "age20to24" & OUTPUT$YEAR == 1987) | 
    (OUTPUT$Var == "age25to29" & OUTPUT$YEAR == 1990) |
    (OUTPUT$Var == "age40to44" & OUTPUT$YEAR == 2005) |
    (OUTPUT$Var == "age45to49" & OUTPUT$YEAR == 2009)
)
)

E_Age24sIN87$cohort <- "Cohort 5"
E_Age24sIN87$cohortXL <- "Cohort 05: Age 20-24 in 1987"

F_Age29sIN87 <- subset(OUTPUT, (
  (OUTPUT$Var == "age25to29" & OUTPUT$YEAR == 1987) | 
    (OUTPUT$Var == "age30to34" & OUTPUT$YEAR == 1990) |
    (OUTPUT$Var == "age45to49" & OUTPUT$YEAR == 2005) |
    (OUTPUT$Var == "age50to54" & OUTPUT$YEAR == 2009)
)
)

F_Age29sIN87$cohort <- "Cohort 6"
F_Age29sIN87$cohortXL <- "Cohort 06: Age 25-29 in 1987"


G_Age34sIN87 <- subset(OUTPUT, (
  (OUTPUT$Var == "age30to34" & OUTPUT$YEAR == 1987) | 
    (OUTPUT$Var == "age35to39" & OUTPUT$YEAR == 1990) |
    (OUTPUT$Var == "age50to54" & OUTPUT$YEAR == 2005) |
    (OUTPUT$Var == "age55to59" & OUTPUT$YEAR == 2009)
)
)

G_Age34sIN87$cohort <- "Cohort 7"
G_Age34sIN87$cohortXL <- "Cohort 07: Age 30-34 in 1987"

H_Age39sIN87 <- subset(OUTPUT, (
  (OUTPUT$Var == "age35to39" & OUTPUT$YEAR == 1987) | 
    (OUTPUT$Var == "age40to44" & OUTPUT$YEAR == 1990) |
    (OUTPUT$Var == "age55to59" & OUTPUT$YEAR == 2005) |
    (OUTPUT$Var == "age60to64" & OUTPUT$YEAR == 2009)
)
)

H_Age39sIN87$cohort <- "Cohort 8"
H_Age39sIN87$cohortXL <- "Cohort 08: Age 35-39 in 1987"


I_Age44sIN87 <- subset(OUTPUT, (
  (OUTPUT$Var == "age40to44" & OUTPUT$YEAR == 1987) | 
    (OUTPUT$Var == "age45to49" & OUTPUT$YEAR == 1990) |
    (OUTPUT$Var == "age60to64" & OUTPUT$YEAR == 2005) |
    (OUTPUT$Var == "age65to69" & OUTPUT$YEAR == 2009)
)
)

I_Age44sIN87$cohort <- "Cohort 9"
I_Age44sIN87$cohortXL <- "Cohort 09: Age 40-44 in 1987"

J_Age49sIN87 <- subset(OUTPUT, (
  (OUTPUT$Var == "age45to49" & OUTPUT$YEAR == 1987) | 
    (OUTPUT$Var == "age50to54" & OUTPUT$YEAR == 1990) |
    (OUTPUT$Var == "age65to69" & OUTPUT$YEAR == 2005) |
    (OUTPUT$Var == "age70to74" & OUTPUT$YEAR == 2009)
)
)

J_Age49sIN87$cohort <- "Cohort 10"
J_Age49sIN87$cohortXL <- "Cohort 10: Age 45-49 in 1987"


K_Age54sIN87 <- subset(OUTPUT, (
  (OUTPUT$Var == "age50to54" & OUTPUT$YEAR == 1987) | 
    (OUTPUT$Var == "age54to59" & OUTPUT$YEAR == 1990) |
    (OUTPUT$Var == "age70to74" & OUTPUT$YEAR == 2005) |
    (OUTPUT$Var == "age75to79" & OUTPUT$YEAR == 2009)
)
)

K_Age54sIN87$cohort <- "Cohort 11"
K_Age54sIN87$cohortXL <- "Cohort 11: Age 50-54 in 1987"

L_Age59sIN87 <- subset(OUTPUT, (
  (OUTPUT$Var == "age55to59" & OUTPUT$YEAR == 1987) | 
    (OUTPUT$Var == "age60to64" & OUTPUT$YEAR == 1990) |
    (OUTPUT$Var == "age75to79" & OUTPUT$YEAR == 2005) |
    (OUTPUT$Var == "age80p" & OUTPUT$YEAR == 2009)
)
)

L_Age59sIN87$cohort <- "Cohort 12"
L_Age59sIN87$cohortXL <- "Cohort 12: Age 55-59 in 1987"


GEN_Y <- rbind(A_Age5sIN87,B_Age9sIN87,C_Age14sIN87,D_Age19sIN87,E_Age24sIN87,F_Age29sIN87,
               G_Age34sIN87,H_Age39sIN87,I_Age44sIN87,J_Age49sIN87,K_Age54sIN87,L_Age59sIN87)

rm(A_Age5sIN87,B_Age9sIN87,C_Age14sIN87,D_Age19sIN87,E_Age24sIN87,F_Age29sIN87,
   G_Age34sIN87,H_Age39sIN87,I_Age44sIN87,J_Age49sIN87,K_Age54sIN87,L_Age59sIN87)





### visualizations for GEN Ys
GEN_Y$Coef <- as.integer(GEN_Y$Coef)

GEN_Y$REGX <- 0
r <- length(unique(GEN_Y$Var))
for (i in 1:r) {
  var <- unique(GEN_Y$Var)[i]
  num <- rank(unique(GEN_Y$Var))[i]
  GEN_Y$REGX <- ifelse(GEN_Y$Var == var, num, GEN_Y$REGX)
}


plotG <-
  ggplot(GEN_Y, aes(x = Var, y = Coef, color = cohortXL, group = cohortXL)) +
  theme_bw() + 
  geom_point()+ #geom_line()+
  # geom_point(aes(color=as.factor(cohort))) +
  facet_wrap(~ varnams, ncol = 2) +
  # geom_linerange(alpha=0.3) +
  geom_smooth(aes(x= REGX, y=Coef),se = F,span = 0.8) +
  guides(col = guide_legend(nrow = 3,"Cohort",title.position = "top")) +
  # guides(col = guide_legend(nrow = 1,"Cohort",title.position = "top")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme(strip.text.x = element_text(size=9, face="bold"),
        strip.text.y = element_text(size=9, face="bold")) +
  theme(legend.position = "bottom") +
  labs(title ="Coefficient estimate ranges by model for 1987 cohorts", x = "Age Group", y = "Estimated Coefficient")  

ggsave("graphics/plot4.pdf", plotG, dpi = 500, width = 9, height = 18)

# 
# ggplot(GEN_Y, aes(x = Var, y = Coef)) + #+ theme_bw() + geom_linerange(alpha=0.3) +
#   geom_smooth(aes(x= REGX, y=Coef, color=as.factor(MDL)),se = T,span = 0.8) +
#   geom_point(aes(x= REGX, y=Coef),alpha=0.4,shape=19) +
#   geom_smooth(aes(x= REGX, y=Coef, color=as.factor(MDL)),se = F,span = 0.8) +
#   theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
#   theme(legend.position = "bottom") +
#   theme(strip.text.x = element_text(size=9, face="bold"),
#         strip.text.y = element_text(size=9, face="bold")) +
#   guides(col = guide_legend(nrow = 4,"Model",title.position = "top")) +
#   facet_wrap(~MDL, ncol=4)  + labs(title ="Coefficient estimate ranges by year", x = "Age Group", y = "Estimated Coefficient")  
# 