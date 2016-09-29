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

A_Age5sIN87$cohort <- "a_5sIN87"
                
# unique(OUTPUT$Var)
B_Age9sIN87 <- subset(OUTPUT, (
  (OUTPUT$Var == "age05to09" & OUTPUT$YEAR == 1987) | 
    (OUTPUT$Var == "age10to14" & OUTPUT$YEAR == 1990) |
    (OUTPUT$Var == "age25to29" & OUTPUT$YEAR == 2005) |
    (OUTPUT$Var == "age30to34" & OUTPUT$YEAR == 2009)
)
) 

B_Age9sIN87$cohort <- "b_9sIN87"


C_Age14sIN87 <- subset(OUTPUT, (
  (OUTPUT$Var == "age10to14" & OUTPUT$YEAR == 1987) | 
    (OUTPUT$Var == "age15to19" & OUTPUT$YEAR == 1990) |
    (OUTPUT$Var == "age30to34" & OUTPUT$YEAR == 2005) |
    (OUTPUT$Var == "age35to39" & OUTPUT$YEAR == 2009)
)
) 

C_Age14sIN87$cohort <- "c_14sIN87"

D_Age19sIN87 <- subset(OUTPUT, (
  (OUTPUT$Var == "age15to19" & OUTPUT$YEAR == 1987) | 
    (OUTPUT$Var == "age20to24" & OUTPUT$YEAR == 1990) |
    (OUTPUT$Var == "age35to39" & OUTPUT$YEAR == 2005) |
    (OUTPUT$Var == "age40to44" & OUTPUT$YEAR == 2009)
)
)

D_Age19sIN87$cohort <- "d_19sIN87"


E_Age24sIN87 <- subset(OUTPUT, (
  (OUTPUT$Var == "age20to24" & OUTPUT$YEAR == 1987) | 
    (OUTPUT$Var == "age25to29" & OUTPUT$YEAR == 1990) |
    (OUTPUT$Var == "age40to44" & OUTPUT$YEAR == 2005) |
    (OUTPUT$Var == "age45to49" & OUTPUT$YEAR == 2009)
)
)

E_Age24sIN87$cohort <- "e_24sIN87"

F_Age29sIN87 <- subset(OUTPUT, (
  (OUTPUT$Var == "age25to29" & OUTPUT$YEAR == 1987) | 
    (OUTPUT$Var == "age30to34" & OUTPUT$YEAR == 1990) |
    (OUTPUT$Var == "age45to49" & OUTPUT$YEAR == 2005) |
    (OUTPUT$Var == "age50to54" & OUTPUT$YEAR == 2009)
)
)

F_Age29sIN87$cohort <- "f_29sIN87"


G_Age34sIN87 <- subset(OUTPUT, (
  (OUTPUT$Var == "age30to34" & OUTPUT$YEAR == 1987) | 
    (OUTPUT$Var == "age35to39" & OUTPUT$YEAR == 1990) |
    (OUTPUT$Var == "age50to54" & OUTPUT$YEAR == 2005) |
    (OUTPUT$Var == "age55to59" & OUTPUT$YEAR == 2009)
)
)

G_Age34sIN87$cohort <- "g_34sIN87"

H_Age39sIN87 <- subset(OUTPUT, (
  (OUTPUT$Var == "age35to39" & OUTPUT$YEAR == 1987) | 
    (OUTPUT$Var == "age40to44" & OUTPUT$YEAR == 1990) |
    (OUTPUT$Var == "age55to59" & OUTPUT$YEAR == 2005) |
    (OUTPUT$Var == "age60to64" & OUTPUT$YEAR == 2009)
)
)

H_Age39sIN87$cohort <- "h_39sIN87"

I_Age44sIN87 <- subset(OUTPUT, (
  (OUTPUT$Var == "age40to44" & OUTPUT$YEAR == 1987) | 
    (OUTPUT$Var == "age45to49" & OUTPUT$YEAR == 1990) |
    (OUTPUT$Var == "age60to64" & OUTPUT$YEAR == 2005) |
    (OUTPUT$Var == "age65to69" & OUTPUT$YEAR == 2009)
)
)

I_Age44sIN87$cohort <- "i_44sIN87"

J_Age49sIN87 <- subset(OUTPUT, (
  (OUTPUT$Var == "age45to49" & OUTPUT$YEAR == 1987) | 
    (OUTPUT$Var == "age50to54" & OUTPUT$YEAR == 1990) |
    (OUTPUT$Var == "age65to69" & OUTPUT$YEAR == 2005) |
    (OUTPUT$Var == "age70to74" & OUTPUT$YEAR == 2009)
)
)

J_Age49sIN87$cohort <- "j_49sIN87"

K_Age54sIN87 <- subset(OUTPUT, (
  (OUTPUT$Var == "age50to54" & OUTPUT$YEAR == 1987) | 
    (OUTPUT$Var == "age54to59" & OUTPUT$YEAR == 1990) |
    (OUTPUT$Var == "age70to74" & OUTPUT$YEAR == 2005) |
    (OUTPUT$Var == "age75to79" & OUTPUT$YEAR == 2009)
)
)

K_Age54sIN87$cohort <- "k_54sIN87"

L_Age59sIN87 <- subset(OUTPUT, (
  (OUTPUT$Var == "age55to59" & OUTPUT$YEAR == 1987) | 
    (OUTPUT$Var == "age60to64" & OUTPUT$YEAR == 1990) |
    (OUTPUT$Var == "age75to79" & OUTPUT$YEAR == 2005) |
    (OUTPUT$Var == "age80p" & OUTPUT$YEAR == 2009)
)
)

L_Age59sIN87$cohort <- "l_59sIN87"

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


plotG <- ggplot(GEN_Y, aes(x = Var, y = Coef, color = cohort, group = cohort)) +
  theme_bw() + 
  geom_point()+ #geom_line()+
  # geom_point(aes(color=as.factor(cohort))) +
  facet_wrap(~ MDL, ncol = 4) +
  # geom_linerange(alpha=0.3) +
  geom_smooth(aes(x= REGX, y=Coef),se = F,span = 0.8) +
  guides(col = guide_legend(nrow = 1,"Cohort",title.position = "top")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme(strip.text.x = element_text(size=9, face="bold"),
        strip.text.y = element_text(size=9, face="bold")) +
  theme(legend.position = "bottom") +
  labs(title ="Coefficient estimate ranges by model for the 1987 cohorts", x = "Age Group", y = "Estimated Coefficient")  

ggsave("graphics/plotG.png", plotG, dpi = 500, width = 12, height = 5)

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