
#library(tidyverse)

library(ggplot2)
library(dplyr)
library(tidyr)
library(patchwork)
library(knitr)
library(kableExtra)
library(ggsci)
library(ggpubr)


# Load the data

data_rate  <-  readxl::read_xlsx("2_disiease_assessment/data/severity_rates.xlsx") %>%
  pivot_longer(cols = -c("set", "leaf_id", "order", "truth"), 
               names_to = "raters") %>% 
  group_by(set, leaf_id) %>% 
  mutate(dev = value-truth,
         set = factor(set, levels = c(1, 2), labels = c("set_1", "set_2")))

head(data_rate)


# Readings per rater
ggplot(data_rate)+
  geom_boxplot(outlier.alpha = 0, aes(x = raters, y = value))+
  geom_jitter(aes(fill = raters, x = raters, y = value), width = 0.2,
              show.legend = FALSE, shape = 21, color = "black", size = 2)+
  geom_rug(aes(y = truth))+
  
  ggsci::scale_fill_npg()+
  scale_y_continuous(limits = c(0,100))+
  scale_x_discrete(expand =  c(.1,0))+
  
  labs(y = "Severity (%)", x = NULL, 
       title = "Disease severity (%) distribution per rater and set",
       subtitle = "Each point represents a leaf reading \nRugs in the plot left are the true values")+
  
  facet_wrap(~set, 
             labeller = labeller(set = c(set_1 = "Set 1", set_2 = "Set 2")))+  
  theme(
    panel.background = element_blank() ,
    panel.grid.major = element_line(colour = "grey95") ,
    panel.border     = element_rect(linetype = "solid",colour = "grey80", size=0.3, fill = NA),
    
    axis.text  = element_text(colour = "black"),
    plot.title =  element_text(hjust =0, face = "bold", size = rel(1.2)),
    
    strip.text.x =  element_text(hjust =0, face = "bold", size = rel(1.5)),
    strip.background =  element_blank())


## ---- Intra-rater reliability ----
 data_intra_rate  <-   
  data_rate %>% ungroup() %>% 
    select(set, raters, leaf_id, truth, value) %>% 
    pivot_wider(names_from = set, values_from = value) 
 
 ggplot(data_intra_rate) +
   geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
   geom_smooth(aes(x= set_1, y = set_2, color = raters),
               show.legend = FALSE, method="lm", se = FALSE)+  
   
   geom_point(aes(x= set_1, y = set_2, fill = raters), size = 3, 
              show.legend = FALSE, shape = 21, color = "black")+

   ggsci::scale_color_npg()+
   ggsci::scale_fill_npg()+
   scale_x_continuous(limits = c(0,100))+
   scale_y_continuous(limits = c(0,100))+  
   
   labs(x = "Ratings from the first set", 
        y = "Ratings from the second set", 
        title = "Relationship between dissease assessment from Set 1 and Set 2", 
        subtitle = "Dashed line is the line or perfect agreement, solid line is the linear regression")+
   
   facet_wrap(~raters, ncol = 3)+
   theme( panel.background = element_blank() ,
          panel.grid.major = element_line(colour = "grey95") ,
          panel.border     = element_rect(linetype = "solid",colour = "grey80", size=0.3, fill = NA),
     
          axis.text  = element_text(colour = "black"),
          plot.title =  element_text(hjust =0, face = "bold", size = rel(1.2)),
         
          strip.text.x =  element_text(hjust =0, face = "bold", size = rel(1.5)),
          strip.text.y =  element_text(face = "bold", size = rel(1.5)),
          strip.background =  element_blank())
 


 ## ---- Intra-rater Stats ----
 table_intra_rater  <-  
 data_intra_rate %>% 
   group_by(raters) %>% 
   summarise( set_m1 = mean(set_1),
              sd1 = sd(set_1),
              set_m2 = mean(set_2),
              sd2 = sd(set_2),
              r   = cor(set_1, set_2) ) %>%
mutate(v  = (sd1/sd2),
          u  = (set_m1-set_m2)/sqrt(sd1*sd2), 
          cb = 2/((v+(1/v)+u**2)),
          pc = r*cb) %>% 
   mutate_at(c("set_m1", "sd1", "set_m2", "sd2"), round, digits=1) %>% 
   mutate_at(c("r", "v", "u", "cb", "pc"), round, digits=2)

 table_intra_rater
 
 # below the table in HTML (optional)
 # kable(table_intra_rater, align = "lcccccccc", 
 #       col.names = c("Rater", "Mean Set1", "SD Set1", "Mean Set2", "SD Set2", "*r*", 
 #                      "$\\nu$", "$\\mu$", "$C_b$", "$\\hat{\\rho_c}$")) %>% 
 # kable_styling("striped")  
 


 
 
 
 
 
 ## ---- Intra-rater reliability ----
 data_inter_rate  <-  
data_rate %>% 
  ungroup() %>% 
  select(-dev, -order) %>% 
  filter(set =="set_1") %>% 
  pivot_wider(names_from = raters, values_from = value) %>% 
  select(-set, -leaf_id, - truth) %>% 
  pivot_longer(-R2, names_to = "raters") %>% 
  mutate(comp = paste(raters, "vs R2"))

  ggplot(data_inter_rate) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  geom_smooth(aes(x=R2, y = value, color = comp),
              show.legend = FALSE, method="lm", se = FALSE)+  
  
  geom_point(aes(x=R2, y = value, fill = comp), size = 3, 
             show.legend = FALSE, shape = 21, color = "black")+
  
  ggsci::scale_color_npg()+
  ggsci::scale_fill_npg()+
  scale_x_continuous(limits = c(0,100))+
  scale_y_continuous(limits = c(0,100))+  
  
  labs(y = "Disease severity (%)", x = "Disease severity (%) rating by R2", 
       title = "Relationship between disease severity ratings from six raters and R2",
       subtitle = "Rater R2 showed the best intra-rater reliability")+
    
  facet_wrap(~comp, ncol = 2)+
  theme( panel.background = element_blank() ,
         panel.grid.major = element_line(colour = "grey95") ,
         panel.border     = element_rect(linetype = "solid",colour = "grey80", size=0.3, fill = NA),
         
        plot.title =  element_text(hjust =0, face = "bold", size = rel(1.2)),
         axis.text  = element_text(colour = "black"),
         
         strip.text.x =  element_text(hjust =0, face = "bold", size = rel(1.5)),
         strip.text.y =  element_text(face = "bold", size = rel(1.5)),
         strip.background =  element_blank())



## ---- Intra-rater stats ----
  table_intra_rater <- 
    data_inter_rate %>% 
    group_by(raters) %>% 
    summarise( set_m1 = mean(value),
               sd1 = sd(value),
               set_m2 = mean(R2),
               sd2 = sd(R2),
               r   = cor(value, R2) ) %>%
    mutate(v  = (sd1/sd2),
           u  = (set_m1-set_m2)/sqrt(sd1*sd2), 
           cb = 2/((v+(1/v)+u**2)),
           pc = r*cb) %>% 
    mutate_at(c("set_m1", "sd1", "set_m2", "sd2"), round, digits=1) %>% 
    mutate_at(c("r", "v", "u", "cb", "pc"), round, digits=2) %>% 
    select(-set_m2, -sd2)  
  table_intra_rater
  
  
# kable(table_intra_rater, align = "lcccccc", 
#         col.names = c("Rater", "Mean Raters", "SD Raters", "*r*", 
#                       "$\\nu$", "$\\mu$", "$C_b$", "$\\hat{\\rho_c}$")) %>% 
#     kable_styling("striped")
 


## ---- Accuracy ----

ggplot(data_rate, aes(x = raters, y = dev))+
  geom_boxplot(outlier.alpha = 0)+
  geom_jitter(aes(fill = raters), width = 0.2,
              show.legend = FALSE, shape = 21, color = "black", size = 2)+  
  
  ggsci::scale_fill_npg()+
  scale_y_continuous(limits = c(-50,50))+

  facet_wrap(~set, 
             labeller = labeller(set = c(set_1 = "Set 1", set_2 = "Set 2")))+
  
  labs(x = "Raters", y = "Deviation from true value")+
  
  theme(
    panel.background = element_blank() ,
    panel.grid.major = element_line(colour = "grey95") ,
    panel.border     = element_rect(linetype = "solid",colour = "grey80", size=0.3, fill = NA),
    
    axis.text  = element_text(colour = "black"),
    
    strip.text.x =  element_text(hjust =0, face = "bold", size = rel(1.5)),
    strip.background =  element_blank())


 ## ---- Accuracy - plots per rater ----
  
acc_data <- 
  data_rate %>%  ungroup() %>% 
    filter(set =="set_1") %>% 
    select(truth, raters, value) %>% 
     group_by(raters)

ggplot(acc_data) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  geom_smooth(aes(x=truth, y = value, color = raters),
              show.legend = FALSE, method="lm", se = FALSE)+  
  
  geom_point(aes(x=truth , y = value, fill = raters), size = 3, 
             show.legend = FALSE, shape = 21, color = "black")+
  
  ggsci::scale_color_npg()+
  ggsci::scale_fill_npg()+
  scale_x_continuous(limits = c(0,100))+
  scale_y_continuous(limits = c(0,100))+  
  
  labs(x = "Disease severity (%) true value", 
       y = "Rates estimate by raters")+
  
  facet_wrap(~raters, ncol = 3)+
  theme( panel.background = element_blank() ,
         panel.grid.major = element_line(colour = "grey95") ,
         panel.border     = element_rect(linetype = "solid",colour = "grey80", size=0.3, fill = NA),
         
         axis.text  = element_text(colour = "black"),
         
         strip.text.x =  element_text(hjust =0, face = "bold", size = rel(1.5)),
         strip.text.y =  element_text(face = "bold", size = rel(1.5)),
         strip.background =  element_blank())



## ---- Accuracy - Stats ----
table_acc_data <- acc_data %>% 
  group_by(raters) %>% 
  summarise( set_m1 = mean(value),
             sd1 = sd(value),
             set_m2 = mean(truth),
             sd2 = sd(truth),
             r   = cor(value, truth)) %>%
  mutate(v  = (sd1/sd2),
         u  = (set_m1-set_m2)/sqrt(sd1*sd2), 
         cb = 2/((v+(1/v)+u**2)),
         pc = r*cb) %>% 
  mutate_at(c("set_m1", "sd1", "set_m2", "sd2"), round, digits=1) %>% 
  mutate_at(c("r", "v", "u", "cb", "pc"), round, digits=2)%>% 
  select(-set_m2, -sd2) 
table_acc_data

  
  # kable(table_acc_data, align = "lcccccc", 
  #       col.names = c("Rater", "Mean Raters", "SD Raters", "*r*", 
  #                     "$\\nu$", "$\\mu$", "$C_b$", "$\\hat{\\rho_c}$")) %>% 
  # kable_styling("striped")



