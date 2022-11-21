## ----setup, include=FALSE------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, warning = FALSE)


## ----libs, message=FALSE, warning=FALSE,class.source = 'fold-show'-------------------------------
list.of.packages <-
  c(
    "dplyr",
    "tidyr",
    "ggplot2",
    "conflicted", #Some functions are named the same way across different packages and they can be conflicts and this package helps sorting this problem 
    "here", #helps with the reproducibility across operating systems=
    "plotly" #  another advanced visualization tool 
  )

new.packages <-
  list.of.packages[!(list.of.packages %in% installed.packages()[, "Package"])]

#Download packages that are not already present in the library
if (length(new.packages))
  install.packages(new.packages)

# Load packages
packages_load <-
  lapply(list.of.packages, require, character.only = TRUE)

#Print warning if there is a problem with installing/loading some of packages
if (any(as.numeric(packages_load) == 0)) {
  warning(paste("Package/s: ", paste(list.of.packages[packages_load != TRUE], sep = ", "), "not loaded!"))
} else {
  print("All packages were successfully loaded.")
}
rm(list.of.packages, new.packages, packages_load)

#Resolve conflicts
if(c("stats", "dplyr")%in% installed.packages()){
  conflict_prefer("filter", "dplyr")
  conflict_prefer("select", "dplyr")
  
}

conflict_prefer("layout", "plotly")

#if install is not working try changing repositories
#install.packages("ROCR", repos = c(CRAN="https://cran.r-project.org/"))


## ----func,class.source = 'fold-show'-------------------------------------------------------------
EstNfcf <- function (M,mean,CV) {
  return((1-mean)/(mean*CV^2))
}
EstFcf <- function (M,mean,CV) {
  Nest<-(1-mean)/(mean*CV^2)
  finite<-Nest/M
  Nsample<-ifelse(finite>0.1,Nest/(1+(Nest/M)),Nest)
  return(round(Nsample,digits=1))
}
NestM <- function (M,mean,CV) {
  Nest<-(1-mean)/(mean*CV^2)
  finite<-Nest/M
  return(round(finite, digits=2))
}



## ----data_prep, fig.width=9, fig.height=7.5,class.source = 'fold-show'---------------------------
gg <- 
as.data.frame(expand.grid(
  mean =seq(.1, .9, .1),
  CV =seq(.05, .3, .01), 
  M =  c(100, 500, 1000)
  ) ) %>% 
  mutate( 
         est_nfcf = EstNfcf(M,mean,CV),
         est_fcf = EstFcf(M,mean,CV),
         nest_m = NestM(M,mean,CV)
  ) %>% 
  #Multiplication factor(in brackets) to make the differences obvious on the plot
  mutate("NEST/M(X100)" = nest_m *100,
         "Estimated-FCF(X5)" = est_fcf*5,
         "Estimated-NFCF" = est_nfcf) %>% 
  pivot_longer(cols = c("Estimated-NFCF", "Estimated-FCF(X5)", "NEST/M(X100)")) %>% 
ggplot(aes(CV, value, group = mean, color = mean))+
  geom_point(size = .5)+
  geom_line()+
  scale_color_viridis_c()+
  facet_grid(M~name)+
  theme_bw()+
  theme(legend.position = "top")




## ----plotlyplot, fig.width=9, fig.height=7.5,class.source = 'fold-show'--------------------------
plotly::ggplotly(gg) %>% 
  plotly::layout(legend = list(orientation = "h", x = 0.4, y = -0.2))


## ----func2---------------------------------------------------------------------------------------
SRS.PROPORTION.1 <- function (mean,z,H) {
  Nest<-((1-mean)/mean)*(z/H)^2
  names(Nest)<-"SampleSize"
  return(Nest)
}



## ----plotlyplot2, fig.width=6, fig.height=5------------------------------------------------------
gg <-
  as.data.frame(expand.grid(
    mean = seq(.05, .3, .05),
    z = 1.96,
    H =  seq(.1, .5, .05)
  )) %>%
  mutate(SampleSize = SRS.PROPORTION.1(mean, z, H)) %>%
  ggplot(aes(H, SampleSize, group = mean, color = mean)) +
  geom_point(size = .5) +
  geom_line() +
  scale_color_viridis_c() +
  theme_bw()

plotly::ggplotly(gg) 


## ----func3---------------------------------------------------------------------------------------
EstNfcf <- function (M,mean,z,H) {
  return(((1-mean)/mean)*(z/H)^2)
}
EstFcf <- function (M,mean,z,H) {
  Nest<-((1-mean)/mean)*(z/H)^2
  finite <-Nest/M
  Nsample <-ifelse(finite>0.1,Nest/(1+(Nest/M)),Nest)
  return(round(Nsample,digits=1))
}
NestM <- function (M,mean,z,H) {
  Nest <-((1-mean)/mean)*(z/H)^2
  finite <-Nest/M
  return(round(finite, digits=2))
}



## ----data_prep3, fig.width=9, fig.height=7.5-----------------------------------------------------
gg <- 
  as.data.frame(expand.grid(
    mean = seq(.05, .3, .05),
    z = 1.96, 
    H =  seq(.1,.5,.05),
    M =1000
  ) ) %>% 
  mutate( 
    est_nfcf = EstNfcf(M,mean,z,H),
    est_fcf = EstFcf(M,mean,z,H),
    nest_m = NestM(M,mean,z,H)
  ) %>% 
  mutate("NEST/M" = nest_m ,
         "Estimated-FCF" = est_fcf,
         "Estimated-NFCF" = est_nfcf) %>% 
  pivot_longer(cols = c("Estimated-NFCF", "Estimated-FCF", "NEST/M")) %>% 
  ggplot(aes(H, value, group = mean, color = mean))+
  geom_point(size = .5)+
  geom_line()+
  scale_color_viridis_c()+
  facet_wrap(~name, ncol = 1, scales = "free_y")+
  theme_bw()+
  theme(legend.position = "top")


## ----plotlyplot3, fig.width=9, fig.height=7.5----------------------------------------------------
plotly::ggplotly(gg) %>% 
  plotly::layout(legend = list(orientation = "h", x = 0.4, y = -0.2))


## ----func4---------------------------------------------------------------------------------------
SRS.PROPORTION.1 <- function (mean,z,h) {
  Nest<-(mean*(1-mean))*(z/h)^2
  names(Nest)<-"SampleSize"
  return(Nest)
}


## ----plotlyplot4, fig.width=6, fig.height=5------------------------------------------------------
gg <- 
as.data.frame(expand.grid(
  mean = seq(.05, .3, .05),
  z = 1.96, 
  h =  seq(.1,.5,.05)
) ) %>% 
  mutate( 
    SampleSize = SRS.PROPORTION.1(mean,z,h)
  ) %>% 
  ggplot(aes(h, SampleSize, group = mean, color = mean))+
  geom_point(size = .5)+
  geom_line()+
  scale_color_viridis_c()+
  theme_bw()

plotly::ggplotly(gg) 


## ----func5---------------------------------------------------------------------------------------
EstNfcf <- function (mean, n, CV, M) {
  return(round((1 - mean) / (n * mean * CV ^ 2), 2))
}
EstFcf <- function (mean, n, CV, M) {
  Nest <- (1 - mean) / (n * mean * CV ^ 2)
  finite <- Nest / M
  Nsample <- ifelse(finite > 0.1, Nest / (1 + (Nest / M)), Nest)
  return(round(Nsample, digits = 1))
}
NestM <- function (mean, n, CV, M) {
  Nest <- (1 - mean) / (n * mean * CV ^ 2)
  finite <- Nest / M
  return(round(finite, digits = 2))
}



## ----data_prep5, fig.width=9, fig.height=7.5-----------------------------------------------------
gg <-
as.data.frame(expand.grid(
  mean =seq(.1, .9, .1),
  n = 10,
  CV =seq(.05, .3, .01), 
  M =  c(100, 500, 1000)
)) %>%
  mutate(
    est_nfcf = EstNfcf(mean, n, CV, M),
    est_fcf = EstFcf(mean, n, CV, M),
    nest_m = NestM(mean, n, CV, M)
  ) %>%
  mutate("NEST/M(X100)" = nest_m *100,
         "Estimated-FCF" = est_fcf,
         "Estimated-NFCF" = est_nfcf) %>% 
  pivot_longer(cols = c("Estimated-NFCF", "Estimated-FCF", "NEST/M(X100)")) %>% 
  ggplot(aes(CV, value, group = mean, color = mean))+
  geom_point(size = .5)+
  geom_line()+
  scale_color_viridis_c()+
  facet_grid(M~name)+
  theme_bw()+
  theme(legend.position = "top")


## ----plotlyplot5, fig.width=9, fig.height=7.5----------------------------------------------------
plotly::ggplotly(gg) %>% 
  plotly::layout(legend = list(orientation = "h", x = 0.4, y = -0.2))


## ----func6---------------------------------------------------------------------------------------
CLUSTER.RANDOM.2 <- function (mean, n, z, H) {
  Nest<-((1-mean)/(n*mean))*(z/H)^2
  names(Nest)<-"SampleSize"
  return(Nest)
}



## ----plotlyplot6, fig.width=6, fig.height=5------------------------------------------------------
gg <-
  as.data.frame(expand.grid(
    mean = seq(.05, .3, .05),
    n = 10,
    z = 1.96,
    H =  seq(.1, .5, .05)
  )) %>%
  mutate(SampleSize = CLUSTER.RANDOM.2(mean, n, z, H)) %>%
  ggplot(aes(H, SampleSize, group = mean, color = mean)) +
  geom_point(size = .5) +
  geom_line() +
  scale_color_viridis_c() +
  theme_bw()

plotly::ggplotly(gg) 


## ----func7---------------------------------------------------------------------------------------
CLUSTER.RANDOM.3 <- function(mean, n, z, h) {
  Nest <- ((mean * (1 - mean)) / n) * (z / h) ^ 2
  names(Nest) <- "SampleSize"
  return(Nest)
}



## ----plotlyplot7, fig.width=6, fig.height=5------------------------------------------------------
gg <- 
as.data.frame(expand.grid(
  mean = seq(.05, .3, .05),
  n = 10,
  z = 1.96, 
  h =  seq(.01,.1,.01)
) ) %>% 
  mutate( 
    SampleSize = CLUSTER.RANDOM.3(mean,n, z,h)
  ) %>% 
  ggplot(aes(h, SampleSize, group = mean, color = mean))+
  geom_point(size = .5)+
  geom_line()+
  scale_color_viridis_c()+
  theme_bw()

plotly::ggplotly(gg) 

