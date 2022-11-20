## ----setup, include=FALSE------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, warning = FALSE)


## ----libs, message=FALSE, warning=FALSE----------------------------------------------------------
list.of.packages <-
  c(
    "dplyr",
    "ggplot2",
    "gt", #package for customizing html table view
    "conflicted", #Some functions are named the same way across different packages and they can be conflicts and this package helps sorting this problem 
    "here", #helps with the reproducibility across operating systems=
    "reshape2", # reshaping the data
    "plotly", #  another advanced visualization tool 
    "pbapply", # Parallelization libraries
    "parallel"
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


#if install is not working try changing repositories
#install.packages("ROCR", repos = c(CRAN="https://cran.r-project.org/"))


## ------------------------------------------------------------------------------------------------
map1 <- matrix(0,10,10)
initial_inf <- 3



## ------------------------------------------------------------------------------------------------
set.seed(123) 


## ----get_data------------------------------------------------------------------------------------
initial_inf <- 3
(row1 <- sample(1:10,size=initial_inf, replace=T))
(col1 <- sample(1:10, size=initial_inf, replace=T))


## ------------------------------------------------------------------------------------------------
for(j in 1:initial_inf) {
  map1[row1[j],col1[j]] <- map1[row1[j],col1[j]] + 1}
map1


## ------------------------------------------------------------------------------------------------
map1_long <- 
reshape2::melt(map1, varnames = c("rows", "cols")) 
# See first rows onf melted matrix
head(map1_long)


## ----initial_vis, out.width="100%"---------------------------------------------------------------
point_size <- 11
 map1_long %>% 
  ggplot(aes(factor(cols), factor(rows), 
             color = value,
             label = as.character(value)))+
  geom_point(size = point_size, shape = 15)+
  geom_text(colour = "black")+
  scale_color_gradient("Health status (counts of infected plants):",
                        low = "lightgreen", high = "#993300")+
  theme_bw()+
  theme(axis.title = element_blank(),
        legend.position = "top",
        panel.background = element_rect(terrain.colors(5)[3]),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  coord_equal()


## ------------------------------------------------------------------------------------------------
PlotMat <- function(data, 
                    point_size = 11 # default value
                    ){
reshape2::melt(data, varnames = c("rows", "cols")) %>% 
  ggplot(aes(factor(cols), factor(rows), 
             color = value,
             label = as.character(value)))+
  geom_point(size = point_size, shape = 15)+
  geom_text(colour = "black")+
  scale_colour_gradient("Health status (counts of infected plants):",
                        low = "lightgreen", high = "#993300")+
  theme_bw()+
  theme(axis.title = element_blank(),
        legend.position = "top",
        panel.background = element_rect(terrain.colors(5)[3]),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  coord_equal()
}


## ------------------------------------------------------------------------------------------------
map2 <- map1
for(xrow in 1:10){
  for(xcol in 1:10){
    numprop <- map1[xrow,xcol]     
    if(xrow > 1){
      map2[xrow-1,xcol] <- map2[xrow-1,xcol] + numprop}
    if(xrow < 10){
      map2[xrow+1,xcol] <- map2[xrow+1,xcol] + numprop}
    if(xcol < 10){
      map2[xrow,xcol+1] <- map2[xrow,xcol+1] + numprop}
    if(xcol > 1){
      map2[xrow,xcol-1] <- map2[xrow,xcol-1] + numprop}
    map2[xrow,xcol] <- map2[xrow,xcol] + numprop
  }
}


## ----fun_map_vis, out.width="100%"---------------------------------------------------------------
lapply(list(map1,map2), PlotMat)


## ------------------------------------------------------------------------------------------------
#Assign the number of generations of dispersal to be studied 
num_gen <- 6


## ------------------------------------------------------------------------------------------------
mapn <- as.list(1:num_gen)


## ------------------------------------------------------------------------------------------------
for(j in 1:num_gen){mapn[[j]] <- matrix(0,10,10)}


## ------------------------------------------------------------------------------------------------
#For each of the next maps
mapn[[1]] <- map1
names(mapn)[1] <- "1"
for(j in 2:num_gen){
  #Temporarily make the map the same as one generation back
  mapn[[j]] <- mapn[[j-1]]
  #Then add the new infections following the rooks' moves
  for(xrow in 1:10){
    for(xcol in 1:10){
      numprop <- mapn[[j-1]][xrow,xcol]     
      if(xrow > 1){mapn[[j]][xrow-1,xcol] <- 
                     mapn[[j]][xrow-1,xcol] + numprop}
      if(xrow < 10){mapn[[j]] [xrow+1,xcol] <- 
                      mapn[[j]][xrow+1,xcol] + numprop}
      if(xcol < 10){mapn[[j]] [xrow,xcol+1] <- 
                      mapn[[j]][xrow,xcol+1] + numprop}
      if(xcol > 1){mapn[[j]] [xrow,xcol-1] <- 
                     mapn[[j]][xrow,xcol-1] + numprop}
      mapn[[j]][xrow,xcol] <- mapn[[j]][xrow,xcol] + numprop
    }
  }
  names(mapn)[j] <- j
}



## ------------------------------------------------------------------------------------------------
plots <- lapply(mapn, PlotMat)


## ----multiple_map_vis, out.width="100%", results='hide',fig.keep='all'---------------------------
plots


## ----plotly, out.width="70%", fig.height=6.5, fig.align='center'---------------------------------
mapn_plot <- list()
for(i in seq(mapn)){
  mapn_plot[[i]] <- 
  reshape2::melt(mapn[[i]]) %>% 
    mutate(time = names(mapn[i]))
}

gg <- 
mapn_plot %>% 
  bind_rows() %>% 
  ggplot(aes(factor(Var1), factor(Var2), 
             color = value,
             label = as.character(value)))+
  geom_point(aes(frame = time), size = point_size, shape = 15)+
  scale_colour_gradient("Health status (counts of infected plants):",
                        low = "lightgreen", high = "#993300")+
  theme_bw()+
  theme(axis.title = element_blank(),
        legend.position = "none",
        panel.background = element_rect(terrain.colors(5)[3]),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

plotly::ggplotly(gg) 


## ------------------------------------------------------------------------------------------------
nstart <- 20


## ------------------------------------------------------------------------------------------------
row1 <- runif(n=nstart, min=0, max=1000)
col1 <- runif(n=nstart, min=0, max=1000)


## ----hist, cache=TRUE, fig.show="hold", out.width="33%"------------------------------------------
hist(rexp(n=1000, rate=2),col="blue",xlim =range(0:13), ylim = range(0:600))
hist(rexp(n=1000, rate=1),col="blue",xlim =range(0:13), ylim = range(0:600))
hist(rexp(n=1000, rate=0.5),col="blue", xlim =range(0:13), ylim = range(0:600))


## ------------------------------------------------------------------------------------------------
coord <- matrix(runif(n=2*nstart, min=0, max=1000),
                nrow=nstart, ncol=2)  
ntot <- nstart
nnew <- 10
ngen <-  3
exppar <-  0.2


## ------------------------------------------------------------------------------------------------
disperse2 <- function(nstart, nnew, ngen, exppar){
  coord <- matrix(runif(n=2*nstart, min=0, max=1000),
                  nrow=nstart, ncol=2)  
  ntot <- nstart
  for(i in 1:ngen){
    for(j in 1:ntot){
      tempx <- coord[j,1]
      tempy <- coord[j,2]
      newdir <- runif(n=nnew, min=-pi, max=pi)
      newdist <- rexp(n=nnew, rate=exppar)
      newx <- tempx + newdist * sin(newdir)
      newy <- tempy + newdist * cos(newdir)
      newcoord <- cbind(newx,newy)
      coord <- rbind(coord,newcoord)
    }
    ntot <- length(coord[,1])
  }
  coord
}



## ------------------------------------------------------------------------------------------------
(combos <-
  expand.grid(rate = c(.05, .1, .5),
              gen = c(1, 2, 3, 4, 5)))


## ----simulations, cache=TRUE---------------------------------------------------------------------
sim_ls  <- list()

(start_time_single <- Sys.time())

for(i in seq(1:nrow(combos))){
  sim_ls[[i]] <- 
  disperse2(nstart=20, nnew=10, 
            ngen=combos[i,2], 
            exppar= combos[i,1]) %>% 
  as_tibble() %>% 
    mutate(rates =paste0("rate = ", combos[i,1]),
           ngen = paste0("gen = ", combos[i,2]))
  # Error checking 
  # Sometimes it is useful to know which operations in the look are finished
  # to be able to determine where the problems lies if an error occurs 
  print(i)
}

# Check the time difference 
end_time_single <- Sys.time() 



## ----simulations_multi---------------------------------------------------------------------------
# Split the combos data frame into a list 
combos_ls <-
  split(combos, 1:nrow(combos))

#calculate the time needed for the calculations
(start_time_multi <- Sys.time())

# Detect the number of cores 
cores <- detectCores()

# sometimes we do not want to use all cores if we are using the machine for other operations
# cores <- ifelse(detectCores() > 1, detectCores()-1, 1) 

# Make a cluster of cores
cl <- makeCluster(cores)

# Export functions used in calculations
clusterExport(cl, c("combos_ls", "sim_ls", "disperse2", "sim_ls"))

# Export libraries needed for computation
clusterEvalQ(cl, library("dplyr", quietly = TRUE, verbose = FALSE))

sim_ls <- 
pblapply(combos_ls, function(x){
  disperse2(nstart=20, nnew=10, 
            ngen=x[,2], 
            exppar= x[,1]) %>% 
    as_tibble() %>% 
    mutate(rates =paste0("rate = ", x[ ,1]),
           ngen = paste0("gen = ", x[ ,2]))
}, cl = cl)

# Check the time difference 
end_time_multi <- Sys.time() 



## ------------------------------------------------------------------------------------------------
stopCluster(cl)


## ------------------------------------------------------------------------------------------------
saveRDS(sim_ls, here::here("6_spatial","simulation.rds"))


## ----time_dif------------------------------------------------------------------------------------
# Duration of single core default R calculation 
end_time_single- start_time_single

# Duration of multic0re calculation
end_time_multi- start_time_multi


## ----final_plot, out.width= "100%"---------------------------------------------------------------
sim_ls %>% 
  bind_rows() %>% 
ggplot(aes(newx, newy))+
  ylim(0,1000)+
  xlim(0,1000)+
  geom_point(size = .3)+
  facet_grid(rates~ngen)+
  theme_bw()+
  xlab("East-West")+
  ylab("South-North")+
  coord_equal()

