## ----libs, message=FALSE, warning=FALSE---------------------------------------
list.of.packages <-
  c(
    "readxl", # import of data from excel files
    "tidyverse",
    "gt", #package for customising html table view
    "conflicted", #some functions are named the same way across different packages and there can be conflicts. This package helps to sort out  this problem 
    "here", #helps with the reproducibility across operating systems
    "agricolae" #set of functions for agricultural research
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


#If install is not working try changing repositories
#install.packages("ROCR", repos = c(CRAN="https://cran.r-project.org/"))



## ----data_one-----------------------------------------------------------------
dta <- 
readxl::read_xlsx(
  here::here("3_temporal_progress/data/sample_data.xlsx"),
  sheet = "1",
  col_types = rep("numeric",5) # define all columns are numeric
  )

library(readxl)
(dta <- 
  read_excel("3_temporal_progress/data/sample_data.xlsx", 
                          col_types = c("numeric", "numeric", "numeric", 
                                        "numeric", "numeric")))
dta
View(sample_data)

#string manipulation - rename variables according to naming conventions
colnames(dta) <-  tolower(colnames(dta))#fun tolower makes all letters small
# shorten names
colnames(dta) <- substr( colnames(dta),
                         start = 1,
                         stop = 4)
# make abbreviations more memorable
colnames(dta)[colnames(dta) == "infe"] <- "inf"



## ----data---------------------------------------------------------------------
# Normally, to call variables in dataframe,  we have to 
dta$inf #or dta[ ,"inf"] or dta[["inf"]]
attach(dta)
inf # callout variables within dataframe without defining the parent object
rate
gt::gt(dta) 


## ----init_fig, figures-side, fig.show="hold", out.width="50%"-----------------

plot(dai, inf, # columns to plot
     ylab="Number of inf plants", #Change label
     type="b", lty=1, pch=19, lwd=3, xlim=c(0,max(dai))) #Graphical parameters
points(dai, rate, type="b", lty=1, pch=19, lwd=3,col = "darkgreen")
mtext("Cumulative",1.9,at=55,-10) #Add text
mtext("Counts",1.9, at=60,-4, col = "darkgreen") 

plot(dai,prop, type="b", lty=1, pch=19, lwd=3)
points(dai,rate,type="b",lty=1, pch=19,lwd=3, col = "darkgreen")
mtext("Cumulative",1.9,at=55,-10) #Add text
mtext("Counts",1.9, at=60,-4, col = "darkgreen") 


## -----------------------------------------------------------------------------
detach(dta)


## ----sub_dta------------------------------------------------------------------
sub_dta <- dta[1:4, ]


attach(sub_dta)


## ----audpc_one----------------------------------------------------------------
# Adjoin two x-coordinates
x.poly <- c( dai,  dai[length( dai)],  dai[1])         
y.poly <- c( prop, 0, 0)  
plot(dai, prop,
     ylab="Number of infected plants",
     type="b", lty=1, pch=19, lwd=3, 
     xlim=c(6,max(dai)),ylim = c(0, .13))
# Draw the polygon showing AUDPC
 polygon(x.poly, y.poly,col=gray(0.95), border=NA)
points(dai, prop, lty=1, pch=19, lwd=3)
# Draw rectangles - notice four points as in input
rect(dai[1],0,dai[2],((prop[1]+prop[2])/2),border="orange")
rect(dai[2],0,dai[3],((prop[2]+prop[3])/2),border="orange")
rect(dai[3],0,dai[4],((prop[3]+prop[4])/2),border="orange")

mtext("AUDPC",1,at=20,-2.3, cex = 2)



## -----------------------------------------------------------------------------
plot(dai, prop, 
     ylab="Number of infected plants",
     type="b", lty=1, pch=19, lwd=3, 
     xlim=c(6,max(dai)),
     ylim = c(0, .13))
rect(dai[1],0,dai[2],((prop[1]+prop[2])/2), border="orange")


## -----------------------------------------------------------------------------
i <- 1 # We are working on the 1st rectangle

#create a numeric vector to be populated with AUDPC values for each triangle 
audpc_scores <- numeric()  

(audpc_scores[i] <- 
    #Find midpoint between two assessments and multiply with time difference between assessments
c((prop[i] + prop[i + 1])/2) * (dai[i + 1] - dai[i])
  )


## -----------------------------------------------------------------------------
i <- 2
(audpc_scores[i] <- 
c((prop[i] + prop[i + 1])/2) * (dai[i + 1] - dai[i])
  )

i <- 3
(audpc_scores[i] <- 
c((prop[i] + prop[i + 1])/2) * (dai[i + 1] - dai[i])
  )



## -----------------------------------------------------------------------------
audpc_scores
sum(audpc_scores)


## -----------------------------------------------------------------------------
# How many rectangles? 
(rectangles <- 1: c(length(dai)-1))

# make an empty vector to store values  
audpc_scores <- numeric()

for (i in rectangles) {

  print(i)
}

for (i in rectangles) {
  # i will change from 1 to 3 and assign the value for the areas of each rectangle
  (audpc_scores[i] <-
     #Find midpoint between two assessments and multiply with time difference between assessments
     c((prop[i] + prop[i + 1]) / 2) * (dai[i + 1] - dai[i]) )
}


## -----------------------------------------------------------------------------
audpc_scores
sum(audpc_scores)


## -----------------------------------------------------------------------------
CalcAUDPC <-
  function(
    #Arguments:
          times, disease #assign vectors from the full data set as arguments
             ) 
    {# The body of the function lies within curly brackets
    
           audpc_scores <- numeric()
             
            # How many rectangles? 
            rectangles <- 1:c(length(times) - 1)
            #Calculate the area for each rectangle 
            for (i in rectangles) {
             (audpc_scores[i] <-
                c((disease[i] + disease[i + 1]) / 2) * (times[i + 1] - times[i]))
            }
           # The final object to be returned 
           return(sum(audpc_scores))
           }


## -----------------------------------------------------------------------------
CalcAUDPC(times =  dta$dai,
          disease = dta$prop)
#or using pipes 
dta %>% 
  summarise(audpc_val = CalcAUDPC(dai, prop))


## ----errors, eval = FALSE-----------------------------------------------------
  CalcAUDPC(dta$dai)
## # Error in CalcAUDPC(dta$dai) :
## #   argument "disease" is missing, with no default


## -----------------------------------------------------------------------------
(years <-
  readxl::excel_sheets(here::here("3_temporal_progress/data/sample_data.xlsx")))


## -----------------------------------------------------------------------------
dta_list <- 
  lapply(years, function(i){
    fun_dta <- 
    readxl::read_xlsx(
      here::here("3_temporal_progress/data/sample_data.xlsx"),
      sheet = i
    )
    # Create id variable  
    # this coulmn could have been simply attached as the last column
    # but it is more logical to have higher level id's at the beginning
    # notice name yr - because function year exists in several packages
    fun_dta <- 
      add_column(fun_dta, yr = i, .before = "DAI")
    return(fun_dta)
  })


## -----------------------------------------------------------------------------
full_dta <- 
  bind_rows(dta_list)


## -----------------------------------------------------------------------------
colnames(full_dta) <-  tolower(colnames(full_dta)) 
colnames(full_dta) <- substr( colnames(full_dta),start = 1, stop = 4)
colnames(full_dta)[colnames(full_dta) == "infe"] <- "inf"


## -----------------------------------------------------------------------------
(audpc_dta <- 
full_dta %>% 
  group_by(yr) %>% 
  summarise(audpc_score = CalcAUDPC(dai, prop) %>% round(2),
            raudpc_score = audpc(prop, dai, type = "relative")%>% round(4)))


## -----------------------------------------------------------------------------

left_join(full_dta, audpc_dta, by = "yr") %>%
  mutate(yr = 
           paste(yr, "(AUDPC =", audpc_score, "; rAUDPC =", raudpc_score, ")")) %>% 
  ggplot( aes(dai, prop, shape = yr)) +
  geom_point(size = 2) +
  geom_line(linetype = "dashed") +
  theme_bw()+
  theme(legend.position = c(0.75, 0.15))+ #legend into a plotting area
  guides(shape = guide_legend(nrow=2,byrow=TRUE)) # 2 row legend

