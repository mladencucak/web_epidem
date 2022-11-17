
### GGPLOT2 ###

# Load the data
data_demo <- readr::read_csv("Data/data_demo.csv")
data_demo


library(ggplot2) # load ggplot2 and other tidyverse package
# library(tidyverse) # you can also load ggplot2 with all the others tidyverse packages

# The first plot
ggplot() # Blank plot


# Adding data to the plot
ggplot(data = data_demo, aes(x = trt, y = yld)) # aes is short for aesthetics


# Now we can inform how to plot 

ggplot(data = data_demo, aes(x = trt, y = yld)) + # Note: we use the "+" sign to connect lines of code 
  geom_point()

# Points with a little of random noise
ggplot(data = data_demo, aes(x = trt, y = yld)) +
  geom_jitter()


# Less horizontal distribution
ggplot(data = data_demo, aes(x = trt, y = yld)) + 
  geom_jitter(width = .2) # the default value is 0.4, we reduce the dispersion by half


## Boxplot
ggplot(data = data_demo, aes(x = trt, y = yld)) + 
  geom_boxplot() 

## Violin
ggplot(data = data_demo, aes(x = trt, y = yld)) + 
  geom_violin() 



## Dotplot

ggplot(data = data_demo, aes(x = trt, y = yld)) + 
  geom_dotplot(binaxis = "y", stackdir = "center") 


# MULTIPLE LAYERS
# Boxplot and points
ggplot(data = data_demo, aes(x = trt, y = yld)) +
  geom_boxplot()+ 
  geom_jitter(width = .2, size = 4) 

# The order matters 
ggplot(data = data_demo, aes(x = trt, y = yld)) +
  geom_boxplot()+ 
  geom_jitter(width = .2, size = 4)+
  geom_violin() # Note that the violin plot is the last layer to be add, therefore it will overlap with the other layers


ggplot(data = data_demo, aes(x = trt, y = yld)) +
  geom_violin()+
  geom_boxplot(width = 0.3)+ # using the argument width we can adjust the box size
  geom_jitter(width = .2, size = 4)
  

# Adding other components (color)
ggplot(data = data_demo, aes(x = trt, y = yld, color = trt)) +
  geom_boxplot()+ 
  geom_jitter(width = 0.2, size = 4)
  
ggplot(data = data_demo, aes(x = trt, y = yld)) +
  geom_boxplot()+ 
  geom_jitter(aes(color = trt), width = 0.2, size = 4) # different colors for each observation/point depending on the treatment
  

# Adding other components (fill)
ggplot(data = data_demo, aes(x = trt, y = yld)) +
  geom_boxplot(aes(fill = trt))+
  geom_jitter(width = 0.2, size = 4)
  


# Adding other components (Alpha - transparence)
ggplot(data = data_demo, aes(x = trt, y = yld)) +
  geom_boxplot()+
  geom_jitter(aes(alpha = yld), # Here we are using alpha to show a yield gradient 
              width = 0.2, size = 4) # increase the point size to see the differences better 
  

ggplot(data = data_demo, aes(x = trt, y = yld)) +
  geom_violin(color = "red")+ # border color equal to red
  geom_boxplot(width = 0.3, fill = "green")+ # fill equal to green
  geom_jitter(alpha = 0.4, # 60% of transparency
              width = 0.2, size = 4) 
  

# Adding other components (size)
ggplot(data = data_demo, aes(x = trt, y = yld)) +
  geom_boxplot()+ 
  geom_jitter(aes(size = sev), #The size of the point changes depending on the severity value
              width = 0.2)
  

# Adding other components (Shapes)
ggplot(data = data_demo, aes(x = trt, y = yld)) +
  geom_boxplot()+ 
  geom_jitter(aes(shape = trt), 
              width = 0.2, size = 4)
  

## Several components
ggplot(data = data_demo, aes(x = trt, y = yld)) +
  geom_boxplot()+ 
  geom_jitter(aes(color = trt, # point border color
                  fill = var), # point fill defined by the variety 
              shape = 21, # we can change fill and color on shape 21 to 25
              stroke = 1.5, # to make point border thicker
              width = 0.2, size = 4)

# when we talk about scale we will learn how to chose the colors
# so far we are using default ggplot options  


# We use pipes to summarize the data by treatment and variety
library(dplyr)
data_line <- data_demo %>% 
  group_by(trt, var) %>% 
  summarise(sev = mean(sev))  
data_line

# Then we create a plot with different line types
  ggplot(data_line) +
  geom_line(aes(x = trt,
                y = sev, 
                linetype = var, # each variety (R or S) will have a different line type
                group = var),   # group is telling R that it should group the data by variety
                size = 1)  # 




##  SCALES  ##

# Playing with axis
ggplot(data = data_demo, aes(x = trt, y = yld)) +
  geom_boxplot()+ 
  geom_jitter(aes(fill = trt), shape = 21, width = 0.2, size = 4, show.legend = FALSE)+
  
  scale_y_continuous( # because our y-axis variable is continuous
                  name = "Title of our axis", # name of the axis
                  limits = c(70, 130),    # here we increase a little our axis limits
                  breaks = c(seq(70,130,10))) # secondary axis breaks




## Second axis
ggplot(data = data_demo, aes(x = trt, y = yld)) +
  geom_boxplot()+ 
  geom_jitter(aes(fill = trt), shape = 21, width = 0.2, size = 4, show.legend = FALSE)+
  
  scale_y_continuous(name = "Yield (bu/ac)", 
                     limits = c(70, 130),
                     breaks = c(seq(70,130,10)),
                     
            # Here we add a second axis
            sec.axis = sec_axis( # if we only want to duplicate an axis, we can use sec.axis = dup_axis())
                                trans = ~ . *0.0672, # we use a transformation to change yield from  bu/ac to ton/ha
                                name =  "Yield (ton/ha)", # secondary axis name
                                breaks = c(seq(4.5,9,0.5)))) # secondary axis breaks


ggplot(data = data_demo, aes(x = trt, y = yld)) +
  geom_boxplot()+ 
  geom_jitter(aes(fill = trt), shape = 21, width = 0.2, size = 4, show.legend = FALSE)+
  
  scale_x_discrete( 
              limits = c("B", "A", "D")) # here we change the order of treatments and exclude the treatment C
   


ggplot(data = data_demo, aes(x = trt, y = yld)) +
  geom_boxplot()+ 
  geom_jitter(aes(fill = trt), shape = 21, width = 0.2, size = 4, show.legend = FALSE)+
  
  scale_x_discrete( 
            labels = c('B' = 'New Label', # pProvide to ggplot new label information for each variable
                       'D' = 'New Label \n in two lines')) # you can use the "\n" to break the sentence in more lines


# Controlling fill

ggplot(data = data_demo, aes(x = trt, y = yld)) +
  geom_boxplot()+ 
  geom_jitter(aes(fill = trt), 
              shape = 21, width = 0.2, size = 4)+
  scale_fill_manual(values = c("blue", # color name
                               "gray67", # color name, but from a different gray intensity
                               "#9f3b6c", # use RGB code to assign a color
                               "#9f3b6c80")) # same color as trt C, but with transparency


ggplot(data = data_demo, aes(x = trt, y = yld)) +
  geom_boxplot()+ 
  geom_jitter(aes(fill = trt), 
              shape = 21, width = 0.2, size = 4)+
  scale_fill_brewer(palette = "Paired") # This scale automatic identify the number of classes and used the defined pallet to fill the colors



install.packages("ggsci")
library(ggsci)

ggplot(data = data_demo, aes(x = trt, y = yld)) +
  geom_boxplot()+ 
  geom_jitter(aes(fill = trt), 
              shape = 21, width = 0.2, size = 4)+
  scale_fill_npg() # This scale automatically identifies the number of classes and uses the defined pallet to fill the colors



## gradient 2 colors

ggplot(data = data_demo, aes(x = trt, y = yld)) +
  geom_boxplot()+ 
  geom_jitter(aes(fill = fdk), 
              shape = 21, width = 0.2, size = 4)+
  scale_fill_gradient(low = "green",
                      high = "red", 
                      na.value = "blue")




## gradient 3 colors

ggplot(data = data_demo, aes(x = trt, y = yld)) +
  geom_boxplot()+ 
  geom_jitter(aes(fill = fdk), 
              shape = 21, width = 0.2, size = 4)+
  
  scale_fill_gradient2(low = "blue", # This is the low value
                       mid = "yellow", # middle value
                       high = "red",  # high value
                       midpoint = 21, # set up the middle point, otherwise will use the default "0"
                       na.value = "black") # we can also change the color of NAs values

# Note, ggplot2 distributes the color symmetric from the middle point, so, depending on where the middle point is, it is possible that low or high color values may not be reached. 



## gradient n colors

ggplot(data = data_demo, aes(x = trt, y = yld)) +
  geom_boxplot()+ 
  geom_jitter(aes(fill = fdk), 
              shape = 21, width = 0.2, size = 4)+
  
  scale_fill_gradientn(colours = c("#FF0000", "#FFFF00", "#00FF00",
                                   "#00FFFF", "#0000FF", "#FF00FF")) 



# Scale alpha
ggplot(data = data_demo, aes(x = trt, y = yld)) +
  geom_boxplot()+ 
  geom_jitter(aes(alpha = fdk), 
              width = 0.2, size = 4)+
  
  scale_alpha_continuous(range = c(0.3, 0.6)) 


ggplot(data = data_demo, aes(x = trt, y = yld)) +
  geom_boxplot()+ 
  geom_jitter(aes(alpha = fdk), 
              width = 0.2, size = 4)+
  
  geom_jitter(data = filter(data_demo, is.na(fdk)), # filter the data such that there are only the two obs with NA values
              aes(x = trt, y = yld), # position
              shape = 8, # we will define a very different shape for NA
              width = 0.2, height = 0, size = 4)+ 
  
  scale_alpha_continuous(range = c(0.4, 1), # our alphas will change from 0.4 to 1
                         na.value = 0)  # Make NAs values completely transparent


# Scale Size

ggplot(data = data_demo, aes(x = trt, y = yld)) +
  geom_boxplot()+ 
  geom_jitter(aes(size = fdk), 
              width = 0.2)+
  
  scale_size_continuous(range = c(1, 6)) # this is actually the default value 
                                       

ggplot(data = data_demo, aes(x = trt, y = yld)) +
  geom_boxplot() + 
  geom_jitter(aes(size = fdk), 
              width = 0.2) + 
  
  # same as previous example to deal with the NA problem
  # warning will continue to show up, but the NAs are now added
  geom_jitter(data = filter(data_demo, is.na(fdk)), 
              aes(x = trt, y = yld), 
              shape = 8, 
              width = 0.2, height = 0, size = 4) + 
  
  scale_size_continuous(range = c(0.5, 5), 
                        trans = "reverse")  # this reverses the size argument, meaning small values have a larger size
                                       
# Scale Shape
ggplot(data = data_demo, aes(x = trt, y = yld)) +
  geom_boxplot()+ 
  geom_jitter(aes(shape = var), # We will use the factor, variety, to define the different shapes
              width = 0.2, size = 4)+
  
  scale_shape_manual(values = c(8, 15)) # Here we defined two different shapes depending on the variety


# Scale linetype
# preparing the data for our plot
line_yld <- data_demo %>% 
  group_by(trt, var) %>% 
  summarize(yld = mean(yld))
line_yld

ggplot(line_yld) +
  geom_line(aes(x = trt,
                y = yld, 
                linetype = var, # each variety (R or S) will have a different line type
                group = var),   
                size = 1) 


  ggplot(line_yld) +
  geom_line(aes(x = trt, y = yld, 
                linetype = var, 
                group = var),   
                size = 1) +
  
  scale_linetype_manual(values = c("twodash", "dashed"))

# ggplot has a lot of options for linetypes  
line_d <-  data.frame(
  linetype = factor(1:6,
    labels = c("solid", "dashed", "dashed", "dotdash", "longdash", "twodash"))) 

ggplot(line_d) +
  geom_hline(aes(linetype = linetype, yintercept = 0), size = 2) +
  scale_linetype_identity() +
  facet_grid(linetype ~ .) +
  theme_void(20)



# Multiple plots (facets)

# facet_wrap
ggplot(data = data_demo, aes(x = trt, y = yld)) +
  geom_boxplot()+ 
  geom_jitter(aes(fill = trt), shape = 21, width = 0.2, size = 4)+
  scale_fill_npg() +
  
  facet_wrap(~var)



ggplot(data = data_demo, aes(x = trt, y = yld)) +
  geom_boxplot(outlier.alpha = 0)+ # we define the outliers as completely transparent

  # Note: By making these outliers observations transparent, they are still considered in the calucations for boxplots,
  # at same time, they are not duplicated in the plot with the geom_jitter
  
  geom_jitter(aes(fill = trt), shape = 21, width = 0.2, size = 4)+
  scale_fill_npg() +
  
  facet_wrap(~var)


# Change the strip labels
ggplot(data = data_demo, aes(x = trt, y = yld)) +
  geom_boxplot(outlier.alpha = 0)+ #outliers defined to be completely transparent
  geom_jitter(aes(fill = trt), shape = 21, width = 0.2, size = 4)+
  scale_fill_npg() +
  
  facet_wrap(~var, 
             scales = "free_y", # scales will be independent if free, or only one dimension if free_y/x
             labeller = labeller( # here is an example of how to change the 
                      var = c("R" = "Resistent", # need to remember the order of the call to define the variables
                              "S" = "Susceptible"))) # then give a new name for each level


ggplot(data = data_demo, aes(x = trt, y = yld)) +
  geom_boxplot(outlier.alpha = 0)+ # outliers are defined to be completely transparent
  geom_jitter(aes(fill = trt), shape = 21, width = 0.2, size = 4)+
  scale_fill_npg() +
  
  facet_wrap(~var, labeller = labeller(var = c("R" = "Resistent", "S" = "Susceptible")), 
             ncol = 1, nrow = 2) # this defines how the graphics are displayed


# facet_grid
data_grid <- data_demo %>% 
    # create a new variable by splitting the incidence using a 10% threshold
    mutate(inc_cat = if_else(inc <=10, "Low Incidence", "High Incidence"))
  
ggplot(data = data_grid, aes(x = trt, y = yld)) +
  geom_boxplot(outlier.alpha = 0)+ 
  geom_jitter(aes(fill = trt), shape = 21, width = 0.2, size = 4)+
  scale_fill_npg() +
  facet_grid(inc_cat~var, # inc_cat = row; var=column
             labeller = labeller(var = c("R" = "Resistent", "S" = "Susceptible")))


ggplot(data = data_demo, aes(x = trt, y = yld)) +
  geom_boxplot(outlier.alpha = 0)+
  geom_jitter(aes(fill = trt), shape = 21, width = 0.2, size = 4)+
  scale_fill_npg() +
  facet_grid(.~var) # grid by column


ggplot(data = data_demo, aes(x = trt, y = yld)) +
  geom_boxplot(outlier.alpha = 0)+ # outliers defined to be completely transparent
  geom_jitter(aes(fill = trt), shape = 21, width = 0.2, size = 4)+
  ggsci::scale_fill_npg() +
  facet_grid(var~.) # grid by row


### Labels - axis, legends, titles, and more

ggplot(data = data_demo, aes(x = trt, y = yld)) +
  geom_boxplot(outlier.alpha = 0)+ 
  geom_jitter(aes(fill = trt), shape = 21, width = 0.2, size = 4)+
  scale_fill_npg() +
  
  labs(
    # labels for the plots in general
    title = "Here is a title",
    subtitle = "Here is the plot subtitle",
    caption = "This is figure caption",
    tag = "A", 
    
    # Labels for the aesthetics 
    y = "y-axis title",
    x = "x-axis title",
    fill = "fill title"
    
  )



### Change the theme (non-data appearance)  ###

# pre-defined themes
ggplot(data = data_demo, aes(x = trt, y = yld)) +
  geom_boxplot(outlier.alpha = 0) + 
  geom_jitter(aes(fill = trt), shape = 21, width = 0.2, size = 4) +
  scale_fill_npg() +
  labs(y = "Yield (bu/ac)", x = "Treatments", fill = "Treatments") +
  # only enter with the theme name
   theme_bw() # theme black and white


ggplot(data = data_demo, aes(x = trt, y = yld)) +
  geom_boxplot(outlier.alpha = 0)+ 
  geom_jitter(aes(fill = trt), shape = 21, width = 0.2, size = 4)+
  scale_fill_npg() +
  labs(y = "Yield (bu/ac)", x = "Treatments", fill = "Treatments")+
  # only enter with the theme name
   theme_minimal() 

ggplot(data = data_demo, aes(x = trt, y = yld)) +
  geom_boxplot(outlier.alpha = 0)+ 
  geom_jitter(aes(fill = trt), shape = 21, width = 0.2, size = 4)+
  scale_fill_npg() +
  labs(y = "Yield (bu/ac)", x = "Treatments", fill = "Treatments")+
  # only enter with the theme name
  theme_classic() 



## --------------------------------------------------------------------------------------------------------------------
# there are packages with theme libraries, such as the package ggthemes
install.packages("ggthemes")
library(ggthemes)

ggplot(data = data_demo, aes(x = trt, y = yld)) +
  geom_boxplot(outlier.alpha = 0)+ 
  geom_jitter(aes(fill = trt), shape = 21, width = 0.2, size = 4)+
  scale_fill_npg() +
  labs(y = "Yield (bu/ac)", x = "Treatments", fill = "Treatments")+
  # only enter with the theme name
  theme_economist()


# This is the default theme (theme_grey)

ggplot(data = data_demo, aes(x = trt, y = yld)) +
  geom_boxplot(outlier.alpha = 0)+ 
  geom_jitter(aes(fill = trt), shape = 21, width = 0.2, size = 4)+
  scale_fill_npg() +
  
  facet_grid(~var, labeller = labeller(var = c("R" = "Resistent", "S" = "Susceptible")))+
  
  labs(y = "Yield (bu/ac)", x = "Treatments", fill = "Treatments") +
  
  theme(
    # theme function is empty, so will plot the default theme
  )


# Here some possible changes from the base theme

ggplot(data = data_demo, aes(x = trt, y = yld)) +
  geom_boxplot(outlier.alpha = 0)+ 
  geom_jitter(aes(fill = trt), shape = 21, width = 0.2, size = 4)+
  scale_fill_npg() +
  
  facet_grid(~var, labeller = labeller(var = c("R" = "Resistent", "S" = "Susceptible")))+
  
  labs(y = "Yield (bu/ac)", fill = "Treatments", 
       x = NULL) + # we suppress the x-axis title, since it will be in the legend
  
   theme(
     # change the background color to white
      panel.background = element_blank() , 
    
     # the default color is white, so we have to change to gray to be different from the background
      panel.grid.major = element_line(colour = "grey88"), 
     
     # the default theme does not have a border
       panel.border = element_rect(colour = "grey80", fill = NA),
    
    # axis text to black (was a little gray)
    axis.text.y  = element_text(colour = "black"),
    #suppress the x-axis text, since it will be in the legend
    axis.text.x  = element_blank(),
    
    # Change titles (y-axis and legend[fill]) to black text, size 12, and bold
    title = element_text(colour = "black", size = 12, face = "bold"),

    # bring the legend to inside of the plot
    legend.position = c(0.90, .20),
    # Legend key was gray, change to white
    legend.key = element_rect(fill = "white"),
    # Legend text attribute
    legend.text = element_text(colour = "black", size = 12),
    
    # Strip text justified almost completely to the left - (hjust = 0.01)
    # hjust = 0.5 is center justification, and hjust = 1 is right justification
    strip.text.x =  element_text(hjust =0.01, face = "bold", size = 14),
    
    # strip text background 
    strip.background =  element_blank())



## --------------------------------------------------------------------------------------------------------------------

# plots not display to save space (will be in the combined plot)

plot_yield <- 
ggplot(data = data_demo, aes(x = trt, y = yld)) +
  geom_boxplot(outlier.alpha = 0)+ 
  geom_jitter(aes(fill = trt), shape = 21, width = 0.2, size = 4)+
  scale_fill_npg() +
  labs(y = "Yield (bu/ac)", x = NULL)+
  theme(
      axis.text  = element_text(colour = "black"),
      title = element_text(colour = "black", size = 12, face = "bold"),
      legend.position = "none")

plot_sev <- 
ggplot(data = data_demo, aes(x = trt, y = sev)) +
  geom_boxplot(outlier.alpha = 0)+ 
  geom_jitter(aes(fill = trt), shape = 21, width = 0.2, size = 4)+
  scale_fill_npg() +
  labs(y = "Severity (%)", x = NULL)+
  theme(
      axis.text  = element_text(colour = "black"),
      title = element_text(colour = "black", size = 12, face = "bold"),
      legend.position = "none")

plot_inc <- 
ggplot(data = data_demo, aes(x = trt, y = inc)) +
  geom_boxplot(outlier.alpha = 0)+ 
  geom_jitter(aes(fill = trt), shape = 21, width = 0.2, size = 4)+
  scale_fill_npg() +
  labs(y = "Incidence (%)", x = NULL)+
  theme(
      axis.text  = element_text(colour = "black"),
      title = element_text(colour = "black", size = 12, face = "bold"),
      legend.position = "none")

plot_fdk <- 
ggplot(data = data_demo, aes(x = trt, y = fdk)) +
  geom_boxplot(outlier.alpha = 0)+ 
  geom_jitter(aes(fill = trt), shape = 21, width = 0.2, size = 4)+
  scale_fill_npg() +
  labs(y = "FDK (%)", x = NULL)+
  theme(
      axis.text  = element_text(colour = "black"),
      title = element_text(colour = "black", size = 12, face = "bold"),
      legend.position = "none")


## Combine plots

# There multiple plots that can handle combination of plots.
# The package patchwork is probably one of the best and simplest
install.packages("patchwork")
library(patchwork)

# just use the "+" signal to combine the plots
plot_yield+plot_sev

# or the "/" to put one on top of the other
plot_yield/plot_sev

# use "()" to create a hierarchical order of combinations
(plot_yield+plot_sev) / # first combine yield and severity, followed by stacking incidence and fdk 
(plot_inc + plot_fdk)

# use "()" to create a hierarchical order of combinations
(plot_yield+plot_sev) / # first combine yield and severity, followed by stacking incidence and fdk
(plot_inc + plot_fdk) +
  
  # add a tag for each plot, there way to customize this
    plot_annotation(tag_levels = "A")  & # very important, use "&" to make theme changes
  theme(
    axis.text.x  = element_text(colour = "red")) # red to make an obvious change

# Save your plot


# The plot display in RStudio may not reflect the one save (because the screen resolution)
# The best way to save a plot is using the finction ggsave() where you can control the dimention and resolution

 ggsave(plot = plot_yield, # name of the plot in R
        filename = "1_intro_to_R/figures/example12.png", # plot will be saved in the folder figures, with name example1, format png
        width = 85, height = 80, units = "mm", # plot dimension
        dpi = 450 ) # resolution

