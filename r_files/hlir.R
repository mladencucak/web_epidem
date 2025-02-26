## ----setup, include=FALSE------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ----libs, message=FALSE, warning=FALSE----------------------------------------------------------
# R convenience packages
library("conflicted")
library("here") #helps with the reproducibility across operating systems

# Data mundging 
library("dplyr")

# App building pckages
library("shiny")
library("shinythemes")
library("DT")
library("shinydashboard")

# Computation
library("deSolve")  # Solvers for Initial Value Problems of Differential Equations

# Plotting 
library("ggplot2")

# Packages with same functions can cause problems 
 conflict_prefer("filter", "dplyr")


## ----hlir_app, echo=FALSE------------------------------------------------------------------------
shiny::shinyAppDir(
  here::here( "5_HLIR/apps"),
  options = list(
    width = "100%", height = 900
  )
)


