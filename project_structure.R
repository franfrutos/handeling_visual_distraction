# Author: Francisco Garre-Frutos

# Script to create the project structure. Run only at the beginning of the project.

# Load relevant packages ----
if (!require(pacman)) {
  install.packages("pacman")
  library(pacman)
}

p_load(here) # package to manage paths relative to the working directory

# Creating project structure ----
# Folders for data and analysis scripts of the main analyses:
dir.create("Input")
dir.create("Output")
dir.create(here("Output/plots"))
dir.create("Scripts")