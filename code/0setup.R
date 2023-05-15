# Install packages
# install.packages(c(
#     'here', 'tidyverse', 'lubridate', 'data.table', 'logitr', 'fastDummies',
#     'directlabels', 'cowplot', 'ggbump', 'ggrepel', 'janitor', 'likert',
#     'gtsummary', 'knitr', 'flextable', 'remotes', 'logitr', 'cbcTools'
# ))
#remotes::install_github("jhelvy/maddTools")

# Data cleaning
library(here)
library(tidyverse)
library(lubridate)
library(data.table)
library(janitor)

# Modeling
library(logitr)
library(fastDummies)
library(maddTools)

# Survey design
library(cbcTools)

# Plotting / reporting
library(directlabels)
library(cowplot)
library(ggbump)
library(ggrepel)
library(likert)
library(gtsummary)
library(knitr)
library(flextable)

# View all columns in data frames
options(dplyr.width = Inf)

# Set random seed
set.seed(1234)
