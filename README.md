# Wellbeing Economy Monitor
The Economy Board requested that OCEA lead on developing a performance framework for the Economy Board, taking into account the framework developed for the Enterprise and Skills Strategic Board.

![Image of the home page](https://github.com/DataScienceScotland/sg-wellbeing-economy-monitor/blob/master/sshot-2020-12-19-20-23-22.png?raw=true)

## 📦 Packages
- library(shiny)                # For the app iteslf.
- library(shinythemes)          # For the "cerulean" theme.
- library(shinyhelper)          # Used for help modal boxes.
- library(shinyEffects)         # Used for home page effects.
- library(shinyanimate)         # Used for home page effects.
- library(shinycssloaders)      # For loaders.
- library(magrittr)             # Dependency of other packages. Lets use the pipe operator (%>%).
- library(data.table)           # Needed for function na.omit().
- library(DT)                   # For the interactive tables.
- library(dygraphs)             # For the interactive graphs.
- library(leaflet)              # For interactive maps.
- library(RColorBrewer)         # Used to create a colour palette for the map.
- library(rgdal)                # Needed to load shapfile for the map.
- library(plyr)                 # Needed for function revalue(), for editing country names.
- library(rmarkdown)            # Used for reports, especially important is function render().
- library(knitr)                # Takes Rmd. file and turns it into .md document which includes the R code and it’s output.
- library(networkD3)            # Used to create the sankey diagram.
- library(treemap)              # Used to create the static treemap.
- library(dplyr)                # Used for data manipulation.
- library(ggplot2)              # Used for plots in country and sector profiles.
- library(plotly)               # Used for the streamgraph.
- library(ggsci)                # Used for colour palettes.
- library(ggflags)              # Expands ggplot with new geom for adding flags.
- library(countrycode)          # Enables conversion from Common country names to ISO codes.
- library(knitr)                # Used for sector definitions table
- library(kableExtra)           # Used for styling the sector definitions table
- library(rsconnect)            # Used for shinyapp.io uploads
- library(tidyr)                # Used for data manipulation
- library(openxlsx)             # Used to read excel spreadsheets
- library(plotly)               # Used for plots

Dependencies:
- library(DBI)
- library(XML)
- library(anytime)
- library(backports)
- library(broom)
- library(cellranger)
- library(classInt)
- library(cowplot)
- library(dbplyr)
- library(dichromat)
- library(flextable)
- library(forcats)
- library(fs)
- library(gdtools)
- library(gtools)
- library(haven)
- library(leafsync)
- library(lubridate)
- library(lwgeom)
- library(modelr)
- library(prettyunits)
- library(progress)
- library(readxl)
- library(reprex)
- library(rgeos)
- library(rintrojs)
- library(rlist)
- library(sf)
- library(shinyBS)
- library(shinyWidgets)
- library(shinyalert)
- library(shinyjs)
- library(tidyverse)
- library(tmap)
- library(tmaptools)
- library(sunburstR)
- library(viridis)

## 📖 Updating with new data
Link to the Word document describing the process of updating the dashboard with new data [here](https://github.com/DataScienceScotland/sg-wellbeing-economy-monitor/blob/master/Updating%20WEM.docx).

## 🔗 Links
* [Wellbeing Economy Monitor (This shiny app)](https://szymkowskidev.shinyapps.io/sg-wellbeing-economy-monitor/)
* [Open Data Platform)](https://statistics.gov.scot/data/search)

## 📧 Contact
[![](https://img.shields.io/twitter/url?label=/SzymkowskiDev&style=social&url=https%3A%2F%2Ftwitter.com%2FSzymkowskiDev)](https://twitter.com/SzymkowskiDev) [![](https://img.shields.io/twitter/url?label=/kamil-szymkowski/&logo=linkedin&logoColor=%230077B5&style=social&url=https%3A%2F%2Fwww.linkedin.com%2Fin%2Fkamil-szymkowski%2F)](https://www.linkedin.com/in/kamil-szymkowski/) [![](https://img.shields.io/twitter/url?label=@szymkowskidev&logo=medium&logoColor=%23292929&style=social&url=https%3A%2F%2Fmedium.com%2F%40szymkowskidev)](https://medium.com/@szymkowskidev) [![](https://img.shields.io/twitter/url?label=/SzymkowskiDev&logo=github&logoColor=%23292929&style=social&url=https%3A%2F%2Fgithub.com%2FSzymkowskiDev)](https://github.com/SzymkowskiDev)
