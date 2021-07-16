# Shiny Budget

A Shiny app for budgeting and keeping track of personal finances. 

## Installation

The easiest way to install is by downloading the zipped program folder from this google drive [link](https://drive.google.com/drive/folders/15CkyY8tW26NCHrFT0iIcloUtxo4TiqDp?usp=sharing), which was created following the [DesktopDeployR](https://github.com/wleepang/DesktopDeployR) framework. Following the download, unzip in desired location and double-click on the **shiny_budget** file to launch. App should start in your browser. This method only works on Windows unfortunately.

Alternatively, you can clone this repo and run the app either in R Studio or directly from an R console with `shiny::runApp("PATH/TO/APP/", launch.browser=TRUE)`. The path should be the path to the folder containing `server.R` and `ui.R`. This does require installing the appropriate packages beforehand. To install packages, open R and run the following command: `install.packages(c("shiny", "shinyhelper", "data.table", "DT", "openxlsx", "magrittr", "shinydashboard", "dashboardthemes","ggplot2"))`. If you get an error because you don't have a launch browser set, run `options(browser = 'firefox')`.
