# Shiny Budget

A Shiny app for budgeting and keeping track of personal finances. 

## Overview

The first aim of this app is to provide a simple way to keep track of your spending. The **Spending Tracker** page includes plots to help you better understand your spending habits, which can in turn help build your budget. It looks like this:

![Spending Tracker](./screenshots/spending_tracker.PNG)

The budget itself is fully customizable. You decide on the categories and how much to set aside for each one, and the app shows you how you're doing:

![Budget](./screenshots/budget.PNG)

New expenses can uploaded in bulk or added one at a time directly from within the app. Data is automatically saved between sessions, so no need to re-upload. 

The app includes built-in documentation, which can be accessed by clicking the question mark circle on each page. For example, here's the first one:

![Help Page](./screenshots/help_page.PNG)

## Installation

The easiest way to install is by navigating to the [Releases](https://github.com/grasskind/shiny-budget/releases) tab and downloading the zipped program folder (**deploy-shiny-budget.zip**). This was created using the [DesktopDeployR](https://github.com/wleepang/DesktopDeployR) framework. Following the download, unzip in your preferred location and double-click on the **shiny_budget** file to launch. App should start in your browser. This method only works on Windows unfortunately. I'll try to compile the program into a proper desktop app in the future, but that requires learning Inno Setup...

Alternatively, you can clone this repo and run the app either in R Studio or directly from an R console with `shiny::runApp("PATH/TO/APP/", launch.browser=TRUE)`. The path should be the path to the folder containing `server.R` and `ui.R`. This does require installing the appropriate packages beforehand. To install packages, open R and run the following command: `install.packages(c("shiny", "shinyhelper", "data.table", "DT", "openxlsx", "magrittr", "shinydashboard", "dashboardthemes","ggplot2"))`. If you get an error from the `runApp` command because you don't have a launch browser set, try running `options(browser = 'firefox')`.

Happy Budgeting :) 
