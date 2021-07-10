library(ggplot2)

# loads all input data (spending database, budget categories, and color palette)
load_db <- function() {
  # Initialize database if one doesn't exist
  if (file.exists("shiny_database.rds")) {
    shiny_database <- readRDS("shiny_database.rds")
  } else {
    shiny_database <- data.table(name = character(),
                                 description = character(),
                                 category = character(),
                                 price = numeric(),
                                 date = character())
  }
  
  # Do the same for the budget 
  if (file.exists("shiny_budget.rds")) {
    shiny_budget <- readRDS("shiny_budget.rds")
  } else {
    shiny_budget <- data.table(category = character(), 
                               budget = numeric()) 
  }
  
  # And the color palette 
  if (file.exists("shiny_palette.rds")) {
    shiny_palette <- readRDS("shiny_palette.rds")
  } else {
    shiny_palette <- c("#000000", "#FFFF00", "#1CE6FF", "#FF34FF", "#FF4A46", "#008941", "#006FA6", "#A30059", "#FFDBE5", "#7A4900", "#0000A6", "#63FFAC", "#B79762", "#004D43", "#8FB0FF", "#997D87", "#5A0007", "#809693", "#FEFFE6", "#1B4400", "#4FC601", "#3B5DFF", "#4A3B53", "#FF2F80", "#61615A", "#BA0900", "#6B7900", "#00C2A0", "#FFAA92", "#FF90C9", "#B903AA", "#D16100", "#DDEFFF", "#000035", "#7B4F4B", "#A1C299", "#300018", "#0AA6D8", "#013349", "#00846F", "#372101", "#FFB500", "#C2FFED", "#A079BF", "#CC0744", "#C0B9B2", "#C2FF99", "#001E09", "#00489C", "#6F0062", "#0CBD66", "#EEC3FF", "#456D75", "#B77B68", "#7A87A1", "#788D66", "#885578", "#FAD09F", "#FF8A9A", "#D157A0", "#BEC459", "#456648", "#0086ED", "#886F4C", "#34362D", "#B4A8BD", "#00A6AA", "#452C2C", "#636375", "#A3C8C9", "#FF913F", "#938A81", "#575329", "#00FECF", "#B05B6F", "#8CD0FF", "#3B9700", "#04F757", "#C8A1A1", "#1E6E00", "#7900D7", "#A77500", "#6367A9", "#A05837", "#6B002C", "#772600", "#D790FF", "#9B9700", "#549E79", "#FFF69F", "#201625", "#72418F", "#BC23FF", "#99ADC0", "#3A2465", "#922329", "#5B4534", "#FDE8DC", "#404E55", "#0089A3", "#CB7E98", "#A4E804", "#324E72", "#6A3A4C")
  }
  return (list(shiny_database, shiny_budget, shiny_palette))
}

# make a named list of options for a date system
get_date_system_choices <- function() {
  date_system_choices <- c("1900", "1904")
  names(date_system_choices) <- c("1900 (Windows)", "1904 (Mac)")
  return(date_system_choices)
}

##################################### Plotting Functions ##########################################

# Line plot of total spending in a category per month
# data = data.table with columns for date, category, and price
#   date should be in format mm/dd/yyyy
# group = spending category, e.g. "Rent"
# year = Year to make plot for
monthly_spending <- function(data, group, year) {
  # Add a month and year column to dataframe
  data$mon <- month(as.Date(data$date, format = "%m/%d/%y"))
  data$yea <- year(as.Date(data$date, format = "%m/%d/%y"))
  
  # Make a new dataframe with the per month spending in group during year
  pdata <- data[category == group & yea == year, .(month_total = sum(price)), by = mon]
  
  # Add a column with 3 letter abbreviations for each month
  pdata$month <- sapply(pdata$mon, function(x) month.abb[x])
  pdata$month <- factor(pdata$month, levels = month.abb)
  
  # Plot. Title includes average spending over the year
  ggplot(pdata, aes(x = month, y = month_total, group = 1)) + 
    geom_line(size = 1.5) +
    geom_point(size = 2, color = "purple") + 
    expand_limits(y = 0) + 
    ggtitle(paste0("Monthly Spending on ", group,", Average: $", round(mean(pdata$month_total)))) +
    labs(x = "Month", y = "Total Spent") + 
    theme(text=element_text(face="bold"),
          axis.text=element_text(size=12),
          axis.title=element_text(size=14),
          title=element_text(size=16),
          axis.ticks.x = element_blank()
    )
}

# Barplot of average spending over the year in each category. Ordered by spending left to right
# data = data.table with columns for date, category, and price
#   date should be in format mm/dd/yyyy
# year = Year to make plot for
barplot_by_category <- function(data, year) {
  # Add month and year columns
  data$mon <- month(as.Date(data$date, format = "%m/%d/%y"))
  data$yea <- year(as.Date(data$date, format = "%m/%d/%y"))
  groups <- unique(data$category)
  
  # Calculate average spending for the year per category
  year_avg <- function(d, y, group) {
    pd <- d[category == group & yea == year, .(month_total = sum(price)), by = mon]
    return(mean(pd$month_total))
  }
  avg_spending_list <- sapply(groups, function(x) year_avg(data, year, x))
  avg_spending_df <- data.frame(avg_spending = avg_spending_list, 
                                category = names(avg_spending_list),
                                row.names = NULL)
  
  # Order the categories by spending
  avg_spending_df$category <- factor(avg_spending_df$category, 
                                     levels = avg_spending_df$category[order(avg_spending_df$avg_spending, decreasing = T)])
  
  plt <- ggplot(avg_spending_df, aes(x = category, y = avg_spending, fill = category)) +
    geom_col() + 
    ggtitle(paste0("Average Spending by Category in ", year)) +
    xlab("Category") +
    ylab("Spending ($)") +
    theme(text=element_text(face="bold"),
          axis.text=element_text(size=12),
          axis.title=element_text(size=14),
          title=element_text(size=16),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank()) +
    geom_text(aes(label=round(avg_spending)), position=position_dodge(width=0.9), vjust=-0.25)
  
  return(plt)
}

# Barplot of spending per category during a month of interest
# data = data.table with columns for date, category, and price
#   date should be in format mm/dd/yyyy
# mon = month to plot spending for. Should be in month.name.
# year = Year to make plot for, should be numeric
month_totals_bp <- function(data, mon, year) {
  mon.index <- which(month.name == mon)
  
  # Add month and year columns
  data$month <- month(as.Date(data$date, format = "%m/%d/%y"))
  data$yea <- year(as.Date(data$date, format = "%m/%d/%y"))
  
  # Make a new dataframe with the spending per category
  pdata <- data[month == mon.index & yea == year, .(month_total = sum(price)), by = category]
  
  # Order categories by spending
  pdata$category <- factor(pdata$category,
                           levels = pdata$category[order(pdata$month_total,decreasing = T)])
  
  # Plot. Title includes total spending
  ggplot(pdata, aes(x = category, y = month_total, fill = category)) + 
    geom_col() +
    ggtitle(paste0("Spending in ", mon," of ", year, ", Total: ", sum(pdata$month_total))) +
    xlab("Category") +
    ylab("Spending ($)") +
    theme(text=element_text(face="bold"),
          axis.text=element_text(size=12),
          axis.title=element_text(size=14),
          title=element_text(size=16),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank()) +
    geom_text(aes(label=round(month_total)), position=position_dodge(width=0.9), vjust=-0.25)
  
}

monthly_budget_remaining <- function(data, budget_df) {
  mon.index <- as.numeric(data.table::month(Sys.Date()))
  year.index <- as.numeric(data.table::year(Sys.Date()))
  
  # Add month and year columns
  data$month <- data.table::month(as.Date(data$date, format = "%m/%d/%y"))
  data$yea <- data.table::year(as.Date(data$date, format = "%m/%d/%y"))
  
  # Make a new dataframe with the spending per category
  pdata <- data[month == mon.index & yea == year.index, .(month_total = sum(price)), by = category]
  
  # Set categories that haven't been spent on this month yet to 0
  plot_df <- merge(pdata, budget_df, by = "category", all.y = T)
  plot_df[is.na(plot_df$month_total), "month_total"] <- 0
  plot_df$remaining <- plot_df$budget - plot_df$month_total 
  
  # Order categories by spending
  plot_df$category <- factor(plot_df$category,
                           levels = plot_df$category[order(plot_df$month_total,decreasing = T)])
  
  ggplot(plot_df, aes(x = category, y = month_total, fill = category)) + 
    geom_col() +
    geom_point(mapping = aes(x = category, y = budget)) + 
    ggtitle(paste0("Spending in ", month.abb[mon.index]," of ", year.index, ", Total: ", sum(plot_df$month_total))) +
    xlab("Category") +
    ylab("Spending ($)") +
    theme(text=element_text(face="bold"),
          axis.text=element_text(size=12),
          axis.title=element_text(size=14),
          title=element_text(size=16),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank()) +
    geom_text(aes(label=round(remaining)), position=position_dodge(width=0.9), vjust=-0.25)
}

####################################### Archived ##################################################

# custom_range_spending <- function(data, start_date, end_date) {
#   data$date <- as.Date(data$date, format = "%m/%d/%y")
#   data <- data[data$date >= start_date & data$date <= end_date,]
#   ggplot(data, aes(fill = category, x = date, y = price)) + 
#     geom_bar(position="dodge", stat="identity") +
#     ggtitle("Spending Over Time") +
#     theme(text=element_text(face="bold"),
#           axis.text=element_text(size=12),
#           axis.title=element_text(size=14),
#           title=element_text(size=16),
#           #axis.text.x = element_blank(),
#           #axis.ticks.x = element_blank()
#     ) + scale_x_date(date_breaks = "1 day", labels = (date_format("%A")))
# }

# Plot by category over time (one for the past week, one for the past month, and one for the past year)
# week_spending <- function(data) {
#   data$date <- as.Date(data$date, format = "%m/%d/%y")
#   data <- data[data$date >= Sys.Date() - 6,]
#   df <- sapply(unique(data$date), function(d) {
#     sums_vec <- sapply(unique(data$category), function(x) { 
#       sum(data[data$category == x & data$date == d, "price"])
#     })
#     data.frame(price = sums_vec, category = names(sums_vec), date = rep(date, length(sums_vec)))
#   }, simplify = F)
#   return(df)
#   ggplot(data, aes(fill = category, x = date, y = price)) + 
#     geom_bar(position="dodge", stat="identity") +
#     ggtitle("Spending Over Time") +
#     theme(text=element_text(face="bold"),
#           axis.text=element_text(size=12),
#           axis.title=element_text(size=14),
#           title=element_text(size=16),
#           # axis.text.x = element_blank(),
#           # axis.ticks.x = element_blank())
#     ) + scale_x_date(date_breaks = "1 day", labels = (date_format("%a")))
# }
# setwd("C:/Users/grask/OneDrive/Desktop/Projects/shiny-budget/")