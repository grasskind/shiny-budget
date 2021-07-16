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
                                 date = character(),
                                 month = numeric(),
                                 year = numeric())
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
    shiny_palette <- c('#e6194b', '#3cb44b', '#ffe119', '#4363d8', '#f58231', '#911eb4', '#46f0f0', '#f032e6', '#bcf60c', '#fabebe', '#008080', '#e6beff', '#9a6324', '#fffac8', '#800000', '#aaffc3', '#808000', '#ffd8b1', '#000075', '#808080', '#ffffff', '#000000',"#466791","#60bf37","#953ada","#4fbe6c","#ce49d3","#a7b43d","#5a51dc","#d49f36","#552095","#507f2d","#db37aa","#84b67c","#a06fda","#df462a","#5b83db","#c76c2d","#4f49a3","#82702d","#dd6bbb","#334c22","#d83979","#55baad","#dc4555","#62aad3","#8c3025","#417d61","#862977","#bba672","#403367","#da8a6d","#a79cd4","#71482c","#c689d0","#6b2940","#d593a7","#895c8b","#bd5975")
#    shiny_palette <- c("#000000", "#FFFF00", "#1CE6FF", "#FF34FF", "#FF4A46", "#008941", "#006FA6", "#A30059", "#FFDBE5", "#7A4900", "#0000A6", "#63FFAC", "#B79762", "#004D43", "#8FB0FF", "#997D87", "#5A0007", "#809693", "#FEFFE6", "#1B4400", "#4FC601", "#3B5DFF", "#4A3B53", "#FF2F80", "#61615A", "#BA0900", "#6B7900", "#00C2A0", "#FFAA92", "#FF90C9", "#B903AA", "#D16100", "#DDEFFF", "#000035", "#7B4F4B", "#A1C299", "#300018", "#0AA6D8", "#013349", "#00846F", "#372101", "#FFB500", "#C2FFED", "#A079BF", "#CC0744", "#C0B9B2", "#C2FF99", "#001E09", "#00489C", "#6F0062", "#0CBD66", "#EEC3FF", "#456D75", "#B77B68", "#7A87A1", "#788D66", "#885578", "#FAD09F", "#FF8A9A", "#D157A0", "#BEC459", "#456648", "#0086ED", "#886F4C", "#34362D", "#B4A8BD", "#00A6AA", "#452C2C", "#636375", "#A3C8C9", "#FF913F", "#938A81", "#575329", "#00FECF", "#B05B6F", "#8CD0FF", "#3B9700", "#04F757", "#C8A1A1", "#1E6E00", "#7900D7", "#A77500", "#6367A9", "#A05837", "#6B002C", "#772600", "#D790FF", "#9B9700", "#549E79", "#FFF69F", "#201625", "#72418F", "#BC23FF", "#99ADC0", "#3A2465", "#922329", "#5B4534", "#FDE8DC", "#404E55", "#0089A3", "#CB7E98", "#A4E804", "#324E72", "#6A3A4C")
  }
  
  # And finally the category-color matching database
  if (file.exists("shiny_color_choices.rds")) {
    shiny_color_choices <- readRDS("shiny_color_choices.rds")
  } else {
    shiny_color_choices <- data.table(category = character(),
                                        col = character())
  }
  return (list(shiny_database, shiny_budget, shiny_palette, shiny_color_choices))
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
# group = spending category, e.g. "Rent"
# year = Year to make plot for
monthly_spending <- function(data, group, yr) {
  
  # Make a new dataframe with the per month spending in group during year
  pdata <- data[category == group & year == yr, .(month_total = sum(price)), by = month]
  
  # If category doesn't exist for this year, don't plot anything
  if(pdata[,.N] == 0) {
    return(FALSE)
  }
  
  # Add a column with 3 letter abbreviations for each month
  pdata$month_abbr <- sapply(pdata$month, function(x) month.abb[x])
  pdata$month_abbr <- factor(pdata$month_abbr, levels = month.abb)
  
  # Plot. Title includes average spending over the year
  ggplot(pdata, aes(x = month_abbr, y = month_total, group = 1)) + 
    geom_line(size = 1.5) +
    geom_point(size = 2, color = "purple") + 
    expand_limits(y = 0) + 
    ggtitle(paste0("Monthly Spending on ", group,", Average: $", round(mean(pdata$month_total)))) +
    labs(x = "Month", y = "Total Spent") + 
    theme(text=element_text(face="bold"),
          axis.text=element_text(size=12),
          axis.title=element_text(size=14),
          title=element_text(size=16),
          axis.ticks.x = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank()      
    )
}

# Barplot of average spending over the year in each category. Ordered by spending left to right
# data = data.table with columns for date, category, price, month, and year
# yr = Year to make plot for
# color_df = dataframe with color palette 
barplot_by_category <- function(data, yr, color_df) {
  # Make a list of the input year's spending categories
  groups <- unique(data$category[data$year == yr])
  
  if (length(groups) == 0) {
    return(FALSE)
  }

  # Calculate average spending for the year per category
  year_avg <- function(d, ct) {
    pd <- d[category == ct & year == yr , .(month_total = sum(price)), by = month]
    return(mean(pd$month_total))
  }
  avg_spending_list <- sapply(groups, function(x) year_avg(data, x))
  avg_spending_df <- data.frame(avg_spending = avg_spending_list, 
                                category = names(avg_spending_list),
                                row.names = NULL)
  
  # Add a color palette 
  avg_spending_df <- merge(avg_spending_df, color_df, by = "category", all.x = T) 
  
  # Order the categories by spending
  avg_spending_df$category <- factor(avg_spending_df$category, 
                                     levels = avg_spending_df$category[order(avg_spending_df$avg_spending, decreasing = T)])
  avg_spending_df$col <- avg_spending_df$col[order(avg_spending_df$avg_spending, decreasing = T)]
  
  plt <- ggplot(avg_spending_df, aes(x = category, y = avg_spending, fill = category)) +
    geom_col() + 
    scale_fill_manual(values = avg_spending_df$col) + 
    ggtitle(paste0("Average Spending by Category in ", yr)) +
    xlab("Category") +
    ylab("Spending ($)") +
    theme(text=element_text(face="bold"),
          axis.text=element_text(size=12),
          axis.title=element_text(size=14),
          title=element_text(size=16),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank()) +
    geom_text(aes(label=round(avg_spending)), position=position_dodge(width=0.9), vjust=-0.25)
  
  return(plt)
}

# Barplot of spending per category during a month of interest
# data = data.table with columns for date, category, and price
#   date should be in format mm/dd/yyyy
# mon = month to plot spending for. Should be in month.name.
# yr = Year to make plot for, should be numeric
# color_df = dataframe of categories and corresponding colors
month_totals_bp <- function(data, mon, yr, color_df) {
  mon.index <- which(month.name == mon)
  
  # Make a new dataframe with the spending per category
  pdata <- data[month == mon.index & year == yr, .(month_total = sum(price)), by = category]
  
  # Add a color palette 
  pdata <- merge(pdata, color_df, by = "category", all.x = T) 
  
  # Order categories by spending
  pdata$category <- factor(pdata$category,
                           levels = pdata$category[order(pdata$month_total,decreasing = T)])
  
  pdata$col <- pdata$col[order(pdata$month_total, decreasing = T)]
  
  # Plot. Title includes total spending
  ggplot(pdata, aes(x = category, y = month_total, fill = category)) + 
    geom_col() +
    scale_fill_manual(values = pdata$col) + 
    ggtitle(paste0("Spending in ", mon," of ", yr, ", Total: ", sum(pdata$month_total))) +
    xlab("Category") +
    ylab("Spending ($)") +
    theme(text=element_text(face="bold"),
          axis.text=element_text(size=12),
          axis.title=element_text(size=14),
          title=element_text(size=16),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank()) +
    geom_text(aes(label=round(month_total)), position=position_dodge(width=0.9), vjust=-0.25)
  
}

monthly_budget_remaining <- function(data, budget_df, color_df) {
  mon.index <- as.numeric(data.table::month(Sys.Date()))
  year.index <- as.numeric(data.table::year(Sys.Date()))
  
  # Make a new dataframe with the spending per category
  pdata <- data[month == mon.index & year == year.index, .(month_total = sum(price)), by = category]
  
  # Set categories that haven't been spent on this month yet to 0
  plot_df <- merge(pdata, budget_df, by = "category", all.y = T)
  plot_df[is.na(plot_df$month_total), "month_total"] <- 0
  plot_df$remaining <- plot_df$budget - plot_df$month_total 
  
  # Add a color palette 
  plot_df <- merge(plot_df, color_df, by = "category", all.x = T) 
  
  # Order categories by spending
  plot_df$category <- factor(plot_df$category,
                           levels = plot_df$category[order(plot_df$budget,decreasing = T)])
  plot_df$col <- plot_df$col[order(plot_df$budget, decreasing = T)]
  
  ggplot(plot_df, aes(x = category, y = month_total, fill = category)) + 
    geom_col() +
    geom_point(mapping = aes(x = category, y = budget)) + 
    scale_fill_manual(values = plot_df$col) + 
    ggtitle(paste0("Spending in ", month.name[mon.index]," of ", year.index, ", Total: ", sum(plot_df$month_total))) +
    xlab("Category") +
    ylab("Spending ($)") +
    theme(text=element_text(face="bold"),
          axis.text=element_text(size=12),
          axis.title=element_text(size=14),
          title=element_text(size=16),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank()) +
    geom_text(aes(label=round(remaining)), position=position_dodge(width=0.9), vjust=-0.25)
}