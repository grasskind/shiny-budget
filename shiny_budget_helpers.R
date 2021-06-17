library(ggplot2)

# loads database or initializes a new one if one doesn't already exist
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
  return (shiny_database)
}

# Makes a simple boxplot of spending by category
barplot_by_category <- function(data, year) {
  data$mon <- month(as.Date(data$date, format = "%m/%d/%y"))
  data$yea <- year(as.Date(data$date, format = "%m/%d/%y"))
  groups <- unique(data$category)
  
  monthly_avg <- function(d, y, group) {
    pd <- d[category == group & yea == year, .(month_total = sum(price)), by = mon]
    return(mean(pd$month_total))
  }
  avg_spending_list <- sapply(groups, function(x) monthly_avg(data, year, x))
  avg_spending_df <- data.frame(avg_spending = avg_spending_list, 
                                category = names(avg_spending_list),
                                row.names = NULL)
  
  avg_spending_df <- avg_spending_df[order(avg_spending_df$avg_spending, decreasing = T),]
  avg_spending_df$category <- factor(avg_spending_df$category, levels = avg_spending_df$category)
  
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
    
    #theme_gray(base_size = 16)
  return(plt)
}

# make a named list of options for a date system
get_date_system_choices <- function() {
  date_system_choices <- c("1900", "1904")
  names(date_system_choices) <- c("1900 (Windows)", "1904 (Mac)")
  return(date_system_choices)
}

monthly_spending <- function(data, group, year) {
  data$mon <- month(as.Date(data$date, format = "%m/%d/%y"))
  data$yea <- year(as.Date(data$date, format = "%m/%d/%y"))
  
  pdata <- data[category == group & yea == year, .(month_total = sum(price)), by = mon]
  pdata$month <- sapply(pdata$mon, function(x) month.abb[x])
  pdata$month <- factor(pdata$month, levels = month.abb)
  
  ggplot(pdata, aes(x = month, y = month_total, group = 1)) + 
    geom_line(size = 1.5) +
    geom_point(size = 2, color = "purple") + 
    expand_limits(y = 0) + 
    ggtitle(paste0("Monthly Spending on ", group,", Average: $", mean(pdata$month_total))) +
    labs(x = "Month", y = "Total Spent") + 
    theme(text=element_text(face="bold"),
          axis.text=element_text(size=12),
          axis.title=element_text(size=14),
          title=element_text(size=16),
          axis.ticks.x = element_blank()
    )
}

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