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
barplot_by_category <- function(data) {
  
  # sum price over each category
  pdata <- data[, sum(price), by = category]
  colnames(pdata)[2] <- "total_spent"
  
  # order database in decreasing order of amount spent
  pdata$category <- with(pdata, reorder(category, -total_spent))
  
  plt1 <- ggplot(pdata, aes(x = category, y = total_spent, fill = category)) +
    geom_col() + 
    ggtitle("Spending by Category") +
    xlab("Category") +
    ylab("Spending ($)") +
    theme(text=element_text(face="bold"),
          axis.text=element_text(size=12),
          axis.title=element_text(size=14),
          title=element_text(size=16),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank()) +
    geom_text(aes(label=total_spent), position=position_dodge(width=0.9), vjust=-0.25)
    
    #theme_gray(base_size = 16)
  return(plt1)
}

# make a named list of options for a date system
get_date_system_choices <- function() {
  date_system_choices <- c("1900", "1904")
  names(date_system_choices) <- c("1900 (Windows)", "1904 (Mac)")
  return(date_system_choices)
}

# Plot by category over time (one for the past week, one for the past month, and one for the past year)
week_spending <- function(data) {
  data$date <- as.Date(data$date, format = "%m/%d/%y")
  data <- data[data$date >= Sys.Date() - 6,]
  df <- sapply(unique(data$date), function(d) {
    sums_vec <- sapply(unique(data$category), function(x) { 
      sum(data[data$category == x & data$date == d, "price"])
    })
    data.frame(price = sums_vec, category = names(sums_vec), date = rep(date, length(sums_vec)))
  }, simplify = F)
  return(df)
  ggplot(data, aes(fill = category, x = date, y = price)) + 
    geom_bar(position="dodge", stat="identity") +
    ggtitle("Spending Over Time") +
    theme(text=element_text(face="bold"),
          axis.text=element_text(size=12),
          axis.title=element_text(size=14),
          title=element_text(size=16),
          # axis.text.x = element_blank(),
          # axis.ticks.x = element_blank())
    ) + scale_x_date(date_breaks = "1 day", labels = (date_format("%a")))
}

month_spending <- function(data) {
  data$date <- as.Date(data$date, format = "%m/%d/%y")
  data <- data[data$date >= Sys.Date() - 29,]
  ggplot(data, aes(fill = category, x = date, y = price)) + 
    geom_bar(position="dodge", stat="identity") +
    ggtitle("Spending Over Time") +
    theme(text=element_text(face="bold"),
          axis.text=element_text(size=12),
          axis.title=element_text(size=14),
          title=element_text(size=16),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank()
    ) + scale_x_date(date_breaks = "1 day", labels = (date_format("%A")))
}

custom_range_spending <- function(data, start_date, end_date) {
  data$date <- as.Date(data$date, format = "%m/%d/%y")
  data <- data[data$date >= start_date & data$date <= end_date,]
  ggplot(data, aes(fill = category, x = date, y = price)) + 
    geom_bar(position="dodge", stat="identity") +
    ggtitle("Spending Over Time") +
    theme(text=element_text(face="bold"),
          axis.text=element_text(size=12),
          axis.title=element_text(size=14),
          title=element_text(size=16),
          #axis.text.x = element_blank(),
          #axis.ticks.x = element_blank()
    ) + scale_x_date(date_breaks = "1 day", labels = (date_format("%A")))
}
