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

