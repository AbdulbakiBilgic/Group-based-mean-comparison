#===============================================================================
# Author : Abdulbaki Bilgic
# Topic  : Group-wise averaging of target variables
#===============================================================================
rm(list=ls()); cat("\f"); graphics.off()
#===============================================================================
# Required Libraries:
library(dplyr)
library(ggplot2)
library(purrr)
#===============================================================================
# Pyramid Plot Function:
pyramid_plot_by_group <- function(data, group_vars, main_var, split_var, 
                                  split_labels   = c("Group1", "Group2"),
                                  feature_labels = NULL,
                                  colors         = c("Group1" = "#4C9BE8",             
                                                     "Group2" = "#1B4F72")) {           
  
  vars_exist <- c(group_vars, main_var, split_var) %in% names(data)
  if (!all(vars_exist)) {
    stop("Some specified variables do not exist in the data.")
  }
  
  if (is.null(feature_labels)) {
    feature_labels <- setNames(group_vars, group_vars)
  }
  
  results <- map_dfr(group_vars, function(var) {
    data %>%
      mutate(
        Split = case_when(
          !!sym(split_var) == 1 ~ split_labels[1],
          !!sym(split_var) == 0 ~ split_labels[2],
          TRUE ~ NA_character_
        )
      ) %>%
      filter(!is.na(Split)) %>%
      group_by(Split, Group = !!sym(var)) %>%
      summarise(
        MeanValue = mean(!!sym(main_var), na.rm = TRUE),
        .groups   = "drop"
      ) %>%
      filter(Group == 1) %>%
      mutate(Variable = feature_labels[[var]]) %>%
      dplyr::select(Variable, Split, MeanValue)
  })
  
  results_ordered <- results %>%
    mutate(Variable = factor(Variable, 
                             levels = feature_labels[group_vars],
                             labels = feature_labels[group_vars])) %>%
    filter(!is.na(Variable))
  
  plot_data <- results_ordered %>%
    mutate(Value = ifelse(Split == split_labels[1], 
                          -MeanValue, MeanValue))
  
  ggplot(plot_data, aes(x = Variable, y = Value, fill = Split)) +
    geom_bar(stat = "identity", position = "identity", width = 0.85) +
    geom_text(aes(label = sprintf("μ = %.2f", abs(MeanValue))),
              position  = position_stack(vjust = 0.5), size = 2.1) +
    coord_flip() +
    scale_y_continuous(labels = function(x) sprintf("%.2f", abs(x))) +
    scale_x_discrete(limits   = rev(levels(plot_data$Variable))) +
    scale_fill_manual(values  = colors, breaks = split_labels) +
    labs(
      x    = "Variables",
      y    = sprintf("Average %s", gsub("([a-z])([A-Z])", "\\1 \\2", main_var)),
      fill = paste0(split_var, ":")
    ) +
    theme_minimal(base_size = 10) +
    theme(
      legend.position = "bottom",
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.line.x      = element_line(color = "black"),
      axis.text.y      = element_text(size = 8, color = "black"),
      axis.text.x      = element_text(color = "black"),
      axis.ticks       = element_line(color = "black"),
      panel.background = element_blank(),
      plot.background  = element_blank()
    )
}
#===============================================================================
# Pseudo (Synthetic) Data Generation
set.seed(123)

# Number of observations:
n <- 2500

# Main variables:
dta1a <- data.frame(
  Gender          = sample(0:1, n, replace = TRUE),      # 0 = Female, 1 = Male
  HealthExpenses  = rnorm(n, mean = 3000, sd = 1000),    # Dependent variable
  SchoolClass     = sample(c("Primary", "Secondary", "HighSchool", 
                             "University", "Graduate"), 
                           n, replace = TRUE, 
                           prob = c(0.15, 0.25, 0.25, 0.25, 0.10))  # Education level
)
#===============================================================================
# Create separate binary variables for each education level:
#===============================================================================
dta1a <- dta1a %>%
  mutate(
    IsPrimary     = ifelse(SchoolClass == "Primary", 1, 0),
    IsSecondary   = ifelse(SchoolClass == "Secondary", 1, 0),
    IsHighSchool  = ifelse(SchoolClass == "HighSchool", 1, 0),
    IsUniversity  = ifelse(SchoolClass == "University", 1, 0),
    IsGraduate    = ifelse(SchoolClass == "Graduate", 1, 0)
  )
#===============================================================================
# Add additional socioeconomic binary variables:
#===============================================================================
extra_vars <- data.frame(
  HasInternet       = sample(0:1, n, replace = TRUE),
  OwnsCar           = sample(0:1, n, replace = TRUE),
  OwnsHouse         = sample(0:1, n, replace = TRUE),
  UsesSmartphone    = sample(0:1, n, replace = TRUE),
  HasCreditCard     = sample(0:1, n, replace = TRUE),
  UrbanResident     = sample(0:1, n, replace = TRUE),
  HasInsurance      = sample(0:1, n, replace = TRUE),
  ExercisesRegularly= sample(0:1, n, replace = TRUE),
  Smokes            = sample(0:1, n, replace = TRUE),
  DrinksAlcohol     = sample(0:1, n, replace = TRUE),
  OwnsLaptop        = sample(0:1, n, replace = TRUE),
  OwnsPet           = sample(0:1, n, replace = TRUE),
  HasChildren       = sample(0:1, n, replace = TRUE),
  Married           = sample(0:1, n, replace = TRUE),
  Employed          = sample(0:1, n, replace = TRUE),
  Vegetarian        = sample(0:1, n, replace = TRUE),
  SocialMediaUser   = sample(0:1, n, replace = TRUE),
  RegularCheckups   = sample(0:1, n, replace = TRUE),
  ChronicDisease    = sample(0:1, n, replace = TRUE),
  HasSavingsAccount = sample(0:1, n, replace = TRUE)
)

# Combine datasets
dta1a <- bind_cols(dta1a, extra_vars)

#===============================================================================
# List of all binary grouping variables (zx):
#===============================================================================
zx <- c("IsPrimary", "IsSecondary", "IsHighSchool", "IsUniversity", "IsGraduate",
        "HasInternet", "OwnsCar", "OwnsHouse", "UsesSmartphone", "HasCreditCard", 
        "UrbanResident", "HasInsurance", "ExercisesRegularly", "Smokes", "DrinksAlcohol",
        "OwnsLaptop", "OwnsPet", "HasChildren", "Married", "Employed", "Vegetarian",
        "SocialMediaUser", "RegularCheckups", "ChronicDisease", "HasSavingsAccount")

#===============================================================================
# Human-readable feature labels:
#===============================================================================
feature_labels <- c(
  "IsPrimary"         = "Primary School",
  "IsSecondary"       = "Secondary School",
  "IsHighSchool"      = "High School",
  "IsUniversity"      = "University",
  "IsGraduate"        = "Graduate Degree",
  "HasInternet"       = "Has Internet",
  "OwnsCar"           = "Owns Car",
  "OwnsHouse"         = "Owns House",
  "UsesSmartphone"    = "Uses Smartphone",
  "HasCreditCard"     = "Has Credit Card",
  "UrbanResident"     = "Urban Resident",
  "HasInsurance"      = "Has Health Insurance",
  "ExercisesRegularly"= "Exercises Regularly",
  "Smokes"            = "Smokes",
  "DrinksAlcohol"     = "Drinks Alcohol",
  "OwnsLaptop"        = "Owns Laptop",
  "OwnsPet"           = "Owns Pet",
  "HasChildren"       = "Has Children",
  "Married"           = "Married",
  "Employed"          = "Employed",
  "Vegetarian"        = "Vegetarian",
  "SocialMediaUser"   = "Social Media User",
  "RegularCheckups"   = "Regular Health Checkups",
  "ChronicDisease"    = "Chronic Disease",
  "HasSavingsAccount" = "Has Savings Account"
)
#===============================================================================
# Data preview:
#===============================================================================
head(dta1a[, c("Gender", "HealthExpenses", "SchoolClass",
               "IsPrimary", "IsUniversity", "IsGraduate")])

#===============================================================================
# Function Call:
#===============================================================================
pyramid_plot_by_group(
  data           = dta1a,
  group_vars     = zx,
  main_var       = "HealthExpenses",
  split_var      = "Gender",
  split_labels   = c("Male", "Female"),
  feature_labels = feature_labels,
  colors         = c("Male" = "green", "Female" = "red")
)
#===============================================================================

