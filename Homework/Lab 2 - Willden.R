#######################
# MATH 3190 Lab 2 - R #
#######################

#############
# Problem 2 #
#############

# Part a
# ------
library("tidyverse")

# Part b
# ------
employee_names <- c("Alice", "Bob", "Charlie", "Diana", "Edward")

# Part c
# ------
monthly_hours <- c(160, 145, 180, 120, 155)

# Part d
# ------
work_data <- data.frame(employee_names, monthly_hours)

# Part e
# ------
work_tibble <- data.frame(employee_names, monthly_hours)

print(work_data)
print(work_tibble)

# Part f
# ------
print(work_data[c(3, 5), ])

# Part g
# ------
first_column_df <- work_data[, 1]
first_column_vector <- work_data$employee_names

# Part h
# ------
library(dplyr)
average_monthly_hours_dplyr <- work_data %>%
  pull(monthly_hours) %>%
  mean()

print(average_monthly_hours_dplyr)


#############
# Problem 3 #
#############

# Part a
# ------
to_kelvin <- function(Fahrenheit) {
  kelvin <- (5/9)*(Fahrenheit + 459.67)
  return(kelvin)
}
to_kelvin(0)
to_kelvin(100)

# Part b
# ------
fuel_cost <- function(miles, mpg, price_per_gallon) {
  total_cost <- ((miles)/(mpg))*(price_per_gallon)
  return(total_cost)
}
fuel_cost(400, 32, 3.25)


#############
# Problem 4 #
#############

library("tidyverse")
?mpg
mpg_summary <- mpg %>%
  filter(manufacturer %in% c("audi", "ford", "toyota")) %>%
  mutate(combined_mpg = (cty + hwy) / 2) %>%
  select(manufacturer, model, cyl, class, combined_mpg) %>%
  group_by(class)%>%
  
  summarize(
    mean_combined = mean(combined_mpg),
    sd_combined = sd(combined_mpg)
) %>%
  arrange(desc(mean_combined))

print(mpg_summary)

# The compact class had the best average combined MPG.


#############
# Problem 5 #
#############

# Part a
# ------
install.packages("ggplot2")

library("ggplot2")
ggplot(midwest, aes(x = poptotal, y = area)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, formula = y ~ x)

# Part b
# ------
library("ggplot2")
ggplot(midwest, aes(x = poptotal, y = area, color = state)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, formula = y ~ x) +
  scale_x_log10() +
  labs(
    x = "Total Population",
    y = "Area",
    title = "Total Population vs Area of Midwest Counties"
  )

# Part c
# ------
library("ggplot2")
ggplot(midwest, aes(x = state, y = percollege)) +
  geom_boxplot(fill = "skyblue", alpha = 0.6) +
  geom_jitter(
    color = "darkgreen",
    size = 0.5,
    width = 0.1
  )

# Part d
# ------
library("ggplot2")
ggplot(midwest, aes(x = percbelowpoverty)) +
  geom_density() +
  facet_grid(. ~ state) +
  theme_bw() +
  labs(title = "Poverty Rates Across Midwest States") +
  theme(
    plot.title = element_text(size = 18, face = "bold")
  )

# Part e
# ------
library("tidyverse")
midwest %>%
  group_by(state) %>%
  summarize(counties = n()) %>%
  ggplot(aes(x = state, y = counties)) +
  geom_col(fill = "steelblue", color = "black") +
  labs(
    x = "State",
    y = "Number of Counties",
    title = "Number of Counties by Midwest State"
  )


#############
# Problem 6 #
#############

# Part a
# ------
mytreeseeds <- read.table(
  "/Users/janewillden/Downloads/treeseeds.txt",
  header = TRUE,
  row.names = NULL
)
head(mytreeseeds, n = 6)

# Part b
# ------
myblood_pressure <- read.table(
  "/Users/janewillden/Downloads/blood_pressure.txt",
  header = TRUE,
  row.names = NULL
)
head(myblood_pressure, n = 6)

# Part c
# ------
install.packages("readxl")
library("readxl")
myConcrete_data <- read_excel(
  "/Users/janewillden/Downloads/Concrete_data.xls")
head(myConcrete_data, n = 6)


#############
# Problem 7 #
#############

# Part a
# ------
monthly_hours <- c(160, 145, 180, 120, 155)
employee_names <- c("Alice", "Bob", "Charlie", "Diana", "Edward")

for (i in 1:5) {
  if (monthly_hours[i] > 150) {
    print(paste(employee_names[i], "Overtime Eligible"))
  } else {
    print(paste(employee_names[i], "Standard Hours"))
  }
}

# Part b
# ------
monthly_hours <- c(160, 145, 180, 120, 155)
employee_names <- c("Alice", "Bob", "Charlie", "Diana", "Edward")

library(dplyr)
work_data <- work_data %>%
  mutate(
    status = ifelse(monthly_hours > 150, "Overtime Eligible", "Standard Hours")
  )

work_data