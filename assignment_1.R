library(tidyverse)
library(magritter)


# Part 1
column_one <- c(8, 10, 4, 1, 30)
column_two <- c("hello", "welcome", "to", "Econ", "245")
column_three <- c(0, 0, 17, NA, 15)

tibble_one <- tibble(column_one, column_two, column_three)

data_type_one <- typeof(tibble_one$column_one)
data_type_two <- typeof(tibble_one$column_two)

summary_stats_column_one <-
  c(mean(column_one), sd(column_one), var(column_one))
summary_stats_column_three <- c(mean(column_one, na.rm = T),
                                sd(column_one, na.rm = T),
                                var(column_one, na.rm = T))

# Part 2

### (1)
festival_data <- readr::read_csv("assign_2.csv")

### (3)
column_names_of_festival <- colnames(festival_data)

### (4)
column_names_of_festival_tibble <-
  column_names_of_festival %>% tibble()

### (5)
festival_data_unique <- festival_data %>% distinct()
total_response <- nrow(festival_data_unique)

### (6)
festival_data_unique_subset <-
  festival_data_unique %>% 
  select(lodging, hours_attend, gender, spend_travel_total)

### (7)
males <- festival_data_unique_subset %>% filter(gender == "Male")
females <-
  festival_data_unique_subset %>% filter(gender == "Female")

### (8)
males %>% 
  summarize(mean_hours_male = mean(hours_attend, na.rm = T))

### (9)
females %>% 
  summarize(mean_hours_female = mean(hours_attend, na.rm = T))

### (10)
festival_data_unique_subset %>% 
  group_by(gender) %>% 
  summarize(mean_hours_attend = mean(hours_attend, na.rm = T))
mean_hours_pnts <- 2.5

### (11)
festival_data_unique_subset <- festival_data_unique_subset %>%
  mutate(resident = ifelse(lodging == "Boulder Resident", 1, 0))

### (12)
festival_data_unique_subset <- festival_data_unique_subset %>%
  mutate(
    non_resident = ifelse(
      lodging == "Hotel or motel" |
        lodging == "Friends, family or no expense",
      1,
      0
    )
  )

### (13)
frac_resident <-
  mean(festival_data_unique_subset$resident, na.rm = T)

### (14)
column_names_of_festival_tibble %>%
  rename("col_names" = ".")
