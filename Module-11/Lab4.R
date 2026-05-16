confirmations <- time_series_covid19_confirmed_US
deaths <- time_series_covid19_deaths_US

# Objective 1
# Creating a vector with all unique states in the dataset
unique_states <- unique(confirmations$Province_State)
# Creating a vector of the length of the unique_states vector
confirmation_sums <- numeric(length(unique_states))
death_sums <- numeric(length(unique_states))
# Using a for loop to calculate the sum of COVID-19 confirmations and deaths per state
for (k in 1:length(unique_states)) {
  confirmation_sums[k] <- sum(confirmations[confirmations$Province_State == unique_states[k], length(confirmations[1,])])
  death_sums[k] <- sum(deaths[deaths$Province_State == unique_states[k], length(deaths[1,])])
  }
# Creating a new data frame with the sums of the confirmations and deaths
sum_data <- data.frame(unique_states, confirmation_sums, death_sums)
# Finding which state/province has had the most confirmations and deaths for COVID-19
# This can be done through finding which row has the max value for both
max_confirmations_row <- which.max(sum_data[, 2])
max_deaths_row <- which.max(sum_data[, 3])
# The "max_row" variables can now be used to identify the states/provinces with the max amount of confirmations and deaths
max_confirmations_state <- sum_data[max_confirmations_row, 1]
max_deaths_state <- sum_data[max_deaths_row, 1]
# Showing the results of the state identified with the most confirmations and deaths
if (max_confirmations_state == max_deaths_state) {
  print(paste("The state with the most confirmations and deaths is ", max_deaths_state))
} else {
  print(paste("The state with the most confirmations is ", max_confirmations_state, "\nThe state with the most deaths is ", max_deaths_state))
}

# Objective 2
# Creating a table called current_tally containing columns from the confirmations and deaths tables
# The select statement in addition to the left_join allow for only the needed columns to be used
current_tally <- left_join(
  confirmations,
  deaths,
  by = c("iso2", "Province_State", "Admin2", "Lat", "Long_")
) %>%
  select(
    Country = iso2,
    State = Province_State,
    City = Admin2,
    Lat,
    Long = Long_
  )
# Adding in the last columns of the confirmations and deaths datasets to complete the table
# I admit, there is a naming conflict in the table, but I genuinely do not know how to solve it. I've tried everything I know to use..
# Regardless, calling current_tally$Confirmations & current_tally$Deaths should look fine, the titles just look a bit odd in the table itself
current_tally$Confirmations <- confirmations[, ncol(confirmations)]
current_tally$Deaths <- deaths[, ncol(deaths)]
# Printing the results; the table itself
print(current_tally)

# Objective 3
# Creating a new column on the current_tally table for patients of COVID-19 unaccounted for
current_tally$Unaccounted <- abs(current_tally$Confirmations - current_tally$Deaths)
# We still have the unique_states list from Objective 1, and can be used to solve the total unaccounted
# This can be used to create an unaccounted vector of length unique_states, which can be filled in using a for loop
unaccounted_sums <- numeric(length(unique_states))
# Filling in unaccounted_sums with a for loop
for (i in 1:length(unique_states)) {
  unaccounted_sums[i] <- sum(current_tally[current_tally$State == unique_states[i], length(current_tally[1,])])
  # Printing the value correlated to each state within the loop
  print(paste(unique_states[i], " has ", unaccounted_sums[i], " unaccounted for"))
}

# Objective 4
# Creating a dataframe to edit
unaccounted_assessment <- data.frame(unique_states, unaccounted_sums)
# Using a for loop to create a dataframe of the countries among the top 5 most unaccounted cases of COVID-19
for (m in 1:5) {
  # In every iteration, a new row with the max amount of unaccounted is saved to top_unaccounted_row
  top_unaccounted_row <- which.max(unaccounted_assessment[, 2])
  # On the first iteration, top_unaccounted is initialized with the first row being added
  if (m == 1) {
    top_unaccounted <- unaccounted_assessment[top_unaccounted_row, ]
  }
  # In all other cases, rbind is used to append more rows onto top_unaccounted
  else {
    top_unaccounted <- rbind(top_unaccounted, unaccounted_assessment[top_unaccounted_row, ])
  }
  # Every iteration also has that row removed from unaccounted_assessment so that the next max can be identified
  unaccounted_assessment <- unaccounted_assessment[-top_unaccounted_row, ]
}

# Turning the top_unaccounted dataframe into a table
kable(top_unaccounted, row.names = FALSE, col.names = c("Country", "Unaccounted"))

# Objective 5
# Recreating a dataframe to analyze
unaccounted_assessment <- data.frame(unique_states, unaccounted_sums)
# We can create a bar plot to evaluate the percentages of unaccounted cases in the US
ggplot(data = unaccounted_assessment) + 
  geom_col(mapping = aes(x = unique_states, y = unaccounted_sums)) +
  coord_flip()