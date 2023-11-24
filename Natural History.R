library(tidyverse)
#Calculating mean incubation period
linelist %>%
	summarise(mean(incubation_period, na.rm = T))
#23.4

#Calculating median incubation period
linelist %>%
	summarise(median(incubation_period, na.rm = T))

#Calculating mean ill duration
linelist %>%
	summarise(mean(illness_duration, na.rm = T))
#25.0

#Calculating median ill duration
linelist %>%
	summarise(median(illness_duration, na.rm = T))
#26.0


#Serial Interval
#Create a variable that subtracts the time of symptom onset in the primary case from the time of symptom onset in the
#secondary case

#Create a new variable that is time of symptom onset by adding time of exposure to how many hours after exposure did you devlop symptoms
# Convert date_of_exposure and time_of_exposure to POSIXct format
raw_data2 <- raw_data %>%
  mutate(
    datetime_of_exposure = as.POSIXct(paste(date_of_exposure, time_of_exposure), format="%Y-%m-%d %H:%M:%S"),
    hours_to_symptoms = as.numeric(how_many_hours_after_exposure_did_you_develop_symptoms)
  )

# Create a new column for the time of the start of symptoms
raw_data3 <- raw_data2 %>%
  mutate(
    time_of_start_of_symptoms = datetime_of_exposure + hours_to_symptoms * 3600
  )

# View the modified dataframe
raw_data3 %>% 
  select(date_of_exposure, time_of_exposure, how_many_hours_after_exposure_did_you_develop_symptoms, time_of_start_of_symptoms)
library(dplyr)

# Merge the data frame with itself to get pairs of infected and infector
merged_data <- merge(raw_data3, raw_data3, by.x = "infectedby", by.y = "case_id", suffixes = c("_infected", "_infector"))

# Check merged data
print(merged_data)

# Filter and calculate the serial interval
serial_interval <- merged_data %>%
  filter(!is.na(infectedby) & time_of_start_of_symptoms_infected >= time_of_start_of_symptoms_infector) %>%
  mutate(
    serial_interval = time_of_start_of_symptoms_infected - time_of_start_of_symptoms_infector
  ) %>%
  select(serial_interval) %>%
  pull()

# View the serial interval
print(serial_interval)
