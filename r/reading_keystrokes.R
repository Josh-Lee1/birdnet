library(tidyverse)
library(lubridate)

library(tidyverse)
library(lubridate)

# Define the function
process_notes <- function(data) {
  # Ensure the AEDT_Timestamp column is in hms format
  data$AEDT_Timestamp <- lubridate::hms(data$AEDT_Timestamp)
  
  # Calculate seconds since the first timestamp
  data$seconds_since_start <- as.numeric(data$AEDT_Timestamp - first(data$AEDT_Timestamp))
  
  # Create the 3-second interval column
  data$interval <- floor(data$seconds_since_start / 3)
  
  return(data)
}

notes1 <- read.csv("data/KEY_LOGS/key_log_2025-01-19_14-32-50.csv")
notes1_processed <- process_notes(notes1)
# Plot the results for notes1
ggplot(notes1_processed, aes(seconds_since_start, Translation)) +
  geom_point()