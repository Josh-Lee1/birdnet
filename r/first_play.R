library(tidyverse)
library(lubridate)

data <- read.csv("data/berkeley_birdnet_06_09_24.csv") %>% 
  mutate(Date = dmy(Date)) %>% 
  # removing first and last day (incomplete)
  filter(Date != min(Date) & Date != max(Date)) %>% 
  group_by(Date) %>% 
  mutate(n_detections = n_distinct(File_Name),
         n_species = n_distinct(Com_Name))

ggplot(data, aes(Date, n_species)) + 
  geom_point() +
  scale_x_date(date_labels = "%b") +
  geom_smooth() +
  theme_bw()


PICU <- data %>% 
  filter(Com_Name == "Pied Currawong")

ggplot(PICU, aes(Date, n_detections)) + 
  geom_point() +
  scale_x_date(date_labels = "%b") +
  theme_bw()
hist(PICU$Date, breaks = "weeks",  main = "Histogram of Pied Currawong detections", xlab = "Date")
