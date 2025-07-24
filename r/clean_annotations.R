library(tidyverse)
library(lubridate)

field <- read.csv("data/recordings/1/key_log_2025-01-19_14-32-50.csv") %>%
  mutate(
    AEDT_Timestamp = parse_date_time(
      paste("2025-01-19", AEDT_Timestamp),
      orders = "ymd I:M:S p",
      tz = "Australia/Sydney"
    )
  ) %>% 
  select(AEDT_Timestamp, 
         species = "Translation",
         notes = Notes) %>% 
  mutate(annotation = "field") %>% 
  filter(species != "")
posta <- read.csv("data/recordings/1/post_transcription_1a.csv")
postb <- read.csv("data/recordings/1/post_transcription_1b.csv")

#read birdnet outputs
rec1<- read.delim("data/from_will/smiths_lake_birdnet/trial1/2MM03792_20250119_141450.BirdNET.selection.table.txt")
rec2<- read.delim("data/from_will/smiths_lake_birdnet/trial1/2MM03792_20250119_151402.BirdNET.selection.table.txt")

recording_start_time<-ymd_hms("2025/1/19 14:14:50")
recording2_start_time<-ymd_hms("2025/1/19 15:14:02")

rec1$AEDT_Timestamp<-recording_start_time+rec1$`Begin.Time..s.`
rec2$AEDT_Timestamp<-recording2_start_time+rec2$`Begin.Time..s.`

birdnet <- rbind(rec1, rec2) %>%
  mutate(
    AEDT_Timestamp = ymd_hms(AEDT_Timestamp),
    AEDT_Timestamp = force_tz(AEDT_Timestamp, tzone = "Australia/Sydney")
  ) %>%
  filter(
    AEDT_Timestamp >= ymd_hms("2025-01-19 14:32:50", tz = "Australia/Sydney") &
      AEDT_Timestamp <= ymd_hms("2025-01-19 15:33:37", tz = "Australia/Sydney")
  ) %>%
  mutate(
    annotation = "birdnet",
    species = recode(`Common.Name`, "Scarlet Myzomela" = "Scarlet Honeyeater")
  ) %>%
  select(
    AEDT_Timestamp,
    species,
    Confidence,
    annotation
  )

# Step 1: Combine the known date with the base time string from row 1
base_time_str <- paste("2025-01-19", posta$AEDT_Timestamp[1])
base_time <- parse_date_time(base_time_str, orders = "ymd I:M:S p", tz = "Australia/Sydney")

# Step 2: Ensure min and sec are numeric
posta <- posta %>%
  mutate(
    min = as.numeric(min),
    sec = as.numeric(sec)
  )

# Step 3: Get baseline min/sec
base_min <- posta$min[1]
base_sec <- posta$sec[1]

# Step 4: Compute offset and full timestamp
posta <- posta %>%
  mutate(
    offset_sec = (min - base_min) * 60 + (sec - base_sec),
    full_timestamp = base_time + seconds(offset_sec)
  )

# Step 1: Combine date with the base timestamp string from first row
base_time_str_b <- paste("2025-01-19", postb$AEDT_Timestamp[1])
base_time_b <- parse_date_time(base_time_str_b, orders = "ymd I:M:S p", tz = "Australia/Sydney")

# Step 2: Ensure min/sec are numeric
postb <- postb %>%
  mutate(
    min = as.numeric(min),
    sec = as.numeric(sec)
  )

# Step 3: Get baseline min/sec from first row
base_min_b <- postb$min[1]
base_sec_b <- postb$sec[1]

# Step 4: Calculate offset and full timestamp
postb <- postb %>%
  mutate(
    offset_sec = (min - base_min_b) * 60 + (sec - base_sec_b),
    full_timestamp = base_time_b + seconds(offset_sec)
  )

#bind them
post <- bind_rows(posta, postb) %>% 
  select(AEDT_Timestamp = full_timestamp,
         species = Translation,
         notes = X) %>% 
  mutate(annotation = "post")%>% 
  mutate(
    species = if_else(str_detect(notes, "\\?"), "bird sp.", species),
    annotation = "post"
  ) %>% 
  filter(species != "")

both <- bind_rows(post, field, birdnet) %>% 
  arrange(AEDT_Timestamp)

saveRDS(both, "data/cleaned_raw_firsthour.rds")

######### 
library(fuzzyjoin)

# Step 1: Subset by annotation
field_obs   <- both %>% filter(annotation == "field")
post_obs    <- both %>% filter(annotation == "post")
birdnet_obs <- both %>% filter(annotation == "birdnet")

# Step 2: Match field ↔ post
field_post_matches <- fuzzy_left_join(
  field_obs,
  post_obs,
  by = c("species" = "species",
         "AEDT_Timestamp" = "AEDT_Timestamp"),
  match_fun = list(`==`, function(f, p) abs(as.numeric(difftime(f, p, units = "secs"))) <= 3)
)

# Step 3: Match field ↔ birdnet
field_birdnet_matches <- fuzzy_left_join(
  field_obs,
  birdnet_obs,
  by = c("species" = "species",
         "AEDT_Timestamp" = "AEDT_Timestamp"),
  match_fun = list(`==`, function(f, b) abs(as.numeric(difftime(f, b, units = "secs"))) <= 3)
)

matches %>%
  filter(matched_post) %>%
  count(species, name = "n_matches") %>%
  ggplot(aes(x = reorder(species, -n_matches), y = n_matches)) +
  geom_col(fill = "steelblue") +
  labs(
    x = "Species",
    y = "Number of TRUE Matches",
    title = "Field Observations Matched in Post Within ±3 Seconds"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

matches %>%
  filter(matched_post == FALSE) %>%
  count(species, name = "n_matches") %>%
  ggplot(aes(x = reorder(species, -n_matches), y = n_matches)) +
  geom_col(fill = "steelblue") +
  labs(
    x = "Species",
    y = "Number of FALSE Matches",
    title = "Field Observations Matched in Post Within ±3 Seconds"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



#update to proportion of time bins
# Step 1: Add 6-second time bins
both_binned <- both %>%
  mutate(time_bin = floor_date(AEDT_Timestamp, unit = "6 seconds"))

# Step 2: Summarize presence in each bin by species
bin_presence <- both_binned %>%
  distinct(species, annotation, time_bin) %>%
  pivot_wider(
    names_from = annotation,
    values_from = annotation,
    values_fn = length,
    values_fill = 0
  )

# Step 3: Count bins where species was in field, and where it was also in post
match_proportions_bin <- bin_presence %>%
  filter(field > 0) %>%  # keep only bins where the species was recorded in field
  group_by(species) %>%
  summarise(
    n_field_bins = n(),
    n_matched_bins = sum(post > 0),
    prop_matched = n_matched_bins / n_field_bins
  ) %>%
  ungroup()

ggplot(match_proportions_bin, aes(x = reorder(species, -prop_matched), y = prop_matched)) +
  geom_col(fill = "darkorange") +
  labs(
    x = "Species",
    y = "Proportion of Field Time Bins Also in Post",
    title = "Time Bin Match Rate by Species (6-second bins)"
  ) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 1)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


######## binary time bins 
# Step 1: Bin timestamps into 6-second chunks
both_binned <- both %>%
  mutate(time_bin = floor_date(AEDT_Timestamp, unit = "10 seconds"))

# Step 2: Create presence-absence matrix per species per time bin
presence <- both_binned %>%
  distinct(species, annotation, time_bin) %>%
  pivot_wider(
    names_from = annotation,
    values_from = annotation,
    values_fn = length,
    values_fill = 0
  ) %>%
  mutate(
    detection_status = case_when(
      field > 0 & post > 0 & birdnet > 0 ~ "all",
      field > 0 & post > 0 & birdnet == 0 ~ "field_and_post",
      field > 0 & post == 0 & birdnet > 0 ~ "field_and_birdnet",
      post > 0 & field == 0 & birdnet > 0 ~ "post_and_birdnet",
      field > 0 & post == 0 & birdnet == 0 ~ "field_only",
      post > 0 & field == 0 & birdnet == 0 ~ "post_only",
      birdnet > 0 & field == 0 & post == 0 ~ "birdnet_only",
      TRUE ~ "none"
    )
  )

# Step 3: Plot as a tile grid
ggplot(presence, aes(x = time_bin, y = species, fill = detection_status)) +
  geom_tile(color = "white", width = 10) +
  scale_fill_manual(
    values = c(
      "none" = "#D3D3D3",            # Light gray
      "field_only" = "#4CAF50",      # Green
      "post_only" = "#2196F3",       # Blue
      "birdnet_only" = "#FF9800",    # Orange
      "field_and_post" = "#00796B",  # Teal (green-blue)
      "field_and_birdnet" = "#795548", # Brownish (green-orange)
      "post_and_birdnet" = "#9C27B0", # Purple (blue-orange)
      "all" = "#212121"              # Dark gray/black
    ),
    name = "Detection"
  ) +
  labs(
    x = "Time (10-second bins)",
    y = "Species",
    title = "Species Detection by Field, Post, and Birdnet in 10-Second Time Bins"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid = element_blank()
  )
