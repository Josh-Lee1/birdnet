library(tidyverse)
library(lubridate)

df<- read_rds("data/cleaned_raw_firsthour.rds") %>% 
  slice(-1) %>% 
  filter(AEDT_Timestamp < "2025-01-19 15:33:06")

# Get the actual start time of your data
start_time <- min(df$AEDT_Timestamp)

# Define bin durations in seconds
bin_durations <- list(
  bin_3s   = 3,
  bin_10s  = 10,
  bin_30s  = 30,
  bin_1m   = 60,
  bin_5m   = 5 * 60,
  bin_10m  = 10 * 60,
  bin_30m  = 30 * 60,
  bin_1h   = 60 * 60
)

# Apply binning
df_binned <- df %>%
  mutate(secs_since_start = as.numeric(difftime(AEDT_Timestamp, start_time, units = "secs"))) %>%
  mutate(
    bin_3s   = start_time + floor(secs_since_start / bin_durations$bin_3s)   * bin_durations$bin_3s,
    bin_10s  = start_time + floor(secs_since_start / bin_durations$bin_10s)  * bin_durations$bin_10s,
    bin_30s  = start_time + floor(secs_since_start / bin_durations$bin_30s)  * bin_durations$bin_30s,
    bin_1m   = start_time + floor(secs_since_start / bin_durations$bin_1m)   * bin_durations$bin_1m,
    bin_5m   = start_time + floor(secs_since_start / bin_durations$bin_5m)   * bin_durations$bin_5m,
    bin_10m  = start_time + floor(secs_since_start / bin_durations$bin_10m)  * bin_durations$bin_10m,
    bin_30m  = start_time + floor(secs_since_start / bin_durations$bin_30m)  * bin_durations$bin_30m,
    bin_1h   = start_time + floor(secs_since_start / bin_durations$bin_1h)   * bin_durations$bin_1h
  ) %>%
  select(-secs_since_start)

# Your bin column names
bin_cols <- c("bin_3s", "bin_10s", "bin_30s", "bin_1m", "bin_5m", "bin_10m", "bin_30m", "bin_1h")

# Create a list of detection matrices, one per bin size
detection_matrices <- map(bin_cols, function(bin_col) {
  df_binned %>%
    group_by(species, !!sym(bin_col), annotation) %>%
    summarise(present = 1, .groups = "drop") %>%
    pivot_wider(
      names_from = annotation,
      values_from = present,
      values_fill = list(present = 0)
    ) %>%
    rename(time_bin = !!sym(bin_col))
})

# Name the list elements by the bin columns for easy reference
names(detection_matrices) <- bin_cols

compute_jaccard <- function(df, method1, method2) {
  df %>%
    mutate(
      both_present = !!sym(method1) & !!sym(method2),
      either_present = (!!sym(method1) | !!sym(method2))
    ) %>%
    summarise(
      jaccard = sum(both_present) / sum(either_present)
    ) %>%
    pull(jaccard)
}

# Example for 1-minute bin matrix:
compute_jaccard(detection_matrices$bin_1m, "field", "post")

method_pairs <- list(
  c("field", "post"),
  c("post", "birdnet"),
  c("birdnet", "field")
)

results <- map_df(bin_cols, function(bin) {
  map_df(method_pairs, function(pair) {
    df <- detection_matrices[[bin]]
    tibble(
      bin_size = bin,
      method1 = pair[1],
      method2 = pair[2],
      jaccard = compute_jaccard(df, pair[1], pair[2])
    )
  })
})


# Convert bin names to seconds
results <- results %>%
  mutate(
    bin_seconds = case_when(
      bin_size == "bin_3s"  ~ 3,
      bin_size == "bin_10s" ~ 10,
      bin_size == "bin_30s" ~ 30,
      bin_size == "bin_1m"  ~ 60,
      bin_size == "bin_5m"  ~ 5 * 60,
      bin_size == "bin_10m" ~ 10 * 60,
      bin_size == "bin_30m" ~ 30 * 60,
      bin_size == "bin_1h"  ~ 60 * 60,
      TRUE ~ NA_real_
    )
  )


# Create a combined label for method pairs
results <- results %>%
  mutate(pair = paste(method1, "-", method2))

ggplot(results, aes(x = bin_seconds, y = jaccard, color = pair)) +
  geom_point(size = 3) +
  geom_line(aes(group = pair), size = 1) +
  scale_x_log10(
    breaks = c(3,10,30,60,300,600,1800,3600),
    labels = c("3s","10s","30s","1m","5m","10m","30m","1h")
  ) +
  labs(
    x = "Time Bin Size",
    y = "Jaccard Index (Agreement)",
    color = "Method Pair",
    title = "Agreement of Species Detection vs. Time Bin Size",
    subtitle = "Jaccard index comparing pairs of methods"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom"
  )

###########
# Try for each species

# Jaccard function (reuse)
compute_jaccard <- function(df, method1, method2) {
  x <- df[[method1]]
  y <- df[[method2]]
  both_present <- sum(x == 1 & y == 1)
  either_present <- sum(x == 1 | y == 1)
  if (either_present == 0) return(NA)
  both_present / either_present
}

# List of bin columns
bin_cols <- c("bin_3s", "bin_10s", "bin_30s", "bin_1m", "bin_5m", "bin_10m", "bin_30m", "bin_1h")

# Get list of species
all_species <- unique(df_binned$species)

# Create long dataframe of Jaccard similarities per species, bin size, and comparison
# Jaccard function (reuse)
compute_jaccard <- function(df, method1, method2) {
  x <- df[[method1]]
  y <- df[[method2]]
  both_present <- sum(x == 1 & y == 1)
  either_present <- sum(x == 1 | y == 1)
  if (either_present == 0) return(NA)
  both_present / either_present
}

# List of bin columns
bin_cols <- c("bin_3s", "bin_10s", "bin_30s", "bin_1m", "bin_5m", "bin_10m", "bin_30m", "bin_1h")

# Get list of species
all_species <- unique(df_binned$species)

# Create long dataframe of Jaccard similarities per species, bin size, and comparison
agreement_by_species <- purrr::map_dfr(all_species, function(sp) {
  df_sp <- df_binned %>% filter(species == sp)
  
  purrr::map_dfr(bin_cols, function(bin_col) {
    df_bin <- df_sp %>%
      group_by(across(all_of(c(bin_col, "annotation")))) %>%
      summarise(detected = n() > 0, .groups = "drop") %>%
      pivot_wider(names_from = annotation, values_from = detected, values_fill = FALSE) %>%
      rename(bin = !!sym(bin_col))
    
    tibble(
      species = sp,
      bin_size = bin_col,
      jaccard_field_post = compute_jaccard(df_bin, "field", "post"),
      jaccard_post_birdnet = compute_jaccard(df_bin, "post", "birdnet"),
      jaccard_birdnet_field = compute_jaccard(df_bin, "birdnet", "field")
    )
  })
})

agreement_by_species_long <- agreement_by_species %>%
  pivot_longer(
    cols = starts_with("jaccard"),
    names_to = "comparison",
    values_to = "jaccard"
  ) %>%
  mutate(
    bin_size = factor(bin_size, levels = bin_cols),
    comparison = recode(comparison,
                        jaccard_field_post = "field vs post",
                        jaccard_post_birdnet = "post vs birdnet",
                        jaccard_birdnet_field = "birdnet vs field")
  )

ggplot(agreement_by_species_long, aes(x = bin_size, y = jaccard, color = comparison, group = comparison)) +
  geom_line(size = 1) +
  facet_wrap(~ species) +
  labs(x = "Time bin size", y = "Jaccard similarity", color = "Comparison") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


############
# Venn Diagram
library(ggvenn)
library(patchwork)

# ---- Function to get logical combinations for a Venn diagram ----
get_venn_counts <- function(df_bin) {
  df_bin %>%
    mutate(combo = paste0(as.integer(field), as.integer(post), as.integer(birdnet))) %>%
    count(combo) %>%
    {
      # Convert counts into sets
      list(
        field = which(df_bin$field),
        post = which(df_bin$post),
        birdnet = which(df_bin$birdnet)
      )
    }
}

# ---- A better approach: use set labels rather than indexes ----
# This helper gets sets of timestamps for each method
get_sets <- function(df_bin) {
  # Give each time bin a unique ID
  df_bin <- df_bin %>% mutate(id = row_number())
  
  list(
    field   = df_bin %>% filter(field)   %>% pull(id),
    post    = df_bin %>% filter(post)    %>% pull(id),
    birdnet = df_bin %>% filter(birdnet) %>% pull(id)
  )
}

# ---- Plot venns for all bins ----
venn_plots <- imap(detection_matrices, function(df_bin, bin_name) {
  sets <- get_sets(df_bin)
  ggvenn(sets, fill_color = c("#E41A1C", "#377EB8", "#4DAF4A")) + 
    ggtitle(bin_name)
})

# ---- Combine with patchwork ----
venn_timebins<- wrap_plots(venn_plots, ncol = 4) + plot_annotation(title = "Detection Agreement per Time Bin")
ggsave("figures/venn_timebins.png", venn_timebins, width = 12, height = 8, dpi = 300)
