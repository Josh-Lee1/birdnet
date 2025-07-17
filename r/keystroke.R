# keystroke
library(shiny)
library(lubridate)# For timestamp conversion
library(tidyverse)

#wrangle the translation table
list <- read.csv("data/smiths_summer_birds.csv") %>%
  filter(!X1 %in% c("Exotic: Naturalized", "Exotic: Escapee")) %>%
  mutate(group = row_number() %% 3) %>%
  filter(group %in% c(1, 2)) %>%
  mutate(row_group = ceiling(row_number() / 2)) %>%
  pivot_wider(names_from = group, values_from = X1, names_prefix = "Group_") %>%
  rename(Species = Group_1, Frequency = Group_2) %>%
  select(Species, Frequency) %>% 
  mutate(
    Name = str_replace(Species, "\\s+\\w+\\s+\\w+$", ""),
    Species = str_extract(Species, "\\w+\\s+\\w+$")
  ) 
write.csv(list, file = "data/sl_list_forallocation.csv", row.names = FALSE)
#read it back in with keystroke assignment
trans<- read.csv("data/sl_list_allocated.csv") %>% 
  select(Key, Name) %>% 
  filter(Key != "")




# Define Server
server <- function(input, output, session) {
  # Use the "trans" data frame to create key_lookup
  key_lookup <- trans %>% 
    select(Key, Name) %>% 
    distinct() %>% 
    as.data.frame(stringsAsFactors = FALSE)
  
  
  # Reactive data frame to store key press logs
  key_log <- reactiveVal(data.frame(Key = character(0), Timestamp = character(0), AEDT_Timestamp = character(0), Translation = character(0)))
  
  observeEvent(input$key_data, {
    # List of keys to ignore
    keys_to_ignore <- c("CapsLock", "ArrowUp", "ArrowDown", "Delete")
    
    # Get the current log
    current_log <- key_log()
    
    # Check if the key is in the ignored list
    if (input$key_data$key %in% keys_to_ignore) {
      if (input$key_data$key == "Delete" && nrow(current_log) > 0) {
        # Remove the last entry if 'Delete' key is pressed
        updated_log <- current_log[-nrow(current_log), ]
        key_log(updated_log)
      }
      return() # Exit without logging the key
    }
    
    # Convert the timestamp to AEDT
    utc_time <- ymd_hms(input$key_data$timestamp, tz = "UTC") # Parse the ISO 8601 timestamp in UTC
    aedt_time <- with_tz(utc_time, "Australia/Sydney")         # Convert to AEDT (Sydney time zone)
    
    # Find the translation for the key
    translation <- key_lookup$Name[key_lookup$Key == input$key_data$key]
    if (length(translation) == 0) translation <- NA # Handle keys not in the lookup table
    
    # Add the new entry
    new_entry <- data.frame(
      Key = input$key_data$key,
      Timestamp = as.character(utc_time),
      AEDT_Timestamp = as.character(aedt_time),
      Translation = translation,
      stringsAsFactors = FALSE
    )
    
    updated_log <- unique(rbind(current_log, new_entry)) # Keep only unique keys
    key_log(updated_log)
  })
  
  # Render table with the logs
  output$key_table <- renderTable({
    key_log()
  })
  
  # Provide a download handler
  output$download_log <- downloadHandler(
    filename = function() {
      # Save the app start time
      app_start_time <- Sys.time()
      paste0("key_log_", format(app_start_time, "%Y-%m-%d_%H-%M-%S"), ".csv")
    },
    content = function(file) {
      write_csv(key_log(), file)
    }
  )
}

# Run the app
shinyApp(ui, server)


