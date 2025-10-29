library(tidyverse)
library(dplyr)
library(readxl)

customer_data_old = read.csv(file.choose(new = F)) ##customer master data

customer_data_new = read.csv(file.choose(new = F)) ##customer master data
inventory = read.csv(file.choose(new = F),colClasses = c(SERIAL_NUMBER="character")) ##inventory file
inventory_sel = inventory %>% select(SERIAL_NUMBER,ITEM_CODE,ENTITY_CODE)

customer_data_old_clean <- customer_data_old %>% filter(!is.na(Stb) & !is.na(Vc) & Stb != "" & Vc != "")
customer_data_new_clean <- customer_data_new %>% filter(!is.na(Stb) & !is.na(Vc) & Stb != "" & Vc != "")

# Step 1: Extract unique STB-VC pairs from old data
old_pairs <- customer_data_old_clean %>%
  select(Stb, Vc) %>%
  distinct()

# Step 2: Extract unique STB-VC pairs from new data
new_pairs <- customer_data_new_clean %>%
  select(Stb, Vc) %>%
  distinct()

# Step 3: Find pairs in old that are NOT in new
discontinued_pairs <- anti_join(old_pairs, new_pairs, by = c("Stb", "Vc"))

#join inventory data
# Step 1: Ensure no missing/empty serials (optional but safe)
discontinued_pairs_clean <- discontinued_pairs %>%
  filter(!is.na(Stb) & Stb != "", !is.na(Vc) & Vc != "")

# Step 2: Join STB info
discontinued_with_stb <- discontinued_pairs_clean %>%
  left_join(
    inventory_sel %>%
      rename(STB_ITEM_CODE = ITEM_CODE, STB_ENTITY_CODE = ENTITY_CODE),
    by = c("Stb" = "SERIAL_NUMBER")
  )

# Step 3: Join VC info
discontinued_enriched <- discontinued_with_stb %>%
  left_join(
    inventory_sel %>%
      rename(VC_ITEM_CODE = ITEM_CODE, VC_ENTITY_CODE = ENTITY_CODE),
    by = c("Vc" = "SERIAL_NUMBER")
  )



##add blank customers of LCO##

# Step 1: Get blank customers (no STB, no VC) from customer_data_new
blank_customers <- customer_data_new %>%
  filter(
    (is.na(Stb) | Stb == "") &
      (is.na(Vc)  | Vc  == "")
  ) %>%
  select(Customer.Number, Entity.Code) %>%
  distinct() %>%
  mutate(used = FALSE)  # track usage

# Convert to data.frame for easier row-wise update (or use tibble with rowwise())
blank_customers <- as.data.frame(blank_customers, stringsAsFactors = FALSE)

# Step 2: Prepare discontinued_enriched
# Ensure it's a data frame for consistent indexing
discontinued_df <- as.data.frame(discontinued_enriched, stringsAsFactors = FALSE)
discontinued_df$assigned_customer <- NA_character_  # initialize

# Step 3: Greedy assignment loop
for (i in seq_len(nrow(discontinued_df))) {
  stb_ent <- discontinued_df$STB_ENTITY_CODE[i]
  vc_ent  <- discontinued_df$VC_ENTITY_CODE[i]
  
  # Find available customers matching either entity
  match_idx <- which(
    !blank_customers$used &
      (
        (!is.na(stb_ent) & blank_customers$Entity.Code == stb_ent) |
          (!is.na(vc_ent)  & blank_customers$Entity.Code == vc_ent)
      )
  )
  
  if (length(match_idx) > 0) {
    # Take the first available match
    chosen_idx <- match_idx[1]
    discontinued_df$assigned_customer[i] <- blank_customers$Customer.Number[chosen_idx]
    blank_customers$used[chosen_idx] <- TRUE  # consume it
  }
  # else: remains NA
}

# Step 4: Convert back to tibble (optional)
final_result <- as_tibble(discontinued_df)

write.csv(final_result, "Blank_box_with_blank_customer_number.csv",row.names = F)
