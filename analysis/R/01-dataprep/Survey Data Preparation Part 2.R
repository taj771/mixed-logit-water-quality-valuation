################################################################################
# Survey Data Preparation - Part 2: FIX INCONSISTENT COLUMN NAMES
#
# CHASR's export names the choice columns inconsistently - the block id sits in
# a different place depending on the question, so the same variable appears
# under several patterns. This script standardises them into one scheme, which
# is what lets Part 5 reshape the data into long format cleanly.
#
# IN   data/derived/test/column_names.csv        (from Part 1)
# OUT  data/derived/test/corrected_colnames.csv  (used by Part 5)
#
# Note: line ~296 has a commented-out read from an old OneDrive path. It is dead
# code, left in place deliberately - the folder it points at no longer exists.
################################################################################
################################################################################
# Description : This code is aim to make consistent name patterns across choices. 
# As CHAISR's output files uneven patterns in someplace. So it will make
# easy during the process of making it long format with similar name patterns
################################################################################

# Clean the memory
rm(list = ls())  # Removes all objects in the environment


# filtered column names from original data frame - this contain all the coloum names in original data file
df <- read.csv("data/derived/test/column_names.csv")


df <- df %>%
  select(x) %>%
  filter(x != "CaseId", x != "SUB_BASIN") %>%
  rename(choice_name = x) %>%
  filter(!is.na(choice_name) & choice_name != "PC_32_1" & choice_name != "PC_LSN_NS_2")  # Confirmed with Danny coding errors and will correct fin final launch. Anyway nothing affect to the data


#Extract block id from choice name used by CHAISR
df$B_number <- sub(".*B(\\d+[A]?)_(\\d+).*", "\\1_\\2", df$choice_name)

# Following column names need additional works to extract the block id from column name 
# because of the inconsistent of naming patterns

# Define the named list of patterns to replace
pattern_groups <- list(
  "B17_1A" = c(
    "AR_B17_1A_COST", "AR_B17_1ACURRENT_1", "AR_B17_1ACURRENT_2", "AR_B17_1ACURRENT_3",
    "AR_B17_1ACURRENT_4","AR_B17_1ACURRENT_5","AR_B17_1IMAGE_CURRENT", "AR_B17_1ACURRENT_AVERAGE", "AR_B17_1APOLICY_1",
    "AR_B17_1APOLICY_2", "AR_B17_1APOLICY_3", "AR_B17_1APOLICY_4", "AR_B17_1APOLICY_5","AR_B17_1IMAGE_POLICY",
    "AR_B17_1APOLICY_AVERAGE", "AR_B17_1APOLICY_SIZE_KM", "AR_B17_1APOLICY_SIZE_PERCENT",
    "PC_AR_B17_1A"
  ),
  "B17_2A" = c(
    "AR_B17_2A_COST", "AR_B17_2ACURRENT_1", "AR_B17_2ACURRENT_2", "AR_B17_2ACURRENT_3",
    "AR_B17_2ACURRENT_4", "AR_B17_2ACURRENT_5","AR_B17_2AIMAGE_CURRENT" ,"AR_B17_2ACURRENT_AVERAGE", "AR_B17_2APOLICY_1",
    "AR_B17_2APOLICY_2", "AR_B17_2APOLICY_3", "AR_B17_2APOLICY_4", "AR_B17_2APOLICY_5","AR_B17_2AIMAGE_POLICY",
    "AR_B17_2APOLICY_AVERAGE", "AR_B17_2APOLICY_SIZE_KM", "AR_B17_2APOLICY_SIZE_PERCENT",
    "PC_AR_B17_2A"
  ),
  
  "B22_1A" = c("AR_B22_1A_COST","AR_B22_1ACURRENT_1","AR_B22_1ACURRENT_2","AR_B22_1ACURRENT_3",
               "AR_B22_1ACURRENT_4","AR_B22_1ACURRENT_5","AR_B22_1AIMAGE_CURRENT","AR_B22_1ACURRENT_AVERAGE","AR_B22_1APOLICY_1",
               "AR_B22_1APOLICY_2","AR_B22_1APOLICY_3","AR_B22_1APOLICY_4","AR_B22_1APOLICY_5","AR_B22_1AIMAGE_POLICY",
               "AR_B22_1APOLICY_AVERAGE","AR_B22_1APOLICY_SIZE_KM","AR_B22_1APOLICY_SIZE_PERCENT",
               "PC_AR_B22_1A"
  ),
  
  "B22_2A" = c("AR_B22_2A_COST","AR_B22_2ACURRENT_1","AR_B22_2ACURRENT_2","AR_B22_2ACURRENT_3",
               "AR_B22_2ACURRENT_4","AR_B22_2ACURRENT_5","AR_B22_2AIMAGE_CURRENT","AR_B22_2ACURRENT_AVERAGE","AR_B22_2APOLICY_1",
               "AR_B22_2APOLICY_2","AR_B22_2APOLICY_3","AR_B22_2APOLICY_4","AR_B22_2APOLICY_5","AR_B22_2AIMAGE_POLICY",
               "AR_B22_2APOLICY_AVERAGE","AR_B22_2APOLICY_SIZE_KM","AR_B22_2APOLICY_SIZE_PERCENT",
               "PC_AR_B22_2A"
  ),
  
  "B23_2A" = c("AR_B23_2A_COST","AR_B23_2ACURRENT_1","AR_B23_2ACURRENT_2","AR_B23_2ACURRENT_3",
               "AR_B23_2ACURRENT_4","AR_B23_2ACURRENT_5","AR_B23_2AIMAGE_CURRENT","AR_B23_2ACURRENT_AVERAGE","AR_B23_2APOLICY_1",
               "AR_B23_2APOLICY_2","AR_B23_2APOLICY_3","AR_B23_2APOLICY_4","AR_B23_2APOLICY_5","AR_B23_2AIMAGE_POLICY",
               "AR_B23_2APOLICY_AVERAGE","AR_B23_2APOLICY_SIZE_KM","AR_B23_2APOLICY_SIZE_PERCENT",
               "PC_AR_B23_2A"
  ),
  "B23_1A" = c("AR_B23_1A_COST","AR_B23_1ACURRENT_1","AR_B23_1ACURRENT_2","AR_B23_1ACURRENT_3",
               "AR_B23_1ACURRENT_4","AR_B23_1ACURRENT_5","AR_B23_1AIMAGE_CURRENT","AR_B23_1ACURRENT_AVERAGE","AR_B23_1APOLICY_1",
               "AR_B23_1APOLICY_2","AR_B23_1APOLICY_3","AR_B23_1APOLICY_4","AR_B23_1APOLICY_5","AR_B23_1AIMAGE_POLICY",
               "AR_B23_1APOLICY_AVERAGE","AR_B23_1APOLICY_SIZE_KM","AR_B23_1APOLICY_SIZE_PERCENT",
               "PC_AR_B23_1A"
  ),
  "B24_1A" = c("AR_B24_1A_COST","AR_B24_1ACURRENT_1","AR_B24_1ACURRENT_2","AR_B24_1ACURRENT_3",
               "AR_B24_1ACURRENT_4","AR_B24_1ACURRENT_5","AR_B24_1AIMAGE_CURRENT","AR_B24_1ACURRENT_AVERAGE","AR_B24_1APOLICY_1",
               "AR_B24_1APOLICY_2","AR_B24_1APOLICY_3","AR_B24_1APOLICY_4","AR_B24_1APOLICY_5","AR_B24_1AIMAGE_POLICY",
               "AR_B24_1APOLICY_AVERAGE","AR_B24_1APOLICY_SIZE_KM","AR_B24_1APOLICY_SIZE_PERCENT",
               "PC_AR_B24_1A"
  ),
  "B24_2A" = c("AR_B24_2A_COST","AR_B24_2ACURRENT_1","AR_B24_2ACURRENT_2","AR_B24_2ACURRENT_3",
               "AR_B24_2ACURRENT_4","AR_B24_2ACURRENT_5","AR_B24_2AIMAGE_CURRENT","AR_B24_2ACURRENT_AVERAGE","AR_B24_2APOLICY_1",
               "AR_B24_2APOLICY_2","AR_B24_2APOLICY_3","AR_B24_2APOLICY_4","AR_B24_2APOLICY_5","AR_B24_2AIMAGE_POLICY",
               "AR_B24_2APOLICY_AVERAGE","AR_B24_2APOLICY_SIZE_KM","AR_B24_2APOLICY_SIZE_PERCENT",
               "PC_AR_B24_2A"
  ),
  "B25_1A" = c("AR_B25_1A_COST","AR_B25_1ACURRENT_1","AR_B25_1ACURRENT_2","AR_B25_1ACURRENT_3",
               "AR_B25_1ACURRENT_4","AR_B25_1ACURRENT_5","AR_B25_1AIMAGE_CURRENT","AR_B25_1ACURRENT_AVERAGE","AR_B25_1APOLICY_1",
               "AR_B25_1APOLICY_2","AR_B25_1APOLICY_3","AR_B25_1APOLICY_4","AR_B25_1APOLICY_5","AR_B25_1AIMAGE_POLICY",
               "AR_B25_1APOLICY_AVERAGE","AR_B25_1APOLICY_SIZE_KM","AR_B25_1APOLICY_SIZE_PERCENT",
               "PC_AR_B25_1A"
  ),
  "B25_2A" = c("AR_B25_2A_COST","AR_B25_2ACURRENT_1","AR_B25_2ACURRENT_2","AR_B25_2ACURRENT_3",
               "AR_B25_2ACURRENT_4","AR_B25_2ACURRENT_5","AR_B25_2AIMAGE_CURRENT","AR_B25_2ACURRENT_AVERAGE","AR_B25_2APOLICY_1",
               "AR_B25_2APOLICY_2","AR_B25_2APOLICY_3","AR_B25_2APOLICY_4","AR_B25_2APOLICY_5","AR_B25_2AIMAGE_POLICY",
               "AR_B25_2APOLICY_AVERAGE","AR_B25_2APOLICY_SIZE_KM","AR_B25_2APOLICY_SIZE_PERCENT",
               "PC_AR_B25_2A"
  ),
  "B3_1R2" = c("AR_B3_1_COST_R2","AR_B3_1CURRENT_1_R2","AR_B3_1CURRENT_2_R2","AR_B3_1CURRENT_3_R2",
               "AR_B3_1CURRENT_4_R2","AR_B3_1CURRENT_5_R2","AR_B3_1IMAGE_CURRENT_R2","AR_B3_1CURRENT_AVERAGE_R2","AR_B3_1POLICY_1_R2",
               "AR_B3_1POLICY_2_R2","AR_B3_1POLICY_3_R2","AR_B3_1POLICY_4_R2","AR_B3_1POLICY_5_R2","AR_B3_1IMAGE_POLICY_R2",
               "AR_B3_1POLICY_AVERAGE_R2","AR_B3_1POLICY_SIZE_KM_R2","AR_B3_1POLICY_SIZE_PERCENT_R2",
               "PC_AR_B3_1_R2"
  ),
  "B3_1R1" = c("AR_B3_1_COST_R1","AR_B3_1CURRENT_1_R1","AR_B3_1CURRENT_2_R1","AR_B3_1CURRENT_3_R1",
               "AR_B3_1CURRENT_4_R1","AR_B3_1CURRENT_5_R1","AR_B3_1IMAGE_CURRENT_R1","AR_B3_1CURRENT_AVERAGE_R1","AR_B3_1POLICY_1_R1",
               "AR_B3_1POLICY_2_R1","AR_B3_1POLICY_3_R1","AR_B3_1POLICY_4_R1","AR_B3_1POLICY_5_R1","AR_B3_1IMAGE_POLICY_R1",
               "AR_B3_1POLICY_AVERAGE_R1","AR_B3_1POLICY_SIZE_KM_R1","AR_B3_1POLICY_SIZE_PERCENT_R1",
               "PC_AR_B3_1_R1"
  ),
  "B3_2R1" = c("AR_B3_2_COST_R1","AR_B3_2CURRENT_1_R1","AR_B3_2CURRENT_2_R1","AR_B3_2CURRENT_3_R1",
               "AR_B3_2CURRENT_4_R1","AR_B3_2CURRENT_5_R1","AR_B3_2IMAGE_CURRENT_R1","AR_B3_2CURRENT_AVERAGE_R1","AR_B3_2POLICY_1_R1",
               "AR_B3_2POLICY_2_R1","AR_B3_2POLICY_3_R1","AR_B3_2POLICY_4_R1","AR_B3_2POLICY_5_R1","AR_B3_2IMAGE_POLICY_R1",
               "AR_B3_2POLICY_AVERAGE_R1","AR_B3_2POLICY_SIZE_KM_R1","AR_B3_2POLICY_SIZE_PERCENT_R1",
               "PC_AR_B3_2_R1"
  ),
  "B3_2R2" = c("AR_B3_2_COST_R2","AR_B3_2CURRENT_1_R2","AR_B3_2CURRENT_2_R2","AR_B3_2CURRENT_3_R2",
               "AR_B3_2CURRENT_4_R2","AR_B3_2CURRENT_5_R2","AR_B3_2IMAGE_CURRENT_R2","AR_B3_2CURRENT_AVERAGE_R2","AR_B3_2POLICY_1_R2",
               "AR_B3_2POLICY_2_R2","AR_B3_2POLICY_3_R2","AR_B3_2POLICY_4_R2","AR_B3_2POLICY_5_R2","AR_B3_2IMAGE_POLICY_R2",
               "AR_B3_2POLICY_AVERAGE_R2","AR_B3_2POLICY_SIZE_KM_R2","AR_B3_2POLICY_SIZE_PERCENT_R2",
               "PC_AR_B3_2_R2"
  ),
  "B74_3" = c("B7_3_COST","B7_3CURRENT_1","B7_3CURRENT_2","B7_3CURRENT_3","B7_3CURRENT_4",     #### clarify with Danny
              "B7_3CURRENT_5","B7_3CURRENT_AVERAGE","B7_3IMAGE_CURRENT","B7_3POLICY_1","B7_3POLICY_2",
              "B7_3POLICY_3","B7_3POLICY_4","B7_3POLICY_5","B7_3IMAGE_POLICY","B7_3POLICY_AVERAGE",
              "B7_3POLICY_SIZE_KM","B7_3POLICY_SIZE_PERCENT","PC_NS_B7_3"
              
  ),
  
  "B56_1" = c("B56_COST","B56CURRENT_1","B56CURRENT_2","B56CURRENT_3","B56CURRENT_4",
              "B56CURRENT_5","B56IMAGE_CURRENT","B56CURRENT_AVERAGE","B56POLICY_1","B56POLICY_2","B56POLICY_3",
              "B56POLICY_4","B56POLICY_5","B56IMAGE_POLICY","B56POLICY_AVERAGE","B56POLICY_SIZE_KM",
              "B56POLICY_SIZE_PERCENT", "PC_LSN_B56_1"
              
  ),
  "B49_1" = c("LSN_49_1_COST","LSN_49_1CURRENT_1","LSN_49_1CURRENT_2","LSN_49_1CURRENT_3",
              "LSN_49_1CURRENT_4","LSN_49_1CURRENT_5","LSN_49_1IMAGE_CURRENT","LSN_49_1CURRENT_AVERAGE","LSN_49_1POLICY_1",
              "LSN_49_1POLICY_2","LSN_49_1POLICY_3","LSN_49_1POLICY_4","LSN_49_1POLICY_5","LSN_49_1IMAGE_POLICY",
              "LSN_49_1POLICY_AVERAGE","LSN_49_1POLICY_SIZE_KM","LSN_49_1POLICY_SIZE_PERCENT",
              "PC_LSN_B49_1"
  ),
  "B43_2" = c("LSN_43_2_COST","LSN_43_2CURRENT_1","LSN_43_2CURRENT_2","LSN_43_2CURRENT_3",
              "LSN_43_2CURRENT_4","LSN_43_2CURRENT_5","LSN_43_2IMAGE_CURRENT","LSN_43_2CURRENT_AVERAGE",
              "LSN_43_2POLICY_1","LSN_43_2POLICY_2","LSN_43_2POLICY_3","LSN_43_2POLICY_4",
              "LSN_43_2POLICY_5","LSN_43_2IMAGE_POLICY","LSN_43_2POLICY_AVERAGE",
              "LSN_43_2POLICY_SIZE_KM","LSN_43_2POLICY_SIZE_PERCENT",
              "PC_B43_2"
  ),
  
  "B43_1" = c("LSN_43_1_COST","LSN_43_1CURRENT_1","LSN_43_1CURRENT_2","LSN_43_1CURRENT_3",
              "LSN_43_1CURRENT_4","LSN_43_1CURRENT_5","LSN_43_1IMAGE_CURRENT","LSN_43_1","LSN_43_1CURRENT_AVERAGE",
              "LSN_43_1POLICY_1","LSN_43_1POLICY_2","LSN_43_1POLICY_3","LSN_43_1POLICY_4",
              "LSN_43_1POLICY_5","LSN_43_1IMAGE_POLICY","LSN_43_1POLICY_AVERAGE",
              "LSN_43_1POLICY_SIZE_KM","LSN_43_1POLICY_SIZE_PERCENT",
              "PC_B43_1"
  ),
  "B12_2" = c("AR_12_2POLICY_SIZE_PERCENT","AR_12_2CURRENT_4","PC_AR_B12_2",
              "AR_12_2POLICY_4","AR_12_2POLICY_1","AR_12_2CURRENT_1",
              "AR_12_2CURRENT_AVERAGE","AR_12_2POLICY_2",
              "AR_12_2POLICY_3","AR_12_2CURRENT_3","AR_12_2POLICY_AVERAGE",
              "AR_12_2CURRENT_2","AR_12_2_COST","AR_12_2POLICY_SIZE_KM",
              "AR_12_2CURRENT_5","AR_12_2POLICY_5", "AR_12_2IMAGE_CURRENT", "AR_12_2IMAGE_POLICY"
  ),
  "B12_3" = c("AR_12_3POLICY_SIZE_PERCENT","AR_12_3CURRENT_4","PC_AR_B12_3",
              "AR_12_3POLICY_4","AR_12_3POLICY_1","AR_12_3CURRENT_1",
              "AR_12_3CURRENT_AVERAGE","AR_12_3POLICY_2",
              "AR_12_3POLICY_3","AR_12_3CURRENT_3","AR_12_3POLICY_AVERAGE",
              "AR_12_3CURRENT_2","AR_12_3_COST","AR_12_3POLICY_SIZE_KM",
              "AR_12_3CURRENT_5","AR_12_3POLICY_5", "AR_12_3IMAGE_CURRENT", "AR_12_3IMAGE_POLICY"
  ),
  "B12_4" = c("AR_12_4POLICY_SIZE_PERCENT","AR_12_4CURRENT_4","PC_AR_B12_4",
              "AR_12_4POLICY_4","AR_12_4POLICY_1","AR_12_4CURRENT_1",
              "AR_12_4CURRENT_AVERAGE","AR_12_4POLICY_2",
              "AR_12_4POLICY_3","AR_12_4CURRENT_3","AR_12_4POLICY_AVERAGE",
              "AR_12_4CURRENT_2","AR_12_4_COST","AR_12_4POLICY_SIZE_KM",
              "AR_12_4CURRENT_5","AR_12_4POLICY_5", "AR_12_4IMAGE_CURRENT","AR_12_4IMAGE_POLICY"
  ),
  "B12_5" = c("AR_12_5POLICY_SIZE_PERCENT","AR_12_5CURRENT_4","PC_AR_B12_5",
              "AR_12_5POLICY_4","AR_12_5POLICY_1","AR_12_5CURRENT_1",
              "AR_12_5CURRENT_AVERAGE","AR_12_5POLICY_2",
              "AR_12_5POLICY_3","AR_12_5CURRENT_3","AR_12_5POLICY_AVERAGE",
              "AR_12_5CURRENT_2","AR_12_5_COST","AR_12_5POLICY_SIZE_KM",
              "AR_12_5CURRENT_5","AR_12_5POLICY_5", "AR_12_5IMAGE_CURRENT","AR_12_5IMAGE_POLICY"
  ),
  "B8_3" = c("SPATIAL_AR_B8_3POLICY_SIZE_KM","SPATIAL_AR_B8_3POLICY_1","SPATIAL_AR_B8_3CURRENT_5",
              "SPATIAL_AR_B8_3POLICY_4","SPATIAL_AR_B8_3CURRENT_2","SPATIAL_AR_B8_3POLICY_3",
              "SPATIAL_AR_B8_3CURRENT_4","SPATIAL_AR_B8_3POLICY_AVERAGE",
              "PC_AR_B8_3","SPATIAL_AR_B8_3CURRENT_AVERAGE","SPATIAL_AR_B8_3POLICY_5",
              "SPATIAL_AR_B8_3POLICY_2","SPATIAL_AR_B8_3_COST","SPATIAL_AR_B8_3CURRENT_3",
              "SPATIAL_AR_B8_3CURRENT_1","SPATIAL_AR_B8_3POLICY_SIZE_PERCENT", "SPATIAL_AR_B8_3IMAGE_CURRENT", "SPATIAL_AR_B8_3IMAGE_POLICY"
  )
)


# Flatten the pattern_groups into a single data frame for matching
patterns_df <- bind_rows(
  lapply(names(pattern_groups), function(name) {
    data.frame(pattern = pattern_groups[[name]], replacement = name, stringsAsFactors = FALSE)
  })
)

# Perform the transformation
df <- df %>%
  left_join(patterns_df, by = c("choice_name" = "pattern")) %>%
  mutate(B_number_new = ifelse(!is.na(replacement), replacement, B_number)) %>%
  select(-replacement, -B_number) %>%
  rename(ChoiceID = B_number_new)%>%
  mutate(
    # Only add 'B' to the beginning if it doesn't already start with 'B'
    ChoiceID = ifelse(substr(ChoiceID, 1, 1) != "B" & substr(ChoiceID, 1, 2) != "AR", paste0("B", ChoiceID), ChoiceID)
  )


# remove and add the _ at the following names - easy to handle and make consistent 

df <- df %>%
  mutate(
    var_name = case_when(
      grepl("COST", choice_name, ignore.case = TRUE) ~ "COST",
      grepl("CURRENT_1", choice_name, ignore.case = TRUE) ~ "CURRENT1",
      grepl("CURRENT_2", choice_name, ignore.case = TRUE) ~ "CURRENT2",
      grepl("CURRENT_3", choice_name, ignore.case = TRUE) ~ "CURRENT3",
      grepl("CURRENT_4", choice_name, ignore.case = TRUE) ~ "CURRENT4",
      grepl("CURRENT_5", choice_name, ignore.case = TRUE) ~ "CURRENT5",
      grepl("POLICY_1", choice_name, ignore.case = TRUE) ~ "POLICY1",
      grepl("POLICY_2", choice_name, ignore.case = TRUE) ~ "POLICY2",
      grepl("POLICY_3", choice_name, ignore.case = TRUE) ~ "POLICY3",
      grepl("POLICY_4", choice_name, ignore.case = TRUE) ~ "POLICY4",
      grepl("POLICY_5", choice_name, ignore.case = TRUE) ~ "POLICY5",
      
      grepl("IMAGE_CURRENT", choice_name, ignore.case = TRUE) ~ "IMAGECURRENT",
      grepl("IMAGE_POLICY", choice_name, ignore.case = TRUE) ~ "IMAGEPOLICY",
      
      
      
      grepl("POLICY_AVERAGE", choice_name, ignore.case = TRUE) ~ "POLICY_AVERAGE",
      grepl("CURRENT_AVERAGE", choice_name, ignore.case = TRUE) ~ "CURRENT_AVERAGE",
      
      grepl("POLICY_SIZE_KM", choice_name, ignore.case = TRUE) ~ "POLICY_SIZE_KM",
      
      
      grepl("POLICY_SIZE_PERCENTE", choice_name, ignore.case = TRUE) ~ "POLICY_SIZE_PERCENT",
      
      grepl("POLICY_SIZE_PERCENT", choice_name, ignore.case = TRUE) ~ "POLICY_SIZE_PERCENT",
      
      grepl("PC", choice_name, ignore.case = TRUE) ~ "VOTE",
      
      
  
      TRUE ~ NA_character_  # Assign NA for rows that don't match
    )
  )


df <- df%>%
  mutate(vaiable_name = paste(ChoiceID, var_name, sep = "_"))


# This is missing filed in original data 

df_temp <- data.frame(
  choice_name = c("B117_2POLICY_SIZE_PERCENT"),
  ChoiceID = c("B117_2"),
  var_name = c("POLICY_SIZE_PERCENT"),
  vaiable_name = c("B117_2_POLICY_SIZE_PERCENT")
)
  


# NOTE (verified 2026-09-06): B117_2POLICY_SIZE_PERCENT IS present in the current
# data, so this manual row no longer fixes a gap - it duplicated an existing entry.
# Guarded so it appends only when genuinely absent: unchanged behaviour on data
# where the column is missing, a no-op on the current data.
if (!("B117_2POLICY_SIZE_PERCENT" %in% df$choice_name)) {
  df <- rbind(df, df_temp)
}


# Addressing the issue with LSN_B43_3_COST - in original data it has name as LSN_B43

#df_temp <- data.frame(
#  choice_name = c("LSN_B43"),
#  ChoiceID = c("B43_3"),
#  var_name = c("COST"),
#  vaiable_name = c("B43_3_COST")
#)

#colnames(df_temp) <- colnames(df)

#df <- rbind(df,df_temp)


df <- df%>%
  filter(choice_name != "LSN_B12_1IMAGE_CURRENT")


# Save file and this will use to correc the names in the data set from CHAISR
write.csv(df, "data/derived/test/corrected_colnames.csv", row.names = F)


################################################################################
# Following are some quick quality checks and wont affectt ot the final results
# No need to run unless need some quality cheks
#### Quality check #### Quality check #### Quality check #### Quality check ####

#t <- read.csv("/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter III-UseNonUseValue/R/Chapter III/Output/column_names.csv")%>%
  #select(x) %>%
  #filter(x != "CaseId", x != "SUB_BASIN") %>%
 # rename(choice_name = x) %>%
 # mutate(test =1)%>%
 # filter(!is.na(choice_name) & choice_name != "PC_32_1" & choice_name != "PC_LSN_NS_2")  # Correct logical condition


#tt <- df%>%
  #left_join(t)

# Find rows with any NA
#na_rows <- tt[rowSums(is.na(tt)) > 0, ]

# Quick quality check as we extarct few variables within. the same block, the number of variable
# within block number should be same
# Summarize how many rows fall under each number after B
#summary_df <- as.data.frame(table(df$ChoiceID))

# based on above check the block number 	117_2 missing _POLICY_SIZE_PERCENT variable. So I will add that variable 
# in to data frame later



#summary_df <- summary_df %>%
  #mutate(variable_cleaned = str_remove(Var1, "_\\d+$"))


#t <- summary_df%>%
  #mutate(variable = str_replace_all(variable_cleaned, "A", ""))


#t <- t%>%
  #mutate(variable = str_remove(variable, "_.*"))

#t <- t%>%
  #distinct(variable)

#t <- t %>%
  #mutate(block = str_remove(variable, "B"))

#t$block <- as.numeric(t$block)


#tt <- ChoiceSetDesigns%>%
  #distinct(block)


#t3 <- t%>%
  #left_join(tt)





#t <- t %>%
  #filter(grepl("B3_2R1", name))

#t <- df2 %>%
  #select(contains("117_1"))  # Select columns that contain the string "PC_32_1"


#t <- df2 %>%
  #select(contains("118_1"))  # Select columns that contain the string "PC_32_1"

#colnames(t)
  
  #select(CaseId,PC_32_1, 	
         #PC_B32_1,PC_LSN_NS_2)


