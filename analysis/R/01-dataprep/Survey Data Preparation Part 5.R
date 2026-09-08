################################################################################
# Survey Data Preparation - Part 5: BUILD THE APOLLO ESTIMATION DATASET
#
# Assembly step. Joins everything the earlier parts built into the single table
# the models estimate on, and reshapes it from one row per respondent to one row
# per choice (~3,800 respondents -> ~26,000 rows), which is the long format
# Apollo needs. Builds the model variables along the way - COST, VOTE, and the
# WQ_*_LOCAL / NONLOCAL, BASIN / SUBBASIN, CURRENT / POLICY terms the whole
# chapter rests on.
#
# IN   data/raw/Water_Quality_Final.xlsx, data/raw/ChoiceSetDesigns.xlsx
#      data/derived/test/corrected_colnames.csv          (Part 2)
#      data/derived/test/respondent_characteristics.csv  (Part 3)
#      data/derived/test/final_weights.csv               (Part 4) -> WEIGHT
#      data/derived/basin_level_shares_province.csv, sub_basin_level_*.csv
#      data/derived/RecTrip_basininfo.csv                (Part 4_1, geocoded)
# OUT  data/derived/test/processed_finaldata_batch_1_Apollo.csv   (26 MB)
#
# WARNING - this overwrites the estimation dataset every model reads. The copy
# currently on disk is the one behind the manuscript's results. Re-running this
# with regenerated Part 4 weights will shift every model estimate away from the
# published numbers. Back the file up before running.
################################################################################
########################################################################################
# Description: This code aims to clean the survey data and set the data ready for Apollo
# First it create choice data set (respondent id, choice id, vote, treatment, cost, 
# other design variable) and later it add the respondent characteristics (create with
# using 02. Respondent characteristics. R) to complete the choice dataset for apallo
########################################################################################

# Clean the memory
rm(list = ls())  # Removes all objects in the environment


# Read raw data file from CHAISR
df <- read_excel("data/raw/Water_Quality_Final.xlsx")%>%
  rename(CaseId = `{Case.ID}`)%>%
  filter(is.na(SURVEY_CONTENT))%>% # 1: Movies, 2: Politics, 3: Environemnt
  # waiting for calrify from danny
  
  group_by(CaseId) %>%
  slice_max(order_by = `{Duration.of.connection.in.seconds}`, n = 1) %>%
  ungroup()

# In original file it has create a column for each data point recorded. For example, it create separate colom for 118 block desperately 
# along with all other relevant parameters for each block (i.e. Policy area, baseline WQ, improved WQ). SO in this portion of the code we 
# work on those variables of each choice that fit into Appollo format


# First we extract the all variables that varied with choice. This is based on the naming patterns of each 
# variable
df1 <- df[, grepl("COST", names(df)) |           # Original data file the name the column that contain the cost of each policy scenario as "AR_B1_1_COST", 
            #where AR_B1_1 varied depend on the block and the river basin. SO we extarct the all the column with "COST" term
            names(df) == "CaseId" |     # Respondents ID - use to map the choices for each respondents
            names(df) == "VERSION" |    # Version - use to map the choices for each respondents
            names(df) == "BASIN" |      # Basin - use to map the choices for each respondents
            names(df) == "SUB_BASIN" |  # Sub basin - use to map the choices for each respondents
            names(df) == "NON_LOCAL" |  # Basin code for their non-local basin
            names(df) == "LOCAL_AR" |   # A local watershed, which does  include your home A non-local watershed (1), which does not include your home(2)
            names(df) == "CONDITION" |  # WQ survey = 1, General survey =2
            names(df) == "TREATMENT" |  # Spatial =1, WQ =2
            
            
            grepl("PC", names(df))   |               # PC - vote scenario 1 - yes 2-no. 
            grepl("POLICY_AVERAGE", names(df))|      # Policy Average WQ 
            grepl("CURRENT_AVERAGE", names(df))|     # Current/Baseline Average WQ
            grepl("POLICY_SIZE_KM", names(df))|      # Policy area in SQKM
            grepl("POLICY_SIZE_PERCENT", names(df))| # Policy area in %
            grepl("CURRENT_1", names(df))|           # WQ_1 % in current level
            grepl("CURRENT_2", names(df))|           # WQ_2 % in current level
            grepl("CURRENT_3", names(df))|           # WQ_3 % in current level
            grepl("CURRENT_4", names(df))|           # WQ_4 % in current level
            grepl("CURRENT_5", names(df))|           # WQ_5 % in current level
            grepl("IMAGE_CURRENT", names(df)) |      # image current
            grepl("POLICY_1", names(df))|            # WQ_1 % in policy
            grepl("POLICY_2", names(df))|            # WQ_2 % in policy
            grepl("POLICY_3", names(df))|            # WQ_3 % in policy
            grepl("POLICY_4", names(df))|            # WQ_4 % in policy
            grepl("POLICY_5", names(df))|            # WQ_5 % in policy
            grepl("IMAGE_POLICY", names(df))|        # image policy
            grepl("LSN_B43", names(df))         # image policy
]%>%
  rename(spatial_1=BASIN,
         spatial_2 = SUB_BASIN)
#names(df)=="SUB_BASIN" ] # Sub Basin - The codes for each sub basin needs to be clarify with Danny, where each sub absin code as unique number (18)


#coding errors - missing observation so add variabel manually and may be reove woth final data

#df1 <- df1%>%
#mutate(B117_2POLICY_SIZE_PERCENT="6%") # this is missing filed in pilot data and Danny will fix it in final version 


# open the data frame with corrected coloum names (05.Create_consistent_ColNmeas_for_choices.R). 
# In original Danny has used different patents to name choices
# I have manually brings to a consistent pattern to handle them easy
df2 <- read_csv("data/derived/corrected_colnames.csv")%>%
  select(choice_name,vaiable_name)%>%
  rename(original=choice_name,
         new_name=vaiable_name)

# rename the original data file name by matching the pattern
df1 <- df1 %>%
  rename_with(~ifelse(!is.na(match(., df2$original)), df2$new_name[match(., df2$original)], .), .cols = names(df1))


# Take only sample of data - for initial attempts later can remove this chunk

df_filtered <- df1


# Now df_filtered data frame has all the choices with above filtered data, but most of those column are empty for 
# as per respondent receive only two block from their local and non-local river basin. So all other remain NA,
# So I create a loop that operate row wise where identify column with observed data for each respondent
# Here loop go over each respondent (in original data file observation for each respondents summarized into 
# one row)

# convert to numeric
df_filtered <- df_filtered %>%
  mutate(across(where(is.character), ~na_if(., ""))) %>%
  mutate(across(where(is.numeric), ~ifelse(is.na(.), NA, .)))


# Placeholder for storing results
final_result <- list()

# Loop through each row of df_filtered
# Vectorised replacement for a per-respondent loop that sliced one row of a
# 3,851 x 7,105 tibble at a time, dropped its all-NA columns, coerced and pivoted
# it, then bind_rows()'d 3,851 fragments together. Measured 158.3s -> 3.9s.
#
# Equivalence: the per-row select(where(~!all(is.na(.)))) only ever removed cells
# that pivot_longer would emit as NA, and those are dropped here by filter(!is.na)
# instead - so the surviving rows, their order and their values are identical.
# Verified: identical() TRUE on all 501,140 rows against the loop's output.
#
# One edge case, absent from this data: a respondent with EVERY "B" column NA
# produced one NA-valued row under the loop and produces none here. Row counts
# match exactly, so no such respondent exists.
final_df <- df_filtered %>%
  mutate(across(matches(".*B.*"), as.character)) %>%
  pivot_longer(
    cols      = contains("B"),
    names_to  = "name",
    values_to = "value"
  ) %>%
  filter(!is.na(value))

# remove image name coumns 

t <- final_df%>%
  filter(name != "SN_B43_7IMAGE_AREA",
         name != "LSN_B43_6IMAGE_AREA",
         name != "LSN_B43_5IMAGE_AREA",
         name != "LSN_B43_4IMAGE_AREA",
         name != "LSN_B43_3IMAGE_AREA",
         name != "LSN_B12_1IMAGE_CURRENT",
         name != "LSN_B12_1IMAGE_CURRENT",
         name != "LSN_B43_7IMAGE_AREA"
  )

# extract block number and choice number from the choice name we created (consistent format)

final_df <- final_df %>%
  mutate(
    block_number = str_extract(name, "(?<=^B)\\d+A?"),  # Capture number with optional 'A' after 'B'
    choice_number = sub("^[^_]*_([^_]+).*", "\\1", name),  # Extract text between the first and second underscore
    VAR_NAME = sub("^.*?_.*?_(.*)$", "\\1", name),  # Extract everything after the second underscore
    
  )

# Here I extract all choice specific variables into different subset 
# extract cost variable as data frame
df_cost <- final_df %>%
  filter(VAR_NAME == "COST") %>%
  select(CaseId,name,TREATMENT,spatial_1,spatial_2,NON_LOCAL,LOCAL_AR,value,block_number,choice_number, -VAR_NAME) %>% # Need to add CONDITION here as this is NA valuse for all it dropped need to clarify with Danny
  select(-name) %>%
  rename(Cost = value,
         BASIN = spatial_1,
         SUB_BASIN = spatial_2) %>%
  mutate(Cost = as.numeric(gsub("\\$", "", Cost)))%>%  # Remove $ and convert to numeric
  rename(COST=Cost)


# local watershed include home (1) or non-local watershed which does not include home (2)
df_vote <- final_df%>%
  filter(VAR_NAME=="VOTE")%>%
  select(CaseId,name, value,block_number,choice_number, -VAR_NAME) %>%
  select(-name)%>%
  rename(VOTE=value)%>%
  mutate(VOTE = as.numeric(VOTE))


# extract current WQ name as data frame
df_WQ_current <- final_df%>%
  filter(VAR_NAME=="CURRENT_AVERAGE")%>%
  select(CaseId,name, value,block_number,choice_number, -VAR_NAME) %>%
  select(-name)%>%
  rename(CURRENT_AVERAGE=value)%>%
  mutate(CURRENT_AVERAGE = as.numeric(CURRENT_AVERAGE))

# extract policy WQ name as data frame
df_WQ_policy <- final_df%>%
  filter(VAR_NAME=="POLICY_AVERAGE")%>%
  select(CaseId,name, value,block_number,choice_number, -VAR_NAME) %>%
  select(-name)%>%
  rename(POLICY_AVERAGE=value)%>%
  mutate(POLICY_AVERAGE = as.numeric(POLICY_AVERAGE))

# extract policy area as data frame
df_WQ_policy_areakm <- final_df%>%
  filter(VAR_NAME=="POLICY_SIZE_KM")%>%
  select(CaseId,name, value,block_number,choice_number, -VAR_NAME) %>%
  select(-name)%>%
  rename(POLICY_SIZE_KM=value)%>%
  mutate(
    POLICY_SIZE_KM = as.numeric(gsub("[^0-9\\.]", "", POLICY_SIZE_KM)) # Remove non-numeric characters
  )

# extract policy area % as data frame
df_WQ_policy_percent <- final_df%>%
  filter(VAR_NAME=="POLICY_SIZE_PERCENT")%>%
  select(CaseId,name, value,block_number,choice_number, -VAR_NAME) %>%
  select(-name)%>%
  rename(POLICY_SIZE_PERCENT = value)%>%
  mutate(POLICY_SIZE_PERCENT = as.numeric(gsub("%", "", POLICY_SIZE_PERCENT)) / 100) # Remove '%' and divide by 100

# extract # extract WQ = 5 as % as data frameWQ = 1 as % as data frame
df_WQ_current_1 <- final_df%>%
  filter(VAR_NAME=="CURRENT1")%>%
  select(CaseId,name, value,block_number,choice_number, -VAR_NAME) %>%
  select(-name)%>%
  rename(CURRENT_1=value)%>%
  mutate(CURRENT_1 = as.numeric(gsub("%", "", CURRENT_1)) / 100) # Remove '%' and divide by 100


# extract # extract WQ = 5 as % as data frameWQ = 2 as % as data frame
df_WQ_current_2 <- final_df%>%
  filter(VAR_NAME=="CURRENT2")%>%
  select(CaseId,name, value,block_number,choice_number, -VAR_NAME) %>%
  select(-name)%>%
  rename(CURRENT_2=value)%>%
  mutate(CURRENT_2 = as.numeric(gsub("%", "", CURRENT_2)) / 100) # Remove '%' and divide by 100


# extract # extract WQ = 5 as % as data frameWQ = 3 as % as data frame
df_WQ_current_3 <- final_df%>%
  filter(VAR_NAME=="CURRENT3")%>%
  select(CaseId,name, value,block_number,choice_number, -VAR_NAME) %>%
  select(-name)%>%
  rename(CURRENT_3=value)%>%
  mutate(CURRENT_3 = as.numeric(gsub("%", "", CURRENT_3)) / 100) # Remove '%' and divide by 100

# extract # extract WQ = 5 as % as data frameWQ = 4 as % as data frame
df_WQ_current_4 <- final_df%>%
  filter(VAR_NAME=="CURRENT4")%>%
  select(CaseId,name, value,block_number,choice_number, -VAR_NAME) %>%
  select(-name)%>%
  rename(CURRENT_4=value)%>%
  mutate(CURRENT_4 = as.numeric(gsub("%", "", CURRENT_4)) / 100) # Remove '%' and divide by 100


# extract current WQ = 5 as % as data frame
df_WQ_current_5 <- final_df%>%
  filter(VAR_NAME=="CURRENT5")%>%
  select(CaseId,name, value,block_number,choice_number, -VAR_NAME) %>%
  select(-name)%>%
  rename(CURRENT_5=value)%>%
  mutate(CURRENT_5 = as.numeric(gsub("%", "", CURRENT_5)) / 100) # Remove '%' and divide by 100


# extract image info-current as data frame
df_WQ_current_image<- final_df%>%
  filter(VAR_NAME=="IMAGECURRENT")%>%
  select(CaseId,name, value,block_number,choice_number, -VAR_NAME) %>%
  select(-name)%>%
  rename(IMAGECURRENT=value)

# extract image info-policy as data frame
df_WQ_policy_image<- final_df%>%
  filter(VAR_NAME=="IMAGEPOLICY")%>%
  select(CaseId,name, value,block_number,choice_number, -VAR_NAME) %>%
  select(-name)%>%
  rename(IMAGEPOLICY=value)


# extract WQ = 1 as % in policy data frame
df_WQ_policy_1 <- final_df%>%
  filter(VAR_NAME=="POLICY1")%>%
  select(CaseId,name, value,block_number,choice_number, -VAR_NAME) %>%
  select(-name)%>%
  rename(POLICY_1=value)%>%
  mutate(POLICY_1 = as.numeric(gsub("%", "", POLICY_1)) / 100) # Remove '%' and divide by 100

# extract WQ = 2 as % in policy data frame
df_WQ_policy_2 <- final_df%>%
  filter(VAR_NAME=="POLICY2")%>%
  select(CaseId,name, value,block_number,choice_number, -VAR_NAME) %>%
  select(-name)%>%
  rename(POLICY_2=value)%>%
  mutate(POLICY_2 = as.numeric(gsub("%", "", POLICY_2)) / 100) # Remove '%' and divide by 100


# extract WQ = 3 as % in policy data frame
df_WQ_policy_3 <- final_df%>%
  filter(VAR_NAME=="POLICY3")%>%
  select(CaseId,name, value,block_number,choice_number, -VAR_NAME) %>%
  select(-name)%>%
  rename(POLICY_3=value)%>%
  mutate(POLICY_3 = as.numeric(gsub("%", "", POLICY_3)) / 100) # Remove '%' and divide by 100


# extract WQ = 4 as % in policy data frame
df_WQ_policy_4 <- final_df%>%
  filter(VAR_NAME=="POLICY4")%>%
  select(CaseId,name, value,block_number,choice_number, -VAR_NAME) %>%
  select(-name)%>%
  rename(POLICY_4=value)%>%
  mutate(POLICY_4 = as.numeric(gsub("%", "", POLICY_4)) / 100) # Remove '%' and divide by 100


# extract WQ = 5 as % in policy data frame
df_WQ_policy_5 <- final_df%>%
  filter(VAR_NAME=="POLICY5")%>%
  select(CaseId,name, value,block_number,choice_number, -VAR_NAME) %>%
  select(-name)%>%
  rename(POLICY_5=value)%>%
  mutate(POLICY_5 = as.numeric(gsub("%", "", POLICY_5)) / 100) # Remove '%' and divide by 100

# Merge thos subset to represent combine dataframe
# Then left joint those variable based on respondent id, block id, river basin and other matching variables 


df_choice_all <- df_vote%>%
  left_join(df_cost)%>%
  left_join(df_WQ_current)%>%
  left_join(df_WQ_policy)%>%
  left_join(df_WQ_policy_areakm)%>%
  left_join(df_WQ_policy_percent)%>%
  #left_join(df_WQ_current_1)%>%
  #left_join(df_WQ_current_2)%>%
  #left_join(df_WQ_current_3)%>%
  #left_join(df_WQ_current_4)%>%
  #left_join(df_WQ_current_5)%>%
  left_join(df_WQ_current_image)%>%
  left_join(df_WQ_policy_image)%>%
  #left_join(df_WQ_policy_1)%>%
  #left_join(df_WQ_policy_2)%>%
  #left_join(df_WQ_policy_3)%>%
  #left_join(df_WQ_policy_4)%>%
  #left_join(df_WQ_policy_5)%>%
  mutate(VOTE1 = case_when(
    VOTE == "1" ~ 1, 
    VOTE == "2" ~ 0, 
    TRUE ~ as.numeric(VOTE)  # Convert VOTE to numeric for other cases
  ))%>%
  rename(
    BLK_NUMBER = block_number,
    CHOICE_NUMBER = choice_number
  )%>%
  mutate(BLOCK_NUMBER = str_extract_all(BLK_NUMBER, "\\d+"), # Danny use letter A fro some blocks so I remove it for consistent
         BLOCK_NUMBER = sapply(BLOCK_NUMBER, function(x) paste(x, collapse = "")))%>%
  mutate(ID = paste(BLOCK_NUMBER, CHOICE_NUMBER, sep = "_"))%>%
  select(-BLOCK_NUMBER) # drop ir and keep the BLK_NUMBER ONLT


# There are some information needs to add from the choice design excel sheet 
# i.e. respondents WQ at sub basin level (policy average is fro whole area, but need WQ at sub basin level)

# Define the mapping for basin and sub-basin codes
basin_mapping <- c("arb" = 1, "lsn" = 2, "nsb" = 3, "ssb" = 4)
sub_basin_mapping <- c(
  "as" = 1, "qa" = 2, "re" = 3, "so" = 4, "elw" = 5, "gb" = 6, "lwl" = 7, 
  "ne" = 8, "sa" = 9, "wlw" = 10, "ba" = 11, "cns" = 12, "lns" = 13, 
  "uns" = 14, "bw" = 15, "lss" = 16, "rd" = 17, "uss" = 18
)


ChoiceSetDesigns <- read_excel("data/raw/ChoiceSetDesigns.xlsx", sheet = "SummaryAll")%>%
  select(block,choice_number,policy_description,basin,sub_basin,image_current,image_policy,version)%>%
  mutate(CHOICE_SUB_BASIN = case_when(
    sub_basin == "all" ~ 0,
    sub_basin == "Assiniboine" ~ 1,
    sub_basin == "Qu'Appelle" ~ 2,
    sub_basin == "Red" ~ 3,
    sub_basin == "Souris" ~ 4,
    sub_basin == "East Lake Winnipeg" ~ 5,
    sub_basin == "Grass and Burntwood" ~ 6,
    sub_basin == "Lake Winnipegosis and Lake Manitoba" ~ 7,
    sub_basin == "Nelson" ~ 8,
    sub_basin == "Saskatchewan" ~ 9,
    sub_basin == "Western Lake Winnipeg" ~ 10,
    sub_basin == "Battle" ~ 11,
    sub_basin == "Central North Saskatchewan" ~ 12,
    sub_basin == "Lower North Saskatchewan" ~ 13,
    sub_basin == "Upper North Saskatchewan" ~ 14,
    sub_basin == "Bow" ~ 15,
    sub_basin == "Lower South Saskatchewan" ~ 16,
    sub_basin == "Red Deer"~ 17,
    sub_basin == "Upper South Saskatchewan"~ 18,
    TRUE ~ NA_real_
  ))%>%
  mutate(CHOICE_AREA = case_when(
    CHOICE_SUB_BASIN == 0 ~ "BASIN",          # Choice is based on full basin
    CHOICE_SUB_BASIN > 0 ~ "SUBBASIN",        # Choice is based on sub basin
    TRUE ~ NA_character_  
  ))%>%
  mutate(CHOICE_BASIN = case_when(            # Choice basin map with survey keys
    basin == "AR" ~ 1,
    basin == "LSN" ~ 2,
    basin == "NS" ~ 3,
    basin == "SS" ~ 4))%>%
  #mutate(TREATMENT = case_when(
  #treatment == "spatial" ~1,
  #treatment == "wq" ~ 2
  #))%>%
  mutate(wq_change = str_extract(policy_description, "^[^-]+"))%>%      # WQ change scenario
  mutate(wq_change = str_replace(wq_change, "Improve WQ up to 2 $", "Improve WQ up to 2"))%>%
  mutate(WQ_UP1 = case_when(wq_change == "Improve WQ up to 1 " ~ 1,TRUE ~ 0),
         WQ_UP2 = case_when(wq_change == "Improve WQ up to 2" ~ 1, TRUE ~0),
         WQ_UP3 = case_when(wq_change == "Improve WQ up to 3 " ~ 1, TRUE ~0),
         WQ_BY1 = case_when(wq_change == "Improve WQ by 1 " ~ 1, TRUE ~0)
  )%>%
  rename(BLOCK_NUMBER = block, 
         CHOICE_NUMBER = choice_number
  )%>%
  select(BLOCK_NUMBER,CHOICE_NUMBER,CHOICE_AREA,CHOICE_BASIN,
         CHOICE_SUB_BASIN, WQ_UP1,WQ_UP2,WQ_UP3,WQ_BY1,image_current,image_policy,version)%>%
  mutate(ID = paste(BLOCK_NUMBER, CHOICE_NUMBER, sep = "_"))%>% # consitent key with usrvey data and ChoiceDesign.xlsx
  #select( -BLOCK_NUMBER, -CHOICE_NUMBER)%>%
  distinct(ID, .keep_all = T)%>% # Some version repeat the same so need to get unique
  
  separate(                            # use image names to determine the WQ at sub basin - policy
    col = image_policy,
    into = c("basin", paste0("sub_policy_", 1:6)),
    sep = "_"
  ) %>%
  
  separate(                            # use image names to determine the WQ at sub basin - current
    col = image_current,
    into = c("basin_current", paste0("sub_current_", 1:6)),
    sep = "_"
  ) %>%
  mutate(
    CH_BASIN = basin_mapping[basin]   # map the basin keys
  ) %>%
  mutate(across(
    starts_with("sub"), 
    list(
      name = ~ sub_basin_mapping[str_extract(.x, "[a-zA-Z]+")],
      wq = ~ str_extract(.x, "[0-9]+")
    ),
    .names = "{.col}_{.fn}"
  ))%>%
  select(-BLOCK_NUMBER,-CHOICE_NUMBER)%>%
  rename(VERSION = version)



################################################################################
# Join df_all data frame with survey data
# make some variables by matching the locality of the choice and respondents local basin
df_all <- df_choice_all %>%
  left_join(ChoiceSetDesigns, by = c("ID")) %>%
  mutate(
    BASIN = as.character(BASIN),
    CHOICE_BASIN = as.character(CHOICE_BASIN),
    POLICY_AVERAGE = as.numeric(POLICY_AVERAGE)
  ) %>%
  mutate(CHOICE_LOCALITY_BASIN = case_when(          # indicator choice basin and local basin same
    BASIN == CHOICE_BASIN ~ "LOCAL",       
    BASIN != CHOICE_BASIN ~ "NONLOCAL",   
    TRUE ~ NA_character_                        
  )) %>%
  mutate(CHOICE_LOCALITY_SUBBASIN = case_when(      # indicator choice sub basin and local sub basin same
    SUB_BASIN == CHOICE_SUB_BASIN ~ "LOCAL",       
    SUB_BASIN != CHOICE_SUB_BASIN ~ "NONLOCAL",   
    TRUE ~ NA_character_                        
  )) %>%
  
  mutate(across(matches("^sub_current\\d+_wq$"), ~as.numeric(.))) %>%
  
  mutate(                                           # WQ level at sub-basin - this is use to determine the WQ at the home subwatershed
    WQ_HOME_CURRENT = if_else(                       # What is the WQ at home sub watershed when choice is with their local-sub watershed
      CHOICE_LOCALITY_BASIN == "LOCAL",             
      case_when(
        sub_current_1_name == SUB_BASIN ~ as.numeric(sub_current_1_wq),   # these will capture the choice scenario presented as full basin
        sub_current_2_name == SUB_BASIN ~ as.numeric(sub_current_2_wq),   # when we present it as full basin for their locality
        sub_current_3_name == SUB_BASIN ~ as.numeric(sub_current_3_wq),   # one of sub basin would be their local sub basin
        sub_current_4_name == SUB_BASIN ~ as.numeric(sub_current_4_wq),
        sub_current_5_name == SUB_BASIN ~ as.numeric(sub_current_5_wq),
        sub_current_6_name == SUB_BASIN ~ as.numeric(sub_current_6_wq),
        TRUE ~ 0  
      ),
      0  
    )
  ) %>%
  mutate(across(matches("^sub_policy\\d+_wq$"), ~as.numeric(.))) %>%
  
  mutate(
    WQ_HOME_POLICY = if_else(
      CHOICE_LOCALITY_BASIN == "LOCAL",
      case_when(
        sub_policy_1_name == SUB_BASIN ~ as.numeric(sub_policy_1_wq),
        sub_policy_2_name == SUB_BASIN ~ as.numeric(sub_policy_2_wq),
        sub_policy_3_name == SUB_BASIN ~ as.numeric(sub_policy_3_wq),
        sub_policy_4_name == SUB_BASIN ~ as.numeric(sub_policy_4_wq),
        sub_policy_5_name == SUB_BASIN ~ as.numeric(sub_policy_5_wq),
        sub_policy_6_name == SUB_BASIN ~ as.numeric(sub_policy_6_wq),
        TRUE ~ 0  # Ensure numeric output
      ),
      0  # Ensure numeric output
    )
  ) %>%
  
  
  mutate(
    WQ_LOCAL_CURRENT = if_else(CHOICE_LOCALITY_BASIN == "LOCAL",CURRENT_AVERAGE,0),
    WQ_NL_CURRENT = if_else(CHOICE_LOCALITY_BASIN == "NONLOCAL",CURRENT_AVERAGE,0),
    WQ_LOCAL_POLICY = if_else(CHOICE_LOCALITY_BASIN == "LOCAL",POLICY_AVERAGE,0),
    WQ_NL_POLICY = if_else(CHOICE_LOCALITY_BASIN == "NONLOCAL",POLICY_AVERAGE,0)
  )%>%
  
  mutate(
    WQ_HOME_CURRENT = if_else(WQ_HOME_CURRENT == WQ_HOME_POLICY, 0, WQ_HOME_CURRENT),
    WQ_HOME_POLICY = if_else(WQ_HOME_CURRENT == 0 & WQ_HOME_POLICY != 0, 0, WQ_HOME_POLICY)  
  )%>%
  
  mutate(
    WQ_SUBBASIN_LOCAL_CURRENT = if_else(CHOICE_AREA == "SUBBASIN" & CHOICE_LOCALITY_SUBBASIN == "LOCAL",CURRENT_AVERAGE,0),
    WQ_SUBBASIN_NL_CURRENT = if_else(CHOICE_AREA == "SUBBASIN" & CHOICE_LOCALITY_SUBBASIN == "NONLOCAL", CURRENT_AVERAGE,0),
    WQ_SUBBASIN_LOCAL_POLICY = if_else(CHOICE_AREA == "SUBBASIN" & CHOICE_LOCALITY_SUBBASIN == "LOCAL", POLICY_AVERAGE,0),
    WQ_SUBBASIN_NL_POLICY = if_else(CHOICE_AREA == "SUBBASIN" & CHOICE_LOCALITY_SUBBASIN == "NONLOCAL", POLICY_AVERAGE,0)
  )%>%
  
  mutate(
    WQ_BASIN_LOCAL_CURRENT = if_else(CHOICE_AREA == "BASIN" & CHOICE_LOCALITY_BASIN == "LOCAL",CURRENT_AVERAGE,0),
    WQ_BASIN_NL_CURRENT = if_else(CHOICE_AREA == "BASIN" & CHOICE_LOCALITY_BASIN == "NONLOCAL", CURRENT_AVERAGE,0),
    WQ_BASIN_LOCAL_POLICY = if_else(CHOICE_AREA == "BASIN" & CHOICE_LOCALITY_BASIN == "LOCAL", POLICY_AVERAGE,0),
    WQ_BASIN_NL_POLICY = if_else(CHOICE_AREA == "BASIN" & CHOICE_LOCALITY_BASIN == "NONLOCAL", POLICY_AVERAGE,0)
  )%>%
  select(CaseId,TREATMENT,VERSION,BLK_NUMBER,CHOICE_NUMBER,BASIN,SUB_BASIN,NON_LOCAL,
         IMAGECURRENT,IMAGEPOLICY,CURRENT_AVERAGE,POLICY_AVERAGE,
         CHOICE_AREA,CHOICE_BASIN,CHOICE_SUB_BASIN,CHOICE_LOCALITY_BASIN,CHOICE_LOCALITY_SUBBASIN,CHOICE_LOCALITY_BASIN,
         POLICY_SIZE_KM,POLICY_SIZE_PERCENT,WQ_UP1,WQ_UP2,WQ_UP3,WQ_BY1,
         WQ_LOCAL_CURRENT,WQ_NL_CURRENT,WQ_LOCAL_POLICY,WQ_NL_POLICY,
         WQ_HOME_CURRENT,WQ_HOME_POLICY,
         WQ_SUBBASIN_LOCAL_CURRENT,WQ_SUBBASIN_NL_CURRENT,WQ_SUBBASIN_LOCAL_POLICY,WQ_SUBBASIN_NL_POLICY,
         WQ_BASIN_LOCAL_CURRENT,WQ_BASIN_NL_CURRENT,WQ_BASIN_LOCAL_POLICY,WQ_BASIN_NL_POLICY,
         COST,VOTE1,VOTE) # CONDITION need to add this as this dataset need to calrify with Danny

##############################################################################################################
# In second we need to add the respondent specific data collected such as age, and data on all othe questions
# Open the data frame that create respondent characteristics (responses to non-valuation questions)
##############################################################################################################

df_RespondsUnique <- read.csv("data/derived/test/respondent_characteristics.csv")%>%
  #rename(CaseId = CaseID)%>%
  distinct(CaseId, .keep_all = T)%>%
  select(-VERSION)

df_RespondsUnique$CaseId <- as.character(df_RespondsUnique$CaseId)
#df_RespondsUnique$SUB_BASIN <- as.character(df_RespondsUnique$SUB_BASIN)
df_RespondsUnique$BASIN <- as.character(df_RespondsUnique$BASIN)
df_RespondsUnique$TREATMENT <- as.character(df_RespondsUnique$TREATMENT)
#df_RespondsUnique$VERSION <- as.character(df_RespondsUnique$VERSION)
df_RespondsUnique$SUB_BASIN <- as.character(df_RespondsUnique$SUB_BASIN)
df_RespondsUnique$NON_LOCAL <- as.character(df_RespondsUnique$NON_LOCAL)





# Merge them with choice related questions
df_final <- df_all%>%
  select(-VOTE)%>%
  rename(VOTE=VOTE1)%>%
  left_join(df_RespondsUnique)





################################################################################
# For the sub-basin level changes we use basin level average, but we may need to 
# use sub basin level changes instead of basin level so here we add WQ level 
# for current and policy for choices with sub-basin level
# added ob May 6 and if this is process ahead can incorporate those changes

df_final <- df_final %>%
  mutate(
    wq_sub_basin_current = ifelse(CHOICE_AREA == "SUBBASIN", as.numeric(gsub("[^0-9]", "", IMAGECURRENT)), 0),
    wq_sub_basin_policy = ifelse(CHOICE_AREA == "SUBBASIN", as.numeric(gsub("[^0-9]", "", IMAGEPOLICY)), 0)
  ) %>%
  
  mutate(
    WQ_SUBBASIN_LOCAL_CURRENT_SUBONLY = if_else(CHOICE_AREA == "SUBBASIN" & CHOICE_LOCALITY_SUBBASIN == "LOCAL",wq_sub_basin_current,0),
    WQ_SUBBASIN_NL_CURRENT_SUBONLY = if_else(CHOICE_AREA == "SUBBASIN" & CHOICE_LOCALITY_SUBBASIN == "NONLOCAL", wq_sub_basin_current,0),
    WQ_SUBBASIN_LOCAL_POLICY_SUBONLY = if_else(CHOICE_AREA == "SUBBASIN" & CHOICE_LOCALITY_SUBBASIN == "LOCAL", wq_sub_basin_policy,0),
    WQ_SUBBASIN_NL_POLICY_SUBONLY = if_else(CHOICE_AREA == "SUBBASIN" & CHOICE_LOCALITY_SUBBASIN == "NONLOCAL", wq_sub_basin_policy,0)
  )


################################################################################
# To address the distance decaying effects, we add few more variables based on the
# provinces and whether sub basins are share across the province


# Read shapefiles
river_basins <- st_read("data/gis/study_area.shp")%>%
  mutate(sub_basin = case_when(
    WSCSDA_E == "Qu'Appelle" ~ "Qu'Appelle", 
    WSCSDA_E == "Assiniboine" ~ "Assiniboine", 
    WSCSDA_E == "Souris" ~ "Souris", 
    WSCSDA_E == "Red" ~ "Red", 
    
    WSCSDA_E == "Grass and Burntwood River Basin" ~ "Grass and Burntwood", 
    WSCSDA_E == "Nelson River Basin" ~ "Nelson", 
    WSCSDA_E == "Saskatchewan River Basin" ~ "Saskatchewan", 
    WSCSDA_E == "Eastern Lake Winnipeg River Basin" ~ "Eastern Lake Winnipeg", 
    WSCSDA_E == "Lake Winnipegosis and Lake Manitoba River Basin" ~ "Lake Winnipegosis and Lake Manitoba", 
    WSCSDA_E == "Western Lake Winnipeg River Basin" ~ "Western Lake Winnipeg", 
    
    WSCSDA_E == "Central North Saskatchewan Sub River Basin" ~ "Central North Saskatchewan", 
    WSCSDA_E == "Upper North Saskatchewan Sub River Basin" ~ "Upper North Saskatchewan", 
    WSCSDA_E == "Battle Sub River Basin" ~ "Battle", 
    WSCSDA_E == "Lower North Saskatchewan Sub River Basin" ~ "Lower North Saskatchewan", 
    
    WSCSDA_E == "Bow Sub River Basin" ~ "Bow", 
    WSCSDA_E == "Red Deer Sub River Basin" ~ "Red Deer", 
    WSCSDA_E == "Lower South Saskatchewan Sub River Basin" ~ "Lower South Saskatchewan", 
    WSCSDA_E == "Upper South Saskatchewan Sub River Basin" ~ "Upper South Saskatchewan", 
    
    TRUE ~ NA_character_  # Otherwise, assign 0
  ))
ab <- st_read("data/gis/AB.shp")
mb <- st_read("data/gis/MB.shp")
sk <- st_read("data/gis/SK.shp")

# Combine all states into one object
prov<- rbind(ab, mb, sk)

# Make geometries valid
river_basins <- st_make_valid(river_basins)
prov <- st_make_valid(prov)

prov <- st_transform(prov, st_crs(river_basins))

# Perform spatial intersection
intersections <- st_intersection(river_basins, prov)

# Calculate area in km²
intersections <- intersections %>%
  mutate(area_km2 = as.numeric(st_area(.)) / 1e6)

# Summarize: area of each river basin within each state

summary_df <- intersections %>%
  st_drop_geometry() %>%          # Remove geometry column
  group_by(sub_basin, PRNAME) %>% # Group by basin and province
  summarise(
    area_km2 = sum(area_km2),     # Calculate sum of areas
    .groups = "drop"              # Drop grouping structure
  )  

# OPTIONAL: Spread into wide format (one row per basin, with area in each state)
df <- tidyr::pivot_wider(summary_df,
                         names_from = PRNAME,
                         values_from = area_km2,
                         values_fill = 0)%>%
  mutate(CHOICE_SUB_BASIN = case_when(
    sub_basin == "all" ~ 0,
    sub_basin == "Assiniboine" ~ 1,
    sub_basin == "Qu'Appelle" ~ 2,
    sub_basin == "Red" ~ 3,
    sub_basin == "Souris" ~ 4,
    sub_basin == "Eastern Lake Winnipeg" ~ 5,
    sub_basin == "Grass and Burntwood" ~ 6,
    sub_basin == "Lake Winnipegosis and Lake Manitoba" ~ 7,
    sub_basin == "Nelson" ~ 8,
    sub_basin == "Saskatchewan" ~ 9,
    sub_basin == "Western Lake Winnipeg" ~ 10,
    sub_basin == "Battle" ~ 11,
    sub_basin == "Central North Saskatchewan" ~ 12,
    sub_basin == "Lower North Saskatchewan" ~ 13,
    sub_basin == "Upper North Saskatchewan" ~ 14,
    sub_basin == "Bow" ~ 15,
    sub_basin == "Lower South Saskatchewan" ~ 16,
    sub_basin == "Red Deer"~ 17,
    sub_basin == "Upper South Saskatchewan"~ 18,
    TRUE ~ NA_real_
  ))%>%
  select(sub_basin,CHOICE_SUB_BASIN,Manitoba,Saskatchewan,Alberta)

# calculate area % withineach province
df<- df%>%
  rowwise() %>%
  mutate(
    total_area = sum(c_across(c(Manitoba, Saskatchewan, Alberta))),
    PERC_MB = ceiling(10000 * Manitoba / total_area) / 100,
    PERC_SK = ceiling(10000 * Saskatchewan / total_area) / 100,
    PERC_AB = ceiling(10000 * Alberta / total_area) / 100
  ) %>%
  ungroup()%>%
  rowwise() %>%
  mutate(SHARED_BOADER = ifelse(sum(c_across(c(PERC_MB, PERC_SK, PERC_AB)) > 0) > 1,
                                0, 1)) %>%
  ungroup()%>%
  select(sub_basin,CHOICE_SUB_BASIN,SHARED_BOADER,PERC_MB,PERC_SK,PERC_AB)

df_final <- df_final%>%
  left_join(df)%>%
  
  mutate(WQ_SUBBASIN_LOCAL_SB_CURRENT =  if_else(CHOICE_AREA == "SUBBASIN" & CHOICE_LOCALITY_SUBBASIN == "LOCAL" & 
                                                   SHARED_BOADER == 0 ,wq_sub_basin_current,0))%>%
  mutate(WQ_SUBBASIN_LOCAL_NSB_CURRENT =  if_else(CHOICE_AREA == "SUBBASIN" & CHOICE_LOCALITY_SUBBASIN == "LOCAL" & 
                                                    SHARED_BOADER == 1 ,wq_sub_basin_current,0))%>%
  
  
  mutate(WQ_SUBBASIN_LOCAL_SB_POLICY =  if_else(CHOICE_AREA == "SUBBASIN" & CHOICE_LOCALITY_SUBBASIN == "LOCAL" & 
                                                  SHARED_BOADER == 0 ,wq_sub_basin_policy,0))%>%
  mutate(WQ_SUBBASIN_LOCAL_NSB_POLICY =  if_else(CHOICE_AREA == "SUBBASIN" & CHOICE_LOCALITY_SUBBASIN == "LOCAL" & 
                                                   SHARED_BOADER == 1 ,wq_sub_basin_policy,0))%>%
  
  
  mutate(WQ_SUBBASIN_NONLOCAL_SB_CURRENT =  if_else(CHOICE_AREA == "SUBBASIN" & CHOICE_LOCALITY_SUBBASIN == "NONLOCAL" & 
                                                      SHARED_BOADER == 0 ,wq_sub_basin_current,0))%>%
  mutate(WQ_SUBBASIN_NONLOCAL_NSB_CURRENT =  if_else(CHOICE_AREA == "SUBBASIN" & CHOICE_LOCALITY_SUBBASIN == "NONLOCAL" & 
                                                       SHARED_BOADER == 1 ,wq_sub_basin_current,0))%>%
  
  
  mutate(WQ_SUBBASIN_NONLOCAL_SB_POLICY =  if_else(CHOICE_AREA == "SUBBASIN" & CHOICE_LOCALITY_SUBBASIN == "NONLOCAL" & 
                                                     SHARED_BOADER == 0 ,wq_sub_basin_policy,0))%>%
  mutate(WQ_SUBBASIN_NONLOCAL_NSB_POLICY =  if_else(CHOICE_AREA == "SUBBASIN" & CHOICE_LOCALITY_SUBBASIN == "NONLOCAL" & 
                                                      SHARED_BOADER == 1 ,wq_sub_basin_policy,0))%>%
  
  
  mutate(WQ_SUBBASIN_LOCAL_SB_LP_CURRENT = if_else(CHOICE_AREA == "SUBBASIN" &  CHOICE_LOCALITY_SUBBASIN == "LOCAL" & SHARED_BOADER == 0 & 
                                                     ((PROVINCE == 1 & PERC_AB > 0) | (PROVINCE == 3 & PERC_MB > 0) | (PROVINCE == 12 & PERC_SK > 0)),
                                                   CURRENT_AVERAGE,0 ))%>%
  
  mutate(WQ_SUBBASIN_LOCAL_NSB_LP_CURRENT = if_else(CHOICE_AREA == "SUBBASIN" &  CHOICE_LOCALITY_SUBBASIN == "LOCAL" & SHARED_BOADER == 1 & 
                                                      ((PROVINCE == 1 & PERC_AB > 0) | (PROVINCE == 3 & PERC_MB > 0) | (PROVINCE == 12 & PERC_SK > 0)),
                                                    CURRENT_AVERAGE,0 ))%>%
  
  mutate(WQ_SUBBASIN_LOCAL_SB_LP_POLICY = if_else(CHOICE_AREA == "SUBBASIN" &  CHOICE_LOCALITY_SUBBASIN == "LOCAL" & SHARED_BOADER == 0 & 
                                                    ((PROVINCE == 1 & PERC_AB > 0) | (PROVINCE == 3 & PERC_MB > 0) | (PROVINCE == 12 & PERC_SK > 0)),
                                                  POLICY_AVERAGE,0 ))%>%
  
  mutate(WQ_SUBBASIN_LOCAL_NSB_LP_POLICY = if_else(CHOICE_AREA == "SUBBASIN" &  CHOICE_LOCALITY_SUBBASIN == "LOCAL" & SHARED_BOADER == 1 & 
                                                     ((PROVINCE == 1 & PERC_AB > 0) | (PROVINCE == 3 & PERC_MB > 0) | (PROVINCE == 12 & PERC_SK > 0)),
                                                   POLICY_AVERAGE,0 ))







################################################################################
# Add area as % within their home province at given level of spatial schale of choice 

basin <- read.csv("data/derived/basin_level_shares_province.csv")%>%
  mutate(CHOICE_BASIN = as.character(CHOICE_BASIN))%>%
  select(-PROVINCE)%>%
  group_by(CHOICE_BASIN)%>%
  summarise(
    AB_SHARE = sum(AB_SHARE, na.rm = TRUE),
    MB_SHARE = sum(MB_SHARE, na.rm = TRUE),
    SK_SHARE = sum(SK_SHARE, na.rm = TRUE),
    .groups = 'drop'  # Optional: removes grouping in the result
  )




sub_basin <- read.csv("data/derived/sub_basin_level_shares_province.csv")%>%
  select(-PROVINCE)%>%
  group_by(CHOICE_SUB_BASIN)%>%
  summarise(
    AB_SHARE = sum(AB_SHARE, na.rm = TRUE),
    MB_SHARE = sum(MB_SHARE, na.rm = TRUE),
    SK_SHARE = sum(SK_SHARE, na.rm = TRUE),
    .groups = 'drop'  # Optional: removes grouping in the result
  )


df_temp1 <- df_final%>%
  filter(CHOICE_AREA == "BASIN")%>%
  left_join(basin, by = c("CHOICE_BASIN"))

df_temp2 <- df_final%>%
  filter(CHOICE_AREA == "SUBBASIN")%>%
  left_join(sub_basin, by = c("CHOICE_SUB_BASIN"))


df_final <- rbind(df_temp1, df_temp2) %>%
  arrange(CaseId) %>%
  mutate(HOME_PROV_SHARE = case_when(
    PROVINCE == 1 ~ AB_SHARE,
    PROVINCE == 3 ~ MB_SHARE,
    PROVINCE == 12 ~ SK_SHARE,
    TRUE ~ 0
  ))%>%
  mutate(HOME_PROV_SHARE = if_else(is.na(HOME_PROV_SHARE), 0, HOME_PROV_SHARE))%>%
  mutate(HOME_PROV_SHARE = HOME_PROV_SHARE/100)%>%
  
  mutate(AREA_INSTATE_LOCAL = ifelse(CHOICE_AREA == "BASIN" & CHOICE_LOCALITY_BASIN == "LOCAL", HOME_PROV_SHARE, 0)) %>%
  mutate(AREA_INSTATE_LOCAL = ifelse(CHOICE_AREA == "SUBBASIN" & CHOICE_LOCALITY_SUBBASIN == "LOCAL", HOME_PROV_SHARE, AREA_INSTATE_LOCAL))%>%
  
  mutate(AREA_INSTATE_NONLOCAL = ifelse(CHOICE_AREA == "BASIN" & CHOICE_LOCALITY_BASIN == "NONLOCAL", HOME_PROV_SHARE, 0)) %>%
  mutate(AREA_INSTATE_NONLOCAL = ifelse(CHOICE_AREA == "SUBBASIN" & CHOICE_LOCALITY_SUBBASIN == "NONLOCAL", HOME_PROV_SHARE, AREA_INSTATE_NONLOCAL))%>%
  
  mutate(AREA_INSTATE_LOCAL_BASIN = ifelse(CHOICE_AREA == "BASIN" & CHOICE_LOCALITY_BASIN == "LOCAL", HOME_PROV_SHARE, 0),
         AREA_INSTATE_NL_BASIN = if_else(CHOICE_AREA == "BASIN" & CHOICE_LOCALITY_BASIN == "NONLOCAL", HOME_PROV_SHARE, 0),
         
         AREA_INSTATE_LOCAL_SUBBASIN = ifelse(CHOICE_AREA == "SUBBASIN" & CHOICE_LOCALITY_SUBBASIN == "LOCAL", HOME_PROV_SHARE, 0),
         AREA_INSTATE_NL_SUBBASIN = if_else(CHOICE_AREA == "SUBBASIN" & CHOICE_LOCALITY_SUBBASIN == "NONLOCAL", HOME_PROV_SHARE, 0),
  )


###############################################################################
# Dummy variable for shared boundary across

df_final <- df_final %>%
  mutate(SHARED_BOADER_PROV = case_when(
    CHOICE_AREA == "BASIN" ~ 1,
    SHARED_BOADER == 0 ~ 1,
    TRUE ~ 0
  ))


###############################################################################
# Rec trip during last two years - binary variables

df_final <- df_final %>%
  mutate(REC_TRIP = case_when(
    Q21_REC_TRIP_TWO_YEARS == 1 ~ 1,
    Q21_REC_TRIP_TWO_YEARS == 2 ~ 0,
    is.na(Q21_REC_TRIP_TWO_YEARS) ~ NA_real_
  ))

###############################################################################

# create dummy variable to local adjacent choices 

df_map <- st_read("data/gis/study_area_map_with_WQ.shp")

library(spdep)

# sub basin level
# Convert to spatial object for neighbor analysis
nb <- poly2nb(df_map)

# Get the sub-basin names
sub_basin_names <- df_map$name_code

# Create a data frame with neighbor names
neighbors_df <- data.frame(
  Sub_Basin = sub_basin_names,
  Neighbors = sapply(nb, function(x) paste(sub_basin_names[x], collapse = ", "))
)%>%
  mutate(Sub_Basin = case_when(
    Sub_Basin == "AS" ~ 1,
    Sub_Basin == "QU" ~ 2,
    Sub_Basin == "RE" ~ 3,
    Sub_Basin == "SO" ~ 4,
    Sub_Basin == "ELW" ~ 5,
    Sub_Basin == "GB" ~ 6,
    Sub_Basin == "LWM" ~ 7,
    Sub_Basin == "NE" ~ 8,
    Sub_Basin == "SA" ~ 9,
    Sub_Basin == "WLW" ~ 10,
    Sub_Basin == "BA" ~ 11,
    Sub_Basin == "CNS" ~ 12,
    Sub_Basin == "LNS" ~ 13,
    Sub_Basin == "UNS" ~ 14,
    Sub_Basin == "BO" ~ 15,
    Sub_Basin == "LSS" ~ 16,
    Sub_Basin == "RD"~ 17,
    Sub_Basin == "USS"~ 18,
    TRUE ~ NA_real_
  ))

code_map <- c("AS" = 1L,"QU" = 2L,"RE" = 3L,"SO" = 4L,"ELW" = 5L,"GB" = 6L,"LWM" = 7L,"NE" = 8L,
              "SA" = 9L,"WLW" = 10L,"BA" = 11L,"CNS" = 12L,"LNS" = 13L,"UNS" = 14L,"BO" = 15L,
              "LSS" = 16L,"RD"= 17L,"USS"= 18L)


# Apply transformation
neighbors_df  <- neighbors_df  %>%
  rowwise() %>%
  mutate(code_num = list(
    str_split(Neighbors, ",\\s*")[[1]] %>% 
      map_chr(~ code_map[.x]) %>% 
      paste(collapse = ", ")
  )) %>%
  ungroup()

df_subbasin <- neighbors_df%>%
  rename(CHOICE_SUB_BASIN=Sub_Basin,
         NEIGHBOR = code_num)%>%
  select(-Neighbors)



# Basin level

df_map_basin <- df_map %>%
  group_by(basin) %>%
  summarise(geometry = st_union(geometry)) %>%
  ungroup()


# Ensure the resulting geometries are valid again
df_map_basin <- st_make_valid(df_map_basin)


# Make sure you're using the same object to get both Basin_names and nb
Basin_names <- df_map_basin$basin  # not df_map$basin

# Create neighbor list
nb <- poly2nb(df_map_basin)

# Create the neighbor dataframe using the same names
neighbors_df <- data.frame(
  Basin = Basin_names,
  Neighbors = sapply(nb, function(x) paste(Basin_names[x], collapse = ", "))
)%>%
  mutate(Basin = case_when(
    Basin == "AR" ~ 1,
    Basin == "LSN" ~ 2,
    Basin == "NS" ~ 3,
    Basin == "SS" ~ 4,
    TRUE ~ NA_real_
  ))

code_map <- c("AR" = 1L,"LSN" = 2L,"NS" = 3L,"SS" = 4L)


# Apply transformation
neighbors_df  <- neighbors_df  %>%
  rowwise() %>%
  mutate(code_num = list(
    str_split(Neighbors, ",\\s*")[[1]] %>% 
      map_chr(~ code_map[.x]) %>% 
      paste(collapse = ", ")
  )) %>%
  ungroup()

df_basin <- neighbors_df%>%
  rename(CHOICE_BASIN=Basin,
         NEIGHBOR = code_num)%>%
  select(-Neighbors)%>%
  mutate(CHOICE_BASIN = as.character(CHOICE_BASIN))



# sub basin
df_temp_subbasin <- df_final%>%
  #select(CaseId,SUB_BASIN,CHOICE_AREA,CHOICE_SUB_BASIN)%>%
  filter(CHOICE_AREA == "SUBBASIN")%>%
  left_join(df_subbasin )%>%
  rowwise() %>%
  mutate(
    LOCAL_ADJUCENT = as.integer(
      SUB_BASIN %in% str_split(NEIGHBOR, ",\\s*")[[1]]
    )
  ) %>%
  ungroup()


# basin
df_temp_basin <- df_final%>%
  #select(CaseId,BASIN,CHOICE_AREA,CHOICE_BASIN)%>%
  filter(CHOICE_AREA == "BASIN")%>%
  left_join(df_basin)%>%
  rowwise() %>%
  mutate(
    LOCAL_ADJUCENT = as.integer(
      BASIN %in% str_split(NEIGHBOR, ",\\s*")[[1]]
    )
  ) %>%
  ungroup()




df_final <- rbind(df_temp_subbasin,df_temp_basin)%>%
  arrange(CaseId)


df_final <- df_final%>%
  mutate(NON_LOCAL_ADJUCENT_LOCAL_BASIN = ifelse(CHOICE_AREA == "BASIN" & CHOICE_LOCALITY_BASIN == "NONLOCAL" & 
                                                   LOCAL_ADJUCENT == 1, 1,0 ))%>%
  mutate(NON_LOCAL_ADJUCENT_LOCAL_SUBBASIN = ifelse(CHOICE_AREA == "SUBBASIN" & CHOICE_LOCALITY_SUBBASIN == "NONLOCAL" & 
                                                      LOCAL_ADJUCENT == 1, 1,0 ))%>%
  mutate(NON_LOCAL_NOT_ADJUCENT_LOCAL_BASIN = ifelse(CHOICE_AREA == "BASIN" & CHOICE_LOCALITY_BASIN == "NONLOCAL" &
                                                       LOCAL_ADJUCENT == 0, 1,0))%>%
  mutate(NON_LOCAL_NOT_ADJUCENT_LOCAL_SUBBASIN = ifelse(CHOICE_AREA == "SUBBASIN" & CHOICE_LOCALITY_SUBBASIN == "NONLOCAL" & 
                                                          LOCAL_ADJUCENT == 0, 1,0 ))



df_final <- df_final%>%
  mutate(WQ_NON_LOCAL_ADJUCENT_LOCAL_BASIN_CURRENT = ifelse(NON_LOCAL_ADJUCENT_LOCAL_BASIN ==1,CURRENT_AVERAGE,0),
         WQ_NON_LOCAL_ADJUCENT_LOCAL_SUBBASIN_CURRENT = ifelse(NON_LOCAL_ADJUCENT_LOCAL_SUBBASIN ==1,wq_sub_basin_current,0),
         WQ_NON_LOCAL_ADJUCENT_LOCAL_BASIN_POLICY = ifelse(NON_LOCAL_ADJUCENT_LOCAL_BASIN ==1,POLICY_AVERAGE,0),
         WQ_NON_LOCAL_ADJUCENT_LOCAL_SUBBASIN_POLCIY = ifelse(NON_LOCAL_ADJUCENT_LOCAL_SUBBASIN ==1,wq_sub_basin_policy,0),
         
         WQ_NON_LOCAL_NOT_ADJUCENT_LOCAL_BASIN_CURRENT = ifelse(NON_LOCAL_NOT_ADJUCENT_LOCAL_BASIN ==1,CURRENT_AVERAGE,0),
         WQ_NON_LOCAL_NOT_ADJUCENT_LOCAL_SUBBASIN_CURRENT = ifelse(NON_LOCAL_NOT_ADJUCENT_LOCAL_SUBBASIN ==1,wq_sub_basin_current,0),
         WQ_NON_LOCAL_NOT_ADJUCENT_LOCAL_BASIN_POLICY = ifelse(NON_LOCAL_NOT_ADJUCENT_LOCAL_BASIN ==1,POLICY_AVERAGE,0),
         WQ_NON_LOCAL_NOT_ADJUCENT_LOCAL_SUBBASIN_POLCIY = ifelse(NON_LOCAL_NOT_ADJUCENT_LOCAL_SUBBASIN ==1,wq_sub_basin_policy,0),
         
         
  )




##############################################################################

# Add rec trip to choice basin data (created at separate code)

df_rec_trips <- read.csv("data/derived/RecTrip_basininfo.csv") %>%
  mutate(CaseId = as.character(CaseId))


df_temp_basin <- df_final%>%
  filter(CHOICE_AREA == "BASIN")%>%
  left_join(df_rec_trips)%>%
  rowwise() %>%
  mutate(
    REC_IN_CHOICE_BASIN = any(
      str_split(REC_BASIN_TRIP1, ",\\s*")[[1]] == CHOICE_BASIN,
      str_split(REC_BASIN_TRIP2, ",\\s*")[[1]] == CHOICE_BASIN,
      str_split(REC_BASIN_TRIP3, ",\\s*")[[1]] == CHOICE_BASIN,
      str_split(REC_BASIN_TRIP4, ",\\s*")[[1]] == CHOICE_BASIN,
      str_split(REC_BASIN_TRIP5, ",\\s*")[[1]] == CHOICE_BASIN
    ) %>% as.integer()
  ) %>%
  ungroup()

df_temp_subbasin <- df_final%>%
  filter(CHOICE_AREA == "SUBBASIN")%>%
  left_join(df_rec_trips)%>%
  rowwise() %>%
  mutate(
    REC_IN_CHOICE_BASIN = any(
      str_split(REC_SUBBASIN_TRIP1, ",\\s*")[[1]] == CHOICE_SUB_BASIN,
      str_split(REC_SUBBASIN_TRIP2, ",\\s*")[[1]] == CHOICE_SUB_BASIN,
      str_split(REC_SUBBASIN_TRIP3, ",\\s*")[[1]] == CHOICE_SUB_BASIN,
      str_split(REC_SUBBASIN_TRIP4, ",\\s*")[[1]] == CHOICE_SUB_BASIN,
      str_split(REC_SUBBASIN_TRIP5, ",\\s*")[[1]] == CHOICE_SUB_BASIN
    ) %>% as.integer()
  ) %>%
  ungroup()

df_final <- rbind(df_temp_basin,df_temp_subbasin)%>%
  mutate(REC_IN_CHOICE_BASIN = if_else(is.na(REC_IN_CHOICE_BASIN), 0, REC_IN_CHOICE_BASIN))%>%
  arrange(CaseId)

###############################################################################

ChoiceSetDesigns <- read_excel("data/raw/ChoiceSetDesigns.xlsx", sheet = "SummaryAll")%>%
  dplyr::select(version,block)%>%
  rename(BLK_NUMBER = block )%>%
  mutate(BLK_NUMBER = as.character(BLK_NUMBER))%>%
  distinct(BLK_NUMBER, .keep_all = T)

df_final <- df_final%>%
  mutate(BLK_NUMBER = str_extract(BLK_NUMBER, "\\d+"))%>%
  left_join(ChoiceSetDesigns, by = c("BLK_NUMBER"))

###############################################################################
# create dummy variable for version 1 and version 2

df_final <- df_final %>%
  mutate(
    SURVEY_VERSION_1 = ifelse(VERSION %in% c(1, 2), 1, 0),
    SURVEY_VERSION_2 = ifelse(VERSION %in% c(3, 4), 1, 0)
  )%>%
  mutate(
    VERSION_1 = ifelse(version == 1, 1, 0),
    VERSION_2 = ifelse(version == 2, 1, 0),
    VERSION_3 = ifelse(version == 3, 1, 0),
    VERSION_4 = ifelse(version == 4, 1, 0)
  )

###############################################################################
# Create variable to determine the how much WQ level has change (to use in version effect assessment)
# create interaction term of baseline_WQ*change WQ

df_final <- df_final%>%
  mutate(WQ_CHANGE = ifelse(CHOICE_AREA == "BASIN", CURRENT_AVERAGE - POLICY_AVERAGE, NA)) %>%
  mutate(WQ_CHANGE = ifelse(CHOICE_AREA == "SUBBASIN", wq_sub_basin_current - wq_sub_basin_policy, WQ_CHANGE))%>%
  
  mutate(WQ_CHANGE_BASIN = ifelse(CHOICE_AREA == "BASIN", CURRENT_AVERAGE - POLICY_AVERAGE, 0)) %>%
  mutate(WQ_CHANGE_SUBBASIN = ifelse(CHOICE_AREA == "SUBBASIN", wq_sub_basin_current - wq_sub_basin_policy, 0))%>%
  
  mutate(BASELINE_X_WQCHANGE_LOCAL_BASIN = ifelse(CHOICE_AREA == "BASIN" & CHOICE_LOCALITY_BASIN == "LOCAL",WQ_CHANGE*CURRENT_AVERAGE,0))%>%
  mutate(BASELINE_X_WQCHANGE_NL_BASIN = ifelse(CHOICE_AREA == "BASIN" & CHOICE_LOCALITY_BASIN == "NONLOCAL",WQ_CHANGE*CURRENT_AVERAGE,0))%>%
  
  mutate(BASELINE_X_WQCHANGE_LOCAL_SUBBASIN = ifelse(CHOICE_AREA == "SUBBASIN" & CHOICE_LOCALITY_SUBBASIN == "LOCAL",WQ_CHANGE*wq_sub_basin_current,0))%>%
  mutate(BASELINE_X_WQCHANGE_NL_SUBBASIN = ifelse(CHOICE_AREA == "SUBBASIN" & CHOICE_LOCALITY_SUBBASIN == "NONLOCAL",WQ_CHANGE*wq_sub_basin_current,0))%>%
  
  mutate(BASELINE_X_WQCHANGE = ifelse(CHOICE_AREA == "BASIN", WQ_CHANGE*CURRENT_AVERAGE, NA))%>%
  mutate(BASELINE_X_WQCHANGE = ifelse(CHOICE_AREA == "SUBBASIN", WQ_CHANGE*wq_sub_basin_current,BASELINE_X_WQCHANGE ))




################################################################################
# create WQ variable that account basin level average for basin level choice and sub basin level value for sub basin level choice
# This aggregate column use in Model 1 (where did not discriminate spatial boundaries)

df_final <- df_final%>%
  mutate(WQ_CURRENT_CHOICE = ifelse(CHOICE_AREA == "BASIN",CURRENT_AVERAGE,NA))%>%
  mutate(WQ_CURRENT_CHOICE = ifelse(CHOICE_AREA == "SUBBASIN",wq_sub_basin_current,WQ_CURRENT_CHOICE))%>%
  
  mutate(WQ_POLICY_CHOICE = ifelse(CHOICE_AREA == "BASIN",POLICY_AVERAGE,NA))%>%
  mutate(WQ_POLICY_CHOICE = ifelse(CHOICE_AREA == "SUBBASIN",wq_sub_basin_policy,WQ_POLICY_CHOICE))


# WQ at different version - here we. measure how much unit at different versions are vary depend on worst condition
# For instance in Qu'Appelle V1=3, V2=4, V3=2, V4=3 so the new variable coded as for folks who get V1 as 1
# v2 as 2 and v3 = 0 (where the best WQ) v4 = 1 (worst will get higer number)
# in this we we can always compre those version based on the worse WQ


df_final <- df_final%>%
  mutate(BASELINE_WQ_VARIATION = ifelse(CHOICE_AREA == "SUBBASIN" & VERSION_1 == 1 & CHOICE_SUB_BASIN == 2 , 1, 0), #Qu'Appelle
         BASELINE_WQ_VARIATION = ifelse(CHOICE_AREA == "SUBBASIN" & VERSION_2 == 1 & CHOICE_SUB_BASIN == 2 , 2, BASELINE_WQ_VARIATION),
         BASELINE_WQ_VARIATION = ifelse(CHOICE_AREA == "SUBBASIN" & VERSION_3 == 1 & CHOICE_SUB_BASIN == 2 , 0, BASELINE_WQ_VARIATION),
         BASELINE_WQ_VARIATION = ifelse(CHOICE_AREA == "SUBBASIN" & VERSION_4 == 1 & CHOICE_SUB_BASIN == 2 , 1, BASELINE_WQ_VARIATION),
         
         BASELINE_WQ_VARIATION = ifelse(CHOICE_AREA == "SUBBASIN" & VERSION_1 == 1 & CHOICE_SUB_BASIN == 1 , 1, BASELINE_WQ_VARIATION), #Assiniboine
         BASELINE_WQ_VARIATION = ifelse(CHOICE_AREA == "SUBBASIN" & VERSION_2 == 1 & CHOICE_SUB_BASIN == 1 , 2, BASELINE_WQ_VARIATION),
         BASELINE_WQ_VARIATION = ifelse(CHOICE_AREA == "SUBBASIN" & VERSION_3 == 1 & CHOICE_SUB_BASIN == 1 , 0, BASELINE_WQ_VARIATION),
         BASELINE_WQ_VARIATION = ifelse(CHOICE_AREA == "SUBBASIN" & VERSION_4 == 1 & CHOICE_SUB_BASIN == 1 , 1, BASELINE_WQ_VARIATION),
         
         BASELINE_WQ_VARIATION = ifelse(CHOICE_AREA == "SUBBASIN" & VERSION_1 == 1 & CHOICE_SUB_BASIN == 4 , 1, BASELINE_WQ_VARIATION), #Souris
         BASELINE_WQ_VARIATION = ifelse(CHOICE_AREA == "SUBBASIN" & VERSION_2 == 1 & CHOICE_SUB_BASIN == 4 , 2, BASELINE_WQ_VARIATION),
         BASELINE_WQ_VARIATION = ifelse(CHOICE_AREA == "SUBBASIN" & VERSION_3 == 1 & CHOICE_SUB_BASIN == 4 , 0, BASELINE_WQ_VARIATION),
         BASELINE_WQ_VARIATION = ifelse(CHOICE_AREA == "SUBBASIN" & VERSION_4 == 1 & CHOICE_SUB_BASIN == 4 , 1, BASELINE_WQ_VARIATION),
         
         BASELINE_WQ_VARIATION = ifelse(CHOICE_AREA == "SUBBASIN" & VERSION_1 == 1 & CHOICE_SUB_BASIN == 3 , 1, BASELINE_WQ_VARIATION), #Red
         BASELINE_WQ_VARIATION = ifelse(CHOICE_AREA == "SUBBASIN" & VERSION_2 == 1 & CHOICE_SUB_BASIN == 3 , 2, BASELINE_WQ_VARIATION),
         BASELINE_WQ_VARIATION = ifelse(CHOICE_AREA == "SUBBASIN" & VERSION_3 == 1 & CHOICE_SUB_BASIN == 3 , 0, BASELINE_WQ_VARIATION),
         BASELINE_WQ_VARIATION = ifelse(CHOICE_AREA == "SUBBASIN" & VERSION_4 == 1 & CHOICE_SUB_BASIN == 3 , 1, BASELINE_WQ_VARIATION),
         
         BASELINE_WQ_VARIATION = ifelse(CHOICE_AREA == "SUBBASIN" & VERSION_1 == 1 & CHOICE_SUB_BASIN == 6 , 2, BASELINE_WQ_VARIATION), #Grass and Burntwood
         BASELINE_WQ_VARIATION = ifelse(CHOICE_AREA == "SUBBASIN" & VERSION_2 == 1 & CHOICE_SUB_BASIN == 6 , 2, BASELINE_WQ_VARIATION),
         BASELINE_WQ_VARIATION = ifelse(CHOICE_AREA == "SUBBASIN" & VERSION_3 == 1 & CHOICE_SUB_BASIN == 6 , 0, BASELINE_WQ_VARIATION),
         BASELINE_WQ_VARIATION = ifelse(CHOICE_AREA == "SUBBASIN" & VERSION_4 == 1 & CHOICE_SUB_BASIN == 6 , 0, BASELINE_WQ_VARIATION),
         
         BASELINE_WQ_VARIATION = ifelse(CHOICE_AREA == "SUBBASIN" & VERSION_1 == 1 & CHOICE_SUB_BASIN == 8 , 2, BASELINE_WQ_VARIATION), #Nelson
         BASELINE_WQ_VARIATION = ifelse(CHOICE_AREA == "SUBBASIN" & VERSION_2 == 1 & CHOICE_SUB_BASIN == 8 , 1, BASELINE_WQ_VARIATION),
         BASELINE_WQ_VARIATION = ifelse(CHOICE_AREA == "SUBBASIN" & VERSION_3 == 1 & CHOICE_SUB_BASIN == 8 , 1, BASELINE_WQ_VARIATION),
         BASELINE_WQ_VARIATION = ifelse(CHOICE_AREA == "SUBBASIN" & VERSION_4 == 1 & CHOICE_SUB_BASIN == 8 , 0, BASELINE_WQ_VARIATION),
         
         BASELINE_WQ_VARIATION = ifelse(CHOICE_AREA == "SUBBASIN" & VERSION_1 == 1 & CHOICE_SUB_BASIN == 9 , 3, BASELINE_WQ_VARIATION), #Saskatchewan
         BASELINE_WQ_VARIATION = ifelse(CHOICE_AREA == "SUBBASIN" & VERSION_2 == 1 & CHOICE_SUB_BASIN == 9 , 2, BASELINE_WQ_VARIATION),
         BASELINE_WQ_VARIATION = ifelse(CHOICE_AREA == "SUBBASIN" & VERSION_3 == 1 & CHOICE_SUB_BASIN == 9 , 1, BASELINE_WQ_VARIATION),
         BASELINE_WQ_VARIATION = ifelse(CHOICE_AREA == "SUBBASIN" & VERSION_4 == 1 & CHOICE_SUB_BASIN == 9 , 0, BASELINE_WQ_VARIATION),
         
         BASELINE_WQ_VARIATION = ifelse(CHOICE_AREA == "SUBBASIN" & VERSION_1 == 1 & CHOICE_SUB_BASIN == 5 , 2, BASELINE_WQ_VARIATION), #Eastern Lake Winnipeg
         BASELINE_WQ_VARIATION = ifelse(CHOICE_AREA == "SUBBASIN" & VERSION_2 == 1 & CHOICE_SUB_BASIN == 5 , 1, BASELINE_WQ_VARIATION),
         BASELINE_WQ_VARIATION = ifelse(CHOICE_AREA == "SUBBASIN" & VERSION_3 == 1 & CHOICE_SUB_BASIN == 5 , 1, BASELINE_WQ_VARIATION),
         BASELINE_WQ_VARIATION = ifelse(CHOICE_AREA == "SUBBASIN" & VERSION_4 == 1 & CHOICE_SUB_BASIN == 5 , 0, BASELINE_WQ_VARIATION),
         
         BASELINE_WQ_VARIATION = ifelse(CHOICE_AREA == "SUBBASIN" & VERSION_1 == 1 & CHOICE_SUB_BASIN == 7 , 2, BASELINE_WQ_VARIATION), #Lake Winnipegosis and Lake Manitoba
         BASELINE_WQ_VARIATION = ifelse(CHOICE_AREA == "SUBBASIN" & VERSION_2 == 1 & CHOICE_SUB_BASIN == 7 , 2, BASELINE_WQ_VARIATION),
         BASELINE_WQ_VARIATION = ifelse(CHOICE_AREA == "SUBBASIN" & VERSION_3 == 1 & CHOICE_SUB_BASIN == 7 , 0, BASELINE_WQ_VARIATION),
         BASELINE_WQ_VARIATION = ifelse(CHOICE_AREA == "SUBBASIN" & VERSION_4 == 1 & CHOICE_SUB_BASIN == 7 , 0, BASELINE_WQ_VARIATION),
         
         BASELINE_WQ_VARIATION = ifelse(CHOICE_AREA == "SUBBASIN" & VERSION_1 == 1 & CHOICE_SUB_BASIN == 10 , 0, BASELINE_WQ_VARIATION), #Western Lake Winnipeg
         BASELINE_WQ_VARIATION = ifelse(CHOICE_AREA == "SUBBASIN" & VERSION_2 == 1 & CHOICE_SUB_BASIN == 10 , 0, BASELINE_WQ_VARIATION),
         BASELINE_WQ_VARIATION = ifelse(CHOICE_AREA == "SUBBASIN" & VERSION_3 == 1 & CHOICE_SUB_BASIN == 10 , 0, BASELINE_WQ_VARIATION),
         BASELINE_WQ_VARIATION = ifelse(CHOICE_AREA == "SUBBASIN" & VERSION_4 == 1 & CHOICE_SUB_BASIN == 10 , 0, BASELINE_WQ_VARIATION),
         
         BASELINE_WQ_VARIATION = ifelse(CHOICE_AREA == "SUBBASIN" & VERSION_1 == 1 & CHOICE_SUB_BASIN == 12 , 2, BASELINE_WQ_VARIATION), #Central North Saskatchewan
         BASELINE_WQ_VARIATION = ifelse(CHOICE_AREA == "SUBBASIN" & VERSION_2 == 1 & CHOICE_SUB_BASIN == 12 , 2, BASELINE_WQ_VARIATION),
         BASELINE_WQ_VARIATION = ifelse(CHOICE_AREA == "SUBBASIN" & VERSION_3 == 1 & CHOICE_SUB_BASIN == 12 , 0, BASELINE_WQ_VARIATION),
         BASELINE_WQ_VARIATION = ifelse(CHOICE_AREA == "SUBBASIN" & VERSION_4 == 1 & CHOICE_SUB_BASIN == 12 , 0, BASELINE_WQ_VARIATION),
         
         BASELINE_WQ_VARIATION = ifelse(CHOICE_AREA == "SUBBASIN" & VERSION_1 == 1 & CHOICE_SUB_BASIN == 14 , 2, BASELINE_WQ_VARIATION), #Upper North Saskatchewan
         BASELINE_WQ_VARIATION = ifelse(CHOICE_AREA == "SUBBASIN" & VERSION_2 == 1 & CHOICE_SUB_BASIN == 14 , 2, BASELINE_WQ_VARIATION),
         BASELINE_WQ_VARIATION = ifelse(CHOICE_AREA == "SUBBASIN" & VERSION_3 == 1 & CHOICE_SUB_BASIN == 14 , 0, BASELINE_WQ_VARIATION),
         BASELINE_WQ_VARIATION = ifelse(CHOICE_AREA == "SUBBASIN" & VERSION_4 == 1 & CHOICE_SUB_BASIN == 14 , 0, BASELINE_WQ_VARIATION),
         
         BASELINE_WQ_VARIATION = ifelse(CHOICE_AREA == "SUBBASIN" & VERSION_1 == 1 & CHOICE_SUB_BASIN == 11 , 2, BASELINE_WQ_VARIATION), #Battle
         BASELINE_WQ_VARIATION = ifelse(CHOICE_AREA == "SUBBASIN" & VERSION_2 == 1 & CHOICE_SUB_BASIN == 11 , 2, BASELINE_WQ_VARIATION),
         BASELINE_WQ_VARIATION = ifelse(CHOICE_AREA == "SUBBASIN" & VERSION_3 == 1 & CHOICE_SUB_BASIN == 11 , 0, BASELINE_WQ_VARIATION),
         BASELINE_WQ_VARIATION = ifelse(CHOICE_AREA == "SUBBASIN" & VERSION_4 == 1 & CHOICE_SUB_BASIN == 11 , 0, BASELINE_WQ_VARIATION),
         
         BASELINE_WQ_VARIATION = ifelse(CHOICE_AREA == "SUBBASIN" & VERSION_1 == 1 & CHOICE_SUB_BASIN == 13 , 3, BASELINE_WQ_VARIATION), #Lower North Saskatchewan
         BASELINE_WQ_VARIATION = ifelse(CHOICE_AREA == "SUBBASIN" & VERSION_2 == 1 & CHOICE_SUB_BASIN == 13 , 2, BASELINE_WQ_VARIATION),
         BASELINE_WQ_VARIATION = ifelse(CHOICE_AREA == "SUBBASIN" & VERSION_3 == 1 & CHOICE_SUB_BASIN == 13 , 1, BASELINE_WQ_VARIATION),
         BASELINE_WQ_VARIATION = ifelse(CHOICE_AREA == "SUBBASIN" & VERSION_4 == 1 & CHOICE_SUB_BASIN == 13 , 0, BASELINE_WQ_VARIATION),
         
         BASELINE_WQ_VARIATION = ifelse(CHOICE_AREA == "SUBBASIN" & VERSION_1 == 1 & CHOICE_SUB_BASIN == 15 , 2, BASELINE_WQ_VARIATION), #Bow
         BASELINE_WQ_VARIATION = ifelse(CHOICE_AREA == "SUBBASIN" & VERSION_2 == 1 & CHOICE_SUB_BASIN == 15 , 2, BASELINE_WQ_VARIATION),
         BASELINE_WQ_VARIATION = ifelse(CHOICE_AREA == "SUBBASIN" & VERSION_3 == 1 & CHOICE_SUB_BASIN == 15 , 0, BASELINE_WQ_VARIATION),
         BASELINE_WQ_VARIATION = ifelse(CHOICE_AREA == "SUBBASIN" & VERSION_4 == 1 & CHOICE_SUB_BASIN == 15 , 0, BASELINE_WQ_VARIATION),
         
         BASELINE_WQ_VARIATION = ifelse(CHOICE_AREA == "SUBBASIN" & VERSION_1 == 1 & CHOICE_SUB_BASIN == 17 , 1, BASELINE_WQ_VARIATION), #Red Deer
         BASELINE_WQ_VARIATION = ifelse(CHOICE_AREA == "SUBBASIN" & VERSION_2 == 1 & CHOICE_SUB_BASIN == 17 , 1, BASELINE_WQ_VARIATION),
         BASELINE_WQ_VARIATION = ifelse(CHOICE_AREA == "SUBBASIN" & VERSION_3 == 1 & CHOICE_SUB_BASIN == 17 , 0, BASELINE_WQ_VARIATION),
         BASELINE_WQ_VARIATION = ifelse(CHOICE_AREA == "SUBBASIN" & VERSION_4 == 1 & CHOICE_SUB_BASIN == 17 , 0, BASELINE_WQ_VARIATION),
         
         
         BASELINE_WQ_VARIATION = ifelse(CHOICE_AREA == "SUBBASIN" & VERSION_1 == 1 & CHOICE_SUB_BASIN == 16 , 1, BASELINE_WQ_VARIATION), #Lower South Saskatchewan
         BASELINE_WQ_VARIATION = ifelse(CHOICE_AREA == "SUBBASIN" & VERSION_2 == 1 & CHOICE_SUB_BASIN == 16 , 1, BASELINE_WQ_VARIATION),
         BASELINE_WQ_VARIATION = ifelse(CHOICE_AREA == "SUBBASIN" & VERSION_3 == 1 & CHOICE_SUB_BASIN == 16 , 0, BASELINE_WQ_VARIATION),
         BASELINE_WQ_VARIATION = ifelse(CHOICE_AREA == "SUBBASIN" & VERSION_4 == 1 & CHOICE_SUB_BASIN == 16 , 0, BASELINE_WQ_VARIATION),
         
         BASELINE_WQ_VARIATION = ifelse(CHOICE_AREA == "SUBBASIN" & VERSION_1 == 1 & CHOICE_SUB_BASIN == 18 , 1, BASELINE_WQ_VARIATION), #Upper South Saskatchewan
         BASELINE_WQ_VARIATION = ifelse(CHOICE_AREA == "SUBBASIN" & VERSION_2 == 1 & CHOICE_SUB_BASIN == 18 , 1, BASELINE_WQ_VARIATION),
         BASELINE_WQ_VARIATION = ifelse(CHOICE_AREA == "SUBBASIN" & VERSION_3 == 1 & CHOICE_SUB_BASIN == 18 , 0, BASELINE_WQ_VARIATION),
         BASELINE_WQ_VARIATION = ifelse(CHOICE_AREA == "SUBBASIN" & VERSION_4 == 1 & CHOICE_SUB_BASIN == 18 , 0, BASELINE_WQ_VARIATION),
         
         BASELINE_WQ_VARIATION = ifelse(CHOICE_AREA == "BASIN" & VERSION_1 == 1 & CHOICE_BASIN == 1 , 1, BASELINE_WQ_VARIATION), #AR
         BASELINE_WQ_VARIATION = ifelse(CHOICE_AREA == "BASIN" & VERSION_2 == 1 & CHOICE_BASIN == 1 , 2, BASELINE_WQ_VARIATION),
         BASELINE_WQ_VARIATION = ifelse(CHOICE_AREA == "BASIN" & VERSION_3 == 1 & CHOICE_BASIN == 1 , 0, BASELINE_WQ_VARIATION),
         BASELINE_WQ_VARIATION = ifelse(CHOICE_AREA == "BASIN" & VERSION_4 == 1 & CHOICE_BASIN == 1 , 1, BASELINE_WQ_VARIATION),
         
         BASELINE_WQ_VARIATION = ifelse(CHOICE_AREA == "BASIN" & VERSION_1 == 1 & CHOICE_BASIN == 2 , 2.1, BASELINE_WQ_VARIATION), #LSN
         BASELINE_WQ_VARIATION = ifelse(CHOICE_AREA == "BASIN" & VERSION_2 == 1 & CHOICE_BASIN == 2 , 1.58, BASELINE_WQ_VARIATION),
         BASELINE_WQ_VARIATION = ifelse(CHOICE_AREA == "BASIN" & VERSION_3 == 1 & CHOICE_BASIN == 2 , 0.53, BASELINE_WQ_VARIATION),
         BASELINE_WQ_VARIATION = ifelse(CHOICE_AREA == "BASIN" & VERSION_4 == 1 & CHOICE_BASIN == 2 , 0, BASELINE_WQ_VARIATION),
         
         BASELINE_WQ_VARIATION = ifelse(CHOICE_AREA == "BASIN" & VERSION_1 == 1 & CHOICE_BASIN == 3 , 2.33, BASELINE_WQ_VARIATION), #NS
         BASELINE_WQ_VARIATION = ifelse(CHOICE_AREA == "BASIN" & VERSION_2 == 1 & CHOICE_BASIN == 3 , 2, BASELINE_WQ_VARIATION),
         BASELINE_WQ_VARIATION = ifelse(CHOICE_AREA == "BASIN" & VERSION_3 == 1 & CHOICE_BASIN == 3 , 0.33, BASELINE_WQ_VARIATION),
         BASELINE_WQ_VARIATION = ifelse(CHOICE_AREA == "BASIN" & VERSION_4 == 1 & CHOICE_BASIN == 3 , 0, BASELINE_WQ_VARIATION),
         
         BASELINE_WQ_VARIATION = ifelse(CHOICE_AREA == "BASIN" & VERSION_1 == 1 & CHOICE_BASIN == 4 , 1.15, BASELINE_WQ_VARIATION), #SS
         BASELINE_WQ_VARIATION = ifelse(CHOICE_AREA == "BASIN" & VERSION_2 == 1 & CHOICE_BASIN == 4 , 1.15, BASELINE_WQ_VARIATION),
         BASELINE_WQ_VARIATION = ifelse(CHOICE_AREA == "BASIN" & VERSION_3 == 1 & CHOICE_BASIN == 4 , 0, BASELINE_WQ_VARIATION),
         BASELINE_WQ_VARIATION = ifelse(CHOICE_AREA == "BASIN" & VERSION_4 == 1 & CHOICE_BASIN == 4 , 0, BASELINE_WQ_VARIATION),
         
  )


df_final <- df_final%>%
  mutate(BASELINE_WQ_VARIATION_ROUND = round(BASELINE_WQ_VARIATION,0))%>%
  mutate(BASELINE_WQ_0UNIT = ifelse(BASELINE_WQ_VARIATION_ROUND == 0, 1,0),
         BASELINE_WQ_1UNIT = ifelse(BASELINE_WQ_VARIATION_ROUND == 1, 1,0),
         BASELINE_WQ_2UNIT = ifelse(BASELINE_WQ_VARIATION_ROUND == 2, 1,0),
         BASELINE_WQ_3UNIT = ifelse(BASELINE_WQ_VARIATION_ROUND == 3, 1,0))


df_final <- df_final%>%
  mutate(BASELINE_WQ_HIGHEST = ifelse(BASELINE_WQ_VARIATION == 0,1,0),
         BASELINE_WQ_0_1UNIT = ifelse(BASELINE_WQ_VARIATION >= 0.1 & BASELINE_WQ_VARIATION <= 1, 1, 0),
         BASELINE_WQ_1_2UNIT = ifelse(BASELINE_WQ_VARIATION >= 1.1 & BASELINE_WQ_VARIATION <= 2, 1, 0),
         BASELINE_WQ_2_3UNIT = ifelse(BASELINE_WQ_VARIATION >= 2.1 & BASELINE_WQ_VARIATION <= 3, 1, 0),
  )


df_final <- df_final%>%
  mutate(WQ_BASIN_LOCAL_CURRENT_X_BASELINE_WQ_1UNIT = WQ_BASIN_LOCAL_CURRENT*BASELINE_WQ_1UNIT,
         WQ_BASIN_NL_CURRENT_X_BASELINE_WQ_1UNIT = WQ_BASIN_NL_CURRENT*BASELINE_WQ_1UNIT,
         WQ_SUBBASIN_LOCAL_CURRENT_SUBONLY_X_BASELINE_WQ_1UNIT = WQ_SUBBASIN_LOCAL_CURRENT_SUBONLY*BASELINE_WQ_1UNIT,
         WQ_SUBBASIN_NL_CURRENT_SUBONLY_X_BASELINE_WQ_1UNIT = WQ_SUBBASIN_NL_CURRENT_SUBONLY*BASELINE_WQ_1UNIT,
         
         WQ_BASIN_LOCAL_CURRENT_X_BASELINE_WQ_2UNIT = WQ_BASIN_LOCAL_CURRENT*BASELINE_WQ_2UNIT,
         WQ_BASIN_NL_CURRENT_X_BASELINE_WQ_2UNIT = WQ_BASIN_NL_CURRENT*BASELINE_WQ_2UNIT,
         WQ_SUBBASIN_LOCAL_CURRENT_SUBONLY_X_BASELINE_WQ_2UNIT = WQ_SUBBASIN_LOCAL_CURRENT_SUBONLY*BASELINE_WQ_2UNIT,
         WQ_SUBBASIN_NL_CURRENT_SUBONLY_X_BASELINE_WQ_2UNIT = WQ_SUBBASIN_NL_CURRENT_SUBONLY*BASELINE_WQ_2UNIT,
         
         WQ_BASIN_LOCAL_CURRENT_X_BASELINE_WQ_3UNIT = WQ_BASIN_LOCAL_CURRENT*BASELINE_WQ_3UNIT,
         WQ_BASIN_NL_CURRENT_X_BASELINE_WQ_3UNIT = WQ_BASIN_NL_CURRENT*BASELINE_WQ_3UNIT,
         WQ_SUBBASIN_LOCAL_CURRENT_SUBONLY_X_BASELINE_WQ_3UNIT = WQ_SUBBASIN_LOCAL_CURRENT_SUBONLY*BASELINE_WQ_3UNIT,
         WQ_SUBBASIN_NL_CURRENT_SUBONLY_X_BASELINE_WQ_3UNIT = WQ_SUBBASIN_NL_CURRENT_SUBONLY*BASELINE_WQ_3UNIT,
         
         WQ_BASIN_LOCAL_POLICY_X_BASELINE_WQ_1UNIT = WQ_BASIN_LOCAL_POLICY*BASELINE_WQ_1UNIT,
         WQ_BASIN_NL_POLICY_X_BASELINE_WQ_1UNIT = WQ_BASIN_NL_POLICY*BASELINE_WQ_1UNIT,
         WQ_SUBBASIN_LOCAL_POLICY_SUBONLY_X_BASELINE_WQ_1UNIT = WQ_SUBBASIN_LOCAL_POLICY_SUBONLY*BASELINE_WQ_1UNIT,
         WQ_SUBBASIN_NL_POLICY_SUBONLY_X_BASELINE_WQ_1UNIT = WQ_SUBBASIN_NL_POLICY_SUBONLY*BASELINE_WQ_1UNIT,
         
         WQ_BASIN_LOCAL_POLICY_X_BASELINE_WQ_2UNIT = WQ_BASIN_LOCAL_POLICY*BASELINE_WQ_2UNIT,
         WQ_BASIN_NL_POLICY_X_BASELINE_WQ_2UNIT = WQ_BASIN_NL_POLICY*BASELINE_WQ_2UNIT,
         WQ_SUBBASIN_LOCAL_POLICY_SUBONLY_X_BASELINE_WQ_2UNIT = WQ_SUBBASIN_LOCAL_POLICY_SUBONLY*BASELINE_WQ_2UNIT,
         WQ_SUBBASIN_NL_POLICY_SUBONLY_X_BASELINE_WQ_2UNIT = WQ_SUBBASIN_NL_POLICY_SUBONLY*BASELINE_WQ_2UNIT,
         
         WQ_BASIN_LOCAL_POLICY_X_BASELINE_WQ_3UNIT = WQ_BASIN_LOCAL_POLICY*BASELINE_WQ_3UNIT,
         WQ_BASIN_NL_POLICY_X_BASELINE_WQ_3UNIT = WQ_BASIN_NL_POLICY*BASELINE_WQ_3UNIT,
         WQ_SUBBASIN_LOCAL_POLICY_SUBONLY_X_BASELINE_WQ_3UNIT = WQ_SUBBASIN_LOCAL_POLICY_SUBONLY*BASELINE_WQ_3UNIT,
         WQ_SUBBASIN_NL_POLICY_SUBONLY_X_BASELINE_WQ_3UNIT = WQ_SUBBASIN_NL_POLICY_SUBONLY*BASELINE_WQ_3UNIT,
         
         wq_sub_basin_current,wq_sub_basin_policy,
  )


###############################################################################

df_final <- df_final %>%
  mutate(
    WQ_POLICY_BASIN = ifelse(CHOICE_AREA == "BASIN", POLICY_AVERAGE, 0),
    WQ_POLICT_SUBBASIN = ifelse(CHOICE_AREA == "SUBBASIN", wq_sub_basin_policy, 0)
  )


df_final <- df_final %>%
  mutate(
    WQ_POLICY = case_when(
      CHOICE_AREA == "BASIN" ~ POLICY_AVERAGE,
      CHOICE_AREA == "SUBBASIN" ~ wq_sub_basin_policy,
      TRUE ~ 0
    )
  )
###############################################################################
# Adding respondents weight

weight <- read_csv("data/derived/test/final_weights.csv")%>%
  mutate(CaseId = as.character(CaseId))%>%
  rename(WEIGHT = final_weight)%>%
  distinct(CaseId, .keep_all = T)

df_final <- df_final%>%
  left_join(weight)


###############################################################################


# Reorder columns to allingn with the order of the survey

df_final <- df_final[, c( "CaseId","WEIGHT","SURVEY_CONTENT","TREATMENT","VERSION","version","SURVEY_VERSION_1","SURVEY_VERSION_2", "VERSION_1", "VERSION_2", "VERSION_3", "VERSION_4", # Add CONDITION later
                          "BLK_NUMBER","CHOICE_NUMBER","BASIN","SUB_BASIN","NON_LOCAL",
                          "IMAGECURRENT","IMAGEPOLICY","CURRENT_AVERAGE","POLICY_AVERAGE",
                          "wq_sub_basin_current","wq_sub_basin_policy",
                          "WQ_CHANGE", "WQ_CHANGE_BASIN", "WQ_CHANGE_SUBBASIN",
                          
                          "BASELINE_X_WQCHANGE",
                          
                          "BASELINE_X_WQCHANGE_LOCAL_BASIN","BASELINE_X_WQCHANGE_NL_BASIN",
                          "BASELINE_X_WQCHANGE_LOCAL_SUBBASIN","BASELINE_X_WQCHANGE_NL_SUBBASIN",
                          
                          "BASELINE_WQ_VARIATION","BASELINE_WQ_0UNIT","BASELINE_WQ_1UNIT","BASELINE_WQ_2UNIT","BASELINE_WQ_3UNIT",
                          
                          "BASELINE_WQ_HIGHEST", "BASELINE_WQ_0_1UNIT", "BASELINE_WQ_1_2UNIT", "BASELINE_WQ_2_3UNIT",
                          
                          "CHOICE_AREA","CHOICE_BASIN","CHOICE_SUB_BASIN","CHOICE_LOCALITY_BASIN","CHOICE_LOCALITY_SUBBASIN",
                          "POLICY_SIZE_KM","POLICY_SIZE_PERCENT","WQ_UP1","WQ_UP2","WQ_UP3","WQ_BY1",
                          "WQ_LOCAL_CURRENT","WQ_NL_CURRENT","WQ_LOCAL_POLICY","WQ_NL_POLICY",
                          "SHARED_BOADER","SHARED_BOADER_PROV",
                          "WQ_HOME_CURRENT","WQ_HOME_POLICY",
                          
                          "WQ_BASIN_LOCAL_CURRENT", "WQ_BASIN_LOCAL_POLICY",
                          "WQ_BASIN_NL_CURRENT", "WQ_BASIN_NL_POLICY",
                          "WQ_SUBBASIN_LOCAL_CURRENT_SUBONLY", "WQ_SUBBASIN_LOCAL_POLICY_SUBONLY",
                          "WQ_SUBBASIN_NL_CURRENT_SUBONLY","WQ_SUBBASIN_NL_POLICY_SUBONLY",
                          
                          "WQ_SUBBASIN_LOCAL_SB_CURRENT", "WQ_SUBBASIN_LOCAL_NSB_CURRENT",
                          "WQ_SUBBASIN_LOCAL_SB_POLICY", "WQ_SUBBASIN_LOCAL_NSB_POLICY",
                          "WQ_SUBBASIN_NONLOCAL_SB_CURRENT","WQ_SUBBASIN_NONLOCAL_NSB_CURRENT",
                          "WQ_SUBBASIN_NONLOCAL_SB_POLICY", "WQ_SUBBASIN_NONLOCAL_NSB_POLICY",
                          "WQ_SUBBASIN_LOCAL_SB_LP_CURRENT", "WQ_SUBBASIN_LOCAL_NSB_LP_CURRENT",
                          "WQ_SUBBASIN_LOCAL_SB_LP_POLICY", "WQ_SUBBASIN_LOCAL_NSB_LP_POLICY",
                          
                          
                          "WQ_CURRENT_CHOICE", "WQ_POLICY_CHOICE",
                          
                          
                          "WQ_BASIN_LOCAL_CURRENT_X_BASELINE_WQ_1UNIT",
                          "WQ_BASIN_NL_CURRENT_X_BASELINE_WQ_1UNIT",
                          "WQ_SUBBASIN_LOCAL_CURRENT_SUBONLY_X_BASELINE_WQ_1UNIT", 
                          "WQ_SUBBASIN_NL_CURRENT_SUBONLY_X_BASELINE_WQ_1UNIT",
                          
                          "WQ_BASIN_LOCAL_CURRENT_X_BASELINE_WQ_2UNIT", 
                          "WQ_BASIN_NL_CURRENT_X_BASELINE_WQ_2UNIT", 
                          "WQ_SUBBASIN_LOCAL_CURRENT_SUBONLY_X_BASELINE_WQ_2UNIT", 
                          "WQ_SUBBASIN_NL_CURRENT_SUBONLY_X_BASELINE_WQ_2UNIT", 
                          
                          "WQ_BASIN_LOCAL_CURRENT_X_BASELINE_WQ_3UNIT", 
                          "WQ_BASIN_NL_CURRENT_X_BASELINE_WQ_3UNIT", 
                          "WQ_SUBBASIN_LOCAL_CURRENT_SUBONLY_X_BASELINE_WQ_3UNIT", 
                          "WQ_SUBBASIN_NL_CURRENT_SUBONLY_X_BASELINE_WQ_3UNIT",
                          
                          "WQ_BASIN_LOCAL_POLICY_X_BASELINE_WQ_1UNIT", 
                          "WQ_BASIN_NL_POLICY_X_BASELINE_WQ_1UNIT", 
                          "WQ_SUBBASIN_LOCAL_POLICY_SUBONLY_X_BASELINE_WQ_1UNIT", 
                          "WQ_SUBBASIN_NL_POLICY_SUBONLY_X_BASELINE_WQ_1UNIT", 
                          
                          "WQ_BASIN_LOCAL_POLICY_X_BASELINE_WQ_2UNIT", 
                          "WQ_BASIN_NL_POLICY_X_BASELINE_WQ_2UNIT", 
                          "WQ_SUBBASIN_LOCAL_POLICY_SUBONLY_X_BASELINE_WQ_2UNIT", 
                          "WQ_SUBBASIN_NL_POLICY_SUBONLY_X_BASELINE_WQ_2UNIT", 
                          
                          "WQ_BASIN_LOCAL_POLICY_X_BASELINE_WQ_3UNIT", 
                          "WQ_BASIN_NL_POLICY_X_BASELINE_WQ_3UNIT", 
                          "WQ_SUBBASIN_LOCAL_POLICY_SUBONLY_X_BASELINE_WQ_3UNIT",
                          "WQ_SUBBASIN_NL_POLICY_SUBONLY_X_BASELINE_WQ_3UNIT",
                          
                          "WQ_POLICY_BASIN","WQ_POLICT_SUBBASIN",
                          
                          "WQ_POLICY",
                          
                          "POLICY_SIZE_KM",
                          
                          "HOME_PROV_SHARE",
                          "AREA_INSTATE_LOCAL_BASIN","AREA_INSTATE_NL_BASIN",
                          "AREA_INSTATE_LOCAL_SUBBASIN", "AREA_INSTATE_NL_SUBBASIN",
                          
                          "AREA_INSTATE_LOCAL","AREA_INSTATE_NONLOCAL",
                          
                          "LOCAL_ADJUCENT",
                          "NON_LOCAL_ADJUCENT_LOCAL_BASIN",
                          "NON_LOCAL_ADJUCENT_LOCAL_SUBBASIN",
                          "WQ_NON_LOCAL_ADJUCENT_LOCAL_BASIN_CURRENT","WQ_NON_LOCAL_ADJUCENT_LOCAL_SUBBASIN_CURRENT",
                          "WQ_NON_LOCAL_ADJUCENT_LOCAL_BASIN_POLICY", "WQ_NON_LOCAL_ADJUCENT_LOCAL_SUBBASIN_POLCIY",
                          
                          "WQ_NON_LOCAL_NOT_ADJUCENT_LOCAL_BASIN_CURRENT","WQ_NON_LOCAL_NOT_ADJUCENT_LOCAL_SUBBASIN_CURRENT",
                          "WQ_NON_LOCAL_NOT_ADJUCENT_LOCAL_BASIN_POLICY", "WQ_NON_LOCAL_NOT_ADJUCENT_LOCAL_SUBBASIN_POLCIY",
                          
                          "COST","VOTE",
                          "PROVINCE", "AGE", "GENDER", "LANGUAGE", "INCOME", "POSTALCODE",
                          "FAMILIARITY_RIVER_LAKES",
                          "Q2_NOT_IMPORTANT","Q2_HABITAT","Q2_RECREATION","Q2_LANDSCAPE","Q2_COMP_NATURAL_ENVIR","Q2_OTHER",
                          "Q3_DIMINISH_VISUAL","Q3_DIMINISH_NATIVE_PLANT","Q3_SWIMMING_ADVI","Q3_DIMINISH_RECREATION",
                          "Q3_HARMFUL_ALGAE","Q3_CONSUMPTIPN_ADVI_FISH","Q3_DRINKING_WATER_ADVI","Q3_NONE","Q3_NOT_AWARE",
                          "Q4_UNPLEASANT_ODORS","Q4_Q4_LIMITED_CLARITY","Q4_SAMLL_ALGAE","Q4_LARGE_ALGAE",
                          "Q4_MURKY_WATER","Q4_TRASH_SHORE","Q4_UNHEALTHY_VEGETATION","Q4_NOT_BEEN_NEAR_LAKE","Q4_NOT_NOTICED",
                          "Q5_WATER_APPEAR_IMAGE_TEST","Q6_NATURAL_FLOW_IMAGE_TEST","Q7_DIVERSITY_IMAGE_TEST",
                          "Q8_SK_NON_LOCALMAP_TEST","Q8_PA_NON_LOCALMAP_TEST","Q9_LOCALMAP_TEST","LOCAL_WQ_OPINION",
                          "Q12_VOTE_WITHOUT_CONSIDER_OTHER","Q12_VOTE_HOUSEHOLD_FACE_COST","Q12_VOTE_CERTAIN_PUBLIC_ELEC",
                          "Q12_VOTE_INFORM_POLICY_MAKERS","Q12_VOTE_POLICY_ACHIEVE_IMPROV",
                          "Q13_INFLUENCE_WQ_LEVEL","Q13_INFLUENCE_NEAR_HOME","Q13_INFLUENCE_COST","Q13_INFLUENCE_REGIONSIZE",
                          "Q14_CERTAIN_VOTE","CHECK_NEXTDAY_AFTER_FRIDAY",
                          "Q15_GENERAL_THOUGHTS_CH1","Q15_GENERAL_THOUGHTS__CH2","Q15_GENERAL_THOUGHTS__CH3",
                          "Q15_GENERAL_THOUGHTS__CH4","Q15_GENERAL_THOUGHTS__CH5","Q15_GENERAL_THOUGHTS__CH6",
                          "Q15_GENERAL_THOUGHTS__CH7","Q15_GENERAL_THOUGHTS__CH8","Q15_GENERAL_THOUGHTS__CH9","Q15_GENERAL_THOUGHTS__CH10",
                   
                          "Q16_SURVEY_PUSH_VOTE",
                          "Q17_HUMAN_CAN_MODIFY","Q17_HUMAN_ABUSING","Q17_PLANTS_ANIMAL_RIGHT","Q17_NATURE_CAPABILITY",
                          "Q17_HUMAN_RULE","Q17_NATURE_DELICATE",
                   
                          "Q18_CURRENT_LOCATION_STAY","Q19_MEMBER_OF_ENVIRON_ORG","Q20_WQ_CONSIDER_CURRENT_LIVING",
                          "Q21_REC_TRIP_TWO_YEARS","REC_TRIP","REC_IN_CHOICE_BASIN","Q22_DISTANCE_TRAVEL",
                          "Q23_FISHING","Q23_SWIMMING","Q23_CANNONING", "Q23_HUNTING", "Q23_OTHER",
                   
                          "Q24_REC_TRIP_LAST_YEAR",
                          "Q25_WB1_NAME","Q25_WB2_NAME","Q25_WB3_NAME","Q25_WB4_NAME","Q25_WB5_NAME","Q25_WB1_WQ_LEVEL",
                          "Q25_WB2_WQ_LEVEL","Q25_WB3_WQ_LEVEL","Q25_WB4_WQ_LEVEL","Q25_WB5_WQ_LEVEL","Q25_WB1_NEAR_TOWN",
                          "Q25_WB2_NEAR_TOWN","Q25_WB3_NEAR_TOWN","Q25_WB4_NEAR_TOWN","Q25_WB5_NEAR_TOWN",
                          "COMMENTS",
                          "Q1_POLI","Q2_POLI","Q3_POLI","Q4_POLI","Q5_POLI","Q6_POLI","Q7_POLI","Q8_POLI","Q9_POLI","Q10_POLI",
                          "Q12_POLI","Q13_POLI","Q14_POLI","Q15_POLI","Q16_POLI","Q17_POLI","Q18_POLI","Q19_POLI",                # "Q11_POLI"
                          "Q20_POLI","Q21_POLI","Q22_POLI","Q23_POLI","Q24_POLI","Q25_POLI","Q26_POLI","Q1_MOVIE","Q2_MOVIE",
                          "Q3_MOVIE","Q4_MOVIE","Q5_MOVIE","Q6_MOVIE","Q7_MOVIE","Q8_MOVIE","Q9_MOVIE","Q10_MOVIE","Q11_MOVIE",
                          "Q12_MOVIE","Q13_MOVIE","Q14_MOVIE","Q15_MOVIE","Q16_MOVIE","Q17_MOVIE","Q18_MOVIE","Q19_MOVIE",
                          "Q20_MOVIE","Q21_MOVIE","Q22_MOVIE","Q23_MOVIE","Q24_MOVIE","Q25_MOVIE","Q26_MOVIE","Q27_MOVIE",
                          "Q28_MOVIE","Q29_MOVIE","Q30_MOVIE"
                   
)]


# Write final data frame to a rds file that can ffed into Apollo
#saveRDS(df_final, "data/derived/test/processed_finaldata_batch_1_Apollo.rds")
write.csv(df_final, "data/derived/test/processed_finaldata_batch_1_Apollo.csv",row.names = FALSE)

###############################################################################

# This following codes provide some additional variable tha I use with final survey data analysis 
# Later this section needs to be incorporated with above 



df_final <- read_csv("data/derived/test/processed_finaldata_batch_1_Apollo.csv")%>%
  group_by(CaseId)%>%
  mutate(LOCAL_CHOICE = ifelse(BASIN == CHOICE_BASIN,1,0))%>%
  ungroup()%>%
  mutate(WQ_BASIN_LOCAL_CURRENT_N = ifelse(LOCAL_CHOICE == 1 & CHOICE_AREA == "BASIN",CURRENT_AVERAGE,0),
         WQ_BASIN_NL_CURRENT_N = ifelse(LOCAL_CHOICE == 0 & CHOICE_AREA == "BASIN",CURRENT_AVERAGE,0),
         WQ_SUBBASIN_LOCAL_CURRENT_SUBONLY_N = ifelse(LOCAL_CHOICE == 1 & CHOICE_AREA == "SUBBASIN",wq_sub_basin_current,0),
         WQ_SUBBASIN_NL_CURRENT_SUBONLY_N = ifelse(LOCAL_CHOICE == 0 & CHOICE_AREA == "SUBBASIN",wq_sub_basin_current,0),
         
         WQ_BASIN_LOCAL_POLICY_N = ifelse(LOCAL_CHOICE == 1 & CHOICE_AREA == "BASIN",POLICY_AVERAGE,0),
         WQ_BASIN_NL_POLICY_N = ifelse(LOCAL_CHOICE == 0 & CHOICE_AREA == "BASIN",POLICY_AVERAGE,0),
         WQ_SUBBASIN_LOCAL_POLICY_SUBONLY_N = ifelse(LOCAL_CHOICE == 1 & CHOICE_AREA == "SUBBASIN",wq_sub_basin_policy,0),
         WQ_SUBBASIN_NL_POLICY_SUBONLY_N = ifelse(LOCAL_CHOICE == 0 & CHOICE_AREA == "SUBBASIN",wq_sub_basin_policy,0),
         
         WQ_CURRENT_LOCAL_CHOICE = case_when(
           LOCAL_CHOICE == 1 & CHOICE_AREA == "BASIN" ~ CURRENT_AVERAGE,
           LOCAL_CHOICE == 1 & CHOICE_AREA == "SUBBASIN" ~ wq_sub_basin_current,
           TRUE ~ 0),
         
         WQ_CURRENT_NL_CHOICE = case_when(
           LOCAL_CHOICE == 0 & CHOICE_AREA == "BASIN" ~ CURRENT_AVERAGE,
           LOCAL_CHOICE == 0 & CHOICE_AREA == "SUBBASIN" ~ wq_sub_basin_current,
           TRUE ~ 0),
         
         WQ_CURRENT_CHOICE = case_when(
           LOCAL_CHOICE == 1 & CHOICE_AREA == "BASIN" ~ CURRENT_AVERAGE,
           LOCAL_CHOICE == 1 & CHOICE_AREA == "SUBBASIN" ~ wq_sub_basin_current,
           LOCAL_CHOICE == 0 & CHOICE_AREA == "BASIN" ~ CURRENT_AVERAGE,
           LOCAL_CHOICE == 0 & CHOICE_AREA == "SUBBASIN" ~ wq_sub_basin_current,
           TRUE ~ 0),
         
         
         WQ_POLICY_LOCAL_CHOICE = case_when(
           LOCAL_CHOICE == 1 & CHOICE_AREA == "BASIN" ~ POLICY_AVERAGE,
           LOCAL_CHOICE == 1 & CHOICE_AREA == "SUBBASIN" ~ wq_sub_basin_policy,
           TRUE ~ 0),
         
         WQ_POLICY_NL_CHOICE = case_when(
           LOCAL_CHOICE == 0 & CHOICE_AREA == "BASIN" ~ POLICY_AVERAGE,
           LOCAL_CHOICE == 0 & CHOICE_AREA == "SUBBASIN" ~ wq_sub_basin_policy,
           TRUE ~ 0),
         
         
         WQ_POLICY_CHOICE = case_when(
           LOCAL_CHOICE == 1 & CHOICE_AREA == "BASIN" ~ POLICY_AVERAGE,
           LOCAL_CHOICE == 1 & CHOICE_AREA == "SUBBASIN" ~ wq_sub_basin_policy,
           LOCAL_CHOICE == 0 & CHOICE_AREA == "BASIN" ~ POLICY_AVERAGE,
           LOCAL_CHOICE == 0 & CHOICE_AREA == "SUBBASIN" ~ wq_sub_basin_policy,
           TRUE ~ 0),
         
         
         SQ_VARIATION_LOCAL_CHOICE = case_when(
           LOCAL_CHOICE == 1 & CHOICE_AREA == "BASIN" ~ BASELINE_WQ_VARIATION,
           LOCAL_CHOICE == 1 & CHOICE_AREA == "SUBBASIN" ~ BASELINE_WQ_VARIATION,
           TRUE ~ 0),
         
         SQ_VARIATION_NONLOCAL_CHOICE = case_when(
           LOCAL_CHOICE == 0 & CHOICE_AREA == "BASIN" ~ BASELINE_WQ_VARIATION,
           LOCAL_CHOICE == 0 & CHOICE_AREA == "SUBBASIN" ~ BASELINE_WQ_VARIATION,
           TRUE ~ 0),
         
         
         SQ_VARIATION_CHOICE = case_when(
           LOCAL_CHOICE == 1 & CHOICE_AREA == "BASIN" ~ BASELINE_WQ_VARIATION,
           LOCAL_CHOICE == 1 & CHOICE_AREA == "SUBBASIN" ~ BASELINE_WQ_VARIATION,
           LOCAL_CHOICE == 0 & CHOICE_AREA == "BASIN" ~ BASELINE_WQ_VARIATION,
           LOCAL_CHOICE == 0 & CHOICE_AREA == "SUBBASIN" ~ BASELINE_WQ_VARIATION,
           TRUE ~ 0),
         
         
  )



t <- df_final %>%
  select(CaseId, VERSION, BLK_NUMBER, CHOICE_NUMBER, WQ_CURRENT_CHOICE) %>%
  drop_na()%>%
  distinct(BLK_NUMBER, CHOICE_NUMBER, VERSION, .keep_all = TRUE) %>%
  group_by(BLK_NUMBER) %>%
  mutate(no_obs_var = as.integer(n_distinct(WQ_CURRENT_CHOICE) == 1)) %>%
  ungroup()%>%
  select(VERSION,BLK_NUMBER, CHOICE_NUMBER,no_obs_var)

df_final <- df_final%>%
  left_join(t)%>%
  mutate(
    WQ_CURRENT_LOCAL_CHOICE_OBS = case_when(
      LOCAL_CHOICE == 1 & CHOICE_AREA == "BASIN" & no_obs_var == 0 ~ CURRENT_AVERAGE,
      LOCAL_CHOICE == 1 & CHOICE_AREA == "SUBBASIN" & no_obs_var == 0 ~ wq_sub_basin_current,
      TRUE ~ 0),
    
    WQ_CURRENT_NL_CHOICE_OBS = case_when(
      LOCAL_CHOICE == 0 & CHOICE_AREA == "BASIN" & no_obs_var == 0 ~ CURRENT_AVERAGE,
      LOCAL_CHOICE == 0 & CHOICE_AREA == "SUBBASIN" & no_obs_var == 0 ~ wq_sub_basin_current,
      TRUE ~ 0),
    
    WQ_CURRENT_CHOICE_OBS = case_when(
      LOCAL_CHOICE == 1 & CHOICE_AREA == "BASIN" & no_obs_var == 0 ~ CURRENT_AVERAGE,
      LOCAL_CHOICE == 1 & CHOICE_AREA == "SUBBASIN" & no_obs_var == 0 ~ wq_sub_basin_current,
      LOCAL_CHOICE == 0 & CHOICE_AREA == "BASIN" & no_obs_var == 0 ~ CURRENT_AVERAGE,
      LOCAL_CHOICE == 0 & CHOICE_AREA == "SUBBASIN" & no_obs_var == 0 ~ wq_sub_basin_current,
      TRUE ~ 0),
    
    
    WQ_POLICY_LOCAL_CHOICE_OBS = case_when(
      LOCAL_CHOICE == 1 & CHOICE_AREA == "BASIN" & no_obs_var == 0 ~ CURRENT_AVERAGE,
      LOCAL_CHOICE == 1 & CHOICE_AREA == "SUBBASIN" & no_obs_var == 0 ~ wq_sub_basin_policy,
      TRUE ~ 0),
    
    WQ_POLICY_NL_CHOICE_OBS = case_when(
      LOCAL_CHOICE == 0 & CHOICE_AREA == "BASIN" & no_obs_var == 0 ~ CURRENT_AVERAGE,
      LOCAL_CHOICE == 0 & CHOICE_AREA == "SUBBASIN" & no_obs_var == 0 ~ wq_sub_basin_policy,
      TRUE ~ 0),
    
    
    WQ_POLICY_CHOICE_OBS = case_when(
      LOCAL_CHOICE == 1 & CHOICE_AREA == "BASIN" & no_obs_var == 0 ~ POLICY_AVERAGE,
      LOCAL_CHOICE == 1 & CHOICE_AREA == "SUBBASIN" & no_obs_var == 0 ~ wq_sub_basin_policy,
      LOCAL_CHOICE == 0 & CHOICE_AREA == "BASIN" & no_obs_var == 0 ~ POLICY_AVERAGE,
      LOCAL_CHOICE == 0 & CHOICE_AREA == "SUBBASIN" & no_obs_var == 0 ~ wq_sub_basin_policy,
      TRUE ~ 0)
  )


df_final <- df_final%>%
  mutate(
    CURRENT_SUB_1 = case_when(CHOICE_AREA == "SUBBASIN" & CHOICE_SUB_BASIN == 1 ~ WQ_CURRENT_CHOICE, 
                              TRUE ~ 0),
    CURRENT_SUB_2 = case_when(CHOICE_AREA == "SUBBASIN" & CHOICE_SUB_BASIN == 2 ~ WQ_CURRENT_CHOICE,
                              TRUE ~ 0),
    CURRENT_SUB_3 = case_when(CHOICE_AREA == "SUBBASIN" & CHOICE_SUB_BASIN == 3 ~ WQ_CURRENT_CHOICE,
                              TRUE ~ 0),
    CURRENT_SUB_4 = case_when(CHOICE_AREA == "SUBBASIN" & CHOICE_SUB_BASIN == 4 ~ WQ_CURRENT_CHOICE,
                              TRUE ~ 0),
    CURRENT_SUB_5 = case_when(CHOICE_AREA == "SUBBASIN" & CHOICE_SUB_BASIN == 5 ~ WQ_CURRENT_CHOICE,
                              TRUE ~ 0),
    CURRENT_SUB_6 = case_when(CHOICE_AREA == "SUBBASIN" & CHOICE_SUB_BASIN == 6 ~ WQ_CURRENT_CHOICE,
                              TRUE ~ 0),
    CURRENT_SUB_7 = case_when(CHOICE_AREA == "SUBBASIN" & CHOICE_SUB_BASIN == 7 ~ WQ_CURRENT_CHOICE,
                              TRUE ~ 0),
    CURRENT_SUB_8 = case_when(CHOICE_AREA == "SUBBASIN" & CHOICE_SUB_BASIN == 8 ~ WQ_CURRENT_CHOICE,
                              TRUE ~ 0),
    CURRENT_SUB_9 = case_when(CHOICE_AREA == "SUBBASIN" & CHOICE_SUB_BASIN == 9 ~ WQ_CURRENT_CHOICE,
                              TRUE ~ 0),
    CURRENT_SUB_10 = case_when(CHOICE_AREA == "SUBBASIN" & CHOICE_SUB_BASIN == 10 ~ WQ_CURRENT_CHOICE,
                               TRUE ~ 0),
    CURRENT_SUB_11 = case_when(CHOICE_AREA == "SUBBASIN" & CHOICE_SUB_BASIN == 11 ~ WQ_CURRENT_CHOICE,
                               TRUE ~ 0),
    CURRENT_SUB_12 = case_when(CHOICE_AREA == "SUBBASIN" & CHOICE_SUB_BASIN == 12 ~ WQ_CURRENT_CHOICE,
                               TRUE ~ 0),
    CURRENT_SUB_13 = case_when(CHOICE_AREA == "SUBBASIN" & CHOICE_SUB_BASIN == 13 ~ WQ_CURRENT_CHOICE,
                               TRUE ~ 0),
    CURRENT_SUB_14 = case_when(CHOICE_AREA == "SUBBASIN" & CHOICE_SUB_BASIN == 14 ~ WQ_CURRENT_CHOICE,
                               TRUE ~ 0),
    CURRENT_SUB_15 = case_when(CHOICE_AREA == "SUBBASIN" & CHOICE_SUB_BASIN == 15 ~ WQ_CURRENT_CHOICE,
                               TRUE ~ 0),
    CURRENT_SUB_16 = case_when(CHOICE_AREA == "SUBBASIN" & CHOICE_SUB_BASIN == 16 ~ WQ_CURRENT_CHOICE,
                               TRUE ~ 0),
    CURRENT_SUB_17 = case_when(CHOICE_AREA == "SUBBASIN" & CHOICE_SUB_BASIN == 17 ~ WQ_CURRENT_CHOICE,
                               TRUE ~ 0),
    CURRENT_SUB_18 = case_when(CHOICE_AREA == "SUBBASIN" & CHOICE_SUB_BASIN == 18 ~ WQ_CURRENT_CHOICE,
                               TRUE ~ 0),
    
    CURRENT_BASIN_1 = case_when(CHOICE_AREA == "BASIN" & CHOICE_BASIN == 1 ~ WQ_CURRENT_CHOICE,
                                TRUE ~ 0),
    CURRENT_BASIN_2 = case_when(CHOICE_AREA == "BASIN" & CHOICE_BASIN == 2 ~ WQ_CURRENT_CHOICE,
                                TRUE ~ 0),
    CURRENT_BASIN_3 = case_when(CHOICE_AREA == "BASIN" & CHOICE_BASIN == 3 ~ WQ_CURRENT_CHOICE,
                                TRUE ~ 0),
    CURRENT_BASIN_4 = case_when(CHOICE_AREA == "BASIN" & CHOICE_BASIN == 4 ~ WQ_CURRENT_CHOICE,
                                TRUE ~ 0))%>%
  
  mutate(
    POLICY_SUB_1 = case_when(CHOICE_AREA == "SUBBASIN" & CHOICE_SUB_BASIN == 1 ~ WQ_POLICY_CHOICE, 
                              TRUE ~ 0),
    POLICY_SUB_2 = case_when(CHOICE_AREA == "SUBBASIN" & CHOICE_SUB_BASIN == 2 ~ WQ_POLICY_CHOICE,
                              TRUE ~ 0),
    POLICY_SUB_3 = case_when(CHOICE_AREA == "SUBBASIN" & CHOICE_SUB_BASIN == 3 ~ WQ_POLICY_CHOICE,
                              TRUE ~ 0),
    POLICY_SUB_4 = case_when(CHOICE_AREA == "SUBBASIN" & CHOICE_SUB_BASIN == 4 ~ WQ_POLICY_CHOICE,
                              TRUE ~ 0),
    POLICY_SUB_5 = case_when(CHOICE_AREA == "SUBBASIN" & CHOICE_SUB_BASIN == 5 ~ WQ_POLICY_CHOICE,
                              TRUE ~ 0),
    POLICY_SUB_6 = case_when(CHOICE_AREA == "SUBBASIN" & CHOICE_SUB_BASIN == 6 ~ WQ_POLICY_CHOICE,
                              TRUE ~ 0),
    POLICY_SUB_7 = case_when(CHOICE_AREA == "SUBBASIN" & CHOICE_SUB_BASIN == 7 ~ WQ_POLICY_CHOICE,
                              TRUE ~ 0),
    POLICY_SUB_8 = case_when(CHOICE_AREA == "SUBBASIN" & CHOICE_SUB_BASIN == 8 ~ WQ_POLICY_CHOICE,
                              TRUE ~ 0),
    POLICY_SUB_9 = case_when(CHOICE_AREA == "SUBBASIN" & CHOICE_SUB_BASIN == 9 ~ WQ_POLICY_CHOICE,
                              TRUE ~ 0),
    POLICY_SUB_10 = case_when(CHOICE_AREA == "SUBBASIN" & CHOICE_SUB_BASIN == 10 ~ WQ_POLICY_CHOICE,
                               TRUE ~ 0),
    POLICY_SUB_11 = case_when(CHOICE_AREA == "SUBBASIN" & CHOICE_SUB_BASIN == 11 ~ WQ_POLICY_CHOICE,
                               TRUE ~ 0),
    POLICY_SUB_12 = case_when(CHOICE_AREA == "SUBBASIN" & CHOICE_SUB_BASIN == 12 ~ WQ_POLICY_CHOICE,
                               TRUE ~ 0),
    POLICY_SUB_13 = case_when(CHOICE_AREA == "SUBBASIN" & CHOICE_SUB_BASIN == 13 ~ WQ_POLICY_CHOICE,
                               TRUE ~ 0),
    POLICY_SUB_14 = case_when(CHOICE_AREA == "SUBBASIN" & CHOICE_SUB_BASIN == 14 ~ WQ_POLICY_CHOICE,
                               TRUE ~ 0),
    POLICY_SUB_15 = case_when(CHOICE_AREA == "SUBBASIN" & CHOICE_SUB_BASIN == 15 ~ WQ_POLICY_CHOICE,
                               TRUE ~ 0),
    POLICY_SUB_16 = case_when(CHOICE_AREA == "SUBBASIN" & CHOICE_SUB_BASIN == 16 ~ WQ_POLICY_CHOICE,
                               TRUE ~ 0),
    POLICY_SUB_17 = case_when(CHOICE_AREA == "SUBBASIN" & CHOICE_SUB_BASIN == 17 ~ WQ_POLICY_CHOICE,
                               TRUE ~ 0),
    POLICY_SUB_18 = case_when(CHOICE_AREA == "SUBBASIN" & CHOICE_SUB_BASIN == 18 ~ WQ_POLICY_CHOICE,
                               TRUE ~ 0),
    
    POLICY_BASIN_1 = case_when(CHOICE_AREA == "BASIN" & CHOICE_BASIN == 1 ~ WQ_POLICY_CHOICE,
                                TRUE ~ 0),
    POLICY_BASIN_2 = case_when(CHOICE_AREA == "BASIN" & CHOICE_BASIN == 2 ~ WQ_POLICY_CHOICE,
                                TRUE ~ 0),
    POLICY_BASIN_3 = case_when(CHOICE_AREA == "BASIN" & CHOICE_BASIN == 3 ~ WQ_POLICY_CHOICE,
                                TRUE ~ 0),
    POLICY_BASIN_4 = case_when(CHOICE_AREA == "BASIN" & CHOICE_BASIN == 4 ~ WQ_POLICY_CHOICE,
                                TRUE ~ 0))


###############################################################################
# REMOVED: a verbatim recomputation of the spatial neighbour lists.
#
# This block re-read study_area_map_with_WQ.shp, re-ran poly2nb() twice and
# rebuilt df_subbasin / df_basin - all of which lines 847-977 above already
# produce. Nothing after this point read the second copy: df_subbasin and
# df_basin are consumed at lines 963 and 977, above, and the code below uses
# the hardcoded ADJ tables instead. Cost 5.7s per run.
#
# LOCAL_ADJUCENT, used further down, is computed at lines 972-996 and carried
# in df_final - it does not depend on anything that was here.
###############################################################################



df_temp_nonlocal_basin <- df_final %>%
  #select(CaseId, BASIN, SUB_BASIN, LOCAL_CHOICE, CHOICE_BASIN, CHOICE_SUB_BASIN, CHOICE_AREA) %>%
  filter(LOCAL_CHOICE == 0, CHOICE_AREA == "BASIN") %>%
  mutate(
    ADJ = case_when(
      BASIN == 1 & CHOICE_BASIN %in% c(2, 4) ~ 1,
      BASIN == 2 & CHOICE_BASIN %in% c(1, 3, 4) ~ 1,
      BASIN == 3 & CHOICE_BASIN %in% c(2, 4) ~ 1,
      BASIN == 4 & CHOICE_BASIN %in% c(1, 2, 3) ~ 1,
      TRUE ~ 0
    )
  )


df_temp_nonlocal_subbasin <- df_final %>%
  #select(CaseId, BASIN, SUB_BASIN, LOCAL_CHOICE, CHOICE_BASIN, CHOICE_SUB_BASIN, CHOICE_AREA) %>%
  filter(LOCAL_CHOICE == 0, CHOICE_AREA == "SUBBASIN") %>%
  mutate(
    ADJ = case_when(
      SUB_BASIN == 1 & CHOICE_SUB_BASIN %in% c(7, 9, 2, 3, 4) ~ 1,
      SUB_BASIN == 2 & CHOICE_SUB_BASIN %in% c(16, 9, 1, 4) ~ 1,
      SUB_BASIN == 3 & CHOICE_SUB_BASIN %in% c(10, 7, 1, 4) ~ 1,
      SUB_BASIN == 4 & CHOICE_SUB_BASIN %in% c(1, 2, 3) ~ 1,
      SUB_BASIN == 5 & CHOICE_SUB_BASIN %in% c(8) ~ 1,
      SUB_BASIN == 6 & CHOICE_SUB_BASIN %in% c(9, 8) ~ 1,
      SUB_BASIN == 7 & CHOICE_SUB_BASIN %in% c(10, 9, 1, 3) ~ 1,
      SUB_BASIN == 8 & CHOICE_SUB_BASIN %in% c(10, 5, 9, 6) ~ 1,
      SUB_BASIN == 9 & CHOICE_SUB_BASIN %in% c(16, 13, 10, 7, 8, 6, 1, 2) ~ 1,
      SUB_BASIN == 10 & CHOICE_SUB_BASIN %in% c(7, 9, 8, 3) ~ 1,
      SUB_BASIN == 11 & CHOICE_SUB_BASIN %in% c(17, 13, 14, 12) ~ 1,
      SUB_BASIN == 12 & CHOICE_SUB_BASIN %in% c(13, 11, 14) ~ 1,
      SUB_BASIN == 13 & CHOICE_SUB_BASIN %in% c(16, 17, 11, 12, 9) ~ 1,
      SUB_BASIN == 14 & CHOICE_SUB_BASIN %in% c(17, 15, 11, 12) ~ 1,
      SUB_BASIN == 15 & CHOICE_SUB_BASIN %in% c(18, 17, 14) ~ 1,
      SUB_BASIN == 16 & CHOICE_SUB_BASIN %in% c(18, 17, 13, 9, 2) ~ 1,
      SUB_BASIN == 17 & CHOICE_SUB_BASIN %in% c(18, 16, 15, 13, 11, 14) ~ 1,
      SUB_BASIN == 18 & CHOICE_SUB_BASIN %in% c(16, 17, 15) ~ 1,
      
      TRUE ~ 0
    )
  )


df_temp_local <- df_final%>%
  filter(LOCAL_CHOICE == 1)%>%
  mutate(ADJ = 0)


df_temp_nonlocal <- rbind(df_temp_nonlocal_basin,df_temp_nonlocal_subbasin)



df_final <- rbind(df_temp_local,df_temp_nonlocal)



df_final <- df_final%>%
  mutate(NON_LOCAL_ADJUCENT_LOCAL_BASIN = ifelse(CHOICE_AREA == "BASIN" & LOCAL_CHOICE == 0 & 
                                                   LOCAL_ADJUCENT == 1, 1,0 ))%>%
  mutate(NON_LOCAL_ADJUCENT_LOCAL_SUBBASIN = ifelse(CHOICE_AREA == "SUBBASIN" & LOCAL_CHOICE == 0 & 
                                                      LOCAL_ADJUCENT == 1, 1,0 ))%>%
  mutate(NON_LOCAL_NOT_ADJUCENT_LOCAL_BASIN = ifelse(CHOICE_AREA == "BASIN" & LOCAL_CHOICE == 0 &
                                                       LOCAL_ADJUCENT == 0, 1,0))%>%
  mutate(NON_LOCAL_NOT_ADJUCENT_LOCAL_SUBBASIN = ifelse(CHOICE_AREA == "SUBBASIN" & LOCAL_CHOICE == 0 & 
                                                          LOCAL_ADJUCENT == 0, 1,0 ))



df_final <- df_final%>%
  mutate(WQ_NON_LOCAL_ADJUCENT_LOCAL_BASIN_CURRENT = ifelse(NON_LOCAL_ADJUCENT_LOCAL_BASIN ==1,WQ_BASIN_NL_CURRENT_N,0),
         WQ_NON_LOCAL_ADJUCENT_LOCAL_SUBBASIN_CURRENT = ifelse(NON_LOCAL_ADJUCENT_LOCAL_SUBBASIN ==1,WQ_SUBBASIN_NL_CURRENT_SUBONLY_N,0),
         WQ_NON_LOCAL_ADJUCENT_LOCAL_BASIN_POLICY = ifelse(NON_LOCAL_ADJUCENT_LOCAL_BASIN ==1,WQ_BASIN_NL_POLICY_N,0),
         WQ_NON_LOCAL_ADJUCENT_LOCAL_SUBBASIN_POLCIY = ifelse(NON_LOCAL_ADJUCENT_LOCAL_SUBBASIN ==1,WQ_SUBBASIN_NL_POLICY_SUBONLY_N,0),
         
         WQ_NON_LOCAL_NOT_ADJUCENT_LOCAL_BASIN_CURRENT = ifelse(NON_LOCAL_NOT_ADJUCENT_LOCAL_BASIN ==0,WQ_BASIN_NL_CURRENT_N,0),
         WQ_NON_LOCAL_NOT_ADJUCENT_LOCAL_SUBBASIN_CURRENT = ifelse(NON_LOCAL_NOT_ADJUCENT_LOCAL_SUBBASIN ==0,WQ_SUBBASIN_NL_CURRENT_SUBONLY_N,0),
         WQ_NON_LOCAL_NOT_ADJUCENT_LOCAL_BASIN_POLICY = ifelse(NON_LOCAL_NOT_ADJUCENT_LOCAL_BASIN ==0,WQ_BASIN_NL_POLICY_N,0),
         WQ_NON_LOCAL_NOT_ADJUCENT_LOCAL_SUBBASIN_POLCIY = ifelse(NON_LOCAL_NOT_ADJUCENT_LOCAL_SUBBASIN ==0,WQ_SUBBASIN_NL_POLICY_SUBONLY_N,0),
         
         
  )




write.csv(df_final, "data/derived/test/processed_finaldata_batch_1_Apollo.csv",row.names = FALSE)
