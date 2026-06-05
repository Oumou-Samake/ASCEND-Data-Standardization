get_shape <- function(dataframe, col) {
  dataframe %>%
    mutate(
      shape = str_replace_all(as.character({{ col }}), "[[:alnum:]]", "9")
    )
}

sort_shape <- function(dataframe, col){
  dataframe %>%
    mutate(
      shape_num = case_when(
        {{ col }} == "***"  ~ "0",
        {{ col }} == "9-999-9-99-99"  ~ "1",
        {{ col }} == "9-999-99-99-99"  ~ "2",
        {{ col }} == "9-999-999999-99"  ~ "3",
        {{ col }} == "9-999-999999-999"  ~ "4",
        {{ col }} == "9999999999-9-9-99"  ~ "5",
        {{ col }} == "9999999999-9-99-99"  ~ "6",
        {{ col }} == "9999999999-99-9-99"  ~ "7",
        {{ col }} == "9999999999-99-99-99"  ~ "8",
        {{ col }} == "9999999999-9999999999999"  ~ "9",
      ) 
    )
}

extract_unique_id <- function(dataframe, col){
  dataframe %>%
    get_shape({{ col }}) %>% 
    sort_shape(shape) %>%
    mutate(
      #can change later with mutate
      new_id = case_when(
        shape_num %in% c("5", "6", "7", "8", "9")  ~ str_remove(visit_id , "-.*") , #insert fuction here,
        shape_num %in% c("1", "2", "3", "4") ~ substr(visit_id , start = 1, stop = 5), #insert fuction here,
      )
    )
  }

replace_gender <- function(dataframe, col) {
  dataframe %>%
    mutate(
      {{ col }} := case_when(
        {{ col }} == "NA" ~ ".",
        {{ col }} %in% c("F", "Female") ~ "1",
        {{ col }} %in% c("M", "Male") ~ "2",
        TRUE ~ NA_character_
      )
    )
}

replace_race <- function(dataframe, col) {
  dataframe %>%
    mutate({{ col }} := case_when(
      {{ col }}  %in% c("White or Caucasian", "White")  ~ "1",
      {{ col }} == "Black or African American" ~ "2",
      {{ col }} == "American Indian and Alaska Native" ~ "3",
      {{ col }} == "Asian" ~ "4",
      {{ col }}  %in% c("Native Hawaiian and Other Pacific Islander", "Other Pacific Islander") ~ "5",
      {{ col }} == "Other"  ~ "6",
      {{ col }}  %in% c( NA, "Unknown", "Patient Refused")  ~ "7"
    ))
  # missing "More Than One Race"
}

replace_eth <- function(dataframe, col) {
  dataframe %>%
    mutate(
      {{ col }} := case_when(
        {{ col }} == "Non-Hispanic [1]"  ~ "0",
        {{ col }} == "Hispanic [2]" ~ "1",
        {{ col }} %in% c( NA, "Unknown [3]",  "Patient Refused [4]")  ~ "2"
      ))
}

replace_medicaid <- function(dataframe, col) {
  dataframe %>%
    mutate(
      {{ col }} := case_when(
        {{ col }} %in% c( NA, "Not on file") ~ ".",
        {{ col }} == "No" ~ "0",
        {{ col }} == "Yes" ~ "1"
      ))
}

simple_diagnosis <- function(dataframe, col) {
  dataframe %>%
    mutate(
      DxGroup = case_when(
        {{ col }} %in% c("Mci") ~ "MCI",
        {{ col }} %in% c("vasc dem", "Vascular", "CVS", "vasc den", "vascDem", "vascular") ~ "vasc",
        {{ col }} %in% c("AD/vasc", "AD vasc") ~ "AD-vasc",
        {{ col }} %in% c("dem NOS") ~ "demNOS",
        {{ col }} %in% c(".") ~ "BLANK",
        {{ col }} %in% c("DLB MCI", "MCI DLB") ~ "DLB-MCI",
        {{ col }} %in% c("Major neurocog") ~ "major neurocog",
        {{ col }} %in% c("PPA NOS", "PPA-U") ~ "PPA-NOS",
        {{ col }} %in% c("PPAOSx2", "PPAoS") ~ "PPAOS",
        {{ col }} %in% c("SNAP", "PART") ~ "SNAP/PART",
      ))
}
