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

#unique id from other 
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

### enumate functions
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
        {{ col }} %in% c( NA, "Not on file") ~ NA,
        {{ col }} == "No" ~ "0",
        {{ col }} == "Yes" ~ "1"
      ))
}
### dont use this one 
simple_diagnosis <- function(dataframe, col) {
  dataframe %>%
    mutate(
      simple_diag =case_when(
        {{ col }} %in% c("Mci") ~ "MCI",
        {{ col }} %in% c("vasc dem", "Vascular", "CVS", "vasc den", "vascDem", "vascular", "vMCI") ~ "vasc",
        {{ col }} %in% c("AD/vasc", "AD vasc") ~ "AD-vasc",
        {{ col }} %in% c("dem NOS", "dem", "dementia") ~ "demNOS",
        {{ col }} %in% c(".") ~ "BLANK",
        {{ col }} %in% c("DLB MCI", "MCI DLB") ~ "DLB-MCI",
        {{ col }} %in% c("Major neurocog") ~ "major neurocog",
        {{ col }} %in% c("PPA NOS", "PPA-U") ~ "PPA-NOS",
        {{ col }} %in% c("PPAOSx2", "PPAoS") ~ "PPAOS",
        {{ col }} %in% c("SNAP", "PART") ~ "SNAP/PART",
        {{ col }} %in% c("Hashimoto?") ~ "SREAT",# should SREAT be in the futher diagnosis layer
        {{ col }} %in% c("Wernicke-Korsakoff") ~ "Wernicke",
        {{ col }} %in% c("autism spectrum", "ASD") ~ "autism",
        {{ col }} %in% c("autoimmune enceph") ~ "autoimmune",
        {{ col }} %in% c("basilar migraine") ~ "migraine",
        {{ col }} %in% c("eoAD") ~ "eoAD",
        {{ col }} %in% c("hydrocephalus?") ~ "hydrocephalus",
        {{ col }} %in% c("major neucog", "major neurocog, bipolar", "major neurocognitive disorder", "major neuropsych" 
                         ) ~  "major neurocog",
        {{ col }} %in% c("minor neurocognitive disorder") ~ "minor neurocog",
        {{ col }} %in% c("rtFTD","rsvPPA", "rtvFTD" ) ~ "sbvFTD",
        {{ col }} %in% c("subjective cognitive concerns", "subjective cognitive impairment") ~ "SCI",
        {{ col }} %in% c("DS-AD") ~ "DS",
        {{ col }} %in% c("LBD" ) ~ "PD",
        {{ col }} %in% c("PD-SCI") ~  "PD-MCI",
        {{ col }} %in% c("FND", "FTD phenocopy", "FTD-phenocopy", "FTD phenocopy, ASD", "phenocopy", "bipolar",
                         "schizophrenia") ~ "psych",
        {{ col }} %in% c("FTD", "PLS-FTD", "FTD-MND","PGRN","bvFTD-SOD1", "sbvFTD" ) ~ "bvFTD",
        {{ col }} %in% c("PSP?") ~ "PSP",
        {{ col }} %in% c("anger/TBI") ~ "TBI",
        .default =  NA
      ))
        
}
 #group dignosis into categories
further_diagnosis <- function(dataframe, col) {
  dataframe %>%
    mutate(
      further_diag = case_when( {{ col }} %in% c("autism","autism spectrum","ADHD", "cerebral palsy", "intellectual deficit", "intellectual disability",
                                            "DS", "dev", "DS-AD", "developmental", "ASD") ~ "developmental",
                           {{ col }} %in% c("PCA", "AD-lang", "AD-vasc" , "eoAD", "AD", "dementia,Alzheimer's disease") ~ "AD",
                           {{ col }} %in% c("post-concussive syndrome", "TBI") ~ "PCS",
                           {{ col }} %in% c("aMCI-PSEN1", "aMCI", "eMCI","lMCI", "mMCI", "mild cognitive impairment", "minor neurocog",
                                            "minor neurocognitive disorder", "Minor" ) ~ "MCI",# put Minor
                           {{ col }} %in% c("lvPPA", "svPPA","nfvPPA", "PPA-NOS", "PPA PGRN", "PPAOS", "lvPPA(+)", "lvPPA/CBS", 
                                            "PPA-U" ) ~ "PPA",
                           {{ col }} %in% c("B12","CAA", "SCA", "CCAS", "FBSS","Fahr","HAND", "Hashimoto?", "MS","PART","Susacs",
                                            "NPH", "PRES", "Rasmussen","TGA", "TIA", "Wernicke","Wilsons", "X-ALD","abnormal MRI",
                                            "abscess", "anoxia", "autoimmune", "migraine", "encephalitis", "encephalopathy", "epilepsy",
                                            "foot pain", "grief", "hallucination", "hepatic", "hydrocephalus", "leukoencephalopathy","hydrocephalus?",
                                            "numbness", "phantosmia", "procedure", "pseudotumor", "seizure", "spells","stroke", "surg",
                                            "speech apraxia", "syncope", "tingling", "tox/met", "tremor", "weakness", "CJD", "SNAP/PART","SREAT","SNAP",
                                            "IIH", "autoimmune enceph") ~ "other",  # Wernicke and Hashimoto? is written multiple times "SREAT" is not handled so its being put here
                           {{ col }} %in% c("TBI", "anger/TBI") ~ "PCS",
                           {{ col }} %in% c("PD", "PDD", "DLBMCI", "DLB-MCI", "PD-MCI", "DLB", "DLB MCI", "MCI DLB", "PD-SCI" ) ~  "LBD",
                           {{ col }} %in% c("SCI","subjective cognitive impairment") ~ "SCI",
                           {{ col }} %in% c("eoAD","eoaD") ~ "eoAD",
                           {{ col }} %in% c("major neurocog", "major neurocognitive disorder", "major", "major neucog", "major neurocog, bipolar",
                                            "major neuropsych", "Major", "Major neurocog") ~"major neurocog", # i put 'major' under the
                           {{ col }} == "PCS" ~ "PCS",
                           {{ col }} == "MCI" ~ "MCI",
                           {{ col }} == "bvFTD" ~ "bvFTD",
                           {{ col }}%in% c("vasc","vasc dem", "AD/vasc", "vascular", "Vascular", "vascDem", "vMCI", "vasc den", "CVS", "AD vasc" ) ~ "vasc",
                           {{ col }} %in% c("LBD", "dementia,Lewy" ) ~ "LBD",
                           {{ col }}%in% c("dementia","demNOS","dem NOS", "DEM NOS", "dem" ) ~ "demNOS", #put dem here
                           {{ col }} %in% c("FND", "FTD phenocopy", "FTD-phenocopy", "FTD phenocopy, ASD", "phenocopy", "bipolar",
                                            "schizophrenia", "psych") ~ "psych",
                           {{ col }} %in% c("FTD", "PLS-FTD", "FTD-MND","PGRN","bvFTD-SOD1", "sbvFTD", "rtFTD","rsvPPA", "rtvFTD",
                                            "dementia,frontotemporal dementia","rfvFTD" ) ~ "bvFTD", #dementia,frontotemporal dementia was put here as not being adjusted for
                           {{ col }} %in% c("lvPPA", "svPPA","nfvPPA", "PPA-NOS", "PPA PGRN", "PPAOS", "lvPPA(+)", "lvPPA/CBS", 
                                           "PPA-U", "PPA", "PPAoS", "PPA NOS", "PPA-PGRN", "PPAOSx2" ) ~ "PPA",
                           {{ col }} == "CBS" ~ "CBS",
                           {{ col }} %in% c("PSP?", "PSP") ~ "PSP",
                           {{ col }} %in% c("HC") ~ "HC",
                           .default =  NA
      ))
}
#enumarate these categories
enumerate_diagnosis <- function(dataframe, col) {
  dataframe %>%
    mutate(
      num_diagnosis = case_when(
        {{ col }} == "AD" ~ 0,
        {{ col }} == "LBD"  ~ 1,
        {{ col }} == "MCI" ~ 2,
        {{ col }} == "PCS" ~ 3,
        {{ col }} == "PPA" ~ 4,
        {{ col }} == "SCI" ~ 5,
        {{ col }} == "bvFTD" ~ 6,
        {{ col }} == "demNOS" ~ 7,
        {{ col }} == "." ~ 8, #ask peter
        {{ col }} == "eoAD" ~ 9,
        {{ col }} == "major neurocog" ~ 10,
        {{ col }} == "minor neurocog" ~ 11,
        {{ col }} == "other"  ~ 12,
        {{ col }} == "psych" ~ 13,
        {{ col }} == "vasc" ~ 14,
        {{ col }} == "developmental" ~ 15,
        {{ col }} == "PSP" ~ 16,
        {{ col }} == "CBS" ~ 17,
        {{ col }} == "HC" ~ 18,
        .default =  NA
        
    ))
}
#fill in group
fill_in <- function(dataframe, col, id){
  dataframe %>%
    group_by({{id}}) %>%
    fill({{col}} , .direction = "downup") %>%
    ungroup()
}

age_range <- function(dataframe, col) {
  dataframe %>%
    mutate(
      age_group = case_when(
    as.integer({{ col }}) >= 0  & as.integer({{ col }}) <= 17 ~ "0-17",
    as.integer({{ col }}) >= 18 & as.integer({{ col }}) <= 34 ~ "18-34",
    as.integer({{ col }}) >= 35 & as.integer({{ col }}) <= 49 ~ "35-49",
    as.integer({{ col }}) >= 50 & as.integer({{ col }}) <= 64 ~ "50-64",
    as.integer({{ col }}) >= 65 & as.integer({{ col }}) <= 79 ~ "65-79",
    as.integer({{ col }}) >= 80 & as.integer({{ col }}) <= 94 ~ "80-94",
    as.integer({{ col }}) >= 95  ~ "95+",
    .default =  NA
  ))
}

