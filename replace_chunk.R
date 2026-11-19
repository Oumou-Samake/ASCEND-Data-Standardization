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
replace_gender_peter <- function(dataframe, col) {
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

replace_race_peter <- function(dataframe, col) {
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

replace_eth_peter <- function(dataframe, col) {
  dataframe %>%
    mutate(
      {{ col }} := case_when(
        {{ col }} == "Non-Hispanic [1]"  ~ "0",
        {{ col }} == "Hispanic [2]" ~ "1",
        {{ col }} %in% c( NA, "Unknown [3]",  "Patient Refused [4]")  ~ "2"
      ))
}

replace_medicaid_peter <- function(dataframe, col) {
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

replace_gender <- function(dataframe, col) {
  dataframe %>%
    mutate(
      {{ col }} := factor(
        case_when(
          is.na({{ col }}) ~ NA_character_,
          {{ col }} %in% c("F", "Female") ~ "Female",
          {{ col }} %in% c("M", "Male") ~ "Male",
          TRUE ~ NA_character_
        ),
        levels = c("Female", "Male")
      )
    )
}

replace_race <- function(dataframe, col) {
  dataframe %>%
    mutate({{ col }} := factor(
      case_when(
      {{ col }}  %in% c("White or Caucasian", "White")  ~ "White",
      {{ col }} == "Black or African American" ~ "Black",
      {{ col }} == "American Indian and Alaska Native" ~ "American Indian and Alaska Native",
      {{ col }} == "Asian" ~"Asian",
      {{ col }}  %in% c("Native Hawaiian and Other Pacific Islander", "Other Pacific Islander") ~ "Native Hawaiian and Other Pacific Islander",
      {{ col }} == "Other"  ~ "Other",
      {{ col }}  %in% c( NA, "Unknown", "Patient Refused")  ~ "Missing Value"
    ),
    levels = c("White", "Black","American Indian and Alaska Native","Asian","Native Hawaiian and Other Pacific Islander", "Other","Missing Value" )))
  # missing "More Than One Race"
}

replace_eth <- function(dataframe, col) {
  dataframe %>%
    mutate(
      {{ col }} := factor( case_when(
        {{ col }} == "Non-Hispanic [1]"  ~ "Non-Hispanic",
        {{ col }} == "Hispanic [2]" ~ "Hispanic",
        {{ col }} %in% c( NA, "Unknown [3]",  "Patient Refused [4]")  ~ "Missing Value"
      ),
      levels = c("Non-Hispanic", "Hispanic", "Missing Value" )))}

replace_medicaid <- function(dataframe, col) {
  dataframe %>%
    mutate(
      {{ col }} := factor( case_when(
        {{ col }} %in% c( NA, "Not on file") ~"Missing Value",
        {{ col }} == "No" ~ "No",
        {{ col }} == "Yes" ~"Yes"
      ),
      levels = c("No", "Yes", "Missing Value" )))}

unpack_crampingtexs <- function(input_df, crampingtexs_col_name) {
  # Three steps to unpacking this dense column:
  # 1. separate into new columns, keeping around the original column
  # 2. recode to human-readable labels
  # 3. turn into factors
  
  {{input_df}} %>% separate_wider_position({{crampingtexs_col_name}}, 
                                       c(
                                         C_Computer=3, 
                                         R_Diarization=3, 
                                         A_Anonymization=3, 
                                         M_Microphone=3, 
                                         P_Program=3, 
                                         I_Identification_stage=3, 
                                         N_Noise_reduction=3, 
                                         G_Granularity=3,
                                         T_Task=3, 
                                         E_Environment=3, 
                                         X_Extraction_toolkit=3, 
                                         S_Speaker_identity=3
                                       ),
                                       cols_remove=FALSE # keep the original crampingtexs column around in case we need it for joining, comparing, etc.
  ) %>% 
    mutate(
      C_Computer = case_when(
      C_Computer == 'Cma' ~ "MAC",
      C_Computer == 'Cpc' ~ "PC",
      C_Computer == 'Cuk' ~ "Unknown"
      ),
      R_Diarization = case_when(
      R_Diarization == 'Rno' ~ "none",
      R_Diarization == 'Rpa' ~ "pyannote",
      R_Diarization == 'Rdl' ~ "DiarizationLM",
      R_Diarization == 'Rgs' ~ "gold standard"
      ),
      A_Anonymization = case_when(
      A_Anonymization == 'Aor' ~ "original"
      ),
      M_Microphone = case_when(
      M_Microphone == 'Mlv' ~ "lavalier",
      M_Microphone == 'Mbi' ~ "built-in"
      ),
      P_Program= case_when(
      P_Program == 'Pau' ~ "Audacity",
      P_Program == 'Pav' ~ "Avid",
      P_Program == 'Puk' ~ "unknown"
      ),
      I_Identification_stage = case_when(
      I_Identification_stage == 'Iid' ~ "identified"
      ),
      N_Noise_reduction = case_when(
      N_Noise_reduction == 'Nor' ~ "none/original",
      N_Noise_reduction == 'Npi' ~ "Python denoised"
      ),
      G_Granularity = case_when(
      G_Granularity == 'Gag' ~ "none/original",
      G_Granularity %in%  c('Gtr', "Gwi", "Gfr") ~ "reserved"
      ),
      T_Task = case_when(
      T_Task == 'Tgf' ~ "grandfather",
      T_Task == 'Tss' ~ "spontaneous speech",
      T_Task == 'Tpi' ~ "picnic description",
      T_Task == 'Tcv' ~ "conflict conversation",
      T_Task == 'Tme' ~ "motor speech evaluation",
      T_Task == 'Tuk' ~ "unknown"
      ),
      E_Environment= case_when(
      E_Environment == 'Ecl' ~ "clinical",
      E_Environment == 'Ers' ~ "research",
      E_Environment == 'Eli' ~ "LIIA"
      ),
      X_Extraction_toolkit = case_when(
      X_Extraction_toolkit == 'Xcp' ~ "ComParE",
      X_Extraction_toolkit == 'Xeg' ~ "eGeMAPS",
      X_Extraction_toolkit == 'Xpm' ~ "Parselmouth",
      X_Extraction_toolkit == 'Xwx' ~ "WhisperX",
      X_Extraction_toolkit == 'Xli' ~ "Librosa"
      ),
      S_Speaker_identity = case_when( 
        # SB note: Oumou, this is what you'd had; it doesn't align with what I'm seeing in the data so let's discuss
        # S_Speaker_identity == 'Sco' ~ "combined",
        # S_Speaker_identity == 'Ss' ~ "arbitrary diarization labels",
        # S_Speaker_identity == 'Sex' ~ "examiner",
        # S_Speaker_identity == 'Spt' ~ "participant",
        # S_Speaker_identity == 'Suk' ~ "unknown"
      S_Speaker_identity == "Sco" ~ "combined", # computed from entire recording, irrespective of which speaker was talking
      S_Speaker_identity == "Ss0" ~ "speaker_0", # computed from the portions of audio originating with the first speaker identified by the diarizer
      S_Speaker_identity == "Ss1" ~ "speaker_1", # computed from the _second_ speaker
      S_Speaker_identity == "Ss2" ~ "speaker_2", # third speaker
      S_Speaker_identity == "Ss3" ~ "speaker_3", # fourth speaker
      S_Speaker_identity == "Sex" ~ "examiner", # examiner speech
      S_Speaker_identity == "Spt" ~ "participant", # participant speech
      S_Speaker_identity == "Suk" ~ "unknown", # participant speech
      )) %>% 
    mutate(across(c(C_Computer, 
                    R_Diarization, 
                    A_Anonymization, 
                    M_Microphone, 
                    P_Program, 
                    I_Identification_stage, 
                    N_Noise_reduction,
                    G_Granularity,
                    T_Task,
                    E_Environment,
                    X_Extraction_toolkit,
                    S_Speaker_identity
    ), factor)) # turn into factors
  
}

new_duck_db <- function(nlp_table){
  nlp_pivoted <- {{ nlp_table }} %>% 
    collect %>% 
    pivot_longer(starts_with('nlp_'), names_to="raw.colname", values_to="value")
  nlp_unpacked <- nlp_pivoted %>% extract(col=raw.colname, 
                                        into=c("prefix","feature.name","crampingtexs.data"),
                                        regex="^([^_]+)_(.*)_([^_]+)$"
                                        ) %>% 
    unpack_crampingtexs(crampingtexs.data)
  dbWriteTable(con, "visits_nlp_tier1_pivoted_unpacked", nlp_unpacked)}
