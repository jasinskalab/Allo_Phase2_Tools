source('AlloAlphabet.R')

MakeConstruct1 <- function(redcap_data) {
  literacy_vars <- french_literacy(redcap_data)
  cons1_vars <- literacy_vars[,c('letter_french','word_french','pseudo_french')]
  composite <- composite_index(cons1_vars,
                               control_key=redcap_data$redcap_event_name=='baseline_arm_1',
                               col_key=c(1,1,1))
  output_construct <- as.data.frame(cbind(redcap_data$record_id,composite,cons1_vars))
  names(output_construct)[1] <- 'record_id'
  return(output_construct)
}

MakeConstruct3 <- function(redcap_data) {
  phono_vars <- french_phoneme(redcap_data)
  vocab_vars <- french_vocab(redcap_data)
  oralcomp_vars <- french_oralcomp(redcap_data)
  cons3_vars <- cbind(phono_vars$phoneme_french,vocab_vars$vocab_french,oralcomp_vars$french_oralcomp_long)
  composite <- composite_index(cons3_vars,
                               control_key=redcap_data$redcap_event_name=='baseline_arm_1',
                               col_key=c(1,1,1))  
  output_construct <- as.data.frame(cbind(redcap_data$record_id,composite,cons3_vars))
  names(output_construct)[1] <- 'record_id'
  names(output_construct)[3:5] <- c('phoneme_french','vocab_french','french_oralcomp_long')
  return(output_construct)
}

MakeConstruct4 <- function(redcap_data) {
  allo_iso_data <- allo_isomorphic(redcap_data)
  # Since there is only one scalar measure to use in Construct 4,
  # we don't build a composite. However, the z-score of this measure
  # is still set according to the Control group mean/sd to be
  # consistent with the other constructs.
  mean_allo_control <- mean(allo_iso_data[redcap_data$redcap_event_name=='baseline_arm_1','allo_mini_score'],na.rm=TRUE)
  sd_allo_control <- sd(allo_iso_data[redcap_data$redcap_event_name=='baseline_arm_1','allo_mini_score'],na.rm=TRUE)
  allo_iso_data$allo_z_score <- (allo_iso_data$allo_mini_score - mean_allo_control) / sd_allo_control
  output_construct <- allo_iso_data[,c('record_id','allo_z_score','allo_mini_score')]
  return(output_construct)
}

MakeConstruct6 <- function(redcap_data) {
  ## Household language and literacy background
  # (1) binary measure of whether household adults speak French
  # (2) number of family adults speaking French
  # (3) number of family adults who read
  # (4) binary measure of whether household has "reading books" (livre du lecture, text books)
  # (5) binary measure of whether household has children's books (livre pour enfant)
  # (6) binary measure of whether household has other reading material (livres, journaux, ou autres choses à lire)
  # (7) scalar of 11 items for how adults support children's education
  
  french_vars <- family_french(redcap_data)
  book_vars <- book_at_home(redcap_data)
  reading_vars <- family_readers(redcap_data)
  educ_vars <- parent_involvedEducation(redcap_data)
  cons6_vars <- cbind(
    french_vars[,c('french_at_home','family_french_num')],
    reading_vars[,'family_readers_num'],
    book_vars[,c('book_at_home_lecture','book_at_home_enfant','book_at_home_autre')],
    educ_vars[,'parent_involvedEducation'])
  composite <- composite_index(cons6_vars,
                               control_key=redcap_data$redcap_event_name=='baseline_arm_1')  
  output_construct <- as.data.frame(cbind(redcap_data$record_id,composite,cons6_vars))
  names(output_construct)[1] <- 'record_id'
  names(output_construct)[5] <- 'family_readers_num'
  names(output_construct)[9] <- 'parent_involvedEducation'
  return(output_construct)
}

MakeConstruct7 <- function(redcap_data) {
  ## Socioeconomic Status
  # (1) Household object inventory (0-15) from EGRA
  # (2) Ordinal measure of parents' education, based on caretaker data or child data (if caretaker data not available)
  # (3) Binary measure of whether caretakers are farmers (this variable is REVERSE CODED in that we hypothesize TRUE/1 predicts of lower outcome scores)
  
  # Household object inventory
  ses_vars <- ses_inventory(redcap_data)
  
  # Ordinal parent education level (based on caretaker data where possible, supplemented with child data if caretaker data not available)
  educ_vars <- parent_educ(redcap_data)
  
  # Binary caretaker-is-farmer variable. If either mother OR father is reported as farmer, this variable is 1/TRUE
  farm_vars <- caretaker_job(redcap_data)
  
  # Combine in composite measure
  cons7_vars <- cbind(
    ses_vars$ses_score,
    educ_vars$parents_education,
    as.numeric(farm_vars$caretaker_farmer))
  composite <- composite_index(cons7_vars,
                               control_key=redcap_data$redcap_event_name=='baseline_arm_1',
                               col_key=c(1,1,-1)) # Reverse-score the farmer measure  
  output_construct <- as.data.frame(cbind(redcap_data$record_id,composite,cons7_vars))
  names(output_construct)[1] <- 'record_id'
  names(output_construct)[3:5] <- c('SES_inventory','Parent_education','Caretaker_IsFarmer')
  return(output_construct)
}

MakeConstruct8 <- function(redcap_data) {
  ## Children's activities
  # All measures are counts of activities that children reported participating in
  # (1) Participation in non-hazardous agricultural activities
  # (2) Participation in hazardous agricultural activities
  # (3) Participation in domestic and economic activities (non-agricultural)
  
  # Import the counts of cocoa-related agricultural activities using
  # the cocoa_activities function.
  cocoa_vars <- cocoa_activities(redcap_data)
  
  # In the FULL arm, children were only administered the questionnaire if they responded yes to 'WorkCocoaQ',
  # but we don't have that Q for LITE arm, so both questionnaires are re-conditioned on inferred 'WorkCocoa'.
  # (see AlloAlphabet.R for rationale on inferring status.) If WorkCocoa is FALSE, activities set to zero.
  cocoa_vars$Total_activityCocoa[!cocoa_vars$WorkCocoa] <-0 
  cocoa_vars$Total_activityCocoa_Hazard[!cocoa_vars$WorkCocoa] <-0 
  cocoa_vars$Total_activityCocoa_Nonhazard[!cocoa_vars$WorkCocoa] <-0 
  
  
  # This construct combines the economic and domestic activities (basically all non-agricultural)
  # into a single measure. Here we just add activityChore and activity Economic together.
  domestic_vars <- domestic_activities(redcap_data)
  econ_vars <- economic_activities(redcap_data)
  Total_activityEconDomes <- domestic_vars$Total_activityChore + econ_vars$Total_activityEconomic
  
  
  cons8_vars <- cbind(
    cocoa_vars[,c('Total_activityCocoa_Nonhazard','Total_activityCocoa_Hazard')],
    Total_activityEconDomes)
  composite <- composite_index(cons8_vars,
                               control_key=redcap_data$redcap_event_name=='baseline_arm_1',
                               col_key=c(1,1)) # Reverse-score the farmer measure  
  output_construct <- as.data.frame(cbind(redcap_data$record_id,composite,cons8_vars))
  names(output_construct)[1] <- 'record_id'
  return(output_construct)
}