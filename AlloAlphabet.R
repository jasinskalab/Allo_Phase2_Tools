#' @export
# Data importing from Redcap file
load_Redcap <- function(data_directory){
  myCurrentFolder <- getwd()
  message(c('Current folder is: ',myCurrentFolder))
  myRedcapStorage <- data_directory
  message(c('Moving to folder: ',myRedcapStorage))
  data_filetmpl <- 'LIV2PhoneBasedLitera_DATA_\\d+'
  
  ## Load required packages
  reqd_packages = c('janitor','stringr','data.table')
  for (pack_i in (reqd_packages)) {
    if (!length(find.package(pack_i,quiet=TRUE))) {install.packages(pack_i)}
    library(pack_i,character.only=TRUE,warn.conflicts=FALSE) 
  }
  
  setwd(myRedcapStorage)
  
  ## Identify any Redcap Data files and save names to a dataframe
  allo_redcap_files <- as.data.frame(list.files('.',data_filetmpl),stringsAsFactors = FALSE)
  names(allo_redcap_files) <- 'filename'
  
  ## Find the newest stored data file and load it
  ## Find the newest file
  for( f in 1:dim(allo_redcap_files)[1] ) {
    filename_segs <- strsplit(strsplit(allo_redcap_files$filename[f],'\\.')[[1]][1],'_')[[1]]
    allo_redcap_files$datetime_str[f] <- paste(filename_segs[c(length(filename_segs)-1,length(filename_segs))],collapse='_')
  }
  allo_redcap_files$posix <- as.POSIXct(strptime(allo_redcap_files$datetime_str,'%Y-%m-%d_%H%M'))
  allo_redcap_files$is_new <- allo_redcap_files$posix==max(allo_redcap_files$posix)
  message(paste(c("Newest file found:",allo_redcap_files$filename[allo_redcap_files$is_new]),collapse=" "))
  message('Loading first file only!')
  message('')
  
  redcap_data <- clean_names(read.csv(allo_redcap_files$filename[allo_redcap_files$is_new][[1]],stringsAsFactors = FALSE))
  
  setwd(myCurrentFolder)
  message(c('Returning to folder: ',myCurrentFolder))
  return(redcap_data)
}

###-----PROCESS FRENCH LETTER / WORD / PSEUDOWORD -----
french_literacy <- function(redcap_data){
  
  #----- STANDARDIZE NAMES OF VARIABLES -----
  names(redcap_data)[names(redcap_data) %like% '^grapheme(s)?\\d+_\\d+_all_\\d+$'] <- sprintf('graphemes_%d',seq(1:100))
  names(redcap_data)[names(redcap_data) %like% '^motsfamilier(s)?\\d+_\\d+_all_\\d+$'] <- sprintf('motsfamiliers_%d',seq(1:50))
  names(redcap_data)[names(redcap_data) %like% '^motsinvente(s)?\\d+_\\d+_all_\\d+$'] <- sprintf('motsinventes_%d',seq(1:50))
  
  # Trim 'all' suffix off the reading tests
  names(redcap_data)[names(redcap_data) %like% '^french_((letters)|(word)|(pseudoword))_((crochet)|(time)|(autostop))_all$'] <- 
    substr(
      names(redcap_data)[names(redcap_data) %like% '^french_((letters)|(word)|(pseudoword))_((crochet)|(time)|(autostop))_all$'],
      1,
      nchar(names(redcap_data)[names(redcap_data) %like% '^french_((letters)|(word)|(pseudoword))_((crochet)|(time)|(autostop))_all$'])-4)
  
  ## Process letter reading (graphemes)
  # Set all rows to NA
  redcap_data$letter_french <- NA
  # Cap the crochet (number trials completed) at 100. All values > 100 are set to 100
  redcap_data$french_letters_crochet <- pmin(redcap_data$french_letters_crochet,100)
  # If autostop has a value 0/1, proceed with scoring. Otherwise, the score will remain NA
  # If autostop is 1, score is automatically zero
  redcap_data$letter_french[!is.na(redcap_data$french_letters_autostop) & redcap_data$french_letters_autostop==1] <- 0
  # If autostop is 0, score is equal to "crochet - rowSum(incorrect==1)" for all columns matching the regex below
  redcap_data$letter_french[!is.na(redcap_data$french_letters_autostop) & redcap_data$french_letters_autostop==0] <- 
    redcap_data$french_letters_crochet[!is.na(redcap_data$french_letters_autostop) & redcap_data$french_letters_autostop==0] -
    rowSums(  redcap_data[!is.na(redcap_data$french_letters_autostop) & redcap_data$french_letters_autostop==0, 
                          names(redcap_data) %like% '^graphemes_\\d+$'] ==1 ,na.rm=TRUE)
  
  # Same procedure is applied to frenchwords
  redcap_data$word_french <- NA
  redcap_data$french_word_crochet <- pmin(redcap_data$french_word_crochet,50)
  redcap_data$word_french[!is.na(redcap_data$french_word_autostop) & redcap_data$french_word_autostop==1] <- 0
  redcap_data$word_french[!is.na(redcap_data$french_word_autostop) & redcap_data$french_word_autostop==0] <- 
    redcap_data$french_word_crochet[!is.na(redcap_data$french_word_autostop) & redcap_data$french_word_autostop==0] -
    rowSums(  redcap_data[!is.na(redcap_data$french_word_autostop) & redcap_data$french_word_autostop==0, 
                          names(redcap_data) %like% '^motsfamiliers_\\d+$'] ==1 ,na.rm=TRUE)
  
  # Same procedure is applied to french pseduowords
  redcap_data$pseudo_french <- NA
  redcap_data$french_pseudoword_crochet <- pmin(redcap_data$french_pseudoword_crochet,50)
  redcap_data$pseudo_french[!is.na(redcap_data$french_pseudoword_autostop) & redcap_data$french_pseudoword_autostop==1] <- 0
  redcap_data$pseudo_french[!is.na(redcap_data$french_pseudoword_autostop) & redcap_data$french_pseudoword_autostop==0] <- 
    redcap_data$french_pseudoword_crochet[!is.na(redcap_data$french_pseudoword_autostop) & redcap_data$french_pseudoword_autostop==0] -
    rowSums(  redcap_data[!is.na(redcap_data$french_pseudoword_autostop) & redcap_data$french_pseudoword_autostop==0, 
                          names(redcap_data) %like% '^motsinventes_\\d+$'] ==1 ,na.rm=TRUE)
  
  ## Return a dataframe with the same number of rows as the input, but containing new columsn
  # Which columns to send
  incl_col <- keep_cols <- c(
    'record_id',
    'letter_french',
    'word_french',
    'pseudo_french',
    names(redcap_data)[names(redcap_data) %like% '^graphemes_\\d+$'],
    names(redcap_data)[names(redcap_data) %like% '^motsfamiliers_\\d+$'],
    names(redcap_data)[names(redcap_data) %like% '^motsinventes_\\d+$']
  )
  return(redcap_data[,incl_col])
}  

french_phoneme <- function(redcap_data){
  
  ## French Initial Phoneme Suppression
  # Set all rows to NA to start
  redcap_data$sup_initial_score_fr<-NA
  # If autostop is set to 1, set score to 0
  redcap_data$sup_initial_score_fr[redcap_data$french_int_autostop==1] <- 0
  # If autostop is set to 0, score the data; Otherwise, leave as NA
  # Scoring is rowSums of all CORRECT trials matching the regular expression below
  redcap_data$sup_initial_score_fr[!is.na(redcap_data$french_int_autostop) & redcap_data$french_int_autostop==0] <- rowSums(
    redcap_data[!is.na(redcap_data$french_int_autostop) & redcap_data$french_int_autostop==0, 
                names(redcap_data) %like% '^french_int\\d+$'] ==1 ,na.rm=TRUE)
  
  ## French Initial Phoneme Identification
  # same procedure as above
  redcap_data$identification_score_fr[redcap_data$french_id_autostop==1] <- 0
  redcap_data$identification_score_fr[!is.na(redcap_data$french_id_autostop) & redcap_data$french_id_autostop==0] <- rowSums(
    redcap_data[!is.na(redcap_data$french_id_autostop) & redcap_data$french_id_autostop==0, 
                names(redcap_data) %like% '^french_id\\d+$'] ==1 ,na.rm=TRUE)
  
  ## French Final Phoneme Suppression
  # same procedure as above
  redcap_data$sup_final_score_fr <- NA
  redcap_data$sup_final_score_fr[redcap_data$french_fin_autostop==1] <- 0
  redcap_data$sup_final_score_fr[!is.na(redcap_data$french_fin_autostop) & redcap_data$french_fin_autostop==0] <- rowSums(
    redcap_data[!is.na(redcap_data$french_fin_autostop) & redcap_data$french_fin_autostop==0, 
                names(redcap_data) %like% '^french_fin\\d+$'] ==1 ,na.rm=TRUE)
  
  ## French Phoneme Segmentation
  # same procedure as above
  redcap_data$seg_score_fr <- NA
  redcap_data$seg_score_fr[redcap_data$french_seg_autostop==1] <- 0
  redcap_data$seg_score_fr[!is.na(redcap_data$french_seg_autostop) & redcap_data$french_seg_autostop==0] <- rowSums(
    redcap_data[!is.na(redcap_data$french_seg_autostop) & redcap_data$french_seg_autostop==0, 
                names(redcap_data) %like% '^french_seg\\d+$'] ==1 ,na.rm=TRUE)
  
  ## Sum of the four phoneme types
  redcap_data$phoneme_french <- rowSums(redcap_data[,c('sup_initial_score_fr','identification_score_fr','sup_final_score_fr','seg_score_fr')])
  
  ## Return a dataframe with the same number of rows as the input, but containing new columsn
  # Which columns to send
  incl_col <- keep_cols <- c(
    'record_id',
    'phoneme_french',
    'sup_initial_score_fr',
    'identification_score_fr',
    'sup_final_score_fr',
    'seg_score_fr',
    names(redcap_data)[names(redcap_data) %like% '^french_int\\d+$'],
    names(redcap_data)[names(redcap_data) %like% '^french_id\\d+$'],
    names(redcap_data)[names(redcap_data) %like% '^french_fin\\d+$'],
    names(redcap_data)[names(redcap_data) %like% '^french_seg\\d+$']
  )
  return(redcap_data[,incl_col])
}

french_vocab <- function(redcap_data){
  # This measure not available for LITE (it was abbreviated and not comparable)
  redcap_data$french_antonyms <- NA
  redcap_data$french_antonyms[redcap_data$french_ant_autostop==1] <- 0
  redcap_data$french_antonyms[!is.na(redcap_data$french_ant_autostop) & redcap_data$french_ant_autostop==0] <- rowSums(
    redcap_data[!is.na(redcap_data$french_ant_autostop) & redcap_data$french_ant_autostop==0, 
              names(redcap_data) %like% '^french_ant\\d+$'] ==1 ,na.rm=TRUE)
  
  # This measure not available for LITE (it was abbreviated and not comparable)
  redcap_data$french_synonyms <- NA
  redcap_data$french_synonyms[redcap_data$french_syn_autostop==1] <- 0
  redcap_data$french_synonyms[!is.na(redcap_data$french_syn_autostop) & redcap_data$french_syn_autostop==0] <- rowSums(
    redcap_data[!is.na(redcap_data$french_syn_autostop) & redcap_data$french_syn_autostop==0, 
              names(redcap_data) %like% '^french_syn\\d+$'] ==1 ,na.rm=TRUE)
  
  # This measure not available for LITE (syn and ant were abbreviated and not comparable)
  redcap_data$vocab_french <- NA
  redcap_data$vocab_french[!is.na(redcap_data$french_synonyms) & !is.na(redcap_data$french_antonyms)] <- 
    redcap_data$french_synonyms[!is.na(redcap_data$french_synonyms) & !is.na(redcap_data$french_antonyms)] +
    redcap_data$french_antonyms[!is.na(redcap_data$french_synonyms) & !is.na(redcap_data$french_antonyms)]
  
  ## Return a dataframe with the same number of rows as the input, but containing new columsn
  # Which columns to send
  incl_col <- keep_cols <- c(
    'record_id',
    'vocab_french',
    'french_synonyms',
    'french_antonyms',
    names(redcap_data)[names(redcap_data) %like% '^french_syn\\d+$'],
    names(redcap_data)[names(redcap_data) %like% '^french_ant\\d+$']
  )
  return(redcap_data[,incl_col])
}

french_oralcomp <- function(redcap_data){
  
  # This measure was administered to all children (longer oral comprehension passage). 
  # If children got 0 out of 5 on this exercise AND they were in the FULL arm, they completed the short passage version
  redcap_data$french_oralcomp_long <- NA
  
  # AUTOSTOP WAS ONLY USED IN LITE FOR SOME REASON, SO IGNORING IT
  #redcap_data$french_oralcomp_long[redcap_data$french_comp2_autostop==1] <- 0
  #redcap_data$french_oralcomp_long[redcap_data$french_comp2_autostop_lite==1] <- 0
  
  # Test whether answers were recorded for at least one oralcomp_long question
  redcap_data$did_french_oralcomp_lite <-
    !apply(is.na(redcap_data[,names(redcap_data) %like% '^french_comp2q\\d_lite$']),1,all)
  redcap_data$did_french_oralcomp_full <-
    !apply(is.na(redcap_data[,names(redcap_data) %like% '^french_comp2q\\d$']),1,all)
  
  # Copy the LITE data into the FULL columns before doing the rowSums
  redcap_data[redcap_data$did_french_oralcomp_lite,names(redcap_data) %like% '^french_comp2q\\d$'] <-
    redcap_data[redcap_data$did_french_oralcomp_lite,names(redcap_data) %like% '^french_comp2q\\d_lite$']
  # Perform the sum over the FULL columns only (having copied LITE columns over where applicable)
  redcap_data$french_oralcomp_long[redcap_data$did_french_oralcomp_lite | redcap_data$did_french_oralcomp_full] <- rowSums(
    redcap_data[redcap_data$did_french_oralcomp_lite | redcap_data$did_french_oralcomp_full, 
                names(redcap_data) %like% '^french_comp2q\\d$']==1 ,na.rm=TRUE)

  ## Return a dataframe with the same number of rows as the input, but containing new columsn
  # Which columns to send
  incl_col <- keep_cols <- c(
    'record_id',
    'french_oralcomp_long',
    names(redcap_data)[names(redcap_data) %like% '^french_comp2q\\d$']
    )
  return(redcap_data[,incl_col])
}

allo_isomorphic <- function(redcap_data){
  
  # This measure was administered to all children (longer oral comprehension passage). 
  # If children got 0 out of 5 on this exercise AND they were in the FULL arm, they completed the short passage version
  redcap_data$allo_mini_score <- NA
 
  # Test whether answers were recorded for at least one allo_item question
  redcap_data$did_allo_mini <-
    !apply(is.na(redcap_data[,names(redcap_data) %like% '^allo_item\\d+_v2_mini$']),1,all)
  
  # Perform the sum over all allo_item columns
  redcap_data$allo_mini_score[redcap_data$did_allo_mini] <- rowSums(
    redcap_data[redcap_data$did_allo_mini, names(redcap_data) %like% '^allo_item\\d+_v2_mini$']==1 ,na.rm=TRUE)
  
  ## Return a dataframe with the same number of rows as the input, but containing new columsn
  # Which columns to send
  incl_col <- keep_cols <- c(
    'record_id',
    'allo_mini_score',
    names(redcap_data)[names(redcap_data) %like% '^allo_item\\d+_v2_mini$']
  )
  return(redcap_data[,incl_col])
}

ses_inventory <- function(redcap_data){
  
  # This inventory is the sum of 15 yes/no items for what the child reports having in household
  # Administered separately in FULL and LITE versions, but same 15 questions
  redcap_data$ses_score <- NA
  
  # Aggregate FULL scores (as checkbox, they will auto-populate 0 if the form was saved, NA if form was not saved)
  redcap_data$ses_score_full <-rowSums(redcap_data[,names(redcap_data) %like% '^objects_at_home_\\d+$']==1, na.rm=FALSE)

  # Aggregate LITE scores
  redcap_data$ses_score_lite <-rowSums(redcap_data[,names(redcap_data) %like% '^objects_at_home_lite_\\d+$']==1, na.rm=FALSE)
  
  # Combine the scores
  redcap_data$ses_score[!is.na(redcap_data$ses_score_lite)] <- redcap_data$ses_score_lite[!is.na(redcap_data$ses_score_lite)]
  redcap_data$ses_score[!is.na(redcap_data$ses_score_full)] <- redcap_data$ses_score_full[!is.na(redcap_data$ses_score_full)]
  
  # Combine the items by copying non-NA LITE data into FULL columns
  redcap_data[!is.na(redcap_data$ses_score_lite), names(redcap_data) %like% '^objects_at_home_\\d+$'] <-
    redcap_data[!is.na(redcap_data$ses_score_lite), names(redcap_data) %like% '^objects_at_home_lite_\\d+$']
  
  ## Return a dataframe with the same number of rows as the input, but containing new columsn
  # Which columns to send
  incl_col <- keep_cols <- c(
    'record_id',
    'ses_score',
    names(redcap_data)[names(redcap_data) %like% '^objects_at_home_\\d+$']
  )
  return(redcap_data[,incl_col])
}

parent_educ <- function(redcap_data){
  
  # Father education (1: None or Some Primary, 2: Completed Primary, 3: Completed Secondary, 4: Completed University)
  redcap_data$father_education
  
  # Mother education (1: None or Some Primary, 2: Completed Primary, 3: Completed Secondary, 4: Completed University)
  redcap_data$mother_education
  
  # How do we combine two ordinals? Mean / median produce same output with only two inputs
  redcap_data$parents_education <- rowMeans(redcap_data[,c('father_education','mother_education')],na.rm=FALSE)
    
  ## Return a dataframe with the same number of rows as the input, but containing new columsn
  # Which columns to send
  incl_col <- keep_cols <- c(
    'record_id',
    'parents_education',
    'father_education',
    'mother_education'
  )
  return(redcap_data[,incl_col])
}

caretaker_job <- function(redcap_data){
  
  redcap_data$caretaker_farmer <- NA
  
  # Only available in FULL Questionnaire: job_of_caretaker
  # Items 1-3 are cocoa farmer, rubber farmer, other farmer
  redcap_data$caretaker_farmer_childreport <- apply(redcap_data[,names(redcap_data) %like% '^job_of_caretaker_[1-3]$'],1,any)
  
  # Only available in TREATMENT Phone Contact: father_work, mother_work
  # Items 1-3 are cocoa farmer, rubber farmer, other farmer
  redcap_data$father_farmer_parentreport <- apply(redcap_data[,names(redcap_data) %like% '^father_work_[1-3]$'],1,any)
  redcap_data$mother_farmer_parentreport <- apply(redcap_data[,names(redcap_data) %like% '^mother_work_[1-3]$'],1,any)
  redcap_data$parent_farmer_parentreport <- (redcap_data$father_farmer_parentreport | redcap_data$mother_farmer_parentreport)
  
  # Primacy for parent report
  redcap_data$caretaker_farmer <- redcap_data$parent_farmer_parentreport
  redcap_data$caretaker_farmer[is.na(redcap_data$parent_farmer_parentreport)] <- redcap_data$caretaker_farmer_childreport[is.na(redcap_data$parent_farmer_parentreport)]

  ## Return a dataframe with the same number of rows as the input, but containing new columsn
  # Which columns to send
  incl_col <- keep_cols <- c(
    'record_id',
    'caretaker_farmer',
    names(redcap_data)[names(redcap_data) %like% '^job_of_caretaker_\\d+$']
  )
  return(redcap_data[,incl_col])
}

economic_activities <- function(redcap_data){
  # Economic activities
  set_prefix <- 'activityEconomic_'
  idx_start = which(names(redcap_data) %like% '^manage_business$')
  idx_stop = which(names(redcap_data) %like% '^produce_property$')
  idx_start_lite = which(names(redcap_data) %like% '^manage_business_lite$')
  idx_stop_lite = which(names(redcap_data) %like% '^produce_property_lite$')
  redcap_data[redcap_data$redcap_event_name=='baseline_arm_3',seq(idx_start,idx_stop)] <- 
    redcap_data[redcap_data$redcap_event_name=='baseline_arm_3',seq(idx_start_lite,idx_stop_lite)] 
  names(redcap_data)[seq(idx_start,idx_stop)] <- sprintf('%s%s',set_prefix,names(redcap_data)[seq(idx_start,idx_stop)])
  resp_list = c('Oui','Non','NSP','PdR')
  for (coli in seq(idx_start,idx_stop)) {
    new_value <- resp_list[redcap_data[,coli]]
    redcap_data[ , coli] <- new_value
  }
  
  redcap_data$Total_activityEconomic <- rowSums(redcap_data[,names(redcap_data) %like% '^activityEconomic_']=="Oui",na.rm=TRUE)
  redcap_data$Total_activityEconomic[apply(is.na(redcap_data[,names(redcap_data) %like% '^activityEconomic_']),1,all)] <- NA  
  ## Return a dataframe with the same number of rows as the input, but containing new columsn
  # Which columns to send
  incl_col <- c(
    'record_id',
    'Total_activityEconomic',
    names(redcap_data)[names(redcap_data) %like% '^activityEconomic_']
  )
  return(redcap_data[,incl_col])
}

domestic_activities <-function(redcap_data){
  # Chore-related activities
  set_prefix <- 'activityChore_'
  idx_start = which(names(redcap_data) %like% '^make_purchases$')
  idx_stop = which(names(redcap_data) %like% '^other_housework$')
  idx_start_lite = which(names(redcap_data) %like% '^make_purchases_lite$')
  idx_stop_lite = which(names(redcap_data) %like% '^other_housework_lite$')
  redcap_data[redcap_data$redcap_event_name=='baseline_arm_3',seq(idx_start,idx_stop)] <- 
    redcap_data[redcap_data$redcap_event_name=='baseline_arm_3',seq(idx_start_lite,idx_stop_lite)] 
  names(redcap_data)[seq(idx_start,idx_stop)] <- sprintf('%s%s',set_prefix,names(redcap_data)[seq(idx_start,idx_stop)])
  resp_list = c('Oui','Non','NSP','PdR')
  for (coli in seq(idx_start,idx_stop)) {
    new_value <- resp_list[redcap_data[,coli]]
    redcap_data[ , coli] <- new_value
  }
  redcap_data$Total_activityChore <- rowSums(redcap_data[,names(redcap_data) %like% '^activityChore_']=="Oui",na.rm=TRUE)
  redcap_data$Total_activityChore[apply(is.na(redcap_data[,names(redcap_data) %like% '^activityChore_']),1,all)] <- NA  
  ## Return a dataframe with the same number of rows as the input, but containing new columsn
  # Which columns to send
  incl_col <- c(
    'record_id',
    'Total_activityChore',
    names(redcap_data)[names(redcap_data) %like% '^activityChore_']
  )
  return(redcap_data[,incl_col])
}

cocoa_activities <-function(redcap_data){
  # Cocoa-related activities
  
  redcap_data$WorkCocoa <- apply(redcap_data[,names(redcap_data) %like% '^activityCocoa_']=='Oui',1,any) # Cocoa
  
  set_prefix <- 'activityCocoa_'
  idx_start = which(names(redcap_data) %like% '^clean_fields$')
  idx_stop = which(names(redcap_data) %like% '^other_job$')
  idx_start_lite = which(names(redcap_data) %like% '^clean_fields_lite$')
  idx_stop_lite = which(names(redcap_data) %like% '^other_job_lite$')
  redcap_data[redcap_data$redcap_event_name=='baseline_arm_3',seq(idx_start,idx_stop)] <- 
    redcap_data[redcap_data$redcap_event_name=='baseline_arm_3',seq(idx_start_lite,idx_stop_lite)] 
  names(redcap_data)[seq(idx_start,idx_stop)] <- sprintf('%s%s',set_prefix,names(redcap_data)[seq(idx_start,idx_stop)])
  resp_list = c('Oui','Non','NSP','PdR')
  for (coli in seq(idx_start,idx_stop)) {
    new_value <- resp_list[redcap_data[,coli]]
    redcap_data[ , coli] <- new_value
  }
  # This is meant to correct the activityCocoa to only include cocoa and not other agriculture. 
  # For the FULL kids (baseline_arm_1 and 2) that works, but for LITE kids, it was never asked.
  #redcap_data[
  #  (!is.na(redcap_data$fieldwork_type_1) & redcap_data$fieldwork_type_1==0),
  #  names(redcap_data) %like% '^activityCocoa_'] <- "Non"
  
  
  redcap_data$Total_activityCocoa <- rowSums(redcap_data[,names(redcap_data) %like% '^activityCocoa_']=="Oui",na.rm=TRUE)
  redcap_data$Total_activityCocoa[apply(is.na(redcap_data[,names(redcap_data) %like% '^activityCocoa_']),1,all)] <- NA  
  
  
  haz_work_types <- c('activityCocoa_spray_insecticides','activityCocoa_spread_fertilizer','activityCocoa_spread_chemicals',
                      'activityCocoa_beans_to_storage','activityCocoa_cutting_trees','activityCocoa_burning_trees')
  redcap_data$Total_activityCocoa_Hazard <- rowSums(redcap_data[,haz_work_types]=="Oui")
  redcap_data$Total_activityCocoa_Hazard[apply(is.na(redcap_data[,haz_work_types]),1,all)] <- NA
  redcap_data$Total_activityCocoa_Hazard[redcap_data$WorkCocoa==FALSE] <- 0
  
  # Non-hazardous is all remaining columns
  redcap_data$Total_activityCocoa_Nonhazard <- redcap_data$Total_activityCocoa - redcap_data$Total_activityCocoa_Hazard
  
  ## Return a dataframe with the same number of rows as the input, but containing new columsn
  # Which columns to send
  incl_col <- c(
    'record_id',
    'WorkCocoa',
    'Total_activityCocoa',
    'Total_activityCocoa_Hazard',
    'Total_activityCocoa_Nonhazard',
    names(redcap_data)[names(redcap_data) %like% '^activityCocoa_']
  )
  return(redcap_data[,incl_col])
}

family_readers <- function(redcap_data){
  
  # First aggregate the FULL and LITE versions for the binary question of whether any family member reads
  # Currently PdR and NSP are both interpreted as Non. This can be adjusted (treat as NA) if needed.
  redcap_data$family_readers_bin <- apply(redcap_data[,c('family_readers','family_readers_lite')]==1,1,any,na.rm=T)
  # At this point, two NAs come back as False, so correct that
  redcap_data$family_readers_bin[apply(is.na(redcap_data[,c('family_readers','family_readers_lite')]),1,all)] <- NA
  
  # For FULL version only, count the number of checks in the "who_reads" list (1-14), 15=NSP, 16=PdR
  # These checkboxes are only populated if the child participants in FULL (NA for LITE)
  # If a child responds Non, PdR, NSP in FULL, the checkboxes all remain as 0's
  redcap_data$family_readers_num <- rowSums(redcap_data[,names(redcap_data) %like% '^who_reads_([1-9]|(1[0-4]))$'],na.rm=F)
  
  # Note: there are ~4 kids who responded "Oui" to family_readers_bin and then 0 to all who_reads
  
  ## Return a dataframe with the same number of rows as the input, but containing new columsn
  # Which columns to send
  incl_col <- c(
    'record_id',
    'family_readers_bin',
    'family_readers_num',
    names(redcap_data)[names(redcap_data) %like% '^who_reads_([1-9]|(1[0-4]))$']
  )
  return(redcap_data[,incl_col])
}

book_at_home <- function(redcap_data){
  
  ## As-tu un livre de lecture ?
  # First aggregate the FULL and LITE versions for the binary question
  # Currently PdR and NSP are both interpreted as Non. This can be adjusted (treat as NA) if needed 
  # for FULL (Oui=1,Non=2,NSP=3,PdR=4), but LITE only gave two options (Oui=1/Non=0).
  redcap_data$book_at_home_lecture <- as.numeric(apply(redcap_data[,c('own_a_book','read_book_lite')]==1,1,any,na.rm=T))
  # At this point, two NAs come back as False, so correct that
  redcap_data$book_at_home_lecture[apply(is.na(redcap_data[,c('own_a_book','read_book_lite')]),1,all)] <- NA
  
  ## Note: objects_at_home_2/3 & objects_at_home_lite_2/3 also cover this question:
  # Chez toi à la maison, y a-t-il (2) un livre pour enfant? (3) livres, journaux, ou autres choses à lir?
  # These are not currently included, but maybe they should be.
  
  ## Chez toi à la maison, y a-t-il un livre pour enfant ?
  # Checkbox response, gave two options (Oui=1/Non=0).
  redcap_data$book_at_home_enfant <- as.numeric(apply(redcap_data[,c('objects_at_home_2','objects_at_home_lite_2')]==1,1,any,na.rm=T))
  # At this point, two NAs come back as False, so correct that
  redcap_data$book_at_home_enfant[apply(is.na(redcap_data[,c('objects_at_home_2','objects_at_home_lite_2')]),1,all)] <- NA
  
  ## Chez toi à la maison, y a-t-i livres, journaux, ou autres choses à lire
  # Checkbox response, gave two options (Oui=1/Non=0).
  redcap_data$book_at_home_autre <- as.numeric(apply(redcap_data[,c('objects_at_home_3','objects_at_home_lite_3')]==1,1,any,na.rm=T))
  # At this point, two NAs come back as False, so correct that
  redcap_data$book_at_home_autre[apply(is.na(redcap_data[,c('objects_at_home_3','objects_at_home_lite_3')]),1,all)] <- NA
  
  ## Return a dataframe with the same number of rows as the input, but containing new columsn
  # Which columns to send
  incl_col <- c(
    'record_id',
    'book_at_home_lecture',
    'book_at_home_enfant',
    'book_at_home_autre'
  )
  return(redcap_data[,incl_col])
}

family_french <- function(redcap_data){
  
  # First aggregate the FULL and LITE versions for the binary question of whether any family member reads
  # Currently PdR and NSP are both interpreted as Non. This can be adjusted (treat as NA) if needed.
  
  # First rename the languages_at_home_1 from FULL to something easier to recognize (french_at_home)
  redcap_data$french_at_home <- redcap_data$languages_at_home_1
  # If a row is NA for the FULL data, copy the LITE data over
  no_full_data <- is.na(redcap_data$french_at_home)
  redcap_data$french_at_home[no_full_data] <- redcap_data$languages_at_home_lite_1[no_full_data]
  
  # First rename the columns from FULL to something easier to recognize
  names(redcap_data)[names(redcap_data) %like% '^languages_spoken_by_fr_\\d+$'] <- 
    sprintf('speaks_french_%d',seq(1:length(names(redcap_data)[names(redcap_data) %like% '^languages_spoken_by_fr_\\d+$'])))
  
  # If a row is NA for the FULL data, copy the LITE data over
  no_full_data <- apply(is.na(redcap_data[,names(redcap_data) %like% '^speaks_french_\\d+$']),1,all)
  redcap_data[no_full_data,names(redcap_data) %like% '^speaks_french_\\d+$'] <- redcap_data[no_full_data,names(redcap_data) %like% '^lang_spoken_by_fr_lite_\\d+$']

  # For FULL + LITE versions, count the number of checks in the "speaks_french" list (1-14), 15=NSP, 16=PdR
  redcap_data$family_french_num <- rowSums(redcap_data[,names(redcap_data) %like% '^speaks_french_([1-9]|(1[0-4]))$'],na.rm=F)
  
  # Note: there are ~4 kids who responded "Oui" to family_readers_bin and then 0 to all who_reads
  
  ## Return a dataframe with the same number of rows as the input, but containing new columsn
  # Which columns to send
  incl_col <- c(
    'record_id',
    'french_at_home',
    'family_french_num',
    names(redcap_data)[names(redcap_data) %like% '^speaks_french_([1-9]|(1[0-4]))$']
  )
  return(redcap_data[,incl_col])
}

parent_involvedEducation <- function(redcap_data){
  
  # This instrument was administered with the phone distribution form
  # Therefore only available for Treatment children (FULL+LITE)
  
  # Questions are flagged with prefix "pp_"
  col_list <- names(redcap_data[,names(redcap_data) %like% '^pp_'])
  # Remove the last questions for "not involved at all"
  col_list <- col_list[col_list!='pp_not_involved']
  
  # First add up all the affirmative responses (Oui=1)
  redcap_data$parent_involvedEducation <- rowSums(redcap_data[,col_list]==1,na.rm=TRUE)
  # If all NA values, replace with NA
  redcap_data$parent_involvedEducation[apply(is.na(redcap_data[,col_list]),1,all)] <- NA
  # If all NSP (don't know) values, replace with NA
  redcap_data$parent_involvedEducation[apply(redcap_data[,col_list]==3,1,all,na.rm=TRUE)] <- NA
  
  # Copy the pp_not_involved to another column and export for comparison
  # NOTE: There are several cases where this item disagrees with previous, i.e., several
  # of the parent involvement questions are answered "Oui" and this question (parent not
  # at all involved in education) is also answered "Oui".
  redcap_data$parent_NOTinvolvedEducation <- redcap_data$pp_not_involved==1
  
  ## Return a dataframe with the same number of rows as the input, but containing new columsn
  # Which columns to send
  incl_col <- c(
    'record_id',
    'parent_involvedEducation',
    'parent_NOTinvolvedEducation',
    col_list
    )
  return(redcap_data[,incl_col])
}

composite_index <- function(dataset,control_key=NA,col_key=NA) {
  
  # If no reverse-coding key is specified, assign 1 (not reversed)
  # to every column in the dataset.
  if (is.na(col_key)){
    col_key = rep(1,dim(dataset)[2])
  }
  
  # If no set of control arm observations is specified, assign
  # control status (TRUE) to every row in the dataset.
  # Only control observations are used in standardizing.
  if (is.na(control_key)){
    control_key = rep(TRUE,dim(dataset)[1])
  }
  
  # Reverse code any columns where col_key is < 0 (ideally, -1)
  dataset[,col_key<0] <- dataset[,col_key<0] * -1
  
  # Compute the means and SDs of the variables for standardizing.
  # This operation ONLY uses control arm observations (control_key=TRUE)
  varMeans <- apply(dataset[control_key,],2,mean,na.rm=TRUE)
  varSDs <- apply(dataset[control_key,],2,sd,na.rm=TRUE)
  
  # Standardize the dataset using they means/SDs computed above.
  z_dataset <- dataset
  for (c in (1:ncol(dataset))) {
    z_dataset[,c] <- (dataset[,c] - varMeans[c]) / varSDs[c]
  }
  
  # Calculate the covariance matrix between all obs, all columns
  covars <- cov(z_dataset,use='pairwise.complete')
  
  # Compute the composite score as follows:
  # comp = (1' cov^-1 1)^-1 (1' cov^-1 z_dataset)
  # for some reason z_dataset needs to be transposed, which is not clear
  # in the Anderson 2008 description, but seems to work(?)
  scores <- rep(NA,dim(z_dataset)[1])
  scorable <- complete.cases(z_dataset)
  scores[scorable] = solve( t(rep(1,ncol(z_dataset))) %*% solve(covars) %*% (rep(1,ncol(z_dataset))) ) %*%
    ( t(rep(1,ncol(z_dataset))) %*% as.matrix(solve(covars)) %*% t(z_dataset[scorable,]) )
  
  return(scores)
}

  