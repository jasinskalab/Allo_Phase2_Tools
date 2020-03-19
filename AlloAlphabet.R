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
  
  ## Return a dataframe with the same number of rows as the input, but containing new columsn
  # Which columns to send
  incl_col <- keep_cols <- c(
    'record_id',
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