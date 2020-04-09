# Allo_Phase2_Tools

source('AlloAlphabet.R')

Allo_data_frame <- load_Redcap('~/directory/containing/Redcap/table/')

Phonological_awareness_results <- french_phoneme(Allo_data_frame)

Literacy_results <- french_literacy(Allo_data_frame)

construct1 <- composite_index(Literacy_results[,c('letter_french','word_french','pseudo_french')],control_key=Allo_data_frame$redcap_event_name=='baseline_arm_1')