# Allo_Phase2_Tools
source('AlloAlphabet.R')
Allo_data_frame <- load_Redcap('~/directory/containing/Redcap/table/')
Phonological_awareness_results <- french_phoneme(Allo_data_frame)
Literacy_results <- french_ literacy(Allo_data_frame)

