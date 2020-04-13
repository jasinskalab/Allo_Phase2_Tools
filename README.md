# Allo_Phase2_Tools

source('AlloAlphabet.R')

Allo_data_frame <- load_Redcap('~/directory/containing/Redcap/table/')

Phonological_awareness_results <- french_phoneme(Allo_data_frame)

Literacy_results <- french_literacy(Allo_data_frame)

# This is the manual way of creating a construct, but it's not necessary to 
# specify all of this information for the existing (pre-registered) constructs
construct1 <- composite_index(Literacy_results[,c('letter_french','word_french','pseudo_french')],control_key=Allo_data_frame$redcap_event_name=='baseline_arm_1')

# For the pre-registered constructs, we have provided automatic builders
source('ConstructBuild.R')
construct1 <- MakeConstruct1(Allo_data_frame)
construct3 <- MakeConstruct3(Allo_data_frame)
construct4 <- MakeConstruct4(Allo_data_frame)