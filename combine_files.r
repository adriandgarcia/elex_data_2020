
library(tidyverse)

READ <- function(FILE, OUTPUT) {
  read_csv(FILE,
           trim_ws = TRUE,
           col_types = cols(
             committee_id = col_character(),
             committee_name  = col_character(),
             report_year = col_number(),
             report_type = col_character(),
             image_number = col_number(),
             line_number = col_character(),
             transaction_id = col_character(),
             file_number = col_number(),
             committee_name_1 = col_character(),
             entity_type = col_character(),
             entity_type_desc = col_character(),
             unused_contbr_id = col_character(),
             unused_contbr_id = col_character(),
             contributor_prefix = col_character(),
             contributor_name = col_character(),
             recipient_committee_type = col_character(),
             recipient_committee_org_type = col_character(),
             recipient_committee_designation = col_character(),
             contributor_first_name = col_character(),
             contributor_middle_name = col_character(),
             contributor_last_name  = col_character(),
             contributor_suffix = col_character(),
             contributor_street_1 = col_character(),
             contributor_street_2 = col_character(),
             contributor_city = col_character(),
             contributor_state = col_character(),
             contributor_zip = col_character(),
             contributor_employer = col_character(),
             contributor_occupation = col_character(),
             contributor_id = col_character(),
             receipt_type = col_character(),
             receipt_type_desc = col_character(),
             receipt_type_full = col_character(),
             memo_code = col_character(),
             memo_code_full = col_character(),
             contribution_receipt_date = col_character(),
             contribution_receipt_amount = col_number(),
             contributor_aggregate_ytd  = col_number(),
             candidate_id = col_character(),
             candidate_name = col_character(),
             candidate_first_name = col_character(),
             candidate_last_name = col_character(),
             candidate_middle_name = col_character(),
             candidate_prefix = col_character(),
             candidate_suffix = col_character(),
             candidate_office = col_character(),
             candidate_office_full = col_character(),
             candidate_office_state = col_character(),
             candidate_office_state_full = col_character(),
             candidate_office_district = col_character(),
             conduit_committee_id = col_character(),
             conduit_committee_name = col_character(),
             conduit_committee_street1 = col_character(),
             conduit_committee_street2 = col_character(),
             conduit_committee_city = col_character(),
             conduit_committee_state = col_character(),
             conduit_committee_zip = col_character(),
             donor_committee_name = col_character(),
             national_committee_nonfederal_account = col_character(),
             election_type = col_character(),
             election_type_full = col_character(),
             fec_election_type_desc = col_character(),
             fec_election_year = col_number(),
             amendment_indicator = col_character(),
             amendment_indicator_desc = col_character(),
             schedule_type_full = col_character(),
             load_date = col_character(),
             original_sub_id = col_character(),
             back_reference_transaction_id = col_character(),
             back_reference_schedule_name = col_character(),
             filing_form = col_character(),
             link_id = col_number(),
             is_individual = col_character(),
             memo_text = col_character(),
             two_year_transaction_period = col_number(),
             schedule_type = col_character(),
             increased_limit = col_character(),
             sub_id = col_character(),
             pdf_url = col_character(),
             line_number_label = col_character()
           )
  )
}

abigail_johnson <- READ("FEC/indiv_09082020/abigail_johnson.csv") %>% 
  mutate(name = "abigail johnson",
         spouse = "christopher mckown")

annette_nazareth <- READ("FEC/indiv_09082020/annette_nazareth.csv") %>% 
  mutate(name = "annette nazareth",
         spouse = "roger ferguson")

christopher_mckown <- READ("FEC/indiv_09082020/christopher_mckown.csv") %>% 
  mutate(name = "christopher mckown",
         spouse = "abigail johnson")

dana_emery <- READ("FEC/indiv_09082020/dana_emery.csv") %>% 
  mutate(name = "dana emery",
         spouse = "robert emery")

david_butler <- READ("FEC/indiv_09082020/david_butler.csv") %>% 
  mutate(name = "david butler",
         spouse = "chrissy butler")

elizabeth_norris <- READ("FEC/indiv_09082020/elizabeth_norris.csv") %>% 
  mutate(name = "elizabeth norris",
         spouse = "mortimer buckley")

gerard_oreilly <- READ("FEC/indiv_09082020/gerard_oreilly.csv") %>% 
  mutate(name = "gerard oreilly",
         spouse = "sisi dinh oreilly")

jennifer_flanagan <- READ("FEC/indiv_09082020/jennifer_flanagan.csv") %>% 
  mutate(name = "jennifer flanagan",
         spouse = "martin flanagan")

jennifer_johnson <- READ("FEC/indiv_09082020/jennifer_johnson.csv") %>% 
  mutate(name = "jennifer johnson",
         spouse = "")

kathy_truscott <- READ("FEC/indiv_09082020/kathy_truscott.csv") %>% 
  mutate(name = "kathy truscott",
         spouse = "william truscott")

laurence_fink <- READ("FEC/indiv_09082020/laurence_fink.csv") %>% 
  mutate(name = "laurence fink",
         spouse = "lori fink")

martin_flanagan <- READ("FEC/indiv_09082020/martin_flanagan.csv") %>% 
  mutate(name = "martin flanagan",
         spouse = "jennifer flanagan")

mary_callahan_erdoes <- READ("FEC/indiv_09082020/mary_callahan_erdoes.csv") %>% 
  mutate(name = "mary callahan erdoes",
         spouse = "philip erdoes")

matt_oconnor <- READ("FEC/indiv_09082020/matt_oconnor.csv") %>% 
  mutate(name = "matt oconnor",
         spouse = "susan oconnor")

michael_falcon <- READ("FEC/indiv_09082020/michael_falcon.csv") %>% 
  mutate(name = "michael falcon",
         spouse = "susan falcon")

michael_roberge <- READ("FEC/indiv_09082020/michael_roberge.csv") %>% 
  mutate(name = "michael roberge",
         spouse = "tracy roberge")


mortimer_buckley <- READ("FEC/indiv_09082020/mortimer_buckley.csv") %>% 
  mutate(name = "mortimer buckley",
         spouse = "elizabeth norris")

philip_erdoes <- READ("FEC/indiv_09082020/philip_erdoes.csv") %>% 
  mutate(name = "philip erdoes",
         spouse = "mary callahan erdoes")

robert_emery <- READ("FEC/indiv_09082020/robert_emery.csv") %>% 
  mutate(name = "robert emery",
         spouse = "dana emery")

roger_ferguson <- READ("FEC/indiv_09082020/roger_ferguson.csv") %>% 
  mutate(name = "roger ferguson",
         spouse = "annette nazareth")


susan_oconnor <- READ("FEC/indiv_09082020/susan_oconnor.csv") %>% 
  mutate(name = "susan oconnor",
         spouse = "matthew oconnor")

tim_armour <- READ("FEC/indiv_09082020/tim_armour.csv") %>% 
  mutate(name = "tim armour",
         spouse = "")

tracy_roberge <- READ("FEC/indiv_09082020/tracy_roberge.csv") %>% 
  mutate(name = "tracy roberge",
         spouse = "michael roberge")

william_stromberg <- READ("FEC/indiv_09082020/william_stromberg.csv") %>% 
  mutate(name = "william stromberg",
         spouse = "lisa stromberg")

william_truscott <- READ("FEC/indiv_09082020/william_truscott.csv") %>% 
  mutate(name = "william truscott",
         spouse = "kathy truscott")


CEO_FEC_09082020 <- full_join(abigail_johnson, annette_nazareth) %>% 
  full_join(., christopher_mckown) %>% 
  full_join(., dana_emery) %>% 
  full_join(., david_butler) %>% 
  full_join(., elizabeth_norris) %>% 
  full_join(., gerard_oreilly) %>% 
  full_join(., jennifer_flanagan) %>% 
  full_join(., jennifer_johnson) %>%
  full_join(., kathy_truscott) %>% 
  full_join(., laurence_fink) %>% 
  full_join(., martin_flanagan) %>% 
  full_join(., mary_callahan_erdoes) %>% 
  full_join(., matt_oconnor) %>% 
  full_join(., michael_falcon) %>% 
  full_join(., michael_roberge) %>% 
  full_join(., mortimer_buckley) %>% 
  full_join(., philip_erdoes) %>% 
  full_join(., robert_emery) %>% 
  full_join(., roger_ferguson) %>% 
  full_join(., susan_oconnor) %>% 
  full_join(., tim_armour) %>% 
  full_join(., tracy_roberge) %>% 
  full_join(., william_stromberg) %>% 
  full_join(., william_truscott)


write_csv(CEO_FEC_09082020, "FEC/CEO_FEC_09082020.csv")

com_hed <- colnames(read_csv("FEC/cm_091020/cm_header_file.csv"))
com_20<- read_delim("FEC/cm_091020/cm_20.txt", delim = "|", col_names = com_hed)
com_18<- read_delim("FEC/cm_091020/cm_18.txt", delim = "|", col_names = com_hed)
com_16<- read_delim("FEC/cm_091020/cm_16.txt", delim = "|", col_names = com_hed)
com_14<- read_delim("FEC/cm_091020/cm_14.txt", delim = "|", col_names = com_hed)
com_12<- read_delim("FEC/cm_091020/cm_12.txt", delim = "|", col_names = com_hed)
com_10<- read_delim("FEC/cm_091020/cm_10.txt", delim = "|", col_names = com_hed)
com_08<- read_delim("FEC/cm_091020/cm_08.txt", delim = "|", col_names = com_hed)

Com_FEC_09102020 <- full_join(com_20, com_18) %>% 
  full_join(., com_16) %>% 
  full_join(., com_14) %>% 
  full_join(., com_12) %>% 
  full_join(., com_10) %>% 
  full_join(., com_08)

write_csv(com_20, "FEC/com_20.csv")
write_csv(com_18, "FEC/com_18.csv")
write_csv(com_16, "FEC/com_16.csv")
write_csv(com_14, "FEC/com_14.csv")
write_csv(com_12, "FEC/com_12.csv")
write_csv(com_10, "FEC/com_10.csv")
write_csv(com_08, "FEC/com_08.csv")
write_csv(Com_FEC_09102020, "FEC/Com_FEC_09102020.csv")


rm(list = ls())


