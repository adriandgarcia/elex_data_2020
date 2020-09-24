library(tidyverse)

FEC_Data1 <- read_csv(
  "FEC/pacs_09152020/pacs_pt1_091520.csv",
  col_types =
    cols(
      committee_id = col_character(),
      committee_name = col_character(),
      report_year = col_double(),
      report_type = col_character(),
      image_number = col_double(),
      line_number = col_character(),
      transaction_id = col_character(),
      file_number = col_double(),
      entity_type = col_character(),
      entity_type_desc = col_character(),
      unused_recipient_committee_id = col_character(),
      recipient_committee_id = col_character(),
      recipient_name = col_character(),
      recipient_state = col_character(),
      beneficiary_committee_name = col_character(),
      national_committee_nonfederal_account = col_character(),
      disbursement_type = col_character(),
      disbursement_type_description = col_character(),
      disbursement_description = col_character(),
      memo_code = col_character(),
      memo_code_full = col_character(),
      disbursement_date = col_datetime(format = ""),
      disbursement_amount = col_double(),
      candidate_office = col_character(),
      candidate_office_description = col_character(),
      candidate_office_district = col_character(),
      candidate_id = col_character(),
      candidate_name = col_character(),
      candidate_first_name = col_character(),
      candidate_last_name = col_character(),
      candidate_middle_name = col_character(),
      candidate_prefix = col_character(),
      candidate_suffix = col_character(),
      candidate_office_state = col_character(),
      candidate_office_state_full = col_character(),
      election_type = col_character(),
      election_type_full = col_character(),
      fec_election_type_desc = col_character(),
      fec_election_year = col_double(),
      amendment_indicator = col_character(),
      amendment_indicator_desc = col_character(),
      schedule_type_full = col_character(),
      load_date = col_datetime(format = ""),
      original_sub_id = col_character(),
      back_reference_transaction_id = col_character(),
      back_reference_schedule_id = col_character(),
      semi_annual_bundled_refund = col_double(),
      payee_last_name = col_character(),
      payee_first_name = col_character(),
      payee_middle_name = col_character(),
      category_code = col_character(),
      category_code_full = col_character(),
      conduit_committee_name = col_character(),
      conduit_committee_street1 = col_character(),
      conduit_committee_street2 = col_character(),
      conduit_committee_city = col_character(),
      conduit_committee_state = col_character(),
      conduit_committee_zip = col_character(),
      spender_committee_type = col_character(),
      spender_committee_org_type = col_character(),
      spender_committee_designation = col_character(),
      filing_form = col_character(),
      link_id = col_double(),
      recipient_city = col_character(),
      recipient_zip = col_character(),
      disbursement_purpose_category = col_character(),
      memo_text = col_character(),
      two_year_transaction_period = col_double(),
      schedule_type = col_character(),
      sub_id = col_double(),
      pdf_url = col_character(),
      line_number_label = col_character(),
      payee_prefix = col_character(),
      payee_suffix = col_character(),
      payee_employer = col_character(),
      payee_occupation = col_character(),
      ref_disp_excess_flg = col_character(),
      comm_dt = col_character()
    )
)

FEC_Data2 <- read_csv("FEC/pacs_09152020/pacs_pt2_091520.csv",
                      col_types =
                        spec(FEC_Data1))

FEC_Data <- full_join(FEC_Data1, FEC_Data2)


Coms_20 <- read_csv("FEC/com_20.csv")
Coms_18 <- read_csv("FEC/com_18.csv")
Coms_16 <- read_csv("FEC/com_16.csv")
Coms_14 <- read_csv("FEC/com_14.csv")
Coms_12 <- read_csv("FEC/com_12.csv")
Coms_10 <- read_csv("FEC/com_10.csv")
Coms_08 <- read_csv("FEC/com_08.csv")

FEC_20 <-
  FEC_Data %>% filter(two_year_transaction_period == "2020") %>%
  left_join(., Coms_20, by = c("recipient_committee_id" = "CMTE_ID")) %>%
  mutate(
    committee_type =
      case_when(
        grepl("AMERIPRISE", committee_name) ~ "Company Pac",
        grepl("BLACKROCK", committee_name) ~ "Company Pac",
        grepl("CAPITAL GROUP", committee_name) ~ "Company Pac",
        grepl("FMR", committee_name) ~ "Company Pac",
        grepl("INVESCO", committee_name) ~ "Company Pac",
        grepl("INVESTMENT COMPANY INSTITUTE", committee_name) ~ "ICI Pac",
        grepl("MORGAN", committee_name) ~ "Company Pac",
        grepl("JACKSON", committee_name) ~ "Company Pac",
        grepl("MASSACHUSETTS", committee_name) ~ "Company Pac",
        grepl("TEACHERS", committee_name) ~ "Company Pac",
        grepl("VANGUARD", committee_name) ~ "Company Pac"
      )) %>%
  filter(disbursement_purpose_category %in% c("REFUNDS", "CONTRIBUTIONS"))

FEC_18 <-
  FEC_Data %>% filter(two_year_transaction_period == "2018") %>%
  left_join(., Coms_18, by = c("recipient_committee_id" = "CMTE_ID")) %>%
  mutate(
    committee_type =
      case_when(
        grepl("AMERIPRISE", committee_name) ~ "Company Pac",
        grepl("BLACKROCK", committee_name) ~ "Company Pac",
        grepl("CAPITAL GROUP", committee_name) ~ "Company Pac",
        grepl("FMR", committee_name) ~ "Company Pac",
        grepl("INVESCO", committee_name) ~ "Company Pac",
        grepl("INVESTMENT COMPANY INSTITUTE", committee_name) ~ "ICI Pac",
        grepl("MORGAN", committee_name) ~ "Company Pac",
        grepl("JACKSON", committee_name) ~ "Company Pac",
        grepl("MASSACHUSETTS", committee_name) ~ "Company Pac",
        grepl("TEACHERS", committee_name) ~ "Company Pac",
        grepl("VANGUARD", committee_name) ~ "Company Pac"
      )) %>%
  filter(disbursement_purpose_category %in% c("REFUNDS", "CONTRIBUTIONS"))

FEC_16 <-
  FEC_Data %>% filter(two_year_transaction_period == "2016") %>%
  left_join(., Coms_16, by = c("recipient_committee_id" = "CMTE_ID")) %>%
  mutate(
    committee_type =
        case_when(
          grepl("AMERIPRISE", committee_name) ~ "Company Pac",
          grepl("BLACKROCK", committee_name) ~ "Company Pac",
          grepl("CAPITAL GROUP", committee_name) ~ "Company Pac",
          grepl("FMR", committee_name) ~ "Company Pac",
          grepl("INVESCO", committee_name) ~ "Company Pac",
          grepl("INVESTMENT COMPANY INSTITUTE", committee_name) ~ "ICI Pac",
          grepl("MORGAN", committee_name) ~ "Company Pac",
          grepl("JACKSON", committee_name) ~ "Company Pac",
          grepl("MASSACHUSETTS", committee_name) ~ "Company Pac",
          grepl("TEACHERS", committee_name) ~ "Company Pac",
          grepl("VANGUARD", committee_name) ~ "Company Pac"
        )) %>%
  filter(disbursement_purpose_category %in% c("REFUNDS", "CONTRIBUTIONS"))

FEC_14 <-
  FEC_Data %>% filter(two_year_transaction_period == "2014") %>%
  left_join(., Coms_14, by = c("recipient_committee_id" = "CMTE_ID")) %>%
  mutate(
    committee_type =
      case_when(
        grepl("AMERIPRISE", committee_name) ~ "Company Pac",
        grepl("BLACKROCK", committee_name) ~ "Company Pac",
        grepl("CAPITAL GROUP", committee_name) ~ "Company Pac",
        grepl("FMR", committee_name) ~ "Company Pac",
        grepl("INVESCO", committee_name) ~ "Company Pac",
        grepl("INVESTMENT COMPANY INSTITUTE", committee_name) ~ "ICI Pac",
        grepl("MORGAN", committee_name) ~ "Company Pac",
        grepl("JACKSON", committee_name) ~ "Company Pac",
        grepl("MASSACHUSETTS", committee_name) ~ "Company Pac",
        grepl("TEACHERS", committee_name) ~ "Company Pac",
        grepl("VANGUARD", committee_name) ~ "Company Pac"
      )) %>%
  filter(disbursement_purpose_category %in% c("REFUNDS", "CONTRIBUTIONS"))

FEC_12 <-
  FEC_Data %>% filter(two_year_transaction_period == "2012") %>%
  left_join(., Coms_12, by = c("recipient_committee_id" = "CMTE_ID")) %>%
  mutate(
    committee_type =
      case_when(
        grepl("AMERIPRISE", committee_name) ~ "Company Pac",
        grepl("BLACKROCK", committee_name) ~ "Company Pac",
        grepl("CAPITAL GROUP", committee_name) ~ "Company Pac",
        grepl("FMR", committee_name) ~ "Company Pac",
        grepl("INVESCO", committee_name) ~ "Company Pac",
        grepl("INVESTMENT COMPANY INSTITUTE", committee_name) ~ "ICI Pac",
        grepl("MORGAN", committee_name) ~ "Company Pac",
        grepl("JACKSON", committee_name) ~ "Company Pac",
        grepl("MASSACHUSETTS", committee_name) ~ "Company Pac",
        grepl("TEACHERS", committee_name) ~ "Company Pac",
        grepl("VANGUARD", committee_name) ~ "Company Pac"
      )) %>%
  filter(disbursement_purpose_category %in% c("REFUNDS", "CONTRIBUTIONS"))

FEC_10 <-
  FEC_Data %>% filter(two_year_transaction_period == "2010") %>%
  left_join(., Coms_10, by = c("recipient_committee_id" = "CMTE_ID"))  %>%
  mutate(
    committee_type =
      case_when(
        grepl("AMERIPRISE", committee_name) ~ "Company Pac",
        grepl("BLACKROCK", committee_name) ~ "Company Pac",
        grepl("CAPITAL GROUP", committee_name) ~ "Company Pac",
        grepl("FMR", committee_name) ~ "Company Pac",
        grepl("INVESCO", committee_name) ~ "Company Pac",
        grepl("INVESTMENT COMPANY INSTITUTE", committee_name) ~ "ICI Pac",
        grepl("MORGAN", committee_name) ~ "Company Pac",
        grepl("JACKSON", committee_name) ~ "Company Pac",
        grepl("MASSACHUSETTS", committee_name) ~ "Company Pac",
        grepl("TEACHERS", committee_name) ~ "Company Pac",
        grepl("VANGUARD", committee_name) ~ "Company Pac"
      )) %>%
  filter(disbursement_purpose_category %in% c("REFUNDS", "CONTRIBUTIONS"))

FEC_08 <-
  FEC_Data %>% filter(two_year_transaction_period == "2008") %>%
  left_join(., Coms_08, by = c("recipient_committee_id" = "CMTE_ID"))  %>%
  mutate(
    committee_type =
      case_when(
        grepl("AMERIPRISE", committee_name) ~ "Company Pac",
        grepl("BLACKROCK", committee_name) ~ "Company Pac",
        grepl("CAPITAL GROUP", committee_name) ~ "Company Pac",
        grepl("FMR", committee_name) ~ "Company Pac",
        grepl("INVESCO", committee_name) ~ "Company Pac",
        grepl("INVESTMENT COMPANY INSTITUTE", committee_name) ~ "ICI Pac",
        grepl("MORGAN", committee_name) ~ "Company Pac",
        grepl("JACKSON", committee_name) ~ "Company Pac",
        grepl("MASSACHUSETTS", committee_name) ~ "Company Pac",
        grepl("TEACHERS", committee_name) ~ "Company Pac",
        grepl("VANGUARD", committee_name) ~ "Company Pac"
      )) %>%
  filter(disbursement_purpose_category %in% c("REFUNDS", "CONTRIBUTIONS"))

FEC_PAC_08_to_20 <- full_join(FEC_20, FEC_18) %>%
  full_join(., FEC_16) %>%
  full_join(., FEC_14) %>%
  full_join(., FEC_12) %>%
  full_join(., FEC_10) %>%
  full_join(., FEC_08)

write_csv(FEC_PAC_08_to_20, "FEC_PAC_08_to_20.csv")

rm(list = ls())
