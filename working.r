library(tidyverse)

FEC_Data <- read_csv("FEC/CEO_FEC_09082020.csv")


Coms_20 <- read_csv("FEC/com_20.csv")
Coms_18 <- read_csv("FEC/com_18.csv")
Coms_16 <- read_csv("FEC/com_16.csv")
Coms_14 <- read_csv("FEC/com_14.csv")
Coms_12 <- read_csv("FEC/com_12.csv")
Coms_10 <- read_csv("FEC/com_10.csv")
Coms_08 <- read_csv("FEC/com_08.csv")

FEC_20 <- FEC_Data %>% filter(two_year_transaction_period == "2020") %>% 
  left_join(., Coms_20, by = c("committee_id" = "CMTE_ID")) %>%  
  mutate(committee_type =
           case_when(
             committee_name == "AMERIPRISE FINANCIAL INC. POLITICAL ACTION COMMITTEE (AMERIPRISEPAC)" ~ "Company Pac",
             committee_name == "TEACHERS INSURANCE ANNUITY ASSOCIATION OF AMERICA PAC (TIAA PAC)" ~ "Company Pac",
             committee_name == "INVESCO HOLDING COMPANY (US), INC. POLITICAL ACTION COMMITTEE" ~ "Company Pac",
             committee_name == "THE VANGUARD GROUP COMMITTEE FOR RESPONSIBLE GOVERNMENT" ~ "Company Pac",
             committee_name == "INVESTMENT COMPANY INSTITUTE POLITICAL ACTION COMMITTEE (ICI PAC)" ~ "ICI Pac",
             committee_name == "THE CAPITAL GROUP COMPANIES INC POLITICAL ACTION COMMITTEE" ~ "Company Pac",
             committee_name == "AMERICAN COUNCIL OF LIFE INSURERS POLITICAL ACTION COMMITTEE" ~ "American Council of Life Insurers Pac",
             committee_name == "MOONEY VICTORY FUND" ~ "REP", 
             committee_name == "SCALISE LEADERSHIP FUND" ~ "REP",
             committee_name == "BIDEN VICTORY FUND" ~ "Dem",
             committee_name == "JACKSON HOLDINGS LLC AND JACKSON NATIONAL LIFE INSURANCE COMPANY FEDERAL SSF (JACKSON NATIONAL FEDERAL PAC)" ~ "Company Pac",
             committee_name == "JPMORGAN CHASE & CO. FEDERAL POLITICAL ACTION COMMITTEE" ~ "Company Pac",
             committee_name == "PORTMAN FOR SENATE COMMITTEE" ~ "REP",
             committee_name == "ACTBLUE" ~ "ActBlue",
             TRUE ~ as.character(CMTE_PTY_AFFILIATION)
           ))

FEC_18 <- FEC_Data %>% filter(two_year_transaction_period == "2018") %>% 
  left_join(., Coms_18, by = c("committee_id" = "CMTE_ID")) %>% 
  mutate(committee_type =
           case_when(
             committee_name == "AMERIPRISE FINANCIAL INC. POLITICAL ACTION COMMITTEE (AMERIPRISEPAC)" ~ "Company Pac",
             committee_name == "TEACHERS INSURANCE ANNUITY ASSOCIATION OF AMERICA PAC (TIAA PAC)" ~ "Company Pac",
             committee_name == "INVESCO HOLDING COMPANY (US), INC. POLITICAL ACTION COMMITTEE" ~ "Company Pac",
             committee_name == "THE VANGUARD GROUP COMMITTEE FOR RESPONSIBLE GOVERNMENT" ~ "Company Pac",
             committee_name == "INVESTMENT COMPANY INSTITUTE POLITICAL ACTION COMMITTEE (ICI PAC)" ~ "ICI Pac",
             committee_name == "THE CAPITAL GROUP COMPANIES INC POLITICAL ACTION COMMITTEE" ~ "Company Pac",
             committee_name == "AMERICAN COUNCIL OF LIFE INSURERS POLITICAL ACTION COMMITTEE" ~ "American Council of Life Insurers Pac",
             committee_name == "MOONEY VICTORY FUND" ~ "REP", 
             committee_name == "SCALISE LEADERSHIP FUND" ~ "REP",
             committee_name == "BIDEN VICTORY FUND" ~ "Dem",
             committee_name == "JACKSON HOLDINGS LLC AND JACKSON NATIONAL LIFE INSURANCE COMPANY FEDERAL SSF (JACKSON NATIONAL FEDERAL PAC)" ~ "Company Pac",
             committee_name == "JPMORGAN CHASE & CO. FEDERAL POLITICAL ACTION COMMITTEE" ~ "Company Pac",
             committee_name == "PORTMAN FOR SENATE COMMITTEE" ~ "REP",
             committee_name == "ACTBLUE" ~ "ActBlue",
             committee_name == "CAPITAL GROUP COMPANIES INC POLITICAL ACTION COMMITTEE; THE" ~ "Company Pac",
             committee_name == "CRAPO VICTORY COMMITTEE" ~ "REP",
             committee_name == "INVESTMENT COMPANY INSTITUTE POLITICAL ACTION COMMITTEE" ~ "ICI Pac",
             committee_name == "TEAM RYAN" ~ "REP",
             committee_name == "AMERICANS FOR LEGISLATING EXCELLENCE PAC" ~ "REP",
             TRUE ~ as.character(CMTE_PTY_AFFILIATION)
           ))

FEC_16 <- FEC_Data %>% filter(two_year_transaction_period == "2016") %>% 
  left_join(., Coms_16, by = c("committee_id" = "CMTE_ID")) %>% 
  mutate(committee_type =
           case_when(
             committee_name == "AMERIPRISE FINANCIAL INC. POLITICAL ACTION COMMITTEE (AMERIPRISEPAC)" ~ "Company Pac",
             committee_name == "TEACHERS INSURANCE ANNUITY ASSOCIATION OF AMERICA PAC (TIAA PAC)" ~ "Company Pac",
             committee_name == "INVESCO HOLDING COMPANY (US), INC. POLITICAL ACTION COMMITTEE" ~ "Company Pac",
             committee_name == "THE VANGUARD GROUP COMMITTEE FOR RESPONSIBLE GOVERNMENT" ~ "Company Pac",
             committee_name == "INVESTMENT COMPANY INSTITUTE POLITICAL ACTION COMMITTEE (ICI PAC)" ~ "ICI Pac",
             committee_name == "THE CAPITAL GROUP COMPANIES INC POLITICAL ACTION COMMITTEE" ~ "Company Pac",
             committee_name == "AMERICAN COUNCIL OF LIFE INSURERS POLITICAL ACTION COMMITTEE" ~ "American Council of Life Insurers Pac",
             committee_name == "MOONEY VICTORY FUND" ~ "REP", 
             committee_name == "SCALISE LEADERSHIP FUND" ~ "REP",
             committee_name == "BIDEN VICTORY FUND" ~ "Dem",
             committee_name == "JACKSON HOLDINGS LLC AND JACKSON NATIONAL LIFE INSURANCE COMPANY FEDERAL SSF (JACKSON NATIONAL FEDERAL PAC)" ~ "Company Pac",
             committee_name == "JPMORGAN CHASE & CO. FEDERAL POLITICAL ACTION COMMITTEE" ~ "Company Pac",
             committee_name == "PORTMAN FOR SENATE COMMITTEE" ~ "REP",
             committee_name == "ACTBLUE" ~ "ActBlue",
             committee_name == "CAPITAL GROUP COMPANIES INC POLITICAL ACTION COMMITTEE; THE" ~ "Company Pac",
             committee_name == "CRAPO VICTORY COMMITTEE" ~ "REP",
             committee_name == "INVESTMENT COMPANY INSTITUTE POLITICAL ACTION COMMITTEE" ~ "ICI Pac",
             committee_name == "TEAM RYAN" ~ "REP",
             committee_name == "FRIENDS OF KELLY AYOTTE INC" ~ "REP",
             committee_name == "GEORGIANS FOR ISAKSON" ~ "REP",
             committee_name == "HILLARY ACTION FUND" ~ "DEM",
             committee_name == "KELLY VICTORY FUND" ~ "REP",
             committee_name == "MASSACHUSETTS FINANCIAL SERVICES COMPANY POLITICAL ACTION COMMITTEE (MFS PAC)" ~ "Company Pac",
             committee_name == "AYOTTE MAJORITY COMMITTEE" ~ "REP",
             committee_name == "HILLARY VICTORY FUND" ~ "DEM",
             committee_name == "KASICH FOR AMERICA INC" ~ "REP",
             committee_name == "KEEP ELECTED LEADERS LISTENING TO YOU PAC" ~ "REP",
             committee_name == "NEW DAY INDEPENDENT MEDIA COMMITTEE INC" ~ "REP",
             committee_name == "NEW DAY INDEPENDENT MEDIA COMMITTEE INC" ~ "REP",
             committee_name == "PEOPLE FOR PATTY MURRAY" ~ "DEM",
             TRUE ~ as.character(CMTE_PTY_AFFILIATION)
           )) 

FEC_14 <- FEC_Data %>% filter(two_year_transaction_period == "2014") %>% 
  left_join(., Coms_14, by = c("committee_id" = "CMTE_ID")) %>% 
  mutate(committee_type =
           case_when(
             committee_name == "AMERIPRISE FINANCIAL INC. POLITICAL ACTION COMMITTEE (AMERIPRISEPAC)" ~ "Company Pac",
             committee_name == "TEACHERS INSURANCE ANNUITY ASSOCIATION OF AMERICA PAC (TIAA PAC)" ~ "Company Pac",
             committee_name == "INVESCO HOLDING COMPANY (US), INC. POLITICAL ACTION COMMITTEE" ~ "Company Pac",
             committee_name == "THE VANGUARD GROUP COMMITTEE FOR RESPONSIBLE GOVERNMENT" ~ "Company Pac",
             committee_name == "INVESTMENT COMPANY INSTITUTE POLITICAL ACTION COMMITTEE (ICI PAC)" ~ "ICI Pac",
             committee_name == "THE CAPITAL GROUP COMPANIES INC POLITICAL ACTION COMMITTEE" ~ "Company Pac",
             committee_name == "AMERICAN COUNCIL OF LIFE INSURERS POLITICAL ACTION COMMITTEE" ~ "American Council of Life Insurers Pac",
             committee_name == "MOONEY VICTORY FUND" ~ "REP", 
             committee_name == "SCALISE LEADERSHIP FUND" ~ "REP",
             committee_name == "BIDEN VICTORY FUND" ~ "Dem",
             committee_name == "JACKSON HOLDINGS LLC AND JACKSON NATIONAL LIFE INSURANCE COMPANY FEDERAL SSF (JACKSON NATIONAL FEDERAL PAC)" ~ "Company Pac",
             committee_name == "JPMORGAN CHASE & CO. FEDERAL POLITICAL ACTION COMMITTEE" ~ "Company Pac",
             committee_name == "PORTMAN FOR SENATE COMMITTEE" ~ "REP",
             committee_name == "ACTBLUE" ~ "ActBlue",
             committee_name == "CAPITAL GROUP COMPANIES INC POLITICAL ACTION COMMITTEE; THE" ~ "Company Pac",
             committee_name == "CRAPO VICTORY COMMITTEE" ~ "REP",
             committee_name == "INVESTMENT COMPANY INSTITUTE POLITICAL ACTION COMMITTEE" ~ "ICI Pac",
             committee_name == "TEAM RYAN" ~ "REP",
             committee_name == "FRIENDS OF KELLY AYOTTE INC" ~ "REP",
             committee_name == "GEORGIANS FOR ISAKSON" ~ "REP",
             committee_name == "HILLARY ACTION FUND" ~ "DEM",
             committee_name == "KELLY VICTORY FUND" ~ "REP",
             committee_name == "MASSACHUSETTS FINANCIAL SERVICES COMPANY POLITICAL ACTION COMMITTEE (MFS PAC)" ~ "Company Pac",
             committee_name == "AYOTTE MAJORITY COMMITTEE" ~ "REP",
             committee_name == "HILLARY VICTORY FUND" ~ "DEM",
             committee_name == "KASICH FOR AMERICA INC" ~ "REP",
             committee_name == "KEEP ELECTED LEADERS LISTENING TO YOU PAC" ~ "REP",
             committee_name == "NEW DAY INDEPENDENT MEDIA COMMITTEE INC" ~ "REP",
             committee_name == "NEW DAY INDEPENDENT MEDIA COMMITTEE INC" ~ "REP",
             committee_name == "PEOPLE FOR PATTY MURRAY" ~ "DEM",
             committee_name == "LINCOLN CLUB OF NORTHERN CALIFORNIA" ~ "REP",
             committee_name == "BOEHNER FOR SPEAKER" ~ "REP",
             committee_name == "DENHAM VICTORY FUND" ~ "REP",
             committee_name == "FMR LLC POLITICAL ACTION COMMITTEE - FEDERAL (FIDELITY PAC)" ~ "Company Pac",
             committee_name == "NEW HAMPSHIRE FOR SCOTT BROWN" ~ "REP",
             committee_name == "RECLAIM AMERICA PAC" ~ "REP",
             committee_name == "RUBIO VICTORY COMMITTEE" ~ "REP",
             committee_name == "SECURITIES INDUSTRY AND FINANCIAL MARKETS ASSOCIATION POLITICAL ACTION COMMITTEE" ~ "Securities Industry and Financial Markets Association Pac",
             committee_name == "SENATE BATTLEGROUND FUND" ~ "REP",
             committee_name == "TEACHERS INSURANCE ANNUITY ASSOC OF AMERICA COLLEGE RETIREMENT EQUITIES FUND PAC TIAA-CREF" ~ "Company Pac",
             TRUE ~ as.character(CMTE_PTY_AFFILIATION)
           ))

FEC_12 <- FEC_Data %>% filter(two_year_transaction_period == "2012") %>% 
  left_join(., Coms_12, by = c("committee_id" = "CMTE_ID")) %>% 
  mutate(committee_type =
           case_when(
             committee_name == "AMERIPRISE FINANCIAL INC. POLITICAL ACTION COMMITTEE (AMERIPRISEPAC)" ~ "Company Pac",
             committee_name == "TEACHERS INSURANCE ANNUITY ASSOCIATION OF AMERICA PAC (TIAA PAC)" ~ "Company Pac",
             committee_name == "INVESCO HOLDING COMPANY (US), INC. POLITICAL ACTION COMMITTEE" ~ "Company Pac",
             committee_name == "THE VANGUARD GROUP COMMITTEE FOR RESPONSIBLE GOVERNMENT" ~ "Company Pac",
             committee_name == "INVESTMENT COMPANY INSTITUTE POLITICAL ACTION COMMITTEE (ICI PAC)" ~ "ICI Pac",
             committee_name == "THE CAPITAL GROUP COMPANIES INC POLITICAL ACTION COMMITTEE" ~ "Company Pac",
             committee_name == "AMERICAN COUNCIL OF LIFE INSURERS POLITICAL ACTION COMMITTEE" ~ "American Council of Life Insurers Pac",
             committee_name == "MOONEY VICTORY FUND" ~ "REP", 
             committee_name == "SCALISE LEADERSHIP FUND" ~ "REP",
             committee_name == "BIDEN VICTORY FUND" ~ "Dem",
             committee_name == "JACKSON HOLDINGS LLC AND JACKSON NATIONAL LIFE INSURANCE COMPANY FEDERAL SSF (JACKSON NATIONAL FEDERAL PAC)" ~ "Company Pac",
             committee_name == "JPMORGAN CHASE & CO. FEDERAL POLITICAL ACTION COMMITTEE" ~ "Company Pac",
             committee_name == "PORTMAN FOR SENATE COMMITTEE" ~ "REP",
             committee_name == "ACTBLUE" ~ "ActBlue",
             committee_name == "CAPITAL GROUP COMPANIES INC POLITICAL ACTION COMMITTEE; THE" ~ "Company Pac",
             committee_name == "CRAPO VICTORY COMMITTEE" ~ "REP",
             committee_name == "INVESTMENT COMPANY INSTITUTE POLITICAL ACTION COMMITTEE" ~ "ICI Pac",
             committee_name == "TEAM RYAN" ~ "REP",
             committee_name == "FRIENDS OF KELLY AYOTTE INC" ~ "REP",
             committee_name == "GEORGIANS FOR ISAKSON" ~ "REP",
             committee_name == "HILLARY ACTION FUND" ~ "DEM",
             committee_name == "KELLY VICTORY FUND" ~ "REP",
             committee_name == "MASSACHUSETTS FINANCIAL SERVICES COMPANY POLITICAL ACTION COMMITTEE (MFS PAC)" ~ "Company Pac",
             committee_name == "AYOTTE MAJORITY COMMITTEE" ~ "REP",
             committee_name == "HILLARY VICTORY FUND" ~ "DEM",
             committee_name == "KASICH FOR AMERICA INC" ~ "REP",
             committee_name == "KEEP ELECTED LEADERS LISTENING TO YOU PAC" ~ "REP",
             committee_name == "NEW DAY INDEPENDENT MEDIA COMMITTEE INC" ~ "REP",
             committee_name == "NEW DAY INDEPENDENT MEDIA COMMITTEE INC" ~ "REP",
             committee_name == "PEOPLE FOR PATTY MURRAY" ~ "DEM",
             committee_name == "LINCOLN CLUB OF NORTHERN CALIFORNIA" ~ "REP",
             committee_name == "BOEHNER FOR SPEAKER" ~ "REP",
             committee_name == "DENHAM VICTORY FUND" ~ "REP",
             committee_name == "FMR LLC POLITICAL ACTION COMMITTEE - FEDERAL (FIDELITY PAC)" ~ "Company Pac",
             committee_name == "NEW HAMPSHIRE FOR SCOTT BROWN" ~ "REP",
             committee_name == "RECLAIM AMERICA PAC" ~ "REP",
             committee_name == "RUBIO VICTORY COMMITTEE" ~ "REP",
             committee_name == "SECURITIES INDUSTRY AND FINANCIAL MARKETS ASSOCIATION POLITICAL ACTION COMMITTEE" ~ "Securities Industry and Financial Markets Association Pac",
             committee_name == "SENATE BATTLEGROUND FUND" ~ "REP",
             committee_name == "TEACHERS INSURANCE ANNUITY ASSOC OF AMERICA COLLEGE RETIREMENT EQUITIES FUND PAC TIAA-CREF" ~ "Company Pac",
             committee_name == "BLACKROCK CAPITAL MANAGEMENT INC. POLITICAL ACTION COMMITTEE (BLACKROCK PAC)" ~ "Company Pac",
             committee_name == "FRIENDS OF KELLY AYOTTE" ~ "REP",
             committee_name == "OBAMA VICTORY FUND 2012" ~ "DEM",
             committee_name == "ROMNEY VICTORY INC" ~ "REP",
             committee_name == "SCOTT BROWN VICTORY COMMITTEE" ~ "REP",
             committee_name == "VIRGINIA COLORADO FUND" ~ "DEM",
             TRUE ~ as.character(CMTE_PTY_AFFILIATION)
           ))

FEC_10 <- FEC_Data %>% filter(two_year_transaction_period == "2010") %>% 
  left_join(., Coms_10, by = c("committee_id" = "CMTE_ID"))  %>% 
  mutate(committee_type =
           case_when(
             committee_name == "AMERIPRISE FINANCIAL INC. POLITICAL ACTION COMMITTEE (AMERIPRISEPAC)" ~ "Company Pac",
             committee_name == "TEACHERS INSURANCE ANNUITY ASSOCIATION OF AMERICA PAC (TIAA PAC)" ~ "Company Pac",
             committee_name == "INVESCO HOLDING COMPANY (US), INC. POLITICAL ACTION COMMITTEE" ~ "Company Pac",
             committee_name == "THE VANGUARD GROUP COMMITTEE FOR RESPONSIBLE GOVERNMENT" ~ "Company Pac",
             committee_name == "INVESTMENT COMPANY INSTITUTE POLITICAL ACTION COMMITTEE (ICI PAC)" ~ "ICI Pac",
             committee_name == "THE CAPITAL GROUP COMPANIES INC POLITICAL ACTION COMMITTEE" ~ "Company Pac",
             committee_name == "AMERICAN COUNCIL OF LIFE INSURERS POLITICAL ACTION COMMITTEE" ~ "American Council of Life Insurers Pac",
             committee_name == "MOONEY VICTORY FUND" ~ "REP", 
             committee_name == "SCALISE LEADERSHIP FUND" ~ "REP",
             committee_name == "BIDEN VICTORY FUND" ~ "Dem",
             committee_name == "JACKSON HOLDINGS LLC AND JACKSON NATIONAL LIFE INSURANCE COMPANY FEDERAL SSF (JACKSON NATIONAL FEDERAL PAC)" ~ "Company Pac",
             committee_name == "JPMORGAN CHASE & CO. FEDERAL POLITICAL ACTION COMMITTEE" ~ "Company Pac",
             committee_name == "PORTMAN FOR SENATE COMMITTEE" ~ "REP",
             committee_name == "ACTBLUE" ~ "ActBlue",
             committee_name == "CAPITAL GROUP COMPANIES INC POLITICAL ACTION COMMITTEE; THE" ~ "Company Pac",
             committee_name == "CRAPO VICTORY COMMITTEE" ~ "REP",
             committee_name == "INVESTMENT COMPANY INSTITUTE POLITICAL ACTION COMMITTEE" ~ "ICI Pac",
             committee_name == "TEAM RYAN" ~ "REP",
             committee_name == "FRIENDS OF KELLY AYOTTE INC" ~ "REP",
             committee_name == "GEORGIANS FOR ISAKSON" ~ "REP",
             committee_name == "HILLARY ACTION FUND" ~ "DEM",
             committee_name == "KELLY VICTORY FUND" ~ "REP",
             committee_name == "MASSACHUSETTS FINANCIAL SERVICES COMPANY POLITICAL ACTION COMMITTEE (MFS PAC)" ~ "Company Pac",
             committee_name == "AYOTTE MAJORITY COMMITTEE" ~ "REP",
             committee_name == "HILLARY VICTORY FUND" ~ "DEM",
             committee_name == "KASICH FOR AMERICA INC" ~ "REP",
             committee_name == "KEEP ELECTED LEADERS LISTENING TO YOU PAC" ~ "REP",
             committee_name == "NEW DAY INDEPENDENT MEDIA COMMITTEE INC" ~ "REP",
             committee_name == "NEW DAY INDEPENDENT MEDIA COMMITTEE INC" ~ "REP",
             committee_name == "PEOPLE FOR PATTY MURRAY" ~ "DEM",
             committee_name == "LINCOLN CLUB OF NORTHERN CALIFORNIA" ~ "REP",
             committee_name == "BOEHNER FOR SPEAKER" ~ "REP",
             committee_name == "DENHAM VICTORY FUND" ~ "REP",
             committee_name == "FMR LLC POLITICAL ACTION COMMITTEE - FEDERAL (FIDELITY PAC)" ~ "Company Pac",
             committee_name == "NEW HAMPSHIRE FOR SCOTT BROWN" ~ "REP",
             committee_name == "RECLAIM AMERICA PAC" ~ "REP",
             committee_name == "RUBIO VICTORY COMMITTEE" ~ "REP",
             committee_name == "SECURITIES INDUSTRY AND FINANCIAL MARKETS ASSOCIATION POLITICAL ACTION COMMITTEE" ~ "Securities Industry and Financial Markets Association Pac",
             committee_name == "SENATE BATTLEGROUND FUND" ~ "REP",
             committee_name == "TEACHERS INSURANCE ANNUITY ASSOC OF AMERICA COLLEGE RETIREMENT EQUITIES FUND PAC TIAA-CREF" ~ "Company Pac",
             committee_name == "BLACKROCK CAPITAL MANAGEMENT INC. POLITICAL ACTION COMMITTEE (BLACKROCK PAC)" ~ "Company Pac",
             committee_name == "FRIENDS OF KELLY AYOTTE" ~ "REP",
             committee_name == "OBAMA VICTORY FUND 2012" ~ "DEM",
             committee_name == "ROMNEY VICTORY INC" ~ "REP",
             committee_name == "SCOTT BROWN VICTORY COMMITTEE" ~ "REP",
             committee_name == "VIRGINIA COLORADO FUND" ~ "DEM",
             committee_name == "FMR LLC POLITICAL ACTION COMMITTEE (FIDELITY PAC)" ~ "Company Pac",
             committee_name == "JPMORGAN CHASE & CO. PAC" ~ "Company Pac",
             committee_name == "THE VANGUARD GROUP COMMITTEE FOR RESPONSIBLE GOVERNMENT (VANGUARD COMMITTEE FOR RESPONSIBL" ~ "Company Pac",
             TRUE ~ as.character(CMTE_PTY_AFFILIATION)
           ))

FEC_08 <- FEC_Data %>% filter(two_year_transaction_period == "2008") %>% 
  left_join(., Coms_08, by = c("committee_id" = "CMTE_ID"))  %>% 
  mutate(committee_type =
           case_when(
             committee_name == "AMERIPRISE FINANCIAL INC. POLITICAL ACTION COMMITTEE (AMERIPRISEPAC)" ~ "Company Pac",
             committee_name == "TEACHERS INSURANCE ANNUITY ASSOCIATION OF AMERICA PAC (TIAA PAC)" ~ "Company Pac",
             committee_name == "INVESCO HOLDING COMPANY (US), INC. POLITICAL ACTION COMMITTEE" ~ "Company Pac",
             committee_name == "THE VANGUARD GROUP COMMITTEE FOR RESPONSIBLE GOVERNMENT" ~ "Company Pac",
             committee_name == "INVESTMENT COMPANY INSTITUTE POLITICAL ACTION COMMITTEE (ICI PAC)" ~ "ICI Pac",
             committee_name == "THE CAPITAL GROUP COMPANIES INC POLITICAL ACTION COMMITTEE" ~ "Company Pac",
             committee_name == "AMERICAN COUNCIL OF LIFE INSURERS POLITICAL ACTION COMMITTEE" ~ "American Council of Life Insurers Pac",
             committee_name == "MOONEY VICTORY FUND" ~ "REP", 
             committee_name == "SCALISE LEADERSHIP FUND" ~ "REP",
             committee_name == "BIDEN VICTORY FUND" ~ "Dem",
             committee_name == "JACKSON HOLDINGS LLC AND JACKSON NATIONAL LIFE INSURANCE COMPANY FEDERAL SSF (JACKSON NATIONAL FEDERAL PAC)" ~ "Company Pac",
             committee_name == "JPMORGAN CHASE & CO. FEDERAL POLITICAL ACTION COMMITTEE" ~ "Company Pac",
             committee_name == "PORTMAN FOR SENATE COMMITTEE" ~ "REP",
             committee_name == "ACTBLUE" ~ "ActBlue",
             committee_name == "CAPITAL GROUP COMPANIES INC POLITICAL ACTION COMMITTEE; THE" ~ "Company Pac",
             committee_name == "CRAPO VICTORY COMMITTEE" ~ "REP",
             committee_name == "INVESTMENT COMPANY INSTITUTE POLITICAL ACTION COMMITTEE" ~ "ICI Pac",
             committee_name == "TEAM RYAN" ~ "REP",
             committee_name == "FRIENDS OF KELLY AYOTTE INC" ~ "REP",
             committee_name == "GEORGIANS FOR ISAKSON" ~ "REP",
             committee_name == "HILLARY ACTION FUND" ~ "DEM",
             committee_name == "KELLY VICTORY FUND" ~ "REP",
             committee_name == "MASSACHUSETTS FINANCIAL SERVICES COMPANY POLITICAL ACTION COMMITTEE (MFS PAC)" ~ "Company Pac",
             committee_name == "AYOTTE MAJORITY COMMITTEE" ~ "REP",
             committee_name == "HILLARY VICTORY FUND" ~ "DEM",
             committee_name == "KASICH FOR AMERICA INC" ~ "REP",
             committee_name == "KEEP ELECTED LEADERS LISTENING TO YOU PAC" ~ "REP",
             committee_name == "NEW DAY INDEPENDENT MEDIA COMMITTEE INC" ~ "REP",
             committee_name == "NEW DAY INDEPENDENT MEDIA COMMITTEE INC" ~ "REP",
             committee_name == "PEOPLE FOR PATTY MURRAY" ~ "DEM",
             committee_name == "LINCOLN CLUB OF NORTHERN CALIFORNIA" ~ "REP",
             committee_name == "BOEHNER FOR SPEAKER" ~ "REP",
             committee_name == "DENHAM VICTORY FUND" ~ "REP",
             committee_name == "FMR LLC POLITICAL ACTION COMMITTEE - FEDERAL (FIDELITY PAC)" ~ "Company Pac",
             committee_name == "NEW HAMPSHIRE FOR SCOTT BROWN" ~ "REP",
             committee_name == "RECLAIM AMERICA PAC" ~ "REP",
             committee_name == "RUBIO VICTORY COMMITTEE" ~ "REP",
             committee_name == "SECURITIES INDUSTRY AND FINANCIAL MARKETS ASSOCIATION POLITICAL ACTION COMMITTEE" ~ "Securities Industry and Financial Markets Association Pac",
             committee_name == "SENATE BATTLEGROUND FUND" ~ "REP",
             committee_name == "TEACHERS INSURANCE ANNUITY ASSOC OF AMERICA COLLEGE RETIREMENT EQUITIES FUND PAC TIAA-CREF" ~ "Company Pac",
             committee_name == "BLACKROCK CAPITAL MANAGEMENT INC. POLITICAL ACTION COMMITTEE (BLACKROCK PAC)" ~ "Company Pac",
             committee_name == "FRIENDS OF KELLY AYOTTE" ~ "REP",
             committee_name == "OBAMA VICTORY FUND 2012" ~ "DEM",
             committee_name == "ROMNEY VICTORY INC" ~ "REP",
             committee_name == "SCOTT BROWN VICTORY COMMITTEE" ~ "REP",
             committee_name == "VIRGINIA COLORADO FUND" ~ "DEM",
             committee_name == "FMR LLC POLITICAL ACTION COMMITTEE (FIDELITY PAC)" ~ "Company Pac",
             committee_name == "JPMORGAN CHASE & CO. PAC" ~ "Company Pac",
             committee_name == "THE VANGUARD GROUP COMMITTEE FOR RESPONSIBLE GOVERNMENT (VANGUARD COMMITTEE FOR RESPONSIBL" ~ "Company Pac",
             committee_name == "INVESCO PLC PAC" ~ "Company Pac",
             committee_name == "OBAMA VICTORY FUND" ~ "DEM",
             committee_name == "VANGUARD GROUP COMMITTEE FOR RESPONSIBLE GOVERNMENT (VANGUARD COMMITTEE FOR RESPONSIBL, TH" ~ "Company Pac",
             TRUE ~ as.character(CMTE_PTY_AFFILIATION)
           ))

FEC_08_to_20 <- full_join(FEC_20, FEC_18) %>% 
  full_join(., FEC_16) %>% 
  full_join(., FEC_14) %>% 
  full_join(., FEC_12) %>% 
  full_join(., FEC_10) %>% 
  full_join(., FEC_08)

write_csv(FEC_08_to_20, "FEC/FEC_08_to_20.csv")
