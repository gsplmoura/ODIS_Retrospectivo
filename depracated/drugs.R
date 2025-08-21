# DRUGS DATA WRANGLING

# Load libraries ----------------------------------------------------------
library(tidyverse)
library(here)
library(readxl)
library(janitor)

# Load data ---------------------------------------------------------------
## ODIS prescriptions for all medications [@ISS]
drugs_ODIS <- read_csv( 
  here("data","drugs_ODIS.csv")) %>%
    janitor::clean_names() %>%
  mutate(
    dta_receita = lubridate::dmy(dta_receita),
    across(where(is.character), tolower)
  )

## ODIS prescriptions for standard coded/medications [@ISS]
drugs_standards <- readxl::read_xlsx(
  here("data","drugs_standards.xlsx"),
  sheet = "drugs_standards_ISS") %>%
  janitor::clean_names() %>%
  mutate(
    across(where(is.character), tolower)
  )

## ODIS prescriptions for NON-standard medications (NON-coded) [@ISS]  
drugs_uncoded <- readxl::read_xlsx(
  here("data","drugs_uncoded.xlsx"), 
  sheet = "drugs_uncoded_ISS") %>% 
  janitor::clean_names() %>%
  mutate(
    across(where(is.character), tolower)
  )

## Drug classes encompassing all prescriptions [@ISS]
drugs_classes <- readxl::read_xlsx(
  here("data","drugs_classes_isabela.xlsx")) %>% 
  janitor::clean_names() %>%
  mutate(
    across(where(is.character), tolower)
  )
  
# Lookup table ----------------------------------------------------------
## Create a lookup table where each unique cod_novo group gets a single integer
lookup_table <- drugs_standards %>%
  filter(!is.na(cod_novo)) %>%              # Remove NA values
  mutate(cod_novo_group = cod_novo) %>%     # Preserve the original group for mapping
  separate_rows(cod_novo, sep = ",") %>%   # Split multi-code entries into rows
  distinct(cod_novo_group, cod_novo) %>%    # Keep unique (group, code) pairs
  group_by(cod_novo_group) %>%              # Group by original cod_novo group
  mutate(drug_code = cur_group_id()) %>%    # Assign the same ID to all values in the same group
  ungroup() %>% 
  mutate(
    cod_novo = trimws(cod_novo)
  )

# Step 1: Identify standard names not found in nomes_agrupados
unmatched_drugs <- drugs_uncoded %>%
  filter(!standard_name %in% unlist(str_split(drugs_standards$nomes_agrupados, pattern = " \\| ")))

# Step 2: View unmatched standard names
print(unmatched_drugs)
