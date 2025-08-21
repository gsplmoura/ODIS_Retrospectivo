rm(list=ls()) # Clear existing data and graphics
graphics.off()
cat("\014")  # Clear any pending RStudio sessions or temporary files
library(tidyverse)
library(stringr)
library(stringi)
setwd('/Users/gustavosplmoura/Library/Mobile Documents/com~apple~CloudDocs/Medicina/Biblioteca/Research/Data Science/Data Science/PROJECTS/ODIS_Retrospectivo')

# 1.ALL ODIS PRESCRIPTIONS
ODIS <- read_csv("2_Data_1_Original_240411_3_XLSX_Original_5_Receitas.csv") |> 
    filter(
        SGL_ESPECIALIDADE_HC == "ODIS" & !is.na(NOM_MEDICAMENTO)
    ) |>
    # Normalize strings
    mutate(
        NOM_MEDICAMENTO_CLEAN = tolower(      # Convert to lowercase
            str_trim(                  # Remove leading/trailing spaces
                stri_trans_general(NOM_MEDICAMENTO, "Latin-ASCII") # Remove accents
            )
        )
    ) |>
    mutate(
        MED_NAME = str_extract(NOM_MEDICAMENTO_CLEAN, "^[^0-9]+"), 
        # Extract everything before the first number
        DOSAGE = str_extract(NOM_MEDICAMENTO_CLEAN, "[0-9].*$")   
        # Extract everything starting from the first number
    ) |> 
    select(
        -c(SGL_ESPECIALIDADE_HC:NOM_ESPECIALIDADE_HC)
    ) |> 
    relocate(
        COD_PACIENTE, DTA_RECEITA, COD_MEDICAMENTO, 
        NOM_MEDICAMENTO, NOM_MEDICAMENTO_CLEAN, 
        MED_NAME, DOSAGE, 
        DSC_POSOLOGIA, DSC_APRESENTACAO, DSC_TIPO_USO, DSC_USO_CONTINUO, 
        OBS_MEDICAMENTO
    )

write_csv(ODIS,"5_Receitas_ODIS.csv", na = "")


# 2. UNIQUE STANDARDIZED MEDICATIONS 
# Unique medications prescribed (name/code pairs: 242)
# Note: excluding rows with missing medication codes
unique_medications <- ODIS |> 
    filter(!is.na(COD_MEDICAMENTO)) |> 
    distinct(COD_MEDICAMENTO, MED_NAME)
write_csv(unique_medications,"medicamentos_utilizados.csv", na = "")

# MEDICATION CODE ABSENT (Non-standardized prescriptions)
## 1385 rows (16.4%)
cod_NA <-  ODIS |>
    filter(
        is.na(COD_MEDICAMENTO)
    ) |> 
    count(
        MED_NAME
    ) |>
    arrange(
        MED_NAME
    )
write_csv(cod_NA,"medicamentos_sem_codigo.csv", na = "")

## Many medications without a code are the same, with small variations in...

accentuation, white spaces, typos, etc.

Attempted solution:
 
cod_NA <- ODIS |>
     filter(is.na(COD_MEDICAMENTO)) |>  # Keep rows with NA in COD_MEDICAMENTO
     # Normalize strings
     mutate(
         MED_NAME_CLEAN = tolower(      # Convert to lowercase
             str_trim(                  # Remove leading/trailing spaces
                 stri_trans_general(MED_NAME, "Latin-ASCII") # Remove accents
             )
         )
     ) |>
     count(MED_NAME_CLEAN) |>           # Count occurrences of unique names
     arrange(MED_NAME_CLEAN)            # Arrange alphabetically
 
 # Step 2: Calculate string distances
 dist_matrix <- stringdist::stringdistmatrix(cod_NA$MED_NAME_CLEAN, method = "osa")
 
 # Step 3: Perform hierarchical clustering
 hc <- hclust(as.dist(dist_matrix), method = "complete")
 
 Step 4: Define clusters
# AJUSTE DE SENSIBILIDADE
# 'h' determines similarity threshold
# adjust 'h' for sensitivity: A higher h (h = 3 or h = 4) will group more names
# together because it allows for greater "distance" between strings. 
# 'h' = 3 | 4 did not yield good results
# Sensibilidade 2
# cod_NA$CLUSTER_2 <- cutree(hc, h = 2)  # 'h' determines similarity threshold
# Sensibilidade 3
#cod_NA$CLUSTER_3 <- cutree(hc, h = 3)  # 'h' determines similarity threshold
# Sensibilidade 4
#cod_NA$CLUSTER_4 <- cutree(hc, h = 4)  # 'h' determines similarity threshold
 Step 5: Summarize results
# Sensibilidade 2 
 cod_NA_S2 <- cod_NA |> 
     group_by(CLUSTER_2) |> 
     summarise(
         REPRESENTATIVE_NAME = first(MED_NAME_CLEAN),
         NAMES_IN_GROUP = paste(unique(MED_NAME_CLEAN), collapse = ", ")
     )
### Sensibilidade 3
#cod_NA_S3 <- cod_NA |> 
#    group_by(CLUSTER_3) |> 
#    summarise(
#        REPRESENTATIVE_NAME = first(MED_NAME_CLEAN),
#        NAMES_IN_GROUP = paste(unique(MED_NAME_CLEAN), collapse = ", ")
#    )
#
### Sensibilidade 4
#cod_NA_S4 <- cod_NA |> 
#    group_by(CLUSTER_4) |> 
#    summarise(
#        REPRESENTATIVE_NAME = first(MED_NAME_CLEAN),
#        NAMES_IN_GROUP = paste(unique(MED_NAME_CLEAN), collapse = ", ")
#    )

# Define stopwords and escape special characters
stopwords <- c(
    "comprimido", "injetavel", "oral", "via oral", "gota", "gotas", "creme", 
    "capsula", "drageas", "pomada", "spray", "envelope", "barreira", "solucao", 
    "colirio", "ampola", "sache", "lavavel", "frasco", "glicemico", "\\-", "\\("
)

# Create a regex pattern from the stopwords
stopwords_pattern <- paste0("\\b(", paste(stopwords, collapse = "|"), ")\\b")

# Clean and group medication names
cod_NA_SW <- ODIS |>
    filter(is.na(COD_MEDICAMENTO)) |>
    # Normalize strings
    mutate(
        MED_NAME_CLEAN = str_to_lower(                       # Convert to lowercase
            str_trim(                                        # Remove leading/trailing spaces
                stri_trans_general(MED_NAME, "Latin-ASCII")  # Remove accents
            )
        ),
        MED_NAME_CLEAN = str_remove_all(MED_NAME_CLEAN, stopwords_pattern),  # Remove stopwords
        MED_NAME_CLEAN = str_squish(MED_NAME_CLEAN)          # Remove extra spaces
    ) |>
    count(MED_NAME_CLEAN) |>
    arrange(MED_NAME_CLEAN)

# Step 2: Calculate string distances
dist_matrix <- stringdist::stringdistmatrix(cod_NA_SW$MED_NAME_CLEAN, method = "osa")

# Step 3: Perform hierarchical clustering
hc <- hclust(as.dist(dist_matrix), method = "complete")

# Step 4: Define clusters
cod_NA_SW$CLUSTER_2 <- cutree(hc, h = 2)  # 'h' determines similarity threshold

# Step 5: Summarize results
cod_NA_SW_grouped <- cod_NA_SW |> 
    group_by(CLUSTER_2) |> 
    summarise(
        REPRESENTATIVE_NAME = first(MED_NAME_CLEAN),
        NAMES_IN_GROUP = paste(unique(MED_NAME_CLEAN), collapse = ", ")
    )
