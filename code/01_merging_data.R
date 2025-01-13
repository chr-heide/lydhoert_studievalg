# Setup -------------------------------------------------------------------

# Script that merges the different datasets into one dataset
# Cleaning data into desired format as a part of merging

# Libraries
library(tidyverse)
library(readxl)
library(stringr)


# UFM data ----------------------------------------------------------------

ufm_df <- read_xlsx("data/raw/KOT_søgning_alder_eksamenskvotient.xlsx",
                    na = c("", "0"))


ufm_df <- ufm_df |> 
  # Renaming columns
  rename(
    år = Ansøgningsår,
    uddannelse = `KOT Optagelsesområde`,
    kvote_1 = `Behandlet i kvote 1`,
    kvote_2 = `Behandlet i kvote 2`,
    køn = Køn,
    ansøgninger_prio1 = `1. prioritets ansøgninger`,
    ansøgninger_total = Ansøgninger,
    gennemsnitskarakter = `Gennemsnits eksamenskvotient KGY`,
    mediankarakter = `Median eksamenskvotient KGY`,
    gennemsnitsalder = `Gennemsnits alder`) |> 
  
  # Obvious data problems with average age! Quick fix to just remove values below
  # 18 and replace them with NA ..
  mutate(gennemsnitsalder = if_else(
    gennemsnitsalder < 18,
    NA,
    gennemsnitsalder)) |> 
  
  # Changing kvote-variables to booleans
  mutate(
    kvote_1 = if_else(
      kvote_1 == "Behandlet i kvote 1: Ja",
      TRUE,
      FALSE),
    kvote_2 = if_else(
      kvote_2 == "Behandlet i kvote 2: Ja",
      TRUE,
      FALSE)
  )

head(ufm_df)

# Making it wide to get rows to be unit pr. year
ufm_df_wide <- ufm_df |> 
  pivot_wider(names_from = c(køn, kvote_1, kvote_2), 
              values_from = c(ansøgninger_prio1, ansøgninger_total, gennemsnitskarakter, mediankarakter, gennemsnitsalder),
              names_sep = "_")

# Cleaning the wide data
ufm_df_clean <- ufm_df_wide |> 
  mutate(
    # Keeping identifiers
    år = år,
    uddannelse = uddannelse,
    
    # Calculating totals
    ansøgninger_total = rowSums(
      select(cur_data(), contains("total")),
      na.rm = TRUE),
    ansøgninger_prio1 = rowSums(
      select(cur_data(), contains("prio1")),
      na.rm = TRUE),
    ansøgninger_total_kvote2 = rowSums(
      select(cur_data(), contains("total") & ends_with("TRUE")),
      na.rm = TRUE),
    ansøgninger_prio1_kvote2 = rowSums(
      select(cur_data(), contains("prio1") & ends_with("TRUE")),
      na.rm = TRUE),
    
    # Calculating totals for women
    kvinder_total = rowSums(
      select(cur_data(), contains("total") & contains("Kvinder")),
      na.rm = TRUE),
    kvinder_prio1 = rowSums(
      select(cur_data(), contains("prio1") & contains("Kvinder")),
      na.rm = TRUE),
    kvinder_total_kvote2 = rowSums(
      select(cur_data(), contains("total") & ends_with("TRUE") & contains("Kvinder")),
      na.rm = TRUE),
    kvinder_prio1_kvote2 = rowSums(
      select(cur_data(), contains("prio1") & ends_with("TRUE") & contains("Kvinder")),
      na.rm = TRUE),
    
    # Calculating totals for men
    mænd_total = rowSums(
      select(cur_data(), contains("total") & contains("Mænd")),
      na.rm = TRUE),
    mænd_prio1 = rowSums(
      select(cur_data(), contains("prio1") & contains("Mænd")),
      na.rm = TRUE),
    mænd_total_kvote2 = rowSums(
      select(cur_data(), contains("total") & ends_with("TRUE") & contains("Mænd")),
      na.rm = TRUE),
    mænd_prio1_kvote2 = rowSums(
      select(cur_data(), contains("prio1") & ends_with("TRUE") & contains("Mænd")),
      na.rm = TRUE),
    
    # Getting grade-columns by gender and quota
    # Note that this results in NaNs for groups that are 0.
    karakter_kvinder_kvote1 = gennemsnitskarakter_Kvinder_TRUE_FALSE,
    karakter_kvinder_kvote2 = (
      replace_na(gennemsnitskarakter_Kvinder_FALSE_TRUE, 0) * (
        replace_na(ansøgninger_total_Kvinder_FALSE_TRUE, 0) / replace_na(kvinder_total_kvote2, 1)) 
      + replace_na(gennemsnitskarakter_Kvinder_TRUE_TRUE, 0) * (
        replace_na(ansøgninger_total_Kvinder_TRUE_TRUE, 0) / replace_na(kvinder_total_kvote2, 1))),
    karakter_mænd_kvote1 = gennemsnitskarakter_Mænd_TRUE_FALSE,
    karakter_mænd_kvote2 = (
      replace_na(gennemsnitskarakter_Mænd_FALSE_TRUE, 0) * (
        replace_na(ansøgninger_total_Mænd_FALSE_TRUE, 0) / replace_na(mænd_total_kvote2, 1)) 
      + replace_na(gennemsnitskarakter_Mænd_TRUE_TRUE, 0) * (
        replace_na(ansøgninger_total_Mænd_TRUE_TRUE, 0) / replace_na(mænd_total_kvote2, 1))),
    
    # Replacing NaNs with NAs
    across(starts_with("karakter"), ~ ifelse(is.nan(.), NA, .)),
    
    # Getting age-columns by gender and quota
    alder_kvinder_kvote1 = gennemsnitsalder_Kvinder_TRUE_FALSE,
    alder_kvinder_kvote2 = (
      gennemsnitsalder_Kvinder_FALSE_TRUE * (ansøgninger_total_Kvinder_FALSE_TRUE / kvinder_total_kvote2)
      + gennemsnitsalder_Kvinder_TRUE_TRUE * (ansøgninger_total_Kvinder_TRUE_TRUE / kvinder_total_kvote2)),
    alder_mænd_kvote1 = gennemsnitsalder_Mænd_TRUE_FALSE,
    alder_mænd_kvote2 = (
      gennemsnitsalder_Mænd_FALSE_TRUE * (ansøgninger_total_Mænd_FALSE_TRUE / mænd_total_kvote2)
      + gennemsnitsalder_Mænd_TRUE_TRUE * (ansøgninger_total_Mænd_TRUE_TRUE / mænd_total_kvote2)),
    
    # Replacing NaNs with NAs
    across(starts_with("alder"), ~ ifelse(is.nan(.), NA, .)),
    
    # Removing redundant columns
    .keep = "none") |> 
  
  # Making columns with gender shares
  # Note this results in division by 0 some places - but I'll likely exclude these
  # later as these are all small educations. 
  mutate(
    andel_kvinder_total = kvinder_total / ansøgninger_total,
    andel_mænd_total = mænd_total / ansøgninger_total,
    andel_kvinder_prio1 = kvinder_prio1 / ansøgninger_prio1,
    andel_mænd_prio1 = mænd_prio1 / ansøgninger_prio1,
    andel_kvinder_total_kvote2 = kvinder_total_kvote2 / ansøgninger_total_kvote2,
    andel_mænd_total_kvote2 = mænd_total_kvote2 / ansøgninger_total_kvote2,
    andel_kvinder_prio1_kvote2 = kvinder_prio1_kvote2 / ansøgninger_prio1_kvote2,
    andel_mænd_prio1_kvote2 = mænd_prio1_kvote2 / ansøgninger_prio1_kvote2, 
  
    # Replacing NaNs with NAs. These are due to division by 0
    across(starts_with("andel"), ~ ifelse(is.nan(.), NA, .))) |> 

  
  # Calculating weighted averages of grades and age across genders
  mutate(
    # Grade
    karakter_kvote1 = (
      karakter_mænd_kvote1 *
        ((mænd_total - mænd_total_kvote2) /
           (ansøgninger_total - ansøgninger_total_kvote2)) +
        karakter_kvinder_kvote1 *
        ((kvinder_total - kvinder_total_kvote2) /
           (ansøgninger_total - ansøgninger_total_kvote2))),
    karakter_kvote2 = (
      karakter_mænd_kvote2 * andel_mænd_total_kvote2 +
        karakter_kvinder_kvote2 * andel_kvinder_total_kvote2),
    karakter = (
      karakter_kvote1 *
        ((ansøgninger_total - ansøgninger_total_kvote2) / ansøgninger_total) +
        karakter_kvote2 * (ansøgninger_total_kvote2 / ansøgninger_total)),
      
      # Age
      alder_kvote1 = (
        alder_mænd_kvote1 *
          ((mænd_total - mænd_total_kvote2) /
             (ansøgninger_total - ansøgninger_total_kvote2)) +
          alder_kvinder_kvote1 *
          ((kvinder_total - kvinder_total_kvote2) /
             (ansøgninger_total - ansøgninger_total_kvote2))),
      alder_kvote2 = (
        alder_mænd_kvote2 * andel_mænd_total_kvote2 +
          alder_kvinder_kvote2 * andel_kvinder_total_kvote2),
      alder = (
        alder_kvote1 *
          ((ansøgninger_total - ansøgninger_total_kvote2) / ansøgninger_total) +
          alder_kvote2 * (ansøgninger_total_kvote2 / ansøgninger_total)
    )) |> 
  
  # Removing columns with gender totals - can be calculated based on the share if
  # needed
  select(-c(kvinder_total, mænd_total, kvinder_prio1, mænd_prio1, kvinder_total_kvote2,
            mænd_total_kvote2, kvinder_prio1_kvote2, mænd_prio1_kvote2))

# Adding an "uddannelse_kort" column to be used for matching with the other
# datasets
ufm_df_clean$uddannelse_kort <- sub(",.*", "", ufm_df_clean$uddannelse)

# And converting year to numeric
ufm_df_clean$år <- as.numeric(ufm_df_clean$år)

saveRDS(ufm_df_clean, "data/processed/individual_datasets/2009-2024.RDS")

# Pre-2009 data -----------------------------------------------------------

# First I am saving a seperate dataset for each year. I cannot automate this,
# as the excel-files vary in format for each year..


## 2008 --------------------------------------------------------------------

df_2008 <- read_excel("data/raw/prio1_ansøgere_koen/Ansøgere_kone_2008.xls", skip = 3) |> 
  rename(id = ...1,
         uddannelse_kort = ...2) |> 
  filter(!is.na(`Sum omr.`)) |> 
  group_by(uddannelse_kort) |> 
  summarise(ansøgninger_prio1 = sum(`Sum omr.`),
            andel_kvinder_prio1 = sum(Kvinder) / ansøgninger_prio1,
            andel_mænd_prio1 = sum(Mænd) / ansøgninger_prio1) |> 
  ungroup() |> 
  filter(!is.na(uddannelse_kort)) |> 
  mutate(år = 2008)

## 2007 --------------------------------------------------------------------

df_2007 <- read_excel("data/raw/prio1_ansøgere_koen/Ansøgere_kone_2007.xls", skip = 3) |> 
  rename(id = ...1,
         uddannelse_kort = ...2) |> 
  filter(!is.na(`Sum omr.`)) |> 
  group_by(uddannelse_kort) |> 
  summarise(ansøgninger_prio1 = sum(`Sum omr.`),
            andel_kvinder_prio1 = sum(Kvinder) / ansøgninger_prio1,
            andel_mænd_prio1 = sum(Mænd) / ansøgninger_prio1) |> 
  ungroup() |> 
  filter(!is.na(uddannelse_kort) & uddannelse_kort != "I alt")|> 
  mutate(år = 2007)

## 2006 --------------------------------------------------------------------

df_2006 <- read_excel("data/raw/prio1_ansøgere_koen/Ansøgere_kone_2006.xls", skip = 3) |> 
  rename(id = kode,
         uddannelse_kort = optagelsesområde) |> 
  filter(!is.na(sum)) |> 
  group_by(uddannelse_kort) |> 
  summarise(ansøgninger_prio1 = sum(sum),
            andel_kvinder_prio1 = sum(kvinder) / ansøgninger_prio1,
            andel_mænd_prio1 = sum(mænd) / ansøgninger_prio1) |> 
  ungroup() |> 
  filter(!is.na(uddannelse_kort) & uddannelse_kort != "I alt")|> 
  mutate(år = 2006)

## 2005 --------------------------------------------------------------------

df_2005 <- read_excel("data/raw/prio1_ansøgere_koen/Ansøgere_kone_2005.xls", skip = 3) |> 
  rename(id = kode,
         uddannelse_kort = optagelsesområde) |> 
  filter(!is.na(sum)) |> 
  group_by(uddannelse_kort) |> 
  summarise(ansøgninger_prio1 = sum(sum),
            andel_kvinder_prio1 = sum(kvinder) / ansøgninger_prio1,
            andel_mænd_prio1 = sum(mænd) / ansøgninger_prio1) |> 
  ungroup() |> 
  filter(!is.na(uddannelse_kort) & uddannelse_kort != "I alt")|> 
  mutate(år = 2005)


## 2004 --------------------------------------------------------------------

df_2004 <- read_excel("data/raw/prio1_ansøgere_koen/Ansøgere_kone_2004.xls", skip = 3) |> 
  rename(id = kode,
         uddannelse_kort = optagelsesområde) |> 
  filter(!is.na(sum)) |> 
  group_by(uddannelse_kort) |> 
  summarise(ansøgninger_prio1 = sum(sum),
            andel_kvinder_prio1 = sum(kvinder) / ansøgninger_prio1,
            andel_mænd_prio1 = sum(mænd) / ansøgninger_prio1) |> 
  ungroup() |> 
  filter(!is.na(uddannelse_kort) & uddannelse_kort != "I alt")|> 
  mutate(år = 2004)

## 2003 --------------------------------------------------------------------

df_2003 <- read_excel("data/raw/prio1_ansøgere_koen/Ansøgere_kone_2003.xls", skip = 3) |> 
  rename(id = kode,
         uddannelse_kort = optagelsesområde) |> 
  filter(!is.na(sum)) |> 
  group_by(uddannelse_kort) |> 
  summarise(ansøgninger_prio1 = sum(sum),
            andel_kvinder_prio1 = sum(kvinder) / ansøgninger_prio1,
            andel_mænd_prio1 = sum(mænd) / ansøgninger_prio1) |> 
  ungroup() |> 
  filter(!is.na(uddannelse_kort) & uddannelse_kort != "Den Koordinerede Tilmelding i alt")|> 
  mutate(år = 2003)

## 2002 --------------------------------------------------------------------

df_2002 <- read_excel("data/raw/prio1_ansøgere_koen/Ansøgere_kone_2002.xls", skip = 3) |> 
  rename(id = Opt.omr.,
         uddannelse_kort = `Inst/udd.`) |> 
  filter(!is.na(alle)) |> 
  group_by(uddannelse_kort) |> 
  summarise(ansøgninger_prio1 = sum(alle),
            andel_kvinder_prio1 = sum(kvinder) / ansøgninger_prio1,
            andel_mænd_prio1 = sum(mænd) / ansøgninger_prio1) |> 
  ungroup() |> 
  filter(!is.na(uddannelse_kort) & uddannelse_kort != "Den Koordinerede Tilmelding i alt")|> 
  mutate(år = 2002)

## 2001 --------------------------------------------------------------------

df_2001 <- read_excel("data/raw/prio1_ansøgere_koen/Ansøgere_kone_2001.xls", skip = 3) |> 
  rename(id = Opt.omr.,
         uddannelse_kort = `Institution/uddannelse`) |> 
  filter(!is.na(`Sum Omr`)) |> 
  group_by(uddannelse_kort) |> 
  summarise(ansøgninger_prio1 = sum(`Sum Omr`),
            andel_kvinder_prio1 = sum(Kvinder) / ansøgninger_prio1,
            andel_mænd_prio1 = sum(Mænd) / ansøgninger_prio1) |> 
  ungroup() |> 
  filter(!is.na(uddannelse_kort) & uddannelse_kort != "Sum") |> 
  mutate(år = 2001)

## 2000 --------------------------------------------------------------------

df_2000 <- read_excel("data/raw/prio1_ansøgere_koen/Ansøgere_kone_2000.xls", skip = 3) |> 
  rename(id = Opt.omr.,
         uddannelse_kort = `Institution/uddannelse`) |> 
  filter(!is.na(`Sum Omr`)) |> 
  group_by(uddannelse_kort) |> 
  summarise(ansøgninger_prio1 = sum(`Sum Omr`),
            andel_kvinder_prio1 = sum(Kvinder) / ansøgninger_prio1,
            andel_mænd_prio1 = sum(Mænd) / ansøgninger_prio1) |> 
  ungroup() |> 
  filter(!is.na(uddannelse_kort) & uddannelse_kort != "Sum") |> 
  mutate(år = 2000)

## 1999 --------------------------------------------------------------------

df_1999 <- read_excel("data/raw/prio1_ansøgere_koen/Ansøgere_kone_1999.xls", skip = 3) |> 
  rename(id = Opt.omr.,
         uddannelse_kort = `Institution/uddannelse`) |> 
  filter(!is.na(`Sum Omr`)) |> 
  group_by(uddannelse_kort) |> 
  summarise(ansøgninger_prio1 = sum(`Sum Omr`),
            andel_kvinder_prio1 = sum(Kvinder) / ansøgninger_prio1,
            andel_mænd_prio1 = sum(Mænd) / ansøgninger_prio1) |> 
  ungroup() |> 
  filter(!is.na(uddannelse_kort) & uddannelse_kort != "Sum") |> 
  mutate(år = 1999)

## 1998 --------------------------------------------------------------------

df_1998 <- read_excel("data/raw/prio1_ansøgere_koen/Ansøgere_kone_1998.xls", skip = 3) |> 
  rename(id = Opt.omr.,
         uddannelse_kort = `Institution/uddannelse`) |> 
  filter(!is.na(Sum)) |> 
  group_by(uddannelse_kort) |> 
  summarise(ansøgninger_prio1 = sum(Sum),
            andel_kvinder_prio1 = sum(Kvinder) / ansøgninger_prio1,
            andel_mænd_prio1 = sum(Mænd) / ansøgninger_prio1) |> 
  ungroup() |> 
  filter(!is.na(uddannelse_kort) & uddannelse_kort != "Sum") |> 
  mutate(år = 1998)

## 1997 --------------------------------------------------------------------

df_1997 <- read_excel("data/raw/prio1_ansøgere_koen/Ansøgere_kone_1997.xls", skip = 3) |> 
  rename(id = Opt.omr.,
         uddannelse_kort = `Institution/uddannelse`) |> 
  filter(!is.na(Sum)) |> 
  group_by(uddannelse_kort) |> 
  summarise(ansøgninger_prio1 = sum(Sum),
            andel_kvinder_prio1 = sum(Kvinder) / ansøgninger_prio1,
            andel_mænd_prio1 = sum(Mænd) / ansøgninger_prio1) |> 
  ungroup() |> 
  filter(!is.na(uddannelse_kort) & uddannelse_kort != "Sum") |> 
  mutate(år = 1997)


## Binding all years ------------------------------------------------------

df_pre2009 <- bind_rows(
  df_1997,
  df_1998,
  df_1999,
  df_2000,
  df_2001,
  df_2002,
  df_2003,
  df_2004,
  df_2005,
  df_2006,
  df_2007,
  df_2008
)



# Fixing names ------------------------------------------------------------

# Theres a discrepancy in how the education's are named in the different datasets
# To ensure that all are named consistently throughout the period, I fix the 
# names in the pre 2009 data to be consistent with the naming post 2009.


## Removing parentheses ---------------------------------------------------

# For some educations there is a parantheses at the end indicating the place,
# which is not needed since I'm working with aggregate data. I therefore remove
# these

df_pre2009$uddannelse_kort <- sub(" \\(.*\\)", "", df_pre2009$uddannelse_kort)
df_pre2009$uddannelse_kort <- sub("\\(.*\\)", "", df_pre2009$uddannelse_kort)


## Identifying inconsistencies --------------------------------------------

# Unmatched educations in ufm_df
unmatched_post <- ufm_df_clean |> 
  group_by(uddannelse_kort) |> 
  summarise(total = sum(ansøgninger_prio1)) |> 
  anti_join(
    df_pre2009 |> 
      group_by(uddannelse_kort) |> 
      summarise(count2 = n())
  )

# Unmatched educations in pre2009
unmatched_pre <- df_pre2009 |> 
  group_by(uddannelse_kort) |> 
  summarise(total = sum(ansøgninger_prio1)) |> 
  anti_join(
    ufm_df_clean |> 
      group_by(uddannelse_kort) |> 
      summarise(count2 = n())
  )


## Creating map -----------------------------------------------------------

# I create a named vector to work as a map. The first value is the old name,
# and the second value is the value to replace the old one with.

names_map <- c(
  "Lægevidenskab" = "Medicin",
  "Statskundskab/Samfundsfag" = "Statskundskab",
  "Almen Erhvervsøkonomi, HA" = "Erhvervsøkonomi",
  "Almen erhvervsøkonomi, HA" = "Erhvervsøkonomi",
  "Almen Erhvervsøkonomi" = "Erhvervsøkonomi",
  "Socialrådgiveruddannelsen" = "Socialrådgiver",
  "Arkitektuddannelsen" = "Arkitekt",
  "Sygeplejerske - Odense" = "Sygeplejerske",
  "Sygeplejerske . Svendborg" = "Sygeplejerske",
  "Pædagog, Viborg" = "Pædagog",
  "Pædagog, Slagelse" = "Pædagog",
  "Pædagog, Thisted" = "Pædagog",
  "Pædagog, Århus" = "Pædagog",
  "Pædagog, Strandvejen" = "Pædagog",
  "Pædagog, Holbæk" = "Pædagog",
  "Pædagog, Randers" = "Pædagog",
  "Pædagog, Odsherred" = "Pædagog",
  "Pædagog, Hartmansvej" = "Pædagog",
  "Folkeskolelærer, Bornholm" = "Folkeskolelærer",
  "Tandlæge" = "Odontologi (tandlæge)",
  "Tandlægeuddannelsen" = "Odontologi (tandlæge)",
  "Veterinær" = "Veterinærmedicin",
  "Jordemoderuddannelsen" = "Jordemoder",
  "Socialrådgiver, Odense" = "Socialrådgiver",
  "Socialrådgiver, International" = "Socialrådgiver",
  "Socialrådgiver, Holstebro" = "Socialrådgiver",
  "Socialrådgiver, Nyk. F" = "Socialrådgiver",
  "Socialrådgiver, Århus" = "Socialrådgiver",
  "Socialrådg.udd. internationale" = "Socialrådgiver",
  "Socialrådgiver, Esbjerg" = "Socialrådgiver",
  "Socialrådgiver, Bornholm" = "Socialrådgiver",
  "Socialrådgiver, Inter. Århus" = "Socialrådgiver",
  "Socialrådgiver, Aabenraa" = "Socialrådgiver",
  "Socialrådg.udd., International" = "Socialrådgiver",
  "Socialrådg.udd., Bornholm" = "Socialrådgiver",
  "Serviceøkonom" = "Service- og oplevelesøkonom",
  "Dansk" = "Dansk/nordisk",
  "Farmaceut" = "Farmaci",
  "Farmaceutuddannelsen" = "Farmaci",
  "Farmakonomelev" = "Farmakonom",
  "Hospitalslaborant" = "Laborant",
  "Laborant, Sommer" = "Laborant",
  "Laborant, sommer" = "Laborant",
  "Laborant sommer" = "Laborant",
  "Laborant, Vinter" = "Laborant",
  "Laborant, vinter" = "Laborant",
  " Laborant" = "Laborant",
  "Tekstile fag og formidling" = "Tekstildesign",
  "Tekstile fag og formidl." = "Tekstildesign",
  "Administration/samfundsfag" = "Offentlig administration",
  "Ergoterapeut Esbjerg" = "Ergoterapeut",
  "Ergoterapeut Holstebro" = "Ergoterapeut",
  "Ergoterapeutskole" = "Ergoterapeut",
  " Ernær. og sund." = "Ernæring og sundhed",
  "Afspændingspædagog" = "Afspændingspædagogik og psykomotorik",
  "Radiograf Ringk/Vib/Årh/N.jyll" = "Radiograf",
  "Radiograf, Fyns Amt" = "Radiograf",
  "Radiograf, Ribe Amt" = "Radiograf",
  "Radiograf, Sønderjyllands Amt" = "Radiograf",
  "Radiograf, Vejle Amt" = "Radiograf",
  "Civil/diploming. eksporttekn." = "Diplomingeniør",
  "Civil/diploming. kemi" = "Diplomingeniør",
  "Civil/diploming.kemi - Esbjerg" = "Diplomingeniør",
  "Maskinmester m værkstedskursus" = "Maskinmester",
  "Skibsofficer/juniorofficer" = "Skibsfører",
  "Skibs-/juniorofficer Fanø Nav." = "Skibsfører",
  "Bygningkonstruktør" = "Bygningskonstruktør",
  "Konstruktør" = "Bygningskonstruktør",
  "Skov- og lands.ing.+adg.kursus" = "Skov- og landskabsingeniør",
  "Skov- og lands.ing.-adg.kursus" = "Skov- og landskabsingeniør",
  "Skov-og landskabsingeniør" = "Skov- og landskabsingeniør",
  "Eksport" = "Eksport og teknologi",
  "Eksporttekniker" = "Eksport og teknologi",
  "Eksportekniker" = "Eksport og teknologi",
  "Prof.bach. i procesøkonomi" = "Procesøkonomi og værdikædeledelse",
  "Value Chain Management" = "Procesøkonomi og værdikædeledelse",
  "Prof.bachelor i økonomi og IT" = "Økonomi og informationsteknologi",
  " Prof.bachelor i økonomi og IT" = "Økonomi og informationsteknologi",
  "Pof.bachelor i økonomi og IT" = "Økonomi og informationsteknologi",
  "Grafisk kommunikation" = "Grafisk Kommunikation"
)


## Mapping names ----------------------------------------------------------

# I use the created map to map new consistent names to the inconsistencies

df_pre2009 <- df_pre2009 |> 
  mutate(uddannelse_kort = recode(uddannelse_kort, !!!names_map))

# A lot of diplomingeniør educations are inconsistently named. But they are just
# one category, so I just replace them altogether as this is what is done in the
# newer data. Do the same for a number of other educations

df_pre2009 <- df_pre2009 |> 
  mutate(
    uddannelse_kort = if_else(
      str_starts(uddannelse_kort, "Diploming."),
      "Diplomingeniør",
      uddannelse_kort),
    uddannelse_kort = if_else(
      str_starts(uddannelse_kort, "Bygningskonst"),
      "Bygningskonstruktør",
      uddannelse_kort)
    )

# And again checking which are still not matched

# Unmatched educations in ufm_df - 153 rows down from 163
# Do note that a lot of these are new educations, so it's not a big problem
unmatched_post <- ufm_df_clean |> 
  group_by(uddannelse_kort) |> 
  summarise(total = sum(ansøgninger_prio1)) |> 
  anti_join(
    df_pre2009 |> 
      group_by(uddannelse_kort) |> 
      summarise(count2 = n())
  )

# Unmatched educations in pre2009 - 776 rows down from 920. Also note a lot of these
# are educations that no longer exists
unmatched_pre <- df_pre2009 |> 
  group_by(uddannelse_kort) |> 
  summarise(total = sum(ansøgninger_prio1)) |> 
  anti_join(
    ufm_df_clean |> 
      group_by(uddannelse_kort) |> 
      summarise(count2 = n())
  )

# Conclusion: There is some data loss due to different naming. But I have matched
# all possible professionsbachelor-educations, which are the most important 
# since I only use these for constructing the synthetic control.


## Re-aggregating ---------------------------------------------------------

# Because some naming was initially wrong I need to aggregate again, so each
# education is only observed once per year

df_pre2009 <- df_pre2009 |> 
  group_by(uddannelse_kort, år) |> 
  summarise(
    andel_kvinder_prio1 = sum(andel_kvinder_prio1 *
                                (ansøgninger_prio1 / sum(ansøgninger_prio1))),
    andel_mænd_prio1 = sum(andel_mænd_prio1 *
                                (ansøgninger_prio1 / sum(ansøgninger_prio1))),
    ansøgninger_prio1 = sum(ansøgninger_prio1))



# Cleaning pre-2009 data --------------------------------------------------

# With the pre-2009 data fixed, I can now do some final cleaning before merging.
# First, I filter the pre-2009 data to only include educations that are also in
# the UFM data

df_pre2009 <- df_pre2009 |> 
  filter(uddannelse_kort %in% ufm_df_clean$uddannelse_kort)

# Next I add the full education name to the data
df_pre2009 <- df_pre2009 |> 
  left_join(
    ufm_df_clean |> 
      group_by(uddannelse, uddannelse_kort) |> 
      summarise(n = n()) |> 
      select(uddannelse, uddannelse_kort),
    by = "uddannelse_kort")

# And I save the final pre-2009 data
saveRDS(df_pre2009, "data/processed/individual_datasets/1997-2008.RDS")



# Merging and finalizing data ---------------------------------------------

# I merge the two datasets to create the final dataset
df_final <- ufm_df_clean |> 
  bind_rows(df_pre2009) |> 
  arrange(uddannelse, år)

# Checking how many educations I have a complete time series for
# = only 53 educations are observed the complete 28 years.
complete_ts_count <- df_final |> 
  group_by(uddannelse) |> 
  summarise(n = n()) |> 
  filter(n == 28)

# Adding an indicator for the type of education
df_final <- df_final |> 
  mutate(uddannelsesniveau = case_when(
    str_detect(uddannelse, "prof\\. bach") ~ "Prof. bachelor",
    str_detect(uddannelse, ", bach") ~ "Bachelor",
    str_detect(uddannelse, ", EA") ~ "Erhvervsakademi"
  ))

# Adding an indicator for whether an education is in a treatment group and which treatment group
df_final <- df_final |> 
  mutate(treatment = case_when(
    uddannelse_kort == "Folkeskolelærer" ~ "Lærerlockout",
    uddannelse_kort == "Sygeplejerske" ~ "Sygeplejerskestrejke",
    .default = "Kontrol"))

# Adding post-indicator
df_final <- df_final |> 
  mutate(
    post_lockout = if_else(
      år >= 2013,
      1,
      0),
    post_strejke = if_else(
      år >= 2021,
      1,
      0))

# Adding binary treatment-indicator
df_final <- df_final |> 
  mutate(
    d = case_when(
      treatment == "Sygeplejerskestrejke" & post_strejke == 1 ~ 1,
      treatment == "Lærerlockout" & post_lockout == 1 ~ 1,
      .default = 0))

# And adding a numeric education ID
df_final <- df_final |> 
  mutate(id = as.numeric(factor(uddannelse)))

# Calculating share of applications that are kvote 2
df_final <- df_final |> 
  mutate(
    andel_kvote2_total = ansøgninger_total_kvote2 / ansøgninger_total,
    andel_kvote2_prio1 = ansøgninger_prio1_kvote2 / ansøgninger_prio1
  )

# Saving the cleaned data
saveRDS(df_final, "data/processed/ansøgninger_1997-2024.RDS")

