# =============================================================================
# Script Name: 03g_MERGE_SCORECARD_DIMS.R
# Purpose: Merging the social protection indicators with grid
# Input Dataset: grid_10km.shp, HH_surveys_all_dim_ADM2.Rds
# Output Dataset: grid_social_dim2.Rds
# Author: Chitra Balasubramanian
# Last Updated: 2024-08-05
# =============================================================================


# Load Data ---------------------------------------------------------------

social_dims <- readRDS(file.path(data_hh_survey,"HH_surveys_all_dim_ADM2.Rds"))
codebook_survey_grid <- read_excel(file.path(data_hh_survey,"codebook_survey_grid.xlsx"))#codebook
grid_sf <- st_read(file.path(final_replication, "grid_10km.shp"))



# Merge codebook ----------------------------------------------------------
# Merge everything except Jordan
social_dims_sub <- social_dims %>% dplyr::filter(countrycode!= "JOR")
codebook_survey_grid_sub <- codebook_survey_grid %>% filter(countrycode!= "JOR")
hh_surveys <- social_dims_sub %>% left_join(codebook_survey_grid_sub, by = c("countrycode","subnatid"))


#Merge Jordan
social_dims_jor <- social_dims %>% filter(countrycode == "JOR")
codebook_jor <- codebook_survey_grid %>% filter(countrycode == "JOR")
hh_survey_jor <- social_dims_jor %>% left_join(codebook_jor, by = c("countrycode","subnatid", "subnatid2"))


#Append to hh survey grid
hh_surveys <- hh_surveys %>% dplyr::select(-subnatid2.y) %>% rename(subnatid2 = subnatid2.x)
hh_surveys <- rbind(hh_surveys,hh_survey_jor)



# Match names to grid names -----------------------------------------------
#Djibouti
hh_surveys_dji <- hh_surveys %>% filter(countrycode == "DJI")
dji <- grid_sf %>% filter(ISO_A2 == "DJ")
unique(hh_surveys_dji$id_adm)
unique(dji$WB_ADM1_NA)

hh_surveys_dji <- hh_surveys_dji %>%
  dplyr::mutate(WB_ADM1_NA = case_when(
    id_adm == "10 - Djibouti" ~ "Djibouti",
    id_adm == "60 - Arta" ~ "Djibouti",
    id_adm == "20 - Ali Sabieh" ~ "Ali Sabieh",
    id_adm == "30 - Dikhil" ~ "Dikhil",
    id_adm == "40 - Tadjourah" ~ "Tadjourah",
    id_adm == "50 - Obock" ~ "Obock"
  ))


# EGY (Cannot Merge Egypt because the mapping of WB_ADM1_NA does not match hh survey)
hh_surveys_egy <- hh_surveys %>% filter(countrycode == "EGY")
egy <- grid_sf %>% filter(ISO_A2 == "EG")
unique(hh_surveys_egy$id_adm)
unique(egy$WB_ADM1_NA)



#IRN
hh_surveys_irn <- hh_surveys %>% filter(countrycode == "IRN")
irn <- grid_sf %>% filter(ISO_A2 == "IR")
unique(hh_surveys_irn$id_adm)
unique(irn$WB_ADM1_NA)
hh_surveys_irn <- hh_surveys_irn %>%
  dplyr::mutate(WB_ADM1_NA = case_when(
    id_adm == "11-Sistan" ~ "Sistan-o baluchestan",
    id_adm == "7-Fars" ~ "Fars",
    id_adm == "0-Markazi" ~ "Markazi",
    id_adm == "9-Khorasan Razavi" ~ "Khorasan",
    id_adm == "10-Isfahan" ~ "Esfahan" ,
    id_adm == "12-Kurdestan" ~ "Kordestan",
    id_adm == "13-Hamadan" ~ "Hamedan" ,
    id_adm == "14-Bakhtiari"  ~ "Chaharmahal-o bakhtiyar",
    id_adm == "15-Lorestan" ~ "Lorestan",
    id_adm == "16-Ilam" ~ "Ilam",
    id_adm == "17-Kohkiloyeh" ~ "Kohgiluyeh va boyerahma",
    id_adm == "18-Bushehr" ~ "Bushehr",
    id_adm == "1-Gilan"  ~ "Gilan",
    id_adm == "19-Zanjan" ~ "Zanjan" ,
    id_adm == "20-Semnan" ~ "Semnan",
    id_adm ==  "21-Yazd"  ~ "Yazd",
    id_adm == "22-Hormozgan" ~ "Hormozgan"  ,
    id_adm == "23-Tehran" ~ "Tehran",
    id_adm == "24-Ardebil" ~ "Ardebil" ,
    id_adm == "25-Qom" ~ "Ghom",
    id_adm ==  "26-Qazvin" ~ "Ghazvin" ,
    id_adm == "27-Golestan" ~ "Golestan",
    id_adm == "28-N. Khorasan" ~ "Khorasan",
    id_adm == "2-Mazandaran" ~ "Mazandaran",
    id_adm == "29-S. Khorasan" ~ "Khorasan",
    id_adm == "30-Alborz" ~ "Tehran",
    id_adm == "3-E.Azarbaijan" ~ "East Azarbayejan" ,
    id_adm == "4-W.Azarbaijan" ~ "West Azarbayejan",
    id_adm == "5-Kermanshah" ~  "Kermanshah" ,
    id_adm == "6-Khuzestan" ~ "Khuzestan" ,
    id_adm == "7-Fars" ~ "Fars",
    id_adm == "8-Kerman" ~ "Kerman"
  ))

#IRQ
hh_surveys_irq <- hh_surveys %>% filter(countrycode == "IRQ")
irq <- grid_sf %>% filter(ISO_A2 == "IQ")
unique(hh_surveys_irq$id_adm)
unique(irq$WB_ADM1_NA)
hh_surveys_irq <- hh_surveys_irq %>%
  dplyr::mutate(WB_ADM1_NA = case_when(
    id_adm == "11 - Duhouk" ~  "Dahuk",
    id_adm == "12 - Nineveh" ~  "Ninevah" ,
    id_adm == "13 - Suleimaniya" ~ "As Sulaymaniyah",
    id_adm == "14 - Karkouk" ~ "Ta'meem",
    id_adm == "15 - Erbil" ~ "Arbil" ,
    id_adm == "21 - Diala" ~ "Diala",
    id_adm == "22 - Al-Anbar" ~ "Anbar",
    id_adm == "23 - Baghdad" ~ "Baghdad" ,
    id_adm == "24 - Babil" ~  "Babil" ,
    id_adm == "25 - Kerbala" ~ "Kerbala",
    id_adm == "26 - Wasit" ~ "Wasit",
    id_adm == "27 - Salahuddin"~ "Salaheldin",
    id_adm == "28 - Al-Najaf" ~ "Najaf" ,
    id_adm == "31 - Al-Qadisiya" ~ "Qadisiyah",
    id_adm == "32 - Al-Muthanna" ~ "Muthanna",
    id_adm == "33 - Thi-Qar" ~ "Thiqar",
    id_adm == "34 - Missan" ~ "Missan",
    id_adm == "35 - Basrah" ~ "Basrah"
  )
  )


#LBN
hh_surveys_lbn <- hh_surveys %>% filter(countrycode == "LBN")
lbn <- grid_sf %>% filter(ISO_A2 == "LB")
unique(hh_surveys_lbn$id_adm)
unique(lbn$WB_ADM1_NA)
hh_surveys_lbn <- hh_surveys_lbn %>%
  dplyr::mutate(WB_ADM1_NA = case_when(
    id_adm == "1 - Beirut" ~ "Beyrouth",
    id_adm == "2 - Mount Lebanon" ~ "Mont Liban",
    id_adm == "3 - North" ~ "Liban Nord",
    id_adm == "5 - Bekaa" ~ "Beqaa" ,
    id_adm == "6 - South" ~ "Liban Sud",
    id_adm == "7 - Nabatieh" ~ "Nabatieh"
  )
  )


#MAR Cannot match because mapping is old only 7 regions versus the new administrative boundaries post 1997
hh_surveys_mar <- hh_surveys %>% filter(countrycode == "MAR")

#MLT # Only 1 division available that cannot be matched
hh_surveys_mlt <- hh_surveys %>% filter(countrycode == "MLT")

#PSE # The mapping does not match, Only Gaza and West Bank in the survey
hh_surveys_pse <- hh_surveys %>% filter(countrycode == "PSE")


#TUN The mapping of the ADMS is not the same
hh_surveys_tun <- hh_surveys %>% filter(countrycode == "TUN")
tun <- grid_sf %>% filter(ISO_A2 == "TN")
unique(hh_surveys_tun$id_adm)
unique(tun$WB_ADM1_NA)


#YEM
hh_surveys_yem <- hh_surveys %>% filter(countrycode == "YEM")
yem <- grid_sf %>% filter(ISO_A2 == "YE")
unique(hh_surveys_yem$id_adm)
unique(yem$WB_ADM1_NA)
hh_surveys_yem <- hh_surveys_yem %>%
  dplyr::mutate(WB_ADM1_NA = case_when(
    id_adm == "11 - Ibb" ~ "Ibb" ,
    id_adm == "12 - Abyan" ~ "Abyan",
    # Not making a distinction between the city and the region
    id_adm == "13 - Sanaa City" ~ "Sana,'a" ,
    id_adm == "14 - Al-Baida" ~ "Al Bayda",
    id_adm == "15 - Taiz" ~ "Taizz" ,
    id_adm == "16 - Al-Jawf" ~ "Al Jawf",
    id_adm == "17 - Hajja" ~ "Hajjah" ,
    id_adm == "18 - Al-Hodeida" ~ "Al Hudaydah",
    id_adm == "19 - Hadramout" ~ "Hadramaut",
    id_adm == "20 - Dhamar" ~ "Dhamar",
    id_adm == "21 - Shabwah" ~ "Shabwah",
    id_adm == "22 - Saadah" ~ "S,adah",
    # Not making a distinction between the city and the region
    id_adm == "23 - Sanaa Region" ~ "Sana'a" , 
    id_adm == "24 - Aden" ~ "Aden",
    id_adm == "25 - Laheg" ~ "Lahj" ,
    id_adm == "26 - Mareb" ~  "Marib",
    id_adm == "27 - Al-Mahweet" ~ "Al Mahwit" ,
    id_adm == "28 - Al-Maharh" ~ "Al Maharah",
    id_adm == "29 - Amran" ~ "Amran",
    id_adm == "30 - Al-Dhale"  ~  "Ad Dali",
    id_adm == "31 - Remah" ~ "Sana'a" , # Eyeballed this on google
    id_adm ==  "31 - Socatra" ~ "19 - Hadramout",
  )
  )



#JOR
hh_surveys_jor <- hh_surveys %>% filter(countrycode == "JOR")
jor <- grid_sf %>% filter(ISO_A2 == "JO")
unique(hh_surveys_jor$subnatid2)
unique(jor$WB_ADM1_NA)
hh_surveys_jor <- hh_surveys_jor %>%
  dplyr::mutate(WB_ADM1_NA = case_when(
    subnatid2 == "11 - Amman" ~ "Amman"  ,
    subnatid2 == "12 - Balqa" ~ "Al Balqa",
    subnatid2 == "13 - Zarqa" ~ "Az Zarqa" ,
    subnatid2 == "14 - Madaba" ~ "Madaba",
    subnatid2 == "21 - Irbid" ~ "Irbid" ,
    subnatid2 == "22 - Mafraq" ~ "Al Mafraq"  ,
    subnatid2 == "23 - Jarash" ~ "Jarash",
    subnatid2 == "24 - Ajlun" ~ "Ajloun",
    subnatid2 == "31 - Karak" ~ "Al Karak",
    subnatid2 == "32 - Tafiela" ~ "At Tafilah" ,
    subnatid2 == "33 - Ma'an" ~ "Ma'an" ,
    subnatid2 == "34 - Aqaba" ~ "Al Aqabah"
  )
  )



# Merge back the grids ----------------------------------------------------
unique(hh_surveys$countrycode)
merged_grid <- bind_rows(hh_surveys_dji,
                         hh_surveys_egy,
                         hh_surveys_irn,
                         hh_surveys_irq,
                         hh_surveys_jor,
                         hh_surveys_lbn,
                         hh_surveys_tun,
                         hh_surveys_yem,
                         hh_surveys_mlt,
                         hh_surveys_pse,
                         hh_surveys_mar)

##  select only the mapped vars WB_ADM1_NA, id_adm,subnatid
names(merged_grid)
merged_grid_summ <- merged_grid %>%
  group_by(ISO_A2, WB_ADM1_NA) %>%
  summarise(
    countrycode = first(countrycode),
    subnatid = first(subnatid),
    subnatid2 = first(subnatid2),
    subnatid3 = first(subnatid3),
    id_adm = first(id_adm),
    across(na_share_water:exp_dim_5, \(x) mean(x, na.rm = TRUE))
  )



#add back to original grid
grid_sf <- grid_sf %>%
  left_join(merged_grid_summ, by = c("ISO_A2", "WB_ADM1_NA"))



# Export ------------------------------------------------------------------
saveRDS(grid_sf, file.path(final_replication, "grid_social_dim2.Rds"))


