#依照TaiCOL進行TaxaTree的IUCN,國內紅皮書,保育類等更新
#setting path ====
setwd("F:/iucn")
getwd()
rm(list=ls())
##library prep ====
library(readxl) # import xlsx
library(magrittr) # %>%
library(dplyr)
library(stringr)
library(tidyr)
library(purrr)

#import data ====
TC_taxon <- read.csv("TaiCOL_taxon_20250429.csv")
TT_name <- read.csv("taxatree_20250429.csv")

TC_version <- "20250429"
TT_version <- "20250429"

#篩選重要欄位 ====
TT_selected <- TT_name %>%
  select(taxonUUID,taxonRank,taiCOLNameCode,simplifiedScientificName,kingdom,class,
         nativeness,endemism,protectedStatusTW,categoryRedlistTW,categoryIUCN,sensitiveCategory,categoryRedlistVersionTW) %>%
  rename(taxon_id = taiCOLNameCode)

TT_selected  <- TT_selected  %>%
  mutate(across(everything(), ~ as.character(.)))

#colnames(TT_name)
#unique(TT_selected$nativeness)
#unique(TT_selected$protectedStatusTW)
#unique(TT_selected$categoryRedlistTW)
#unique(TT_selected$categoryIUCN)
#unique(TT_selected$nativeness)
#unique(TT_selected$taxonRank)
#unique(TT_selected$endemism)

TC_selected <- TC_taxon %>%
  select(taxon_id,rank,simple_name,is_endemic,alien_type,iucn,redlist,protected) %>%
  filter(rank %in% c("Species", "Subspecies", "Variety", "Form", "Special Form", "Hybrid Formula")) %>%
  mutate(
    name_parts = str_split(simple_name, " "),
    name_parts_clean = map(name_parts, ~ .x[!tolower(.x) %in% c("var.", "subsp.", "fo.", "forma", "ssp.")]),
    new_simple_name = map_chr(name_parts_clean, ~ str_trim(str_c(.x, collapse = " "))),
    genus = map_chr(name_parts_clean, ~ .x[1]),
    specificEpithet = map_chr(name_parts_clean, ~ .x[2] %||% NA_character_),
    infraspecies = map_chr(name_parts_clean, ~ .x[3] %||% NA_character_)) %>%
  select(taxon_id,rank,new_simple_name,is_endemic,alien_type,iucn,redlist,protected,genus,specificEpithet,infraspecies)%>%
    rename(new_iucn = iucn,
           new_protected = protected,
           new_redlistTW = redlist) 

TC_selected  <- TC_selected  %>%
  mutate(across(everything(), ~ as.character(.)))

#unique(TC_selected$new_simple_name)
#colnames(TC_taxon)
#unique(TC_taxon$rank)
#unique(TC_selected$new_iucn)
#unique(TC_selected$new_protected)
#unique(TC_selected$new_redlistTW)
#unique(TC_selected$rank)
#unique(TC_selected$alien_type)

combine.table <- left_join(TT_selected, TC_selected, by = "taxon_id") %>%
  filter(taxonRank %in% c("species","infraspecies")) %>%
  filter(taxon_id != "") %>%
  mutate(nativeness = recode(nativeness,
                             "原生 Native" = "native",
                             "外來栽培 Cultivated (non-native)" = "non-native",
                             "外來歸化 Naturalized (non-native)" = "non-native",
                             "外來 Non-native"  = "non-native",
                             "境外 Overseas"  = "overseas"),
         endemism = recode(endemism,
                           "台灣特有" = "true"),
         protectedStatusTW = recode(protectedStatusTW,
                                    "瀕臨絕種保育類野生動物" = "I",
                                    "珍貴稀有保育類野生動物" = "II",
                                    "其他應予保育之野生動物" = "III",
                                    "文化資產保存法：珍貴稀有植物" = "1"),
         categoryRedlistTW = recode(categoryRedlistTW,
                                    "易危（VU, Vulnerable）" = "NVU",
                                    "暫無危機（LC, Least Concern）"  = "NLC",
                                    "接近受脅（NT, Near Threatened）" = "NNT", 
                                    "區域滅絕（RE, Regionally Extinct）" = "RE", 
                                    "瀕危（EN, Endangered）" = "NEN",
                                    "極危（CR, Critically Endangered）"= "NCR",
                                    "資料缺乏（DD, Data Deficient）"   = "DD" ,
                                    "滅絕（EX, Extinct）" = "EX",
                                    "不適用"   = "NA",
                                    "未評估（NE, Not Evaluated）"  = "NE",
                                    "野外滅絕（EW, Extinct in the Wild）" = "EW"),
         categoryIUCN = recode(categoryIUCN,
                               "未評估（NE, Not Evaluated）" = "NE",
                               "接近受脅（NT, Near Threatened）" = "NT" ,
                               "易危（VU, Vulnerable）" =  "VU",
                               "極危（CR, Critically Endangered）"= "CR",
                               "瀕危（EN, Endangered）" = "EN",
                               "暫無危機（LC, Least Concern）" =  "LC" ,
                               "資料缺乏（DD, Data Deficient）"= "DD",
                               "野外滅絕（EW, Extinct in the Wild）"="EW"),
         new_iucn = recode(new_iucn,
                           "LR/nt"= "NT",
                           "LR/cd" = "LR",
                           "LR/lc" = "LC"),
         rank = recode(rank,
                       "Phylum" = "phylum",       
                       "Class"  = "class",
                       "Order"  = "order",
                       "Family" = "family",
                       "Genus"  = "genus",      
                       "Species" = "species",
                       "Subspecies" = "infraspecies",
                       "Variety"    = "infraspecies",
                       "Form"       = "infraspecies",
                       "Superfamily" = "superfamily",
                       "Hybrid Formula" = "species",
                       "Special Form" = "infraspecies"),
         alien_type = recode(alien_type,
                             "cultured" = "non-native",
                             "naturalized" = "non-native",
                             "invasive" = "non-native"),
         is_endemic = recode(is_endemic,
                             "false" = ""))

combine.table <- combine.table %>%
  mutate(across(everything(), ~ as.character(.)))

#taxon_id被刪除
if (!dir.exists("output_namecheck")) {
  dir.create("output_namecheck", recursive = TRUE)
}

combine.table_nomatch <- combine.table %>%
  filter(is.na(new_simple_name) | new_simple_name == "")

write.csv(combine.table_nomatch , "output_namecheck/combine.table_nomatch.csv",row.names = F)

TT_selected_noID <- TT_selected %>%
  filter(taxonRank %in% c("species","infraspecies"),
         taxon_id == "",
         kingdom != "Plantae")

write.csv(TT_selected_noID,"output_namecheck/TT_selected_noID.csv",row.names = F)

#新增比對欄位
compare.table <- combine.table  %>%
  filter(!is.na(new_simple_name)) %>%
  mutate(comparison_name = simplifiedScientificName == new_simple_name,
         comparison_IUCN = if_else(is.na(categoryIUCN) & is.na(new_iucn), TRUE, categoryIUCN == new_iucn),
         comparison_redlistTW = if_else(is.na(categoryRedlistTW) & is.na(new_redlistTW), TRUE, categoryRedlistTW == new_redlistTW),
         comparison_protected = if_else(is.na(protectedStatusTW) & is.na(new_protected), TRUE, protectedStatusTW == new_protected))

##name不一致 => 須更動名字
combine.table_namecheck <- compare.table  %>%
  filter(comparison_name == FALSE) 

write.csv(combine.table_namecheck ,file="output_namecheck/combine.table_namecheck .csv",row.names = F)

#IUCN比對 part I: 比對欄位====
if (!dir.exists("output_IUCN change ")) {
  dir.create("output_IUCN change", recursive = TRUE)
}

#IUCN一致、name一致 => 不須更動
IUCN_TRUE <- compare.table %>%
  filter(comparison_IUCN == TRUE) %>%
  filter(comparison_name == TRUE) 

#IUCN不一致、name一致 => 須更動IUCN
IUCN_change <- compare.table %>%
  filter(comparison_IUCN == FALSE) %>%
  filter(comparison_name == TRUE) 

#IUCN不一致、name一致 => 須更動IUCN
IUCN_NA <- compare.table  %>%
  filter(is.na(comparison_IUCN)) %>%
  filter(comparison_name == TRUE) 

write.csv(IUCN_TRUE,file="output_IUCN change/IUCN_TRUE.csv",row.names = F)

#IUCN比對 part II: output TT需修改的分類群====
IUCN_change <- IUCN_change %>%
  rbind(IUCN_NA) %>%
  select(taxonUUID,taxonRank,kingdom,simplifiedScientificName,categoryIUCN,new_iucn)

#種改為無
IUCN_change_sp <- IUCN_change %>%
  filter(taxonRank == "species" & new_iucn == "") %>%
  left_join(TT_name[c(1,4,58)],by=c("taxonUUID"="parentUUID")) %>%
  rename(infrasp_name = simplifiedScientificName.y,
         infrasp_taxonUUID = taxonUUID.y ) %>%
  left_join(compare.table[c(1,18)],by=c("infrasp_taxonUUID"="taxonUUID")) %>%
  rename(new_iucn = new_iucn.y) %>%
  mutate(comparison_iucn = if_else(is.na(categoryIUCN) & is.na(new_iucn), TRUE, categoryIUCN == new_iucn))

IUCN_change_sp_output <- IUCN_change_sp %>%
  filter(is.na(infrasp_taxonUUID) | comparison_iucn == FALSE ) %>% #無種下，且iucn需改為無
  mutate(source_process = "IUCN_change_sp_output") %>%
  mutate(categoryIUCN = recode(categoryIUCN,
                               "NE" = "未評估（NE, Not Evaluated）",
                               "NT" = "接近受脅（NT, Near Threatened）",
                               "VU" = "易危（VU, Vulnerable）",
                               "CR" = "極危（CR, Critically Endangered）",
                               "EN" = "瀕危（EN, Endangered）",
                               "LC" = "暫無危機（LC, Least Concern）",
                               "DD" = "資料缺乏（DD, Data Deficient）",
                               "EW" = "野外滅絕（EW, Extinct in the Wild）"),
         new_iucn = recode(new_iucn,
                           "NE" = "未評估（NE, Not Evaluated）",
                           "NT" = "接近受脅（NT, Near Threatened）",
                           "VU" = "易危（VU, Vulnerable）",
                           "CR" = "極危（CR, Critically Endangered）",
                           "EN" = "瀕危（EN, Endangered）",
                           "LC" = "暫無危機（LC, Least Concern）",
                           "DD" = "資料缺乏（DD, Data Deficient）",
                           "EW" = "野外滅絕（EW, Extinct in the Wild）"))%>%
  mutate(source_process = "IUCN_change_sp_output")   %>%
  select(taxonUUID,taxonRank,kingdom,simplifiedScientificName.x,categoryIUCN,new_iucn.x,source_process) %>%
  rename(simplifiedScientificName = simplifiedScientificName.x,
         new_iucn = new_iucn.x)

#write.csv(IUCN_change_sp_output ,"output_IUCN change/IUCN_change_sp_output .csv",row.names = F)

#種下改為無，獨立確認 
IUCN_change_infrasp <- IUCN_change %>%
  filter(taxonRank == "infraspecies" & new_iucn == "") %>%
  select(taxonUUID,kingdom,taxonRank,simplifiedScientificName,categoryIUCN) %>%
  left_join(TT_name[c(1,4,5)],by="taxonUUID") %>%
  left_join(compare.table[c(1,18)],by= c("parentUUID"="taxonUUID"))  %>%
  mutate(comparison_iucn = if_else(is.na(categoryIUCN) & is.na(new_iucn), TRUE, categoryIUCN == new_iucn))

IUCN_change_infrasp_output <- IUCN_change_infrasp %>%
  filter(is.na(comparison_iucn) | comparison_iucn == FALSE ) %>% #紅皮書類別需依照種階層修改
  mutate(categoryIUCN = recode(categoryIUCN,
                               "NE" = "未評估（NE, Not Evaluated）",
                               "NT" = "接近受脅（NT, Near Threatened）",
                               "VU" = "易危（VU, Vulnerable）",
                               "CR" = "極危（CR, Critically Endangered）",
                               "EN" = "瀕危（EN, Endangered）",
                               "LC" = "暫無危機（LC, Least Concern）",
                               "DD" = "資料缺乏（DD, Data Deficient）",
                               "EW" = "野外滅絕（EW, Extinct in the Wild）"),
         new_iucn = recode(new_iucn,
                           "NE" = "未評估（NE, Not Evaluated）",
                           "NT" = "接近受脅（NT, Near Threatened）",
                           "VU" = "易危（VU, Vulnerable）",
                           "CR" = "極危（CR, Critically Endangered）",
                           "EN" = "瀕危（EN, Endangered）",
                           "LC" = "暫無危機（LC, Least Concern）",
                           "DD" = "資料缺乏（DD, Data Deficient）",
                           "EW" = "野外滅絕（EW, Extinct in the Wild）")) %>%
  mutate(source_process = "IUCN_change_infrasp_output") %>%
  select(taxonUUID,taxonRank,kingdom,simplifiedScientificName,categoryIUCN,new_iucn,source_process)

#write.csv(IUCN_change_infrasp_output,"output_IUCN change/IUCN_change_infrasp_output.csv",row.names = F)

#IUCN需修改
IUCN_change_output <- IUCN_change %>%
  anti_join(IUCN_change_infrasp, by = "taxonUUID")%>%
  anti_join(IUCN_change_sp, by = "taxonUUID") %>%
  mutate(categoryIUCN = recode(categoryIUCN,
                               "NE" = "未評估（NE, Not Evaluated）",
                               "NT" = "接近受脅（NT, Near Threatened）",
                               "VU" = "易危（VU, Vulnerable）",
                               "CR" = "極危（CR, Critically Endangered）",
                               "EN" = "瀕危（EN, Endangered）",
                               "LC" = "暫無危機（LC, Least Concern）",
                               "DD" = "資料缺乏（DD, Data Deficient）",
                               "EW" = "野外滅絕（EW, Extinct in the Wild）"),
         new_iucn = recode(new_iucn,
                           "NE" = "未評估（NE, Not Evaluated）",
                           "NT" = "接近受脅（NT, Near Threatened）",
                           "VU" = "易危（VU, Vulnerable）",
                           "CR" = "極危（CR, Critically Endangered）",
                           "EN" = "瀕危（EN, Endangered）",
                           "LC" = "暫無危機（LC, Least Concern）",
                           "DD" = "資料缺乏（DD, Data Deficient）",
                           "EW" = "野外滅絕（EW, Extinct in the Wild）")) %>%
  mutate(source_process = "IUCN_change_output") %>%
  select(taxonUUID,taxonRank,kingdom,simplifiedScientificName,categoryIUCN,new_iucn,source_process)

#write.csv(IUCN_change_output,"output_IUCN change/IUCN_change_output.csv",row.names = F)

IUCN_change_final_output <- bind_rows(IUCN_change_sp_output, IUCN_change_infrasp_output, IUCN_change_output)

write.csv(IUCN_change_final_output,"output_IUCN change/IUCN_change_final_output.csv",row.names = F)

#IUCN比對 part III: bug遺珠 ====
TC_taxon_iucn_true <- TC_selected %>%
  filter(new_iucn != "")

extra_taxon_id <- setdiff(TC_taxon_iucn_true$taxon_id, combine.table$taxon_id)

extra_rows <- TC_taxon_iucn_true %>%
  filter(taxon_id %in% extra_taxon_id)

extra_rows <- left_join(extra_rows, TC_taxon[,c("taxon_id","kingdom","class")], by = "taxon_id")
  
extra_taxon_iucn_Animalia <- extra_rows %>%
  select(new_simple_name,taxon_id,kingdom,class,new_iucn) %>%
  filter(kingdom == "Animalia")

extra_taxon_iucn_Plantae <- extra_rows %>%
  select(new_simple_name,taxon_id,kingdom,class,new_iucn) %>%
  filter(kingdom == "Plantae")

write.csv(extra_taxon_iucn_Animalia, "output_IUCN change/extra_taxon_iucn_Animalia.csv",row.names = F)
write.csv(extra_taxon_iucn_Plantae, "output_IUCN change/extra_taxon_iucn_Plantae.csv",row.names = F)

#國內紅皮書比對 part I: 比對欄位====
if (!dir.exists("output_redlistTW change ")) {
  dir.create("output_redlistTW change", recursive = TRUE)
}

#redlistTW一致、name一致 => 不須更動
redlistTW_TRUE <- compare.table %>%
  filter(comparison_redlistTW == TRUE) %>%
  filter(comparison_name == TRUE) 

#redlistTW不一致、name一致 => 須更動redlistTW
redlistTW_change <- compare.table %>%
  filter(comparison_redlistTW == FALSE) %>%
  filter(comparison_name == TRUE) 

#redlistTW不一致、name一致 => 須更動redlistTW
redlistTW_NA <- compare.table %>%
  filter(is.na(comparison_redlistTW)) %>%
  filter(comparison_name == TRUE) 

write.csv(redlistTW_TRUE,file="output_redlistTW change/redlistTW_TRUE.csv",row.names = F)

#國內紅皮書比對 part II: TT需修改的分類群====
redlistTW_change <- redlistTW_change %>%
  rbind(redlistTW_NA) %>%
  select(taxonUUID,taxonRank,kingdom,class,simplifiedScientificName,categoryRedlistTW,new_redlistTW,categoryRedlistVersionTW) 
#NA="不適用"需手動調整

#種改為無，獨立確認
redlistTW_change_sp <- redlistTW_change %>%
  filter(taxonRank == "species" & new_redlistTW == "") %>%
  left_join(TT_name[c(1,4,58)],by=c("taxonUUID"="parentUUID")) %>%
  rename(infrasp_name = simplifiedScientificName.y,
         infrasp_taxonUUID = taxonUUID.y ) %>%
  left_join(compare.table[c(1,19)],by=c("infrasp_taxonUUID"="taxonUUID")) %>%
  rename(new_redlistTW = new_redlistTW.y) %>%
  mutate(comparison_redlistTW = if_else(is.na(categoryRedlistTW) & is.na(new_redlistTW), TRUE,categoryRedlistTW == new_redlistTW))

redlistTW_change_sp_output <- redlistTW_change_sp %>%
  filter(is.na(infrasp_taxonUUID) | comparison_redlistTW == FALSE ) %>% #無種下，且iucn需改為無 
  filter(!class %in% c("Actinopterygii","Aves","Amphibia","Reptilia","Mammalia")) %>%
  filter(kingdom != "Plantae") %>%
  mutate(source_process = "redlistTW_change_sp_output") %>%
  select(taxonUUID,taxonRank,kingdom,class,simplifiedScientificName.x,categoryRedlistTW,new_redlistTW.x,source_process) %>%
  rename(simplifiedScientificName = simplifiedScientificName.x,
         new_redlistTW = new_redlistTW.x)

#write.csv(redlistTW_change_sp_output,"output_redlistTW change/redlistTW_change_sp_output.csv",row.names = F)

#種下改為無，獨立確認 
redlistTW_change_infrasp <- redlistTW_change %>%
  filter(taxonRank == "infraspecies" & new_redlistTW == "") %>%
  left_join(TT_name[c(1,4,5)],by="taxonUUID") %>%
  left_join(compare.table[c(1,19)],by= c("parentUUID"="taxonUUID"))  %>%
  rename(new_redlistTW = new_redlistTW.y) %>%
  mutate(comparison_redlistTW = if_else(is.na(categoryRedlistTW) & is.na(new_redlistTW), TRUE, categoryRedlistTW == new_redlistTW),
         redlistTW_date = ifelse(new_redlistTW != "" | is.na(new_redlistTW), "2024-12-01", NA),
         comparison_date = categoryRedlistVersionTW == redlistTW_date)

redlistTW_change_infrasp_output <- redlistTW_change_infrasp %>%
  filter(is.na(comparison_redlistTW) | comparison_redlistTW == FALSE) %>% #紅皮書類別需依照種階層修改
  mutate(source_process = "redlistTW_change_infrasp_output") %>%
  select(taxonUUID,taxonRank,kingdom,class,simplifiedScientificName,categoryRedlistTW,new_redlistTW,source_process)

redlistTW_change_infrasp_output_date <- redlistTW_change_infrasp %>%
  filter(comparison_date == FALSE) %>%
  filter(class %in% c("Actinopterygii","Aves","Amphibia","Reptilia","Mammalia")) %>%
  mutate(source_process = "redlistTW_change_infrasp_output") %>%
  select(taxonUUID,taxonRank,kingdom,class,simplifiedScientificName,new_redlistTW,redlistTW_date,source_process)

#write.csv(redlistTW_change_infrasp_output_date,"output_redlistTW change/redlistTW_change_infrasp_output_date.csv",row.names = F)
#write.csv(redlistTW_change_infrasp_output,"output_redlistTW change/redlistTW_change_infrasp_output.csv",row.names = F)

#國內紅皮書需修改
redlistTW_change_output <- redlistTW_change %>%
  anti_join(redlistTW_change_sp, by = "taxonUUID") %>%
  anti_join(redlistTW_change_infrasp, by = "taxonUUID") %>%
  filter(class %in% c("Actinopterygii","Aves","Amphibia","Reptilia","Mammalia")) %>%
  mutate(source_process = "redlistTW_change_output") %>%
  select(taxonUUID,taxonRank,kingdom,class,simplifiedScientificName,categoryRedlistTW,new_redlistTW,source_process)

redlistTW_change_output_date <- redlistTW_change %>%
  anti_join(redlistTW_change_sp, by = "taxonUUID") %>%
  anti_join(redlistTW_change_infrasp, by = "taxonUUID") %>%
  filter(class %in% c("Actinopterygii","Aves","Amphibia","Reptilia","Mammalia")) %>%
  mutate(redlistTW_date = ifelse(new_redlistTW != "" | is.na(new_redlistTW), "2024-12-01", NA),
         comparison_date = categoryRedlistVersionTW == redlistTW_date) %>%
  filter(comparison_date == FALSE) %>%
  mutate(source_process = "redlistTW_change_infrasp_output") %>%
  select(taxonUUID,taxonRank,kingdom,class,simplifiedScientificName,new_redlistTW,redlistTW_date,source_process)

#write.csv(redlistTW_change_output,"output_redlistTW change/redlistTW_change_output.csv",row.names = F)
#write.csv(redlistTW_change_output_date,"output_redlistTW change/redlistTW_change_output_date.csv",row.names = F)

redlistTW_change_final_output <- bind_rows(redlistTW_change_sp_output, redlistTW_change_infrasp_output, redlistTW_change_output)

redlistTW_change_final_output <- redlistTW_change_final_output %>%
  mutate(categoryRedlistTW = recode(categoryRedlistTW,
                                    "NVU" = "國家易危（NVU，Nationally Vulnerable）",
                                    "NLC" = "國家暫無危機（NLC，Nationally Least Concern）",
                                    "NNT" = "國家接近受脅（NNT，Nationally Near-threatened）", 
                                    "RE" = "區域滅絕（RE, Regionally Extinct）", 
                                    "NEN" = "國家瀕危（NEN，Nationally Endangered）",
                                    "NCR" = "國家極度瀕危（NCR，Nationally Critically Endangered）",
                                    "DD"   =  "資料缺乏（DD, Data Deficient）",
                                    "NA" = "不適用",
                                    "EX" = "滅絕（EX, Extinct）",
                                    "NE"  ="未評估（NE, Not Evaluated）",
                                    "EW" = "野外滅絕（EW, Extinct in the Wild）"),
         new_redlistTW = recode(new_redlistTW,
                                "NVU" = "易危（VU, Vulnerable）",
                                "NLC" = "暫無危機（LC, Least Concern）",
                                "NNT" = "接近受脅（NT, Near Threatened）", 
                                "RE" = "區域滅絕（RE, Regionally Extinct）", 
                                "NEN" = "瀕危（EN, Endangered）",
                                "NCR" = "極危（CR, Critically Endangered）",
                                "DD"   =  "資料缺乏（DD, Data Deficient）",
                                "EX" = "滅絕（EX, Extinct）",
                                "NE"  ="未評估（NE, Not Evaluated）",
                                "EW" = "野外滅絕（EW, Extinct in the Wild）"),
         new_redlistTW = replace_na(new_redlistTW, "不適用"),
         comparison_redlistTW = if_else(is.na(categoryRedlistTW) & is.na(new_redlistTW), TRUE, categoryRedlistTW == new_redlistTW)) %>%
  filter(comparison_redlistTW != TRUE)

write.csv(redlistTW_change_final_output,"output_redlistTW change/redlistTW_change_final_output.csv",row.names = F)

redlistTW_change_final_output_date <- bind_rows(redlistTW_change_infrasp_output_date, redlistTW_change_output_date)

write.csv(redlistTW_change_final_output_date,"output_redlistTW change/redlistTW_change_final_output_date.csv",row.names = F)

#國內紅皮書比對 part III: bug遺珠====
TC_taxon_redlistTW_true <- TC_selected %>%
  filter(new_redlistTW != "")

extra_taxon_id <- setdiff(TC_taxon_redlistTW_true$taxon_id, combine.table$taxon_id)

extra_rows <- TC_taxon_redlistTW_true %>%
  filter(taxon_id %in% extra_taxon_id)

extra_rows <- left_join(extra_rows, TC_taxon[,c("taxon_id","kingdom","class")], by = "taxon_id")

extra_taxon_redlistTW_Animalia <- extra_rows %>%
  select(new_simple_name,taxon_id,kingdom,class,new_redlistTW) %>%
  filter(kingdom == "Animalia") %>%
  filter(class %in% c("Actinopterygii","Aves","Amphibia","Reptilia","Mammalia")) %>%
  mutate(redlistTW_date = "2024-12-01")

extra_taxon_redlistTW_Plantae <- extra_rows %>%
  select(new_simple_name,taxon_id,kingdom,class,new_redlistTW) %>%
  filter(kingdom == "Plantae")

write.csv(extra_taxon_redlistTW_Animalia, "output_redlistTW change/extra_taxon_redlistTW_Animalia.csv",row.names = F,fileEncoding = "UTF-8")
write.csv(extra_taxon_redlistTW_Plantae, "output_redlistTW change/extra_taxon_redlistTW_Plantae.csv",row.names = F,fileEncoding = "UTF-8")

#國內紅皮書比對 part IV: 出版年月====
TT_taxon_redlistTW_true <- TT_selected %>%
  filter(categoryRedlistTW != "") %>%
  filter(class %in% c("Actinopterygii","Aves","Amphibia","Reptilia","Mammalia")) %>%
  filter(categoryRedlistVersionTW != "2024-12-01") 

extra_taxon_id <- setdiff(TT_taxon_redlistTW_true$taxonUUID, redlistTW_change$taxonUUID)

extra_rows <- TT_taxon_redlistTW_true %>%
  filter(taxonUUID %in% extra_taxon_id)

extra_date_redlistTW_Animalia_TT <- extra_rows %>%
  filter(kingdom != "Plantae") %>%
  select(taxonUUID,kingdom,class,simplifiedScientificName,categoryRedlistTW,taxon_id) %>%
  mutate(redlistTW_date = "2024-12-01")

write.csv(extra_date_redlistTW_Animalia_TT , "output_redlistTW change/extra_date_redlistTW_Animalia_TT.csv",row.names = F)

#保育類比對 part I: 比對欄位 ====
if (!dir.exists("output_protected change ")) {
  dir.create("output_protected change", recursive = TRUE) 
  }

#protected一致、name一致 => 不須更動
protected_TRUE <- compare.table %>%
  filter(comparison_protected == TRUE) %>%
  filter(comparison_name == TRUE) 

#protected不一致、name一致 => 須更動protected
protected_change <- compare.table %>%
  filter(comparison_protected == FALSE) %>%
  filter(comparison_name == TRUE) 

#protected不一致、name一致 => 須更動protected
protected_NA <- compare.table %>%
  filter(is.na(comparison_protected)) %>%
  filter(comparison_name == TRUE) 

write.csv(protected_TRUE,file="output_protected change/protected_TRUE.csv",row.names = F)

#保育類比對 part II: TT需修改的分類群====
protected_change <- protected_change %>%
  rbind(protected_NA) %>%
  select(taxonUUID,taxonRank,kingdom,class,simplifiedScientificName,nativeness,protectedStatusTW,new_protected) 

#種改為無，獨立確認
protected_change_sp <- protected_change %>%
  filter(taxonRank == "species" & new_protected == "") %>%
  left_join(TT_name[c(1,4,58)],by=c("taxonUUID"="parentUUID")) %>%
  rename(infrasp_name = simplifiedScientificName.y,
         infrasp_taxonUUID = taxonUUID.y ) %>%
  left_join(compare.table[c(1,20)],by=c("infrasp_taxonUUID"="taxonUUID")) %>%
  rename(new_protected = new_protected.y) %>%
  mutate(comparison_protected = if_else(is.na(protectedStatusTW) & is.na(new_protected), TRUE,protectedStatusTW == new_protected))

protected_change_sp_output <- protected_change_sp %>%
  filter(is.na(infrasp_taxonUUID) | comparison_protected  == FALSE )%>% #無種下，且iucn需改為無 
  mutate(source_process = "protected_change_sp_output") %>%
  select(taxonUUID,taxonRank,kingdom,simplifiedScientificName.x,protectedStatusTW,new_protected.x,source_process) %>%
  rename(simplifiedScientificName = simplifiedScientificName.x,
         new_protected = new_protected.x)

#write.csv(protected_change_sp_output,"output_protected change/protectedchange_sp_output.csv",row.names = F)

#種下改為無，獨立確認 
protected_change_infrasp <- protected_change %>%
  filter(taxonRank == "infraspecies" & new_protected == "") %>%
  left_join(TT_name[c(1,4,5)],by="taxonUUID") %>%
  left_join(compare.table[c(1,20)],by= c("parentUUID"="taxonUUID"))  %>%
  rename(new_protected = new_protected.y) %>%
  mutate(comparison_protected = if_else(is.na(protectedStatusTW) & is.na(new_protected), TRUE,protectedStatusTW == new_protected))

protected_change_infrasp_output <- protected_change_infrasp %>%
  filter(is.na(comparison_protected) | comparison_protected  == FALSE) %>% #紅皮書類別需依照種階層修改
  mutate(protectedStatusTW = recode(protectedStatusTW,
                                    "I" = "瀕臨絕種保育類野生動物",
                                    "II" = "珍貴稀有保育類野生動物",
                                    "III" = "其他應予保育之野生動物",
                                    "1" = "文化資產保存法：珍貴稀有植物"),
         new_protected = recode(new_protected,
                                "I" = "瀕臨絕種保育類野生動物",
                                "II" = "珍貴稀有保育類野生動物",
                                "III" = "其他應予保育之野生動物",
                                "1" = "文化資產保存法：珍貴稀有植物")) %>%
  mutate(source_process = "protected_change_infrasp_output")   %>%
  select(taxonUUID,taxonRank,kingdom,simplifiedScientificName,protectedStatusTW,new_protected,source_process) 

#write.csv(protected_change_infrasp_output,"output_protected change/protected_change_infrasp_output.csv",row.names = F)

#保育類需修改
protected_change_output <- protected_change %>%
  anti_join(protected_change_sp, by = "taxonUUID") %>%
  anti_join(protected_change_infrasp, by = "taxonUUID") %>%
  mutate(protectedStatusTW = recode(protectedStatusTW,
                                    "I" = "瀕臨絕種保育類野生動物",
                                    "II" = "珍貴稀有保育類野生動物",
                                    "III" = "其他應予保育之野生動物",
                                    "1" = "文化資產保存法：珍貴稀有植物"),
         new_protected = recode(new_protected,
                                "I" = "瀕臨絕種保育類野生動物",
                                "II" = "珍貴稀有保育類野生動物",
                                "III" = "其他應予保育之野生動物",
                                "1" = "文化資產保存法：珍貴稀有植物")) %>%
  mutate(source_process = "protected_change_output")   %>%
  select(taxonUUID,taxonRank,kingdom,simplifiedScientificName,protectedStatusTW,new_protected,source_process) 

#write.csv(protected_change_output,"output_protected change/protected_change_output.csv",row.names = F)

protected_change_final_output <- bind_rows(protected_change_sp_output, protected_change_infrasp_output, protected_change_output)

write.csv(protected_change_final_output,"output_protected change/protected_change_final_output.csv",row.names = F)

#保育類比對 part III: bug遺珠====
TC_taxon_protected_true <- TC_selected %>%
  filter(new_protected != "")

extra_taxon_id <- setdiff(TC_taxon_protected_true$taxon_id, combine.table$taxon_id)

extra_rows <- TC_taxon_protected_true %>%
  filter(taxon_id %in% extra_taxon_id)

extra_rows <- left_join(extra_rows, TC_taxon[,c("taxon_id","kingdom","class")], by = "taxon_id")

extra_taxon_protected_Animalia <- extra_rows %>%
  select(new_simple_name,taxon_id,kingdom,class,new_protected) %>%
  filter(kingdom == "Animalia") 

extra_taxon_protected_Plantae <- extra_rows %>%
  select(new_simple_name,taxon_id,kingdom,class,new_protected) %>%
  filter(kingdom == "Plantae")

write.csv(extra_taxon_protected_Animalia, "output_protected change/extra_taxon_protected_Animalia.csv",row.names = F)
write.csv(extra_taxon_protected_Plantae, "output_protected change/extra_taxon_protected_Plantae.csv",row.names = F)

#敏感狀態修正====
if (!dir.exists("output_sensitive change")) {
  dir.create("output_sensitive change", recursive = TRUE)
}
#new_iucn 
#new_protected 
#new_redlistTW 
#protectedStatusTW
#categoryRedlistTW
#categoryIUCN
sensitive_change <- combine.table %>%
  filter(!is.na(new_simple_name)) %>%
  mutate(
    iucn_sensitive = ifelse(new_iucn %in% c("EX", "EW", "CR", "EN", "VU"), "sensitive", NA),
    protected_sensitive = ifelse(protectedStatusTW %in% c("I", "II", "III"), "sensitive", NA),
    redlistTW_sensitive = ifelse(new_redlistTW %in% c("EX", "EW", "RE", "NCR", "NEN", "NVU"), "sensitive", NA),
    sensitive = case_when(
      sensitiveCategory %in% c("分類群不開放", "重度") ~ sensitiveCategory,
      nativeness == "native" & 
        rowSums(across(c(iucn_sensitive, protected_sensitive, redlistTW_sensitive), ~ . == "sensitive"), na.rm = TRUE) > 0 ~ "輕度",
      TRUE ~ NA_character_
    ),
    date = NA,
    comparison_sensitive = sensitive == sensitiveCategory
  )

sensitive_change_output <- sensitive_change %>%
  filter(nativeness =="native",
         kingdom != "Plantae",
         comparison_sensitive == FALSE) %>%
  select(taxonUUID,taxonRank,kingdom,simplifiedScientificName,new_iucn,protectedStatusTW,new_redlistTW,sensitiveCategory,sensitive,comparison_sensitive)

write.csv(sensitive_change_output, "output_sensitive change/sensitive_change_output.csv")

#找出TT已有敏感狀態，但須改為無
TT_taxon_sensitive_true <- TT_selected %>%
  filter(sensitiveCategory != "",
         kingdom != "Plantae")

extra_taxon_id <- setdiff(TT_taxon_sensitive_true$taxonUUID, combine.table$taxonUUID)

extra_rows <- TT_taxon_sensitive_true %>%
  filter(taxonUUID %in% extra_taxon_id)

extra_taxon_sensitive_Animalia_TT <- extra_rows %>%
  select(taxonUUID,kingdom,class,simplifiedScientificName,nativeness,protectedStatusTW,categoryRedlistTW,categoryIUCN,taxon_id,sensitiveCategory) %>%
  mutate(nativeness = recode(nativeness,
                             "原生 Native" = "native",
                             "外來栽培 Cultivated (non-native)" = "non-native",
                             "外來歸化 Naturalized (non-native)" = "non-native",
                             "外來 Non-native"  = "non-native",
                             "境外 Overseas"  = "overseas"),
         protectedStatusTW = recode(protectedStatusTW,
                                    "瀕臨絕種保育類野生動物" = "I",
                                    "珍貴稀有保育類野生動物" = "II",
                                    "其他應予保育之野生動物" = "III",
                                    "文化資產保存法：珍貴稀有植物" = "1"),
         categoryRedlistTW = recode(categoryRedlistTW,
                                    "易危（VU, Vulnerable）" = "NVU",
                                    "暫無危機（LC, Least Concern）"  = "NLC",
                                    "接近受脅（NT, Near Threatened）" = "NNT", 
                                    "區域滅絕（RE, Regionally Extinct）" = "RE", 
                                    "瀕危（EN, Endangered）" = "NEN",
                                    "極危（CR, Critically Endangered）"= "NCR",
                                    "資料缺乏（DD, Data Deficient）"   = "DD" ,
                                    "滅絕（EX, Extinct）" = "EX",
                                    "不適用"   = "NA",
                                    "未評估（NE, Not Evaluated）"  = "NE",
                                    "野外滅絕（EW, Extinct in the Wild）" = "EW"),
         categoryIUCN = recode(categoryIUCN,
                               "未評估（NE, Not Evaluated）" = "NE",
                               "接近受脅（NT, Near Threatened）" = "NT" ,
                               "易危（VU, Vulnerable）" =  "VU",
                               "極危（CR, Critically Endangered）"= "CR",
                               "瀕危（EN, Endangered）" = "EN",
                               "暫無危機（LC, Least Concern）" =  "LC" ,
                               "資料缺乏（DD, Data Deficient）"= "DD",
                               "野外滅絕（EW, Extinct in the Wild）"="EW"))%>%
  mutate(
    iucn_sensitive = ifelse(categoryIUCN %in% c("EX", "EW", "CR", "EN", "VU"), "sensitive", NA),
    protected_sensitive = ifelse(protectedStatusTW %in% c("I", "II", "III"), "sensitive", NA),
    redlistTW_sensitive = ifelse(categoryRedlistTW %in% c("EX", "EW", "RE", "NCR", "NEN", "NVU"), "sensitive", NA),
    sensitive = case_when(
      sensitiveCategory %in% c("分類群不開放", "重度") ~ sensitiveCategory,
      nativeness == "native" & 
        rowSums(across(c(iucn_sensitive, protected_sensitive, redlistTW_sensitive), ~ . == "sensitive"), na.rm = TRUE) > 0 ~ "輕度",
      TRUE ~ NA_character_
    ),
    date = NA,
    comparison_sensitive = sensitive == sensitiveCategory
  )

write.csv(extra_taxon_sensitive_Animalia_TT , "output_sensitive change/extra_taxon_sensitive_Animalia_TT.csv",row.names = F)

## 使用以上邏輯，產生出TT_selected中，sensitiveCategory "輕度" 不符合條件的表單
TT_selected_light_sensitive_wrong <- combine.table %>%
  filter(sensitiveCategory == "輕度") %>%
  mutate(
    iucn_sensitive = ifelse(new_iucn %in% c("EX", "EW", "CR", "EN", "VU"), "sensitive", NA),
    protected_sensitive = ifelse(protectedStatusTW %in% c("I", "II", "III"), "sensitive", NA),
    redlistTW_sensitive = ifelse(new_redlistTW %in% c("EX", "EW", "RE", "NCR", "NEN", "NVU"), "sensitive", NA),
    sensitive_check = case_when(
      nativeness == "native" &
        rowSums(across(c(iucn_sensitive, protected_sensitive, redlistTW_sensitive), ~ . == "sensitive"), na.rm = TRUE) > 0 ~ "輕度",
      TRUE ~ NA_character_
    )
  ) %>%
  # 過濾出不符合條件的
  filter(sensitive_check != "輕度" | is.na(sensitive_check)) 

#TT_selected_light_sensitive_wrong <- TT_selected_light_sensitive_wrong %>%
 # filter(taxonRank=="species")

write.csv(TT_selected_light_sensitive_wrong, "output_sensitive change/TT_selected_light_sensitive_wrong.csv",row.names = F)

#總結表單 ====
output_folders <- c("output_namecheck","output_sensitive change", "output_redlistTW change","output_IUCN change","output_protected change")
# 取得所有CSV檔案
all_csv_files <- list.files(path = output_folders, pattern = "\\.csv$", recursive = TRUE, full.names = TRUE)

# 建立 summary 表單
library(readr)
library(fs)
summary_table <- lapply(output_folders, function(folder) {
  csv_files <- list.files(path = folder, pattern = "\\.csv$", full.names = TRUE)
  
  lapply(csv_files, function(file) {
    data <- read_csv(file, show_col_types = FALSE)
    data.frame(
      TC_version = TC_version,
      TT_version = TT_version,
      folder_name = basename(folder),
      file_name = basename(file),
      row_count = nrow(data)
    )
  }) %>% bind_rows()
}) %>% bind_rows()

# 顯示 summary
print(summary_table)

# 可選：輸出為 summary 表單
write.csv(summary_table, "summary_table.csv", row.names = FALSE)
