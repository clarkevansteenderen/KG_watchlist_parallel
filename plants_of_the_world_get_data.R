library(rWCVP)
#remotes::install_github('matildabrown/rWCVPdata')
library(rWCVPdata)
library(dplyr)

?rWCVP::wcvp_summary
?wcvp_occ_mat
?wcvp_checklist

maur_code = get_wgsrpd3_codes("Mauritius")
reun_code = get_wgsrpd3_codes("Réunion")
rodrigues_code = get_wgsrpd3_codes("Rodrigues")

checklist.rod = rWCVP::wcvp_checklist(area = rodrigues_code, synonyms = TRUE)
checklist.maur = rWCVP::wcvp_checklist(area = maur_code, synonyms = TRUE)

(endemic_summary.rod = checklist.rod %>% 
    distinct(taxon_name, endemic) %>% 
    group_by(endemic) %>% 
    summarise(number.of.sp = n()))

(endemic_summary.maur = checklist.maur %>% 
    distinct(taxon_name, endemic) %>% 
    group_by(endemic) %>% 
    summarise(number.of.sp = n()))

rod.introduced = checklist.rod %>%
  filter(occurrence_type == "introduced" & area_code_l3 == rodrigues_code)

maur.introduced = checklist.maur %>%
  filter(occurrence_type == "introduced" & area_code_l3 == maur_code)
  

# endemics list
endemics = checklist %>%
  filter(endemic == TRUE)

write.csv(endemics, row.names = F, "exclude/pow_maur_endemics.csv")

write.csv(rod.introduced, row.names = F, "exclude/pow_rod_introduced.csv")
write.csv(maur.introduced, row.names = F, "exclude/pow_maur_introduced.csv")
