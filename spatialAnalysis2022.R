library(readr)

shlaSpatial22 <- read_csv(file = "hornedLarks/tbl_all_potential_survey_sites_with_2022_results_04_06_2023.csv",
                          col_select = c(1:3, 61:77, 80:81))
