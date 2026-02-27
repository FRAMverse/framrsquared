## script to test that the SONCC functions work.


fram_path <- "C:/Users/edwc1477/OneDrive - Washington State Executive Branch Agencies/Documents/WDFW FRAM team work/committees and teams/STT 2026/../../FRAM runs/2026 season/2026NOF_CohoFRAMdatabase_DRAFT/2026NOF_CohoFRAMdatabase_DRAFT.mdb"

filepath <- "C:/Users/edwc1477/OneDrive - Washington State Executive Branch Agencies/Documents/WDFW FRAM team work/committees and teams/STT 2026/test_soncc_pastable_pkg2.xlsx"

library(framrsquared)
fram_db <- connect_fram_db(fram_path)

create_soncc_pasteable(fram_db, run_id = 152, filepath)

disconnect_fram_db(fram_db)
