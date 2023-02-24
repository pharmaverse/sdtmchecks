# sdtmchecks 0.1.4 "Very Cool Nickname"

##  Refinements of existing checks

* Updated existing data checks
  - `check_lb_lbstresu.R` - add more text strings (NEGATIVE, POSITIVE, NOT DONE) allowed for LBORRES to reduce false positives (#27)
  - `check_tr_trstresn_ldiam.R` - include a return message that summarizes total TR records that are missing, NOT DONE, NOT EVALUABLE to help with interpreting the output (#27) 

## New functions

* New helper function utilities added to `util.R`
  + `create_R_script()` can use sdtmchecksmeta as input to programmatically generate an R script with function calls (#22)
  + `report_to_xlsx()` to generate the output results from selected data check functions as an .xlsx file including a tab for each check with potential discrepancies flagged and a summary page with conditional formatting to provide an overview of the results (#11)

## New documentation

* Vignette "Writing a New Check" published to https://pharmaverse.github.io/sdtmchecks/articles/write_a_check.html (#5)

## Misc updates

* Add CI/CD workflow (#2, #8)
* Add package nickname as `nickname.RData` and reference in `data.R`, `globals.R` (#12)



# sdtmchecks 0.1.2 "The one without the nickname"

## Misc updates

* Removed package nickname from Description



# sdtmchecks 0.1.1 "Open Up" 

## Initial package deployment
* Initial deployment of open-source sdtmchecks Pharmaverse package, based on code migrated from the internal Roche sdtmchecks package, which includes a variety of data checks and utility functions
* All data check functions (`check_xx....R`) modified from Roche-specific version to include: 
  + Updated example headers with generalized examples
  + Pre-processing in the function call (`preproc=identity,...`) to allow company-specific pre-processing
* Proprietary datasets call-ins removed
* Metadata `sdtmchecksmeta.RData` in the data subdirectory with Roche-specific acronyms removed
* Utility `roche_utils.R` added for Roche-specific processing and to serve as a reference for implementation of company-specific pre-processing
* `globals.R` added with explicit list to pass through `utils::globalVariables()`
* Other general utility functions that are invoked within `check_xx....R` functions consolidated from separate .R scripts into `utils.R`: 
  + `pass()`
  + `fail()`
  + `is_sas_na()`
  + `truncate_var_strings()`
  + `dtc_dupl_early()`
  + `impute_day01()`
  + `convert_var_to_ascii()`
  + `%lacks_all%()`, `%lacks_any%()`, `lacks_msg()`, `%has_all%()`,  and `%has_any%()` (from `lacks_has.R`) 
* Readme, License, Description, vignette files tailored to github.com/pharmaverse (#1)
* Package site created via Pkgdown

## New data check functions (post-migration)
* `check_ae_aeacnoth_ds_disctx.R` flags if an AE record indicates person discontinued from study but there is no corresponding DS record indicating study discontinuation (where DS.DSSCAT = "STUDY COMPLETION/EARLY DISCONTINUATION" and DS.DSDECOD != "COMPLETED") (#3)
