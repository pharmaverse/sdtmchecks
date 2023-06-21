# sdtmchecks 0.1.6 --- not deployed yet  

## Refinements of existing data check functions

* Minor update to return message for `check_tu_rs_new_lesions()` ([#98](https://github.com/pharmaverse/sdtmchecks/issues/98))

## Metadata updates

* Update [`sdtmchecksmeta.RData`](https://pharmaverse.github.io/sdtmchecks/reference/sdtmchecksmeta.html) as version=2 not version=3 for backwards compatibility with R < 3.5.0 ([#84](https://github.com/pharmaverse/sdtmchecks/issues/84))

## Documentation updates

* Search of data check functions (`check_xx....R`) published as [Vignette](https://pharmaverse.github.io/sdtmchecks/articles/search_checks.html) ([#71](https://github.com/pharmaverse/sdtmchecks/issues/71))
* Remove template in _pkgdown.yml file ([#104](https://github.com/pharmaverse/sdtmchecks/issues/104))
* Add clickable version releases as links in News dropdown of pkgdown site ([#121](https://github.com/pharmaverse/sdtmchecks/pull/121))
* Updates to sdtmchecks.Rmd ([#114](https://github.com/pharmaverse/sdtmchecks/pull/114))
* Roxygen2 header updates

## Misc package updates

* Add unit testing (copied in from earlier internal package version) ([#126](https://github.com/pharmaverse/sdtmchecks/pull/126), [#112](https://github.com/pharmaverse/sdtmchecks/pull/112))
* Update DESCRIPTION for new dependencies (testthat, DT) and Repository parameter
<!-- * Use [{renv}](https://rstudio.github.io/renv/articles/renv.html) for package dependency management, adding renvignore, renv.lock, renv subfolder and including updated .Rprofile ([#111](https://github.com/pharmaverse/sdtmchecks/issues/111))--> 


# sdtmchecks 0.1.5 "Hunter Pi" 

## New data check functions

* `check_ds_multdeath_dsstdtc()` - flags if DS has multiple non-missing death dates in DSSTDTC that do not match ([#62](https://github.com/pharmaverse/sdtmchecks/issues/62))

## Refinements of existing data check functions

* Updated existing data checks to list the `n` function from dplyr in the header ([#67](https://github.com/pharmaverse/sdtmchecks/issues/67)):
  - `check_dm_usubjid_dup()`
  - `check_qs_dup()`

## Metadata updates

* Metadata corresponding to new data check added to [`sdtmchecksmeta.RData`](https://pharmaverse.github.io/sdtmchecks/reference/sdtmchecksmeta.html) ([#63](https://github.com/pharmaverse/sdtmchecks/pull/63))
* Update [`sdtmchecksmeta.RData`](https://pharmaverse.github.io/sdtmchecks/reference/sdtmchecksmeta.html) to reference MedDRA v26.0 instead of v25.1  ([#89](https://github.com/pharmaverse/sdtmchecks/issues/89))

## Misc updates

* Clarification added to "Writing a New Check" [Vignette](https://pharmaverse.github.io/sdtmchecks/articles/write_a_check.html) if contributor does not have write access to the repo ([#65](https://github.com/pharmaverse/sdtmchecks/issues/65))



# sdtmchecks 0.1.4 "Very Cool Nickname"

## Refinements of existing data check functions

* Updated existing data checks
  - `check_lb_lbstresu()` - add more text strings (NEGATIVE, POSITIVE, NOT DONE) allowed for LBORRES to reduce false positives ([#27](https://github.com/pharmaverse/sdtmchecks/issues/27))
  - `check_tr_trstresn_ldiam()` - include a return message that summarizes total TR records that are missing, NOT DONE, NOT EVALUABLE to help with interpreting the output ([#27](https://github.com/pharmaverse/sdtmchecks/issues/27))
  - `check_ae_aeacnoth_ds_disctx()` - corrected typo in return message ([#56](https://github.com/pharmaverse/sdtmchecks/issues/56))

## New functions

* New helper function utilities added to `util.R`
  + `create_R_script()` can use `sdtmchecksmeta.RData` as input to programmatically generate an R script with function calls ([#22](https://github.com/pharmaverse/sdtmchecks/issues/22))
  + `report_to_xlsx()` to generate the output results from selected data check functions as an .xlsx file including a tab for each check with potential discrepancies flagged and a summary page with conditional formatting to provide an overview of the results ([#11](https://github.com/pharmaverse/sdtmchecks/issues/11))

## New documentation

* "Writing a New Check" published as [Vignette](https://pharmaverse.github.io/sdtmchecks/articles/write_a_check.html) ([#5](https://github.com/pharmaverse/sdtmchecks/issues/5))

## Misc updates

* Add CI/CD workflow ([#2](https://github.com/pharmaverse/sdtmchecks/issues/2), [#8](https://github.com/pharmaverse/sdtmchecks/issues/8))
* Add package nickname as [`nickname.RData`](https://pharmaverse.github.io/sdtmchecks/reference/nickname.html) and reference in `data.R`, `globals.R` ([#12](https://github.com/pharmaverse/sdtmchecks/issues/12))
* Remove repeated occurrence of `convert_var_to_ascii()` from utils.R ([#31](https://github.com/pharmaverse/sdtmchecks/issues/31))



# sdtmchecks 0.1.2 "The one without the nickname"

## Misc updates

* Removed package nickname from Description



# sdtmchecks 0.1.1 "Open Up" 

## Initial package deployment
* Initial deployment of open-source sdtmchecks [Pharmaverse](https://github.com/pharmaverse) package, based on code migrated from the internal Roche sdtmchecks package, which includes a variety of data checks and utility functions
* All data check functions (`check_xx....R`) modified from Roche-specific version to include: 
  + Updated [roxygen2](https://roxygen2.r-lib.org/) headers with generalized examples
  + Pre-processing in the function call (`preproc=identity,...`) to allow company-specific pre-processing
* Proprietary dataset call-ins removed
* Metadata corresponding to all data check functions [`sdtmchecksmeta.RData`](https://pharmaverse.github.io/sdtmchecks/reference/sdtmchecksmeta.html) added to the data subdirectory; Roche-specific acronyms removed from descriptive text
* Utility functions added within separate `roche_utils.R` file for Roche-specific processing and to serve as a reference for implementation of company-specific pre-processing
* `globals.R` added with explicit list to pass through `utils::globalVariables()`
* Other general utility functions that are invoked within `check_xx....R` functions consolidated from separate .R scripts into `utils.R`: 
  + `pass()`
  + `fail()`
  + `is_sas_na()`
  + `truncate_var_strings()`
  + `dtc_dupl_early()`
  + `impute_day01()`
  + `convert_var_to_ascii()`
  + [`%lacks_all%()`](https://pharmaverse.github.io/sdtmchecks/reference/grapes-lacks_all-grapes.html), [`%lacks_any%()`](https://pharmaverse.github.io/sdtmchecks/reference/grapes-lacks_any-grapes.html), `lacks_msg()`, [`%has_all%()`](https://pharmaverse.github.io/sdtmchecks/reference/grapes-has_all-grapes.html),  and [`%has_any%()`](https://pharmaverse.github.io/sdtmchecks/reference/grapes-has_any-grapes.html) (from `lacks_has.R`) 
* README, LICENSE, DESCRIPTION, _pkgdown.yml, vignette files tailored to github.com/pharmaverse ([#1](https://github.com/pharmaverse/sdtmchecks/issues/1))
* Package logo added to man/figures
* Package site created via [pkgdown](https://pkgdown.r-lib.org/index.html)

## New data check functions (post-migration)
* `check_ae_aeacnoth_ds_disctx()` flags if an AE record indicates person discontinued from study but there is no corresponding DS record indicating study discontinuation (`where DS.DSSCAT = "STUDY COMPLETION/EARLY DISCONTINUATION" and DS.DSDECOD != "COMPLETED"`) ([#3](https://github.com/pharmaverse/sdtmchecks/issues/3))
