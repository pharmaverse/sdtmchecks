# sdtmchecks (development version)

## Refinements of existing data check functions

* [`check_ss_ssdtc_dead_ds`](https://pharmaverse.github.io/sdtmchecks/reference/check_ss_ssdtc_dead_ds.html) updated to include pre-processing input and return RAVE column if applicable ([#263](https://github.com/pharmaverse/sdtmchecks/pull/263))



## Metadata updates

* [`sdtmchecksmeta.RData`](https://pharmaverse.github.io/sdtmchecks/reference/sdtmchecksmeta.html) with added `preproc=roche_derive_rave_row` 



# sdtmchecks 0.1.8 "Bubble and Squeak"

## Refinements of existing data check functions

* [`check_tu_rs_new_lesions`](https://pharmaverse.github.io/sdtmchecks/reference/check_tu_rs_new_lesions.html) updated to include overall response of `PMD` as an indicator of progressive disease. Visit info also added to check result. ([#197](https://github.com/pharmaverse/sdtmchecks/pull/197))
* [`check_ae_fatal`](https://pharmaverse.github.io/sdtmchecks/reference/check_ae_fatal.html) updated to address bug that was causing warning. Logic was subsequently streamlined. ([#215](https://github.com/pharmaverse/sdtmchecks/pull/215))
* Checks that expect specific preferred terms identifying Covid-19 related AEs were updated to warn if that metadata was not provided. ([#220](https://github.com/pharmaverse/sdtmchecks/pull/220) & [#223](https://github.com/pharmaverse/sdtmchecks/pull/223))
* `--SEQ` variables were removed from checks. ([#246](https://github.com/pharmaverse/sdtmchecks/pull/246/))


## Metadata updates

* [`sdtmchecksmeta.RData`](https://pharmaverse.github.io/sdtmchecks/reference/sdtmchecksmeta.html) with descriptive update related to Progressive Metabolic Disease ("PMD") inclusion in [`check_tu_rs_new_lesions`](https://pharmaverse.github.io/sdtmchecks/reference/check_tu_rs_new_lesions.html)  ([#197](https://github.com/pharmaverse/sdtmchecks/pull/197))


## Documentation updates

  * Roxygen2 header updated to include `@family` and `@keyword` for data check function categories OPHTH and COVID  ([#214](https://github.com/pharmaverse/sdtmchecks/pull/214)) - applied to the following data checks:
    * COVID: 
      * check_ae_aeacn_ds_disctx_covid.R
      * check_ae_aeacnoth_ds_stddisc_covid.R
      * check_dv_ae_aedecod_covid.R
      * check_dv_covid.R
    * OPHTH:
      * check_ae_aelat.R
      * check_cm_cmlat.R
      * check_cm_cmlat_prior_ocular.R
      * check_oe_bcva_1m_late_early_tot.R
      * check_oe_bcva_4m_late_early_tot.R
      * check_oe_bcva_4m_vs_1m_req.R
      * check_oe_bcva_tot_mismatch.R
      * check_oe_sc_lat_count_fingers.R
      * check_pr_prlat.R 
      * check_sc_dm_eligcrit.R
      * check_sc_dm_seyeselc.R
  


# sdtmchecks 0.1.6 "Bubble and Squeak"

## Refinements of existing data check functions

* Minor update to return message for [`check_tu_rs_new_lesions()`](https://pharmaverse.github.io/sdtmchecks/reference/check_tu_rs_new_lesions.html) and more header examples added ([#98](https://github.com/pharmaverse/sdtmchecks/issues/98))
* Typo in return message corrected for [`check_ae_aetoxgr()`](https://pharmaverse.github.io/sdtmchecks/reference/check_ae_aetoxgr.html) ([#187](https://github.com/pharmaverse/sdtmchecks/issues/187))


## New functions
* Added [`xls2list()`](https://pharmaverse.github.io/sdtmchecks/reference/xlsx2list.html) reporting function to create a list from spreadsheet tabs using the {openxlsx} package ([#85](https://github.com/pharmaverse/sdtmchecks/pull/85))

## Metadata updates

* [`sdtmchecksmeta.RData`](https://pharmaverse.github.io/sdtmchecks/reference/sdtmchecksmeta.html) saves as version=2 not version=3 for backwards compatibility with R < 3.5.0 when .RData loaded ([#84](https://github.com/pharmaverse/sdtmchecks/issues/84))

## Documentation updates

* **New vignettes:** 
  * [Search Data Check Functions](https://pharmaverse.github.io/sdtmchecks/articles/search_checks.html) published as article on pkgdown site to allow users to search data check functions (`check_xx....R`) ([#71](https://github.com/pharmaverse/sdtmchecks/issues/71))
  * [FAQs page](https://pharmaverse.github.io/sdtmchecks/articles/faqs.html) ([#130](https://github.com/pharmaverse/sdtmchecks/pull/130))

* Other minor updates: 
  * Edited `@title` of `check_ae_aeacn_ds_disctx_covid()`, `check_ae_aeacnoth_ds_stddisc_covid()` to be single line to address warning during package build
  * Edited headers of `dtc_dupl_early()` and `fail()` in utils.R
  * Commented out template assigned in [_pkgdown.yml](https://github.com/pharmaverse/sdtmchecks/blob/main/_pkgdown.yml) file ([#104](https://github.com/pharmaverse/sdtmchecks/issues/104))
  * Added clickable version releases as links in News dropdown of pkgdown site ([#121](https://github.com/pharmaverse/sdtmchecks/pull/121))
  * Edited headings in [Get started article](https://pharmaverse.github.io/sdtmchecks/articles/sdtmchecks.html) ([#114](https://github.com/pharmaverse/sdtmchecks/pull/114))
  * Added a few bullet points to [Writing a New Check](https://r.roche.com/s/75335867330e8e3b52af3/files/pharmaverse/sdtmchecks/docs/articles/write_a_check.html)
  * Updated [pkgdown site](https://pharmaverse.github.io/sdtmchecks/index.html) based on latest version of {Roxygen2} ([v7.2.3](https://github.com/r-lib/roxygen2/releases/tag/v7.2.3)) instead of ([v7.1.1](https://github.com/r-lib/roxygen2/releases/tag/v7.1.1)). DESCRIPTION reflects version in `RoxygenNote`, and associated man/*.Rd files updated. 
  * Roxygen2 header updates to include `@family` and `@keyword` for functions in `run_all_checks.R`, `run_check.R`, `utils.R` ([#85](https://github.com/pharmaverse/sdtmchecks/pull/85))
  * Moved sdtmchecks package information from `utils.R` to `sdtmchecks-package.R` ([#85](https://github.com/pharmaverse/sdtmchecks/pull/85)) 

## Misc package updates

* **Unit tests:** 
  * Added unit testing (copied in from earlier internal Roche package version) ([#126](https://github.com/pharmaverse/sdtmchecks/pull/126),  [#112](https://github.com/pharmaverse/sdtmchecks/pull/112))
  * Included more unit tests to expand coverage ([#141](https://github.com/pharmaverse/sdtmchecks/issues/141)), with thanks to `@harriscw` ([#137](https://github.com/pharmaverse/sdtmchecks/pull/137), [#144](https://github.com/pharmaverse/sdtmchecks/pull/144), [#145](https://github.com/pharmaverse/sdtmchecks/pull/145), [#155](https://github.com/pharmaverse/sdtmchecks/pull/155), [#162](https://github.com/pharmaverse/sdtmchecks/pull/162)), `@J-Lox` ([#147](https://github.com/pharmaverse/sdtmchecks/pull/147), [#156](https://github.com/pharmaverse/sdtmchecks/pull/156)), `@laywang142` ([#199](https://github.com/pharmaverse/sdtmchecks/pull/199), [#158](https://github.com/pharmaverse/sdtmchecks/pull/158), [#153](https://github.com/pharmaverse/sdtmchecks/pull/153), [#146](https://github.com/pharmaverse/sdtmchecks/pull/146)), `@sarabodach` ([#150](https://github.com/pharmaverse/sdtmchecks/pull/150), [#151](https://github.com/pharmaverse/sdtmchecks/pull/151), [#159](https://github.com/pharmaverse/sdtmchecks/pull/159), [#160](https://github.com/pharmaverse/sdtmchecks/pull/160), [#183](https://github.com/pharmaverse/sdtmchecks/pull/183))
  * Added GitHub action for test coverage report produced by [{covr}](https://covr.r-lib.org/index.html) and uploaded to [codecov](https://about.codecov.io/) ([#163](https://github.com/pharmaverse/sdtmchecks/pull/163))
* **README:** 
  * Include badges on README:
    * Test coverage badge from [{covr}](https://covr.r-lib.org/index.html) ([#122](https://github.com/pharmaverse/sdtmchecks/issues/122), [#133](https://github.com/pharmaverse/sdtmchecks/issues/133))
    * R CMD Check badge ([#132](https://github.com/pharmaverse/sdtmchecks/pull/132))
    * CRAN status badge ([#132](https://github.com/pharmaverse/sdtmchecks/pull/132))
  * README.Rmd added to render README.md ([#172](https://github.com/pharmaverse/sdtmchecks/issues/172))
  * Add installation instructions from "https://pharmaverse.r-universe.dev" (devel branch, default) [#195](https://github.com/pharmaverse/sdtmchecks/issues/195), [#226](https://github.com/pharmaverse/sdtmchecks/pull/226)
  * Specify recommended installation from main branch: `devtools::install_github("pharmaverse/sdtmchecks", ref = "main")`
* Updated **DESCRIPTION** to specify: 
  * New dependency: [{testthat}](https://testthat.r-lib.org/)
  * GitHub as Repository for sdtmchecks ([#123](https://github.com/pharmaverse/sdtmchecks/issues/123))
  * Config/testthat/edition: 3 ([#138](https://github.com/pharmaverse/sdtmchecks/pull/138))
* Implemented [{renv}](https://rstudio.github.io/renv/articles/renv.html) for package dependency management, adding renvignore, renv.lock, renv subfolder and including updated .Rprofile ([#111](https://github.com/pharmaverse/sdtmchecks/issues/111))
* Package version 0.1.5.1 used prior to update to 0.1.6




# sdtmchecks 0.1.5 "Hunter Pi" 

## New data check functions

* [`check_ds_multdeath_dsstdtc()`](https://pharmaverse.github.io/sdtmchecks/reference/check_ds_multdeath_dsstdtc.html) - flags if DS has multiple non-missing death dates in DSSTDTC that do not match ([#62](https://github.com/pharmaverse/sdtmchecks/issues/62))

## Refinements of existing data check functions

* Updated existing data checks to list the `n` function from {dplyr} in the header ([#67](https://github.com/pharmaverse/sdtmchecks/issues/67)):
  - [`check_dm_usubjid_dup()`](https://pharmaverse.github.io/sdtmchecks/reference/check_dm_usubjid_dup.html)
  - [`check_qs_dup()`](https://pharmaverse.github.io/sdtmchecks/reference/check_qs_dup.html)

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
* Other general utility functions that are invoked within [`check_xx....R` functions](https://pharmaverse.github.io/sdtmchecks/reference/index.html#data-checks) consolidated from separate .R scripts into `utils.R`: 
  + [`pass()`](https://pharmaverse.github.io/sdtmchecks/reference/pass.html)
  + [`fail()`](https://pharmaverse.github.io/sdtmchecks/reference/fail.html)
  + [`is_sas_na()`](https://pharmaverse.github.io/sdtmchecks/reference/is_sas_na.html)
  + [`truncate_var_strings()`](https://pharmaverse.github.io/sdtmchecks/reference/truncate_var_strings.html)
  + [`dtc_dupl_early()`](https://pharmaverse.github.io/sdtmchecks/reference/dtc_dupl_early.html)
  + [`impute_day01()`](https://pharmaverse.github.io/sdtmchecks/reference/impute_day01.html)
  + [`convert_var_to_ascii()`](https://pharmaverse.github.io/sdtmchecks/reference/convert_var_to_ascii.html)
  + [`%lacks_all%()`](https://pharmaverse.github.io/sdtmchecks/reference/grapes-lacks_all-grapes.html), [`%lacks_any%()`](https://pharmaverse.github.io/sdtmchecks/reference/grapes-lacks_any-grapes.html), [`lacks_msg()`](https://pharmaverse.github.io/sdtmchecks/reference/lacks_msg.html), [`%has_all%()`](https://pharmaverse.github.io/sdtmchecks/reference/grapes-has_all-grapes.html),  and [`%has_any%()`](https://pharmaverse.github.io/sdtmchecks/reference/grapes-has_any-grapes.html) (from lacks_has.R) 
* README, LICENSE, DESCRIPTION, _pkgdown.yml, vignette files tailored to github.com/pharmaverse ([#1](https://github.com/pharmaverse/sdtmchecks/issues/1))
* Package logo added to man/figures
* Package site created via [pkgdown](https://pkgdown.r-lib.org/index.html)

## New data check functions (post-migration)
* `check_ae_aeacnoth_ds_disctx()` flags if an AE record indicates person discontinued from study but there is no corresponding DS record indicating study discontinuation (`where DS.DSSCAT = "STUDY COMPLETION/EARLY DISCONTINUATION" and DS.DSDECOD != "COMPLETED"`) ([#3](https://github.com/pharmaverse/sdtmchecks/issues/3))
