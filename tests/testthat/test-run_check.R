library(sdtmchecks)


### Experimental 

###  One possible approach to testing the function `run_check()`

### Compare result function `check_ae_aeacnoth` to `run_check()` for same data. 

### The data is dummy data, copied from example in check_ae_aeacnoth.R  


### To generalize:
###    One approach might be to collect a number of dummy datasets previously used as examples.


### Assemble dummy datatests (nested list)
source("~/code/try_things_here/sdtm_checks_PLAY/010_toy_datasets_for_testing.R")
L


### Select single check 
metads = sdtmchecksmeta |> dplyr::filter(check == "check_ae_aeacnoth")
metads$check


### Select one dummy dataset
AE=L[[c("check_ae_aeacnoth", "pass", "one")]]
ae=AE


### compare, when expect pass:
result1 =  check_ae_aeacnoth(AE)


result2=run_check(check = metads$check,
          fxn_in = metads$fxn_in,
          xls_title = metads$xls_title,
          pdf_title = metads$pdf_title,
          pdf_subtitle = metads$pdf_subtitle,
          pdf_return = metads$pdf_return,
          verbose=T)

result1

#  TODO .. no simple T/F to check ?? 
result2



### compare, when expect fail
AE =L[[c("check_ae_aeacnoth", "fail", "one")]]
ae=AE

result1 = check_ae_aeacnoth(AE)
result1

# 
result2=run_check(check = metads$check,
          fxn_in = metads$fxn_in,
          xls_title = metads$xls_title,
          pdf_title = metads$pdf_title,
          pdf_subtitle = metads$pdf_subtitle,
          pdf_return = metads$pdf_return,
          verbose=T)


identical(
attr(result1, "msg"),
result2$msg)

identical(
attr(result1, "data"),
result2$data
)

