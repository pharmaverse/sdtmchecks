# Metadata for sdtmchecks
#
# A dataset containing the SDTM checks in the package. The data can be used as input into functions.
#
# format - A data frame with a row for each R check in the package:
#
#   - check - R check name, without .R file extension}
#   - category - Therapeutic area grouping}
#   - priority - High, Medium, Low}
#   - domains - SDTM domains used in function}
#   - xls_title - Excel title for tab}
#   - pdf_title - PDF title for check}
#   - pdf_subtitle - PDF subtitle for check, with * at the start of each subtitle line}
#   - pdf_return - PDF return message when SDTM domain not available}
#
# source - https://docs.google.com/spreadsheets/d/12InU-fUT6heO5pbjVxlTI-t6p_tlVzWL5XNpXq48NPE/edit?ts=5f52b260#gid=0}
#
# docType - data
# keywords - datasets
# name - sdtmchecksmeta
# usage - load("data/sdtmchecksmeta.RData")
# archived copy stored with system date in inst/data_archive


## -----------------------------------------------------------------------------
## -------   Set-up ------------------------------------------------------------

#install.packages("googlesheets4", repos =  c(CRAN = 'https://cloud.r-project.org'))


library(googlesheets4)
library(dplyr)

# Authorize up front because googlesheets doesn't stop code execution to auth
googlesheets4::gs4_auth()



## -----------------------------------------------------------------------------
## -------   INPUT: Read in gDoc   ---------------------------------------------


sdtmchecksmeta_url <- "https://docs.google.com/spreadsheets/d/12InU-fUT6heO5pbjVxlTI-t6p_tlVzWL5XNpXq48NPE/edit?ts=5f52b260#gid=0"
sdtmchecksmeta <- googlesheets4::read_sheet(sdtmchecksmeta_url)





## -------   Replace placeholder in pdf_subtitle with MedDRA version   ---------

# Assign MedDRA version for Covid-19 checks
# only update this in May and November when updating and re-running covid.R
## MedDRA &MEDDRV. COVID-19 SMQ (Narrow) --> MedDRA v23.1 COVID-19 SMQ (Narrow)

MEDDRV <- "v27.0"
sdtmchecksmeta$pdf_subtitle <- gsub("&MEDDRV.", MEDDRV, sdtmchecksmeta$pdf_subtitle)


## -------   Drop administrative columns ---------------------------------------

# Exclude columns beginning with 'open'
sdtmchecksmeta <- sdtmchecksmeta %>%
    select(-starts_with("OPEN", ignore.case = TRUE))


## -------   Derive 'pdf_return' based on 'domains' ----------------------------

# PRELIM: count the number of domain inputs
sdtmchecksmeta$domains_n=lengths(regmatches(sdtmchecksmeta$domains, gregexpr(",", sdtmchecksmeta$domains))) + 1
table(sdtmchecksmeta$domains_n,exclude=NULL)

# PRELIM: rename pdf_return from the spreadsheet (This column has been removed from the gDoc)
#names(sdtmchecksmeta)[names(sdtmchecksmeta) == "pdf_return"] <- "gdoc_pdf_return"

# PRELIM: assign pdf_return
pdf_string <- " does not exist or failed to read"

# PRELIM: clean domains column
sdtmchecksmeta$domains2=gsub("[[:space:]]", "", toupper(sdtmchecksmeta$domains))
head(sdtmchecksmeta$domains2)
# ck_domains <- sdtmchecksmeta %>%
#     select(starts_with("DOMAINS", ignore.case = TRUE))
# ck_domains


#======== pdf_return ========
sdtmchecksmeta$pdf_return = with(sdtmchecksmeta,
                                 ifelse(domains_n==1, paste0(domains2, pdf_string),
                                        ifelse(domains_n==2, paste0(gsub(",", " and/or ", domains2), pdf_string),
                                               ifelse(domains_n>=3, paste0("One or more of ", gsub(",", "/", domains2), pdf_string), ""))))

sdtmchecksmeta = sdtmchecksmeta %>%
    mutate(pdf_return = ifelse(category=="COVID" & domains != "dv", paste(pdf_return,"or covid terms not found"),pdf_return))
head(sdtmchecksmeta$pdf_return)

## -------   Derive 'fxn_in' & 'exist_string' based on 'domains' --------------------------------


# PRELIM: Create variables with strings needed to run code
myvec=strsplit(sdtmchecksmeta$domains,",")


# PRELIM:
#   This code creates a column that contains R code evaluated by the app
#   It just defines the parameters for each function
mylist=lapply(myvec,function(x) paste0(trimws(toupper(x)),"=",trimws(x)))
sdtmchecksmeta$fxn_in=unlist(lapply(mylist,function(x) paste0(x,collapse=",")))

###
# Roche specific
###

# e.g. handling the situation where TS may or may not be available.

sdtmchecksmeta = sdtmchecksmeta %>%
    mutate(fxn_in_roche = ifelse(!is.na(roche_extra_params),
                               paste0(fxn_in,",",roche_extra_params),
                               fxn_in)
           ) %>%
    select(-roche_extra_params)

head(sdtmchecksmeta[,c("fxn_in")])
head(sdtmchecksmeta[,c("fxn_in_roche")])



# PRELIM:
mylist=lapply(myvec,function(x) paste0('exists("', trimws(x), '")'))

#======== exist_string ==========
sdtmchecksmeta$exist_string=unlist(lapply(mylist,function(x) paste0(x,collapse=" & ")))
head(sdtmchecksmeta$exist_string)

## -------   Drop temp variables -----------------------------------------------

# drop the gDoc column (later can remove from the input spreadsheet)
#sdtmchecksmeta = sdtmchecksmeta[,!grepl("^gdoc_pdf_return",names(sdtmchecksmeta))]
sdtmchecksmeta = sdtmchecksmeta %>%
    select(-domains_n, -domains2, -death, -coding, -entry, -P21)




## -----------------------------------------------------------------------------
## -------   Run checks on this metadata ---------------------------------------

# Check for Excel tab label greater than 31 characters
meta <- data.frame(check = sdtmchecksmeta$check,
                   xls_title = sdtmchecksmeta$xls_title,
                   length = apply(sdtmchecksmeta, 2, nchar)[,"xls_title"])
mydf <- meta %>%
    filter(meta$length > 31)

if(nrow(mydf)>0){
    print(mydf)
    stop("Excel tab label greater than 31 characters")
    }


# Check for missing domain information
mydf_xx <- sdtmchecksmeta %>%
    filter(sdtmchecksmeta$domains =="")
if(nrow(mydf_xx)>0){
    print(mydf_xx)
    stop("Domains information is missing")
}



mydf_pdf <- sdtmchecksmeta %>%
    filter(sdtmchecksmeta$pdf_return =="")
if(nrow(mydf_pdf)>0){
    print(mydf_pdf)
    stop("PDF return message is missing")
}


# Compare with check functions in R folder
file_list <- gsub(".R", "", list.files(path = "R"))

checks <- subset(file_list, grepl("check_", file_list))

# Any checks in package aren't in the spreadsheet"
checksall_p <- checks[!(checks %in% sdtmchecksmeta$check)] #in package, not in sheet
if(length(checksall_p)>0){stop("There are checks in package that aren't in the spreadsheet")}


#Any checks in the spreadsheet aren't in the package

#checksall_s <- sdtmchecksmeta$check[!(sdtmchecksmeta$check %in% checks)] #in sheet, not in package
#if(length(checksall_s)>0){stop("There are checks in spreadsheet that aren't in the package")}



#Spreadsheet has duplicate Excel tab labels

mydf <- data.frame(check = sdtmchecksmeta$check,
                   xls_title = sdtmchecksmeta$xls_title)  %>%
    count(xls_title) %>%
    filter(n>1)

if(nrow(mydf)>0){stop("Spreadsheet has duplicate Excel tab labels")}

## --- compare the update and existing df ---

#install.packages("diffdf", repos =  c(CRAN = 'https://cloud.r-project.org'))
#library(diffdf)


## name the newly created version "new"
new <- sdtmchecksmeta

# bring in the sdtmchecksmeta dataset already incorporated in the package
load_prior <- function() {
    load("data/sdtmchecksmeta.RData")
    existing_df <- sdtmchecksmeta
    ## remove the old version that is part of the package
    rm(sdtmchecksmeta)
    return(existing_df)
}

## rename sdtmchecksmeta as "prior"
prior <- load_prior()

# check number of records
nrow(new)
nrow(prior)

## confirm that only the expected changes are presented as differences
# diffdf::diffdf(new, prior)


## name the new dataset the expected package dataframe name prior to saving
sdtmchecksmeta <- new


## -----------------------------------------------------------------------------
## -------   OUTPUT: Save sdtmchecksmeta ---------------------------------------
save(sdtmchecksmeta, file = "data/sdtmchecksmeta.RData", version=2, compress=TRUE)


## -----------------------------------------------------------------------------
## -------   OUTPUT: Save nickname.RData ---------------------------------------
### Keep a nickname

nickname="CRAN Try"
save(nickname, file = "data/nickname.RData", version=2)
