#' @title Check for duplicate EX records
#'
#' @description This check looks for duplicate treatment records in EX
#'
#' @param EX Exposure SDTM dataset with variables USUBJID, EXTRT, EXDOSE, EXSTDTC, EXSTDTC.
#'   VISIT is optional.
#'
#' @return boolean value if check failed or passed with 'msg' attribute if the
#'   test failed
#'
#' @export
#'
#' @author Fang Yuan
#'
#' @examples
#'
#' EX <- data.frame(
#'  USUBJID = rep(1,2),
#'  EXTRT = rep(1,2),
#'  EXDOSE = rep(1,2),
#'  EXSTDTC = rep(1,2),
#'  EXOCCUR = "Y"
#' )
#' check_ex_dup(EX)
#'
#' EX$EXOCCUR <- NULL
#'
#' check_ex_dup(EX)
#'
#' EX$EXDOSE <- NULL
#'
#' check_ex_dup(EX)
#'
#' # test with sample data without duplicates
#'
#' EX <- data.frame(
#' USUBJID = 1:2,
#' EXTRT = 1:2,
#' EXDOSE = 1:2,
#' EXSTDTC = 1:2,
#' EXOCCUR = "Y"
#' )
#'
#' check_ex_dup(EX)
#'
#' EX = rbind(EX,EX)
#'
#' check_ex_dup(EX)
#'
#' # check non existing vars
#'
#' EX$EXTRT <- NULL
#' EX$EXOCCUR <- NULL
#'
#' check_ex_dup(EX)
#'
#'

## Check for duplicate dosing (EX)

check_ex_dup <- function(EX){

  ###First bifurcate EX into a df based on occurrence of EXOCCUR
  if ("EXOCCUR" %in% names(EX)) {
    EX <- EX %>% filter(EXOCCUR == "Y")
  }

  ###Check that required variables exist and return a message if they don't
  if(EX %lacks_any% c("USUBJID", "EXTRT", "EXDOSE", "EXSTDTC")){

      fail(lacks_msg(EX, c("USUBJID", "EXTRT", "EXDOSE", "EXSTDTC")))

  } else{

    # leave only variables on which we want to check duplicate dosing
    ex0 <- subset(EX, select = c("USUBJID", "EXTRT", "EXDOSE", "EXSTDTC"))

    # sort
    ex1 <- with(ex0, ex0[order(USUBJID, EXTRT, EXDOSE, EXSTDTC),])

    # check if there are duplicate dosing
    dups <- subset(ex1,duplicated(ex1),c("USUBJID", "EXTRT", "EXDOSE", "EXSTDTC"))
    rownames(dups)=NULL

    n0 <- ''; n1 <- '';
    # declare number of duplicated dosing and print them
    n0 <- paste('There are ', nrow(dups), ' duplicated exposure records. ', sep ='')

    if(nrow(dups)>0){
      ## check that the visit variable is in the dataset
      if(EX %lacks_any% "VISIT"){
        #print this list without visit information appended if there is no visit variable
          fail(
              msg = n0,
              data = dups
          )
      } else{

        # merge this back to get the visit to print in the duplicate list
        ex2 <- subset(EX, select = c("USUBJID","EXTRT","EXDOSE","EXSTDTC","VISIT"))
        # sort
        ex2s <- with(ex2, ex2[order(USUBJID, EXTRT, EXDOSE, EXSTDTC, VISIT),])

        # left join to merge on VISIT
        dupswithvisit <- merge(x=dups, y=ex2s, by=c("USUBJID","EXTRT","EXDOSE","EXSTDTC"), all.x=TRUE)
        dupswithvisit = unique(dupswithvisit)
        #print this list with visit appended
        fail(
            msg = n0,
            data = dupswithvisit
        )
      }

    } else if(nrow(dups)==0){
        pass()
    }
  }
}


