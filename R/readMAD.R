#' @include MADproject.R
NULL

#' Read the SQLite databases from MAD# into the MADproject object.
#'
#' \code{readMAD} returns an updated MADproject object with data from
#' the MAD# databases.
#'
#' @param proj The MADproject object with the slots \code{madname},
#'   \code{resultname}, and \code{xpath} specified.
#' @return An updated MADproject object with slots \code{numTimesteps},
#'    \code{numLocations}, \code{numSamples}, \code{observations},
#'    and \code{realizations} filled in from the MAD# databases.
setGeneric("readMAD", function(proj) {
  standardGeneric("readMAD")
})

setMethod("readMAD",
          signature(proj="MADproject"),
          function(proj) {
            message("I'm gonna read files!")
            #Setup database
            database <- paste(proj@xpath,"/",proj@madname,"_",proj@resultname,".xResult",sep="")
            datasample <- paste(proj@xpath,"/",proj@resultname,"/",proj@madname,"_",proj@resultname,sep="")
            drv <- RSQLite::dbDriver("SQLite")
            con <- RSQLite::dbConnect(drv, dbname =database)
            #Read project specifics
            proj@numLocations <- as.numeric(RSQLite::dbGetQuery( con,'select count(*) from selection' ))
            proj@numTimesteps <- as.numeric(RSQLite::dbGetQuery( con,'select count(*) from selectionvalues' ))/proj@numLocations
            proj@numSamples <- length(list.files(paste0(proj@xpath,proj@resultname)))
            #Read observations
            sqlvector=paste("select sv.value from  selectionvalues sv, likelihoodselecgroup s where sv.idselectionvalues= s.idselectionvalues   and s.idlikegroup=",1," order by sv.idselectionvalues;",sep='');
            res<- RSQLite::dbSendQuery(con,sqlvector)
            vector <- RSQLite::fetch(res, n=-1)
            Observationvector <- vector$value
            RSQLite::dbClearResult(res)
            proj@observations <- matrix(Observationvector,nrow=proj@numTimesteps,
                                        ncol=proj@numLocations, byrow=FALSE)
            #Read realizations
            proj@realizations <- vector("list",proj@numSamples)
            for(sample in 1:proj@numSamples){
              dbs=paste(datasample,sample,".xdata",sep='');
              if (file.exists(dbs)){
                consa <- RSQLite::dbConnect(drv, dbname =dbs)

                sql1=paste("select sv.idselectionvalues from  likelihoodselecgroup sv where  sv.idlikegroup=",1," order by sv.idselectionvalues",sep='');
                res<- RSQLite::dbSendQuery(con,sql1)
                zvectorid <- RSQLite::fetch(res, n=-1)
                zvector=zvectorid$idselectionvalues
                RSQLite::dbClearResult(res)
                counter=1
                sqlverify=paste("select count(*) as numrealizations from resultselection r, resultid ri where r.idresult=ri.idresult and ri.sample=",sample," and r.idselectionvalues=",zvector[1],";",sep='');
                numrea<- RSQLite::dbGetQuery(consa,sqlverify)[[1]]

                proj@realizations[[sample]] <-  array(0,c(numrea,length(zvector)))

                for(i in zvector){
                  sql2=paste("select r.value  from resultselection r, resultid ri where r.idresult=ri.idresult and ri.sample=",sample," and r.idselectionvalues=",i,"  order by ri.realization limit ", numrea,";",sep='');
                  res<- RSQLite::dbSendQuery(consa,sql2);
                  realizations<- RSQLite::fetch(res, n=-1);
                  value= realizations$value;
                  proj@realizations[[sample]][,counter]=value;
                  counter=counter+1;
                  RSQLite::dbClearResult(res);
                }
                RSQLite::dbDisconnect(consa);
              }
            }

            return(proj)
          }
          )

