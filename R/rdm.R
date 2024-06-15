#' @title Create sample data
#' @description Creates sample data with construct-data
#' 
#' @param n number of datasets
#' @param a see details
#' @param v see details
#' @param zr see details
#' @param t0 see details
#' @param d see details
#' @param szr see details
#' @param sv see details
#' @param st0 see details
#' @param N see details
#' @param data_path see details
#' @param remoteTempFiles see details
#' @return data.frame, sample set
#' @export
rdm <-
function(n, a=1, zr=0.5, v=1, t0=0.25, d=0, szr=0, sv=0, st0=0, N=1, data_path = "", removeTempFiles=T){
  if(a <= 0 | zr >= 1 | zr <= 0 | szr < 0 | sv < 0 | st0 < 0){
    stop("invalid parameter manifestations")
  }else if(N <= 0 | n <= 0) stop("number of replicates and samples must be >0")
  wd_fastdm = paste0(find.package("fastRdm"),"/fastdm_30_2")
  if(data_path=="") {
    data_path = wd_fastdm
  } else {
    dir.create(data_path)
  }    
  wd_temp <- getwd(); setwd(data_path) #change working directory to fast-dm
  call_args <- paste("-a",a,"-z",zr,"-v",v,"-t",t0,"-d",d,"-Z",szr,"-V",sv,"-T",st0,"-r","-n",n,"-N",N)
  system(paste(paste0(wd_fastdm,"/",get_os_binary("construct-samples")),call_args,'-o "%d.rdm"'), show.output.on.console=FALSE)
  if(N == 1){
    out <- read.table("0.rdm", header = FALSE)
    names(out) <- c("RESPONSE","TIME")
  }else{
    out <- list()
    for(i in list.files(pattern=".rdm")){
      out[[i]] <- read.table(i, header = FALSE)
      names(out[[i]]) <- c("RESPONSE","TIME")
    }
  }
  if(removeTempFiles){
    if(sum(file.remove(list.files(pattern = ".rdm"))) > 0) message("data generation successfully completed")
  }
  setwd(wd_temp)
  return(out)
}
