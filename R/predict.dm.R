predict.dm <-
function(type="pdf", output="preds", a=1, zr=0.5, v=1, t0=0.25, d=0, szr=0, sv=0, st0=0, data_path = "", returnPreds=T){
  wd_fastdm = paste0(find.package("fastRdm"),"/fastdm_30_2")
  if(data_path=="") {
    data_path = wd_fastdm
  } else {
    dir.create(data_path)
  }  
  if(a <= 0 | zr >= 1 | zr <= 0 | szr < 0 | sv < 0 | st0 < 0) stop("invalid parameter manifestations")
  wd_temp <- getwd(); setwd(data_path) #change working directory to fast-dm
  call_args <- paste("-a",a,"-z",zr,"-v",v,"-d",d,"-Z",szr,"-V",sv,"-T",st0)
  switch(type,
         cdf = system(paste(paste0(wd_fastdm,"/",get_os_binary("plot-cdf")),call_args,'-o "cdf.lst"'), show.output.on.console=FALSE),
         pdf = system(paste(paste0(wd_fastdm,"/",get_os_binary("plot-density")),call_args,'-o "pdf.lst"'), show.output.on.console=FALSE))
  out <- read.table(list.files(pattern = ".lst"), header = FALSE)
  if(sum(file.remove(list.files(pattern = ".lst"))) > 0) message(paste("generating",type,"of first-passage time distribution"))
  setwd(wd_temp)
  
  plot.dm <- function(out, type, xlab="response time (sec)", lwd=1){
    if(type == "pdf"){
      with(out, plot(range(V1),range(c(V2,V3)), type="n", xlab=xlab, ylab="density"))
      abline(h = 0, lty=2, col="lightgrey", lwd=lwd)  
      with(out, lines(V1, V2, lwd=lwd))
      with(out, lines(V1, V3, lty=2, lwd=lwd))
    }else if(type == "cdf"){
      with(out, plot(V1,V2, type="l", xlab=xlab, ylab="incidence probability", lwd=lwd))
      abline(v = 0, lty=2, col="lightgrey", lwd=lwd) 
    }
  }
  
  switch(output, preds = return(out), plot = plot.dm(out, type))
}
