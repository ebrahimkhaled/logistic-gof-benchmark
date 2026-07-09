suppressMessages(devtools::load_all("C:/Users/ebrah/.cursor-tutor/projects/Ebrahim Frangton", quiet=TRUE))
suppressMessages(library(ResourceSelection))
options(warn=-1)
G <- 10; NREP <- 2000; NS <- c(200,500,1000,2000,5000)
OUT <- "C:/Users/ebrah/AppData/Local/Temp/claude/C--Users-ebrah--gemini-Projects/e6ecd6bc-f9c9-4839-bd03-ee90b0e125eb/scratchpad/ef_edge_slads_2k.csv"
COLS <- c("EF","EDGE_poly2","EDGE_poly3","EDGE_stukel","HL")

gen <- function(sc, n){
  if(sc=="Uniform_-3_3"){x<-runif(n,-3,3); eta<-0.8*x; df<-data.frame(x=x); form<-y~x}
  else if(sc=="Uniform_-6_6"){x<-runif(n,-6,6); eta<-0.8*x; df<-data.frame(x=x); form<-y~x}
  else if(sc=="Normal_0_1.5"){x<-rnorm(n,0,1.5); eta<-0.8*x; df<-data.frame(x=x); form<-y~x}
  else if(sc=="Chi2_4"){x<-rchisq(n,4); eta<- -4.9+0.65*x; df<-data.frame(x=x); form<-y~x}
  else if(sc=="Multi_Indep"){x1<-runif(n,-6,6); x2<-rnorm(n,0,1.5); x3<-rchisq(n,4)
    eta<- -1.3+(0.8/3)*x1+(0.8/3)*x2+(0.65/3)*x3; df<-data.frame(x1=x1,x2=x2,x3=x3); form<-y~x1+x2+x3}
  else if(sc=="Quad_Slight"){x<-runif(n,-3,3); eta<- -1.138+1.257*x+0.035*x^2; df<-data.frame(x=x); form<-y~x}
  else if(sc=="Quad_Pronounced"){x<-runif(n,-3,3); eta<- -3.232+0.558*x+0.500*x^2; df<-data.frame(x=x); form<-y~x}
  else if(sc=="Interact_Slight"){x<-runif(n,-3,3); d<-rbinom(n,1,0.5); eta<- -1.792+0.135*x+0.270*d+0.090*x*d; df<-data.frame(x=x,d=d); form<-y~x+d}
  else if(sc=="Interact_Pronounced"){x<-runif(n,-3,3); d<-rbinom(n,1,0.5); eta<- -1.792+0.135*x+1.791*d+0.597*x*d; df<-data.frame(x=x,d=d); form<-y~x+d}
  df$y <- rbinom(n,1,plogis(eta)); list(df=df, form=form)
}
scenarios <- c("Uniform_-3_3","Uniform_-6_6","Normal_0_1.5","Chi2_4","Multi_Indep",
               "Quad_Slight","Quad_Pronounced","Interact_Slight","Interact_Pronounced")
set.seed(2025); res <- list(); t_all <- Sys.time()
for(sc in scenarios) for(n in NS){
  pv <- matrix(NA_real_, NREP, length(COLS), dimnames=list(NULL,COLS))
  for(i in 1:NREP){
    g <- gen(sc,n); m <- tryCatch(glm(g$form,data=g$df,family=binomial), error=function(e) NULL)
    if(is.null(m)) next
    ph <- fitted(m); y <- g$df$y
    pv[i,"EF"]          <- tryCatch(ef.gof(y,ph,G=G)$p_value,                    error=function(e) NA_real_)
    pv[i,"EDGE_poly2"]  <- tryCatch(def.gof(m,G=G,basis="poly2")$p_value,        error=function(e) NA_real_)
    pv[i,"EDGE_poly3"]  <- tryCatch(def.gof(m,G=G,basis="poly3")$p_value,        error=function(e) NA_real_)
    pv[i,"EDGE_stukel"] <- tryCatch(def.gof(m,G=G,basis="stukel")$p_value,       error=function(e) NA_real_)
    pv[i,"HL"]          <- tryCatch(hoslem.test(y,ph,g=G)$p.value,               error=function(e) NA_real_)
  }
  rr <- colMeans(pv < 0.05, na.rm=TRUE)
  for(t in COLS)
    res[[paste(sc,n,t)]] <- data.frame(Scenario=sc, n=n, Test=t, Power=round(rr[[t]],4), Reps=sum(!is.na(pv[,t])))
  write.csv(do.call(rbind,res), OUT, row.names=FALSE)
  cat(sprintf("%-20s n=%-5d EF=%.3f p2=%.3f p3=%.3f stk=%.3f HL=%.3f [%.1f min]\n",
      sc, n, rr[["EF"]], rr[["EDGE_poly2"]], rr[["EDGE_poly3"]], rr[["EDGE_stukel"]], rr[["HL"]],
      as.numeric(Sys.time()-t_all,units="mins")))
}
cat("=== SIMULATION COMPLETE ===\n")
