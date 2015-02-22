corr <- function(directory, threshold = 0) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files        
        ## 'threshold' is a numeric vector of length 1 indicating the
        ## number of completely observed observations (on all
        ## variables) required to compute the correlation between
        ## nitrate and sulfate; the default is 0        
        ## Return a numeric vector of correlations
        
        path<-"C:\\Users\\carmen.fornier\\Documents\\Rworkingdirectory\\"        
        completef<-complete(directory,1:332)##evaluate ids
        idOK<-completef["nobs"]>threshold ##is with condition ok
        vIdOk<-completef[idOK,][["id"]] ## num complete cases
        idCount<-length(vIdOk)##get id count
        vNitrate<-vector("numeric")
        vsulfate<-vector("numeric")
        vAuxn<-vector("numeric")
        vAuxs<-vector("numeric")
        vres<-vector("numeric")
        if(idCount>0)
        {
                for (i in 1:idCount) 
                {
                        if(vIdOk[i]<10)
                                filename<-paste("00",as.character(vIdOk[i]),sep='')
                        else if(vIdOk[i]>=10 && vIdOk[i]<100)
                                filename<-paste("0",as.character(vIdOk[i]),sep='')
                        else
                                filename<-as.character(vIdOk[i])
                        fid <- read.csv(file=paste(path,directory,"\\",filename,".csv",sep=''),header=TRUE) ##load frame to read
                        
                        vOK<-complete.cases(fid) ##vector with ok indexes
                        fidok<-fid[vOK,]##frame with complete cases
                        
                        vAuxn<-fidok["nitrate"]##just nitrate
                        ##vNitrate<-c(vNitrate,vAux[,1])##append to vector
                        
                        vAuxs<-fidok["sulfate"]#just sulfate
                        ##vsulfate<-c(vsulfate,vAux[,1])##append to vector  
                        vres<-c(vres,cor(vAuxn,vAuxs))
                        
                }
        }
        ##cor(vNitrate,vsulfate)
        vres
}