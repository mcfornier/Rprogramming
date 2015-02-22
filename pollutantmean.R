pollutantmean <- function(directory, pollutant, id = 1:332) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files
        
        ## 'pollutant' is a character vector of length 1 indicating
        ## the name of the pollutant for which we will calculate the
        ## mean; either "sulfate" or "nitrate".
        
        ## 'id' is an integer vector indicating the monitor ID numbers
        ## to be used
        
        ## Return the mean of the pollutant across all monitors list
        ## in the 'id' vector (ignoring NA values)
        
        idcount<-length(id) ##get id count
        path<-"C:\\Users\\carmen.fornier\\Documents\\Rworkingdirectory\\"
        vIds<-vector("numeric")
        for (i in 1:idcount)
        {		
                if(id[i]<10)
                        filename<-paste("00",as.character(id[i]),sep='')
                else if(id[i]>=10 && id[i]<100)
                        filename<-paste("0",as.character(id[i]),sep='')
                else
                        filename<-as.character(id[i])
                fid <- read.csv(file=paste(path,directory,"\\",filename,".csv",sep=''),header=TRUE) ##load frame to read
                
                vpollutant<-fid[pollutant]	
                ####vIdAux<-vpollutant[!is.na(vpollutant)]	##full pollutant vector		
                ####vIds<-c(vIds,vIdAux) ##append all  values across all selected ids
                vIds<-c(vIds,vpollutant[[1]])
        }
        mean(vIds,na.rm=TRUE)##mean of values, removing NA
}