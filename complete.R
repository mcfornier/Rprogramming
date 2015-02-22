complete <- function(directory, id = 1:332) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files
        ## 'id' is an integer vector indicating the monitor ID numbers
        ## to be used        
        ## Return a data frame of the form:
        ## id nobs
        ## 1  117
        ## 2  1041
        ## ...
        ## where 'id' is the monitor ID number and 'nobs' is the
        ## number of complete cases		
		idcount<-length(id) ##get id count	
		path<-"C:\\Users\\carmen.fornier\\Documents\\Rworkingdirectory\\"
		vid<-vector("numeric")	
		vnobs<-vector("numeric")			
		for (i in 1:idcount)##every id file
		{
			if(id[i]<10)
					filename<-paste("00",as.character(id[i]),sep='')
			else if(id[i]>=10 && id[i]<100)
					filename<-paste("0",as.character(id[i]),sep='')
				else
						filename<-as.character(id[i])
            fid <- read.csv(file=paste(path,directory,"\\",filename,".csv",sep=''),header=TRUE) ##load frame to read
			
			vOK<-complete.cases(fid) ##index
			num<-length(vOK[vOK==TRUE]) ## num complete cases
			vid<-c(vid,i)
			vnobs<-c(vnobs,num)
		}
		data.frame(id=vid,nobs=vnobs) ##results frame
}
