# Function for subsetting data based on POSIXct dates
# returns data frame with all rows subset by dates contained in timelimit_vec
# name of dates column needs to be "logger_time_formatted" and it needs to be in POSIXct format

#make some test data
test = data.frame(logger_time_formatted = seq(as.POSIXct('2012-01-01 00:00:00',origin = '1970-01-01 00:00:00', tz="GMT"),as.POSIXct('2012-01-02 00:00:00',origin = '1970-01-01 00:00:00', tz="GMT"), by = 60*60*2))
test$data = rnorm(n = nrow(test),mean = 10,sd = 2 )
testlist = list(test$logger_time_formatted[3], test$logger_time_formatted[12])





 subdat.fun = function(timelimit_list, dat){
      #check that any date classes are POSIXct
      # timelimit_list = testlist
      # dat = test
     ck = as.POSIXct('2012-01-01 01:00:00',origin = '1970-01-01 00:00:00', tz="GMT")

     if(!sum(class(timelimit_list[[1]]) == class(ck)) == 2){stop('timelimit_vec not POSIXct format')}
     if(!sum(class(timelimit_list[[2]]) == class(ck)) == 2){stop('timelimit_vec not POSIXct format')}
     if(length(timelimit_list) > 2){stop('timelimit_vec greater than 2')}
     if(!sum(class(dat$logger_time_formatted) == class(ck)) == 2 ){stop('dat$logger_time_formatted not POSIXct format')}
     tempdat = dat[dat$logger_time_formatted > timelimit_list[[1]] & dat$logger_time_formatted < timelimit_list[[2]],]
     tempdat
}




 #test it out
subdat.fun(timelimit_list = testlist, dat = test)
