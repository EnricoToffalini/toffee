
##########################################

#' Polar json to dataframe
#' @description Takes a Polar .json file as input and returns a dataframe as output. The following four types of Polar data files are accepted as input: 1) heart rate (files starting with "247ohr-..."); 2) activity (files starting with "activity-..."); 3) temperature (files starting with "generic-period-..."); 4) PPI (files starting with "ppi_samples_...")
#' @param file a string indicating the path to a Polar .json file
#' @return a dataframe with all relevant data processed
#' @export
Polar_json2dataframe = function(file=NA){
  
  if (!requireNamespace("jsonlite", quietly = TRUE)) stop("The 'jsonlite' package is required but not installed. Please install it using install.packages('jsonlite').")
  if (!requireNamespace("tools", quietly = TRUE)) stop("The 'tools' is required but not installed. Please install it using install.packages('tools').")
  
  require(tools)
  require(jsonlite)
  
  if(!file.exists(file)) stop("file not found! check the working directory path or possible misspellings")
  ext = tools::file_ext(file)
  
  notamongError = "file type is not among the four .json Polar data output file types supported by this function; supported types are: 1) heart rate (files starting with '247ohr-...'); 2) activity (files starting with 'activity-...'); 3) temperature (files starting with 'generic-period-...'); 4) PPI (files starting with 'ppi_samples_...')"
  if(ext != "json") stop(notamongError)
  
  js = fromJSON(file)
  type = NA
  df = data.frame()
  
  # determine type
  if( !is.null(js$deviceDays$samples) ){
    type = "247ohr"
  }else if( !is.null(js$samples$mets$value) ){
    type = "activity"
  }else if( !is.null(js$data$temperatureMeasurementSamples$temperatureCelsius) ){
    type = "generic period"
  }else if( !is.null(js$devicePpiSamplesList) ){
    type = "ppi"
  }
  
  if(is.na(type)) stop(notamongError)
  
  # extract and format data
  if(type == "247ohr"){
    dates = js$deviceDays$date
    
    for(i in 1:length(dates)){
      day = dates[i]
      sec = js$deviceDays$samples[[i]]$secondsFromDayStart
      TimeFull = as.POSIXct(sec, origin=day, tz = "UTC")
      time = format(TimeFull, "%H:%M:%S")
      if(!is.null(sec)){
        x = data.frame(deviceID=js$deviceDays$deviceId[[i]],
                       day=day,
                       TimeFull=TimeFull,
                       time=time,
                       heartRate=js$deviceDays$samples[[i]]$heartRate)
        df = rbind(df,x)
      }
    }
  }else if(type == "activity"){
    day = js$date
    sec = NA
    TimeFull = NA
    time = NA
    mov = js$samples$mets$value
    stps = rep(js$samples$steps$value,each=2)[1:length(mov)]
    stps[c((length(mov)-1):length(mov))] = stps[(length(mov)-2)]
    if(length(mov)>=2878){
      if(length(mov<2880)){
        mov[length(mov):2880] = mov[length(mov)]
        stps[length(stps):2880] = stps[length(stps)]
      }
      sec = ((1:2880)*30)-15
      TimeFull = as.POSIXct(sec, origin=day, tz = "UTC")
      time = format(TimeFull, "%H:%M:%S")
    }
    df = data.frame(deviceID=js$samples$metSources,
                    day=day,
                    TimeFull=TimeFull,
                    time=time,
                    movement=mov,
                    steps=stps)
  }else if(type == "generic period"){
    tmps = js$data$temperatureMeasurementSamples$temperatureCelsius
    day = format(as.POSIXct(js$meta$created), format="%Y-%m-%d")
    sec = as.numeric(js$data$temperatureMeasurementSamples$recordingTimeDeltaMilliseconds)/1000
    sec[1] = 0
    TimeFull = as.POSIXct(sec, origin=day, tz = "UTC")
    time = format(TimeFull, "%H:%M:%S")
    df = data.frame(deviceID=js$data$sourceDeviceId,
                    day=day,
                    TimeFull=TimeFull,
                    time=time,
                    temperature=tmps)
  }else if(type == "ppi"){
    dates = js$date
    
    df = data.frame()
    for(i in 1:length(dates)){
      TimeFull = js$devicePpiSamplesList[[i]]$ppiSamples[[1]]$sampleDateTime
      day = substr(TimeFull,1,10)
      time = format(substr(TimeFull,12,nchar(TimeFull)), format="%H:%M:%OS3")
      time = gsub(" ","",time)
      time = ifelse(nchar(time)<10,paste0(time,".000"),time)
      x = data.frame(deviceID=js$devicePpiSamplesList[[i]]$deviceId,
                     day=day,
                     TimeFull=TimeFull,
                     time=time,
                     ppi=js$devicePpiSamplesList[[i]]$ppiSamples[[1]]$pulseLength)
      df = rbind(df,x)
    }
  }
  
  # return
  return(df)
}

##########################################

