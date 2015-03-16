calsim_cplex_text_to_dt <- function(fn, date_, cycle_){
    require(data.table)
    require(stringr)

    lines = readLines(fn)
    start = grep('Maximize',lines) + 1
    end = grep('constraint',lines) - 2

    keep_lines = lines[start:end]
    clean = gsub('\\+','',keep_lines)

    dt = data.table(read.table(textConnection(clean)))
    setnames(dt,c('value','variable'))
    dt[,variable:=str_trim(variable)]
    dt[,date:=date_]
    dt[,cycle:=cycle_]
    dt
}

calsim_text_to_dt <- function(fn, date_, cycle_){
    require(data.table)
    require(stringr)

    dt = data.table(read.table(fn,sep=':',comment.char = '/',stringsAsFactors=FALSE))
    setnames(dt,c('variable','value'))
    dt[,variable:=str_trim(variable)]
    dt[,value:= as.numeric(gsub(",","", value))]
    dt[,date:=date_]
    dt[,cycle:=cycle_]
    dt
}

file_name_parts_to_dt <- function(files, names=NULL, name_in_col=TRUE){
  require(tools)
  require(data.table)

  parts_list = strsplit(file_path_sans_ext(basename(files)),'_')
  dt = rbindlist(lapply(parts_list,function(x)data.frame(t(as.matrix(x)), stringsAsFactors=FALSE)))
  if(!is.null(names)) setnames(dt,names)
  if(name_in_col) dt[,filename:=files]
  dt
}

# Determine if range of vector is FP 0.
zero_range <- function(x, tol = .Machine$double.eps ^ 0.5) {
    if (length(x) == 1) return(TRUE)
    x <- range(x) / mean(x)
    isTRUE(all.equal(x[1], x[2], tolerance = tol))
}

concatSinglePath <- function(row){
    paste('',paste(row[1:6],collapse='/'),'',sep='/')
}

concatPathParts <- function(parts){
    apply(parts,1,concatSinglePath)
}

get_wildcard_ts <- function(path,dss){
    require(dssrip)
    getFullTSC(dss, fullPathByWildcard(paths))
}

posixct_to_dss_time <- function(x){
    as.integer(x)/60 + 2209075200/60
}

dss_time_to_posixct <- function(x){
    as.POSIXct(x*60, origin="1899-12-31 00:00", tz="UTC")
}


monthly_data_to_tsc <- function(values, times, dssMetadata=NULL, ...){

  library(lubridate)
  
  # this next bit is convouted, but the goal is to make a list of 
  # dates that spans from the first date in the series to the last 
  # date in the series with no missing values in between. The reason 
  # things are so complicated is that months are not a fixed length
  # The other reason is that dss reports the end time for a monthly
  # time step as the first day of the following month...

  first = year(times) == year(min(times))
  last = year(times) == year(max(times))
  first_year = rep(year(min(times)), abs(month(min(times)) - 12)+1)
  middle_years = rep(unique(year(times[!first & !last])), each=12)
  last_year = rep(year(max(times)), month(max(times)))
  years = c(first_year, middle_years, last_year)
  mons = (month(min(times))+0:(length(years)-1)) %% 12
  mons = ifelse(mons==0,12,mons)
  fullTimes = round_date(ymd(sprintf('%04d-%02d-01',years,mons)),'day')

  #blankTimes = fullTimes[!(fullTimes %in% times)]
  #empties = xts(rep(J("hec/script/Constants")$UNDEFINED, length(blankTimes)), order.by=blankTimes)
  #colnames(empties) = colnames(tsObject)
  #tsObject = rbind(tsObject, empties)
  df = merge(data.frame(times=fullTimes),data.frame(times=times,values=values),all.x=TRUE)
  
  ## Configure slots for TimeSeriesContainer object
  times_java = posixct_to_dss_time(fullTimes)
  values = as.numeric(df$values)
  #browser()
  metadata = list(
    times = .jarray(as.integer(times_java), contents.class="java/lang/Integer"), #, as.integer(times)), new.class="java/lang/Integer")
    values = .jarray(values, contents.class="java/lang/Double"),
    endTime = max(times_java),
    startTime = min(times_java),
    numberValues = length(values),
    storedAsdoubles = TRUE,
    modified=FALSE,
    fileName="",
    ...
  )

  if(!is.null(dssMetadata)){
    for(mdName in colnames(dssMetadata)){
      if(mdName %in% names(metadata)){
        next
      }
      metadata[[mdName]] = first(dssMetadata[[mdName]])
    }
  }
  #browser()
  ePart = "1MON"
  dPart = paste0("01JAN", lubridate::year(times[1]))
  metadata$fullName = paste("", metadata$watershed, metadata$location, metadata$parameter, dPart, ePart, metadata$version, "", sep="/")
  tsc = .jnew("hec/io/TimeSeriesContainer")
  tscFieldsDF = get("tscFieldsDF", envir=dssrip:::hecJavaObjectsDB)
  for(n in names(metadata)){
    #print(sprintf("%s:", n))
    #print(metadata[[n]])
    writeVal = metadata[[n]]
    if(typeof(writeVal)!='S4'){
      if(is.na(writeVal) | writeVal == ""){
        #print("Value is NA, not writing.")
        next
      }
    }
    if(is.factor(writeVal)){
      writeVal = as.character(writeVal)
    }
    if(tscFieldsDF$CLASS[tscFieldsDF$SHORTNAME == n] %in% c("int")){
      #print("Converting to integer.")
      writeVal = as.integer(writeVal)
    }
    .jfield(tsc, n) = writeVal
  }
  return(tsc)
}

get_pa_variable_dt <- function(variable, dss, pa_year, start_year, end_year, start_month, run_length=1, year_labels=TRUE){

    require(dssrip)
    require(dplyr)
    require(data.table)

    end_month = ifelse(start_month == 1, 12, start_month-1)
    start_date = ymd(sprintf('%d-%02d-01',start_year,start_month))

    # does the model run end in the following calendar year or not
    year_modifier = ifelse(start_month == 1, run_length-1, run_length)
    init_month = end_month

    # Range of dates for the first model run period
    first_date_range = seq(start_date, start_date + months(12*run_length) - months(1), by='months')

    # the starting years of each model run
    run_start_year = seq(start_year, end_year, by=run_length)

    # open the DSS file and fiddle with the paths to allow bulk extraction of a 
    # single variable. Internally, DSS stores monthly data in 10 year chunks (why?!?!)
    paths = getAllPaths(dss)
    parts = separatePathParts(paths)
    parts$D = '*'
    wildcard_paths = unique(concatPathParts(parts))

    wildcard_path = grep(sprintf('/%s/',variable),wildcard_paths,value=TRUE)
    path_group = fullPathByWildcard(paths, wildcard_path)

    # get a single variable as a data.table
    dt = getFullDT(dss,path_group)

    # dss reports the end of month timestamp as the last day of the month timestamp with 
    # hour 24:00, which is not actually a real time (damn you dss), and so gets converted to 
    # the first day of the next month with an hour of 00:00 by R, so move back a month so that 
    # now all the timestamps are at them beginning of each month
    dt[,datetime:=datetime-months(1)]
    dt$group=NA_real_

    # create a new data colum equal to the year the run was started, 
    # this is used to group the data, hence the name
    for(i in 1:length(run_start_year))
        dt[datetime %in% (first_date_range+years(i-1)),group:=run_start_year[i]]
    dt = na.omit(dt)

    # the init value for all the runs is on the start date because of the shifted month,
    # it's convoluted I know
    init_value = dt[datetime == start_date]$value

    # year_mod is the number of years each run should be shifted so that they can all be 
    # plotted on the same time window
    year_mod = summarise(group_by(dt,group),modifier=min(pa_year-year(datetime)))
    dt = merge(dt,year_mod,by='group')

    # create the new dates with the year shift, used for plotting
    dt[,pa_date := datetime+years(modifier)]

    # add in the data for the init point
    init_dt = data.table(datetime=ymd(sprintf('%d-%02d-01',start_year:end_year-year_modifier,init_month)),
        pa_date=ymd(sprintf('%d-%02d-01',pa_year-year_modifier,init_month)),
        value=init_value, units=dt$units[1], group=start_year:end_year, modifier=0)
    dt = rbind(dt,init_dt,use.names=TRUE)

    return(dt)
}