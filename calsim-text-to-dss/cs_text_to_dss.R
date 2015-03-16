
#dvar_in_dir = 'CStextfiles/CalSim_data_baseline_090513/dvar'
#svar_in_dir = 'CStextfiles/CalSim_data_baseline_090513/svar'
#lp_in_dir = 'CStextfiles/CalSim_data_baseline_090513/cplexlp'
## Path to the new init file
#output_dss = 'baseline.dss'
#version = 'baseline_090513'

dvar_in_dir = 'CStextfiles/CalSim_data_preRPA_run090513/dvar'
svar_in_dir = 'CStextfiles/CalSim_data_preRPA_run090513/svar'
lp_in_dir = 'CStextfiles/CalSim_data_preRPA_run090513/cplexlp'
# Path to the new init file
output_dss = 'prerpa.dss'
version = 'preRPA_run090513'


#####################################
#
# Dont change anything below here
#
#####################################
#library(devtools)
#install_github('DSS-Rip','eheisman')
library(dssrip)
library(lubridate)
library(tools)
library(parallel)

source('cs_text_to_dss_lib.R')

if(file.exists(output_dss))unlink(output_dss)
dss_file = opendss(output_dss)

cores = detectCores()
cl = makeCluster(cores)

dvar_files = list.files(dvar_in_dir, full.names=TRUE)
svar_files = list.files(svar_in_dir, full.names=TRUE)
lp_files = list.files(lp_in_dir, full.names=TRUE)

file_list = list(lp_files, dvar_files, svar_files)
file_kinds = c('lp', 'dvar', 'svar')

# do it in two chunks because the data is too big to fit in memory
for(k in 1:3){
    files = file_list[[k]]
    kind = file_kinds[k]

    parts = file_name_parts_to_dt(files,c('year','month','cycle'))
    parts[,year:=as.integer(year)]
    parts[,month:=as.integer(month)]
    # its calsim so force the correct timezone
    parts[,date:=ymd(sprintf('%s-%s-01',year,month), tz='GMT')+
        days(days_in_month(month))+
        days(ifelse(leap_year(parts$year) & month==2, 1, 0))]
    parts[,date:=round_date(date,'day')]

    #full_times = ymd(sprintf('%04d-%02d-01',rep(start_init_year:end_init_year,each=12),1:12))

    read_fun = switch(kind, 
        dvar=calsim_text_to_dt, 
        svar=calsim_text_to_dt,
        lp=calsim_cplex_text_to_dt
        )

    message('Reading data.')
    cs_output_list = clusterMap(cl,read_fun, parts$filename, parts$date, parts$cycle,SIMPLIFY = FALSE)
    cs_output = rbindlist(cs_output_list)

    vars = unique(cs_output$variable)

    message('Writing data.')
    pb = txtProgressBar(1,length(vars),style=3)
    for(i in 1:length(vars)){
        setTxtProgressBar(pb,i)

        variable_ = vars[i]
        var_dt = cs_output[variable==variable_]
        cycles = sort(unique(as.character(var_dt$cycle)))


        for(j in 1:length(cycles)){
            cycle_ = cycles[j]

            metadata = data.frame(
                #fullName         : Factor w/ 1 level "/CALSIM/A18/SURFACE-AREA/01JAN1920/1MON/2020D09E/": 1
                #interval         : int 43200
                location = sprintf('%s%s', variable_, ifelse(kind == 'lp', '_wt','')),
                parameter = cycle_,
                version = version,
                watershed = "CALSIM"
                )

            dt = var_dt[cycle==cycle_]
            times = dt$date
            values = dt$value
            n_init_values = nrow(dt)

            #browser()
            tsc = monthly_data_to_tsc(values, times, metadata)
            sink('junk.txt')
            dss_file$put(tsc)
            sink(NULL)
            #browser()
        }
    }
    close(pb)
    rm(cs_output_list)
    rm(cs_output)
}



dss_file$close()