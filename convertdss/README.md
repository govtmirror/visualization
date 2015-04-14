devtools::install_github("eheisman/DSS-Rip",args="--no-multiarch")

```
dss = opendss('my_dss.dss')
    # read out one variable to get the time range
v = read_dss_variable('MY_VAR',dss)
    # initilize the netcdf file
nc = dss_to_ncdf_init(dss,v$data$datetime)
dss_to_netcdf(dss,nc)
```