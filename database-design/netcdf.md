This document contains a general description of using NetCDF to hold general model output

- Each model variable set up as a "var" in NetCDF
    - Can contain time series or gridded output
    - Timezone specified as GMT
    - For CALSIM each cycle in separate variable for compatibility with other models
    - All values specified by datetime, no need to consider time step length
- All metadata contained in the variable attribute section 
    - Decide on naming convention
        - Lat/lat/Latitude, etc.
- Access instructions will depend on the NetCDF interface used.
    - e.g. from R: `ncdf_getvar('S_MELON_C1')`