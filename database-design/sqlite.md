This document describes a design for a general database schema for holding model output. Overall, each variable is held in a separate table with a few metadata tables.

# variables table
This table contains the names of variables and their descriptions

|column name | description|
|-----------|------------|
|var_name	|Display Name (unique key)|
|units|	Units of measurement such as: feet,cfs, or acre-feet|
|time_interval| month, week, day (may not be necessary)	|
|description|	Description for data such as: daily average flow|
|table_name|	Unique database table name for this Series/row|
|notes|	User notes|

#variable data tables (table name is variable name)
This set of tables contains the actual data and flags for individual points

|column name | description|
|-----------|------------|
|datetime	| date and time|
|value | data value |
|flag | flag value, such as a quality flag |

# flags table
This table contains data flags and their descriptions

|column name | description|
|-----------|------------|
|flag	| flag name|
|description | description of data flag |


# attributes table
This table contains attribute values for each variable

|column name | description|
|-----------|------------|
|var_name	|Display Name and name for equations referencing this Series/row|
|attribute_name|	id from attribute description table|
|value|	attribute value, problem of character/numeric data|


# attribute_descriptions table
This table contains descriptions of attributes

|column name | description|
|-----------|------------|
|attribute_name|	unique key: time_interval, lat, long, etc.|
|description|	Description for attribute |
|notes|	User notes

Example pulling attributes for the variable `S_MELON_C1`:

```sql
select * from attributes
where var_name='S_MELON_C1';
```