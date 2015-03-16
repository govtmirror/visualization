
## SeriesCatalog Table

The SeriesCatalog table is a list of all Series in the database along with properties such as units, parameter, name, and the name of a separate table that stores the time Series data.

Each Series data is stored in a separate table.  A table named Î÷ÎõSeriesCatalogÎéÎ÷ is a listing of these tables and each column of the SeriesCatalog maps to properties of Series such as database table names, units of measure, and parameter name for each table. 

|column name | description|
|-----------|------------|
|id	|Primary key (integer)|
|ParentID *|	SiteDataTypeID of containing folder|
|IsFolder *|	When true this row represents a folder not a series|
|SortOrder *|	Sort order within a folder for user interface|
|iconname *|	Use to render an icon based on the source of data|
|Name	|Display Name and name for equations referencing this Series/row|
|SiteID|	Reference to site/location information|
|Units|	Units of measurement such as: feet,cfs, or acre-feet|
|TimeInterval|	One of : (Instant, Daily, Monthly)|
|Parameter|	Description for data such as: daily average flow|
|TableName|	Unique database table name for this Series/row|
|Provider|	Name of a class derived from Reclamation.TimeSeries.Series (or Series)|
|ConnectionString|	Provider specific connection information such as a path to an excel file, sheet name, or specific parameter code |
Expression|	Equation expression for computed series|
Notes|	User notes|
Enabled|	Used to active or deactive calculations and presentation of data|


![schema for pisces database](https://raw.githubusercontent.com/usbr/Pisces/master/Pisces_open/images/schema.png)


Here is the SQLite command to create seriescatalog. 

```sql
CREATE TABLE seriescatalog  (  id int not null primary key , parentid int not null default 0,  isfolder smallint not null default 0,  sortorder int not null default 0, iconname  nvarchar(100)  not null default '', name  nvarchar(200)  not null default '', siteid  nvarchar(2600)  not null default '', units  nvarchar(100)  not null default '', timeinterval  nvarchar(100)  not null default 'irregular', parameter  nvarchar(100)  not null default '', tablename  nvarchar(128)  not null default '', provider  nvarchar(200)  not null default '',  connectionstring  nvarchar(2600)  not null default '',  expression  nvarchar(2048)  not null default '',  notes  nvarchar(2048)  not null default '',  enabled smallint not null default 1  );
```

For individual series/variables the command is like this:
```sql
CREATE TABLE instant_karl_fb( datetime DateTime primary key, value float, flag  nvarchar(50)  );
INSERT INTO instant_karl_fb VALUES('2013-01-01 00:00:00',1.0,'');
INSERT INTO instant_karl_fb VALUES('2013-01-02 00:00:00',2.0,'');
INSERT INTO instant_karl_fb VALUES('2013-01-03 00:00:00',3.0,'');
INSERT INTO instant_karl_fb VALUES('2013-01-04 00:00:00',4.0,''); 
```


Or for more efficient, limited to one second in the time stamp it looks like this:


```sql
CREATE TABLE piscesinfo ( name   nvarchar(255)  not null primary key,  value  nvarchar(1024)  not null default ''  );
INSERT INTO piscesinfo VALUES('FileVersion',2);
INSERT INTO piscesinfo VALUES('LookupOption','SeriesName');
INSERT INTO piscesinfo VALUES('UnixDateTime','True');

CREATE TABLE test_series( datetime DateTime primary key, value float, flag  nvarchar(50)  );
INSERT INTO test_series VALUES(1423785600,0.0,'');
INSERT INTO test_series VALUES(1423872000,1.0,'');
INSERT INTO test_series VALUES(1423958400,2.0,'');
INSERT INTO test_series VALUES(1424044800,3.0,'');
INSERT INTO test_series VALUES(1424131200,4.0,'');
 ```

