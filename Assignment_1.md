Chicago Narcotics
================
Edward Huh
2018-10-15

Setting up the environment for analysis
=======================================

scraping with API
=================

We will be working with crime data in 2016

``` r
socrata.file <- "https://data.cityofchicago.org/resource/kf95-mnd6.csv"
crime.data <- read.socrata(socrata.file)
head(crime.data)
```

    ##         ID Case.Number Date               Block IUCR        Primary.Type
    ## 1 10372201    HZ108010 <NA> 071XX N MANKATO AVE 2820       OTHER OFFENSE
    ## 2 10375758    HZ111926 <NA> 076XX N SHERIDAN RD 0820               THEFT
    ## 3 10376371    HZ112605 <NA>     004XX E 48TH ST 0910 MOTOR VEHICLE THEFT
    ## 4 10378473    HZ114778 <NA>   023XX W TOUHY AVE 0486             BATTERY
    ## 5 10380488    HZ116467 <NA>  091XX S PAXTON AVE 4387       OTHER OFFENSE
    ## 6 10380619    HZ116583 <NA>  091XX S PAXTON AVE 4387       OTHER OFFENSE
    ##                   Description Location.Description Arrest Domestic Beat
    ## 1            TELEPHONE THREAT            RESIDENCE   true    false 1621
    ## 2              $500 AND UNDER             SIDEWALK   true    false 2422
    ## 3                  AUTOMOBILE               STREET   true    false  223
    ## 4     DOMESTIC BATTERY SIMPLE            APARTMENT   true    false 2411
    ## 5 VIOLATE ORDER OF PROTECTION            RESIDENCE   true     true  413
    ## 6 VIOLATE ORDER OF PROTECTION            RESIDENCE   true     true  413
    ##   District Ward Community.Area FBI.Code X.Coordinate Y.Coordinate Year
    ## 1       16   41             12       26      1135130      1947162 2016
    ## 2       24   49              1       06      1165527      1950516 2016
    ## 3        2    3             38       07      1180055      1873293 2016
    ## 4       24   49              2      08B      1159245      1947721 2016
    ## 5        4    7             48       26      1192434      1844707 2016
    ## 6        4    7             48       26      1192434      1844707 2016
    ##   Updated.On Latitude Longitude                      Location
    ## 1       <NA> 42.01120 -87.77815  (42.011203975, -87.77814883)
    ## 2       <NA> 42.01981 -87.66621 (42.019811963, -87.666209262)
    ## 3       <NA> 41.80759 -87.61513 (41.807586328, -87.615127788)
    ## 4       <NA> 42.01227 -87.68940 (42.012274439, -87.689403816)
    ## 5       <NA> 41.72885 -87.57066 (41.728851343, -87.570655525)
    ## 6       <NA> 41.72885 -87.57066 (41.728851343, -87.570655525)

The data clearly shows how there are lots of missing data. In particular, the `Date` column is done practially useless. This is perfectly fine, because the objective of this project is to create point graphs and choropleth graphs, which do not have inherent date specifications.

``` r
dim(crime.data)
```

    ## [1] 268756     22

``` r
unique(crime.data$Primary.Type)
```

    ##  [1] "OTHER OFFENSE"                    
    ##  [2] "THEFT"                            
    ##  [3] "MOTOR VEHICLE THEFT"              
    ##  [4] "BATTERY"                          
    ##  [5] "ROBBERY"                          
    ##  [6] "WEAPONS VIOLATION"                
    ##  [7] "DECEPTIVE PRACTICE"               
    ##  [8] "BURGLARY"                         
    ##  [9] "CRIMINAL TRESPASS"                
    ## [10] "ASSAULT"                          
    ## [11] "CRIMINAL DAMAGE"                  
    ## [12] "ARSON"                            
    ## [13] "NARCOTICS"                        
    ## [14] "PUBLIC PEACE VIOLATION"           
    ## [15] "PROSTITUTION"                     
    ## [16] "SEX OFFENSE"                      
    ## [17] "STALKING"                         
    ## [18] "OFFENSE INVOLVING CHILDREN"       
    ## [19] "CRIM SEXUAL ASSAULT"              
    ## [20] "INTERFERENCE WITH PUBLIC OFFICER" 
    ## [21] "KIDNAPPING"                       
    ## [22] "HOMICIDE"                         
    ## [23] "LIQUOR LAW VIOLATION"             
    ## [24] "INTIMIDATION"                     
    ## [25] "NON-CRIMINAL (SUBJECT SPECIFIED)" 
    ## [26] "CONCEALED CARRY LICENSE VIOLATION"
    ## [27] "OBSCENITY"                        
    ## [28] "GAMBLING"                         
    ## [29] "NON - CRIMINAL"                   
    ## [30] "OTHER NARCOTIC VIOLATION"         
    ## [31] "NON-CRIMINAL"                     
    ## [32] "PUBLIC INDECENCY"                 
    ## [33] "HUMAN TRAFFICKING"

So, a more effective way of reducing this dataset will be selecting a type of crime. Let's restrict this to NARCOTIC cases. Looking back, there needed to be more restriction. Thankfully, the Incidence reports are strictly increasing. So, I found the last police record of January to be 10399221. We will use this to find incidences of January of 2018

``` r
crime.data %>%
  filter(Primary.Type == "NARCOTICS") %>%
  filter(ID <= 10399221)-> narc.data
dim(narc.data)
```

    ## [1] 952  22

Now the data is in a much more reasonable size. 952 rows! But we definitely do not need all the columns

``` r
names(narc.data)
```

    ##  [1] "ID"                   "Case.Number"          "Date"                
    ##  [4] "Block"                "IUCR"                 "Primary.Type"        
    ##  [7] "Description"          "Location.Description" "Arrest"              
    ## [10] "Domestic"             "Beat"                 "District"            
    ## [13] "Ward"                 "Community.Area"       "FBI.Code"            
    ## [16] "X.Coordinate"         "Y.Coordinate"         "Year"                
    ## [19] "Updated.On"           "Latitude"             "Longitude"           
    ## [22] "Location"

For our analysis, we only really need the `Community.Area`, `Latitute`, and `Longitude`.

``` r
narc.data %>% select(comm = Community.Area, 
                     lat = Latitude, long = Longitude) -> narc.final
head(narc.final)
```

    ##   comm      lat      long
    ## 1   25 41.88019 -87.76853
    ## 2    8 41.90313 -87.63218
    ## 3   29 41.86631 -87.71376
    ## 4   26 41.88075 -87.72398
    ## 5   23 41.89807 -87.71640
    ## 6   23 41.89905 -87.72291

Creating a point layer
======================

Now, we will use the features in the `sp` package that will allow us to create a spatial points layer. We will use the `lat` and `lon` for this. But in order for this process to be successful, I must eliminate all the missing data points.

``` r
narc.coord <- narc.final %>% filter(!is.na(lat))
dim(narc.coord)
```

    ## [1] 951   3

There is one data point without coordinates

Within the `sf` package, there is the `st_as_sf` function that converts coordinates to spatial objects. So, this is the command that we will be using:

``` r
narc.points = st_as_sf(narc.coord, coords = c("long", "lat"), crs = 4326, agr = "constant")
class(narc.points)
```

    ## [1] "sf"         "data.frame"

Now that we are certain that this is in the correct format, we will plot the points

``` r
plot(narc.points)
```

![](Assignment_1_files/figure-markdown_github/unnamed-chunk-8-1.png)

Community Area boundary file
----------------------------

`sf` can read a geojson formatted file directly from the web, and we will exploit that functionality. So we will call this information directly from the Chicago Data Portal.

``` r
comm.file <- "https://data.cityofchicago.org/resource/igwz-8jzy.geojson"
chicago.comm <- read_sf(comm.file)
class(chicago.comm)
```

    ## [1] "sf"         "tbl_df"     "tbl"        "data.frame"

``` r
st_crs(chicago.comm)
```

    ## Coordinate Reference System:
    ##   EPSG: 4326 
    ##   proj4string: "+proj=longlat +datum=WGS84 +no_defs"

``` r
head(chicago.comm)
```

    ## Simple feature collection with 6 features and 9 fields
    ## geometry type:  MULTIPOLYGON
    ## dimension:      XY
    ## bbox:           xmin: -87.7069 ymin: 41.79448 xmax: -87.58001 ymax: 41.99076
    ## epsg (SRID):    4326
    ## proj4string:    +proj=longlat +datum=WGS84 +no_defs
    ## # A tibble: 6 x 10
    ##   community area  shape_area perimeter area_num_1 area_numbe comarea_id
    ##   <chr>     <chr> <chr>      <chr>     <chr>      <chr>      <chr>     
    ## 1 DOUGLAS   0     46004621.~ 0         35         35         0         
    ## 2 OAKLAND   0     16913961.~ 0         36         36         0         
    ## 3 FULLER P~ 0     19916704.~ 0         37         37         0         
    ## 4 GRAND BO~ 0     48492503.~ 0         38         38         0         
    ## 5 KENWOOD   0     29071741.~ 0         39         39         0         
    ## 6 LINCOLN ~ 0     71352328.~ 0         4          4          0         
    ## # ... with 3 more variables: comarea <chr>, shape_len <chr>,
    ## #   geometry <MULTIPOLYGON [째]>

Before moving on to the spatial join operation, we will convert both the community area boundaries and the narcotics points to the same projection, using the `st_transform` command. We assign the UTM (Universal Tranverse Mercator) zone 16N, which the the proper one for Chicago, with an EPSG code of 32616. After the projection transformation, we check the result using `st_crs`.

``` r
chicago.comm <- st_transform(chicago.comm,32616)
st_crs(chicago.comm)
```

    ## Coordinate Reference System:
    ##   EPSG: 32616 
    ##   proj4string: "+proj=utm +zone=16 +datum=WGS84 +units=m +no_defs"

``` r
narc.points <- st_transform(narc.points,32616)
st_crs(narc.points)
```

    ## Coordinate Reference System:
    ##   EPSG: 32616 
    ##   proj4string: "+proj=utm +zone=16 +datum=WGS84 +units=m +no_defs"

Spatial Join
============

``` r
comm.pts <- st_join(narc.points, chicago.comm["area_num_1"])
head(comm.pts)
```

    ## Simple feature collection with 6 features and 2 fields
    ## geometry type:  POINT
    ## dimension:      XY
    ## bbox:           xmin: 436232.5 ymin: 4635179 xmax: 447564.7 ymax: 4639214
    ## epsg (SRID):    32616
    ## proj4string:    +proj=utm +zone=16 +datum=WGS84 +units=m +no_defs
    ##   comm area_num_1                 geometry
    ## 1   25         25 POINT (436232.5 4636759)
    ## 2    8          8 POINT (447564.7 4639214)
    ## 3   29         29 POINT (440763.9 4635179)
    ## 4   26         26 POINT (439929.6 4636789)
    ## 5   23         23 POINT (440574.7 4638708)
    ## 6   23         23 POINT (440035.9 4638821)

``` r
is.numeric(comm.pts$area_num_1)
```

    ## [1] FALSE

``` r
comm.pts$area_num_1 <- as.integer(comm.pts$area_num_1)
is.integer(comm.pts$area_num_1)
```

    ## [1] TRUE

``` r
chicago.comm$area_num_1 <- as.integer(chicago.comm$area_num_1)
```

Counts by community area
========================

``` r
st_geometry(comm.pts) <- NULL
class(comm.pts)
```

    ## [1] "data.frame"

``` r
narc.cnts <- comm.pts %>% count(area_num_1)
narc.cnts <- narc.cnts %>% rename(comm = area_num_1, AGG.COUNT = n)

head(narc.cnts)
```

    ## # A tibble: 6 x 2
    ##    comm AGG.COUNT
    ##   <int>     <int>
    ## 1     1         5
    ## 2     2         6
    ## 3     3         9
    ## 4     4         4
    ## 5     5         2
    ## 6     6         3

Mapping the Narcotics counts
============================

Now that we have a polygon layer with some identifiers (`chicago.comm`) and a community identifier and the aggregate vehicle count (`narc.cnts`), we will do a `left_join` command.

``` r
chicago.comm <- left_join(chicago.comm,narc.cnts, by = c("area_num_1" = "comm"))
head(chicago.comm)
```

    ## Simple feature collection with 6 features and 10 fields
    ## geometry type:  MULTIPOLYGON
    ## dimension:      XY
    ## bbox:           xmin: 441440.4 ymin: 4627153 xmax: 451817.1 ymax: 4648971
    ## epsg (SRID):    32616
    ## proj4string:    +proj=utm +zone=16 +datum=WGS84 +units=m +no_defs
    ## # A tibble: 6 x 11
    ##   community area  shape_area perimeter area_num_1 area_numbe comarea_id
    ##   <chr>     <chr> <chr>      <chr>          <int> <chr>      <chr>     
    ## 1 DOUGLAS   0     46004621.~ 0                 35 35         0         
    ## 2 OAKLAND   0     16913961.~ 0                 36 36         0         
    ## 3 FULLER P~ 0     19916704.~ 0                 37 37         0         
    ## 4 GRAND BO~ 0     48492503.~ 0                 38 38         0         
    ## 5 KENWOOD   0     29071741.~ 0                 39 39         0         
    ## 6 LINCOLN ~ 0     71352328.~ 0                  4 4          0         
    ## # ... with 4 more variables: comarea <chr>, shape_len <chr>,
    ## #   AGG.COUNT <int>, geometry <MULTIPOLYGON [m]>

Basic choropleth map
====================

``` r
tm_shape(chicago.comm) +
  tm_polygons("AGG.COUNT")
```

![](Assignment_1_files/figure-markdown_github/unnamed-chunk-18-1.png)

--------------------------------------------------------------------
====================================================================

Extracting a pdf file
=====================

We will use the `pdftools` package to turn the contents of a pdf file into a list of long character strings.

``` r
pdf.file <- "http://www.actforchildren.org/wp-content/uploads/2017/02/Census-Data-by-Chicago-Community-Area-2016.pdf"
pop.dat <- pdf_text(pdf.file)
class(pop.dat)
```

    ## [1] "character"

``` r
pop.dat[1]
```

    ## [1] "                                                                            Population and Poverty Data by Chicago Community Area, updated December 2016\r\n                                                                                                                                                                                                                         Percent of Children\r\n                                                                                                                                                                                                                       Below 185% of Poverty\r\n                                                                                                                                                                      Percent of Children with Percent of Children in Level (Income Eligible for\r\n                                                            Population                                           Race/Ethnicity of Total Population                   All Parents in Labor Force     Poverty           Child Care Assistance)\r\n                                                                                                                                                       Percent Non<U+2010>\r\n                        Community\r\n                                                                                                                              Percent                    Hispanic\r\n                                                                                      Families                 Percent Non<U+2010>    Non<U+2010>     Percent Non<U+2010>     Other or\r\n                                      Total       Age       Age             Age     with a child   Percent       Hispanic     Hispanic    Hispanic       Multiple      Children     Children     Children  Children      Children     Children\r\n     Community          Number      Population    0<U+2010>2       3<U+2010>4    Age 5    6<U+2010>12    under age 6    Hispanic        Black       White       Asian          Races          0<U+2010>5          6<U+2010>17         0<U+2010>5       6<U+2010>11          0<U+2010>5          6<U+2010>11\r\nRogers Park                 1           54,402      2,131    1,196    809     3,577       2,875          24%            24%        42%            6%             3%          58%           66%         26%       33%           45%          72%\r\nWest Ridge                  2           72,211      3,547    1,987    993     7,113       4,072          18%            11%        44%           23%             4%          54%           67%         30%       38%           52%          59%\r\nUptown                      3           55,137      1,459      857    279     2,244       1,860          12%            21%        55%            9%             4%          71%           68%         23%       33%           37%          63%\r\nLincoln Square              4           40,761      1,939      955    300     2,199       2,243          18%             3%        65%           11%             3%          60%           65%         12%       17%           24%          29%\r\nNorth Center                5           34,623      1,934    1,042    456     2,613       2,317          14%             2%        76%            4%             3%          71%           78%          3%        4%            6%          12%\r\nLake View                   6           98,212      3,770    1,427    865     3,271       4,131           7%             3%        80%            7%             3%          75%           69%          5%        7%            9%          13%\r\nLincoln Park                7           66,959      2,335    1,438    487     3,423       2,775           7%             5%        80%            6%             3%          68%           54%          6%        6%            7%          10%\r\nNear North Side             8           89,465      2,343      927    312     2,516       2,529           5%             9%        72%           11%             3%          63%           64%         16%       36%           20%          41%\r\nEdison Park                 9           11,356        260      293     40     1,130          411          9%             1%        87%            1%             1%          63%           70%          0%        0%            4%           4%\r\nNorwood Park               10           36,083      1,249    1,033    432     2,882       1,772          12%             1%         81%           4%             2%          75%           82%          5%        6%           12%          13%\r\nJefferson Park             11           27,264      1,004      935    198     1,938       1,417          23%             1%        65%            9%             2%          62%           75%         14%       10%           26%          30%\r\nForest Glen                12           18,437        562      661    212     2,120          936         12%             1%        72%           13%             2%          78%           76%          3%        3%            9%           8%\r\nNorth Park                 13           18,442        644      591    255     1,379          947         20%             3%        51%           25%             2%          61%           74%         34%        8%           51%          39%\r\nAlbany Park                14           52,079      2,134    1,643    645     4,428       2,884          48%             4%         29%          16%             2%          61%           74%         26%       34%           49%          60%\r\nPortage Park               15           64,846      2,415    1,421    968     5,210       3,335          42%             1%         50%           4%             2%          68%           69%         16%       22%           38%          43%\r\nIrving Park                16           56,520      2,607    1,701    728     4,726       3,483          47%             2%         40%           8%             3%          66%           68%         19%       13%           45%          50%\r\nDunning                    17           43,025      1,835      979    507     3,151       2,281          27%             2%        66%            4%             1%          78%           72%         21%       11%           39%          35%\r\nMontclare                  18           12,973        576      547    205     1,095          825         62%             4%        28%            5%             1%          73%           77%          9%       26%           37%          64%\r\nBelmont Cragin             19           79,210      3,539    2,815  1,494     9,121       5,284          80%             3%        14%            1%             1%          65%           70%         29%       29%           63%          61%\r\nHermosa                    20           25,489      1,246      949    508     2,539       1,668          88%             1%          7%           1%             2%          67%           77%         31%       36%           66%          69%\r\nAvondale                   21           39,721      1,773    1,377    521     3,456       2,403          62%             2%        31%            3%             2%          59%           50%         25%       31%           56%          65%\r\nLogan Square               22           74,549      3,311    1,903    960     5,208       4,124          47%             5%        44%            3%             2%          65%           65%         22%       28%           35%          53%\r\nHumboldt Park              23           55,011      2,453    2,163    999     6,267        3,414         52%            41%          5%           0%             1%          65%           69%         52%       50%           81%          78%\r\nWest Town                  24           81,828      3,830    1,828    745     4,404       4,366          28%             7%        58%            4%             3%          76%           74%         15%       31%           24%          42%\r\nAustin                     25           97,643      4,108    3,042    953     9,813       5,075          11%            83%          4%           0%             1%          71%           76%         45%       44%           70%          68%\r\nWest Garfield Park         26           17,733      1,033      741    249     2,156       1,119           2%            96%          2%           0%             0%          57%           63%         69%       61%           83%          81%\r\nEast Garfield Park         27           20,665      1,054      678    440     2,409       1,349           4%            91%          4%           0%             1%          56%           61%         56%       61%           80%          80%\r\nNear West Side             28           61,768      2,211    1,248    510     2,927       2,752          10%            30%        42%           16%             2%          76%           81%         25%       30%           35%          49%\r\nNorth Lawndale             29           35,276      1,652    1,166    709     4,554       2,210           7%            89%          2%           0%             1%          64%           63%         64%       68%           85%          85%\r\nSouth Lawndale             30           73,826      3,664    2,396  1,166     8,098       4,602          85%            11%          3%           0%             0%          57%           55%         47%       57%           81%          85%\r\nLower West Side            31           34,410      1,166      972    515     3,734       1,823          80%             3%        13%            2%             1%          69%           63%         40%       40%           73%          76%\r\nLoop                       32           33,442        995      377    111       557       1,125           6%            12%        61%           17%             3%          74%           96%         11%       13%           11%          13%\r\nNear South Side            33           22,401        994      631    270     1,152       1,264           6%            24%        46%           20%             3%          55%           82%         15%       15%           22%          16%\r\nArmour Square              34           14,007        499      134     67     1,052          546          3%            10%        11%           74%             3%          66%           73%         42%       36%           74%          71%\r\nDouglas                    35           20,323        522      294    138     1,372          724          2%            71%        11%           14%             2%          73%           80%         59%       57%           70%          64%\r\nOakland                    36             6,422       251      201    154       656          485          3%            93%          2%           2%             0%          94%           86%         50%       30%           66%          55%\r\nFuller Park                37             2,457        87       63     51       171          134          6%            92%          2%           0%             0%          73%           64%         35%       60%           62%          65%\r\nGrand Boulevard            38           22,373        854      906    273     2,088       1,314           3%            91%          3%           1%             3%          77%           85%         49%       45%           63%          64%\r\nKenwood                    39           17,601        468      489    133     1,120          791          4%            68%        17%            9%             3%          63%           88%         31%       30%           38%          44%\r\nWashington Park            40           12,081        811      583    166     1,745       1,002           1%            96%          0%           0%             2%          76%           80%         73%       63%           88%          83%\r\nHyde Park                  41           26,893        707      642    158     1,338       1,008           7%            30%        47%           12%             4%          65%           77%         18%       13%           38%          26%\r\nWoodlawn                   42           26,446      1,063      866    429     2,783       1,538           3%            85%          7%           3%             2%          63%           69%         54%       48%           73%          69%\r\nPrepared by Illinois Action for Children                                                                        www.actforchildren.org                                                                                                  Page 1 of 2\r\n"

We realize that this extracts each page as a single entry.

``` r
length(pop.dat)
```

    ## [1] 2

We will extract the population information from this unknown data type of strings. This information will be initialized in `nnlist`. We will store each line into this list, by parsing it with the new line character `/n`.

``` r
nnlist <- ""
ppage <- strsplit(pop.dat[[1]],split="\n")
ppage[[1]]
```

    ##  [1] "                                                                            Population and Poverty Data by Chicago Community Area, updated December 2016\r"                                                                                           
    ##  [2] "                                                                                                                                                                                                                         Percent of Children\r"       
    ##  [3] "                                                                                                                                                                                                                       Below 185% of Poverty\r"       
    ##  [4] "                                                                                                                                                                      Percent of Children with Percent of Children in Level (Income Eligible for\r"   
    ##  [5] "                                                            Population                                           Race/Ethnicity of Total Population                   All Parents in Labor Force     Poverty           Child Care Assistance)\r"      
    ##  [6] "                                                                                                                                                       Percent Non<U+2010>\r"                                                                         
    ##  [7] "                        Community\r"                                                                                                                                                                                                                  
    ##  [8] "                                                                                                                              Percent                    Hispanic\r"                                                                                  
    ##  [9] "                                                                                      Families                 Percent Non<U+2010>    Non<U+2010>     Percent Non<U+2010>     Other or\r"                                                             
    ## [10] "                                      Total       Age       Age             Age     with a child   Percent       Hispanic     Hispanic    Hispanic       Multiple      Children     Children     Children  Children      Children     Children\r"     
    ## [11] "     Community          Number      Population    0<U+2010>2       3<U+2010>4    Age 5    6<U+2010>12    under age 6    Hispanic        Black       White       Asian          Races          0<U+2010>5          6<U+2010>17         0<U+2010>5       6<U+2010>11          0<U+2010>5          6<U+2010>11\r"
    ## [12] "Rogers Park                 1           54,402      2,131    1,196    809     3,577       2,875          24%            24%        42%            6%             3%          58%           66%         26%       33%           45%          72%\r"    
    ## [13] "West Ridge                  2           72,211      3,547    1,987    993     7,113       4,072          18%            11%        44%           23%             4%          54%           67%         30%       38%           52%          59%\r"    
    ## [14] "Uptown                      3           55,137      1,459      857    279     2,244       1,860          12%            21%        55%            9%             4%          71%           68%         23%       33%           37%          63%\r"    
    ## [15] "Lincoln Square              4           40,761      1,939      955    300     2,199       2,243          18%             3%        65%           11%             3%          60%           65%         12%       17%           24%          29%\r"    
    ## [16] "North Center                5           34,623      1,934    1,042    456     2,613       2,317          14%             2%        76%            4%             3%          71%           78%          3%        4%            6%          12%\r"    
    ## [17] "Lake View                   6           98,212      3,770    1,427    865     3,271       4,131           7%             3%        80%            7%             3%          75%           69%          5%        7%            9%          13%\r"    
    ## [18] "Lincoln Park                7           66,959      2,335    1,438    487     3,423       2,775           7%             5%        80%            6%             3%          68%           54%          6%        6%            7%          10%\r"    
    ## [19] "Near North Side             8           89,465      2,343      927    312     2,516       2,529           5%             9%        72%           11%             3%          63%           64%         16%       36%           20%          41%\r"    
    ## [20] "Edison Park                 9           11,356        260      293     40     1,130          411          9%             1%        87%            1%             1%          63%           70%          0%        0%            4%           4%\r"    
    ## [21] "Norwood Park               10           36,083      1,249    1,033    432     2,882       1,772          12%             1%         81%           4%             2%          75%           82%          5%        6%           12%          13%\r"    
    ## [22] "Jefferson Park             11           27,264      1,004      935    198     1,938       1,417          23%             1%        65%            9%             2%          62%           75%         14%       10%           26%          30%\r"    
    ## [23] "Forest Glen                12           18,437        562      661    212     2,120          936         12%             1%        72%           13%             2%          78%           76%          3%        3%            9%           8%\r"    
    ## [24] "North Park                 13           18,442        644      591    255     1,379          947         20%             3%        51%           25%             2%          61%           74%         34%        8%           51%          39%\r"    
    ## [25] "Albany Park                14           52,079      2,134    1,643    645     4,428       2,884          48%             4%         29%          16%             2%          61%           74%         26%       34%           49%          60%\r"    
    ## [26] "Portage Park               15           64,846      2,415    1,421    968     5,210       3,335          42%             1%         50%           4%             2%          68%           69%         16%       22%           38%          43%\r"    
    ## [27] "Irving Park                16           56,520      2,607    1,701    728     4,726       3,483          47%             2%         40%           8%             3%          66%           68%         19%       13%           45%          50%\r"    
    ## [28] "Dunning                    17           43,025      1,835      979    507     3,151       2,281          27%             2%        66%            4%             1%          78%           72%         21%       11%           39%          35%\r"    
    ## [29] "Montclare                  18           12,973        576      547    205     1,095          825         62%             4%        28%            5%             1%          73%           77%          9%       26%           37%          64%\r"    
    ## [30] "Belmont Cragin             19           79,210      3,539    2,815  1,494     9,121       5,284          80%             3%        14%            1%             1%          65%           70%         29%       29%           63%          61%\r"    
    ## [31] "Hermosa                    20           25,489      1,246      949    508     2,539       1,668          88%             1%          7%           1%             2%          67%           77%         31%       36%           66%          69%\r"    
    ## [32] "Avondale                   21           39,721      1,773    1,377    521     3,456       2,403          62%             2%        31%            3%             2%          59%           50%         25%       31%           56%          65%\r"    
    ## [33] "Logan Square               22           74,549      3,311    1,903    960     5,208       4,124          47%             5%        44%            3%             2%          65%           65%         22%       28%           35%          53%\r"    
    ## [34] "Humboldt Park              23           55,011      2,453    2,163    999     6,267        3,414         52%            41%          5%           0%             1%          65%           69%         52%       50%           81%          78%\r"    
    ## [35] "West Town                  24           81,828      3,830    1,828    745     4,404       4,366          28%             7%        58%            4%             3%          76%           74%         15%       31%           24%          42%\r"    
    ## [36] "Austin                     25           97,643      4,108    3,042    953     9,813       5,075          11%            83%          4%           0%             1%          71%           76%         45%       44%           70%          68%\r"    
    ## [37] "West Garfield Park         26           17,733      1,033      741    249     2,156       1,119           2%            96%          2%           0%             0%          57%           63%         69%       61%           83%          81%\r"    
    ## [38] "East Garfield Park         27           20,665      1,054      678    440     2,409       1,349           4%            91%          4%           0%             1%          56%           61%         56%       61%           80%          80%\r"    
    ## [39] "Near West Side             28           61,768      2,211    1,248    510     2,927       2,752          10%            30%        42%           16%             2%          76%           81%         25%       30%           35%          49%\r"    
    ## [40] "North Lawndale             29           35,276      1,652    1,166    709     4,554       2,210           7%            89%          2%           0%             1%          64%           63%         64%       68%           85%          85%\r"    
    ## [41] "South Lawndale             30           73,826      3,664    2,396  1,166     8,098       4,602          85%            11%          3%           0%             0%          57%           55%         47%       57%           81%          85%\r"    
    ## [42] "Lower West Side            31           34,410      1,166      972    515     3,734       1,823          80%             3%        13%            2%             1%          69%           63%         40%       40%           73%          76%\r"    
    ## [43] "Loop                       32           33,442        995      377    111       557       1,125           6%            12%        61%           17%             3%          74%           96%         11%       13%           11%          13%\r"    
    ## [44] "Near South Side            33           22,401        994      631    270     1,152       1,264           6%            24%        46%           20%             3%          55%           82%         15%       15%           22%          16%\r"    
    ## [45] "Armour Square              34           14,007        499      134     67     1,052          546          3%            10%        11%           74%             3%          66%           73%         42%       36%           74%          71%\r"    
    ## [46] "Douglas                    35           20,323        522      294    138     1,372          724          2%            71%        11%           14%             2%          73%           80%         59%       57%           70%          64%\r"    
    ## [47] "Oakland                    36             6,422       251      201    154       656          485          3%            93%          2%           2%             0%          94%           86%         50%       30%           66%          55%\r"    
    ## [48] "Fuller Park                37             2,457        87       63     51       171          134          6%            92%          2%           0%             0%          73%           64%         35%       60%           62%          65%\r"    
    ## [49] "Grand Boulevard            38           22,373        854      906    273     2,088       1,314           3%            91%          3%           1%             3%          77%           85%         49%       45%           63%          64%\r"    
    ## [50] "Kenwood                    39           17,601        468      489    133     1,120          791          4%            68%        17%            9%             3%          63%           88%         31%       30%           38%          44%\r"    
    ## [51] "Washington Park            40           12,081        811      583    166     1,745       1,002           1%            96%          0%           0%             2%          76%           80%         73%       63%           88%          83%\r"    
    ## [52] "Hyde Park                  41           26,893        707      642    158     1,338       1,008           7%            30%        47%           12%             4%          65%           77%         18%       13%           38%          26%\r"    
    ## [53] "Woodlawn                   42           26,446      1,063      866    429     2,783       1,538           3%            85%          7%           3%             2%          63%           69%         54%       48%           73%          69%\r"    
    ## [54] "Prepared by Illinois Action for Children                                                                        www.actforchildren.org                                                                                                  Page 1 of 2\r"

The first eleven lines are useless for our purpose. So, we will eliminate them

``` r
nni <- ppage[[1]]
nni <- nni[-(1:11)]
nni
```

    ##  [1] "Rogers Park                 1           54,402      2,131    1,196    809     3,577       2,875          24%            24%        42%            6%             3%          58%           66%         26%       33%           45%          72%\r"    
    ##  [2] "West Ridge                  2           72,211      3,547    1,987    993     7,113       4,072          18%            11%        44%           23%             4%          54%           67%         30%       38%           52%          59%\r"    
    ##  [3] "Uptown                      3           55,137      1,459      857    279     2,244       1,860          12%            21%        55%            9%             4%          71%           68%         23%       33%           37%          63%\r"    
    ##  [4] "Lincoln Square              4           40,761      1,939      955    300     2,199       2,243          18%             3%        65%           11%             3%          60%           65%         12%       17%           24%          29%\r"    
    ##  [5] "North Center                5           34,623      1,934    1,042    456     2,613       2,317          14%             2%        76%            4%             3%          71%           78%          3%        4%            6%          12%\r"    
    ##  [6] "Lake View                   6           98,212      3,770    1,427    865     3,271       4,131           7%             3%        80%            7%             3%          75%           69%          5%        7%            9%          13%\r"    
    ##  [7] "Lincoln Park                7           66,959      2,335    1,438    487     3,423       2,775           7%             5%        80%            6%             3%          68%           54%          6%        6%            7%          10%\r"    
    ##  [8] "Near North Side             8           89,465      2,343      927    312     2,516       2,529           5%             9%        72%           11%             3%          63%           64%         16%       36%           20%          41%\r"    
    ##  [9] "Edison Park                 9           11,356        260      293     40     1,130          411          9%             1%        87%            1%             1%          63%           70%          0%        0%            4%           4%\r"    
    ## [10] "Norwood Park               10           36,083      1,249    1,033    432     2,882       1,772          12%             1%         81%           4%             2%          75%           82%          5%        6%           12%          13%\r"    
    ## [11] "Jefferson Park             11           27,264      1,004      935    198     1,938       1,417          23%             1%        65%            9%             2%          62%           75%         14%       10%           26%          30%\r"    
    ## [12] "Forest Glen                12           18,437        562      661    212     2,120          936         12%             1%        72%           13%             2%          78%           76%          3%        3%            9%           8%\r"    
    ## [13] "North Park                 13           18,442        644      591    255     1,379          947         20%             3%        51%           25%             2%          61%           74%         34%        8%           51%          39%\r"    
    ## [14] "Albany Park                14           52,079      2,134    1,643    645     4,428       2,884          48%             4%         29%          16%             2%          61%           74%         26%       34%           49%          60%\r"    
    ## [15] "Portage Park               15           64,846      2,415    1,421    968     5,210       3,335          42%             1%         50%           4%             2%          68%           69%         16%       22%           38%          43%\r"    
    ## [16] "Irving Park                16           56,520      2,607    1,701    728     4,726       3,483          47%             2%         40%           8%             3%          66%           68%         19%       13%           45%          50%\r"    
    ## [17] "Dunning                    17           43,025      1,835      979    507     3,151       2,281          27%             2%        66%            4%             1%          78%           72%         21%       11%           39%          35%\r"    
    ## [18] "Montclare                  18           12,973        576      547    205     1,095          825         62%             4%        28%            5%             1%          73%           77%          9%       26%           37%          64%\r"    
    ## [19] "Belmont Cragin             19           79,210      3,539    2,815  1,494     9,121       5,284          80%             3%        14%            1%             1%          65%           70%         29%       29%           63%          61%\r"    
    ## [20] "Hermosa                    20           25,489      1,246      949    508     2,539       1,668          88%             1%          7%           1%             2%          67%           77%         31%       36%           66%          69%\r"    
    ## [21] "Avondale                   21           39,721      1,773    1,377    521     3,456       2,403          62%             2%        31%            3%             2%          59%           50%         25%       31%           56%          65%\r"    
    ## [22] "Logan Square               22           74,549      3,311    1,903    960     5,208       4,124          47%             5%        44%            3%             2%          65%           65%         22%       28%           35%          53%\r"    
    ## [23] "Humboldt Park              23           55,011      2,453    2,163    999     6,267        3,414         52%            41%          5%           0%             1%          65%           69%         52%       50%           81%          78%\r"    
    ## [24] "West Town                  24           81,828      3,830    1,828    745     4,404       4,366          28%             7%        58%            4%             3%          76%           74%         15%       31%           24%          42%\r"    
    ## [25] "Austin                     25           97,643      4,108    3,042    953     9,813       5,075          11%            83%          4%           0%             1%          71%           76%         45%       44%           70%          68%\r"    
    ## [26] "West Garfield Park         26           17,733      1,033      741    249     2,156       1,119           2%            96%          2%           0%             0%          57%           63%         69%       61%           83%          81%\r"    
    ## [27] "East Garfield Park         27           20,665      1,054      678    440     2,409       1,349           4%            91%          4%           0%             1%          56%           61%         56%       61%           80%          80%\r"    
    ## [28] "Near West Side             28           61,768      2,211    1,248    510     2,927       2,752          10%            30%        42%           16%             2%          76%           81%         25%       30%           35%          49%\r"    
    ## [29] "North Lawndale             29           35,276      1,652    1,166    709     4,554       2,210           7%            89%          2%           0%             1%          64%           63%         64%       68%           85%          85%\r"    
    ## [30] "South Lawndale             30           73,826      3,664    2,396  1,166     8,098       4,602          85%            11%          3%           0%             0%          57%           55%         47%       57%           81%          85%\r"    
    ## [31] "Lower West Side            31           34,410      1,166      972    515     3,734       1,823          80%             3%        13%            2%             1%          69%           63%         40%       40%           73%          76%\r"    
    ## [32] "Loop                       32           33,442        995      377    111       557       1,125           6%            12%        61%           17%             3%          74%           96%         11%       13%           11%          13%\r"    
    ## [33] "Near South Side            33           22,401        994      631    270     1,152       1,264           6%            24%        46%           20%             3%          55%           82%         15%       15%           22%          16%\r"    
    ## [34] "Armour Square              34           14,007        499      134     67     1,052          546          3%            10%        11%           74%             3%          66%           73%         42%       36%           74%          71%\r"    
    ## [35] "Douglas                    35           20,323        522      294    138     1,372          724          2%            71%        11%           14%             2%          73%           80%         59%       57%           70%          64%\r"    
    ## [36] "Oakland                    36             6,422       251      201    154       656          485          3%            93%          2%           2%             0%          94%           86%         50%       30%           66%          55%\r"    
    ## [37] "Fuller Park                37             2,457        87       63     51       171          134          6%            92%          2%           0%             0%          73%           64%         35%       60%           62%          65%\r"    
    ## [38] "Grand Boulevard            38           22,373        854      906    273     2,088       1,314           3%            91%          3%           1%             3%          77%           85%         49%       45%           63%          64%\r"    
    ## [39] "Kenwood                    39           17,601        468      489    133     1,120          791          4%            68%        17%            9%             3%          63%           88%         31%       30%           38%          44%\r"    
    ## [40] "Washington Park            40           12,081        811      583    166     1,745       1,002           1%            96%          0%           0%             2%          76%           80%         73%       63%           88%          83%\r"    
    ## [41] "Hyde Park                  41           26,893        707      642    158     1,338       1,008           7%            30%        47%           12%             4%          65%           77%         18%       13%           38%          26%\r"    
    ## [42] "Woodlawn                   42           26,446      1,063      866    429     2,783       1,538           3%            85%          7%           3%             2%          63%           69%         54%       48%           73%          69%\r"    
    ## [43] "Prepared by Illinois Action for Children                                                                        www.actforchildren.org                                                                                                  Page 1 of 2\r"

Now we append this with the information on the next page:

``` r
nnu <- unlist(nni)
nnlist <- c(nnlist,nnu)

nnlist <- ""
for (i in 1:2) {
  ppage <- strsplit(pop.dat[[i]],split="\n")
  nni <- ppage[[1]]
  nni <- nni[-(1:11)]
  nnu <- unlist(nni)
  nnlist <- c(nnlist,nnu)
}
# we extract the first and last elements, as they are not meaningful to our analysis
nnlist <- nnlist[2:(length(nnlist)-2)]
nnlist <- nnlist[-43]
```

Extracting the population values
================================

``` r
nnpop <- vector(mode = "numeric", length = length(nnlist))
for (i in (1:length(nnlist))) {
  popchar <- strsplit(nnlist[i], "\\s+")
  popchar <- popchar[[1]][1:5]
  if(grepl(",", popchar[3])) {
    popval <- as.numeric(gsub(",", "", popchar[3]))
    nnpop[i] <- popval
    } else if(grepl(",", popchar[4])){
    popval <- as.numeric(gsub(",", "", popchar[4]))
    nnpop[i] <- popval
  } else if(grepl(",", popchar[5])){
    popval <- as.numeric(gsub(",", "", popchar[5]))
    nnpop[i] <- popval
  }
}

nnpop
```

    ##  [1] 54402 72211 55137 40761 34623 98212 66959 89465 11356 36083 27264
    ## [12] 18437 18442 52079 64846 56520 43025 12973 79210 25489 39721 74549
    ## [23] 55011 81828 97643 17733 20665 61768 35276 73826 34410 33442 22401
    ## [34] 14007 20323  6422  2457 22373 17601 12081 26893 26446 49155 31359
    ## [45]  9813 28095  2601 13732 42505  6501 15305 23817 27973  7090  8952
    ## [56] 35632 13293 44202 16336 33939 41083 19192 41202 24962 32749 55551
    ## [67] 32156 26121 32346 42752 45842 20815 27116 18783 22908 13483 54873

Then, we want to create an indexing for these values;

``` r
nnid <- (1:length(nnlist))
nnid
```

    ##  [1]  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23
    ## [24] 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46
    ## [47] 47 48 49 50 51 52 53 54 55 56 57 58 59 60 61 62 63 64 65 66 67 68 69
    ## [70] 70 71 72 73 74 75 76 77

Now, we will convert `nnid` and \``nnpop` into a data frame using the `data.frame` command.

``` r
neighpop <- data.frame(as.integer(nnid), nnpop)
names(neighpop) <- c("NID", "POP2016")
head(neighpop)
```

    ##   NID POP2016
    ## 1   1   54402
    ## 2   2   72211
    ## 3   3   55137
    ## 4   4   40761
    ## 5   5   34623
    ## 6   6   98212

Mapping Community Area Abandoned Vehicles per Capita
====================================================

Computing abandoned vehicles per capita
---------------------------------------

``` r
chicago.comm <- left_join(chicago.comm, neighpop, by = c("area_num_1" = "NID"))
head(chicago.comm)
```

    ## Simple feature collection with 6 features and 11 fields
    ## geometry type:  MULTIPOLYGON
    ## dimension:      XY
    ## bbox:           xmin: 441440.4 ymin: 4627153 xmax: 451817.1 ymax: 4648971
    ## epsg (SRID):    32616
    ## proj4string:    +proj=utm +zone=16 +datum=WGS84 +units=m +no_defs
    ## # A tibble: 6 x 12
    ##   community area  shape_area perimeter area_num_1 area_numbe comarea_id
    ##   <chr>     <chr> <chr>      <chr>          <int> <chr>      <chr>     
    ## 1 DOUGLAS   0     46004621.~ 0                 35 35         0         
    ## 2 OAKLAND   0     16913961.~ 0                 36 36         0         
    ## 3 FULLER P~ 0     19916704.~ 0                 37 37         0         
    ## 4 GRAND BO~ 0     48492503.~ 0                 38 38         0         
    ## 5 KENWOOD   0     29071741.~ 0                 39 39         0         
    ## 6 LINCOLN ~ 0     71352328.~ 0                  4 4          0         
    ## # ... with 5 more variables: comarea <chr>, shape_len <chr>,
    ## #   AGG.COUNT <int>, POP2016 <dbl>, geometry <MULTIPOLYGON [m]>

``` r
chicago.comm <- chicago.comm %>% mutate(narcpcap = (AGG.COUNT / POP2016) * 1000) 
head(chicago.comm)
```

    ## Simple feature collection with 6 features and 12 fields
    ## geometry type:  MULTIPOLYGON
    ## dimension:      XY
    ## bbox:           xmin: 441440.4 ymin: 4627153 xmax: 451817.1 ymax: 4648971
    ## epsg (SRID):    32616
    ## proj4string:    +proj=utm +zone=16 +datum=WGS84 +units=m +no_defs
    ## # A tibble: 6 x 13
    ##   community area  shape_area perimeter area_num_1 area_numbe comarea_id
    ##   <chr>     <chr> <chr>      <chr>          <int> <chr>      <chr>     
    ## 1 DOUGLAS   0     46004621.~ 0                 35 35         0         
    ## 2 OAKLAND   0     16913961.~ 0                 36 36         0         
    ## 3 FULLER P~ 0     19916704.~ 0                 37 37         0         
    ## 4 GRAND BO~ 0     48492503.~ 0                 38 38         0         
    ## 5 KENWOOD   0     29071741.~ 0                 39 39         0         
    ## 6 LINCOLN ~ 0     71352328.~ 0                  4 4          0         
    ## # ... with 6 more variables: comarea <chr>, shape_len <chr>,
    ## #   AGG.COUNT <int>, POP2016 <dbl>, narcpcap <dbl>, geometry <MULTIPOLYGON
    ## #   [m]>

Final choropleth map
====================

``` r
tm_shape(chicago.comm) +
  tm_polygons("narcpcap")
```

![](Assignment_1_files/figure-markdown_github/unnamed-chunk-29-1.png)

This result is completely crazy! `narcpcap` maximum is 7.5. This is East Garfield Park The second highest is North Lawndale.
