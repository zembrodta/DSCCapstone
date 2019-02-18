---
title: "5 Facts about the Injury Data Set"
date: '2019-02-18'
output:
  html_document:
    keep_md: true
    number_sections: true
    fig_caption: true
    toc: true
    fig_width: 9
    fig_height: 6
    theme: cosmo
    highlight: tango
    code_folding: hide
    self_contained: no 
---


#Data Set

For the first section of our project, we are focusing on the OSHA data set and separating the jobs into service sector jobs and non-service sector jobs. All of my code and the data set are available directly on Alli's github https://github.com/zembrodta/DSCCapstone.

##Packages 

I am using the tidyverse for basic cleaning. Lubridate for making handling dates easier. Plotly to make ggplot visualizations interactive. Knitr to make easy tables in Rmarkdown. Reshape2 to transform some of our data. These will all need to be installed in order for this code to be runnable.

##Import Data

The following code imports our data set. Right off the bat we decided to remove the column Address2, and Inspection because they were mostly null. We also made sure to convert the eventdate column into a data format and make the ID column a character because there are some weird values which made it not numeric. I also added columns for month and year so we could do some analysis based on time of year. 


```r
 severeinjury <- read_csv("~/Documents/GitHub/DSCCapstone/datasets/severeinjury.csv", 
    col_types = cols(Address2 = col_skip(), 
    EventDate = col_date(format = "%m/%d/%Y"), 
    ID = col_character(), Inspection = col_skip()))
colnames(severeinjury)[colnames(severeinjury)=="Primary NAICS"] <- "naics"
colnames(severeinjury)[colnames(severeinjury)=="Part of Body Title"] <- "bodyPart"
severeinjury$month = month(severeinjury$EventDate)
severeinjury$year= year(severeinjury$EventDate)
```


##Cleaning
Now that our dataset is imported, we would like to check to see if data has duplicate values of any nulls or duplicate values. 

###Missing Data 

Let's print out a complete summary of the number of missings in each column. The code below summarizes each column and counts the number of nulls. 


```r
Nulls= severeinjury%>%
  summarise_all(funs(sum(is.na(.))))
Nulls=melt(Nulls)
kable(Nulls)
```



variable                  value
-----------------------  ------
ID                            0
UPA                           0
EventDate                     0
Employer                      0
Address1                     11
City                         11
State                         0
Zip                          13
Latitude                      4
Longitude                     4
naics                         3
Hospitalized                  0
Amputation                    2
Final Narrative               0
Nature                        0
NatureTitle                   0
Part of Body                  0
bodyPart                      0
Event                         0
EventTitle                    0
Source                        0
SourceTitle                   0
Secondary Source          23509
Secondary Source Title    23509
month                         0
year                          0

For the most part our data looks pretty good. There are two columns that are about 60% null, but those are about secondary sources. It would make sense for secondary source to be mainly null, because sometimes there are not always two sources. For right now, I am going to leave those columns because we aren't really using them. 

What I am more concerned about is the 3 nulls in the NAISC code column. Since we are going to be breaking up the records into two different files, the service sector and the non-service sector, we will need to get rid of those. I will use the function, drop_na in the tidyverse to remove these values. I am also going to get rid of the 2 null values in the amputation column because we may use that for some analysis and it will mess up any counting and averaging. 


```r
severeinjury = severeinjury %>%
  drop_na(naics, Amputation)
```

###Duplicate Data

Since the id column should be unique, I am going to check to make sure the number of rows in the severeInjury data equals the number of unique Ids. 


```r
length(unique(severeinjury$ID))
```

```
## [1] 32117
```

```r
nrow(severeinjury)
```

```
## [1] 32122
```
There are 5 id values that are the same. So, ID is either not unique or we have some duplicates. Let's investigate further. 


```r
NotUniqueIDs = severeinjury %>%
  group_by(ID) %>%
  summarise( count = n()) %>%
  filter( count >1)

SameIDs = severeinjury %>%
  filter( ID == "2015010015" | ID == "2015010016" | ID == "2015010018" |ID =="2015010020" | ID =='2015010021') %>% select (ID, EventDate, Employer, City, State, bodyPart) 
SameIDs = SameIDs[order(SameIDs$ID),]
kable(SameIDs)
```



ID           EventDate    Employer                                         City          State          bodyPart                         
-----------  -----------  -----------------------------------------------  ------------  -------------  ---------------------------------
2015010015   2015-01-01   FCI Otisville Federal Correctional Institution   OTISVILLE     NEW YORK       Lower leg(s)                     
2015010015   2015-01-19   Lochridge Priest, Inc.                           WACO          TEXAS          Thigh(s)                         
2015010016   2015-01-01   Kalahari Manufacturing LLC                       LAKE DELTON   WISCONSIN      Leg(s), n.e.c.                   
2015010016   2015-01-26   HDA Motors, Inc. dba Continental Honda           COUNTRYSIDE   ILLINOIS       Brain                            
2015010018   2015-01-01   Schneider National Bulk Carrier                  CORAOPOLIS    PENNSYLVANIA   Nonclassifiable                  
2015010018   2015-01-28   RCS SYSTEMS, INC.                                MILWAUKEE     WISCONSIN      Hand(s), unspecified             
2015010020   2015-01-01   North American Pipe Corporation                  JANESVILLE    WISCONSIN      Finger(s), fingernail(s), n.e.c. 
2015010020   2015-01-30   Schiavone Construction Co, Inc                   JERSEY CITY   NEW JERSEY     Fingertip(s)                     
2015010021   2015-01-01   The Home Depot, Inc.                             CAPE CORAL    FLORIDA        Elbow(s)                         
2015010021   2015-01-30   Schiavone Construction Co. LLC                   JERSEY CITY   NEW JERSEY     Fingertip(s)                     

So, even though they have the same ID these are unique records. This is kind of concerning because it means that ID is not unique. I am going to do a check that NatureTitle, Day and Employer are all unique. To me, that would be a unique combination. 


```r
NatureDayEmployer = severeinjury %>%
  group_by(NatureTitle, EventDate, Employer)

nrow(NatureDayEmployer) == nrow(severeinjury)
```

```
## [1] TRUE
```

They are the same, so I am going to assume that we have unique values. I would like to check with the CDC/ NIOSH representatives to ensure that this was a correct assumption and make sure they are aware that ID is not completely unique. 


#Non Service and Service Jobs 

One of the big goals of our project is to see if there are any differences between service sector jobs and non-service sector jobs. We will be looking at the different types of injuries common in the two sectors, the difference in body parts, and the time of year the injuries occur. Before we begin that analysis, I we will need to separate the data set. We will be doing this by using NAICS codes. 

##Naics Code Digit Checks

I want to make sure that all the records have a full 6 digit naics code before I remove them. I saw something suspicious when I was looking through the data manually and I want to double check that everything has the correct number of digits based on the chart we are using. 


```r
severeWithdigits = severeinjury %>%
  mutate(NumberOfNaics = nchar(naics))

max(severeWithdigits$NumberOfNaics)
```

```
## [1] 6
```

```r
min(severeWithdigits$NumberOfNaics)
```

```
## [1] 2
```


The max number of digits is 6 which is what we expected, but the minimum is 2. This is concerning, because we do not know how to interpret the 2 digit codes. Let's check how many records have less than 6 to see if it is significant enough to worry about them. The code below checks to see how many are less than 6. 


```r
sum(severeWithdigits$NumberOfNaics <= 5)
```

```
## [1] 131
```

There are 131 records that have naics codes that either need to be handled separately or just thrown out, for now I am going to remove them by putting them in a different data set in case we want to investigate them further later. 


```r
NotRealNaisc = severeWithdigits %>%
  filter( NumberOfNaics <= 5)

Injury = severeWithdigits %>%
  filter( NumberOfNaics == 6)
```


##Seperate the data sets into Service and NonService 

We will be using the following chart to separate our data sets. The numbers in the far-right column correspond to the first two digits of our NAICS codes. Utilities made this slightly tricky because it has a NAICS code of 22 which falls in the middle of the NonService producing numbers, but with a little work around our datasets were separated.

![NAICS Codes.](~/Documents/GitHub/DSCCapstone/figures/NAICSCodes.png)




```r
Utilities = Injury %>% 
  filter(naics >= 220000 & naics <230000)
  
Service = Injury %>%
  filter( naics >= 400000)

Service = rbind(Service, Utilities)

NonServiceTop = Injury %>%
  filter(naics < 400000 & naics >= 230000) 

NonServiceBottom = Injury %>%
  filter(naics < 220000)

NonService = rbind(NonServiceTop, NonServiceBottom)
```

#Fun Facts 
After all of our cleaning, it is finally time to start looking at interesting facts in our data. 

##Fact 1: Percentage Hospitalized Injuries in Service and NonService Job Sectors


```r
a=sum(NonService$Hospitalized)/nrow(NonService)
b= sum(Service$Hospitalized)/nrow(Service)

Sector = c( 'NonService', 'Service' )
Percentage = c( a, b)

HospitalizationPercentage = data.frame( Sector, Percentage)
kable( HospitalizationPercentage)
```



Sector        Percentage
-----------  -----------
NonService     0.7574010
Service        0.8735039

There is quite a big difference in the hospitalizations recorded between the non-service sector and the service sector. This would require some domain knowledge in order to interpret, but it is the opposite of what you would expect. Non-service jobs are traditionally "more dangerous" and would be believed to cause more injuries. What may be happening is that only the major injuries of the Service fields are being recorded and most of those require trips to the hospital. 

##Fact 2: Amputations

```r
c= sum(NonService$Amputation)/nrow(NonService)
d= sum(Service$Amputation)/nrow(Service)

Sector = c( 'NonService', 'Service' )
Percentage = c( c, d)

AmputationPercentage = data.frame( Sector, Percentage)
kable( AmputationPercentage)
```



Sector        Percentage
-----------  -----------
NonService     0.3395609
Service        0.1804630

This proportion makes sense to us. The nonService sectors are generally more dangerous, so it seems likely that they have the higher amputation rate. The amputation rates seem very high. This is probably due to the fact that amputations are something that must be reported, so it makes sense that they take up a large portion of our data. However, this may be a data quality issue if in reality our proportion on amputations is just being inflated because they must be reported.


##Fact 3: Top 10 Nature Titles for Service and Non Service  


```r
Top10Service = Service %>%
  group_by(NatureTitle) %>%
  summarize( countSer = n()) %>%
  filter(countSer > 190) %>%
  mutate(sector = "NonService") %>%
  mutate(NatureTitle = str_wrap(NatureTitle, width = 18)) 

p1 = ggplot(Top10Service, aes(reorder(NatureTitle, countSer), countSer))+geom_bar(stat= "identity", fill = "indianred4")+coord_flip()+ labs (y = "Count ", x = "Injury", title= "Service Sector")+ theme(axis.text.y = element_text(size=6)) 

Top10NonService = NonService %>%
  group_by(NatureTitle) %>%
  summarize( countNon = n()) %>%
  filter(countNon > 250) %>%
  mutate(sector = "NonService") %>%
  mutate(NatureTitle = str_wrap(NatureTitle, width = 18)) 

p2 = ggplot(Top10NonService, aes(reorder(NatureTitle, countNon), countNon))+geom_bar(stat= "identity", fill = 'royalblue4')+ labs (y = "Count ", x = "Injury", title = "Non-Service Sector")+coord_flip()+ theme(axis.text.y = element_text(size=6))

p1= ggplotly(p1)
p2= ggplotly(p2)
p = subplot( p1, p2, nrows = 1, margin = .08) %>% layout(title = "Service vs. Non Service Injuries")
p
```

<!--html_preserve--><div id="htmlwidget-836c3b32cae6d93305af" style="width:864px;height:576px;" class="plotly html-widget"></div>
<script type="application/json" data-for="htmlwidget-836c3b32cae6d93305af">{"x":{"data":[{"orientation":"h","width":[0.9,0.9,0.9,0.9,0.9,0.9,0.9,0.899999999999999,0.899999999999999,0.899999999999999],"base":[0,0,0,0,0,0,0,0,0,0],"x":[197,208,218,250,464,527,622,1485,2503,4812],"y":[1,2,3,4,5,6,7,8,9,10],"text":["reorder(NatureTitle, countSer): Internal injuries<br />to organs and<br />blood vessels of<br />the trunk<br />countSer:  197","reorder(NatureTitle, countSer): Heat (thermal)<br />burns, unspecified<br />countSer:  208","reorder(NatureTitle, countSer): Puncture wounds,<br />except gunshot<br />wounds<br />countSer:  218","reorder(NatureTitle, countSer): Crushing injuries<br />countSer:  250","reorder(NatureTitle, countSer): Intracranial<br />injuries,<br />unspecified<br />countSer:  464","reorder(NatureTitle, countSer): Traumatic injuries<br />and disorders,<br />unspecified<br />countSer:  527","reorder(NatureTitle, countSer): Cuts, lacerations<br />countSer:  622","reorder(NatureTitle, countSer): Soreness, pain,<br />hurt-nonspecified<br />injury<br />countSer: 1485","reorder(NatureTitle, countSer): Amputations<br />countSer: 2503","reorder(NatureTitle, countSer): Fractures<br />countSer: 4812"],"type":"bar","marker":{"autocolorscale":false,"color":"rgba(139,58,58,1)","line":{"width":1.88976377952756,"color":"transparent"}},"showlegend":false,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"orientation":"h","width":[0.9,0.9,0.9,0.9,0.9,0.9,0.9,0.899999999999999,0.899999999999999,0.899999999999999],"base":[0,0,0,0,0,0,0,0,0,0],"x":[285,291,296,392,426,587,933,1359,4553,6144],"y":[1,2,3,4,5,6,7,8,9,10],"text":["reorder(NatureTitle, countNon): Intracranial<br />injuries,<br />unspecified<br />countNon:  285","reorder(NatureTitle, countNon): Puncture wounds,<br />except gunshot<br />wounds<br />countNon:  291","reorder(NatureTitle, countNon): Internal injuries<br />to organs and<br />blood vessels of<br />the trunk<br />countNon:  296","reorder(NatureTitle, countNon): Heat (thermal)<br />burns, unspecified<br />countNon:  392","reorder(NatureTitle, countNon): Crushing injuries<br />countNon:  426","reorder(NatureTitle, countNon): Traumatic injuries<br />and disorders,<br />unspecified<br />countNon:  587","reorder(NatureTitle, countNon): Cuts, lacerations<br />countNon:  933","reorder(NatureTitle, countNon): Soreness, pain,<br />hurt-nonspecified<br />injury<br />countNon: 1359","reorder(NatureTitle, countNon): Fractures<br />countNon: 4553","reorder(NatureTitle, countNon): Amputations<br />countNon: 6144"],"type":"bar","marker":{"autocolorscale":false,"color":"rgba(39,64,139,1)","line":{"width":1.88976377952756,"color":"transparent"}},"showlegend":false,"xaxis":"x2","yaxis":"y2","hoverinfo":"text","frame":null}],"layout":{"xaxis":{"domain":[0,0.42],"automargin":true,"type":"linear","autorange":false,"range":[-240.6,5052.6],"tickmode":"array","ticktext":["0","1000","2000","3000","4000","5000"],"tickvals":[2.8421709430404e-14,1000,2000,3000,4000,5000],"categoryorder":"array","categoryarray":["0","1000","2000","3000","4000","5000"],"nticks":null,"ticks":"outside","tickcolor":"rgba(0,0,0,1)","ticklen":4.64923204649232,"tickwidth":0.66417600664176,"showticklabels":true,"tickfont":{"color":"rgba(0,0,0,1)","family":"","size":15.9375674553757},"tickangle":-0,"showline":true,"linecolor":"rgba(0,0,0,1)","linewidth":0.66417600664176,"showgrid":false,"gridcolor":null,"gridwidth":0,"zeroline":false,"anchor":"y","titlefont":{"color":"rgba(0,0,0,1)","family":"","size":18.5969281859693},"hoverformat":".2f"},"xaxis2":{"domain":[0.58,1],"automargin":true,"type":"linear","autorange":false,"range":[-307.2,6451.2],"tickmode":"array","ticktext":["0","2000","4000","6000"],"tickvals":[0,2000,4000,6000],"categoryorder":"array","categoryarray":["0","2000","4000","6000"],"nticks":null,"ticks":"outside","tickcolor":"rgba(0,0,0,1)","ticklen":4.64923204649232,"tickwidth":0.66417600664176,"showticklabels":true,"tickfont":{"color":"rgba(0,0,0,1)","family":"","size":15.9375674553757},"tickangle":-0,"showline":true,"linecolor":"rgba(0,0,0,1)","linewidth":0.66417600664176,"showgrid":false,"gridcolor":null,"gridwidth":0,"zeroline":false,"anchor":"y2","titlefont":{"color":"rgba(0,0,0,1)","family":"","size":18.5969281859693},"hoverformat":".2f"},"yaxis2":{"domain":[0,1],"automargin":true,"type":"linear","autorange":false,"range":[0.4,10.6],"tickmode":"array","ticktext":["Intracranial<br />injuries,<br />unspecified","Puncture wounds,<br />except gunshot<br />wounds","Internal injuries<br />to organs and<br />blood vessels of<br />the trunk","Heat (thermal)<br />burns, unspecified","Crushing injuries","Traumatic injuries<br />and disorders,<br />unspecified","Cuts, lacerations","Soreness, pain,<br />hurt-nonspecified<br />injury","Fractures","Amputations"],"tickvals":[1,2,3,4,5,6,7,8,9,10],"categoryorder":"array","categoryarray":["Intracranial<br />injuries,<br />unspecified","Puncture wounds,<br />except gunshot<br />wounds","Internal injuries<br />to organs and<br />blood vessels of<br />the trunk","Heat (thermal)<br />burns, unspecified","Crushing injuries","Traumatic injuries<br />and disorders,<br />unspecified","Cuts, lacerations","Soreness, pain,<br />hurt-nonspecified<br />injury","Fractures","Amputations"],"nticks":null,"ticks":"outside","tickcolor":"rgba(0,0,0,1)","ticklen":4.64923204649232,"tickwidth":0.66417600664176,"showticklabels":true,"tickfont":{"color":"rgba(0,0,0,1)","family":"","size":7.97011207970112},"tickangle":-0,"showline":true,"linecolor":"rgba(0,0,0,1)","linewidth":0.66417600664176,"showgrid":false,"gridcolor":null,"gridwidth":0,"zeroline":false,"anchor":"x2","titlefont":{"color":"rgba(0,0,0,1)","family":"","size":18.5969281859693},"hoverformat":".2f"},"yaxis":{"domain":[0,1],"automargin":true,"type":"linear","autorange":false,"range":[0.4,10.6],"tickmode":"array","ticktext":["Internal injuries<br />to organs and<br />blood vessels of<br />the trunk","Heat (thermal)<br />burns, unspecified","Puncture wounds,<br />except gunshot<br />wounds","Crushing injuries","Intracranial<br />injuries,<br />unspecified","Traumatic injuries<br />and disorders,<br />unspecified","Cuts, lacerations","Soreness, pain,<br />hurt-nonspecified<br />injury","Amputations","Fractures"],"tickvals":[1,2,3,4,5,6,7,8,9,10],"categoryorder":"array","categoryarray":["Internal injuries<br />to organs and<br />blood vessels of<br />the trunk","Heat (thermal)<br />burns, unspecified","Puncture wounds,<br />except gunshot<br />wounds","Crushing injuries","Intracranial<br />injuries,<br />unspecified","Traumatic injuries<br />and disorders,<br />unspecified","Cuts, lacerations","Soreness, pain,<br />hurt-nonspecified<br />injury","Amputations","Fractures"],"nticks":null,"ticks":"outside","tickcolor":"rgba(0,0,0,1)","ticklen":4.64923204649232,"tickwidth":0.66417600664176,"showticklabels":true,"tickfont":{"color":"rgba(0,0,0,1)","family":"","size":7.97011207970112},"tickangle":-0,"showline":true,"linecolor":"rgba(0,0,0,1)","linewidth":0.66417600664176,"showgrid":false,"gridcolor":null,"gridwidth":0,"zeroline":false,"anchor":"x","titlefont":{"color":"rgba(0,0,0,1)","family":"","size":18.5969281859693},"hoverformat":".2f"},"shapes":[{"type":"rect","fillcolor":null,"line":{"color":null,"width":0,"linetype":[]},"yref":"paper","xref":"paper","x0":0,"x1":0.42,"y0":0,"y1":1},{"type":"rect","fillcolor":null,"line":{"color":null,"width":0,"linetype":[]},"yref":"paper","xref":"paper","x0":0.58,"x1":1,"y0":0,"y1":1}],"margin":{"t":48.5446243254463,"r":9.29846409298464,"b":53.1314238273143,"l":263.677874636779},"font":{"color":"rgba(0,0,0,1)","family":"","size":18.5969281859693},"title":"Service vs. Non Service Injuries","titlefont":{"color":"rgba(0,0,0,1)","family":"","size":18.5969281859693},"showlegend":false,"legend":{"bgcolor":null,"bordercolor":null,"borderwidth":0,"font":{"color":"rgba(0,0,0,1)","family":"","size":15.9375674553757}},"hovermode":"closest","barmode":"relative"},"attrs":{"1651ee1afe68":{"x":{},"y":{},"type":"bar"},"1651e7ed00299":{"x":{},"y":{},"type":"bar"}},"source":"A","config":{"doubleClick":"reset","modeBarButtonsToAdd":[{"name":"Collaborate","icon":{"width":1000,"ascent":500,"descent":-50,"path":"M487 375c7-10 9-23 5-36l-79-259c-3-12-11-23-22-31-11-8-22-12-35-12l-263 0c-15 0-29 5-43 15-13 10-23 23-28 37-5 13-5 25-1 37 0 0 0 3 1 7 1 5 1 8 1 11 0 2 0 4-1 6 0 3-1 5-1 6 1 2 2 4 3 6 1 2 2 4 4 6 2 3 4 5 5 7 5 7 9 16 13 26 4 10 7 19 9 26 0 2 0 5 0 9-1 4-1 6 0 8 0 2 2 5 4 8 3 3 5 5 5 7 4 6 8 15 12 26 4 11 7 19 7 26 1 1 0 4 0 9-1 4-1 7 0 8 1 2 3 5 6 8 4 4 6 6 6 7 4 5 8 13 13 24 4 11 7 20 7 28 1 1 0 4 0 7-1 3-1 6-1 7 0 2 1 4 3 6 1 1 3 4 5 6 2 3 3 5 5 6 1 2 3 5 4 9 2 3 3 7 5 10 1 3 2 6 4 10 2 4 4 7 6 9 2 3 4 5 7 7 3 2 7 3 11 3 3 0 8 0 13-1l0-1c7 2 12 2 14 2l218 0c14 0 25-5 32-16 8-10 10-23 6-37l-79-259c-7-22-13-37-20-43-7-7-19-10-37-10l-248 0c-5 0-9-2-11-5-2-3-2-7 0-12 4-13 18-20 41-20l264 0c5 0 10 2 16 5 5 3 8 6 10 11l85 282c2 5 2 10 2 17 7-3 13-7 17-13z m-304 0c-1-3-1-5 0-7 1-1 3-2 6-2l174 0c2 0 4 1 7 2 2 2 4 4 5 7l6 18c0 3 0 5-1 7-1 1-3 2-6 2l-173 0c-3 0-5-1-8-2-2-2-4-4-4-7z m-24-73c-1-3-1-5 0-7 2-2 3-2 6-2l174 0c2 0 5 0 7 2 3 2 4 4 5 7l6 18c1 2 0 5-1 6-1 2-3 3-5 3l-174 0c-3 0-5-1-7-3-3-1-4-4-5-6z"},"click":"function(gd) { \n        // is this being viewed in RStudio?\n        if (location.search == '?viewer_pane=1') {\n          alert('To learn about plotly for collaboration, visit:\\n https://cpsievert.github.io/plotly_book/plot-ly-for-collaboration.html');\n        } else {\n          window.open('https://cpsievert.github.io/plotly_book/plot-ly-for-collaboration.html', '_blank');\n        }\n      }"}],"cloud":false},"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.2,"selected":{"opacity":1},"debounce":0},"subplot":true,"base_url":"https://plot.ly"},"evals":["config.modeBarButtonsToAdd.0.click"],"jsHooks":[]}</script><!--/html_preserve-->

The top categories are different in the Service and NonService sector. In the NonService amputations are the most common injury, but in the service sector fractures are the most common. Some other stand out differences are that internal injuries are more common to NonService and Heat injuries and burns are less common in the Service sector. It will be interesting to dive a little deeper into the exact body parts of the injuries and see if there are any more differences.

##Fact 4: Body Parts 

```r
Top10BodyPartService = Service %>%
  group_by(bodyPart) %>%
  summarize( count = n()) %>%
  filter(count >500) %>%
  mutate(bodyPart = str_wrap(bodyPart, width = 18)) 


p3= ggplot(Top10BodyPartService, aes(reorder(bodyPart, count), count))+geom_bar(stat= "identity", fill ="indianred4" )+coord_flip()+labs (y = "Count ", x = "Body Part", title = "Service Sector")+coord_flip()+ theme(axis.text.y = element_text(size=6))


Top10BodyPartNonService = NonService %>%
  group_by(bodyPart) %>%
  summarize( count = n()) %>%
  filter(count >500) %>% 
  mutate(bodyPart = str_wrap(bodyPart, width = 18)) 

p4= ggplot(Top10BodyPartNonService, aes(reorder(bodyPart, count), count))+geom_bar(stat= "identity", fill = 'royalblue4' )+coord_flip()+labs (y = "Count ", x = "Body Part", title = "Non-Service Sector")+coord_flip()+ theme(axis.text.y = element_text(size=6))

subplot( p3, p4, nrows = 1, margin = .08) %>%layout(title = "Service vs. Non Service Body Part")
```

<!--html_preserve--><div id="htmlwidget-7c4478012635545747a5" style="width:864px;height:576px;" class="plotly html-widget"></div>
<script type="application/json" data-for="htmlwidget-7c4478012635545747a5">{"x":{"data":[{"orientation":"h","width":[0.9,0.9,0.9,0.9,0.9,0.9,0.9,0.899999999999999,0.899999999999999,0.899999999999999],"base":[0,0,0,0,0,0,0,0,0,0],"x":[576,683,703,719,745,824,854,856,1074,1112],"y":[1,2,3,4,5,6,7,8,9,10],"text":["reorder(bodyPart, count): Finger(s),<br />fingernail(s),<br />unspecified<br />count:  576","reorder(bodyPart, count): Ankle(s)<br />count:  683","reorder(bodyPart, count): Brain<br />count:  703","reorder(bodyPart, count): Multiple body<br />parts, n.e.c.<br />count:  719","reorder(bodyPart, count): Leg(s),<br />unspecified<br />count:  745","reorder(bodyPart, count): Hip(s)<br />count:  824","reorder(bodyPart, count): Nonclassifiable<br />count:  854","reorder(bodyPart, count): BODY SYSTEMS<br />count:  856","reorder(bodyPart, count): Finger(s),<br />fingernail(s),<br />n.e.c.<br />count: 1074","reorder(bodyPart, count): Fingertip(s)<br />count: 1112"],"type":"bar","marker":{"autocolorscale":false,"color":"rgba(139,58,58,1)","line":{"width":1.88976377952756,"color":"transparent"}},"showlegend":false,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"orientation":"h","width":[0.9,0.9,0.9,0.9,0.9,0.9,0.9,0.899999999999999,0.899999999999999],"base":[0,0,0,0,0,0,0,0,0],"x":[513,573,727,747,1001,1044,1314,2588,2833],"y":[1,2,3,4,5,6,7,8,9],"text":["reorder(bodyPart, count): Chest, except<br />internal location<br />of diseases or<br />disorders<br />count:  513","reorder(bodyPart, count): Hand(s),<br />unspecified<br />count:  573","reorder(bodyPart, count): BODY SYSTEMS<br />count:  727","reorder(bodyPart, count): Leg(s),<br />unspecified<br />count:  747","reorder(bodyPart, count): Nonclassifiable<br />count: 1001","reorder(bodyPart, count): Multiple body<br />parts, n.e.c.<br />count: 1044","reorder(bodyPart, count): Finger(s),<br />fingernail(s),<br />unspecified<br />count: 1314","reorder(bodyPart, count): Fingertip(s)<br />count: 2588","reorder(bodyPart, count): Finger(s),<br />fingernail(s),<br />n.e.c.<br />count: 2833"],"type":"bar","marker":{"autocolorscale":false,"color":"rgba(39,64,139,1)","line":{"width":1.88976377952756,"color":"transparent"}},"showlegend":false,"xaxis":"x2","yaxis":"y2","hoverinfo":"text","frame":null}],"layout":{"xaxis":{"domain":[0,0.42],"automargin":true,"type":"linear","autorange":false,"range":[-55.6,1167.6],"tickmode":"array","ticktext":["0","300","600","900"],"tickvals":[0,300,600,900],"categoryorder":"array","categoryarray":["0","300","600","900"],"nticks":null,"ticks":"outside","tickcolor":"rgba(0,0,0,1)","ticklen":4.64923204649232,"tickwidth":0.66417600664176,"showticklabels":true,"tickfont":{"color":"rgba(0,0,0,1)","family":"","size":15.9375674553757},"tickangle":-0,"showline":true,"linecolor":"rgba(0,0,0,1)","linewidth":0.66417600664176,"showgrid":false,"gridcolor":null,"gridwidth":0,"zeroline":false,"anchor":"y","titlefont":{"color":"rgba(0,0,0,1)","family":"","size":18.5969281859693},"hoverformat":".2f"},"xaxis2":{"domain":[0.58,1],"automargin":true,"type":"linear","autorange":false,"range":[-141.65,2974.65],"tickmode":"array","ticktext":["0","1000","2000"],"tickvals":[0,1000,2000],"categoryorder":"array","categoryarray":["0","1000","2000"],"nticks":null,"ticks":"outside","tickcolor":"rgba(0,0,0,1)","ticklen":4.64923204649232,"tickwidth":0.66417600664176,"showticklabels":true,"tickfont":{"color":"rgba(0,0,0,1)","family":"","size":15.9375674553757},"tickangle":-0,"showline":true,"linecolor":"rgba(0,0,0,1)","linewidth":0.66417600664176,"showgrid":false,"gridcolor":null,"gridwidth":0,"zeroline":false,"anchor":"y2","titlefont":{"color":"rgba(0,0,0,1)","family":"","size":18.5969281859693},"hoverformat":".2f"},"yaxis2":{"domain":[0,1],"automargin":true,"type":"linear","autorange":false,"range":[0.4,9.6],"tickmode":"array","ticktext":["Chest, except<br />internal location<br />of diseases or<br />disorders","Hand(s),<br />unspecified","BODY SYSTEMS","Leg(s),<br />unspecified","Nonclassifiable","Multiple body<br />parts, n.e.c.","Finger(s),<br />fingernail(s),<br />unspecified","Fingertip(s)","Finger(s),<br />fingernail(s),<br />n.e.c."],"tickvals":[1,2,3,4,5,6,7,8,9],"categoryorder":"array","categoryarray":["Chest, except<br />internal location<br />of diseases or<br />disorders","Hand(s),<br />unspecified","BODY SYSTEMS","Leg(s),<br />unspecified","Nonclassifiable","Multiple body<br />parts, n.e.c.","Finger(s),<br />fingernail(s),<br />unspecified","Fingertip(s)","Finger(s),<br />fingernail(s),<br />n.e.c."],"nticks":null,"ticks":"outside","tickcolor":"rgba(0,0,0,1)","ticklen":4.64923204649232,"tickwidth":0.66417600664176,"showticklabels":true,"tickfont":{"color":"rgba(0,0,0,1)","family":"","size":7.97011207970112},"tickangle":-0,"showline":true,"linecolor":"rgba(0,0,0,1)","linewidth":0.66417600664176,"showgrid":false,"gridcolor":null,"gridwidth":0,"zeroline":false,"anchor":"x2","titlefont":{"color":"rgba(0,0,0,1)","family":"","size":18.5969281859693},"hoverformat":".2f"},"yaxis":{"domain":[0,1],"automargin":true,"type":"linear","autorange":false,"range":[0.4,10.6],"tickmode":"array","ticktext":["Finger(s),<br />fingernail(s),<br />unspecified","Ankle(s)","Brain","Multiple body<br />parts, n.e.c.","Leg(s),<br />unspecified","Hip(s)","Nonclassifiable","BODY SYSTEMS","Finger(s),<br />fingernail(s),<br />n.e.c.","Fingertip(s)"],"tickvals":[1,2,3,4,5,6,7,8,9,10],"categoryorder":"array","categoryarray":["Finger(s),<br />fingernail(s),<br />unspecified","Ankle(s)","Brain","Multiple body<br />parts, n.e.c.","Leg(s),<br />unspecified","Hip(s)","Nonclassifiable","BODY SYSTEMS","Finger(s),<br />fingernail(s),<br />n.e.c.","Fingertip(s)"],"nticks":null,"ticks":"outside","tickcolor":"rgba(0,0,0,1)","ticklen":4.64923204649232,"tickwidth":0.66417600664176,"showticklabels":true,"tickfont":{"color":"rgba(0,0,0,1)","family":"","size":7.97011207970112},"tickangle":-0,"showline":true,"linecolor":"rgba(0,0,0,1)","linewidth":0.66417600664176,"showgrid":false,"gridcolor":null,"gridwidth":0,"zeroline":false,"anchor":"x","titlefont":{"color":"rgba(0,0,0,1)","family":"","size":18.5969281859693},"hoverformat":".2f"},"shapes":[{"type":"rect","fillcolor":null,"line":{"color":null,"width":0,"linetype":[]},"yref":"paper","xref":"paper","x0":0,"x1":0.42,"y0":0,"y1":1},{"type":"rect","fillcolor":null,"line":{"color":null,"width":0,"linetype":[]},"yref":"paper","xref":"paper","x0":0.58,"x1":1,"y0":0,"y1":1}],"margin":{"t":48.5446243254463,"r":9.29846409298464,"b":53.1314238273143,"l":255.707762557078},"font":{"color":"rgba(0,0,0,1)","family":"","size":18.5969281859693},"title":"Service vs. Non Service Body Part","titlefont":{"color":"rgba(0,0,0,1)","family":"","size":18.5969281859693},"showlegend":false,"legend":{"bgcolor":null,"bordercolor":null,"borderwidth":0,"font":{"color":"rgba(0,0,0,1)","family":"","size":15.9375674553757}},"hovermode":"closest","barmode":"relative"},"attrs":{"1651e9e750dc":{"x":{},"y":{},"type":"bar"},"1651e24270d36":{"x":{},"y":{},"type":"bar"}},"source":"A","config":{"doubleClick":"reset","modeBarButtonsToAdd":[{"name":"Collaborate","icon":{"width":1000,"ascent":500,"descent":-50,"path":"M487 375c7-10 9-23 5-36l-79-259c-3-12-11-23-22-31-11-8-22-12-35-12l-263 0c-15 0-29 5-43 15-13 10-23 23-28 37-5 13-5 25-1 37 0 0 0 3 1 7 1 5 1 8 1 11 0 2 0 4-1 6 0 3-1 5-1 6 1 2 2 4 3 6 1 2 2 4 4 6 2 3 4 5 5 7 5 7 9 16 13 26 4 10 7 19 9 26 0 2 0 5 0 9-1 4-1 6 0 8 0 2 2 5 4 8 3 3 5 5 5 7 4 6 8 15 12 26 4 11 7 19 7 26 1 1 0 4 0 9-1 4-1 7 0 8 1 2 3 5 6 8 4 4 6 6 6 7 4 5 8 13 13 24 4 11 7 20 7 28 1 1 0 4 0 7-1 3-1 6-1 7 0 2 1 4 3 6 1 1 3 4 5 6 2 3 3 5 5 6 1 2 3 5 4 9 2 3 3 7 5 10 1 3 2 6 4 10 2 4 4 7 6 9 2 3 4 5 7 7 3 2 7 3 11 3 3 0 8 0 13-1l0-1c7 2 12 2 14 2l218 0c14 0 25-5 32-16 8-10 10-23 6-37l-79-259c-7-22-13-37-20-43-7-7-19-10-37-10l-248 0c-5 0-9-2-11-5-2-3-2-7 0-12 4-13 18-20 41-20l264 0c5 0 10 2 16 5 5 3 8 6 10 11l85 282c2 5 2 10 2 17 7-3 13-7 17-13z m-304 0c-1-3-1-5 0-7 1-1 3-2 6-2l174 0c2 0 4 1 7 2 2 2 4 4 5 7l6 18c0 3 0 5-1 7-1 1-3 2-6 2l-173 0c-3 0-5-1-8-2-2-2-4-4-4-7z m-24-73c-1-3-1-5 0-7 2-2 3-2 6-2l174 0c2 0 5 0 7 2 3 2 4 4 5 7l6 18c1 2 0 5-1 6-1 2-3 3-5 3l-174 0c-3 0-5-1-7-3-3-1-4-4-5-6z"},"click":"function(gd) { \n        // is this being viewed in RStudio?\n        if (location.search == '?viewer_pane=1') {\n          alert('To learn about plotly for collaboration, visit:\\n https://cpsievert.github.io/plotly_book/plot-ly-for-collaboration.html');\n        } else {\n          window.open('https://cpsievert.github.io/plotly_book/plot-ly-for-collaboration.html', '_blank');\n        }\n      }"}],"cloud":false},"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.2,"selected":{"opacity":1},"debounce":0},"subplot":true,"base_url":"https://plot.ly"},"evals":["config.modeBarButtonsToAdd.0.click"],"jsHooks":[]}</script><!--/html_preserve-->

Overwhelmingly fingers and finger tips are the most common body part injured. Most work involves your hands, so this falls in line with what one would expect. Hip injuries appear on Service, but not on NonService. This may be people in offices that are falling. Ankles and Brain also are high on the number of injuries for Service workers. Chest injuries are common in NonService injuries, but that does not show up in the top 10 for Service. There are some big differences in the body parts that are injured between the service and the non service sector. Hands, fingers, legs, and the chest are some of the most common for Non Service work. While for Service, fingers, Hips, Brain, and Ankles are some of the more common.


##Fact 5: Time Data
 
 Lets look and see if there are any points from the last 3 years that have extremely high points of data. 

```r
ServiceDate = Service %>%
  group_by(month,year) %>%
  filter( year < 2018) %>% 
  summarize(numInjuries = n()) %>%
  mutate(FullDate = paste(month,year, sep = "/"))

NonServiceDate = NonService %>%
  group_by(month,year) %>%
  filter( year < 2018) %>% 
  summarize(numInjuries = n()) %>%
  mutate(FullDate = paste(month,year, sep = "/"))

p=ggplot()+ geom_line(data=ServiceDate, aes(group=1,x= month,y= numInjuries), color = "indianred4")+ geom_line(data=NonServiceDate, aes(group=1,x= month,y= numInjuries), color = "royalblue4")+facet_wrap(~ year, ncol = 4)+ labs( title = "Number of Injuries over Time")
ggplotly(p)
```

<!--html_preserve--><div id="htmlwidget-784e35b90ad4dbbf55ff" style="width:864px;height:576px;" class="plotly html-widget"></div>
<script type="application/json" data-for="htmlwidget-784e35b90ad4dbbf55ff">{"x":{"data":[{"x":[1,2,3,4,5,6,7,8,9,10,11,12],"y":[356,359,345,323,345,345,420,352,378,317,316,320],"text":["month:  1<br />numInjuries: 356","month:  2<br />numInjuries: 359","month:  3<br />numInjuries: 345","month:  4<br />numInjuries: 323","month:  5<br />numInjuries: 345","month:  6<br />numInjuries: 345","month:  7<br />numInjuries: 420","month:  8<br />numInjuries: 352","month:  9<br />numInjuries: 378","month: 10<br />numInjuries: 317","month: 11<br />numInjuries: 316","month: 12<br />numInjuries: 320"],"type":"scatter","mode":"lines","line":{"width":1.88976377952756,"color":"rgba(139,58,58,1)","dash":"solid"},"hoveron":"points","showlegend":false,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":[1,2,3,4,5,6,7,8,9,10,11,12],"y":[360,348,347,339,321,391,428,445,362,365,357,345],"text":["month:  1<br />numInjuries: 360","month:  2<br />numInjuries: 348","month:  3<br />numInjuries: 347","month:  4<br />numInjuries: 339","month:  5<br />numInjuries: 321","month:  6<br />numInjuries: 391","month:  7<br />numInjuries: 428","month:  8<br />numInjuries: 445","month:  9<br />numInjuries: 362","month: 10<br />numInjuries: 365","month: 11<br />numInjuries: 357","month: 12<br />numInjuries: 345"],"type":"scatter","mode":"lines","line":{"width":1.88976377952756,"color":"rgba(139,58,58,1)","dash":"solid"},"hoveron":"points","showlegend":false,"xaxis":"x2","yaxis":"y","hoverinfo":"text","frame":null},{"x":[1,2,3,4,5,6,7,8,9,10,11,12],"y":[421,347,411,336,388,397,423,426,360,366,340,349],"text":["month:  1<br />numInjuries: 421","month:  2<br />numInjuries: 347","month:  3<br />numInjuries: 411","month:  4<br />numInjuries: 336","month:  5<br />numInjuries: 388","month:  6<br />numInjuries: 397","month:  7<br />numInjuries: 423","month:  8<br />numInjuries: 426","month:  9<br />numInjuries: 360","month: 10<br />numInjuries: 366","month: 11<br />numInjuries: 340","month: 12<br />numInjuries: 349"],"type":"scatter","mode":"lines","line":{"width":1.88976377952756,"color":"rgba(139,58,58,1)","dash":"solid"},"hoveron":"points","showlegend":false,"xaxis":"x3","yaxis":"y","hoverinfo":"text","frame":null},{"x":[1,2,3,4,5,6,7,8,9,10,11,12],"y":[496,404,503,476,447,479,522,488,507,479,417,418],"text":["month:  1<br />numInjuries: 496","month:  2<br />numInjuries: 404","month:  3<br />numInjuries: 503","month:  4<br />numInjuries: 476","month:  5<br />numInjuries: 447","month:  6<br />numInjuries: 479","month:  7<br />numInjuries: 522","month:  8<br />numInjuries: 488","month:  9<br />numInjuries: 507","month: 10<br />numInjuries: 479","month: 11<br />numInjuries: 417","month: 12<br />numInjuries: 418"],"type":"scatter","mode":"lines","line":{"width":1.88976377952756,"color":"rgba(39,64,139,1)","dash":"solid"},"hoveron":"points","showlegend":false,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":[1,2,3,4,5,6,7,8,9,10,11,12],"y":[447,447,408,415,412,527,510,619,526,484,444,398],"text":["month:  1<br />numInjuries: 447","month:  2<br />numInjuries: 447","month:  3<br />numInjuries: 408","month:  4<br />numInjuries: 415","month:  5<br />numInjuries: 412","month:  6<br />numInjuries: 527","month:  7<br />numInjuries: 510","month:  8<br />numInjuries: 619","month:  9<br />numInjuries: 526","month: 10<br />numInjuries: 484","month: 11<br />numInjuries: 444","month: 12<br />numInjuries: 398"],"type":"scatter","mode":"lines","line":{"width":1.88976377952756,"color":"rgba(39,64,139,1)","dash":"solid"},"hoveron":"points","showlegend":false,"xaxis":"x2","yaxis":"y","hoverinfo":"text","frame":null},{"x":[1,2,3,4,5,6,7,8,9,10,11,12],"y":[423,441,482,456,459,548,569,541,476,540,453,456],"text":["month:  1<br />numInjuries: 423","month:  2<br />numInjuries: 441","month:  3<br />numInjuries: 482","month:  4<br />numInjuries: 456","month:  5<br />numInjuries: 459","month:  6<br />numInjuries: 548","month:  7<br />numInjuries: 569","month:  8<br />numInjuries: 541","month:  9<br />numInjuries: 476","month: 10<br />numInjuries: 540","month: 11<br />numInjuries: 453","month: 12<br />numInjuries: 456"],"type":"scatter","mode":"lines","line":{"width":1.88976377952756,"color":"rgba(39,64,139,1)","dash":"solid"},"hoveron":"points","showlegend":false,"xaxis":"x3","yaxis":"y","hoverinfo":"text","frame":null}],"layout":{"margin":{"t":64.4821917808219,"r":9.29846409298464,"b":53.1314238273143,"l":56.4509755085098},"font":{"color":"rgba(0,0,0,1)","family":"","size":18.5969281859693},"title":"<b> Number of Injuries over Time <\/b>","titlefont":{"color":"rgba(0,0,0,1)","family":"","size":18.5969281859693},"xaxis":{"domain":[0,0.322571222114601],"automargin":true,"type":"linear","autorange":false,"range":[0.45,12.55],"tickmode":"array","ticktext":["2.5","5.0","7.5","10.0","12.5"],"tickvals":[2.5,5,7.5,10,12.5],"categoryorder":"array","categoryarray":["2.5","5.0","7.5","10.0","12.5"],"nticks":null,"ticks":"outside","tickcolor":"rgba(0,0,0,1)","ticklen":4.64923204649232,"tickwidth":0.66417600664176,"showticklabels":true,"tickfont":{"color":"rgba(0,0,0,1)","family":"","size":15.9375674553757},"tickangle":-0,"showline":true,"linecolor":"rgba(0,0,0,1)","linewidth":0.66417600664176,"showgrid":false,"gridcolor":null,"gridwidth":0,"zeroline":false,"anchor":"y","title":"","titlefont":{"color":"rgba(0,0,0,1)","family":"","size":18.5969281859693},"hoverformat":".2f"},"annotations":[{"text":"month","x":0.5,"y":-0.0518841381855081,"showarrow":false,"ax":0,"ay":0,"font":{"color":"rgba(0,0,0,1)","family":"","size":18.5969281859693},"xref":"paper","yref":"paper","textangle":-0,"xanchor":"center","yanchor":"top","annotationType":"axis"},{"text":"numInjuries","x":-0.0438125547714589,"y":0.5,"showarrow":false,"ax":0,"ay":0,"font":{"color":"rgba(0,0,0,1)","family":"","size":18.5969281859693},"xref":"paper","yref":"paper","textangle":-90,"xanchor":"right","yanchor":"center","annotationType":"axis"},{"text":"2015","x":0.161285611057301,"y":1,"showarrow":false,"ax":0,"ay":0,"font":{"color":"rgba(0,0,0,1)","family":"","size":15.9375674553757},"xref":"paper","yref":"paper","textangle":-0,"xanchor":"center","yanchor":"bottom"},{"text":"2016","x":0.5,"y":1,"showarrow":false,"ax":0,"ay":0,"font":{"color":"rgba(0,0,0,1)","family":"","size":15.9375674553757},"xref":"paper","yref":"paper","textangle":-0,"xanchor":"center","yanchor":"bottom"},{"text":"2017","x":0.838714388942699,"y":1,"showarrow":false,"ax":0,"ay":0,"font":{"color":"rgba(0,0,0,1)","family":"","size":15.9375674553757},"xref":"paper","yref":"paper","textangle":-0,"xanchor":"center","yanchor":"bottom"}],"yaxis":{"domain":[0,1],"automargin":true,"type":"linear","autorange":false,"range":[300.85,634.15],"tickmode":"array","ticktext":["400","500","600"],"tickvals":[400,500,600],"categoryorder":"array","categoryarray":["400","500","600"],"nticks":null,"ticks":"outside","tickcolor":"rgba(0,0,0,1)","ticklen":4.64923204649232,"tickwidth":0.66417600664176,"showticklabels":true,"tickfont":{"color":"rgba(0,0,0,1)","family":"","size":15.9375674553757},"tickangle":-0,"showline":true,"linecolor":"rgba(0,0,0,1)","linewidth":0.66417600664176,"showgrid":false,"gridcolor":null,"gridwidth":0,"zeroline":false,"anchor":"x","title":"","titlefont":{"color":"rgba(0,0,0,1)","family":"","size":18.5969281859693},"hoverformat":".2f"},"shapes":[{"type":"rect","fillcolor":null,"line":{"color":null,"width":0,"linetype":[]},"yref":"paper","xref":"paper","x0":0,"x1":0.322571222114601,"y0":0,"y1":1},{"type":"rect","fillcolor":"rgba(204,204,204,1)","line":{"color":"rgba(127,127,127,1)","width":0,"linetype":"none"},"yref":"paper","xref":"paper","x0":0,"x1":0.322571222114601,"y0":0,"y1":15.9375674553757,"yanchor":1,"ysizemode":"pixel"},{"type":"rect","fillcolor":null,"line":{"color":null,"width":0,"linetype":[]},"yref":"paper","xref":"paper","x0":0.344095444552066,"x1":0.655904555447934,"y0":0,"y1":1},{"type":"rect","fillcolor":"rgba(204,204,204,1)","line":{"color":"rgba(127,127,127,1)","width":0,"linetype":"none"},"yref":"paper","xref":"paper","x0":0.344095444552066,"x1":0.655904555447934,"y0":0,"y1":15.9375674553757,"yanchor":1,"ysizemode":"pixel"},{"type":"rect","fillcolor":null,"line":{"color":null,"width":0,"linetype":[]},"yref":"paper","xref":"paper","x0":0.677428777885399,"x1":1,"y0":0,"y1":1},{"type":"rect","fillcolor":"rgba(204,204,204,1)","line":{"color":"rgba(127,127,127,1)","width":0,"linetype":"none"},"yref":"paper","xref":"paper","x0":0.677428777885399,"x1":1,"y0":0,"y1":15.9375674553757,"yanchor":1,"ysizemode":"pixel"}],"xaxis2":{"type":"linear","autorange":false,"range":[0.45,12.55],"tickmode":"array","ticktext":["2.5","5.0","7.5","10.0","12.5"],"tickvals":[2.5,5,7.5,10,12.5],"categoryorder":"array","categoryarray":["2.5","5.0","7.5","10.0","12.5"],"nticks":null,"ticks":"outside","tickcolor":"rgba(0,0,0,1)","ticklen":4.64923204649232,"tickwidth":0.66417600664176,"showticklabels":true,"tickfont":{"color":"rgba(0,0,0,1)","family":"","size":15.9375674553757},"tickangle":-0,"showline":true,"linecolor":"rgba(0,0,0,1)","linewidth":0.66417600664176,"showgrid":false,"domain":[0.344095444552066,0.655904555447934],"gridcolor":null,"gridwidth":0,"zeroline":false,"anchor":"y","title":"","titlefont":{"color":"rgba(0,0,0,1)","family":"","size":18.5969281859693},"hoverformat":".2f"},"xaxis3":{"type":"linear","autorange":false,"range":[0.45,12.55],"tickmode":"array","ticktext":["2.5","5.0","7.5","10.0","12.5"],"tickvals":[2.5,5,7.5,10,12.5],"categoryorder":"array","categoryarray":["2.5","5.0","7.5","10.0","12.5"],"nticks":null,"ticks":"outside","tickcolor":"rgba(0,0,0,1)","ticklen":4.64923204649232,"tickwidth":0.66417600664176,"showticklabels":true,"tickfont":{"color":"rgba(0,0,0,1)","family":"","size":15.9375674553757},"tickangle":-0,"showline":true,"linecolor":"rgba(0,0,0,1)","linewidth":0.66417600664176,"showgrid":false,"domain":[0.677428777885399,1],"gridcolor":null,"gridwidth":0,"zeroline":false,"anchor":"y","title":"","titlefont":{"color":"rgba(0,0,0,1)","family":"","size":18.5969281859693},"hoverformat":".2f"},"showlegend":false,"legend":{"bgcolor":null,"bordercolor":null,"borderwidth":0,"font":{"color":"rgba(0,0,0,1)","family":"","size":15.9375674553757}},"hovermode":"closest","barmode":"relative"},"config":{"doubleClick":"reset","modeBarButtonsToAdd":[{"name":"Collaborate","icon":{"width":1000,"ascent":500,"descent":-50,"path":"M487 375c7-10 9-23 5-36l-79-259c-3-12-11-23-22-31-11-8-22-12-35-12l-263 0c-15 0-29 5-43 15-13 10-23 23-28 37-5 13-5 25-1 37 0 0 0 3 1 7 1 5 1 8 1 11 0 2 0 4-1 6 0 3-1 5-1 6 1 2 2 4 3 6 1 2 2 4 4 6 2 3 4 5 5 7 5 7 9 16 13 26 4 10 7 19 9 26 0 2 0 5 0 9-1 4-1 6 0 8 0 2 2 5 4 8 3 3 5 5 5 7 4 6 8 15 12 26 4 11 7 19 7 26 1 1 0 4 0 9-1 4-1 7 0 8 1 2 3 5 6 8 4 4 6 6 6 7 4 5 8 13 13 24 4 11 7 20 7 28 1 1 0 4 0 7-1 3-1 6-1 7 0 2 1 4 3 6 1 1 3 4 5 6 2 3 3 5 5 6 1 2 3 5 4 9 2 3 3 7 5 10 1 3 2 6 4 10 2 4 4 7 6 9 2 3 4 5 7 7 3 2 7 3 11 3 3 0 8 0 13-1l0-1c7 2 12 2 14 2l218 0c14 0 25-5 32-16 8-10 10-23 6-37l-79-259c-7-22-13-37-20-43-7-7-19-10-37-10l-248 0c-5 0-9-2-11-5-2-3-2-7 0-12 4-13 18-20 41-20l264 0c5 0 10 2 16 5 5 3 8 6 10 11l85 282c2 5 2 10 2 17 7-3 13-7 17-13z m-304 0c-1-3-1-5 0-7 1-1 3-2 6-2l174 0c2 0 4 1 7 2 2 2 4 4 5 7l6 18c0 3 0 5-1 7-1 1-3 2-6 2l-173 0c-3 0-5-1-8-2-2-2-4-4-4-7z m-24-73c-1-3-1-5 0-7 2-2 3-2 6-2l174 0c2 0 5 0 7 2 3 2 4 4 5 7l6 18c1 2 0 5-1 6-1 2-3 3-5 3l-174 0c-3 0-5-1-7-3-3-1-4-4-5-6z"},"click":"function(gd) { \n        // is this being viewed in RStudio?\n        if (location.search == '?viewer_pane=1') {\n          alert('To learn about plotly for collaboration, visit:\\n https://cpsievert.github.io/plotly_book/plot-ly-for-collaboration.html');\n        } else {\n          window.open('https://cpsievert.github.io/plotly_book/plot-ly-for-collaboration.html', '_blank');\n        }\n      }"}],"cloud":false},"source":"A","attrs":{"1651e4c0eef24":{"x":{},"y":{},"type":"scatter"},"1651e68764b7e":{"x":{},"y":{}}},"cur_data":"1651e4c0eef24","visdat":{"1651e4c0eef24":["function (y) ","x"],"1651e68764b7e":["function (y) ","x"]},"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.2,"selected":{"opacity":1},"debounce":0},"base_url":"https://plot.ly"},"evals":["config.modeBarButtonsToAdd.0.click"],"jsHooks":[]}</script><!--/html_preserve-->

Something interesting about this is they follow a very similar pattern. That really isn't something I would expect. I would have thought that they would be a bit more random, however the trends seem to follow each other. This makes me think there is some outside force that is making them do more reports that month. We seem to see spikes in September and October. One inital thought is that those are some of the hotter months, so there could be a rise in heat stokes and dehydration. I would like to follow up and get more information on what happened in August of 2016 to cause such a huge spike. 


###BONUS: What happened in August of 2016? 


```r
Top10ServiceAug2016 = Service %>%
  filter(month == 8, year == 2016) %>%
  group_by(NatureTitle) %>%
  summarize( countSer = n()) %>%
  filter(countSer>9)%>%
  mutate(NatureTitle = str_wrap(NatureTitle, width = 18)) 
  

p6 = ggplot(Top10ServiceAug2016, aes(reorder(NatureTitle, countSer), countSer))+geom_bar(stat= "identity", fill ="indianred4" )+coord_flip()+labs (y = "Count ", x = "Body Part", title = "Service Sector")+coord_flip()+ theme(axis.text.y = element_text(size=6))

Top10NonServiceAug2016 = NonService %>%
  filter(month == 8, year == 2016) %>%
  group_by(NatureTitle) %>%
  summarize( count = n()) %>%
  filter(count >13)%>%
  mutate(NatureTitle = str_wrap(NatureTitle, width = 18)) 
  

p7 = ggplot(Top10NonServiceAug2016, aes(reorder(NatureTitle, count), count))+geom_bar(stat= "identity" ,fill = "royalblue4")+coord_flip()+ labs( title = "Number of Injuries over Time")+ theme(axis.text.y = element_text(size=6))

subplot( p6, p7, margin = .06) %>%layout(title = "Aug. 2016 Injury Types")
```

<!--html_preserve--><div id="htmlwidget-408ece960330225597ce" style="width:864px;height:576px;" class="plotly html-widget"></div>
<script type="application/json" data-for="htmlwidget-408ece960330225597ce">{"x":{"data":[{"orientation":"h","width":[0.9,0.9,0.9,0.9,0.9,0.9,0.9],"base":[0,0,0,0,0,0,0],"x":[11,19,19,22,34,82,143],"y":[1,2,3,4,5,6,7],"text":["reorder(NatureTitle, countSer): Traumatic injuries<br />and disorders,<br />unspecified<br />countSer:  11","reorder(NatureTitle, countSer): Cuts, lacerations<br />countSer:  19","reorder(NatureTitle, countSer): Effects of heat<br />and light, n.e.c.<br />countSer:  19","reorder(NatureTitle, countSer): Effects of<br />heat and light,<br />unspecified<br />countSer:  22","reorder(NatureTitle, countSer): Soreness, pain,<br />hurt-nonspecified<br />injury<br />countSer:  34","reorder(NatureTitle, countSer): Amputations<br />countSer:  82","reorder(NatureTitle, countSer): Fractures<br />countSer: 143"],"type":"bar","marker":{"autocolorscale":false,"color":"rgba(139,58,58,1)","line":{"width":1.88976377952756,"color":"transparent"}},"showlegend":false,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"orientation":"h","width":[0.9,0.9,0.9,0.9,0.9,0.9,0.9,0.899999999999999],"base":[0,0,0,0,0,0,0,0],"x":[14,14,16,16,33,41,149,191],"y":[1,2,3,4,5,6,7,8],"text":["reorder(NatureTitle, count): Heat (thermal)<br />burns, unspecified<br />count:  14","reorder(NatureTitle, count): Internal injuries<br />to organs and<br />blood vessels of<br />the trunk<br />count:  14","reorder(NatureTitle, count): Effects of<br />heat and light,<br />unspecified<br />count:  16","reorder(NatureTitle, count): Heat exhaustion,<br />prostration<br />count:  16","reorder(NatureTitle, count): Cuts, lacerations<br />count:  33","reorder(NatureTitle, count): Soreness, pain,<br />hurt-nonspecified<br />injury<br />count:  41","reorder(NatureTitle, count): Fractures<br />count: 149","reorder(NatureTitle, count): Amputations<br />count: 191"],"type":"bar","marker":{"autocolorscale":false,"color":"rgba(39,64,139,1)","line":{"width":1.88976377952756,"color":"transparent"}},"showlegend":false,"xaxis":"x2","yaxis":"y2","hoverinfo":"text","frame":null}],"layout":{"xaxis":{"domain":[0,0.44],"automargin":true,"type":"linear","autorange":false,"range":[-7.15,150.15],"tickmode":"array","ticktext":["0","50","100","150"],"tickvals":[0,50,100,150],"categoryorder":"array","categoryarray":["0","50","100","150"],"nticks":null,"ticks":"outside","tickcolor":"rgba(0,0,0,1)","ticklen":4.64923204649232,"tickwidth":0.66417600664176,"showticklabels":true,"tickfont":{"color":"rgba(0,0,0,1)","family":"","size":15.9375674553757},"tickangle":-0,"showline":true,"linecolor":"rgba(0,0,0,1)","linewidth":0.66417600664176,"showgrid":false,"gridcolor":null,"gridwidth":0,"zeroline":false,"anchor":"y","titlefont":{"color":"rgba(0,0,0,1)","family":"","size":18.5969281859693},"hoverformat":".2f"},"xaxis2":{"domain":[0.56,1],"automargin":true,"type":"linear","autorange":false,"range":[-9.55,200.55],"tickmode":"array","ticktext":["0","50","100","150","200"],"tickvals":[0,50,100,150,200],"categoryorder":"array","categoryarray":["0","50","100","150","200"],"nticks":null,"ticks":"outside","tickcolor":"rgba(0,0,0,1)","ticklen":4.64923204649232,"tickwidth":0.66417600664176,"showticklabels":true,"tickfont":{"color":"rgba(0,0,0,1)","family":"","size":15.9375674553757},"tickangle":-0,"showline":true,"linecolor":"rgba(0,0,0,1)","linewidth":0.66417600664176,"showgrid":false,"gridcolor":null,"gridwidth":0,"zeroline":false,"anchor":"y2","titlefont":{"color":"rgba(0,0,0,1)","family":"","size":18.5969281859693},"hoverformat":".2f"},"yaxis2":{"domain":[0,1],"automargin":true,"type":"linear","autorange":false,"range":[0.4,8.6],"tickmode":"array","ticktext":["Heat (thermal)<br />burns, unspecified","Internal injuries<br />to organs and<br />blood vessels of<br />the trunk","Effects of<br />heat and light,<br />unspecified","Heat exhaustion,<br />prostration","Cuts, lacerations","Soreness, pain,<br />hurt-nonspecified<br />injury","Fractures","Amputations"],"tickvals":[1,2,3,4,5,6,7,8],"categoryorder":"array","categoryarray":["Heat (thermal)<br />burns, unspecified","Internal injuries<br />to organs and<br />blood vessels of<br />the trunk","Effects of<br />heat and light,<br />unspecified","Heat exhaustion,<br />prostration","Cuts, lacerations","Soreness, pain,<br />hurt-nonspecified<br />injury","Fractures","Amputations"],"nticks":null,"ticks":"outside","tickcolor":"rgba(0,0,0,1)","ticklen":4.64923204649232,"tickwidth":0.66417600664176,"showticklabels":true,"tickfont":{"color":"rgba(0,0,0,1)","family":"","size":7.97011207970112},"tickangle":-0,"showline":true,"linecolor":"rgba(0,0,0,1)","linewidth":0.66417600664176,"showgrid":false,"gridcolor":null,"gridwidth":0,"zeroline":false,"anchor":"x2","titlefont":{"color":"rgba(0,0,0,1)","family":"","size":18.5969281859693},"hoverformat":".2f"},"yaxis":{"domain":[0,1],"automargin":true,"type":"linear","autorange":false,"range":[0.4,7.6],"tickmode":"array","ticktext":["Traumatic injuries<br />and disorders,<br />unspecified","Cuts, lacerations","Effects of heat<br />and light, n.e.c.","Effects of<br />heat and light,<br />unspecified","Soreness, pain,<br />hurt-nonspecified<br />injury","Amputations","Fractures"],"tickvals":[1,2,3,4,5,6,7],"categoryorder":"array","categoryarray":["Traumatic injuries<br />and disorders,<br />unspecified","Cuts, lacerations","Effects of heat<br />and light, n.e.c.","Effects of<br />heat and light,<br />unspecified","Soreness, pain,<br />hurt-nonspecified<br />injury","Amputations","Fractures"],"nticks":null,"ticks":"outside","tickcolor":"rgba(0,0,0,1)","ticklen":4.64923204649232,"tickwidth":0.66417600664176,"showticklabels":true,"tickfont":{"color":"rgba(0,0,0,1)","family":"","size":7.97011207970112},"tickangle":-0,"showline":true,"linecolor":"rgba(0,0,0,1)","linewidth":0.66417600664176,"showgrid":false,"gridcolor":null,"gridwidth":0,"zeroline":false,"anchor":"x","titlefont":{"color":"rgba(0,0,0,1)","family":"","size":18.5969281859693},"hoverformat":".2f"},"shapes":[{"type":"rect","fillcolor":null,"line":{"color":null,"width":0,"linetype":[]},"yref":"paper","xref":"paper","x0":0,"x1":0.44,"y0":0,"y1":1},{"type":"rect","fillcolor":null,"line":{"color":null,"width":0,"linetype":[]},"yref":"paper","xref":"paper","x0":0.56,"x1":1,"y0":0,"y1":1}],"margin":{"t":48.5446243254463,"r":9.29846409298464,"b":53.1314238273143,"l":263.677874636779},"font":{"color":"rgba(0,0,0,1)","family":"","size":18.5969281859693},"title":"Aug. 2016 Injury Types","titlefont":{"color":"rgba(0,0,0,1)","family":"","size":18.5969281859693},"showlegend":false,"legend":{"bgcolor":null,"bordercolor":null,"borderwidth":0,"font":{"color":"rgba(0,0,0,1)","family":"","size":15.9375674553757}},"hovermode":"closest","barmode":"relative"},"attrs":{"1651e340fab0c":{"x":{},"y":{},"type":"bar"},"1651e151e8229":{"x":{},"y":{},"type":"bar"}},"source":"A","config":{"doubleClick":"reset","modeBarButtonsToAdd":[{"name":"Collaborate","icon":{"width":1000,"ascent":500,"descent":-50,"path":"M487 375c7-10 9-23 5-36l-79-259c-3-12-11-23-22-31-11-8-22-12-35-12l-263 0c-15 0-29 5-43 15-13 10-23 23-28 37-5 13-5 25-1 37 0 0 0 3 1 7 1 5 1 8 1 11 0 2 0 4-1 6 0 3-1 5-1 6 1 2 2 4 3 6 1 2 2 4 4 6 2 3 4 5 5 7 5 7 9 16 13 26 4 10 7 19 9 26 0 2 0 5 0 9-1 4-1 6 0 8 0 2 2 5 4 8 3 3 5 5 5 7 4 6 8 15 12 26 4 11 7 19 7 26 1 1 0 4 0 9-1 4-1 7 0 8 1 2 3 5 6 8 4 4 6 6 6 7 4 5 8 13 13 24 4 11 7 20 7 28 1 1 0 4 0 7-1 3-1 6-1 7 0 2 1 4 3 6 1 1 3 4 5 6 2 3 3 5 5 6 1 2 3 5 4 9 2 3 3 7 5 10 1 3 2 6 4 10 2 4 4 7 6 9 2 3 4 5 7 7 3 2 7 3 11 3 3 0 8 0 13-1l0-1c7 2 12 2 14 2l218 0c14 0 25-5 32-16 8-10 10-23 6-37l-79-259c-7-22-13-37-20-43-7-7-19-10-37-10l-248 0c-5 0-9-2-11-5-2-3-2-7 0-12 4-13 18-20 41-20l264 0c5 0 10 2 16 5 5 3 8 6 10 11l85 282c2 5 2 10 2 17 7-3 13-7 17-13z m-304 0c-1-3-1-5 0-7 1-1 3-2 6-2l174 0c2 0 4 1 7 2 2 2 4 4 5 7l6 18c0 3 0 5-1 7-1 1-3 2-6 2l-173 0c-3 0-5-1-8-2-2-2-4-4-4-7z m-24-73c-1-3-1-5 0-7 2-2 3-2 6-2l174 0c2 0 5 0 7 2 3 2 4 4 5 7l6 18c1 2 0 5-1 6-1 2-3 3-5 3l-174 0c-3 0-5-1-7-3-3-1-4-4-5-6z"},"click":"function(gd) { \n        // is this being viewed in RStudio?\n        if (location.search == '?viewer_pane=1') {\n          alert('To learn about plotly for collaboration, visit:\\n https://cpsievert.github.io/plotly_book/plot-ly-for-collaboration.html');\n        } else {\n          window.open('https://cpsievert.github.io/plotly_book/plot-ly-for-collaboration.html', '_blank');\n        }\n      }"}],"cloud":false},"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.2,"selected":{"opacity":1},"debounce":0},"subplot":true,"base_url":"https://plot.ly"},"evals":["config.modeBarButtonsToAdd.0.click"],"jsHooks":[]}</script><!--/html_preserve-->


We see a lot more heat and light injuries, I wonder if the temperature was extremely high that year in September? This would be a situation where outside weather data would provide more background.

##Fact 6: Are the most popular states for service injuries the same as the most popular states for non service injuires? 

To accomplish this, I decided it would be easier to rank the states by number of records. So, if a state is ranked 1 that means that state has the greatest number of records, or injuries. If a state is ranked in the 50s that means, there were hardly any injuries recorded. Surprisingly, there are some states like New Jersey that rank very high in amount of Service injuries (8), but it is ranked in the middle for NonService (18). The differences between where the states fall in the non service and service states probably has something to do with the job markets in those areas. 



```r
ServiceStates = Service %>%
  group_by(State) %>%
  summarise(ServiceRecords = n())
NonServiceStates = NonService %>%
  group_by(State)%>%
  summarise(NonServiceRecords = n())
StatesRecords = left_join(ServiceStates, NonServiceStates)
#View(StatesRecords)
```


```r
ServiceStates$ServiceRank= NA
ServiceStates$ServiceRank[order(-ServiceStates$ServiceRecords)]= 1:nrow(ServiceStates)

NonServiceStates$NonServiceRank= NA
NonServiceStates$NonServiceRank[order(-NonServiceStates$NonServiceRecords)]= 1:nrow(NonServiceStates)


StateRecords = left_join(ServiceStates, NonServiceStates)
```


```r
StateRecords = StateRecords %>%
  select(State,ServiceRank, NonServiceRank)
StateRecords = StateRecords[order(StateRecords$ServiceRank),]

StateRecordsDiff = StateRecords %>%
  mutate( difference = abs(ServiceRank-NonServiceRank))

kable(StateRecordsDiff)
```



State                       ServiceRank   NonServiceRank   difference
-------------------------  ------------  ---------------  -----------
TEXAS                                 1                1            0
FLORIDA                               2                2            0
PENNSYLVANIA                          3                4            1
OHIO                                  4                3            1
ILLINOIS                              5                5            0
NEW YORK                              6                8            2
GEORGIA                               7                6            1
NEW JERSEY                            8               18           10
COLORADO                              9               10            1
MISSOURI                             10               11            1
MASSACHUSETTS                        11               17            6
WISCONSIN                            12                7            5
LOUISIANA                            13               13            0
ALABAMA                              14                9            5
ARKANSAS                             15               12            3
KANSAS                               16               16            0
OKLAHOMA                             17               14            3
NEBRASKA                             18               19            1
MISSISSIPPI                          19               15            4
CONNECTICUT                          20               23            3
CALIFORNIA                           21               32           11
MAINE                                22               25            3
WEST VIRGINIA                        23               22            1
IDAHO                                24               21            3
NORTH DAKOTA                         25               20            5
NEW HAMPSHIRE                        26               27            1
MONTANA                              27               28            1
SOUTH DAKOTA                         28               24            4
DISTRICT OF COLUMBIA                 29               31            2
VIRGINIA                             30               29            1
RHODE ISLAND                         31               30            1
DELAWARE                             32               26            6
WASHINGTON                           33               33            0
TENNESSEE                            34               40            6
ARIZONA                              35               37            2
MARYLAND                             36               35            1
MICHIGAN                             37               51           14
NORTH CAROLINA                       38               36            2
NEW MEXICO                           39               46            7
OREGON                               40               38            2
UTAH                                 41               50            9
SOUTH CAROLINA                       42               39            3
MINNESOTA                            43               52            9
HAWAII                               44               41            3
INDIANA                              45               NA           NA
KENTUCKY                             46               34           12
IOWA                                 47               NA           NA
WYOMING                              48               45            3
ALASKA                               49               43            6
GUAM                                 50               42            8
NEVADA                               51               NA           NA
VERMONT                              52               NA           NA
NORTHERN MARIANA ISLANDS             53               48            5
AMERICAN SAMOA                       54               44           10
PUERTO RICO                          55               49            6


#ONET Data Fact 

We have barely touched the surface of this data set. It took us a long time to realize what was the best way to read/interpret it. We decided to only use the columns for title, SOC number, Element ID, Element Name, and Data Value. These are the values we know how to interpret right now, so these are what we will be using. For a basic fact, we wanted to see the top 5 jobs that involve working in extreme temperatures. 

##Import Data Set

```r
WorkContext <- read_excel("~/Downloads/Work Context.xlsx")

WorkContext = WorkContext %>%
  filter( WorkContext$`Scale Name` == "Context")
```

##Fact 7: Which Occupations have the highest Very Hot or Cold Temperatures? 


```r
HotCold = WorkContext %>%
  filter( `Element Name` == "Very Hot or Cold Temperatures")

Top5HotCold = HotCold %>%
  filter(`Data Value` >4.73) %>%
  select( Title, `Element Name`, `Data Value`)

Top5HotCold = Top5HotCold[order(Top5HotCold$`Data Value`),]
kable(Top5HotCold)
```



Title                                                Element Name                     Data Value
---------------------------------------------------  ------------------------------  -----------
Landscaping and Groundskeeping Workers               Very Hot or Cold Temperatures          4.74
Refractory Materials Repairers, Except Brickmasons   Very Hot or Cold Temperatures          4.74
Metal-Refining Furnace Operators and Tenders         Very Hot or Cold Temperatures          4.82
Sailors and Marine Oilers                            Very Hot or Cold Temperatures          4.93
Pourers and Casters, Metal                           Very Hot or Cold Temperatures          4.98


#Conclusion 

Splitting the injuries data set has led us to some very interesting conclusions. There is clearly a difference in the amount of injuries, nature of injuries, and parts of the body that they affect. Once we are able to dig deeper into our data, we will be able to come up with even more visualizations to showcase the differences between the service and non-service sector jobs and hopefully come up with our interactive dashboard to allow the user to narrow down into specific jobs they are interested in. 


