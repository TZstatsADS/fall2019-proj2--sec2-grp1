# Project 2: Shiny App Development Version 2.0

### [Project Description](doc/project2_desc.md)


In this second project of GR5243 Applied Data Science, we develop a version 2.0 of an *Exploratory Data Analysis and Visualization* shiny app on a topic of your choice using [NYC Open Data](https://opendata.cityofnewyork.us/) or U.S. government open data released on the [data.gov](https://data.gov/) website. See [Project 2 Description](doc/project2_desc.md) for more details.  

The **learning goals** for this project is:

- business intelligence for data science
- study legacy codes and further development
- data cleaning
- data visualization
- systems development/design life cycle
- shiny app/shiny server


## Project Title NYC Restaurant Inspection Shiny App 
Term: Fall 2019

+ Team # Group 1
+ **Projec title**: + Team members
	+ team member 1  Gao, Qiwen qg2165@columbia.edu
  + team member 2  Guo, Xudong xg2305@columbia.edu
  + team member 3  Hu, Hang hh2718@columbia.edu
  + team member 4  Yang, Siyu sy2796@columbia.edu
  + team member 5  Zhao, Nuanjun nz2295@columbia.edu
                  


+ **Project summary**: In this Shiny App Project, our group developed a shiny app about the restaurant inspection in New York city. Nowadays, when people look for somewhere to hang out or have a dinner, the most frequently used APP is yelp, in which people can refer to ratings, locations and comments from others. However, the ratings and comments of the restaurants that people could see on yelp are mostly based on flavor of food, quality of service and environment of the restaurant. But what about the food quality? Is the water potable? Is the kitchen appropriately cleaned? In fact these are also important elements for customers to decide whether they should go to the restaurant or not. Inspired by such kind of user demand and the yelp app, our group developed the nyc restaurant inspection shiny app, which shows the inspection results of the nyc restaurants on map, compares the scores of different restaurants and makes recommendations. There are 5 tabs in the shiny app, including Zipcode Map Visualization, Borough Map Visualization, Score Distribution, Top recommended restaurants and Contact us. The screenshots of each tab are shown below. 

The data set we used in this project is the 'New York City Restaurant Inspection Results' provided by Department of Health and Mental Hygiene, and it can be found on the NYC Open Data website. There are about 250 thousand records of restaurant inspections up to September 28 in 2019 in our data set after data cleaning, and each record includes details of the name, location, inspection date, violation code, letter grade and numerical score of that specific restaurant.

![Tab1](https://github.com/TZstatsADS/fall2019-proj2--sec2-grp1/blob/master/lib/Tab3.png)
![Tab2](https://github.com/TZstatsADS/fall2019-proj2--sec2-grp1/blob/master/lib/Tab4.png)
![Tab3](https://github.com/TZstatsADS/fall2019-proj2--sec2-grp1/blob/master/lib/Tab1.png)
![Tab4](https://github.com/TZstatsADS/fall2019-proj2--sec2-grp1/blob/master/lib/Tab2.png)
![Tab5](https://github.com/TZstatsADS/fall2019-proj2--sec2-grp1/blob/master/lib/Tab5.png)

+ **App Link** [Link](https://proj2.shinyapps.io/Nycrestaurant_inspection/)


+ **Contribution statement**: 

Data acquisition: Nuanjun Zhao, Qiwen Gao, Siyu Yang, Hang Hu, Xudong Guo

Data manipulation: Nuanjun Zhao, Qiwen Gao, Siyu Yang, Hang Hu

Data integration: Nuanjun Zhao

Tab1-Zipcode map visualization: Xudong Guo

Tab2-Borough map visualization:Siyu Yang

Tab3-Score comparison: Nuanjun Zhao, Hang Hu

Tab4-Restaurant recommendation: Nuanjun Zhao,Hang Hu

Tab5-Contact: Xudong Guo

Code integration: Nuanjun Zhao, Siyu Yang, Hang Hu, Xudong Guo

Presentation:Qiwen Gao

Github arrangement: Nuanjun Zhao, Qiwen Gao,Siyu Yang, Hang Hu

Following [suggestions](http://nicercode.github.io/blog/2013-04-05-projects/) by [RICH FITZJOHN](http://nicercode.github.io/about/#Team) (@richfitz). This folder is orgarnized as follows.

```
proj/
_________ app/
_________ lib/
_________ data/
_________ doc/
_________ output/
```

Please see each subfolder for a README file.

