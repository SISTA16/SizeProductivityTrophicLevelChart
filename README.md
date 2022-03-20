# Size Productivity Trophic Level Chart #

```
source("size_productivity_trophiclevel_chart.R")
setwd("./")

#Default value:
  #sp.classes.df = data.frame(sp.class.names = c("Myx","Pet","Hol","Ela","Cla","Act","Coe","Dip"), sp.class.codes = c(1,2,3,4,9,6,10,11))
  #axes.labels = data.frame(productivity.labels<-c("High","Medium","Low","Very-low"),size.labels<-c("Small","Medium","Large","Very large"))
  
#input files
file = "./BioDiv_14.csv"

#example 1 - produce the chart with default labels and don't save the plot (default)
sptchart(file=file)

#example 2 - produce the chart with default labels and save the plot
sptchart(file=file,save.plot = T)


#set custom labels and codes:
  sp.classes.df = data.frame(sp.class.names = c("My","Pe","Ho","El","Cl","Ac","Co","Di"), sp.class.codes = c(1,2,3,4,9,6,10,11))
  axes.labels = data.frame(productivity.labels<-c("H","M","L","VL"),size.labels<-c("S","M","L","VL"))
  
#example 3 - produce the chart with custom class labels
sptchart(file=file,sp.classes.df=sp.classes.df)

#example 4 - produce the chart with custom labels and save the plot
sptchart(file=file,sp.classes.df=sp.classes.df,axes.labels=axes.labels,save.plot = T)

#example 5 - produce the chart with custom axes labels
sptchart(file=file,axes.labels=axes.labels)

order.age.file = "./OrderAge.csv"
#example 6 - produce the chart using custom order-age file
sptchart(file=file,order.age.file=order.age.file)
```
