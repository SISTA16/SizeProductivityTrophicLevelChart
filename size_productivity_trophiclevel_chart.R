#V6
list.of.packages <- c("scales")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
library(scales)

drawfish<-function(type, demersal=F, pelagic=F, reef=F, deepwater=F, troph.groups.height){
  
  tr     <- troph.groups.height # height of troph: herb 0.57, omn 0.742, low 0.914, mid 1.086, top 1.258
  asp    <- 12/9 # aspect ratio of window 
  aspy    <- 1
  xoff   <- (1.43-0.57)/40  # x offset to position drawing in plot
  yoff   <- (1.43-0.57)/40  # y offset to position drawing in plot
  addoffx<-0
  addoffy<-0
  dmoffset<-0
  peloffset<-0.1
  reefoffset<-0
  
  if (type == "fusiform"){
    ys<-c(5,5.25,6,7,7,5.75,7.25,7.5,6.5,3.5,2.5,2.75,4,2.8,2.8,3.75,5)
    xs<-c(1,1,2,4,9,12.25,14.25,15.5,14.7,14.7,15.5,14.25,12.25,9,4,2,1)
    addoffx<-0.88
    addoffy<-tr-0.01
    lines(x=xs*xoff/asp+addoffx,y=ys*yoff+addoffy)
    points(x=4*xoff/asp+addoffx,y=5.5*yoff+addoffy,pch=16) # eye
    dmoffset<--0.01
    reefoffset<--0.01
    peloffset<-0.11
    
  }
  else if (type == "elongated"){
    ys<-c(4.8,6,6.5,6.5,5.5,6.5,6.75,5,3.25,3.5,4.5,3.75,3.75,3.75,4,4.8)
    xs<-c(1,2.25,4,13,16,18,19,18,19,18,16,13.5,4,3,2,1)
    addoffx<-0.85
    addoffy<-tr
    lines(x=xs*xoff/asp+addoffx,y=ys*yoff+addoffy) 
    points(x=3.5*xoff/asp+addoffx,y=5.5*yoff+addoffy,pch=16) # eye
    
    peloffset<-0.11
    
  }
  else if (type == "shortdeep"){
    ys<-c(4.4,4.7,5,7,8,8,5.5,7,5,3,4.5,2,2,3,4.4)
    xs<-c(1.2,1.4,1,2.5,4,7,10,12,11.5,12,10,7,4,2.5,1.2)
    addoffx<-0.90
    addoffy<-tr-0.01
    lines(x=xs*xoff/asp+addoffx,y=ys*yoff+addoffy) 
    points(x=4*xoff/asp+addoffx,y=6*yoff+addoffy,pch=16) # eye
    dmoffset<--0.02
    peloffset<-0.115
    reefoffset<--0.02
    bobbleoffset<-0.03
    bobbleoffsetx<-0.03
  }
  else if (type == "eel-like"){
    ys<-c(6.5,7.25,8.328,8.5,9,9,8.5,8,7,6,5,4,3,2.5,2.5,2.5,2.8,3.1,3.5,3.5,4,5,6,6.5,6.5,6.5)
    xs<-c(2.1,3,5,6.3,10.5,13,15,16,17,17.5,17.5,17,16,13,10,5,2.75,3.2,11.2,11.2,14.5,15.5,15.5,14.5,5,2.2)
    addoffx<-0.85
    addoffy<-tr-0.03
    
    lines(x=xs*xoff/asp+addoffx,y=ys*yoff+addoffy) 
    points(x=5*xoff/asp+addoffx,y=7.4*yoff+addoffy,pch=16) # eye
    dmoffset<--0.03
    reefoffset<--0.03
    peloffset<-0.115
    
  }
  else if (type == "ray"){
    ys<-c(5,5.75,7,8.7,8,5.75,4.75,4,2,1.5,3,4.25,5)
    xs<-c(1,2,3,6.5,10,11,19,11,10,6.5,3,2,1)
    addoffx<-0.87
    addoffy<-tr-0.02
    
    lines(x=xs*xoff/asp+addoffx,y=ys*yoff+addoffy) 
    points(x=3.5*xoff/asp+addoffx,y=4*yoff+addoffy,pch=16) # eye
    points(x=3.5*xoff/asp+addoffx,y=6*yoff+addoffy,pch=16) # eye
    dmoffset<--0.03
    peloffset<-0.115
    reefoffset<--0.03
    
  }
  else if (type == "shark"){
    ys<-c(3,4.3,5.8,6,7,6,4.7,6.5,4,3.2,4,3,3,2.7,3,3,3,3)
    xs<-c(1,2.75,6,7,9,8.8,15,19.3,16.5,17.2,15,10,10,2.7,4.5,2.7,2.2,1)
    addoffx<-0.85
    addoffy<-tr-0.01
    
    lines(x=xs*xoff/asp+addoffx,y=ys*yoff+addoffy) 
    points(x=3.5*xoff/asp+addoffx,y=4*yoff+addoffy,pch=16,cex=0.7) # eye
    peloffset<-0.10
    dmoffset<--0.01
    reefoffset<--0.01
    
  }
  
  else if (type == "seahorse"){
    ys<-c(8,8.5,10,9,8,6,4,1.5,0,1,0.8,1.25,2.75,4.5,7.5,7.5,8)
    xs<-c(6,7.4,8.5,10,10.5,10,10,11,10,8.25,9.6,10,9,8,8.5,7.5,6)
    addoffx<-0.87
    addoffy<-tr+0.02
    
    aspy    <- 1.5
    asp<-1.5
    lines(x=xs*xoff/asp+addoffx,y=ys*yoff/aspy+addoffy) 
    points(x=8.5*xoff/asp+addoffx,y=8.5*yoff/aspy+addoffy,pch=16,cex=0.7) # eye
    peloffset<-0.115
    dmoffset<--0.035
    reefoffset<--0.035
    bobbleoffset<--0.00
    bobbleoffsetx<--0.01
  }
  
  else if (type == "chimaera"){
    ys<-c(4.2,5.5,6,7,7.5,6.5,6,4.4,4,3.7,2.7,2,2,2.1,2.8,3.6,3.6,3.4,2.7,2.5,2.2,2.5,3,3.7,4.2,4.2)
    xs<-c(0.9,2.75,6,7,8.5,8,8.25,17,18.5,19.5,19.2,17.2,14,17,19,19,15,11,8,5.5,3,1.5,1,0.5,0.8,0.9)
    aspy    <- 1
    asp<-1.4
    addoffx<-0.86
    addoffy<-tr-0.01
    
    lines(x=xs*xoff/asp+addoffx,y=ys*yoff/aspy+addoffy) 
    points(x=3*xoff/asp+addoffx,y=4.5*yoff+addoffy,pch=16) # eye
    dmoffset<--0.03
    reefoffset<--0.03
    peloffset<-0.11
    
  }
  
  if (demersal==T){
    lines(x=c(1,18)*xoff/asp+0.85,y=c(2,2)*yoff+tr+dmoffset,lwd=2) # line for demersal
  } else if (pelagic==T){
    lines(x=c(1,18)*xoff/asp+0.85,y=c(2,2)*yoff+tr+peloffset,lwd=2) # line for pelagic
  }else if (reef==T){
    lines(x=c(1,18)*xoff/asp+0.85,y=c(2,2)*yoff+tr+dmoffset,lwd=2) # line for demersal
    lines(x=c(1,1)*xoff/asp+0.85,y=c(2,7)*yoff+tr+reefoffset,lwd=2) # vertical line for reef
  }
  if (deepwater==T){
    xs<-xs*xoff/asp+addoffx
    ys<-ys*yoff/aspy+addoffy
    
    ymin<-min(ys)
    ymax<-max(ys)
    
    xmin<-min(xs)
    xmax<-max(xs)
    linestep <- 0.01
    seqy<-seq(ymin,ymax,by=linestep)
    for (yy in seqy){
      lines(x=c(xmin,xmax),y=c(yy,yy),lwd=1,col=alpha("grey",0.6))
    }
    
  }
  
}


sptpreprocess<-function(file,order.age.file){
  
  # Read file with FishBase data on Phylogeny, Length, Resilience, and Troph
  Bdat         <- read.csv(file, header=T, dec=".", stringsAsFactors = FALSE)
  cat("Input file",basename(file), "read successfully\n")
  
  #PRE-PROCESSING
  # Define key parameters
  sp    <- Bdat$SpecCode
  res   <- Bdat$Resilience
  bw    <- Bdat$Wmax
  # Read Order ages
  if (missing(order.age.file) || is.na(order.age.file)){
    cat("Using default age-order file",basename("OrderAge.csv"),"\n")
    order.age.file = "OrderAge.csv"
  }else
    cat("Using user-provided age-order file",order.age.file,"\n")
  
  OAgeRaw     <- read.csv(order.age.file, header=T, dec=".", stringsAsFactors = FALSE)
  OOrders     <- unique(OAgeRaw$Order)
  OAgeU        <- vector()
  for(i in 1: length(OOrders)){
    OAgeU[i] <- min(OAgeRaw$CommonAncestor[OAgeRaw$Order==OOrders[i]])
  }
  OAge  <- vector()
  for(i in 1:length(sp)){
    OAge[i] <- OAgeU[OOrders==Bdat$Order[i]]
  }
  
  # fix known errors
  bw[sp==1248]    <- 6000
  bw[sp==27028]   <- 6400
  bw[sp==59949]   <- 3
  bw[sp==59953]   <- 3
  res[sp==53414]  <- "High"
  res[sp==54548]  <- "High"
  res[sp==13639]  <- "Medium"
  res[sp==147]    <- "Medium"
  res[sp==223]    <- "Medium"
  res[sp==7734]   <- "Low"
  
  #form factor
  ff <- vector()
  for(i in 1:length(sp)){
    ff[i] <- 10^(log10(Bdat$a[i])+1.358*(Bdat$b[i]-3))    
  }
  
  #body weight geometric means
  mean.log.bw <- mean(log10(bw))
  sd.log.bw   <- sd(log10(bw))
  ##Estimate size group for each species
  size  <- vector()
  sz.grp        <- c("Small","Medium","Large","Very large")
  for(i in 1:length(sp)) {
    if(log10(bw[i]) < (mean.log.bw-sd.log.bw)) {sz = sz.grp[1]} else {
      if(log10(bw[i]) < (mean.log.bw+sd.log.bw)) {sz = sz.grp[2]} else {
        if(log10(bw[i]) < (mean.log.bw+3*sd.log.bw)) {sz = sz.grp[3]} 
        else {sz = sz.grp[4]}}}
    size <- append(size,sz) 
  }
  cat("Size classified for all",length(sp),"species\n")
  
  ##Estimate trophic group for each species
  troph.groups<-c("Herbivores","Omnivores","Low-level predators","Mid-level predators","Top predators")
  # get observed trophs
  obs.troph <- vector()
  for(i in 1:length(sp)) {
    if(is.na(Bdat$DietTroph[i])==F && is.na(Bdat$FoodTroph[i])==F) {
      obs.troph[i] <- mean(c(Bdat$DietTroph[i],Bdat$FoodTroph[i]))
    } else {if(is.na(Bdat$DietTroph[i])==F) {
      obs.troph[i] <- Bdat$DietTroph[i]
    } else {obs.troph[i] <- Bdat$FoodTroph[i]} 
    }
  }
  # fixing known troph errors in FB
  obs.troph[sp==7569]  <- 2.05 
  obs.troph[sp==11942] <- 3
  obs.troph[sp==6238]  <- 3
  obs.troph[sp==6239]  <- 3
  obs.troph[sp==52632] <- 3
  obs.troph[sp==62262] <- 3
  obs.troph[sp==23546] <- 3.8
  obs.troph[sp==8718]  <- 4
  obs.troph[sp==1248]  <- 4.25
  
  
  observed.perc.num<-100-(length(which(is.na(obs.troph)))*100/length(sp))
  unobserved.perc.num<-(length(which(is.na(obs.troph)))*100/length(sp))
  observed.perc = format(observed.perc.num,digits=2)
  unobserved.perc = format(unobserved.perc.num,digits=2)
  
  cat("Observed trophic level was found for",observed.perc,"% of the species\n")
  
  # get predicted trophs for species that have none
  pred.troph <- vector()
  intercept  <- median(obs.troph[Bdat$MaxLengthTL<3],na.rm=T) # median of observed trophs for fish < 2 cm
  cat("Median of observed trophs for fish < 3 cm:",intercept,", n samples used =",length(obs.troph[is.na(obs.troph)==F & Bdat$MaxLengthTL<3]),"\n")
  cat("Assigning trophic level to",unobserved.perc,"% of the species\n")
  for(i in 1:length(sp)){
    if(is.na(obs.troph[i])==F) {pred.troph[i] <- NA; next} else { 
      gen  <- Bdat$Genus[i]
      subf <- Bdat$Subfamily[i]
      fam  <- Bdat$Family[i]
      ord  <- Bdat$Order[i]
      if(length(obs.troph[is.na(obs.troph)==F & Bdat$Genus==gen])>=1) {
        tr  <- obs.troph[is.na(obs.troph)==F & Bdat$Genus==gen]
        tl  <- Bdat$MaxLengthTL[is.na(obs.troph)==F & Bdat$Genus==gen]
        reg <- lm(tr ~ log10(tl) + 0 + offset(rep(intercept,length(tr))))
        slp <- reg$coefficients[1]
        pred.troph[i] <- intercept + slp * log10(Bdat$MaxLengthTL[i])
      } else if(length(obs.troph[is.na(obs.troph)==F & Bdat$Subfamily==subf])>=1) {
        tr   <- obs.troph[is.na(obs.troph)==F & Bdat$Subfamily==subf]
        tl   <- Bdat$MaxLengthTL[is.na(obs.troph)==F & Bdat$Subfamily==subf]
        reg  <- lm(tr ~ log10(tl) + 0 + offset(rep(intercept,length(tr))))
        slp  <- reg$coefficients[1]
        pred.troph[i] <- intercept + slp * log10(Bdat$MaxLengthTL[i])
      } else if(length(obs.troph[is.na(obs.troph)==F & Bdat$Family==fam])>=1) {
        tr   <- obs.troph[is.na(obs.troph)==F & Bdat$Family==fam]
        tl   <- Bdat$MaxLengthTL[is.na(obs.troph)==F & Bdat$Family==fam]
        reg  <- lm(tr ~ log10(tl) + 0 + offset(rep(intercept,length(tr))))
        slp  <- reg$coefficients[1]
        pred.troph[i] <- intercept + slp * log10(Bdat$MaxLengthTL[i]) 
      } else if(length(obs.troph[is.na(obs.troph)==F & Bdat$Order==ord])>=1) {
        tr   <- obs.troph[is.na(obs.troph)==F & Bdat$Order==ord]
        tl   <- Bdat$MaxLengthTL[is.na(obs.troph)==F & Bdat$Order==ord]
        reg  <- lm(tr ~ log10(tl) + 0 + offset(rep(intercept,length(tr))))
        slp  <- reg$coefficients[1]
        pred.troph[i] <- intercept + slp * log10(Bdat$MaxLengthTL[i]) 
      } else pred.troph[i] <-100 # to alert of Order without troph
    }
    if(pred.troph[i]<2) pred.troph[i] <- 2 
  }
  
  cat("Predicted trophic level was assigned to ",unobserved.perc,"% of the species\n")
  # combine observed and predicted trophs
  troph.grp  <- vector() 
  for(i in 1:length(Bdat$SpecCode)) {
    troph <- ifelse(is.na(obs.troph[i])==F,obs.troph[i],pred.troph[i])
    if(troph <= 2.2) {troph.grp[i] = "Herbivores"} else {
      if(troph <= 2.8) {troph.grp[i] = "Omnivores"} else {
        if(troph <= 3.8) {troph.grp[i] = "Low-level predators"} else {
          if(troph <= 4.2) {troph.grp[i] = "Mid-level predators"} else {
            troph.grp[i] = "Top predators"}}}}
  }
  cat("Trophic level classified for all",length(sp),"species\n")
  output <- list("res" = res, "sp"= sp, "ff" = ff,"size" = size, "troph.grp"=troph.grp, "Bdat"=Bdat, "troph.groups" = troph.groups, "sz.grp"=sz.grp, "order.age"=OAge)
  return(output)
  
}


sptchart<-function(file,order.age.file,sp.classes.df=NA,axes.labels=NA, save.plot=F, shapesTodisplayFile) {
  
  if (missing(sp.classes.df))
    sp.classes.df= NA
  if (missing(axes.labels))
    axes.labels= NA
  if (missing(order.age.file))
    order.age.file= NA
  if (missing(shapesTodisplayFile))
    shapesTodisplayFile= NA
  spt.result<-sptpreprocess(file,order.age.file)
  
  res = spt.result$res
  sp = spt.result$sp
  ff = spt.result$ff
  size = spt.result$size
  troph.grp = spt.result$troph.grp
  Bdat = spt.result$Bdat
  troph.groups = spt.result$troph.groups
  sz.grp = spt.result$sz.grp
  OAge = spt.result$order.age
  
  cat("Preprocessing finished correctly\n")
  #CHART PRODUCTION
  #USER INPUT 
  if (is.na(sp.classes.df)[1]){
    sp.class.names<-c("Myx","Pet","Hol","Ela","Cla","Act","Coe","Dip")
    sp.class.codes<-c(1,2,3,4,9,6,10,11)
  }else{
    sp.class.names<-as.character(sp.classes.df$sp.class.names)
    sp.class.codes<-as.numeric(sp.classes.df$sp.class.codes)
  }
  
  if (is.na(axes.labels)[1]){
    chart.col.names<-c("High","Medium","Low","Very low")
    chart.row.names<-c("Small","Medium","Large","Very large")
  }else{
    chart.col.names<-as.character(axes.labels$productivity.labels)
    chart.row.names<-as.character(axes.labels$size.labels)
  }
  
  cat("User input was correctly parsed\n")
  
  #Fixed chart parameters
  outer.label.x = "Size"
  outer.label.y = "Productivity"
  trophic.level.colours=c("forestgreen","chartreuse1","yellow","orange","red")
  n.trophic.levels=length(trophic.level.colours)
  cw <- 2000 # number of species in cell width
  res.grp       <- c("High","Medium","Low","Very low")
  
  cat("Counting n. of species per resilience vs size and form factor stats\n")
  # 1 - get number of species per size group and resilience group
  res.count  <- vector()
  sz.count   <- vector()
  res.median    <- vector()
  sz.median <- vector()
  res.q5 <- vector()
  sz.q5 <- vector()
  res.q95 <- vector()
  sz.q95 <- vector()
  
  
  for(i in 1:4){
    res.count[i] <- length(res[res==res.grp[i]])
    sz.count[i] <- length(size[size==sz.grp[i]])
    res.median [i] <- median(ff[res==res.grp[i]],na.rm=T)
    sz.median [i] <- median(ff[size==sz.grp[i]],na.rm=T)
    res.q5 [i] <- quantile(ff[res==res.grp[i]],na.rm=T,0.05)
    sz.q5 [i] <- quantile(ff[size==sz.grp[i]],na.rm=T,0.05)
    res.q95 [i] <- quantile(ff[res==res.grp[i]],na.rm=T,0.95)
    sz.q95 [i] <- quantile(ff[size==sz.grp[i]],na.rm=T,0.95)
  }
  
  # 2 - get number of species, age, and class, per resilience vs size group
  res.sz.counts <- matrix(nrow=4,ncol = 4)
  res.sz.age    <- matrix(nrow=4,ncol = 4)
  res.sz <- array(rep(0, 4*4*5), dim=c(4, 4, 5))
  res.sz.class <- array(rep(0, 4*4*length(sp.class.codes)), dim=c(4, 4, length(sp.class.codes)))
  for(i in sz.grp) {
    for(j in res.grp) {
      row.idx = which(res.grp==j)
      col.idx = which(sz.grp==i)
      res.sz.counts[row.idx,col.idx] <- length(Bdat$SpecCode[size==i & res==j]) 
      res.sz.age[row.idx,col.idx] <- median(OAge[size==i & res==j])
      for(z in troph.groups) {
        z.idx = which(troph.groups==z)
        res.sz[row.idx,col.idx,z.idx]<-length(Bdat$SpecCode[size==i & res==j & troph.grp==z])
      }
      for(c in sp.class.codes) {
        c.idx = which(sp.class.codes==c)
        res.sz.class[row.idx,col.idx,c.idx]<-length(Bdat$SpecCode[size==i & res==j & Bdat$ClassNum==c])
      }
    }
  }
  
  cat("Producing the chart\n")
  # 3 - create the biodiv plot structure
  windows(12,9)
  par(oma=c(5,6,2,2),mar=c(0,0,0,0),mfrow=c(4,4)) # defines bottom and left outer margins, no inner margins, 4*4 panels
  
  shapes2display<-data.frame()
  #read shapes to display file
  if (!is.na(shapesTodisplayFile) ){
    if (file.exists(shapesTodisplayFile)) {
    shapes2display<-read.csv(shapesTodisplayFile,sep=",")
    cat("Species to display file provided\n")
    }
  }

  # 4 - plot the chart
  #i = productivity; j = size; z = trophic level
  troph.groups.heights<-c( 0.57,  0.742,  0.914,  1.086, 1.258)
  for(i in 1:4) { #cycle through productivity - from Very low to High
    for(j in 1:4) { #cycle through size
      plot(1,1,ylab="",xlab="",type="n",xaxt="n",yaxt="n")
      for(z in n.trophic.levels:1) {
        res.sz.z=res.sz[i,j,z]
        rect(xleft=1-((0.86/cw)*res.sz.z)/2,
             ybottom=1.258-(0.172*(n.trophic.levels-z)),
             xright=1+((0.86/cw)*res.sz.z)/2,
             ytop=1.43-(0.172*(n.trophic.levels-z)),
             col=trophic.level.colours[z],border=NA)
        if(res.sz.z>cw) {
          lines(x=c(0.57,0.57),y=c(0.914,1.086))
          text(x=1,y=1,paste("<             ",res.sz.z,"             >"))
        }
        
        curr.res.grp<-res.grp[length(res.grp)-i+1]
        curr.sz.group<-sz.grp[j]
        curr.trop.group<-troph.groups[z]
        
        if (dim(shapes2display)[1]>0){ #if there are shapes to display, check if the current combination is suitable
          s2d<-shapes2display[tolower(shapes2display$productivity)==tolower(curr.res.grp) & 
                              tolower(shapes2display$size)==tolower(curr.sz.group) & 
                              tolower(shapes2display$trophic_level)==tolower(curr.trop.group),]
        
        if (dim(s2d)[1]>0 ){
          #cat("Found shape to display",curr.res.grp,curr.sz.group,curr.trop.group,"\n")
          #print(s2d)
          demersal = s2d[1,]$demersal=="Y"
          pelagic = s2d[1,]$pelagic=="Y"
          reef = s2d[1,]$reef=="Y"
          deepwater = s2d[1,]$deepwater=="Y"
          type = s2d[1,]$shape
          drawfish(type = type,demersal = demersal, pelagic = pelagic, reef = reef, deepwater = deepwater, troph.groups.height = troph.groups.heights[z])
          }
        }
      }
      
      legend("topright",
             legend=paste0("N.sp:",res.sz.counts[i,j],"\n",
                           ifelse(is.na(res.sz.age[i,j])==F,paste("Med.age:",format(res.sz.age[i,j],digits=0)),"")),box.lty=0)
      
      class.dat <- data.frame(class=sp.class.names,
                              n=res.sz.class[i,j,],
                              stringsAsFactors = FALSE)
      class.dat <- class.dat[class.dat$n>0,]
      class.dat <- class.dat[order(class.dat$n,decreasing = T),]
      class.str <- paste(class.dat$class,collapse = ";")
      legend("bottomleft",class.str,box.lty=0)
    }
    
    mtext(text=chart.row.names[i],at=0.12+(0.25*(i-1)),side=1,line=2,outer=TRUE)
    mtext(text=chart.col.names[i],at=0.12+(0.25*(i-1)),side=2,line=2,outer=TRUE)
    
    sz.pos = 0.22+(0.25*(i-1))
    res.pos = 0.22+(0.25*(i-1))
    mtext(text=paste0("Tot:",sz.count[i]),at=sz.pos,side=1,line=0,outer=TRUE,cex=0.7)
    mtext(text=paste0("Tot:",res.count[length(res.grp)-i+1]),at=res.pos,side=2,line=0,outer=TRUE,cex=0.7)
    resStats = paste0("Med:",
                      format(res.median[length(res.grp)-i+1],digits=2),
                      " [",format(res.q5[length(res.grp)-i+1],digits=1),";",
                      format(res.q95[length(res.grp)-i+1],digits=1),"]")
    mtext(text=resStats,at=res.pos-0.12,side=2,line=0,outer=TRUE,cex=0.7)
    sizeStats = paste0("Med:",
                       format(sz.median[i],digits=2),
                       " [",format(sz.q5[i],digits=1),";",
                       format(sz.q95[i],digits=1),"]")
    mtext(text=sizeStats,at=sz.pos-0.15,side=1,line=0,outer=TRUE,cex=0.7)
  }
  
  # Add chart labels
  mtext(text=outer.label.x,side=1,line=4,outer=TRUE,cex=1.3)
  mtext(text=outer.label.y,side=2,line=4,outer=TRUE,cex=1.3)
  
  #save analytic chart to JPEG file
  if (save.plot==TRUE) 
  {
    jpgfile<-paste(gsub(basename(file),pattern = ".csv",replacement = ""),"_spt.jpg",sep="")
    
    dev.copy(jpeg,jpgfile,
             width = 1024, 
             height = 768, 
             units = "px", 
             pointsize = 18,
             quality = 95,
             res=80,
             antialias="default")
    dev.off()
    cat("Chart saved to",basename(jpgfile),"\n")
  }
  
  cat("Size-productivity-trophic level plot produced.\n")
  
}