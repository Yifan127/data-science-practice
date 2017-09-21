# Code Efficiency
# Q1
# count total number of lines given a set of files
linesFromFiles = function(files){
  count = 0
  for(i in 1:length(files)){
    nlines = 0
    temp = readLines(files[i])
    nlines = length(temp)
    count = count + nlines - 1
  }
  count
}

# nline = as.integer(system(paste("wc -l", files[i], "| awk -e '{print($1)}'"), intern = TRUE))

# Q2
# get the name of all csv file in the target folder
filenames = list.files(path = "D:/UoA/STATS769/Lab/Lab4/BUSDATA", pattern = "*.csv")
# full path of all the csv files
allfiles = paste0("D:/UoA/STATS769/Lab/Lab4/BUSDATA/", filenames)

# measure time and memory
# nfile: number of files 
measure = function(allfiles, nfile, FUN){
  funname = as.character(substitute(FUN))
  # get file and count linesd
  files = head(allfiles, nfile)
  nlines = linesFromFiles(files)
  
  # before reading
  gc.old = gc(reset = TRUE)
  ptm.old = proc.time()
  
  buffer = data.frame()
  if(funname == "manyFiles"){
    buffer = FUN(files)
  }
  if(funname == "singleFile" | funname == "singleDT"){
    buffer = FUN(nlines)
  }
  # print(buffer)
  # after reading
  # calculate differenc and remove the object = gc() - gc.old
  gc.result = gc() - gc.old
  ptm.result = proc.time() - ptm.old
  
  #save result into dataframe for plot
  df = data.frame(funname = funname, nlines = nlines, used.memory = gc.result[2], 
                  max.memory =  gc.result[10], time = ptm.result[1])
  
  # clean up
  rm(buffer)
  invisible(gc(reset = TRUE))
  df
}

# measure time and memory cost for single and many functions
# manyfiles
n = seq(100, 500, 50)
m = vector("list", 10)
for(i in n){
  m[[i]] = measure(allfiles, i, manyFiles)
}
many = do.call(rbind, m)
# singlefile
s = vector("list", 10)
for(i in n) {
  s[[i]] = measure(allfiles, i, singleFile)
}
single = do.call(rbind, s)

# fake data for debuging
nlines = 1:10* 1000
mgc = c(186, 189, 189, 189, 199, 184, 174, 224, 199, 169)
mmgc = c(10.4, 9.8, 15.9, 14.8, 14.8, 14.8, 14.8, 14.8, 14.8, 14.8)
mtime = 1.0:10.0*7
sgc = rep(150, 10)
smgc = c(6.1, 10.6, 14.9, 14.8, 14.8, 14.8, 14.8, 21.8, 21.8, 21.8)
stime = seq(0.1, 1, 0.1)

# ----------------plot ----------------------
library(ggplot2)
many = data.frame(fun = "Many Files", nlines = nlines, used.memory = mgc, max.memory = mmgc, time = mtime)
single = data.frame(fun = "Single File", nlines = nlines, used.memory = sgc, max.memory = smgc, time = stime)
df = merge(many, single, all = TRUE)

plotmeasure = function(df, title, ylab, y){
  ggplot(data = df, aes_string(x = "nlines", y = y, group = "funname", color = "funname")) +
    geom_line(aes(linetype = funname), size = 1.5) + 
    geom_point(aes(shape = funname), size = 3) +
    scale_shape_manual(values = c(15, 16)) +
    scale_color_manual(values = c("blue", "red")) +
    labs(title = title, x = "Number of Lines", y = ylab) +
    theme_classic()+
    theme(plot.title = element_text(size = 18, face="bold", hjust = 0.5),
          axis.title = element_text(size = 12, face="bold"),
          legend.position="top",
          legend.title = element_blank())
}

plotmeasure(df, "Time Measurement", "Time (second)", "time")
plotmeasure(df, "Max used Memory Measurement", "Memory (KB)", "max.memory")
# remove outlier
used.df = df[df$used.memory <= 500, ]
plotmeasure(used.df, "Used Memory Measurement", "Memory (KB)", "used.memory")
  

# Q2: profvis
library(profvis)
source("many-files.R")
files = head(allfiles, 100)
pmany = profvis(manyFiles(files))
htmlwidgets::saveWidget(pmany, "profile_many.html")

profvis({
    routes <- read.csv("/course/AT/routes.txt", stringsAsFactors=FALSE)
    routes$routeID <- gsub("-.+", "", routes$route_id)
    agencies <- read.csv("/course/AT/agency.txt", stringsAsFactors=FALSE)
    agencySums <- numeric(nrow(agencies))
    names(agencySums) <- agencies$agency_name
    agencyDenoms <- numeric(nrow(agencies))
    names(agencyDenoms) <- agencies$agency_name
    for (i in files) {
      file <- read.csv(i)
      file$routeID <- gsub("-.+", "", file$route)
      withRoute <- merge(file, routes)
      withAgency <- merge(withRoute, agencies)
      for (j in unique(withAgency$agency_name)) {
        agencySums[j] <- agencySums[j] + 
          sum(withAgency$arrival[withAgency$agency_name == j],
              na.rm=TRUE)
        agencyDenoms[j] <- agencyDenoms[j] + 
          sum(!is.na(withAgency$arrival[withAgency$agency_name == j]))
      }
    }
    df <- as.data.frame(agencySums/agencyDenoms)
    names(df) <- "arrival"
    df[is.finite(df[,1]), , drop=FALSE]
  }
)

nlines = linesFromFiles(head(allfiles, 100))
source("single-file.R")
psingle = profvis(singleFile(n))
htmlwidgets::saveWidget(psingle, "profile_single.html")

profvis({
  oneFile <- read.csv("/course/AT/alldata2.csv", nrows=nlines,
                      stringsAsFactors=FALSE)
  routes <- read.csv("/course/AT/routes.txt", stringsAsFactors=FALSE)
  agencies <- read.csv("/course/AT/agency.txt", stringsAsFactors=FALSE)
  oneFile$routeID <- gsub("-.+", "", oneFile$route)
  routes$routeID <- gsub("-.+", "", routes$route_id)
  withRoute <- merge(oneFile, routes)
  withAgency <- merge(withRoute, agencies)
  aggregate(withAgency["arrival"],
            list(agency=withAgency$agency_name), mean, na.rm=TRUE)
}
)


# Q3 Q4 single File
# rewrite single file using data table
library(data.table)
singleDT = function(nlines) {
  # read as data table
  oneFile = fread("D:/UoA/STATS769/Lab/Lab4/alldata2.csv", nrows=nlines,stringsAsFactors=FALSE)
  routes = fread("D:/UoA/STATS769/Lab/Lab4/routes.txt", stringsAsFactors=FALSE)
  agencies = fread("D:/UoA/STATS769/Lab/Lab4/agency.txt",stringsAsFactors=FALSE)
  # update route_id and route, truncate the version information
  oneFile = oneFile[ ,route_id := gsub("-.+", "", route)]
  routes = routes[ ,route_id := gsub("-.+", "", route_id)]
  # subset routes and agencies
  routes = routes[,list(route_id, agency_id)]
  agencies = agencies[, list(agency_id, agency_name)]
  # merge two small files first, then merge the large file
  routeAgency = merge(agencies, routes, by = "agency_id")
  tripplus = merge(routeAgency, oneFile, by = "route_id")
  # aggregation
  tripplus[,.(arrival = mean(arrival,na.rm=TRUE)), by= .(agency_name)]
}

# comparing two single file functions on whole dataset
# count number of lines of whole dataset
alllines = fread("D:/UoA/STATS769/Lab/Lab4/alldata2.csv", stringsAsFactors = FALSE)
nlines = nrow(alllines)
# measure singleFile on whole dataset
gc.old = gc(reset = TRUE)
ptm.old = proc.time()

buffer = singleFile(nlines)

gc.single = gc() - gc.old
ptm.single = proc.time() - ptm.old
print(gc.single)
print(ptm.single)

# measure singleDT on whole dataset
gc.old = gc(reset = TRUE)
ptm.old = proc.time()

buffer = singleDT(nlines)

gc.singleDT = gc() - gc.old
ptm.singleDT = proc.time() - ptm.old
print(gc.singleDT)
print(ptm.singleDT)

# measure setsub and all dataset
n = 1:5 * 100
s.old = vector("list", length(n))
for(i in n) {
  s.old[[i]] = measure(allfiles, i, singleFile)
}
single.old = do.call(rbind, s.old)

s.new = vector("list", length(n))
for(i in n) {
  s.new[[i]] = measure(allfiles, i, singleDT)
}
single.new = do.call(rbind, s.new)

# plot
df.single = merge(single.new, single.old, all = TRUE)
df.single = read.csv("df_single.csv")
plotmeasure(df.single, "Comparing Time", "Time (second)", "time")
plotmeasure(df.single, "Comparing Max used Memory", "Memory (KB)", "max.memory")
plotmeasure(df.single, "Comparing Used Memory", "Memory (KB)", "used.memory")
