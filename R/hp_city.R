#' @title House Price calculation of each city
#' @description Calculate every city results in spetial database, based on Odinary Krigging interpolation.
#' @param district City name, character
#' @param host Host of the server, character
#' @param port Port of the server, numeric
#' @param user User of the server, character
#' @param password Password of the server, character
#' @param dbname Database name of city, character
#' @param startmon The firt month in the form such as "200606", character
#' @param endmon The last month in the form as param startmon, character
#' @param resol Mesh resolution, unit: meter, numeric
#' @param outpath Output path
#' @param sys The system type, Linux or Wins, defines the encoding type of configure file here, character
#' @return Magnitude, Link and Year-over-year distibution of House Price; price level; minmax magnitude
#' @details THe outputs mainly contains Altitude, Link and Year-over-year distibution of the price.
#' @export
hp_city <- function(district,host,port,user,password,dbname,startmon,endmon,resol,outpath,sys){
  
  ##############################################
  #the used libraries and functions:           #
  #  library(MASS)                             #
  #  library(gstat)                            #
  #  library(raster)                           #
  #  library(RMySQL)                           #
  #  source("preprocess.R",encoding = 'UTF-8') #
  #  source("boundary.R",encoding = 'UTF-8')   #
  #  source("grid.R",encoding = 'UTF-8')       #
  #  source("readpr.R",encoding = 'UTF-8')     #
  #  source("prsp.R",encoding = 'UTF-8')       #
  #  source("krig.R",encoding = 'UTF-8')       #
  ##############################################
  
  # the encoding type defined by the system
  if (sys == "linux"){
    enctype <- "SET NAMES utf8"
  }else{
    enctype <- "SET NAMES gbk"
  }
  
  ######################################################
  ##################### preprocess #####################
  ######################################################
  result0 <- preprocess(district,host,port,user,password,dbname,startmon,endmon,enctype)
  if (class(result0) == "numeric") {
    if (result0 == 0) return(0)
  }
  result <- result0[[1]]
  startmon <- result0[[2]]
  endmon <- result0[[3]]
  
  ##############################################################################
  ############################ months to calculate #############################
  ##############################################################################
  nmonth<-(as.numeric(substr(endmon,1,4))-as.numeric(substr(startmon,1,4)))*12+
    as.numeric(substr(endmon,5,6))-as.numeric(substr(startmon,5,6))+1
  months<-c()
  months[1]<-as.numeric(startmon)
  if (nmonth>1){
    for (i in 2:nmonth)
    {
      if (as.numeric(substr(months[i-1],5,6))<12)
      {
        months[i]<-months[i-1]+1
      }else{
        months[i]<-months[i-1]+89
      }
    }
  }
  
  ###########################################
  ########## CRS transformation #############
  ###########################################
  swap <- result[4:5]
  names(swap) <- c("long","lat")
  coordinates(swap) <- ~long+lat
  projection(swap) <- CRS("+init=epsg:4326")
  newproj <- CRS("+init=epsg:3857")
  swap <- spTransform(swap,newproj)
  swap <- as.data.frame(swap)
  result[4:5] <- swap
  
  #####################################
  ############ boundary ###############
  #####################################
  bd<-boundary(district,host,port,user,password,dbname,enctype)
  if (class(bd) == "numeric") {
    if (bd == 0) return(0)
  }
  bound<-bd[[1]][1:2]
  housebd<-bd[[2]][1:2]
  
  ####################################
  #### calculate the locate range ####
  ####################################
  xgridmin<-min(na.omit(bound)$long)-0.
  xgridmax<-max(na.omit(bound)$long)+0.
  ygridmin<-min(na.omit(bound)$lat)-0.
  ygridmax<-max(na.omit(bound)$lat)+0.
  
  ############################################
  ### set grids, resolution.default = 500m ###
  ############################################
  xgrid <- seq(xgridmin, xgridmax, by = resol)
  ygrid <- seq(ygridmin, ygridmax, by = resol)
  basexy <- grid(xgrid,ygrid)
  
  #####################################################
  ########### define level and minmax price ###########
  #####################################################
  level <- data.frame("time"=NA,"value"=NA)
  minmaxp <- data.frame("time"=NA,"minp"=NA,"maxp"=NA)
  
  ###############################################################################################
  ##################### calculate the price distribution of the first month #####################
  ###############################################################################################
  # extract data
  pr <- readpr(result,months[1])
  
  # box-cox conversion, and convert to "sp" form
  myprsp <- prsp(pr)
  
  # variogram
  vgm <- variogram(z~1,myprsp)
  
  # fitting
  suppressWarnings(m <- fit.variogram(vgm,vgm(model="Sph",
                          psill=mean(vgm$gamma),range=max(vgm$dist)/2,
                          nugget=min(vgm$gamma)),fit.kappa=TRUE))
  
  # kriging interplation
  krige <- krig(myprsp,pr,basexy,m,26)
  
  # blank and collect the data
  x <- krige$x
  y <- krige$y
  krige$mark1 <- inSide(list("x"=bound$long,"y"=bound$lat),x,y)
  krige$mark2 <- inSide(list("x"=housebd$long,"y"=housebd$lat),x,y)
  krige <- subset(krige,mark1 & mark2)
  
  # convert to raster, and write to local files
  output0 <- rasterFromXYZ(krige[1:3], res = c(resol,resol), crs = "+init=epsg:3857")
  writeRaster(output0,filename=paste0(outpath,"/ras_11_newcalprice","/ras_11_",district,"_newcalprice_",months[1],".tif"),
              format='GTiff', datatype="FLT8S", overwrite=TRUE)
  
  # calculate level,minmax price
  level[1,] <- c(months[1],mean(krige$p))
  minmaxp[1,] <- c(months[1],min(krige$p),max(krige$p))
  
  cat(months[1],"\t")
  
  ###############################################################################################
  #### interpolation of the following months, calculate the link and year-over-year change, #####
  #### always with the price level and minmax price #############################################
  ###############################################################################################
  if (nmonth>1){
    for (i in 2:nmonth)
    {
      pr <- readpr(result,months[i])
      myprsp <- prsp(pr)
      vgm <- variogram(z~1,myprsp)
      suppressWarnings(m <- fit.variogram(vgm,vgm(model="Sph",
                              psill=mean(vgm$gamma),range=max(vgm$dist)/2,
                              nugget=min(vgm$gamma)),fit.kappa=TRUE))
      krige <- krig(myprsp,pr,basexy,m,26)
      x <- krige$x
      y <- krige$y
      krige$mark1 <- inSide(list("x"=bound$long,"y"=bound$lat),x,y)
      krige$mark2 <- inSide(list("x"=housebd$long,"y"=housebd$lat),x,y)
      krige <- subset(krige,mark1 & mark2)
      output1 <- rasterFromXYZ(krige[1:3], res = c(resol,resol), crs = "+init=epsg:3857")
      writeRaster(output1, filename=paste0(outpath,"/ras_11_newcalprice","/ras_11_",district,"_newcalprice_",months[i],".tif"),
                  format='GTiff', datatype="FLT8S", overwrite=TRUE)

      # calculate the link change, output2
      output2 <- (output1-output0)/output1*100.
      writeRaster(output2, filename=paste0(outpath,"/ras_11_newlink","/ras_11_",district,"_newlink_",months[i],".tif"),
                  format='GTiff', datatype="FLT8S", overwrite=TRUE)

      # calculate level, minmax price
      level[i,] <- c(months[i],mean(krige$p))
      minmaxp[i,] <- c(months[i],min(krige$p),max(krige$p))

      #calculate the year over year change
      if (i>12) {
        yoy1 <- raster(paste0(outpath,"/ras_11_newcalprice","/ras_11_",district,"_newcalprice_",months[i-12],".tif"))
        output3 <- (output1-yoy1)/yoy1*100
        writeRaster(output3, filename=paste0(outpath,"/ras_11_newlike","/ras_11_",district,"_newlike_",months[1],".tif"),
                    format='GTiff', datatype="FLT8S", overwrite=TRUE)
      }

      cat(months[i],"\t")

      output0 <- output1
    }
  }
  
  # write level.dat, minmaxp.txt
  write.table(level,paste0(outpath,"/level/",district,"level.dat"),row.names = FALSE)
  write.table(minmaxp,paste0(outpath,"/minmaxp/",district,"minmaxp.dat"),row.names = FALSE)
  
  return(0)
  
}
