#' @title Batches used for House Price calculation of China
#' @description The main fuction of this package.
#' @description Combine all city results of each month, and crop to 16 tiles.
#' @param startmon The firt month in the form such as "200606", character
#' @param endmon The last month in the form as param startmon, character
#' @param resol Mesh resolution, unit: meter, numeric
#' @param configfile The configuration file, with the path, character
#' @param outpath The path storing outfiles, character
#' @param sys The system type, Linux or Wins ?
#' @details THe outputs mainly contains Magnitude, Link and Year-over-year distibution of the price.
#' @details Contains six dirs: 
#' @details (1) ras_11_newcalprice --- altitude
#' @details (2) ras_11_newlike --- year-over-year
#' @details (3) ras_11_newlink --- link
#' @details (4) level --- price level
#' @details (5) minmaxp --- min and max price each month and city
#' @details (6) temp --- temperary dir in which files are not necessary, you can neglect it
#' @return Generate amounts of "tif" files to outpath
#' @export
hp_CHN <- function(startmon,endmon,resol,configfile,outpath,sys){
  
  # set local GDAL installation options
  gdal_setInstallation()
  valid_install <- !is.null(getOption("gdalUtils_gdalPath"))
  if (valid_install == FALSE) {return("Please install gdalUtils!")}
  
  # delete the warning reminder
  options(warn = -1)
  
  ######################################################
  ######## the path and dir saving output files ########
  ######################################################
  outpath <- outpath
  
  ###########################################################################################
  ############## calculate the months, and create the month,level,minmax dir ################
  ###########################################################################################
  if (!file.exists(outpath)) {dir.create(outpath)}
  
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
  
  if (!file.exists(paste0(outpath,"/ras_11_newcalprice"))) {dir.create(paste0(outpath,"/ras_11_newcalprice"))}
  if (!file.exists(paste0(outpath,"/ras_11_newlike"))) {dir.create(paste0(outpath,"/ras_11_newlike"))}
  if (!file.exists(paste0(outpath,"/ras_11_newlink"))) {dir.create(paste0(outpath,"/ras_11_newlink"))}
  if (!file.exists(paste0(outpath,"/level"))) {dir.create(paste0(outpath,"/level"))}
  if (!file.exists(paste0(outpath,"/minmaxp"))) {dir.create(paste0(outpath,"/minmaxp"))}
  if (!file.exists(paste0(outpath,"/temp"))) {dir.create(paste0(outpath,"/temp"))}
  
  ###############################################################################################
  # 计算除禧泰数据库(city_info中的367个城市)中可克达拉和昆玉两个地区以外的365个城市的价格分布、 #
  # 环比和同比涨跌幅分布 ########################################################################
  ###############################################################################################
  cat("1. calculate the data of each city:\n")
  if (!file.exists("city_info.txt")){
    cat("There's no configure file with database information!!!")
    return(0)
  }else{
    cityinfo <- read.table(configfile,header = TRUE, stringsAsFactors = FALSE, fileEncoding = 'UTF-8')
  }
  
  for (i in 1:nrow(cityinfo))
  {
    ######################################################
    ################ server configuration ################
    ######################################################
    # the city's name, pinyinabb  ########################
    district <- cityinfo$pinyinabb[i] ####################
    # the city's server host  ############################
    host <- cityinfo$host[i]  ############################
    # the city's server port  ############################
    port <- cityinfo$port[i]  ############################
    # the city's server user  ############################
    user <- cityinfo$user[i]  ############################
    # the city's server password  ########################
    password <- cityinfo$password[i] #####################
    # the city's database name  ##########################
    dbname <- cityinfo$dbname[i]  ########################
    ######################################################
    ######################################################
    ######################################################
    
    # calculate each city
    cat(cityinfo$chinese[i],"(",i,")\t")
    # the server exist? if not, stop!
    if (is.na(district) | is.na(host) | is.na(port) | is.na(user) | is.na(password) | is.na(dbname)){
      cat("There's no server for this city!\n")
      next
    }
    hp_city(district,host,port,user,password,dbname,startmon,endmon,resol,outpath,sys)
    cat(as.character(Sys.time()),"\tsucced\n")
  }
  
  # stop, if no any data
  if (length(dir(paste0(outpath,"/ras_11_newcalprice"))) == 0) {return(0)}
  
  ###############################################################################################
  ########################### 将各城市数据拼合，并切割为16个瓦片 ################################
  ###############################################################################################
  
  ##################### tile file ######################
  ##### id st_xmin	 st_xmax	st_ymin	     st_ymax #####
  ######################################################
  # 1	73.502355	85.821018	43.6183054	53.563624        #
  # 2	85.821018	98.139681	43.6183054	53.563624        #
  # 3	98.139681	110.458344	43.6183054	53.563624      #
  # 4	110.458344	122.777007	43.6183054	53.563624    #
  # 5	122.777007	135.09567	43.6183054	53.563624      #
  # 6	73.502355	85.821018	33.6729868	43.6183054       #
  # 7	85.821018	98.139681	33.6729868	43.6183054       #
  # 8	98.139681	110.458344	33.6729868	43.6183054     #
  # 9	110.458344	122.777007	33.6729868	43.6183054   #
  # 10	122.777007	135.09567	33.6729868	43.6183054   #
  # 11	73.502355	85.821018	23.7276682	33.6729868     #
  # 12	85.821018	98.139681	23.7276682	33.6729868     #
  # 13	98.139681	110.458344	23.7276682	33.6729868   #
  # 14	110.458344	122.777007	23.7276682	33.6729868 #
  # 15	98.139681	110.458344	13.7823496	23.7276682   #
  # 16	110.458344	122.777007	13.7823496	23.7276682 #
  ######################################################
  
  # read the tile files
  cat("\n2. combine all cities, and crop to tiles:\n")
  #tiles <- read.table("data/tiles.txt",header = TRUE,stringsAsFactors = FALSE)
  
  # set the backgroud raster
  china_tiles <- raster(xmn=min(tiles$st_xmin),xmx=max(tiles$st_xmax),
                        ymn=min(tiles$st_ymin),ymx=max(tiles$st_ymax),ncols=5,nrows=4,
                        crs="+init=epsg:3857",res = c(resol,resol))
  if (!file.exists(paste0(outpath,"/temp/china_tiles.tif"))) {
    writeRaster(china_tiles,paste0(outpath,"/temp/china_tiles.tif"),format='GTiff', datatype="FLT8S")
  }
  
  # merge all districts in one raster map, every month [, every vars]
  vars <- c("newcalprice", "newlink", "newlike")    #价格、环比、同比
  for (k in 1:length(vars))
  {
    cat("\n",vars[k],":\n")
    
    for (j in 1:nmonth)
    {
      rs <- c()
      ii <- 1
      for (i in 1:nrow(cityinfo))
      {
        fname <- paste0(outpath,"/ras_11_",vars[k],"/ras_11_",cityinfo$pinyinabb[i],"_",vars[k],"_",months[j],".tif")
        if (file.exists(fname)){
          rs[ii] <- fname
          ii <- ii+1
        }
      }
      
      # combine, using do.call method
      mosaic_rasters(c(rs,paste0(outpath,"/temp/china_tiles.tif")),paste0(outpath,"/temp/tmp.tif"),overwrite=TRUE)
      
      # crop to tiles(method1=raster; method2=gdalUtils)
      # method1
      r0 <- raster(paste0(outpath,"/temp/tmp.tif"))
      for (tile_i in 1:nrow(tiles))
      {
        crop(r0,extent(tiles$st_xmin[tile_i],tiles$st_xmax[tile_i],
                       tiles$st_ymin[tile_i],tiles$st_ymax[tile_i]),
             filename=paste0(outpath,"/ras_11_",vars[k],"/ras_11_",tile_i,"_",vars[k],"_",months[j],"01.tif"),
             overwrite=TRUE, datatype="FLT8S")
      }
      # method2
      # src_dataset <- paste0(outpath,"/temp/tmp.tif")
      # for (tile_i in 1:nrow(tiles))
      # {
      #   dstfile <- paste0(outpath,"/ras_11_newcalprice","/ras_11_tile",tile_i,"_",vars[k],"_",months[j],"01.tif")
      #   gdalwarp(src_dataset,dstfile,te=c(tiles$st_xmin[tile_i], tiles$st_ymin[tile_i], 
      #                 tiles$st_xmax[tile_i], tiles$st_ymax[tile_i]),overwrite = TRUE)
      # }
      
      cat(months[j],"\t")
    }
  }
  
  ####### delete the temp files ########
  unlink(paste0(outpath,"/temp"),recursive = TRUE)
  
  ####### THE END! #####################
  cat("\nTHE END!")
  
  return(0)
  
}
