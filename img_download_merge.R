library(magick)
library(REdaS)

setwd("D:/abroad/UIUC/Courses/2018 Spring/interGIS project/image")

getMapFromURL = function(x, y, zoom, country, imgtype){
  
  # url of image if it is the cell phone coverage file
  if(imgtype == "cell")  url = sprintf("https://tiles-prod.opensignal.com/?zoom=%d&x=%d&y=%d", zoom, x, y)
  
  # url of image if it is the google maps 
  if(imgtype == "google") url = sprintf("https://maps.googleapis.com/maps/vt?pb=!1m5!1m4!1i%d!2i%d!3i%d!4i256!2m3!1e0!2sm!3i413113432!3m9!2sen-US!3sUS!5e18!12m1!1e68!12m3!1e37!2m1!1ssmartmaps!4e0&token=20120", zoom, x, y)
  
  #if(imgtype != "cell" || imgtype != "google") stop("Type of Image incorrect")
  
  name = paste(as.character(c(country,zoom,x,y)), collapse = "_")
  download.file(url, paste0(getwd(),"/",country,"/",imgtype,"/",name,".png"),mode = "wb")
  return(name)
}

getImgCounty = function(xStart , xEnd, yStart, yEnd, zoom, country,imgtype = "cell"){
  # a combination of all x y pair
  graphIDX = expand.grid(x = xStart:xEnd, y = yStart:yEnd)
  
  # create the directory if it does not exist
  ifelse(!dir.exists(file.path(getwd(), country,imgtype)), dir.create(file.path(getwd(), country,imgtype),recursive = T), FALSE)
  
  # download the file and return names
  namesdownload = sort(mapply(getMapFromURL, graphIDX[,1], graphIDX[,2], MoreArgs = list(zoom = zoom, country = country, imgtype=imgtype)))
  
  # vector to store the dimension of final picture
  picDim = c(length(xStart:xEnd), length(yStart:yEnd), dim(graphIDX)[1])
  names(picDim) = c("x_length","y_length","Num_img")
  
  # store the download x y value
  picPara_DF = data.frame(start = c(xStart,yStart), End = c(xEnd, yEnd),row.names = c("x","y"))
  
  return(list(img_name = namesdownload, picture_dimansion = picDim,Dowload_para = picPara_DF, zoom = zoom, country = country, image_Type = imgtype))
}# end of downloading

mergeImg = function(imgObj){
  
  for(c in 1:(imgObj$picture_dimansion[1]*imgObj$picture_dimansion[2])){
    print(as.character(c))
    y = c%%imgObj$picture_dimansion[2]
    x = as.integer(c/imgObj$picture_dimansion[2])
    country = imgObj$country
    type = imgObj$image_Type
    dir = paste0(getwd(),"/",country,"/",type,"/")
    
    img = image_read(path = paste0(dir,imgObj$img_name[c],".png"))
    print(as.character(x))
    print(as.character(y))
    
    
    if(y != 0){ # when it is append row

      if(y ==1){
        colImg = c(img)  #the first image of each column(row)
      }else{
        colImg = c(colImg,img) # put image into column vec (append row)
      } # end of appending the each row in one column
    }else{ # reach the end of each row
      colImg = c(colImg,img) # append the last image
      # bind the rows in one column together
      colBindImg = image_append(colImg,stack = T)
      
      if(x == 1){ # the first column of total image
        totalImg = c(colBindImg)
      }else{ # rest of the column of total image
        totalImg = c(totalImg,colBindImg)
      }
    }# End of create img grid
    
    if(c == imgObj$picture_dimansion[1]*imgObj$picture_dimansion[2]){
      finalImg = image_append(totalImg)
      image_write(image = finalImg,path = paste0(dir,country,"_",type,"Merge.png" ))
      return(finalImg)
    }# merge the whole image together
    print("-----------")
  }# end of for merge image
}# end of merge image function


# the x and y has to be imput manually
x_start = 19
x_end = 25
y_start = 31
y_end = 38
zoom = 6


# brazil
brazil_obj = getImgCounty(xStart = 19, xEnd  = 25, yStart =31, yEnd = 38, zoom = 6, country = "Brazil")

brazil_google_obj = getImgCounty(xStart = 19, xEnd  = 25, yStart =31, yEnd = 38, zoom = 6, country = "Brazil", imgtype = "google")


brazil_png = mergeImg(brazil_obj)
brazil_google_png = mergeImg(brazil_google_obj)

# south africa
SouthAfrica_obj = getImgCounty(xStart = 139,yStart = 144,xEnd = 151,yEnd = 154,zoom = 8, country = "South_Africa")
SouthAfrica_google_obj = getImgCounty(xStart = 139,yStart = 144,xEnd = 151,yEnd = 154,zoom = 8, country = "South_Africa",imgtype = "google")

SouthAfrica_png = mergeImg(SouthAfrica_obj)
SouthAfrica_google_png = mergeImg(SouthAfrica_google_obj)

# mexico
mexico_obj = getImgCounty(xStart = 44, xEnd = 66,yStart = 103,yEnd = 118,zoom = 8,country = "Mexico")
mexico_google_obj = getImgCounty(xStart = 44, xEnd = 66,yStart = 103,yEnd = 118,zoom = 8,country = "Mexico",imgtype = "google")

mexico_png = mergeImg(mexico_obj)
mexico_google_obj = mergeImg(mexico_google_obj)

# russia 
# Note russia cross the border of 0 so we going to seperate it into two parts

russiaL_obj = getImgCounty(xStart = 36, xEnd = 63,yStart = 10,yEnd = 23,zoom = 6,country = "Russia")
russiaL_png = mergeImg(russiaL_obj)

russiaL_google_obj = getImgCounty(xStart = 36, xEnd = 63,yStart = 10,yEnd = 23,zoom = 6,country = "Russia",imgtype = "google")
russiaL_google_png = mergeImg(russiaL_google_obj)

russiaR_obj = getImgCounty(xStart = 0,xEnd = 2, yStart = 10, yEnd = 23,zoom = 6, country ="RussiaR")
russiaR_png = mergeImg(russiaR_obj)

russiaR_googleR_obj = getImgCounty(xStart = 0,xEnd = 2, yStart = 10, yEnd = 23,zoom = 6, country ="RussiaR",imgtype = "google")
russiaR_googleR_png = mergeImg(russiaR_googleR_obj)


# merge the left and rigth of Russia



Rus_cel = image_append(c(image_read("Russia/cell/Russia_cellMerge.png"),image_read("RussiaR/cell/RussiaR_cellMerge.png")))
image_write(image = Rus_cel,path = "Russia_cellMerge.png",format = "png",depth = 16)

image_write(image = image_append(c(image_read("Russia/google/Russia_googleMerge.png"),image_read("RussiaR/google/RussiaR_googleMerge.png"))), path = "Russia_googleMerge.png", format = "png", depth = 16)



# ----Tile to lat lon----

tile2long = function(xtile,ytile,zoom,x_pos = "N", y_pos = "W"){
  
  # Switch the corner
  if(x_pos == "S") xtile = xtile + 1
  if(y_pos == "E") ytile = ytile + 1
  
  n = 2^zoom
  lon_deg = xtile/n*360 - 180
  lat_rad = atan(asin(pi*(1-2*ytile/n)))
  lat_deg = rad2deg(lat_rad)
  returnVec = c(lon_deg,lat_deg)
  names(returnVec) = c("lon_deg","lat_deg")
  return(returnVec)
}

n = 2^6

pi*(1-2*10/n)

options(digits = 30)

asin(pi*(1-2*10/n))


tile2long(xtile = 139, ytile = 144, zoom = 8)
tile2long(xtile = 151, ytile = 154, zoom = 8,x_pos = "S", y_pos = "E")


tile2long(xtile = 36, ytile = 0, zoom = 6,x_pos = "S", y_pos = "E")
tile2long(xtile = 63, ytile = 23, zoom = 6,x_pos = "S", y_pos = "E")

ytile10 = 68.800248118
xtile10 = 22.500






