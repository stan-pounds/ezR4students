##################################
# use one or more R packages
# provide names of packages enclosed in quotes and separated by commas

use.packages=function(...)
{
  # get the list of requested packages
  package.names=unlist(list(...))
  package.names=as.character(package.names)
  
  # get the list of installed packages
  package.list=installed.packages()
  package.list=package.list[,"Package"]
  package.list=as.character(package.list)
  
  # if neeeded, install BiocManager to check Bioconductor
  if (!is.element("BiocManager",package.list))
  {
    install.packages("BiocManager",
                     update=F,
                     ask=F)
  }
  library(BiocManager)
  

  # Prepare to install and/or load each package
  n.packages=length(package.names)
  package.loaded=rep(F,n.packages)
  
  # install and/or load each package as necessary
  for (i in 1:n.packages)
  {
    package.name=as.character(package.names[i])
    
    if(!is.element(package.name,package.list))
    {
      inst.res=try(install.packages(package.name,
                                    verbose=F,
                                    quiet=T))
      bioc.res=try(BiocManager::install(package.name,
                                        update=F,
                                        ask=F,
                                        verbose=F))
    } 
    lib.res=try(library(package.name,
                        character.only=T,
                        verbose=F))
    package.loaded[i]=(class(lib.res)!="try-error")
  }
  
  if (any(package.loaded))
    alert('Package "',package.names[package.loaded],'" is ready for use. \n')
  
  if(any(!package.loaded))
    alert('Package "',package.names[!package.loaded],'" was not found.  Was the name correctly spelled and capitalized?')
  
  return(invisible())
}

#############################################
# Read a data file programmatically or interactively

read.data.file=function(file.name=NULL)
{
  ##################################
  # Interactively choose file if no file is provided
  if (is.null(file.name))
    file.name=file.choose()
  
  ##############################################
  # Determine whether the file exists
  real.file=file.exists(file.name)
  
  if (!real.file)
  {
    alert(file.name,"does not exist.  Please choose file interactively.")
    file.name=file.choose()
  }
  
  ######################################
  # Determine the type of data
  file.type=file.extension(file.name)
  alert(basename(file.name)," is a ",file.type," file.")

  ######################################
  # Read a csv file
  if (file.type=="csv")
  {
    alert("Reading ",file.name,".")
    fc=count.fields(file.name,sep=",")
    mfc=median(fc,na.rm=T)
    line1=which.min(fc==mfc)
    nline=sum(fc==mfc,na.rm=T)
    dset=try(read.csv(file.name,skip=line1-1))
    
    if (class(dset)=="data.frame")
    {
      alert("Successfully read ",basename(file.name),".")
      alert("Next time, you may use the following R code:")
      alert('my.data=read.csv("',file.name,'", skip=',line1-1,")")
      return(dset)
    }

    alert("Unable to read ",file.name,".")
    alert("Open the file in Excel.  Ensure all rows have an equal number of columns.  Save as new csv file and try again.")
    return(dset)
  }
  
  ####################################
  # read a tab-delimited txt file
  if (file.type=="txt")
  {
    alert("Reading ",file.name,".")
    fc=count.fields(file.name,sep="\t")
    mfc=median(fc,na.rm=T)
    line1=which.min(fc==mfc)
    dset=try(read.csv(file.name,skip=line1-1))
    
    if (class(dset)=="data.frame")
    {
      alert("Successfully read ",basename(file.name),".")
      alert("Next time, you may use the following R code:")
      alert('my.data=read.table("',file.name,'",sep=\t, skip=',line1-1,")")
      return(dset)
    }
    
    alert("Unable to read",file.name,".")
    alert("Open the file in Excel.  Ensure all rows have an equal number of columns.  Save as new csv file and try again.")
    return(dset)
    
  }
  
  ####################################
  # load an R data file
  if (file.type=="Rdata")
  {
    loaded.objects=try(load(file.name,verbose=T))
    alert("Successfully obtained ",loaded.objects," from ",basename(file.name),".")
    alert("Next time you may use the following R command:")
    alert('load("',file.name,'",verbose=T)')
    return(invisible())
  }
  
  
  ###################################
  # read an xlsx file
  if (file.type=="xlsx")
  {
    use.packages("readxl")
    dset=try(read_xlsx(file.name,sheet=1))
    dset=as.data.frame(dset)
    if (class(dset)=="data.frame")
    {
      alert("Successfully read sheet 1 of ",basename(file.name),".")
      alert("Next time you may use the following R code:")
      alert('use.packages("readxl")')
      alert('my.data=read_xlsx("',file.name,'",sheet=1)')
      return(dset)
    }
  }
  
  #############################
  # If all of the above fails 
  
  alert("Data not read.")
  alert("Please specify an Rdata, xlsx, txt (tab delimited text) or csv file.")
  stop("No data read.")
}


#######################################
# List all example data sets in installed packages

list.example.datasets=function()
{
  my.data=data(package = .packages(all.available = TRUE))
  res=my.data$results
  return(res)
}


##################################
# Extract a file extension

file.extension=function(file.name)
{
  split.name=unlist(strsplit(file.name,split=".",fixed=T))
  return(split.name[length(split.name)])
}

####################################
# Alert the user with a message

alert=function(...)
{
  alert.message=paste0(...)
  message(alert.message)
}

####################################
# Alert the user and stop the calculation

stop.alert=function(...)
{
  alert.message=paste0(...)
  stop(alert.message)
}

############################################
# Load some useful packages
use.packages("survival","readxl","writexl","cmprsk")