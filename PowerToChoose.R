updateptc <- function(upload=FALSE)
{
    library("KonostdlibR")
    loadpackage("RMySQL")
    
    user <- as.character(KonostdlibR::getcredentials("AWSMySQLSandbox")$userid)
    password <- as.character(KonostdlibR::getcredentials("AWSMySQLSandbox")$password)
    host <- as.character(KonostdlibR::getcredentials("AWSMySQLSandbox")$host)
    dbname <- as.character(KonostdlibR::getcredentials("AWSMySQLSandbox")$dbname)
    con <- RMySQL::dbConnect(MySQL(),user=user, password=password, host=host, dbname=dbname)
    
    dfptc <- getPTC()
    rightnow <- Sys.time()
    dfcurrent <- runmysql("select idKey from KonoDev.tbl_txptc where endTS is NULL", con=con)
    dftdu <- runmysql("select tdulong, id from KonoDev.tbl_tdu", con=con)
    dfnew <- dfptc[!(dfptc$idKey %in% dfcurrent$idKey),]
    if(nrow(dfnew)>0)
    {
        dfnew <- merge(dfnew,dftdu,by.x="TduCompanyName",by.y="tdulong")
        dfnew$TduCompanyName <- NULL
        names(dfnew)[names(dfnew) == "id"] <- "id_tdu"
        dfnew$startTS <- rightnow
        dfnew$endTS <- NA
        if(upload)
        {
            dbWriteTable(conn=con, name="tbl_txptc", value=dfnew, append=TRUE, overwrite=FALSE, row.names = FALSE)
        }
    }else
    {
        #build empty data frame so it's returned in expected format, even though it won't be uploaded
        dfnew$TDUCompanyName <- NULL
        dfnew$startTS <- as.character()
        dfnew$endTS <- as.character()
        dfnew$id_tdu <- as.integer()
    }
    terminate <- dfcurrent[-which(dfcurrent$idKey %in% dfptc$idKey),]

    if(length(terminate) > 0)
    {
        terminateestatement <- paste0("update KonoDev.tbl_txptc set endTS = '",rightnow,"' where idKey in (")
        for (i in 1:length(terminate))
        {
            if(i != 1)
            {
                terminateestatement <- paste0(terminateestatement, ", ")
            }
            terminateestatement <- paste0(terminateestatement, "'",terminate[i],"'")
        }
        terminateestatement <- paste0(terminateestatement, ")")
        if(upload)
        {
            markterminated <- runmysql(terminateestatement, con=con)
        }
    }
    list(terminated=terminate, update=dfnew)
}

txptctodomo <- function(upload=TRUE)
{
    library("KonostdlibR")
    loadpackage("RMySQL")
    
    user <- as.character(KonostdlibR::getcredentials("AWSMySQLSandbox")$userid)
    password <- as.character(KonostdlibR::getcredentials("AWSMySQLSandbox")$password)
    host <- as.character(KonostdlibR::getcredentials("AWSMySQLSandbox")$host)
    dbname <- as.character(KonostdlibR::getcredentials("AWSMySQLSandbox")$dbname)
    con <- RMySQL::dbConnect(MySQL(),user=user, password=password, host=host, dbname=dbname)
    
    df <- KonostdlibR::runmysql("select p.*, t.*
                                    from KonoDev.tbl_txptc p
                                    left join KonoDev.tbl_tdu t
                                    on p.id_tdu = t.id")
    df$active <- FALSE
    df$active[is.na(df$endTS)] <- TRUE
    if(upload)
    {
        KonostdlibR::pushtodomo(df, "TX PTC")
    }
    df
}


## pulling data from Power to Choose
getPTC <- function()
{
    PTCdata <- read.csv('http://www.powertochoose.org/en-us/Plan/ExportToCsv/power-to-choose-offers.csv')
    names(PTCdata) <- gsub("\\.", "", names(PTCdata))
    names(PTCdata) <- gsub("X", "", names(PTCdata))
    PTCdata <- PTCdata[-which(PTCdata$idKey == "END OF FILE"),]
    PTCdata
}

## Input df for monthly usages
MonthlyUsage <- function(Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec)
{
 df <- data.frame("Jan" = Jan, "Feb" = Feb, "Mar" = Mar, "Apr" = Apr, "May" = May, "Jun" = Jun,
              "Jul" = Jul, "Aug" = Aug, "Sep" = Sep, "Oct" = Oct, "Nov" = Nov, "Dec" = Dec)
}

## Determines the cost by month based on usage at 500, 1000, and 2000 tiers  
Cost <- function(RawData, Usage)
{
  CostData <- data.frame(RawData$idKey, RawData$kwh500, RawData$kwh1000, RawData$kwh2000)
  names(CostData) <- gsub("RawData\\.", "", names(CostData))
#Jan 
  if(Usage$Jan <= 500)
    {
    CostData$Jan <- Usage$Jan * CostData$kwh500
    }
  if(Usage$Jan > 500 & Usage$Jan <= 1000)
    {
      CostData$Jan <- Usage$Jan * CostData$kwh1000
    }
  if(Usage$Jan > 1000)
    {
      CostData$Jan <- Usage$Jan * CostData$kwh2000
    }
  
#Feb 
  if(Usage$Feb <= 500)
    {
      CostData$Feb <- Usage$Feb * CostData$kwh500
    }
  if(Usage$Feb > 500 & Usage$Feb <= 1000)
    {
      CostData$Feb <- Usage$Feb * CostData$kwh1000
    }
  if(Usage$Feb > 1000)
    {
      CostData$Feb <- Usage$Feb * CostData$kwh2000
    }

  #Mar 
  if(Usage$Mar <= 500)
  {
    CostData$Mar <- Usage$Mar * CostData$kwh500
  }
  if(Usage$Mar > 500 & Usage$Mar <= 1000)
  {
    CostData$Mar <- Usage$Mar * CostData$kwh1000
  }
  if(Usage$Mar > 1000)
  {
    CostData$Mar <- Usage$Mar * CostData$kwh2000
  }

  #Apr 
  if(Usage$Apr <= 500)
  {
    CostData$Apr <- Usage$Apr * CostData$kwh500
  }
  if(Usage$Apr > 500 & Usage$Apr <= 1000)
  {
    CostData$Apr <- Usage$Apr * CostData$kwh1000
  }
  if(Usage$Apr > 1000)
  {
    CostData$Apr <- Usage$Apr * CostData$kwh2000
  }
  
  #May 
  if(Usage$May <= 500)
  {
    CostData$May <- Usage$May * CostData$kwh500
  }
  if(Usage$May > 500 & Usage$May <= 1000)
  {
    CostData$May <- Usage$May * CostData$kwh1000
  }
  if(Usage$May > 1000)
  {
    CostData$May <- Usage$May * CostData$kwh2000
  }
  
  #Jun 
  if(Usage$Jun <= 500)
  {
    CostData$Jun <- Usage$Jun * CostData$kwh500
  }
  if(Usage$Jun > 500 & Usage$Jun <= 1000)
  {
    CostData$Jun <- Usage$Jun * CostData$kwh1000
  }
  if(Usage$Jun > 1000)
  {
    CostData$Jun <- Usage$Jun * CostData$kwh2000
  }
  
  #Jul 
  if(Usage$Jul <= 500)
  {
    CostData$Jul <- Usage$Jul * CostData$kwh500
  }
  if(Usage$Jul > 500 & Usage$Jul <= 1000)
  {
    CostData$Jul <- Usage$Jul * CostData$kwh1000
  }
  if(Usage$Jul > 1000)
  {
    CostData$Jul <- Usage$Jul * CostData$kwh2000
  }
  
  #Aug 
  if(Usage$Aug <= 500)
  {
    CostData$Aug <- Usage$Aug * CostData$kwh500
  }
  if(Usage$Aug > 500 & Usage$Aug <= 1000)
  {
    CostData$Aug <- Usage$Aug * CostData$kwh1000
  }
  if(Usage$Aug > 1000)
  {
    CostData$Aug <- Usage$Aug * CostData$kwh2000
  }
  
  #Sep 
  if(Usage$Sep <= 500)
  {
    CostData$Sep <- Usage$Sep * CostData$kwh500
  }
  if(Usage$Sep > 500 & Usage$Sep <= 1000)
  {
    CostData$Sep <- Usage$Sep * CostData$kwh1000
  }
  if(Usage$Sep > 1000)
  {
    CostData$Sep <- Usage$Sep * CostData$kwh2000
  }
  
  #Oct 
  if(Usage$Oct <= 500)
  {
    CostData$Oct <- Usage$Oct * CostData$kwh500
  }
  if(Usage$Oct > 500 & Usage$Oct <= 1000)
  {
    CostData$Oct <- Usage$Oct * CostData$kwh1000
  }
  if(Usage$Oct > 1000)
  {
    CostData$Oct <- Usage$Oct * CostData$kwh2000
  }
  
  #Nov 
  if(Usage$Nov <= 500)
  {
    CostData$Nov <- Usage$Nov * CostData$kwh500
  }
  if(Usage$Nov > 500 & Usage$Nov <= 1000)
  {
    CostData$Nov <- Usage$Nov * CostData$kwh1000
  }
  if(Usage$Nov > 1000)
  {
    CostData$Nov <- Usage$Nov * CostData$kwh2000
  }
  
  #Dec 
  if(Usage$Dec <= 500)
  {
    CostData$Dec <- Usage$Dec * CostData$kwh500
  }
  if(Usage$Dec > 500 & Usage$Dec <= 1000)
  {
    CostData$Dec <- Usage$Dec * CostData$kwh1000
  }
  if(Usage$Dec > 1000)
  {
    CostData$Dec <- Usage$Dec * CostData$kwh2000
  }
#Annual Cost for input usage by plan 
CostData$Annual <- CostData$Jan + CostData$Feb + CostData$Mar + CostData$Apr + CostData$May + CostData$Jun +
                   CostData$Jul + CostData$Aug + CostData$Sep + CostData$Oct + CostData$Nov + CostData$Dec
CostData
}

main <- function()
{
    library('RCurl')

    RawData <- getdata()
    Usage <- MonthlyUsage(600,688,715,819,766,1104,1350,1219,900,640,488,412)
    Price <- Cost(RawData, Usage)
    ## Lowest Price Based on usage
    LowestPrice <- Price[Price$Annual == min(Price$Annual),]
    
    #this is the best plan based on your usage (ignores credits due to inability to easily derive from free form fields)
    CheapestPlan <- RawData[RawData$idKey == Price[which(Price$Annual == min(Price$Annual)),"idKey"],
            c("TduCompanyName","RepCompany","Product","kwh500","kwh1000","kwh2000")]
    CheapestPlan
}