#  MSDS 7333 - Quantifying the World - Case Study #4
#  Web Scrapping - Cherry Blossom Race-Modeling Runners
#  Team Members:
#           Jeffery Lancon, Manisha Pednekar, Andrew Walch, David Stroud
#  Date: 02/05/2019
#  Case Study from: Data Science in R: Nolan,Temple,Lang (Ch 2)
#  Initial source code: http://rdatasciencecases.org/code.html

library(XML)
library(rstudioapi)
current_path <- getActiveDocumentContext()$path
setwd(dirname(current_path ))

# ---------------------- Web Scrapping Section -------------------
# 
#  Searches cheeryblossom.org website and extracts runner's
#  information from the HTML pages for each year 1999 - 20120
#  by gender.  Saves files either to .rda or .txt files for
#  further cleaning/parsing.
#  
# ------------------- Section ends on line 270 -------------------

# createing variable for base website url
# to be appended to for each subsequent year
ubase = "http://www.cherryblossom.org/"

# Begin with 2012 dataset
url = paste(ubase, "results/2012/2012cucb10m-m.htm", sep = "")
doc = htmlParse(url)

# create a list of all nodes the contain 'pre'
# in our case, there is only 1
preNode = getNodeSet(doc, "//pre")
preNode

# Extract the txt contents of preNode List
txt = xmlValue(preNode[[1]])

nchar(txt) #690,904 Characters in txt file/dataset

substr(txt, 1, 50) # Displaying first 50 characters of txt

substr(txt, nchar(txt) - 50, nchar(txt)) # Displaying the last 50 characters of txt

# Splitting the txt into individual lines; using '\\r\\n' to signify the end of each
# line
els = strsplit(txt, "\\r\\n")[[1]]

length(els) # 7201 lines

els[1:3] # Displaying first 3 lines of els

els[ length(els) ] # Displaying last line of els


#------------- extractResTable () Version A --------
#----------Extract Race Table Function -------------
# Repeats above steps and puts it into convienient function

extractResTable =
  # Retrieve data from web site, find preformatted text,
  # return as a character vector.
  function(url)
  {
    doc = htmlParse(url)
    preNode = getNodeSet(doc, "//pre")
    txt = xmlValue(preNode[[1]])
    els = strsplit(txt, "\r\n")[[1]]   
    
    return(els)
  }

m2012 = extractResTable(url) #extract

identical(m2012, els) # Results of function are identical to previous stepwise code

# createing variable for base website url
# to be appended to for each subsequent year
ubase = "http://www.cherryblossom.org/"

# # -------------TEST Code ---------------
# # Create list of urls for each year of the race from 1999 - 2012
# urls = paste(ubase, "results/", 1999:2012, "/",
#              1999:2012, "cucb10m-m.htm", sep = "")
# 
# # Extract the tables for 1999:2012 for Mens race results
# menTables = lapply(urls, extractResTable)
# # FUnction will not work:  URLs for each year do not follow the same pattern
# # as 2011 & 2012... Will have to manually capture Urls and append to ubase for
# # each year
# options(error = recover)
# menTables = lapply(urls, extractResTable)


# Manually created list of urls(minus ubase) for Mens Results for 1999:2012
# Note: Fixed errors from textbook code for 1999, 2000
menURLs = 
  c("results/1999/cb99m.html", "results/2000/Cb003m.htm", "results/2001/oof_m.html",
    "results/2002/oofm.htm", "results/2003/CB03-M.HTM",
    "results/2004/men.htm", "results/2005/CB05-M.htm", 
    "results/2006/men.htm", "results/2007/men.htm", 
    "results/2008/men.htm", "results/2009/09cucb-M.htm",
    "results/2010/2010cucb10m-m.htm", 
    "results/2011/2011cucb10m-m.htm",
    "results/2012/2012cucb10m-m.htm")

# Recreate urls for Men's race results
urls = paste(ubase, menURLs, sep = "")

urls[1:3] # Displaying first 3

# Extract Men's results from website for years 1999:2012
menTables = lapply(urls, extractResTable)

names(menTables) = 1999:2012 # Naming each lists according to its year
#typeof(menTables)

sapply(menTables, length) # List of characters for each year. Appears to be errors
                          # for years 1999, 2000, and 2009

# 1999 2000 2001 2002 2003 2004 2005 2006 2007 2008 2009 2010 2011 2012 
# 1    1 3627 3727 3951 4164 4335 5245 5283 5913    1 6919 7019 7201 

# ------ extractResTable() Version B-------
# Modified for yr 2000 anomaly
extractResTable =
  # Retrieve data from web site, 
  # find the preformatted text,
  # and return as a character vector.
  function(url, year = 1999)
  {
    doc = htmlParse(url)
    
    if (year == 2000) {
      # Get text from 4th font element
      # File is ill-formed so <pre> search doesn't work.
      ff = getNodeSet(doc, "//font")
      txt = xmlValue(ff[[4]])
    }
    else {
      preNode = getNodeSet(doc, "//pre")
      txt = xmlValue(preNode[[1]])
    } 
    
    els = strsplit(txt, "\r\n")[[1]]
    return(els)
  }

years = 1999:2012
menTables = mapply(extractResTable, url = urls, year = years)
names(menTables) = years
sapply(menTables, length)
# Still have issues with 1999 and 2009.  Need to make additional adjustments
# to function to accomidate the different formats


# ------ extractResTable() Version C-[Final]------
# Modified for yr 1999 & 2009 anomalies
extractResTable =
  #
  # Retrieve data from web site, 
  # find the preformatted text,
  # and write lines or return as a character vector.
  #
  function(url = "http://www.cherryblossom.org/results/2009/09cucb-F.htm",
           year = 1999, sex = "male", file = NULL)
  {
    doc = htmlParse(url,encoding = 'utf-8') # encoding needed for Windows users for yr 2009
    
    if (year == 1999) {
      # Get preformatted text from <pre> elements
      pres = getNodeSet(doc, "//pre")
      txt = xmlValue(pres[[1]])
      els = strsplit(txt, "\n")[[1]]  #Line ends with \n 
    }
    else if (year == 2000) {
      # Get preformatted text from 4th font element
      # The top file is ill formed so the <pre> search doesn't work.
      ff = getNodeSet(doc, "//font")
      txt = xmlValue(ff[[4]])
      els = strsplit(txt, "\r\n")[[1]]
    }
    else if (year == 2009 & sex == "male") {
      # Get preformatted text from <div class="Section1"> element
      # Each line of results is in a <pre> element
      div1 = getNodeSet(doc, "//div[@class='Section1']")
      pres = getNodeSet(div1[[1]], "//pre")
      els = sapply(pres, xmlValue)
    }
    else {
      # Get preformatted text from <pre> elements
      pres = getNodeSet(doc, "//pre")
      txt = xmlValue(pres[[1]])
      els = strsplit(txt, "\r\n")[[1]]   
    } 
    
    if (is.null(file)) return(els)
    
    path <- paste0('./',file) # path base is current_path
    # Checking to see if data file path exists, if not, it creates it
    if (dir.exists(path) == FALSE){ dir.create(path,recursive = TRUE)}
    # Write the lines as a text file, in proper subdirectory (MenTxt or WomenTxt)
    writeLines(els, con = paste0(path,'/',year,'.txt'))
  }
#------ End of Function extractResTable() Version C --------


years = 1999:2012 # Added years argument to function call to identify year being parsed

# Switch to Matrix mapply because we are using multiple fuctions to parse data from web.
# also added 'file' argument to do 2 things, identify if we want to store results in .txt format
# and what subdirectory should the data be placed in. file=NULL means no file is created.
# NOTE: Directory must exists prior to executing code. ####
menTables = mapply(extractResTable, url = urls, year = years, file = 'MenTxt') 

#menTables = mapply(extractResTable, url = urls, year = years) #for use when creating .rda files

#---------------------------------------------------------------
#--------- Code only works if file= NULL (no text files created)
    # Adding names to tables
    names(menTables) = years
    
    # Sanity Check; Prints out length of each matrix
    sapply(menTables, length) 

    # #------------- Test Code ---- For troubleshooting
    # doctmp = htmlParse("http://www.cherryblossom.org/results/1999/cb99m.html")
    # ffx = getNodeSet(doctmp, "//PRE")
    # head(menTables$`2009`,15)
    # tail(menTables$`2009`,10)
    
    save(menTables, file = "CBMenTextTables.rda") #saves tables as r-data file format
#--------------------------------



#--------------------------- Gathering Data for Women Runners -------
#--------------------------------------------------------------------

womenURLs = 
  c("results/1999/cb99f.html", "results/2000/Cb003f.htm", "results/2001/oof_f.html",
    "results/2002/ooff.htm", "results/2003/CB03-F.HTM",
    "results/2004/women.htm", "results/2005/CB05-F.htm",
    "results/2006/women.htm", "results/2007/women.htm",
    "results/2008/women.htm", "results/2009/09cucb-F.htm",
    "results/2010/2010cucb10m-f.htm",
    "results/2011/2011cucb10m-f.htm",
    "results/2012/2012cucb10m-f.htm") #Manually collected

# Recreate urls for Women's race results
wurls = paste(ubase, womenURLs, sep = "")

womenTables = mapply(extractResTable, url = wurls,
                     year = years, sex = rep("female", 14),file = 'WomenTxt')

#---------------------------------------------------------------
#--------- Code only works if file= NULL (no text files created)

    names(womenTables) = years # Adding names to tables
    
    sapply(womenTables, length) # Sanity Check; Prints out length of each matrix
    
    # head(womenTables$`2009`,15)
    # tail(womenTables$`2009`,10)
    
    save(womenTables, file = "CBWomenTextTables.rda") #saves tables as r-data file format

# ------------------------------------------------------------------
# ----------------------End of Web Scrapping Section ----------------
# -------------------------------------------------------------------




# -------------------------------------------------------------------------
# -------------------- Cleaning Web Scrapped Data -------------------------
# -------------------------------------------------------------------------

      #------------------ Sample Code using 2012 as an example -------------
      # Read Table does not work properly for raw/parsed data, due to formatting issues
      m2012 = read.table(file="MenTxt/2012.txt", skip = 8)
      
      # Read lines is a better option for reading the Runner's txt files
      els = readLines("MenTxt/2012.txt")
      
      els[1:10] # Sanity check to see if data was properly imported 
      
      els2011 = readLines("MenTxt/2011.txt") # Repeat commands with 2011 data
      els2011[1:10]
      
      # Location of data varies depending on the year the data was obtained
      # One thing in common is that data begins after the line with equal signs '==='
      # Will use grep function to search through character strings for the index where
      # the header is located
      eqIndex = grep("^===", els) 
      eqIndex # Line 8 (2012 data)
      
          # Alternative to using grep. Use substr.  Looks for line that has '===' within
          # the first 3 characters
          first3 = substr(els, 1, 3)
          which(first3 == "===")
      
      spacerRow = els[eqIndex] # Setting spacerRow variable equal to index of row containing '==='
      headerRow = els[eqIndex - 1] # Setting headerROw variable equal to index of row containing '===' - 1
      body = els[ -(1:eqIndex) ] # body variable is everything after the spacerRow 7193 Elements
      
      headerRow = tolower(headerRow) # converting header row to lower case, to avoid issues between
                                    # years because format varies year to year (lower case & upper case)
      
      ageStart = regexpr("ag", headerRow) #Attempt to identify 'Age' category in header using regex
      ageStart #Starts at index 49 and is 2 characters long
      
      # extract age variable in character strings in body variable, using AgeStart to identify location
      age = substr(body, start = ageStart, stop = ageStart + 1) 
      head(age)
      summary(as.numeric(age))
      # Summary results
      # Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
      #    9.00   29.00   35.00   37.75   45.00   89.00       1 
      
      blankLocs = gregexpr(" ", spacerRow) # locating blanks in spacerRow, signifying separation of different variables
      blankLocs #6 18 25 48 51 72 80 88 94
      
      searchLocs = c(0, blankLocs[[1]]) # vector of location of blanks in spacerRow
      
      # Separation of variable in body variable, using searchLocs to distinguish location & length
      Values = mapply(substr, list(body), 
                      start = searchLocs[ -length(searchLocs)] + 1, #adds 1 to every value of searchLocs: 1  7 19 26 49 52 73 81 89
                      stop = searchLocs[ -1 ] - 1)# Subtracts 1 from every value of searchLocs except 1st one: 5 17 24 47 50 71 79 87 93


      
#---------- findColLocs ()  Find column locations --------
# COnverts all the previous exploratory work to identiy column location
# and put it into a single function

findColLocs = function(spacerRow) {
  
  spaceLocs = gregexpr(" ", spacerRow)[[1]]
  rowLength = nchar(spacerRow)
  
  if (substring(spacerRow, rowLength, rowLength) != " ")
    return( c(0, spaceLocs, rowLength + 1))
  else return(c(0, spaceLocs))
}
#-----

#---------- selectCols ()  Rev A Selection of each variable --------
# Converts all the previous exploratory work to select variables and populating
# values. Puts it into a single function      
selectCols = 
  function(colNames, headerRow, searchLocs) 
  {
    sapply(colNames, 
           function(name, headerRow, searchLocs)
           {
             startPos = regexpr(name, headerRow)[[1]] # Returns a -1 if regexpr does not match the
                                                      # name with a value in the headerRow (matches variable
                                                      # to header row value, if not there, does not 
                                                      # populate variable)
             if (startPos == -1) 
               return( c(NA, NA) )
             
             index = sum(startPos >= searchLocs)
             c(searchLocs[index] + 1, searchLocs[index + 1] - 1)
           },
           headerRow = headerRow, searchLocs = searchLocs )
  }
#-----
      
      #------------------ Sample Code using 2012 as an example -------------
      # To confirm functions work as anticipated
      searchLocs = findColLocs(spacerRow)  #0  6 18 25 48 51 72 80 88 94
      ageLoc = selectCols("ag", headerRow, searchLocs) 
      ages = mapply(substr, list(body), 
                    start = ageLoc[1,], stop = ageLoc[2, ])
      
      summary(as.numeric(ages)) #   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
                                #   9.00   29.00   35.00   37.75   45.00   89.00       1
      #--------------------------------------------------------------------

      #------------------ Sample Code using 2012 as an example -------------
      shortColNames = c("name", "home", "ag", "gun", "net", "time") #short name must be enough to uniquely identify
                                                                    # the column without over specifying it
      
      locCols = selectCols(shortColNames, headerRow, searchLocs) #Creates location index for all shortColNames
                                                                # variables that exist in 2012 dataset
                                                                #       name home ag gun net time
                                                                # [1,]   26   52 49  NA  NA   81  (Start)
                                                                # [2,]   47   71 50  NA  NA   87  (Stop)
      
      # Mapping variable values to matrix, using parsing functions
      Values = mapply(substr, list(body), start = locCols[1, ], 
                      stop = locCols[2, ])
      class(Values) #Matrix
      
      colnames(Values) = shortColNames #Adding column name to matrix variables (7193 observations)
        head(Values)
        tail(Values)[ , 1:3]
        #---------------------------------------------------------------------


#---------- extractVariables ()  REv A Extract variables for each observation --------
# Parses the web scrapped data into a list of matrices, using grep to id location
# and variable name: 'name' 'home' 'age' 'gun' 'net', and 'time'.
# If field does not exists, populates with NA. Puts it into a single function           
extractVariables = 
  function(file, varNames =c("name", "home", "ag", "gun",
                             "net", "time"))
  {
    # Find the index of the row with =s
    eqIndex = grep("^===", file)
    # Extract the two key rows and the data
    spacerRow = file[eqIndex] 
    headerRow = tolower(file[ eqIndex - 1 ])
    body = file[ -(1 : eqIndex) ]
    
    # Obtain the starting and ending positions of variables
    searchLocs = findColLocs(spacerRow)
    locCols = selectCols(varNames, headerRow, searchLocs)
    
    Values = mapply(substr, list(body), start = locCols[1, ], 
                    stop = locCols[2, ])
    colnames(Values) = varNames
    # returns values variable   
    invisible(Values)
  }
#--------------

#----------- Parsing of txt files for results from Cherry Blossom Race
#----------- For years 1999 - 2012:  Using
mfilenames = paste("MenTxt/", 1999:2012, ".txt", sep = "") #Creating Path
menFiles = lapply(mfilenames, readLines) # Reading lines of the 14 .txt files as character vector and
                                        # creating a list of Char vectors(one for each row in txt file), 
                                        #  storiing them in menFiles
names(menFiles) = 1999:2012 # Naming list
sapply(menFiles, length)

# Creating Matrix of men's results from web-scrapped data, using extractVariables function
menResMat = lapply(menFiles, extractVariables)
class(menResMat) # A list
sapply(menResMat,class) # Each entry in menResMat is a matrix
length(menResMat) # List of 14 character vector Matrices 1999:2012
        menResMat$`1999`[2,'home']

# Determine the number of observations per character vector Matrices
sapply(menResMat, nrow)
# 1999 2000 2001 2002 2003 2004 2005 2006 2007 2008 2009 2010 2011 2012 
# 3190 3017 3622 3724 3948 4156 4327 5237 5276 5905 6651 6911 7011 7193

        ### The 2001 results for women are missing the === and the column names.
        ### Can we pick it up from the 2001 men? YES! Make an exercise
        #wfilenames = paste("WomenTxt/", 1999:2012, ".txt", sep = "")
        #womenTables = lapply(wfilenames, readLines)
        
        #womenTables[[3]][1:5]
        
        #names(womenTables) = 1999:2012
        #womenResMat = lapply(womenTables, extractVariables)
        #head(womenResMat[[3]], 10)
        #tail(womenResMat[[3]], 10)

# ----------------------------------------
# -------------- Data Cleaning 2.3 -------

      #------------------ Sample Code using 2012 as an example;
      # --- to confirm results of function----------
      age = as.numeric(menResMat[['2012']][ , 'ag'])

      tail(age) # 41 39 56 35 NA 48
      #-----

# Extract men's Age data from yrs 2009:2012.
# using as.numeric since all data is character data (Since only 1 type of variable is allowed
# in a matrix, we have to extract it from the matrix before converting)
age = sapply(menResMat,
             function(x) as.numeric(x[ , 'ag']))
# NOTE: Waring messages means that some values could not be converted to numeric, resulting in NA values
#       Warning messages:
#         1: In FUN(X[[i]], ...) : NAs introduced by coercion (x3)


      #------- Figure 2.4 - Boxplot Ages by yr ----
      # Problems identified through the use of boxplots 2003,2006
pdf("./Figures/FIg_2.4_CB_BoxplotAgeByYr.pdf", width = 8, height = 5)
oldPar = par(mar = c(4.1, 4.1, 1, 1))

boxplot(age, ylab = "Age", xlab = "Year")

par(oldPar)
dev.off()
#-----

        #------------------ Sample Code using 2003, 2006 as an example;
        # --- to confirm results of function----------
        head(menFiles[['2003']])
        menFiles[['2006']][2200:2205]

# Notice Age header in 2003 is shifted by 1 character, so we are only grabbing the 1st digit
# In 2006, some ages are shifted in the column
# solve both problems by modifying the index for the end of each variable by +1 (include space)

#---------- selectCols () Rev B  Selection of each variable --------
# Modified previous function to includ spacer modification      
selectCols = function(shortColNames, headerRow, searchLocs) {
  sapply(shortColNames, function(shortName, headerRow, searchLocs){
    startPos = regexpr(shortName, headerRow)[[1]]
    if (startPos == -1) return( c(NA, NA) )
    index = sum(startPos >= searchLocs)
    c(searchLocs[index] + 1, searchLocs[index + 1]) #removed -1
  }, headerRow = headerRow, searchLocs = searchLocs )
}
#-------

# Rerun menResMat using updated selectCols function        
menResMat = lapply(menFiles, extractVariables)
#womenResMat = lapply(womenFiles, extractVariables)

#ReRun Age extraction
age = sapply(menResMat, 
             function(x) as.numeric(x[ , 'ag']))
# NOTE: Waring messages means that some values could not be converted to numeric, resulting in NA values
#       Warning messages:
#         1: In FUN(X[[i]], ...) : NAs introduced by coercion (x4)


#------- Figure 2.5 - Boxplot Ages by yr ----
# Recreating boxplot for male ages by year
pdf("./Figures/Fig_2.5_CB_BoxplotAgeByYrRevised.pdf", width = 8, height = 5)
oldPar = par(mar = c(4.1, 4.1, 1, 1))
boxplot(age, ylab = "Age", xlab = "Year")
par(oldPar)
dev.off()
#----

# Determining number of NA in age variable
sapply(age,  function(x) sum(is.na(x)))
# 1999 2000 2001 2002 2003 2004 2005 2006 2007 2008 2009 2010 2011 2012 
# 1    1   61    3    2    0   13    2    5    1    2    6    0    1 

# Further investigating year 2001 (61 NA)
age2001 = age[["2001"]]

# Deternine the line === appears in original .txt file for 2001
grep("^===", menFiles[['2001']]) # 5

#Determine the location of NA values (post extraction of data) after removal of top 5 rows
badAgeIndex = which(is.na(age2001)) + 5

menFiles[['2001']][ badAgeIndex ] #Displaying all lines where age NA
# All lines are blank except for last entry which is an annotation line
badAgeIndex
#[1] 1756 1757 1758 1759 1760 1761 1762 1763 1814 1815 1816 1817 1818 1819 1820 1821 1872 1873 1874 1875 1876
#[22] 1877 1878 1879 1930 1931 1932 1933 1934 1935 1936 1937 2538 2539 2540 2541 2542 2543 2544 2545 2546 2897
#[43] 2898 2899 2900 2901 2902 2903 2904 2955 2956 2957 3008 3009 3010 3011 3012 3013 3014 3015 3627


#---------- extractVariables ()  Rev B -[FINAL] Extract variables for each observation --------
# Modified to eliminate blank lines and remove footnotes
extractVariables = 
  function(file, varNames =c("name", "home", "ag", "gun",
                             "net", "time"))
  {
    
    # Find the index of the row with =s
    eqIndex = grep("^===", file)
    # Extract the two key rows and the data 
    spacerRow = file[eqIndex] 
    headerRow = tolower(file[ eqIndex - 1 ])
    body = file[ -(1 : eqIndex) ]
    # Remove footnotes and blank rows
    footnotes = grep("^[[:blank:]]*(\\*|\\#)", body)
    if ( length(footnotes) > 0 ) body = body[ -footnotes ]
    blanks = grep("^[[:blank:]]*$", body)
    if (length(blanks) > 0 ) body = body[ -blanks ]
    
    
    # Obtain the starting and ending positions of variables   
    searchLocs = findColLocs(spacerRow)
    locCols = selectCols(varNames, headerRow, searchLocs)
    
    Values = mapply(substr, list(body), start = locCols[1, ], 
                    stop = locCols[2, ])
    colnames(Values) = varNames
    
    return(Values)
  }
#----

# Rerun menResMat using updated extractVariables function 
menResMat = lapply(menFiles, extractVariables)
#womenResMat = lapply(womenFiles, extractVariables)

which(age2001 < 5) # look at 2001 where age < 5 (3 lines 1377 3063 3112)

menFiles[['2001']][ which(age2001 < 5) + 5 ] # look at these lines manually [Ages listed as 0]



#------ Working with Time variable ------------------
#-----------------------------------------------------

      #------------------ Sample Code using 2012 as an example -------------
      # Extraction and manipulaton of time character variable.
      charTime = menResMat[['2012']][, 'time']
      class(charTime) # Character class
      head(charTime, 5) # "  45:15 " "  46:28 " "  47:33 " "  47:34 " "  47:40 "
      
      tail(charTime, 5) # "2:27:11 " "2:27:20 " "2:27:30 " "2:28:58 " "2:30:59 "
      
      # Splitting hh:mm:ss using ':" as the split parameter
      timePieces = strsplit(charTime, ":")
      typeof(timePieces) #list of character vectors
      timePieces[[1]] # "  45" "15 " 
      
      tail(timePieces, 1) #  "2"   "30"  "59 "
      
      # Converting timePieces from character to numeric
      timePieces = sapply(timePieces, as.numeric)
      timePieces[[1]] #  45 15

      # Combine timePieces character vectors into a numeric variable
      # in minutes 60*hrs + min + sec/60
      runTime = sapply(timePieces, 
                 function(x) {
                   if (length(x) == 2) x[1] + x[2]/60
                   else 60*x[1] + x[2] + x[3]/60
                 })

      summary(runTime)
      # Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
      # 45.25   77.57   87.47   88.43   97.78  150.98

#---------- convertTime ()  Rev A Extracts time variable for each observation --------
# --------- splits time, using ':' then converts timePieces to numeric and
#---------- coverts and records results in minutes
convertTime = function(time) {
  timePieces = strsplit(time, ":")
  timePieces = sapply(timePieces, as.numeric)
  sapply(timePieces, function(x) {
    if (length(x) == 2) x[1] + x[2]/60
    else 60*x[1] + x[2] + x[3]/60
  })
}

#---------- createDF ()  Rev A Crates DataFrame from menResMat --------
# Creating a dataframe with the data from menResMat
# Dataframe will include new variables to track 'sex' and 'year',
# which are currently not part of the data within the matrices
createDF = 
  function(Res, year, sex) 
  {
    # Determine which time to use 'net','gun','time'
    useTime = if( !is.na(Res[1, 'net']) )  # Net time gets top priority if it exists
      Res[ , 'net']
    else if( !is.na(Res[1, 'gun']) ) # gun time gets second priority
      Res[ , 'gun']
    else 
      Res[ , 'time']
    
    runTime = convertTime(useTime)
    
    Results = data.frame(year = rep(year, nrow(Res)),
                         sex = rep(sex, nrow(Res)),
                         name = Res[ , 'name'],
                         home = Res[ , 'home'],
                         age = as.numeric(Res[, 'ag']), 
                         runTime = runTime,
                         stringsAsFactors = FALSE)
    invisible(Results)
  }

# Creating list of dataframe with: year, sex,name,home,age,runtime, using createDF function
menDF = mapply(createDF, menResMat, year = 1999:2012,
               sex = rep("M", 14), SIMPLIFY = FALSE)
# There were 50 or more warnings (use warnings() to see the first 50)
typeof(menDF) #list with 14 elements
sapply(menDF,class) # 14 DataFrames

warnings()[ c(1:2, 49:50) ]
# 1: In lapply(X = X, FUN = FUN, ...) : NAs introduced by coercion
# 2: In lapply(X = X, FUN = FUN, ...) : NAs introduced by coercion
# 3: In lapply(X = X, FUN = FUN, ...) : NAs introduced by coercion
# 4: In lapply(X = X, FUN = FUN, ...) : NAs introduced by coercion

# Checking to see if NAs are coming from time conversion runTime.
# Errors coming from yr 2006, 2007,2009,2010
sapply(menDF, function(x) sum(is.na(x$runTime)))
# 1999 2000 2001 2002 2003 2004 2005 2006 2007 2008 2009 2010 2011 2012 
# 0    0    0    0    0    0    0 5232   83    0   72   68    1    0 


#---------- createDF ()  Rev B - [FINAL] Crates DataFrame from menResMat --------
# -- modifies original function to strip tramp characters from time '#','*',' [[blanks]]'
# -- Drops observations with no time 
createDF = function(Res, year, sex) 
{
  # Determine which time to use
  if ( !is.na(Res[1, 'net']) ) useTime = Res[ , 'net']
  else if ( !is.na(Res[1, 'gun']) ) useTime = Res[ , 'gun']
  else useTime = Res[ , 'time']
  
  # Remove # and * and blanks from time
  useTime = gsub("[#\\*[:blank:]]", "", useTime)
  runTime = convertTime(useTime[ useTime != "" ])
  
  # Drop rows with no time
  Res = Res[ useTime != "", ]
  
  Results = data.frame(year = rep(year, nrow(Res)),
                       sex = rep(sex, nrow(Res)),
                       name = Res[ , 'name'], home = Res[ , 'home'],
                       age = as.numeric(Res[, 'ag']), 
                       runTime = runTime,
                       stringsAsFactors = FALSE)
  invisible(Results)
}

# Rerun updated createDF Function
menDF = mapply(createDF, menResMat, year = 1999:2012,
               sex = rep("M", 14), SIMPLIFY = FALSE)

sapply(menDF, function(x) sum(is.na(x$runTime)))
# 1999 2000 2001 2002 2003 2004 2005 2006 2007 2008 2009 2010 2011 2012 
# 0    0    0    0    0    0    0 5232    0    0    0    0    0    0 

#Displaying 2006 runTime data.  (home variable include home and time;  time variable includes NA)
menDF$`2006`[1:3,c('home','runTime')] 
# home runTime
# 1 Kenya             47:24       NA
# 2 Kenya             47:34       NA
# 3 Kenya             47:38       NA

#Looking back at original file to determine issue
menFiles[['2006']][6:10]
# [6] ""                                                                                         
# [7] "Place Div/Tot  Num    Name                   Ag Hometown        Net Tim Gun Tim  Pace  S "
# [8] "===== ======== ====== ====================== == ======================= =======  ===== = "
# [9] "    1   1/2892      1 Gilbert Okari          27 Kenya             47:24   47:25#  4:45   "
# [10] "    2   2/2892     11 Samuel Ndereba         29 Kenya             47:34   47:35#  4:46   "

# Notice that Net Time header is joined with Hometown, so it does not recognize that this is 2 variables
# and not 1 variable.  We will need to split 'home' variable up into 2 variables

# Manually adjusting time data for year 2006
separatorIdx = grep("^===", menFiles[["2006"]]) # Line 8
separatorRow = menFiles[['2006']][separatorIdx] # Extracting Separator Row
          separatorRow
          nchar(separatorRow) #89
          substr(separatorRow,40,80)

separatorRowX = paste(substring(separatorRow, 1, 63), " ", 
                      substring(separatorRow, 65, nchar(separatorRow)), # Manually imparting a ' ' character
                      sep = "")                                         # in separatorRow @ chr 64, so 
                                                                        # extractVariable function will
                                                                        # work properly
menFiles[['2006']][separatorIdx] = separatorRowX # overwriting original speratorRow with modified one

# Rerun extractVariables function, utilizing modified separator for Yr 2006
menResMat = sapply(menFiles, extractVariables)
menDF = mapply(createDF, menResMat, year = 1999:2012,
               sex = rep("M", 14), SIMPLIFY = FALSE)

#separatorIdx = grep("^===", womenFiles[["2006"]])
#separatorRow = womenFiles[['2006']][separatorIdx]
#separatorRowX = paste(substring(separatorRow, 1, 63), " ", 
#                      substring(separatorRow, 65, nchar(separatorRow)), 
#                      sep = "")
#womenFiles[['2006']][separatorIdx] = separatorRowX

#womenResMat = sapply(womenFiles, extractVariables)
#womenDF = mapply(createDF, womenResMat, year = 1999:2012,
#               sex = rep("W", 14), SIMPLIFY = FALSE)



#------- Figure 2.5b - Boxplot time by yr (Men)----
# Preliminary view of data, to discover any problems with data
# Time data looks fine, no obvious issues
pdf("./Figures/FIg_2.5B_CB_BoxplotTimeByYr.pdf", width = 8, height = 5)
boxplot(sapply(menDF, function(x) x$runTime), 
        xlab = "Year", ylab = "Run Time (min)")
dev.off()
#----

# Since we have added the yr and sex variables, we can convert
# the yearly dataframes into 1 single dataframe and save them to .rda 
# and csv files
cbMen = do.call(rbind, menDF) # 70070 observations x 6 variables

path <- paste0('./MenTxt') # path base is current_path
# Checking to see if data file path exists, if not, it creates it
if (dir.exists(path) == FALSE){ dir.create(path,recursive = TRUE)}

# Write dataframe as a csv file, in proper subdirectory (MenTxt or WomenTxt)
write.csv(cbMen, file = paste0(path,'/cbMen.csv'))
# Write dataframe as R data file
save(cbMen, file = "./MenTxt/cbMen.rda")

dim(cbMen) # 70070 observations x 6 variables

#####################################################################################################
#####################################################################################################
################           Data Exploration
################



load("cbMen.rda")

pdf("CB_Overplot.pdf", width = 8, height = 6)
oldPar = par(mar = c(4.1, 4.1, 1, 1))

plot(runTime ~ age, data = cbMen, ylim = c(40, 180),
     xlab = "Age (years)", ylab = "Run Time (minutes)")

par(oldPar)
dev.off()

library(RColorBrewer)
ls("package:RColorBrewer")

display.brewer.all()

Purples8 = brewer.pal(9, "Purples")[8]
Purples8

Purples8A = paste(Purples8, "14", sep = "")

pdf("CB_OverplotTransparent.pdf", width = 8, height = 6)
oldPar = par(mar = c(4.1, 4.1, 1, 1))
plot(runTime ~ jitter(age, amount = 0.5), 
     data = cbMen, 
     pch = 19,cex = 0.2, col = Purples8A,
     ylim = c(45, 165), xlim = c(15, 85),
     xlab = "Age (years)", ylab = "Run Time (minutes)")
par(oldPar)
dev.off()

pdf("CB_SmoothScatter.pdf", width = 8, height = 6)
oldPar = par(mar = c(4.1, 4.1, 1, 1))

smoothScatter(y = cbMen$runTime, x = cbMen$age,
              ylim = c(40, 165), xlim = c(15, 85),
              xlab = "Age (years)", ylab = "Run Time (minutes)")

par(oldPar)
dev.off()

cbMenSub = cbMen[cbMen$runTime > 30 &
                   !is.na(cbMen$age) & cbMen$age > 15, ]

ageCat = cut(cbMenSub$age, breaks = c(seq(15, 75, 10), 90))
table(ageCat)

pdf("CB_Boxplots.pdf", width = 8, height = 6)
oldPar = par(mar = c(4.1, 4.1, 1, 1))

plot(cbMenSub$runTime ~ ageCat, 
     xlab = "Age (years)", ylab = "Run Time (minutes)")

par(oldPar)
dev.off()

lmAge = lm(runTime ~ age, data = cbMenSub)

lmAge$coefficients

summary(lmAge)

class(lmAge)

pdf("CB_ResidSimpleLM.pdf", width = 8, height = 6)
oldPar = par(mar = c(4.1, 4.1, 1, 1))

smoothScatter(x = cbMenSub$age, y = lmAge$residuals,
              xlab = "Age (years)", ylab = "Residuals")
abline(h = 0, col = "purple", lwd = 3)

resid.lo = loess(resids ~ age, 
                 data = data.frame(resids = residuals(lmAge),
                                   age = cbMenSub$age))

age20to80 = 20:80

resid.lo.pr = 
  predict(resid.lo, newdata = data.frame(age = age20to80))

lines(x = age20to80, y = resid.lo.pr, col = "green", lwd = 2)
par(oldPar)
dev.off()

menRes.lo = loess(runTime ~ age, cbMenSub)

menRes.lo.pr = predict(menRes.lo, data.frame(age = age20to80))

over50 = pmax(0, cbMenSub$age - 50)

lmOver50 = lm(runTime ~ age + over50, data = cbMenSub)

summary(lmOver50)

decades = seq(30, 60, by = 10)
overAge = lapply(decades, 
                 function(x) pmax(0, (cbMenSub$age - x)))
names(overAge) = paste("over", decades, sep = "")
overAge = as.data.frame(overAge)
tail(overAge)

lmPiecewise = lm(runTime ~ . , 
                 data = cbind(cbMenSub[, c("runTime", "age")], 
                              overAge))

summary(lmPiecewise)

overAge20 = lapply(decades, function(x) pmax(0, (age20to80 - x)))
names(overAge20) = paste("over", decades, sep = "")
overAgeDF = cbind(age = data.frame(age = age20to80), overAge20)

tail(overAgeDF)

predPiecewise = predict(lmPiecewise, overAgeDF)

plot(predPiecewise ~ age20to80,
     type = "l", col = "purple", lwd = 3,
     xlab = "Age (years)", ylab = "Run Time Prediction")

lines(x = age20to80, y = menRes.lo.pr, 
      col = "green", lty = 2, lwd = 3)
legend("topleft", col = c("purple", "green"),
       lty = c(1, 2), lwd= 3,
       legend = c("Piecewise Linear", "Loess Curve"), bty = "n")

pdf("CB_PiecewiseLoessCurves.pdf", width = 8, height = 6)
plot(predPiecewise ~ age20to80,
     type = "l", col = "#984ea3", lwd = 3,
     #   type = "l", col = "purple", lwd = 2,
     xlab = "Age (years)", ylab = "Run Time Prediction")

lines(x = age20to80, y = menRes.lo.pr, col = "#4daf4a", lwd = 3, lty = 2)
legend("topleft", col = c("#984ea3", "#4daf4a"), lty = c(1, 2), lwd = 3,
       legend = c("Piecewise Linear", "Loess Curve"), bty = "n")

#lines(x = age20to80, y = menRes.lo.pr, col = "green", lwd = 2)
#legend("topleft", col = c("purple", "green"), lty = 1, lwd = 2,
#       legend = c("Piecewise Linear", "Loess Curve"), bty = "n")
dev.off()


pdf("CB_NumRunnersLinePlot.pdf", width = 8, height = 6)
oldPar = par(mar = c(4.1, 4.1, 1, 1))

numRunners = with(cbMen, tapply(runTime, year, length))
plot(numRunners ~ names(numRunners), type="l", lwd = 2,
     xlab = "Years", ylab = "Number of Runners")
par(oldPar)
dev.off()

summary(cbMenSub$runTime[cbMenSub$year == 1999])

summary(cbMenSub$runTime[cbMenSub$year == 2012])

pdf("CB_AgeDensity99vs12.pdf", width = 8, height = 6)
oldPar = par(mar = c(4.1, 4.1, 1, 1))

age1999 = cbMenSub[ cbMenSub$year == 1999, "age" ]
age2012 = cbMenSub[ cbMenSub$year == 2012, "age" ]

plot(density(age1999, na.rm = TRUE), 
     ylim = c(0, 0.05), col = "purple",
     lwd = 3,  xlab = "Age (years)",  main = "")
lines(density(age2012, na.rm = TRUE), 
      lwd = 3, lty = 2, col="green")
legend("topleft", col = c("purple", "green"), lty= 1:2, lwd = 3,
       legend = c("1999", "2012"), bty = "n")

par(oldPar)
dev.off()

qqplot(age1999, age2012, pch = 19, cex = 0.5, 
       ylim = c(10,90), xlim = c(10,90), 
       xlab = "Age in 1999 Race",
       ylab = "Age in 2012 Race", 
       main = "Quantile-quantile plot of male runner's age")
abline(a =0, b = 1, col="red", lwd = 2)

mR.lo99 = loess(runTime ~ age, cbMenSub[ cbMenSub$year == 1999,])
mR.lo.pr99 = predict(mR.lo99, data.frame(age = age20to80))

mR.lo12 = loess(runTime ~ age, cbMenSub[ cbMenSub$year == 2012,])
mR.lo.pr12 = predict(mR.lo12, data.frame(age = age20to80))

plot(mR.lo.pr99 ~ age20to80,
     type = "l", col = "purple", lwd = 3,
     xlab = "Age (years)", ylab = "Fitted Run Time (minutes)")

lines(x = age20to80, y = mR.lo.pr12,
      col = "green", lty = 2, lwd = 3)

legend("topleft", col = c("purple", "green"), lty = 1:2, lwd = 3,
       legend = c("1999", "2012"), bty = "n")


pdf("CB_Loess99vs12.pdf", width = 8, height = 6)
oldPar = par(mar = c(4.1, 4.1, 1, 1))

plot(mR.lo.pr99 ~ age20to80,
     type = "l", col = "#984ea3", lwd = 3,
     xlab = "Age (years)", ylab = "Prediction (minutes)")  
lines(x = age20to80, y = mR.lo.pr12, col="#4daf4a", lty = 2, lwd = 3) 
legend("topleft", col = c("#984ea3", "#4daf4a"), lty = 1:2, lwd = 3,
       legend = c("1999", "2012"), bty = "n")
par(oldPar)
dev.off()

gap14 = mR.lo.pr12 - mR.lo.pr99

pdf("CB_DifferenceInFittedCurves.pdf", width = 8, height = 6)
oldPar = par(mar = c(4.1, 4.1, 1, 1))

plot(gap14 ~ age20to80, type = "l" , xlab = "Age (years)", 
     ylab = "Difference in Fitted Curves (minutes)", lwd = 2)
par(oldPar)
dev.off()

fastestMan = tapply(menRes$time, menRes$age, min, na.rm = TRUE)
plot(fastestMan ~ names(fastestMan), type ="l", xlim = c(20, 80))
ageFM = as.numeric(names(fastestMan))
mR.loF = loess(fastestMan ~ ageFM)
mR.lo.prF = predict(mR.loF, data.frame(age = ageFM), se = FALSE)
lines(x = ageFM, y = mR.lo.prF, col = "purple", lwd = 2)

timeNorm = menRes$time / mR.lo.prF[as.character(menRes$age)]
time99Norm = timeNorm[menRes$year == 1999]
time12Norm = timeNorm[menRes$year == 2012]
summary(time99Norm)

summary(time12Norm)

plot(density(100*time99Norm, na.rm = TRUE), 
     # ylim = c(0, 0.05), 
     col = "purple",
     lwd = 3,  xlab = "Time (percentage)",
     main = "Time Distribution for 1999 and 2012 Runners\n Percentage of the fastest runner for that age")
lines(density(100*time12Norm, na.rm = TRUE), 
      lwd = 3, col = "green")
legend("topleft", fill = c("purple", "green"),
       legend = c("1999", "2012"), bty = "n")


trimBlanks = function(charVector) {
  nameClean = gsub("^[[:blank:]]+", "", charVector)
  nameClean = gsub("[[:blank:]]+$", "", nameClean)
  nameClean = gsub("[[:blank:]]+", " ", nameClean)
}

nameClean = trimBlanks(cbMenSub$name)

length(nameClean)

length(unique(nameClean))

table(table(nameClean))

head( sort(table(nameClean), decreasing = TRUE), 1)

mSmith = cbMenSub[nameClean == "Michael Smith", ]

head(unique(mSmith$home))

nameClean = tolower(nameClean)

head( sort(table(nameClean), decreasing = TRUE), 1)

nameClean = gsub("[,.]", "", nameClean)

tabNameYr = table(cbMenSub$year, nameClean)

max(tabNameYr)

class(tabNameYr)

mode(tabNameYr)

names(attributes(tabNameYr))

dim(tabNameYr)

head(colnames(tabNameYr), 3)

which( tabNameYr == max(tabNameYr) )

which( tabNameYr == max(tabNameYr), arr.ind = TRUE )

indMax = which( tabNameYr == max(tabNameYr), arr.ind = TRUE )
colnames(tabNameYr)[indMax[2]]

cbMenSub$nameClean = nameClean

cbMenSub$yob = cbMenSub$year - cbMenSub$age

# Fix home in a similar way
homeClean = trimBlanks(tolower(cbMenSub$home))
cbMenSub$homeClean = gsub("[,.]", "", homeClean)

vars = c("year", "homeClean", "nameClean", "yob",  "runTime")
mb = which(nameClean == "michael brown")
birthOrder = order(cbMenSub$yob[mb])
cbMenSub[mb[birthOrder], vars]

cbMenSub$ID = paste(nameClean, cbMenSub$yob, sep = "_")

races = tapply(cbMenSub$year, cbMenSub$ID, length)

races8 = names(races)[which(races >= 8)]

men8 = cbMenSub[ cbMenSub$ID %in% races8, ]

orderByRunner = order(men8$ID, men8$year)
men8 = men8[orderByRunner, ]

men8L = split(men8, men8$ID)
names(men8L) = races8

length(unique(men8$ID))

gapTime = tapply(men8$runTime, men8$ID,
                 function(t) any(abs(diff(t)) > 20))

gapTime = sapply(men8L, function(df) 
  any(abs(diff(df$runTime)) > 20))

sum(gapTime)

lapply(men8L[ gapTime ][1:2], function(df) df[, vars])

homeLen = nchar(cbMenSub$homeClean)

cbMenSub$state = substr(cbMenSub$homeClean, 
                        start = homeLen - 1, stop = homeLen)

cbMenSub$state[cbMenSub$year == 2006] = NA

cbMenSub$ID = paste(cbMenSub$nameClean, cbMenSub$yob, 
                    cbMenSub$state, sep = "_")

numRaces = tapply(cbMenSub$year, cbMenSub$ID, length)
races8 = names(numRaces)[which(numRaces >= 8)]
men8 = cbMenSub[ cbMenSub$ID %in% races8, ]
orderByRunner = order(men8$ID, men8$year)
men8 = men8[orderByRunner, ]

men8L = split(men8, men8$ID)
names(men8L) = races8

length(races8)

groups = 1 + (1:length(men8L) %% 9)

addRunners = function(listRunners, colors, numLty) 
{
  numRunners = length(listRunners)
  colIndx = 1 + (1:numRunners) %% length(colors)
  ltys = rep(1:numLty, each = length(colors), length = numRunners)
  
  mapply(function(df, i) {      
    lines(df$runTime ~ df$age, 
          col = colors[colIndx[i]], lwd = 2, lty = ltys[i])
  }, listRunners, i = 1:numRunners) 
}

colors = c("#e41a1c", "#377eb8","#4daf4a", "#984ea3", 
           "#ff7f00", "#a65628")
par(mfrow = c(3, 3), mar = c(2, 2, 1, 1))
invisible(
  sapply(1:9, function(grpId){
    plot( x = 0, y = 0, type = "n",
          xlim = c(20, 80), ylim = c(50, 130),
          xlab = "Age (years)", ylab = "Run Time (minutes)")
    
    addRunners(men8L[ groups == grpId ], colors, numLty = 6)
  }) )

fitOne = function(oneRunner, addLine = FALSE, col = "grey") {
  lmOne = lm(runTime ~ age, data = oneRunner)
  if (addLine) 
    lines(x = oneRunner$age, y = predict(lmOne), 
          col = col, lwd = 2, lty = 2)
  
  ind = floor( (nrow(oneRunner) + 1) / 2)
  res = c(coefficients(lmOne)[2], oneRunner$age[ind],
          predict(lmOne)[ind])
  names(res) = c("ageCoeff", "medAge", "predRunTime")
  return(res)
}

par(mfrow = c(1, 1), mar = c(5, 4, 1, 1))

plot( x = 0, y = 0, type = "n",
      xlim = c(20, 80), ylim = c(50, 130),
      xlab = "Age (years)", ylab = "Run Time (minutes)")

addRunners(men8L[ groups == 9 ], colors, numLty = 6)
lapply(men8L[groups == 9], fitOne, addLine = TRUE, col = "black")

men8LongFit = lapply(men8L, fitOne)

coeffs = sapply(men8LongFit, "[", "ageCoeff" )
ages = sapply(men8LongFit, "[", "medAge")

longCoeffs = lm(coeffs ~ ages)

summary(longCoeffs)

pdf("CB_LongCoeffs.pdf", width = 10, height = 7)
oldPar = par(mar = c(4.1, 4.1, 1, 1))
plot(coeffs ~ ages, xlab = "Median Age (years)",
     ylab = "Coefficient (minutes per race / year)")
abline(longCoeffs, col = "#984ea3", lwd = 3)
abline(h = 0, col="blue", lwd = 3)
loCoeffs = loess(coeffs ~ ages)
ageV = min(ages):max(ages)
predV = predict(loCoeffs, new = data.frame(ages = ageV))
lines(x = ageV, y = predV, lwd = 3, lty = 2, col = "#4daf4a")
par(oldPar)
dev.off()

