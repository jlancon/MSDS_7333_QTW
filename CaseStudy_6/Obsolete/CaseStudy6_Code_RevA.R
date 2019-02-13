#  MSDS 7333 - Quantifying the World - Case Study #6
#  Spam Detection - Using Statistic Analysis
#  Team Members:
#           Jeffery Lancon, Manisha Pednekar, Andrew Walch, David Stroud
#  Date: 02/19/2019
#  Case Study from: Data Science in R: Nolan,Temple,Lang (Ch 3)
#  Initial source code: http://rdatasciencecases.org/code.html


# Setting the current working directory to the current file location
library(rstudioapi)
current_path <- getActiveDocumentContext()$path
setwd(dirname(current_path ))


################### Set-up and Exploration of Data ##################
#####################################################################

# To programmatically located the  emails, the files
# are stored in a subdirectory /SpamAssassinMessages/Messages/.....
# There are 5 directories easy_ham,easy_ham_2,hard_ham,spam, and spam_2

spamPath = "./SpamAssassinMessages/"

list.dirs(spamPath, full.names = FALSE)
# [1] ""                    "Messages"            "Messages/easy_ham"   "Messages/easy_ham_2" "Messages/hard_ham"  
# [6] "Messages/spam"       "Messages/spam_2"

# Listing folders in Message directory
list.files(path = paste(spamPath, "messages", 
                        sep = .Platform$file.sep))
# [1] "easy_ham"   "easy_ham_2" "hard_ham"   "spam"       "spam_2" 

# examime the first few file names in spam_2 foloder
# The files are named by message number and their MD5 checksum hash
head(list.files(path = paste(spamPath, "messages", "spam_2",
                             sep = .Platform$file.sep)))
# [1] "00001.317e78fa8ee2f54cd4890fdc09ba8176" "00002.9438920e9a55591b18e60d1ed37d992b"
# [3] "00003.590eff932f8704d8b0fcbe69d023b54d" "00004.bdcc075fa4beb5157b5dd6cd41d8887b"
# [5] "00005.ed0aba4d386c5e62bc737cf3f0ed9589" "00006.3ca1f399ccda5d897fecb8c57669a283"

dirNames = list.files(path = paste(spamPath, "messages", 
                                   sep = .Platform$file.sep))

# Check to see how many files located in the Messages directories
length(list.files(paste(spamPath, "messages", dirNames, 
                        sep = .Platform$file.sep)))
# [1] 9353

# Getting a count of files in each of Messages subdirectories
sapply(paste(spamPath, "messages", dirNames, 
             sep = .Platform$file.sep), 
       function(dir) length(list.files(dir)) )
# ./SpamAssassinMessages//messages/easy_ham ./SpamAssassinMessages//messages/easy_ham_2 
# 5052                                        1401 
# ./SpamAssassinMessages//messages/hard_ham       ./SpamAssassinMessages//messages/spam 
# 501                                        1002 
# ./SpamAssassinMessages//messages/spam_2 
# 1398 

# Creating a list of directory names/paths for use in retrieving data
fullDirNames = paste(spamPath, "messages", dirNames, 
                     sep = .Platform$file.sep)
      # ------ Test Code -----
      # List first message/file from messages/easy_ham
      fileNames = list.files(fullDirNames[1], full.names = TRUE)
      fileNames[1] #[1] "./SpamAssassinMessages//messages/easy_ham/00001.7c53336b37003a9286aba55d2945844c"
      # Read and display the first few lines of the first message in easy_ham
      msg = readLines(fileNames[1])
      head(msg)
      # [1] "From exmh-workers-admin@redhat.com  Thu Aug 22 12:36:23 2002"     
      # [2] "Return-Path: <exmh-workers-admin@spamassassin.taint.org>"         
      # [3] "Delivered-To: zzzz@localhost.netnoteinc.com"                      
      # [4] "Received: from localhost (localhost [127.0.0.1])"                 
      # [5] "\tby phobos.labs.netnoteinc.com (Postfix) with ESMTP id D03E543C36"
      # [6] "\tfor <zzzz@localhost>; Thu, 22 Aug 2002 07:36:16 -0400 (EDT)"

      # Selecting 15 files from easy_ham directory to read
      indx = c(1:5, 15, 27, 68, 69, 329, 404, 427, 516, 852, 971)
      fn = list.files(fullDirNames[1], full.names = TRUE)[indx]
      sampleEmail = sapply(fn, readLines) # Creates a list of 15 character vectors       

      # Subsetting out first message in easy_ham directory for analysis
      msg = sampleEmail[[1]]
      which(msg == "")[1] # Identifying first blank line in message (This is to find the first blank line,
                          # by using the [1], in the message, which indicates the separation between header
                          # and body of email) Without index [1], would return a index of all blank lines
      #[1] 63

      match("", msg) # Another way to identifying the first blank line in message/vector)

      # going to create a splitpoint, using the first blank line as the argument
      splitPoint = match("", msg)
      
      #Splitting message into header/body, using splitPoint index as the argument
      # Creating msg using the last 2 lines of header and first 6 lines of body
      msg[ (splitPoint - 2):(splitPoint + 6) ]
          # [1] "List-Archive: <https://listman.spamassassin.taint.org/mailman/private/exmh-workers/>"
          # [2] "Date: Thu, 22 Aug 2002 18:26:25 +0700"                                               
          # [3] ""                                                                                    
          # [4] "    Date:        Wed, 21 Aug 2002 10:54:46 -0500"                                    
          # [5] "    From:        Chris Garrigues <cwg-dated-1030377287.06fa6d@DeepEddy.Com>"         
          # [6] "    Message-ID:  <1029945287.4797.TMDA@deepeddy.vircio.com>"                         
          # [7] ""                                                                                    
          # [8] ""                                                                                    
          # [9] "  | I can't reproduce this error."

      header = msg[1:(splitPoint-1)] # Creating Header of message: everything above splitPoint
      body = msg[ -(1:splitPoint) ] # Creating Body of message: everything below SplitPoint

#----------- splitMessage function () - Rev A ----------
# Split message into two character vectors (header and body) list, using 1st blank line
# as the split Point
splitMessage = function(msg) {
  splitPoint = match("", msg)
  header = msg[1:(splitPoint-1)]
  body = msg[ -(1:splitPoint) ]
  return(list(header = header, body = body))
}
#---
      
      # ------ Test Code -----
      sampleSplit = lapply(sampleEmail, splitMessage) #Testing code on list of 15 previously select messages
      head(sampleSplit[[1]]$header)
      head(sampleSplit[[2]]$body)

      header = sampleSplit[[1]]$header
      grep("Content-Type", header) #identifying index for line containing 'Content-Type' of message #1
      # [1] 46
      grep("multi", tolower(header[46]))  # determing if message has attachments (noted by Content-Type Key value:
                                          # equaling 'multi'), Uses tolower to make text all lowercase.
      # integer(0) - 'multi' not found in line 46; therefore, no attachments
      header[46]
      # [1] "Content-Type: text/plain; charset=us-ascii"

      # Function to determines the content type of all 15 test emails
      headerList = lapply(sampleSplit, function(msg) msg$header)
      CTloc = sapply(headerList, grep, pattern = "Content-Type")
      print(unname(CTloc)) # Does not work properly becuase the 7th element does not have a 'content-Type' Key
      # [[1]]
      # [1] 46
      # [[2]]
      # [1] 45
      # ......
      # [[6]]
      # [1] 54
      # [[7]]
      # integer(0)
      # [[8]]
      # [1] 21
      # ........

      # Add a check to deal with missing 'Content-Type' keys
      sapply(headerList, function(header) {
        CTloc = grep("Content-Type", header)
        if (length(CTloc) == 0) return(NA)
        CTloc
      })


      # Creation of Boolean variable to determine if message contains an attachment 
      hasAttach = sapply(headerList, function(header) {
        CTloc = grep("Content-Type", header)
        if (length(CTloc) == 0) return(FALSE)
        grepl("multi", tolower(header[CTloc])) # grepl() returns a logic value
      })
      
      print(unname(hasAttach)) # Prints out the bollena values for wether it has an attachment or not.  Used unname to reduce clutter
      # [1] FALSE FALSE FALSE FALSE FALSE  TRUE FALSE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE

      #### Attempting to locate 'boundry=' string from those messages that have attachments
      header = sampleSplit[[6]]$header # We know message 6 includes an attachment
      boundaryIdx = grep("boundary=", header) # Finding 'boundry=' within the header
      # [1] 55
      header[boundaryIdx]
      # [1] "    boundary=\"==_Exmh_-1317289252P\";"

      sub(".*boundary=\"(.*)\";.*", "\\1", header[boundaryIdx]) # We want to extract the boundry value, which follows the boundry
      # [1] "==_Exmh_-1317289252P"                              # statement and is grouped, using the ( ) which must be followed
      #?sub()                                                   # by "; giving access to characters located within the ( ) using
                                                                # //1 notatoin.  Complete description on pg 119

      # Try regex on other emails to check flexibility
      # Using message #9
      header2 = headerList[[9]]
      boundaryIdx2 = grep("boundary=", header2)
      # [1] 17
      header2[boundaryIdx2]
      # [1] "Content-Type: multipart/alternative; boundary=Apple-Mail-2-874629474"  ## Pattern is different from message #6
      
      sub('.*boundary="(.*)";.*', "\\1", header2[boundaryIdx2])  # Did not find boundry string correctly due to different patterns
      # [1] "Content-Type: multipart/alternative; boundary=Apple-Mail-2-874629474"

      # Dropping quotation marks from the boundry string, if we eliminate quotation marks, we can remove them from
      # our pattern search as well
      boundary2 = gsub('"', "", header2[boundaryIdx2])

      sub(".*boundary= *(.*);?.*", "\\1", boundary2) # modified sub statement from earlier, to not serach for quotation marks
      # [1] "Apple-Mail-2-874629474"
      
      # Going back and double checking our first one
      boundary = gsub('"', "", header[boundaryIdx])
      sub(".*boundary= *(.*);?.*", "\\1", boundary) # Its not working properly now.
      # [1] "==_Exmh_-1317289252P;"
      
      #We have to modify search slightly, to correct the pattern issue [^;] means except semicolon
      sub(".*boundary= *([^;]*);?.*", "\\1", boundary)
      # [1] "==_Exmh_-1317289252P"

###### Finalized getBoundry () Function - Rev A
getBoundary = function(header) {
  boundaryIdx = grep("boundary=", header)
  boundary = gsub('"', "", header[boundaryIdx])
  gsub(".*boundary= *([^;]*);?.*", "\\1", boundary)
}
#----


      testingBoundry <- sapply(headerList, getBoundary)
      print(unname(testingBoundry))



      # We are now ready to search through the body of the message
      # for attachments
      # Will use message 6 as example: 

      # Dispalying Message 6: Cotains attachment
      sampleSplit[[6]]$body
          # [1] "--==_Exmh_-1317289252P"                                                           
          # [2] "Content-Type: text/plain; charset=us-ascii"                                       
          # [3] ""                                                                                 
          # [4] "> From:  Chris Garrigues <cwg-exmh@DeepEddy.Com>"                                 
          # [5] "> Date:  Wed, 21 Aug 2002 10:40:39 -0500"                                         
          # [6] ">"                                                                                
          # ......
          # [43] "  World War III:  The Wrong-Doers Vs. the Evil-Doers."                            
          # [44] ""                                                                                 
          # [45] ""                                                                                 
          # [46] ""                                                                                 
          # [47] ""                                                                                 
          # [48] "--==_Exmh_-1317289252P"                                                           
          # [49] "Content-Type: application/pgp-signature"                                          
          # [50] ""                                                                                 
          # [51] "-----BEGIN PGP SIGNATURE-----"                                                    
          # [52] "Version: GnuPG v1.0.6 (GNU/Linux)"                                                
          # [53] "Comment: Exmh version 2.2_20000822 06/23/2000"                                    
          # [54] ""                                                                                 
          # [55] "iD8DBQE9ZQJ/K9b4h5R0IUIRAiPuAJwL4mUus5whLNQZC8MsDlGpEdKNrACcDfZH"                 
          # [56] "PcGgN9frLIM+C5Z3vagi2wE="                                                         
          # [57] "=qJoJ"                                                                            
          # [58] "-----END PGP SIGNATURE-----"                                                      
          # [59] ""                                                                                 
          # [60] "--==_Exmh_-1317289252P--"                                                         
          # [61] ""                                                                                 
          # [62] ""                                                                                 
          # [63] ""                                                                                 
          # [64] "_______________________________________________"                                  
          # [65] "Exmh-workers mailing list"                                                        
          # [66] "Exmh-workers@redhat.com"                                                          
          # [67] "https://listman.redhat.com/mailman/listinfo/exmh-workers"                         
          # [68] ""                                                
          
      sampleSplit[[14]]$body # Another example
          # [1] "This is a multi-part message in MIME format."                             
          # [2] ""                                                                         
          # [3] "------=_NextPart_000_0005_01C26412.7545C1D0"                              
          # [4] "Content-Type: text/plain;"                                                
          # [5] "\tcharset=\"iso-8859-1\""                                                  
          # [6] "Content-Transfer-Encoding: 7bit"                                          
          # [7] ""                                                                         
          # [8] "liberalism"                                                               
          # ....                                                                       
          # [27] " http://www.english.upenn.edu/~afilreis/50s/schleslib.html"               
          # [28] ""                                                                         
          # [29] "------=_NextPart_000_0005_01C26412.7545C1D0"                              
          # [30] "Content-Type: application/octet-stream;"                                  
          # [31] "\tname=\"Liberalism in America.url\""                                      
          # [32] "Content-Transfer-Encoding: 7bit"                                          
          # [33] "Content-Disposition: attachment;"                                         
          # [34] "\tfilename=\"Liberalism in America.url\""                                  
          # [35] ""                                                                         
          # [36] "[DEFAULT]"                                                                
          # [37] "BASEURL=http://www.english.upenn.edu/~afilreis/50s/schleslib.html"        
          # [38] "[InternetShortcut]"                                                       
          # [39] "URL=http://www.english.upenn.edu/~afilreis/50s/schleslib.html"            
          # [40] "Modified=E0824ED43364C201DE"                                              
          # [41] ""                                                                         
          # [42] "------=_NextPart_000_0005_01C26412.7545C1D0--"                            
          # [43] ""                                                                         
          # [44] ""                                                                         
          # [45] ""                  

      sampleSplit[[11]]$body # Does not contain an attachment
          # [1] ""                                                                                       
          # [2] "--------------090602010909000705010009"                                                 
          # [3] "Content-Type: text/plain; charset=ISO-8859-1; format=flowed"                            
          # [4] "Content-Transfer-Encoding: 8bit"                                                        
          # [5] ""                                                                                       
          # [6] "Geege wrote:"                                                                           
          # .....                                                                                      
          # [63] "Check out the pictures."                                                                
          # [64] ""                                                                                       
          # [65] ""                                                                                       
          # [66] ""                                                                                       
          # [67] ""                                                                                       
          # [68] "--------------090602010909000705010009--"                                               
          # [69] ""                                                                                       
          # [70] ""                                       
      
      # NOTE: There are 2 occurrences of the boundry string: One before and one after
      # the attachment
      
      sampleSplit[[15]]$body
          # [1] ""                                                                                                                                                                    
          # [2] "--------------080209060700030309080805"                                                                                                                              
          # [3] "Content-Type: text/plain; charset=US-ASCII; format=flowed"                                                                                                           
          # [4] "Content-Transfer-Encoding: 7bit"                                                                                                                                     
          # [5] ""                                                                                                                                                                    
          # [6] "I actually thought of this kind of active chat at AOL (in 1996 I think), "                                                                                           
          # ............
          # [34] ""                                                                                                                                                                    
          # [35] "--------------080209060700030309080805"                                                                                                                              
          # [36] "Content-Type: text/html; charset=US-ASCII"                                                                                                                           
          # [37] "Content-Transfer-Encoding: 7bit"                                                                                                                                     
          # [38] ""                                                                                                                                                                    
          # [39] "<!DOCTYPE html PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\">"                                                                                                   
          # ........
          # [69] ""                                                                                                                                                                    
          # [70] "Yuck"                                                                                                                                                                
          # [71] "  </pre>"                                                                                                                                                            
          # [72] "</blockquote>"                                                                                                                                                       
          # [73] "<br>"               
          # [74] "</body>"                                                                                                                                                             
          # [75] "</html>"                                                                                                                                                             
          # [76] ""                                                                                                                                                                    
          # [77] "--------------080209060700030309080805--"                                                                                                                            
          # [78] ""                                                                                                                                                                    
          # [79] "" 
      
      
      ## We will begin the process of creating a function that does the following:
      ## a) Drop the blank lines before the first boundry string
      ## b) Keep the lines following as part of the first portion of the body and not attachment
      ## c) Use the last line of the email as the end of the attachment if we find no closing boundry string
      
      #Examining message #15 as our test message
      boundary = getBoundary(headerList[[15]]) 
      # [1] "------------080209060700030309080805"
      body = sampleSplit[[15]]$body

      bString = paste("--", boundary, sep = "") # Search body for boundry string preceeded by 2 hyphens
      # [1] "--------------080209060700030309080805"
      bStringLocs = which(bString == body)
      bStringLocs
      #[1]  2 35

      eString = paste("--", boundary, "--", sep = "") # Search for closing boundry string with preceeding and post by 2 hyphens
      # [1] "--------------080209060700030309080805--"
      eStringLoc = which(eString == body)
      eStringLoc
      # [1] 77

      # We locate the first part of the message from the body, minus the attachments
      msg = body[ (bStringLocs[1] + 1) : (bStringLocs[2] - 1)]
      tail(msg,5)
        # [28] ">Yuck"                                                                     
        # [29] ">  "                                                                       
        # [30] ">"                                                                         
        # [31] ""                                                                          
        # [32] ""  

      # Adding the lines after the attachment to the message
      msg = c(msg, body[ (eStringLoc + 1) : length(body) ])
      tail(msg,7)
        # [28] ">Yuck"                                                                     
        # [29] ">  "                                                                       
        # [30] ">"                                                                         
        # [31] ""                                                                          
        # [32] ""                                                                          
        # [33] ""                                                                          
        # [34] ""


#----------- dropAttach function () - Rev A ----------
# Function encompasses the techniques shown above for
# removing Attachments from the body portion of the
# message
dropAttach = function(body, boundary){
  
  bString = paste("--", boundary, sep = "")
  bStringLocs = which(bString == body)
  
  if (length(bStringLocs) <= 1) return(body)
  
  eString = paste("--", boundary, "--", sep = "")
  eStringLoc = which(eString == body)
  if (length(eStringLoc) == 0) # no exit string
    return(body[ (bStringLocs[1] + 1) : (bStringLocs[2] - 1)])
  
  n = length(body)
  if (eStringLoc < n) # concatinates bottom of message to body if end string
                      # is not located at end of body
    return( body[ c( (bStringLocs[1] + 1) : (bStringLocs[2] - 1), 
                     ( (eStringLoc + 1) : n )) ] )
  
  return( body[ (bStringLocs[1] + 1) : (bStringLocs[2] - 1) ]) #If eStringLoc = n, body is defined as 
                                                               # message between the bStrings only
}
#--------

      # Trying function on message 15
      msg15 =dropAttach(sampleSplit[[15]]$body, getBoundary(headerList[[15]]) ) 
          # [1] "Content-Type: text/plain; charset=US-ASCII; format=flowed"                 
          # [2] "Content-Transfer-Encoding: 7bit"                                           
          # [3] ""                                                                          
          # [4] "I actually thought of this kind of active chat at AOL (in 1996 I think), " 
          # ......                                                                         
          # [26] ">\"Oh, you're going to Seattle? I can get you airline tickets for less\""  
          # [27] ">"                                                                         
          # [28] ">Yuck"                                                                     
          # [29] ">  "                                                                       
          # [30] ">"                                                                         
          # [31] ""                                                                          
          # [32] ""                                                                          
          # [33] ""                                                                          
          # [34] "" 
      
      
# ------ 3.5.3 Extracting Words from the Message Body ----------
      # Displaying Message 1
      head(sampleSplit[[1]]$body)
          # [1] "    Date:        Wed, 21 Aug 2002 10:54:46 -0500"                           
          # [2] "    From:        Chris Garrigues <cwg-dated-1030377287.06fa6d@DeepEddy.Com>"
          # [3] "    Message-ID:  <1029945287.4797.TMDA@deepeddy.vircio.com>"                
          # [4] ""                                                                           
          # [5] ""                                                                           
          # [6] "  | I can't reproduce this error." 
      
      # Displaying Message 3
      msg = sampleSplit[[3]]$body
      head(msg)
          # [1] "Man Threatens Explosion In Moscow "                                        
          # [2] ""                                                                          
          # [3] "Thursday August 22, 2002 1:40 PM"                                          
          # [4] "MOSCOW (AP) - Security officers on Thursday seized an unidentified man who"
          # [5] "said he was armed with explosives and threatened to blow up his truck in"  
          # [6] "front of Russia's Federal Security Services headquarters in Moscow, NTV"

      # Selecting lines 1,3,26,27 to extract txt from: testing
      msg[ c(1, 3, 26, 27) ]
          # [1] "Man Threatens Explosion In Moscow "         "Thursday August 22, 2002 1:40 PM"                        
          # [3] "4 DVDs Free +s&p Join Now"                  "http://us.click.yahoo.com/pt6YBB/NXiEAA/mG3HAA/7gSolB/TM"

      # Converting all txt to lowercase; Discord punctuations and numbers and replace them with spaces
      cleanMsg = tolower(gsub("[[:punct:]0-9[:blank:]]+", " ", msg))
      cleanMsg[ c(1, 3, 26, 27) ]
          # [1] "man threatens explosion in moscow "           "thursday august pm"                                   
          # [3] " dvds free s p join now"                      "http us click yahoo com pt ybb nxieaa mg haa gsolb tm"

# Loading 'Text Mining Package'
# https://cran.r-project.org/web/packages/tm/tm.pdf
# Simple example of how to handle plural, possessive and 
# tm package includes a list of stopwords' words that do not add value to classification
# and appear often in text (a, an, me, my, we, same,....)
library(tm)
stopWords = stopwords()
cleanSW = tolower(gsub("[[:punct:]0-9[:blank:]]+", " ", stopWords))
SWords = unlist(strsplit(cleanSW, "[[:blank:]]+"))
SWords = SWords[ nchar(SWords) > 1 ]
stopWords = unique(SWords)


      # Creating a vector of words from the cleanMsg. utilizing 'blanks' for the split variable
      words = unlist(strsplit(cleanMsg, "[[:blank:]]+"))
      length(words) #[1] 272
      # Dropping 0 or 1 letter words
      words = words[ nchar(words) > 1 ]
      length(words) #[1] 260
      # Removal of any words that are in the stopWords
      words = words[ !( words %in% stopWords) ]
      length(words) #[1] 173
      head(words)
          # [1] "man"       "threatens" "explosion" "moscow"    "thursday"  "august" 

      #----------- cleanText function () - Rev A ----------
      # Function removes punctuation and numbers from message
      cleanText =
        function(msg)   {
          tolower(gsub("[[:punct:]0-9[:space:][:blank:]]+", " ", msg))
        }
      #----
            
      #----------- findMsgWords function () - Rev A ----------
      # splits message into individual words and collects unique words
      # strips stopWords from words list
      findMsgWords = 
        function(msg, stopWords) {
          if(is.null(msg))
            return(character())
          
          words = unique(unlist(strsplit(cleanText(msg), "[[:blank:]\t]+")))
          
          # drop empty and 1 letter words
          words = words[ nchar(words) > 1]
          words = words[ !( words %in% stopWords) ]
          invisible(words)
        }
      #-----
      

#----------- processAllWords function () - Rev A ----------
# Reads all files, splits message into body,header
# Strips attachments; Eliminates non-email messages
# Splits message into individual words and collects unique words
# strips stopWords from words list.
processAllWords = function(dirName, stopWords)
{
  # read all files in the directory
  fileNames = list.files(dirName, full.names = TRUE)
  # drop files that are not email, i.e., cmds
  notEmail = grep("cmds$", fileNames)
  if ( length(notEmail) > 0) fileNames = fileNames[ - notEmail ]
  
  messages = lapply(fileNames, readLines, encoding = "latin1")
  
  # split header and body
  emailSplit = lapply(messages, splitMessage)
  # put body and header in own lists
  bodyList = lapply(emailSplit, function(msg) msg$body)
  headerList = lapply(emailSplit, function(msg) msg$header)
  rm(emailSplit)
  
  # determine which messages have attachments
  hasAttach = sapply(headerList, function(header) {
    CTloc = grep("Content-Type", header)
    if (length(CTloc) == 0) return(0)
    multi = grep("multi", tolower(header[CTloc])) 
    if (length(multi) == 0) return(0)
    multi
  })
  
  hasAttach = which(hasAttach > 0)
  
  # find boundary strings for messages with attachments
  boundaries = sapply(headerList[hasAttach], getBoundary)
  
  # drop attachments from message body
  bodyList[hasAttach] = mapply(dropAttach, bodyList[hasAttach], 
                               boundaries, SIMPLIFY = FALSE)
  
  # extract words from body
  msgWordsList = lapply(bodyList, findMsgWords, stopWords)
  
  invisible(msgWordsList)
}
# --------
      
# Processing all messages from SpamAssain database, using
# messages in directories;creating a words list of 5 character
# vectors, each containing the words list for each message in the subdirectory
msgWordsList = lapply(fullDirNames, processAllWords, 
                      stopWords = stopWords)
# Displaying 1st list (easy_ham directory), 12th message [message 00011.fbcde1b4833bdbaaf0ced723edd6e355]
msgWordsList[[1]][12]
  # [1] "hello"        "seen"         "discussed"    "article"      "approach"     "thank"        "http"         "www"         
  # [9] "paulgraham"   "com"          "spam"         "html"         "hell"         "rules"        "trying"       "accomplish"  
  # [17] "something"    "thomas"       "alva"         "edison"       "sf"           "net"          "email"        "sponsored"   
  # [25] "osdn"         "tired"        "old"          "cell"         "phone"        "get"          "new"          "free"        
  # [33] "https"        "inphonic"     "asp"          "sourceforge"  "refcode"      "vs"           "spamassassin" "devel"       
  # [41] "mailing"      "list"         "lists"        "listinfo" 

# We want to classify each message as spam or ham
# we create a counter of the number of msgs in each folder
numMsgs = sapply(msgWordsList, length)
numMsgs
  # [1] 5051 1400  500 1000 1397

# Create a Boolean value for is spam for each message
isSpam = rep(c(FALSE, FALSE, FALSE, TRUE, TRUE), numMsgs)

# we flatten the list into a single list
msgWordsList = unlist(msgWordsList, recursive = FALSE)
length(msgWordsList) #[1] 9348
msgWordsList[[8000]]
isSpam[8000]

##########################################################
################ Beginning of Naive Bayes Classifier
##########################################################

numEmail = length(isSpam) #[1] 9348
numSpam = sum(isSpam) #[1] 2397 (Using True values of isSpam)
numHam = numEmail - numSpam #[1] 6951

set.seed(418910)

# Using sample() function to split the messages into test and traning datasets
# Want to maintain the % split between Spam and Ham that is present in the overall
# dataset
# creating a list of indexes chosen at random for 1/3 of the values of each
# type of message (spam,ham)
testSpamIdx = sample(numSpam, size = floor(numSpam/3))
testHamIdx = sample(numHam, size = floor(numHam/3))

# Note: All spam messages are first then all ham messages are 2nd.  THis will come in handy in just a little bit
testMsgWords = c((msgWordsList[isSpam])[testSpamIdx],   # msgWordsList[isSpam] only selects from messages that correspond
                                                        # with isSpam is True. We then subdivide these messages by the indexes
                                                        # chosen to be part of the test dataset [testSpamIdx]
                 (msgWordsList[!isSpam])[testHamIdx] )  # We repeat this process for the msgWordsList[!isSpam] from messages
                                                        # that are not spam !isSpam and subdivide these ham messages by the indexes
                                                        # chosen to be part of the test dataset [testHamIdx]
length(testMsgWords) #[1] 3116

trainMsgWords = c((msgWordsList[isSpam])[ - testSpamIdx], # Conduct a similar subdivision of messages, this time taking all of
                  (msgWordsList[!isSpam])[ - testHamIdx]) # the remaining spam and ham messages and placing them in training dataset
length(trainMsgWords) #[1] 6232

#Creating an indicator/classification variable for test messages [Utilizing the Spam than Ham ordering]
testIsSpam = rep(c(TRUE, FALSE), # repeats TRUE for length of testSpamIdx and FALSE for length of testHamIdx
                 c(length(testSpamIdx), length(testHamIdx)))
length(testIsSpam) #[1] 3116

trainIsSpam = rep(c(TRUE, FALSE), # repeats TRUE for remaining spam messasge (numSpam-length(testSpamIdx)) indexes and FALSE for remaining ham messages
                  c(numSpam - length(testSpamIdx), 
                    numHam - length(testHamIdx)))
length(trainIsSpam) #[1] 6232

# Creating a bag of words (BOW) of all unique words that appear in the messages
bow = unique(unlist(trainMsgWords))
length(bow) # 80059  (slightly different from the book's value 80481)

# Initiating a count list for all unique words that appear in the messages
spamWordCounts = rep(0, length(bow))
length(spamWordCounts) # [1] 80059

# Creating a named numeric vector for each unique word in messages
names(spamWordCounts) = bow
head(spamWordCounts)
    # doctype     html       public          dtd    transitional       en 
    # 0            0            0            0            0            0 

# We process each Spam message and retrive only the unique words. tmp is a list of 1598 elements of character vectors (equal
# to the number of trainIsSpam messages that are categorized as spam)
tmp = lapply(trainMsgWords[trainIsSpam], unique)

# Since we are using unique words, the values in tt represent the frequency of spam messages that the particular word
# appears in. Using a frequenct table we sums up the number of instances across all messages
tt = table( unlist(tmp) )
tt[1:5]
      # \aÿëa   \033m  \036þw \177ízë  \177ùþ (gibberish becusae we did not prune the words to only be dictionary words)
      # 1       2       2       1       4 

#We update spamWordCount with th frequency values from the frequency table
spamWordCounts[ names(tt) ] = tt
head(spamWordCounts)
      # doctype         html       public          dtd transitional           en 
      # 87          811          141           85           75          170 

#----------- computeFreqs function () - Rev A ----------
# Compute the frequency of each word's appearance in spam and ham messages.
# Computes the present and absent log odds and place them in a matrix
# Inputs bow, trainIsSpam(noting which messages are spam)
# and  wordsList for all messages in training dataset
computeFreqs =
  function(wordsList, spam, bow = unique(unlist(wordsList)))
  {
    # create a matrix for spam, ham, and log odds
    wordTable = matrix(0.5, nrow = 4, ncol = length(bow), 
                       dimnames = list(c("spam", "ham", 
                                         "presentLogOdds", 
                                         "absentLogOdds"),  bow))
    
    # For each spam message, add 1 to counts for words in message
    counts.spam = table(unlist(lapply(wordsList[spam], unique)))
    wordTable["spam", names(counts.spam)] = counts.spam + .5
    
    # Similarly for ham messages
    counts.ham = table(unlist(lapply(wordsList[!spam], unique)))  
    wordTable["ham", names(counts.ham)] = counts.ham + .5  
    
    
    # Find the total number of spam and ham
    numSpam = sum(spam)
    numHam = length(spam) - numSpam
    
    # Prob(word|spam) and Prob(word | ham)
    wordTable["spam", ] = wordTable["spam", ]/(numSpam + .5)
    wordTable["ham", ] = wordTable["ham", ]/(numHam + .5)
    
    # log odds
    wordTable["presentLogOdds", ] = 
      log(wordTable["spam",]) - log(wordTable["ham", ])
    wordTable["absentLogOdds", ] = 
      log((1 - wordTable["spam", ])) - log((1 -wordTable["ham", ]))
    
    invisible(wordTable)
  }
#------

# Compute Frequwncy dataframe for messages
trainTable = computeFreqs(trainMsgWords, trainIsSpam)
    class(trainTable) # [1] "matrix"
    dim(trainTable) # [1]     4 80059
    trainTable[1:4,200:205]
          #                   linux         ie         irish       users      group     mailman
          # spam            0.04347826  0.06537379  0.04410385  0.07100407  0.0603691  0.08977166
          # ham             0.17013702  0.12482468  0.10648398  0.22753264  0.1992664  0.34577624
          # presentLogOdds -1.36434303 -0.64678885 -0.88144752 -1.16455653 -1.1941652 -1.34852252
          # absentLogOdds   0.14204291  0.06572245  0.06748501  0.18451460  0.1599588  0.33024606

    # ----------------- Test Code ---------------
    # Now we can construct the log likelihood ratio for a message.  We Sum the 
    # presentLogOdds for words that appear in the message and the Sum of absentLogOdds for words not 
    # in the message
    newMsg = testMsgWords[[1]] #Taking message 1 for example

    newMsg = newMsg[!is.na(match(newMsg, colnames(trainTable)))] #Drop any words that are not in BOW
    length(newMsg) #[1] 451
    
    present = colnames(trainTable) %in% newMsg #Boolean variable to indicate if words that are in message (TRUE or FALSE)
    sum(present == TRUE) # [1] 451            # out of BOW


    # Compute the log of the probability a message is spam vs ham
    sum(trainTable["presentLogOdds", present]) + 
      sum(trainTable["absentLogOdds", !present])
      # [1] 255.0476 
    
    # We know the first message of testMsgWords is spam, so 255.04 seems correct.  
    # Now, lets calculate the logliklihood odds of a ham message

    newMsg = testMsgWords[[ which(!testIsSpam)[1] ]] # First ham message in the list (67 words)
    newMsg = newMsg[!is.na(match(newMsg, colnames(trainTable)))] #Drop any words that are not in BOW
    length(newMsg) #[1] 67
    present = (colnames(trainTable) %in% newMsg)
    
    # Compute the log of the probability a message is spam vs ham
    sum(trainTable["presentLogOdds", present]) + 
      sum(trainTable["absentLogOdds", !present])
      # [1] -124.3275  Very low / negative number; therefore, message has a high probability of being ham
    
    

#----------- computeMsgLLR function () - Rev A ----------
# Creating a function to compute the log likelihood of a message being
# spam.    
computeMsgLLR = function(words, freqTable) 
{
  # Discards words not in training data.
  words = words[!is.na(match(words, colnames(freqTable)))]
  
  # Find which words are present
  present = colnames(freqTable) %in% words
  
  sum(freqTable["presentLogOdds", present]) +
    sum(freqTable["absentLogOdds", !present])
}
#-----
    
# Calling ComputeMsgLLR function to compute the LLR for all messages in the trainTable
testLLR = sapply(testMsgWords, computeMsgLLR, trainTable)

# Creating summary data for training dataset by spam classification TRUE or FALSE
tapply(testLLR, testIsSpam, summary)
    # $`FALSE`
    #     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
    # -1361.89  -127.06  -101.18  -116.25   -81.26   700.23 
    # 
    # $`TRUE`
    #     Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
    # -60.574     6.369    49.837   137.546   131.719 23518.028 

#------- Figure 3.1 - Boxplot LogLikelihood by true Classification ----
# Plots summary data for loglikelihood for Training Dataset
pdf("./Figures/Fig3.1_SP_Boxplot.pdf", width = 6, height = 6)
spamLab = c("ham", "spam")[1 + testIsSpam] #Converting spam T/F label to ham & spam
boxplot(testLLR ~ spamLab, cex = .4, pch=19, col = 'lightblue', 
        ylab = "Log Likelihood Ratio",
        main = "Log Likelihood Ratio for Spam and Ham\n 3116 Test Messages",
        ylim=c(-500, 500))
dev.off()
#-----



#----------- typeIErrorRate function () - Rev A ----------
#Create function to determine the Type I error rate for 
# certain values of tau   
typeIErrorRate = 
  function(tau, llrVals, spam)
  {
    classify = llrVals > tau
    sum(classify & !spam)/sum(!spam)
  }
#----

# Try tau of 0 for test dataset
typeIErrorRate(0, testLLR,testIsSpam)
    #[1] 0.003452741

# Try tau of -20 for test dataset
typeIErrorRate(-20, testLLR,testIsSpam)
    #[1] 0.005610703


#----------- typeIErrorRate function () - Rev B ----------
#Create function to determine the Type I error rate for 
# The function looks at the llrVals values for ham messages
# and recognizing that the number of Type 1 errors decrease by
# 1 at each of these values and so i/(number of ham messages) 
typeIErrorRates = 
  function(llrVals, isSpam) 
  {
    o = order(llrVals) #Increasing order of index
    llrVals =  llrVals[o]
    isSpam = isSpam[o]
    
    idx = which(!isSpam) #is not Spam
    N = length(idx)
    list(error = (N:1)/N, values = llrVals[idx]) # Creats a list of xI$error and xI$values
  }
#-----

#----------- typeIIErrorRate function () - Rev A ----------
#Create function to determine the Type II error rate for 
# The function looks at the llrVals values for ham messages
# and recognizing that the number of Type II errors decrease by
# 1 at each of these values and so i/(number of spam messages) 
typeIIErrorRates = function(llrVals, isSpam) {
  
  o = order(llrVals)
  llrVals =  llrVals[o]
  isSpam = isSpam[o]
  
  
  idx = which(isSpam) #is Spam
  N = length(idx)
  list(error = (1:(N))/N, values = llrVals[idx]) # Creats a list of xII$error and xII$values
}  
#----

xI = typeIErrorRates(testLLR, testIsSpam) 
xII = typeIIErrorRates(testLLR, testIsSpam)
tau01 = round(min(xI$values[xI$error <= 0.01])) # Can adjust Type I error rate; tau value will be adjusted 
t2 = max(xII$error[ xII$values < tau01 ])

pdf("./Figures/Fig3.2_LinePlotTypeI+IIErrors.pdf", width = 8, height = 6)
library(RColorBrewer)
cols = brewer.pal(9, "Set1")[c(3, 4, 5)]
plot(xII$error ~ xII$values,  type = "l", col = cols[1], lwd = 3,
     xlim = c(-300, 250), ylim = c(0, 1),
     xlab = "Log Likelihood Ratio Values", ylab="Error Rate")
points(xI$error ~ xI$values, type = "l", col = cols[2], lwd = 3)
legend(x = 50, y = 0.4, fill = c(cols[2], cols[1]),
       legend = c("Classify Ham as Spam", 
                  "Classify Spam as Ham"), cex = 0.8,
       bty = "n")
abline(h=0.01, col ="grey", lwd = 3, lty = 2)
text(-250, 0.05, pos = 4, "Type I Error = 0.01", col = cols[2])

mtext(tau01, side = 1, line = 0.5, at = tau01, col = cols[3])
segments(x0 = tau01, y0 = -.50, x1 = tau01, y1 = t2, 
         lwd = 2, col = "grey")
text(tau01 + 20, 0.05, pos = 4,
     paste("Type II Error = ", round(t2, digits = 2)), 
     col = cols[1])

dev.off()
#----

####---- K Fold Cross Valaidation -----------------
k = 5
numTrain = length(trainMsgWords)
partK = sample(numTrain)
tot = k * floor(numTrain/k)
partK = matrix(partK[1:tot], ncol = k) # converting to matrix of k columns

testFoldOdds = NULL
for (i in 1:k) {
  foldIdx = partK[ , i]
  trainTabFold = computeFreqs(trainMsgWords[-foldIdx], trainIsSpam[-foldIdx])
  testFoldOdds = c(testFoldOdds, 
                   sapply(trainMsgWords[ foldIdx ], computeMsgLLR, trainTabFold))
}

testFoldSpam = NULL
for (i in 1:k) {
  foldIdx = partK[ , i]
  testFoldSpam = c(testFoldSpam, trainIsSpam[foldIdx])
}

xFoldI = typeIErrorRates(testFoldOdds, testFoldSpam)
xFoldII = typeIIErrorRates(testFoldOdds, testFoldSpam)
tauFoldI = round(min(xFoldI$values[xFoldI$error <= 0.01]))
tFold2 = xFoldII$error[ xFoldII$values < tauFoldI ]


######### Should plot results of K-Fold CV into a graph ********
testFoldOdds[2]

          ############# Sample Code - Not implemented
          smallNums = rep((1/2)^40, 2000000)
          largeNum = 10000
          
          print(sum(smallNums), digits = 20)
          
          print(largeNum + sum(smallNums), digits = 20)
          
          for (i in 1:length(smallNums)) {
            largeNum = largeNum + smallNums[i]
          }
          print(largeNum, digits = 20)
          
          sampleSplit = lapply(sampleEmail, splitMessage)


######################## Section 3.7 Recursive Partitioning and Classification Trees ###################
########################################################################################################
########################################################################################################

# Reprocessing Dataset from Scratch
# We will reuse some of the functions created in the Naive Bayes section
# to process the messages for Recursive Partitioning
#  We will be using the message header for the majority of this analysis section
# see page 140 for additional details about strategy for handling/parsing messages

          
          #------- Test Code ---------
      # Reusing sampleSplit variable from NB
      header = sampleSplit[[1]]$header
      header[1:12] # Displaying
        # [1] "From exmh-workers-admin@redhat.com  Thu Aug 22 12:36:23 2002"                                      
        # [2] "Return-Path: <exmh-workers-admin@spamassassin.taint.org>"                                          
        # [3] "Delivered-To: zzzz@localhost.netnoteinc.com"                                                       
        # [4] "Received: from localhost (localhost [127.0.0.1])"                                                  
        # [5] "\tby phobos.labs.netnoteinc.com (Postfix) with ESMTP id D03E543C36"                                 
        # [6] "\tfor <zzzz@localhost>; Thu, 22 Aug 2002 07:36:16 -0400 (EDT)"                                      
        # [7] "Received: from phobos [127.0.0.1]"                                                                 
        # [8] "\tby localhost with IMAP (fetchmail-5.9.0)"                                                         
        # [9] "\tfor zzzz@localhost (single-drop); Thu, 22 Aug 2002 12:36:16 +0100 (IST)"                          
        # [10] "Received: from listman.spamassassin.taint.org (listman.spamassassin.taint.org [66.187.233.211]) by"
        # [11] "    dogma.slashnull.org (8.11.6/8.11.6) with ESMTP id g7MBYrZ04811 for"                            
        # [12] "    <zzzz-exmh@spamassassin.taint.org>; Thu, 22 Aug 2002 12:34:53 +0100" 

# The plan is to process the message header by converting the key:value pairs in a named vector
# Observations:
      # Some key:value pairs appear on multiple lines
      # First line is not key:value format
      # Colons appear in the value portion of time Key:value pair
      

      # Processing line 1 of header using regex.  Replacing 'From' with 'Top-From:' using regex
      # Sustituting a value that can be read as a key:value pair
      header[1] = sub("^From", "Top-From:", header[1]) # processing line 1 of header using regex
      header[1] #[1] "Top-From: exmh-workers-admin@redhat.com  Thu Aug 22 12:36:23 2002"


      #?textConnection()
      #?read.dcf()
      headerPieces = read.dcf(textConnection(header), all = TRUE)
      # Using read.dcf() in combo with textConnection() function, converts the header 
      # into a dataframe with 26 variables, one for each Key:value pair 
      headerPieces[, "Delivered-To"] 
      # Delivered-To is a variable in headerPieces dataframe with a list of values that match the key 'Delivered-To'
          #[1] "zzzz@localhost.netnoteinc.com"               "exmh-workers@listman.spamassassin.taint.org"
      headerPieces[,'Delivered-To'][[1]][2]
          #[1] "exmh-workers@listman.spamassassin.taint.org"


      headerVec = unlist(headerPieces) # converted to a named character vector (When a key has a list of values,
                                      # it breaks them up into separate named character vectors)
      head(headerVec)
        # Top-From 
        # "exmh-workers-admin@redhat.com  Thu Aug 22 12:36:23 2002" 
        # Return-Path 
        # "<exmh-workers-admin@spamassassin.taint.org>" 
        # Delivered-To1 
        # "zzzz@localhost.netnoteinc.com" 
        # Delivered-To2 
        # "exmh-workers@listman.spamassassin.taint.org" 
        # Received1 
        # "from localhost (localhost [127.0.0.1])\nby phobos.labs.netnoteinc.com (Postfix) with ESMTP id D03E543C36\nfor <zzzz@localhost>; Thu, 22 Aug 2002 07:36:16 -0400 (EDT)" 
        # Received2 
        # "from phobos [127.0.0.1]\nby localhost with IMAP (fetchmail-5.9.0)\nfor zzzz@localhost (single-drop); Thu, 22 Aug 2002 12:36:16 +0100 (IST)" 
      
      # creates a named vector with the number of values(instances) for each Key
      dupKeys = sapply(headerPieces, function(x) length(unlist(x)))
      head(dupKeys)
          # Top-From  Return-Path Delivered-To    Received     From     To 
          #   1            1            2          10          1         1 

      # Renames headerVec named vector with proper names , using dupKeys values for
      # number of times each name is repeated (removing Deli...To1, Deli...To2) 
      names(headerVec) = rep(colnames(headerPieces), dupKeys)

      headerVec[ which(names(headerVec) == "Delivered-To") ]
          # Delivered-To                                  Delivered-To 
          # "zzzz@localhost.netnoteinc.com" "exmh-workers@listman.spamassassin.taint.org" 

      length(headerVec) #[1] 36

      length(unique(names(headerVec))) #[1] 26

#----------- processHeader function () - Rev A ----------
# Create function that performs the initial parsing of the 
# message header:
# Convert 1st line to key:value format
# process header to key:value matrix using read.dcf function
# unlist header into vector matrix and rename the named character
# vectors with proper names (removing Del..To1, Del..To2 in naming convention)
processHeader = function(header)
{
  # modify the first line to create a key:value pair
  header[1] = sub("^From", "Top-From:", header[1])
  
  headerMat = read.dcf(textConnection(header), all = TRUE)
  headerVec = unlist(headerMat)
  
  dupKeys = sapply(headerMat, function(x) length(unlist(x)))
  names(headerVec) = rep(colnames(headerMat), dupKeys)
  
  return(headerVec)
}
#----
      
# using lapply to process all 15 headers within sampleSplit, breaking them into named character vectors
headerList = lapply(sampleSplit, 
                    function(msg) {
                      processHeader(msg$header)} )


    # extracting the contentTypes from the headerList and storing it as a variable
    contentTypes = sapply(headerList, function(header) 
      header["Content-Type"])
          # ./SpamAssassinMessages//messages/easy_ham/00001.7c53336b37003a9286aba55d2945844c.Content-Type 
          # "text/plain; charset=us-ascii" 
          # ./SpamAssassinMessages//messages/easy_ham/00002.9c4069e25e1ef370c078db7ee85ff9ac.Content-Type 
          # "text/plain; charset=US-ASCII" 
          # ./SpamAssassinMessages//messages/easy_ham/00003.860e3c3cee1b42ead714c5c874fe25f7.Content-Type 
          # "text/plain; charset=US-ASCII" 
    
    names(contentTypes) = NULL # removing variable names
    head(contentTypes)
          # [1] "text/plain; charset=us-ascii"                                                                                   
          # [2] "text/plain; charset=US-ASCII"                                                                                   
          # [3] "text/plain; charset=US-ASCII"                                                                                   
          # [4] "text/plain; charset=\"us-ascii\""                                                                               
          # [5] "text/plain; charset=US-ASCII"                                                                                   
          # [6] "multipart/signed;\nboundary=\"==_Exmh_-1317289252P\";\nmicalg=pgp-sha1;\nprotocol=\"application/pgp-signature\""


###########################################
### 3.8.2  Processing Attachments #########

    # Identifying which messages have attachments using contentType containing 'multi', similar to NB
    hasAttach = grep("^ *multi", tolower(contentTypes))
    hasAttach
     # [1]  6  8  9 10 11 12 13 14 15

    # Using getBoundry function from NB section, to retrive boundry string
    boundaries = getBoundary(contentTypes[ hasAttach ])
    boundaries
    # [1] "==_Exmh_-1317289252P"    "----=_NextPart_000_00C1_01C25017.F2F04E20"   "Apple-Mail-2-874629474"                   
    # [4] "==_Exmh_-518574644P"     "------------090602010909000705010009"        "==_Exmh_-451422450P"                      
    # [7] "==_Exmh_267413022P"      "----=_NextPart_000_0005_01C26412.7545C1D0"   "------------080209060700030309080805" 

    boundary = boundaries[9] # Using message #15 (9th in boundries list) as example
    body = sampleSplit[[15]]$body 

    # Using same delimiters we used for NB to locate attachments by searching for the beginning boundry strings
    bString = paste("--", boundary, sep = "") #"--------------080209060700030309080805"
    bStringLocs = which(bString == body)
    bStringLocs #[1]  2 35  (Lines 2 and 35 mark the start of the body and start of the single attachment)

    # We next find the ending boundry string
    eString = paste("--", boundary, "--", sep = "") #[1] "--------------080209060700030309080805--"
    eStringLoc = which(eString == body)
    eStringLoc #[1] 77

    # Size of Body/Attachment
    diff(c(bStringLocs[-1], eStringLoc)) #[1] 42


#----------- processAttach function () - Rev A ----------
# Create function that returns a list with 2 elements
# a) the first part of the body (message and no attachment) 
# b) a dataframe attachDF with 2 variables aLen, aType 
#    length and type of attachment

### This code has mistakes in it - and we fix them later!
processAttach = function(body, contentType){
  
  boundary = getBoundary(contentType)
  
  bString = paste("--", boundary, "$", sep = "")
  bStringLocs = grep(bString, body)
  
  eString = paste("--", boundary, "--$", sep = "")
  eStringLoc = grep(eString, body)
  
  n = length(body)
  
  if (length(eStringLoc) == 0) eStringLoc = n + 1 #no end strings present
  if (length(bStringLocs) == 1) attachLocs = NULL #no attachments if bstringLoc ==1 (only body)
  else attachLocs = c(bStringLocs[-1],  eStringLoc) # initializing attachment boundry locations
  
  msg = body[ (bStringLocs[1] + 1) : min(n, (bStringLocs[2] - 1), 
                                         na.rm = TRUE)]
  
  if ( eStringLoc < n )
    msg = c(msg, body[ (eStringLoc + 1) : n ])
  
  if ( !is.null(attachLocs) ) {
    attachLens = diff(attachLocs, lag = 1) 
    attachTypes = mapply(function(begL, endL) {
      contentTypeLoc = grep("[Cc]ontent-[Tt]ype", body[ (begL + 1) : (endL - 1)])
      contentType = body[ begL + contentTypeLoc]
      contentType = gsub('"', "", contentType )
      MIMEType = sub(" *Content-Type: *([^;]*);?.*", "\\1", contentType)
      return(MIMEType)
    }, attachLocs[-length(attachLocs)], attachLocs[-1])
  }
  
  if (is.null(attachLocs)) return(list(body = msg, attachInfo = NULL) )
  else return(list(body = msg, 
                   attachDF = data.frame(aLen = attachLens, 
                                         aType = attachTypes,
                                         stringsAsFactors = FALSE)))                                
}
#------

    # Still using 15 messages in sampleSplit as test code
    bodyList = lapply(sampleSplit, function(msg) msg$body)

    # contains a list of 9 
    attList = mapply(processAttach, bodyList[hasAttach], 
                 contentTypes[hasAttach], 
                 SIMPLIFY = FALSE)
    # examining the attachment lengths
    lens = sapply(attList, function(processedA) 
      processedA$attachDF$aLen)
    head(lens)
        # $`./SpamAssassinMessages//messages/easy_ham/00014.cb20e10b2bfcb8210a1c310798532a57`
        # [1] 12
        # $`./SpamAssassinMessages//messages/easy_ham/00062.009f5a1a8fa88f0b38299ad01562bb37`
        # [1] 44 44
        # $`./SpamAssassinMessages//messages/easy_ham/00063.0acbc484a73f0e0b727e06c100d8df7b`
        # [1] 83
        # $`./SpamAssassinMessages//messages/easy_ham/0030.77828e31de08ebb58b583688b87524cc`
        # [1] 12
        # $`./SpamAssassinMessages//messages/easy_ham/00368.f86324a03e7ae7070cc40f302385f5d3`
        # NULL
        # $`./SpamAssassinMessages//messages/easy_ham/00389.8606961eaeef7b921ce1c53773248d69`
        # [1] 12

    attList[[2]]$attachDF
        #     aLen        aType
        # 1   44      text/html
        # 2   44      <META http-equiv=3DContent-Type content=3Dtext/html; =
    # In message 2, Attachment 2: It appears that the 'Content-Type' appears to be
    # located in an HTML tag, see below (issue needs to addressed in function call)
    
    
    body = bodyList[hasAttach][[2]]
    length(body)  #[1] 86
    body[35:45] #Look at lines [3] and [9]
        # [1] ""                                                                
        # [2] "------=_NextPart_000_00C1_01C25017.F2F04E20"                     
        # [3] "Content-Type: text/html;"                                        
        # [4] "\tcharset=\"Windows-1252\""                                       
        # [5] "Content-Transfer-Encoding: quoted-printable"                     
        # [6] ""                                                                
        # [7] "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.0 Transitional//EN\">"
        # [8] "<HTML><HEAD>"                                                    
        # [9] "<META http-equiv=3DContent-Type content=3D\"text/html; ="        
        # [10] "charset=3Dwindows-1252\">"                                       
        # [11] "<META content=3D\"MSHTML 6.00.2716.2200\" name=3DGENERATOR>" 

processAttach = function(body, contentType){
  
  n = length(body)
  boundary = getBoundary(contentType)
  
  bString = paste("--", boundary, sep = "")
  bStringLocs = which(bString == body)
  eString = paste("--", boundary, "--", sep = "")
  eStringLoc = which(eString == body)
  
  if (length(eStringLoc) == 0) eStringLoc = n
  if (length(bStringLocs) <= 1) {
    attachLocs = NULL
    msgLastLine = n
    if (length(bStringLocs) == 0) bStringLocs = 0
  } else {
    attachLocs = c(bStringLocs[ -1 ],  eStringLoc)
    msgLastLine = bStringLocs[2] - 1
  }
  
  msg = body[ (bStringLocs[1] + 1) : msgLastLine] 
  if ( eStringLoc < n )
    msg = c(msg, body[ (eStringLoc + 1) : n ])
  
  if ( !is.null(attachLocs) ) {
    attachLens = diff(attachLocs, lag = 1) 
    attachTypes = mapply(function(begL, endL) {
      CTloc = grep("^[Cc]ontent-[Tt]ype", body[ (begL + 1) : (endL - 1)])
      if ( length(CTloc) == 0 ) {
        MIMEType = NA
      } else {
        CTval = body[ begL + CTloc[1] ]
        CTval = gsub('"', "", CTval )
        MIMEType = sub(" *[Cc]ontent-[Tt]ype: *([^;]*);?.*", "\\1", CTval)   
      }
      return(MIMEType)
    }, attachLocs[-length(attachLocs)], attachLocs[-1])
  }
  
  if (is.null(attachLocs)) return(list(body = msg, attachDF = NULL) )
  return(list(body = msg, 
              attachDF = data.frame(aLen = attachLens, 
                                    aType = unlist(attachTypes),
                                    stringsAsFactors = FALSE)))                                
}                       

readEmail = function(dirName) {
  # retrieve the names of files in directory
  fileNames = list.files(dirName, full.names = TRUE)
  # drop files that are not email
  notEmail = grep("cmds$", fileNames)
  if ( length(notEmail) > 0) fileNames = fileNames[ - notEmail ]
  
  # read all files in the directory
  lapply(fileNames, readLines, encoding = "latin1")
}

processAllEmail = function(dirName, isSpam = FALSE)
{
  # read all files in the directory
  messages = readEmail(dirName)
  fileNames = names(messages)
  n = length(messages)
  
  # split header from body
  eSplit = lapply(messages, splitMessage)
  rm(messages)
  
  # process header as named character vector
  headerList = lapply(eSplit, function(msg) 
    processHeader(msg$header))
  
  # extract content-type key
  contentTypes = sapply(headerList, function(header) 
    header["Content-Type"])
  
  # extract the body
  bodyList = lapply(eSplit, function(msg) msg$body)
  rm(eSplit)
  
  # which email have attachments
  hasAttach = grep("^ *multi", tolower(contentTypes))
  
  # get summary stats for attachments and the shorter body
  attList = mapply(processAttach, bodyList[hasAttach], 
                   contentTypes[hasAttach], SIMPLIFY = FALSE)
  
  bodyList[hasAttach] = lapply(attList, function(attEl) 
    attEl$body)
  
  attachInfo = vector("list", length = n )
  attachInfo[ hasAttach ] = lapply(attList, 
                                   function(attEl) attEl$attachDF)
  
  # prepare return structure
  emailList = mapply(function(header, body, attach, isSpam) {
    list(isSpam = isSpam, header = header, 
         body = body, attach = attach)
  },
  headerList, bodyList, attachInfo, 
  rep(isSpam, n), SIMPLIFY = FALSE )
  names(emailList) = fileNames
  
  invisible(emailList)
}

emailStruct = mapply(processAllEmail, fullDirNames,
                     isSpam = rep( c(FALSE, TRUE), 3:2))      
emailStruct = unlist(emailStruct, recursive = FALSE)

sampleStruct = emailStruct[ indx ]

save(emailStruct, file="emailXX.rda")

header = sampleStruct[[1]]$header
subject = header["Subject"]
els = strsplit(subject, "")
all(els %in% LETTERS)

testSubject = c("DEAR MADAME", "WINNER!", "")

els = strsplit(testSubject, "")
sapply(els, function(subject) all(subject %in% LETTERS))


gsub("[[:punct:] ]", "", testSubject)

gsub("[^[:alpha:]]", "", testSubject)

isYelling = function(msg) {
  if ( "Subject" %in% names(msg$header) ) {
    el = gsub("[^[:alpha:]]", "", msg$header["Subject"])
    if (nchar(el) > 0) 
      nchar(gsub("[A-Z]", "", el)) < 1
    else 
      FALSE
  } else 
    NA
}

perCaps =
  function(msg)
  {
    body = paste(msg$body, collapse = "")
    
    # Return NA if the body of the message is "empty"
    if(length(body) == 0 || nchar(body) == 0) return(NA)
    
    # Eliminate non-alpha characters
    body = gsub("[^[:alpha:]]", "", body)
    capText = gsub("[^A-Z]", "", body)
    100 * nchar(capText)/nchar(body)
  }

sapply(sampleStruct, perCaps)

funcList = list( 
  isRe = function(msg) {
    "Subject" %in% names(msg$header) &&
      length(grep("^[ \t]*Re:", msg$header[["Subject"]])) > 0
  },
  numLines = function(msg) 
    length(msg$body),
  isYelling = function(msg) {
    if ( "Subject" %in% names(msg$header) ) {
      el = gsub("[^[:alpha:]]", "", msg$header["Subject"])
      if (nchar(el) > 0) 
        nchar(gsub("[A-Z]", "", el)) < 1
      else 
        FALSE
    }
    else NA
  },
  perCaps = function(msg) {
    body = paste(msg$body, collapse = "")
    
    # Return NA if the body of the message is "empty"
    if(length(body) == 0 || nchar(body) == 0) return(NA)
    
    # Eliminate non-alpha characters
    body = gsub("[^[:alpha:]]", "", body)
    capText = gsub("[^A-Z]", "", body)
    100 * nchar(capText)/nchar(body)
  }
)

lapply(funcList, function(func) 
  sapply(sampleStruct, function(msg) func(msg)))

createDerivedDF =
  function(email = emailStruct, operations = funcList, 
           verbose = FALSE)
  {
    els = lapply(names(operations),
                 function(id) {
                   if(verbose) print(id)
                   e = operations[[id]]
                   v = if(is.function(e)) 
                     sapply(email, e)
                   else 
                     sapply(email, function(msg) eval(e))
                   v
                 })
    
    df = as.data.frame(els)
    names(df) = names(operations)
    invisible(df)
  }

sampleDF = createDerivedDF(sampleStruct)
head(sampleDF)

funcList = list(
  isSpam =
    expression(msg$isSpam)
  ,
  isRe =
    function(msg) {
      # Can have a Fwd: Re:  ... but we are not looking for this here.
      # We may want to look at In-Reply-To field.
      "Subject" %in% names(msg$header) && 
        length(grep("^[ \t]*Re:", msg$header[["Subject"]])) > 0
    }
  ,
  numLines =
    function(msg) length(msg$body)
  ,
  bodyCharCt =
    function(msg)
      sum(nchar(msg$body))
  ,
  underscore =
    function(msg) {
      if(!"Reply-To" %in% names(msg$header))
        return(FALSE)
      
      txt <- msg$header[["Reply-To"]]
      length(grep("_", txt)) > 0  && 
        length(grep("[0-9A-Za-z]+", txt)) > 0
    }
  ,
  subExcCt = 
    function(msg) {
      x = msg$header["Subject"]
      if(length(x) == 0 || sum(nchar(x)) == 0 || is.na(x))
        return(NA)
      
      sum(nchar(gsub("[^!]","", x)))
    }
  ,
  subQuesCt =
    function(msg) {
      x = msg$header["Subject"]
      if(length(x) == 0 || sum(nchar(x)) == 0 || is.na(x))
        return(NA)
      
      sum(nchar(gsub("[^?]","", x)))
    }
  ,
  numAtt = 
    function(msg) {
      if (is.null(msg$attach)) return(0)
      else nrow(msg$attach)
    }
  
  ,
  priority =
    function(msg) {
      ans <- FALSE
      # Look for names X-Priority, Priority, X-Msmail-Priority
      # Look for high any where in the value
      ind = grep("priority", tolower(names(msg$header)))
      if (length(ind) > 0)  {
        ans <- length(grep("high", tolower(msg$header[ind]))) >0
      }
      ans
    }
  ,
  numRec =
    function(msg) {
      # unique or not.
      els = getMessageRecipients(msg$header)
      
      if(length(els) == 0)
        return(NA)
      
      # Split each line by ","  and in each of these elements, look for
      # the @ sign. This handles
      tmp = sapply(strsplit(els, ","), function(x) grep("@", x))
      sum(sapply(tmp, length))
    }
  ,
  perCaps =
    function(msg)
    {
      body = paste(msg$body, collapse = "")
      
      # Return NA if the body of the message is "empty"
      if(length(body) == 0 || nchar(body) == 0) return(NA)
      
      # Eliminate non-alpha characters and empty lines 
      body = gsub("[^[:alpha:]]", "", body)
      els = unlist(strsplit(body, ""))
      ctCap = sum(els %in% LETTERS)
      100 * ctCap / length(els)
    }
  ,
  isInReplyTo =
    function(msg)
    {
      "In-Reply-To" %in% names(msg$header)
    }
  ,
  sortedRec =
    function(msg)
    {
      ids = getMessageRecipients(msg$header)
      all(sort(ids) == ids)
    }
  ,
  subPunc =
    function(msg)
    {
      if("Subject" %in% names(msg$header)) {
        el = gsub("['/.:@-]", "", msg$header["Subject"])
        length(grep("[A-Za-z][[:punct:]]+[A-Za-z]", el)) > 0
      }
      else
        FALSE
    },
  hour =
    function(msg)
    {
      date = msg$header["Date"]
      if ( is.null(date) ) return(NA)
      # Need to handle that there may be only one digit in the hour
      locate = regexpr("[0-2]?[0-9]:[0-5][0-9]:[0-5][0-9]", date)
      
      if (locate < 0)
        locate = regexpr("[0-2]?[0-9]:[0-5][0-9]", date)
      if (locate < 0) return(NA)
      
      hour = substring(date, locate, locate+1)
      hour = as.numeric(gsub(":", "", hour))
      
      locate = regexpr("PM", date)
      if (locate > 0) hour = hour + 12
      
      locate = regexpr("[+-][0-2][0-9]00", date)
      if (locate < 0) offset = 0
      else offset = as.numeric(substring(date, locate, locate + 2))
      (hour - offset) %% 24
    }
  ,
  multipartText =
    function(msg)
    {
      if (is.null(msg$attach)) return(FALSE)
      numAtt = nrow(msg$attach)
      
      types = 
        length(grep("(html|plain|text)", msg$attach$aType)) > (numAtt/2)
    }
  ,
  hasImages =
    function(msg)
    {
      if (is.null(msg$attach)) return(FALSE)
      
      length(grep("^ *image", tolower(msg$attach$aType))) > 0
    }
  ,
  isPGPsigned =
    function(msg)
    {
      if (is.null(msg$attach)) return(FALSE)
      
      length(grep("pgp", tolower(msg$attach$aType))) > 0
    },
  perHTML =
    function(msg)
    {
      if(! ("Content-Type" %in% names(msg$header))) return(0)
      
      el = tolower(msg$header["Content-Type"]) 
      if (length(grep("html", el)) == 0) return(0)
      
      els = gsub("[[:space:]]", "", msg$body)
      totchar = sum(nchar(els))
      totplain = sum(nchar(gsub("<[^<]+>", "", els )))
      100 * (totchar - totplain)/totchar
    },
  subSpamWords =
    function(msg)
    {
      if("Subject" %in% names(msg$header))
        length(grep(paste(SpamCheckWords, collapse = "|"), 
                    tolower(msg$header["Subject"]))) > 0
      else
        NA
    }
  ,
  subBlanks =
    function(msg)
    {
      if("Subject" %in% names(msg$header)) {
        x = msg$header["Subject"]
        # should we count blank subject line as 0 or 1 or NA?
        if (nchar(x) == 1) return(0)
        else 100 *(1 - (nchar(gsub("[[:blank:]]", "", x))/nchar(x)))
      } else NA
    }
  ,
  noHost =
    function(msg)
    {
      # Or use partial matching.
      idx = pmatch("Message-", names(msg$header))
      
      if(is.na(idx)) return(NA)
      
      tmp = msg$header[idx]
      return(length(grep(".*@[^[:space:]]+", tmp)) ==  0)
    }
  ,
  numEnd =
    function(msg)
    {
      # If we just do a grep("[0-9]@",  )
      # we get matches on messages that have a From something like
      # " \"marty66@aol.com\" <synjan@ecis.com>"
      # and the marty66 is the "user's name" not the login
      # So we can be more precise if we want.
      x = names(msg$header)
      if ( !( "From" %in% x) ) return(NA)
      login = gsub("^.*<", "", msg$header["From"])
      if ( is.null(login) ) 
        login = gsub("^.*<", "", msg$header["X-From"])
      if ( is.null(login) ) return(NA)
      login = strsplit(login, "@")[[1]][1]
      length(grep("[0-9]+$", login)) > 0
    },
  isYelling =
    function(msg)
    {
      if ( "Subject" %in% names(msg$header) ) {
        el = gsub("[^[:alpha:]]", "", msg$header["Subject"])
        if (nchar(el) > 0) nchar(gsub("[A-Z]", "", el)) < 1
        else FALSE
      }
      else
        NA
    },
  forwards =
    function(msg)
    {
      x = msg$body
      if(length(x) == 0 || sum(nchar(x)) == 0)
        return(NA)
      
      ans = length(grep("^[[:space:]]*>", x))
      100 * ans / length(x)
    },
  isOrigMsg =
    function(msg)
    {
      x = msg$body
      if(length(x) == 0) return(NA)
      
      length(grep("^[^[:alpha:]]*original[^[:alpha:]]+message[^[:alpha:]]*$", 
                  tolower(x) ) ) > 0
    },
  isDear =
    function(msg)
    {
      x = msg$body
      if(length(x) == 0) return(NA)
      
      length(grep("^[[:blank:]]*dear +(sir|madam)\\>", 
                  tolower(x))) > 0
    },
  isWrote =
    function(msg)
    {
      x = msg$body
      if(length(x) == 0) return(NA)
      
      length(grep("(wrote|schrieb|ecrit|escribe):", tolower(x) )) > 0
    },
  avgWordLen =
    function(msg)
    {
      txt = paste(msg$body, collapse = " ")
      if(length(txt) == 0 || sum(nchar(txt)) == 0) return(0)
      
      txt = gsub("[^[:alpha:]]", " ", txt)
      words = unlist(strsplit(txt, "[[:blank:]]+"))
      wordLens = nchar(words)
      mean(wordLens[ wordLens > 0 ])
    }
  ,
  numDlr =
    function(msg)
    {
      x = paste(msg$body, collapse = "")
      if(length(x) == 0 || sum(nchar(x)) == 0)
        return(NA)
      
      nchar(gsub("[^$]","", x))
    }
)


SpamCheckWords =
  c("viagra", "pounds", "free", "weight", "guarantee", "million", 
    "dollars", "credit", "risk", "prescription", "generic", "drug",
    "financial", "save", "dollar", "erotic", "million", "barrister",
    "beneficiary", "easy", 
    "money back", "money", "credit card")


getMessageRecipients =
  function(header)
  {
    c(if("To" %in% names(header))  header[["To"]] else character(0),
      if("Cc" %in% names(header))  header[["Cc"]] else character(0),
      if("Bcc" %in% names(header)) header[["Bcc"]] else character(0)
    )
  }

emailDF = createDerivedDF(emailStruct)
dim(emailDF)
#save(emailDF, file = "spamAssassinDerivedDF.rda")

load("Data/spamAssassinDerivedDF.rda")
dim(emailDF)

perCaps2 =
  function(msg)
  {
    body = paste(msg$body, collapse = "")
    
    # Return NA if the body of the message is "empty"
    if(length(body) == 0 || nchar(body) == 0) return(NA)
    
    # Eliminate non-alpha characters and empty lines 
    body = gsub("[^[:alpha:]]", "", body)
    els = unlist(strsplit(body, ""))
    ctCap = sum(els %in% LETTERS)
    100 * ctCap / length(els)
  }

pC = sapply(emailStruct, perCaps)
pC2 = sapply(emailStruct, perCaps2)
identical(pC, pC2)

indNA = which(is.na(emailDF$subExcCt))

indNoSubject = which(sapply(emailStruct, 
                            function(msg) 
                              !("Subject" %in% names(msg$header))))

all(indNA == indNoSubject)

all(emailDF$bodyCharCt > emailDF$numLines)


x.at = c(1,10,100,1000,10000,100000)
y.at = c(1, 5, 10, 50, 100, 500, 5000)
nL = 1 + emailDF$numLines
nC = 1 + emailDF$bodyCharCt
pdf("ScatterPlotNumLinesNumChars.pdf", width = 6, height = 4.5)
plot(nL ~ nC, log = "xy", pch=".", xlim=c(1,100000), axes = FALSE,
     xlab = "Number of Characters", ylab = "Number of Lines")
box() 
axis(1, at = x.at, labels = formatC(x.at, digits = 0, format="d"))
axis(2, at = y.at, labels = formatC(y.at, digits = 0, format="d")) 
abline(a=0, b=1, col="red", lwd = 2)
dev.off()

pdf("SPAM_boxplotsPercentCaps.pdf", width = 5, height = 5)

percent = emailDF$perCaps
isSpamLabs = factor(emailDF$isSpam, labels = c("ham", "spam"))
boxplot(log(1 + percent) ~ isSpamLabs,
        ylab = "Percent Capitals (log)")

dev.off()

logPerCapsSpam = log(1 + emailDF$perCaps[ emailDF$isSpam ])
logPerCapsHam = log(1 + emailDF$perCaps[ !emailDF$isSpam ])

qqplot(logPerCapsSpam, logPerCapsHam, 
       xlab = "Regular Email", ylab = "Spam Email", 
       main = "Percentage of Capital Letters (log scale)",
       pch = 19, cex = 0.3)

pdf("SPAM_scatterplotPercentCapsTotChars.pdf", width = 8, height = 6)

colI = c("#4DAF4A80", "#984EA380")
logBodyCharCt = log(1 + emailDF$bodyCharCt)
logPerCaps = log(1 + emailDF$perCaps)
plot(logPerCaps ~ logBodyCharCt, xlab = "Total Characters (log)",
     ylab = "Percent Capitals (log)",
     col = colI[1 + emailDF$isSpam],
     xlim = c(2,12), pch = 19, cex = 0.5)

dev.off()

table(emailDF$numAtt, isSpamLabs)

pdf("SPAM_mosaicPlots.pdf", width = 8, height = 4)

oldPar = par(mfrow = c(1, 2), mar = c(1,1,1,1))

colM = c("#E41A1C80", "#377EB880")
isRe = factor(emailDF$isRe, labels = c("no Re:", "Re:"))
mosaicplot(table(isSpamLabs, isRe), main = "",
           xlab = "", ylab = "", color = colM)

fromNE = factor(emailDF$numEnd, labels = c("No #", "#"))
mosaicplot(table(isSpamLabs, fromNE), color = colM,
           main = "", xlab="", ylab = "")

par(oldPar)

dev.off()

library(rpart)

setupRpart = function(data) {
  logicalVars = which(sapply(data, is.logical))
  facVars = lapply(data[ , logicalVars], 
                   function(x) {
                     x = as.factor(x)
                     levels(x) = c("F", "T")
                     x
                   })
  cbind(facVars, data[ , - logicalVars])
}

emailDFrp = setupRpart(emailDF)


set.seed(418910)
testSpamIdx = sample(numSpam, size = floor(numSpam/3))
testHamIdx = sample(numHam, size = floor(numHam/3))

testDF = 
  rbind( emailDFrp[ emailDFrp$isSpam == "T", ][testSpamIdx, ],
         emailDFrp[emailDFrp$isSpam == "F", ][testHamIdx, ] )
trainDF =
  rbind( emailDFrp[emailDFrp$isSpam == "T", ][-testSpamIdx, ], 
         emailDFrp[emailDFrp$isSpam == "F", ][-testHamIdx, ])

rpartFit = rpart(isSpam ~ ., data = trainDF, method = "class")

library(rpart.plot)
prp(rpartFit, extra = 1)

library(rpart.plot)
pdf("SPAM_rpartTree.pdf", width = 7, height = 7)

prp(rpartFit, extra = 1)
dev.off()

predictions = predict(rpartFit, 
                      newdata = testDF[, names(testDF) != "isSpam"],
                      type = "class")

predsForHam = predictions[ testDF$isSpam == "F" ]
summary(predsForHam)

sum(predsForHam == "T") / length(predsForHam)

predsForSpam = predictions[ testDF$isSpam == "T" ]
sum(predsForSpam == "F") / length(predsForSpam)

complexityVals = c(seq(0.00001, 0.0001, length=19),
                   seq(0.0001, 0.001, length=19), 
                   seq(0.001, 0.005, length=9),
                   seq(0.005, 0.01, length=9))

fits = lapply(complexityVals, function(x) {
  rpartObj = rpart(isSpam ~ ., data = trainDF,
                   method="class", 
                   control = rpart.control(cp=x) )
  
  predict(rpartObj, 
          newdata = testDF[ , names(testDF) != "isSpam"],
          type = "class")
})

spam = testDF$isSpam == "T"
numSpam = sum(spam)
numHam = sum(!spam)
errs = sapply(fits, function(preds) {
  typeI = sum(preds[ !spam ] == "T") / numHam
  typeII = sum(preds[ spam ] == "F") / numSpam
  c(typeI = typeI, typeII = typeII)
})

pdf("SPAM_rpartTypeIandII.pdf", width = 8, height = 7)
library(RColorBrewer)
cols = brewer.pal(9, "Set1")[c(3, 4, 5)]
plot(errs[1,] ~ complexityVals, type="l", col=cols[2], 
     lwd = 2, ylim = c(0,0.2), xlim = c(0,0.005), 
     ylab="Error", xlab="complexity parameter values")
points(errs[2,] ~ complexityVals, type="l", col=cols[1], lwd = 2)

text(x =c(0.003, 0.0035), y = c(0.12, 0.05), 
     labels=c("Type II Error", "Type I Error"))

minI = which(errs[1,] == min(errs[1,]))[1]
abline(v = complexityVals[minI], col ="grey", lty =3, lwd=2)

text(0.0007, errs[1, minI]+0.01, 
     formatC(errs[1, minI], digits = 2))
text(0.0007, errs[2, minI]+0.01, 
     formatC(errs[2, minI], digits = 3))

dev.off()