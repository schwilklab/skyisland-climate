library(EML)
library(rjson)




camel_case <- function(x) {
 s <- strsplit(x, " ")[[1]]
 paste(tolower(s[1]),
       paste(toupper(substring(s[-1], 1,1)), substring(s[-1], 2), sep="", collapse=""),
       sep="")
}


capitalize <- function(x) {
   paste(toupper(substring(x, 1,1)), substring(x, 2), sep="")
}



# Read a csv file containing metadata specific to a single table. This funciton
# does minimal reading and conversion and leaves any conversion requiring the
# actual data.frame to make_dataTable()
read_md_file <- function(fn) {
  md <- read.csv(fn, stringsAsFactors=FALSE)

  ## required headers
  md$attributeName <- md$variable
  md$attributeDefinition <- md$definition
  md$unit[md$unit==""] <- NA
 
  return(md)
}


# This function converts my simple tabular emtadata into the more complete EML 
# takes two argumements,1) a path to a csv data file and 2) a dataframe
# describing the metadata
make_data_table <- function(x) { 
  data <- read.table(file=x$path, header=TRUE, sep=",", stringsAsFactors=FALSE)
  md <- read_md_file(x$metadata)
  
  i <- match(names(data),md$attributeName)
  
  if(any(is.na(i)))
    stop(paste("metadata incomplete", names(data)[is.na(i)], "missing from metadata:", names(md), "\n"))

  physical <- set_physical(x$path) # TODO? options


  ## set the "classes" and "number types" in EML vocabularly. Not 1:1 with R
  ## "classes"
  col_classes <- sapply(data, class) # get vector of class names, but not
  # set numberType
  integers <- md$type=="numeric" & col_classes=="integer"
  reals <- md$type=="numeric" & col_classes=="numeric"
  bools <- md$type=="bool"
  md$numberType[integers]  <- "integer"
  md$numberType[bools]  <- "integer" # no boolean support so use integer
  md$unit[bools] <- "dimensionless" # HACK
  md$numberType[reals]  <- "real"

  # clean up col_classes to allowed only
  col_classes[col_classes=="integer"] <- "numeric"
  col_classes[bools]=="numeric"

#  print(col_classes)
#  print(md$numberType)
  attribute_list <- set_attributes(md, NULL, col_classes)

  dataTable <- new("dataTable",
                 entityName = basename(x$path),
                 entityDescription = x$description,
                 physical = physical,
                 attributeList = attribute_list)
  return(dataTable)
}



# Make creator and contact information
make_person <- function(x, type="contact"){
  address <- new("address",
                    deliveryPoint = x$address,
                    city = x$city,
                    administrativeArea = x$state,
                    postalCode = x$postalCode,
                 country = x$country)
  p <- as.person(x$name)
  contact <-  new(type,
                  individualName = new(
                    "individualName",
                    "givenName" = as.character(p$given),
                    "surName" = as(as.character(p$family), "surName")),
                  electronicMail = x$email,
                  address = address,
                  organizationName = x$organization,
                  phone = x$phone)
  return(contact)
}



make_data_set <- function(details_file) {
  # read json details file
  details <- fromJSON(paste(readLines(details_file), collapse=""))


  # creator and contacts
  creators <- new("ListOfcreator", lapply(details$creator, make_person, type="creator"))
  other_researchers <- new("ListOfassociatedParty", lapply(details$associatedParty, make_person, type="associatedParty"))

  contact <- creators[[1]]

  # We then construct a methods node containing this text as the description:
  method.steps <- new("ListOfmethodStep",
                      lapply(paste(names(details$methods), ":", details$methods),
                             function(x) new("methodStep", description = x)))

  # locations
  latlon <- do.call(rbind, details$coverage$locations)

  # Species list can contain max two words

  spp <- sapply(strsplit(details$coverage$scientific_names, " "), function(x) paste(x[1], x[2]))

  keywords <- new("keywordSet",
                  keyword = details$publication$keywords)
  
  coverage <-  set_coverage(sci_names = spp,
                            begin = min(details$coverage$dates),
                            end = max(details$coverage$dates),
                            geographicDescription = details$coverage$geographic_description,
                            west = min(latlon[,2]), east = max(latlon[,2]), 
                            north = max(latlon[,1]), south = min(latlon[,1]))


  # make datatables
  dataTables <- new("ListOfdataTable", lapply(details$files, make_data_table))

  
  # Assemble the EML dataset:
  dataset <- new("dataset",
                 title =  paste('Data from:',  details$publication$title),
                 creator = creators,
                 pubDate =  details$publication$date,
                 associatedParty = other_researchers[[1]], # should be able to give list
                 intellectualRights =  details$rights,
                 abstract =  details$publication$abstract,
                 keywordSet = keywords,
                 coverage = coverage,
                 contact = contact,
                 methods = set_methods(details$methods_file),
                 dataTable = dataTables)

  eml <- new("eml",
             packageId = uuid::UUIDgenerate(),
             system = "uuid", # type of identifier
             dataset = dataset
             )

  return(eml)
}
