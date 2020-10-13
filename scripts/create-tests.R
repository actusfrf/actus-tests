
# global options
options(stringsAsFactors = FALSE)
options(scipen=999)

# load libraries
library(jsonlite)
library(magrittr)
library(readr)
library(readxl)

# define paths
json_path = '../data/json/'
excel_path = '../data/excel/'

# utility function
tocamel=function(x,delim=" ") {
	s <- strsplit(x, split=delim,fixed=TRUE)
	sapply(s, function(y) {
		if (any(is.na(y))) {
		    y
		}
		else {
		    first <- substring(y, 1, 1)
		    first <- toupper(first)
		    first[1] <- tolower(first[1])
		    paste(first, substring(y, 2), sep = "", collapse = "")
		}
	    })
}

# list available excel files
testbeds=list.files(excel_path,full.names=TRUE)

# go through all files and parse to json
for(file in testbeds) {

	# empty list for this contract's test cases
	cttests=list()

	# load terms and number of sheets
	sheets = excel_sheets(file)
	terms=read_excel(file, sheet=sheets[1])
	#terms[is.na(terms)]=""
	colnames(terms)=tocamel(colnames(terms),delim=".")
	terms$contractID=as.character(terms$contractID)	
	# convert external data to json and separate from terms
	to = terms$to
	dataObserved=lapply(terms$dataObserved, function(data) {
			if(is.na(data)) fromJSON("{}") else fromJSON(data)
		})
	eventsObserved=lapply(terms$eventsObserved, function(data) {
			if(is.na(data)) fromJSON("[]") else fromJSON(data)
		})
	terms = terms[-which(colnames(terms)%in%c("to","dataObserved","eventsObserved"))]
	# convert terms data.frame to list and contractStructure term to json, and remove null-valued terms
	terms_norm=apply(terms,1,function(test) {
		test_norm=as.list(test[!is.na(test)])
		if("contractStructure"%in%names(test_norm)) test_norm[["contractStructure"]]=fromJSON(test_norm[["contractStructure"]])
		if("arrayCycleAnchorDateOfInterestPayment"%in%names(test_norm)) test_norm[["arrayCycleAnchorDateOfInterestPayment"]]=fromJSON(test_norm[["arrayCycleAnchorDateOfInterestPayment"]])
		if("arrayCycleOfInterestPayment"%in%names(test_norm)) test_norm[["arrayCycleOfInterestPayment"]]=fromJSON(test_norm[["arrayCycleOfInterestPayment"]])
		if("arrayCycleAnchorDateOfPrincipalRedemption"%in%names(test_norm)) test_norm[["arrayCycleAnchorDateOfPrincipalRedemption"]]=fromJSON(test_norm[["arrayCycleAnchorDateOfPrincipalRedemption"]])
		if("arrayCycleOfPrincipalRedemption"%in%names(test_norm)) test_norm[["arrayCycleOfPrincipalRedemption"]]=fromJSON(test_norm[["arrayCycleOfPrincipalRedemption"]])
		if("arrayNextPrincipalRedemptionPayment"%in%names(test_norm)) test_norm[["arrayNextPrincipalRedemptionPayment"]]=fromJSON(test_norm[["arrayNextPrincipalRedemptionPayment"]])
		if("arrayIncreaseDecrease"%in%names(test_norm)) test_norm[["arrayIncreaseDecrease"]]=fromJSON(test_norm[["arrayIncreaseDecrease"]])
		if("arrayCycleAnchorDateOfRateReset"%in%names(test_norm)) test_norm[["arrayCycleAnchorDateOfRateReset"]]=fromJSON(test_norm[["arrayCycleAnchorDateOfRateReset"]])
		if("arrayCycleOfRateReset"%in%names(test_norm)) test_norm[["arrayCycleOfRateReset"]]=fromJSON(test_norm[["arrayCycleOfRateReset"]])
		if("arrayRate"%in%names(test_norm)) test_norm[["arrayRate"]]=fromJSON(test_norm[["arrayRate"]])
		if("arrayFixedVariable"%in%names(test_norm)) test_norm[["arrayFixedVariable"]]=fromJSON(test_norm[["arrayFixedVariable"]])
		return(test_norm)
	})
	
	#as.list),function(test) {
	#	test_norm=test[unlist(test) != "{}"]
	#	if(!is.null(test_norm[["contractStructure"]])) test_norm[["contractStructure"]]=fromJSON(test_norm[["contractStructure"]])
	#	return(test)
	#})

	# go through test cases/sheets and load results
	for(i in 2:length(sheets)) {
		caseIdentifier=as.character(terms_norm[[i-1]]$contractID)
		# read/format results
		results=read_excel(file,sheet=sheets[i])		
		colnames(results)=tocamel(colnames(results),delim=" ")
		# combine data in list (i.e. json object)
		cttests[[caseIdentifier]] = 
			list(identifier = caseIdentifier,
				terms = terms_norm[[i-1]], #unbox(terms[which(terms$contractID==caseIdentifier),]),
				to = to[[i-1]],
				dataObserved = dataObserved[[i-1]],
				eventsObserved = eventsObserved[[i-1]],
				results = results)
	}
	
	# convert to json
	jsonData = prettify(toJSON(cttests,auto_unbox=TRUE,pretty=TRUE,digits=NA) )
	
	# write json
	jsonData %>% write_lines(paste0(json_path,'actus-tests-', tolower(terms$contractType[i-1]),'.json'))
}
