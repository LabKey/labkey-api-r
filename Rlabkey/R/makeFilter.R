##
#  Copyright (c) 2010-2018 LabKey Corporation
# 
#  Licensed under the Apache License, Version 2.0 (the "License");
#  you may not use this file except in compliance with the License.
#  You may obtain a copy of the License at
# 
#      http://www.apache.org/licenses/LICENSE-2.0
# 
#  Unless required by applicable law or agreed to in writing, software
#  distributed under the License is distributed on an "AS IS" BASIS,
#  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#  See the License for the specific language governing permissions and
#  limitations under the License.
##

makeFilter <- function(..., asList=FALSE)
{
    fargs <- list(...)
    flen <- lapply(fargs, function(x) {len <- length(x); if(len<3){stop ("each filter must be of length 3")}})

 	# Determine number of filters
 	fmat <- rbind(...)
 	fcount <- dim(fmat)[1]
 	# Construct filters
 	filters <- list()
 	for (i in 1:fcount)
    {
        # Match the operator
        fop  <- switch(EXPR=toupper(fmat[i,2]),
            #
            # These operators require a data value
            #

            "EQUALS"="eq",
            "EQUAL"="eq",
            "DATE_EQUAL"="dateeq",

            "NEQ"="neq",
            "NOT_EQUALS"="neq",
            "NOT_EQUAL"="neq",
            "DATE_NOT_EQUAL"="dateneq",

            "NOT_EQUAL_OR_NULL"="neqornull",
            "NOT_EQUAL_OR_MISSING"="neqornull",

            "GT"="gt",
            "GREATER_THAN"="gt",
            "DATE_GT"="dategt",
            "DATE_GREATER_THAN"="dategt",

            "LT"="lt",
            "LESS_THAN"="lt",
            "DATE_LT"="datelt",
            "DATE_LESS_THAN"="datelt",

            "GTE"="gte",
            "GREATER_THAN_OR_EQUAL_TO"="gte",
            "GREATER_THAN_OR_EQUAL"="gte",
            "DATE_GTE"="dategte",
            "DATE_GREATER_THAN_OR_EQUAL"="dategte",

            "LTE"="lte",
            "LESS_THAN_OR_EQUAL_TO"="lte",
            "LESS_THAN_OR_EQUAL"="lte",
            "DATE_LTE"="datelte",
            "DATE_LESS_THAN_OR_EQUAL"="datelte",

            "STARTS_WITH"="startswith",
            "DOES_NOT_START_WITH"="doesnotstartwith",

            "CONTAINS"="contains",
            "DOES_NOT_CONTAIN"="doesnotcontain",

            "CONTAINS_ONE_OF"="containsoneof",
            "CONTAINS_NONE_OF"="containsnoneof",

            "IN"="in",
            "EQUALS_ONE_OF"="in",

            "NOT_IN"="notin",
            "EQUALS_NONE_OF"="notin",

            "BETWEEN"="between",
            "NOT_BETWEEN"="notbetween",

            "MEMBER_OF"="memberof",

            #
            # These are the "no data value" operators
            #

            "ISBLANK"="isblank",
            "IS_MISSING"="isblank",
            "MISSING"="isblank",
            "NON_BLANK"="isnonblank",
            "IS_NOT_MISSING"="isnonblank",
            "NOT_MISSING"="isnonblank",

            "MV_INDICATOR"="hasmvvalue",
            "QC_VALUE"="hasmvvalue",
            "NO_MV_INDICATOR"="nomvvalue",
            "NOT_QC_VALUE"="nomvvalue",

            #
            # Table/Query-wise operators
            #

            "Q"="q",

            #
            # Ontology operators
            #
            "ONTOLOGY_IN_SUBTREE"="concept:insubtree",
            "ONTOLOGY_NOT_IN_SUBTREE"="concept:notinsubtree",

            #
            # Lineage operators
            #
            "EXP_CHILD_OF" = "exp:childof",
            "EXP_PARENT_OF" = "exp:parentof",
            "EXP_LINEAGE_OF" = "exp:lineageof",
        )

        if (is.null(fop)==TRUE) stop ("Invalid operator name.")
        # grab the column name and filter value
        col <- fmat[i,1]
        fvalue <- fmat[i,3]

        # add the param pair to the parameter list
        paramList <- list(fvalue)
        names(paramList) <- paste("query.", col, "~", fop, sep="")

        filters <- c(filters, paramList)
    }

    if (asList)
    {
     	return(filters)
    }
    else
    {
        # convert to URL encoded vector of parameters (legacy behavior)
        filterVec <- c()
        for (i in 1:length(filters))
        {
            url <- parse_url("")
            url$query <- filters[i]
            myurl <- build_url(url)

            idx <- regexpr("\\?.*", myurl)
            if (idx != -1)
                filterVec <- c(filterVec, regmatches(myurl, idx+1))
        }
        return (filterVec)
    }
}
