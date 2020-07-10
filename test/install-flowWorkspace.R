##
#  Copyright (c) 2010 LabKey Corporation
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

vers <- getRversion()
if (vers < "2.15.1") {
    stop("Your R version is ", vers, ". The flowWorkspace library requires R version 2.15.1 or greater")
}

source("install-util.R")
install.dependencies("flowWorkspace", c(), c("flowWorkspace"))
