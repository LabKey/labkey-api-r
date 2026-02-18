#include <Rcpp.h>
#include <string>
using namespace Rcpp;

// [[Rcpp::export]]
CharacterMatrix listToMatrix(List data, List names) {
  int rowCount = data.size(); // rows
  int validColumnCount = names.size(); // name count
  int indexColumnCount;
  List indexList = *new List();
  
  CharacterMatrix cMatrix = CharacterMatrix(rowCount, validColumnCount); 
  cMatrix.fill(NA_STRING);
  
  // Get data indices of valid names. Faster to look up by direct access than searching list for name.
  for(int c = 0; c < validColumnCount; c++)
  {
    indexList.push_back(as<List>(data[0]).findName(names[c]));
  }
  
  indexColumnCount = indexList.size();
  
  // Loop through data
  for(int i = 0; i < rowCount; i++)
  {
    // Loop through indices of valid names
    for(int j = 0; j < indexColumnCount; j++)
    {
      // If values are not null add to matrix
      if(!Rf_isNull((as<List>(data[i]))[as<int>(indexList[j])])) 
      {
        // Issue 36837: GROUP CONCAT comes back as an array of values
        if(TYPEOF((as<List>(data[i]))[as<int>(indexList[j])]) != VECSXP)
        {
          cMatrix(i,j) = (as<CharacterVector>((as<List>(as<List>(data[i]))[as<int>(indexList[j])])))[0];
        }
        else
        {
          // only try to parse the vector elements if the first element is not null
          GenericVector gv = as<GenericVector>((as<List>(data[i]))[as<int>(indexList[j])]);
          if(gv.size() > 0 && !Rf_isNull(gv[0]))
          {
            // Check if first element is a string - if so, concatenate all elements
            if(TYPEOF(gv[0]) == STRSXP)
            {
              std::string result;
              for(int k = 0; k < gv.size(); k++)
              {
                if(!Rf_isNull(gv[k]) && TYPEOF(gv[k]) == STRSXP)
                {
                  String s = as<String>(gv[k]);
                  if(k > 0) result += ",";
                  result += s.get_cstring();
                }
              }
              cMatrix(i,j) = result;
            }
            else
            {
              // Fallback to just returning the first element
              cMatrix(i,j) = as<CharacterVector>(gv[0])[0];
            }
          }
        }
      }
    }
  }
  
  return cMatrix;
}
