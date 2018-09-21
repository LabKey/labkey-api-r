
labkey.transform.readRunPropertiesFile <- function(runInfoPath)
{
    # set up a data frame to store the run properties
      properties = data.frame(NA, NA, NA, NA);
      colnames(properties) = c("name", "val1", "val2", "val3");

      #read in the run properties from the TSV
      lines = readLines(runInfoPath);

      # each line has a run property with the name, val1, val2, etc.
      for (i in 1:length(lines))
      {
        # split the line into the various parts (tab separated)
        parts = strsplit(lines[i], split="\t")[[1]];

        # if the line does not have 4 parts, add NA's as needed
        if (length(parts) < 4)
        {
          for (j in 1:4)
          {
            if (is.na(parts[j]))
            {
              parts[j] = NA;
            }
          }
        }

        # add the parts for the given run property to the properties data frame
        properties[i,] = parts;
      }

      return (properties)
}

labkey.transform.getRunPropertyValue <- function(runProps, propName)
{
    value = NA;
    if (any(runProps$name == propName))
    {
        value = runProps$val1[runProps$name == propName];

        # return NA for an empty string
        if (nchar(value) == 0)
        {
            value = NA;
        }
    }
    return (value)
}