
labkey.transform.readRunPropertiesFile <- function(run.info.path)
{
    print(paste("TODO read lines from", run.info.path));
}

labkey.transform.getRunPropertyValue <- function(run.props, prop.name)
{
    value = NA;
    if (any(run.props$name == prop.name))
    {
        value = run.props$val1[run.props$name == prop.name];

        # return NA for an empty string
        if (nchar(value) == 0)
        {
            value = NA;
        }
    }
    value;
}