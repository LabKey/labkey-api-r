## material inputs and data outputs
mi <- data.frame(lsid=c("urn:lsid:labkey.com:Sample.251.MySamples:sample1", "urn:lsid:labkey.com:Sample.251.MySamples:sample2"))
do <- data.frame(lsid="urn:lsid:labkey.com:AssayRunTSVData.Folder-251:12c70994-7ce5-1038-82f0-9c1487dbd334")

p <- labkey.provenance.createProvenanceParams(name="step1", description="initial step", materialInputs=mi)

ra <- labkey.provenance.startRecording(baseUrl="http://localhost:8080/labkey/", folderPath = "Provenance", provenanceParams=p)
labkey.provenance.stopRecording(baseUrl="http://localhost:8080/labkey/", folderPath = "Provenance",
    provenanceParams=labkey.provenance.createProvenanceParams(name="final step", recordingId=ra$recordingId, dataOutputs=do))


## object inputs (from an assay run) and material inputs
##
oi <- labkey.selectRows(baseUrl="http://localhost:8080/labkey/", folderPath = "Provenance",
        schemaName="assay.General.nestle",
        queryName="Data",
        colSelect= c("LSID"),
        colFilter=makeFilter(c("Run/RowId","EQUAL","253")))
mi <- data.frame(lsid=c("urn:lsid:labkey.com:Sample.251.MySamples:sample1", "urn:lsid:labkey.com:Sample.251.MySamples:sample2"))

p <- labkey.provenance.createProvenanceParams(name="step1", description="initial step", objectInputs=oi[["LSID"]], materialInputs=mi)
ra <- labkey.provenance.startRecording(baseUrl="http://localhost:8080/labkey/", folderPath = "Provenance", provenanceParams=p)
labkey.provenance.stopRecording(baseUrl="http://localhost:8080/labkey/", folderPath = "Provenance",
    provenanceParams=labkey.provenance.createProvenanceParams(name="final step", recordingId=ra$recordingId))

## intermediate step using insert rows
mi <- data.frame(lsid=c("urn:lsid:labkey.com:Sample.251.MySamples:sample1", "urn:lsid:labkey.com:Sample.251.MySamples:sample2"))

p <- labkey.provenance.createProvenanceParams(name="step1", description="initial step", materialInputs=mi)
ra <- labkey.provenance.startRecording(baseUrl="http://localhost:8080/labkey/", folderPath = "Provenance", provenanceParams=p)
do <- data.frame(lsid="urn:lsid:labkey.com:AssayRunTSVData.Folder-251:12c70994-7ce5-1038-82f0-9c1487dbd334")

rows <- fromJSON(txt='[{
        "name" : "sample3",
        "protein" : "p3",
        "prov:objectInputs" : ["1234", "456"]
    },{
        "name" : "sample4",
        "protein" : "p4",
        "prov:objectInputs" : ["1234", "456"]
    }
]')

labkey.insertRows(baseUrl="http://localhost:8080/labkey/", folderPath = "Provenance",
    schemaName="samples", queryName="MySamples", toInsert=rows,
    provenanceParams=labkey.provenance.createProvenanceParams(name="query step", recordingId=ra$recordingId))
labkey.provenance.stopRecording(baseUrl="http://localhost:8080/labkey/", folderPath = "Provenance",
    provenanceParams=labkey.provenance.createProvenanceParams(name="final step", recordingId=ra$recordingId, dataOutputs=do))
