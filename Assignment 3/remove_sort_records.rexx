inputFile  = "input.txt"    /* Input PS file */
outputFile = "output.txt"   /* Output file after removing duplicates & sorting */

say "Processing file: " inputFile

/* Step 1: Read the file and store unique records */
records. = ""
uniqueRecords. = ""
sortedKeys. = ""
recordCount = 0
uniqueCount = 0

/* Read the input file */
infile = open(inputFile, "r")
if infile == 0 then do
    say "Error: Unable to open input file."
    exit 1
end

do while lines(inputFile) > 0
    record = linein(inputFile)
    recordCount = recordCount + 1
    key = substr(record, 25, 8)  /* Extract primary key from columns 25-32 */
    
    /* Store unique records using the primary key as an index */
    if uniqueRecords.key == "" then do
        uniqueCount = uniqueCount + 1
        uniqueRecords.key = record  /* Store record with primary key as index */
        sortedKeys.uniqueCount = key /* Store key for sorting */
    end
end
call lineout inputFile  /* Close input file */

say recordCount "records read."
say uniqueCount "unique records found."

/* Step 2: Sort unique records based on primary key */
call sortKeys sortedKeys, uniqueCount

/* Step 3: Write sorted records to output file */
call lineout outputFile

do i = 1 to uniqueCount
    key = sortedKeys.i
    call lineout outputFile, uniqueRecords.key
end
call lineout outputFile

say "Processing complete. Sorted output saved in " outputFile
exit

/* Function to sort keys */
sortKeys: procedure
    parse arg sortedKeys., count
    do i = 1 to count - 1
        do j = i + 1 to count
            if sortedKeys.i > sortedKeys.j then do
                temp = sortedKeys.i
                sortedKeys.i = sortedKeys.j
                sortedKeys.j = temp
                
                tempRec = uniqueRecords.sortedKeys.i
                uniqueRecords.sortedKeys.i = uniqueRecords.sortedKeys.j
                uniqueRecords.sortedKeys.j = tempRec
            end
        end
    end