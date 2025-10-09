### materials
- a final table, unit being victim
- scraped pdfs arranged by working progress
- 4 out of 5 regions are annotated


### expectation
- to establish the relation between table records and saved pdfs
- cleaned up corpus for finetuning

### current status
- rearranged the table to be victim-source, for better linkage between record and files
- populated the last region and merge with the final table
- rearranged the pdfs in region/victim structure rather than working progress
- rescraped lost files, so that 80% of the victim-sources are recovered, having correct files linking to the records
- preliminary cleaning of the pdfs
**show reference files here**

### issues
- original table unit is not suitable for buiding record-file relation (solved)
- files are not structured in a way that can be easily tracked (solved)
- pdfs cannot be linked back to records (solved)
- lost files (recovered partially)
- one region is not annotated (not solved)
- pdfs contains content that are not related to the topic, such as ads, comments, placeholders. (not solved. Docling and olmocr can only identify some of them and no 100% guarantee)
**add examples here**

### what could have done differently
- rescrape everthing at the beginning will save massive time. Reverse engineering is more costly than having a clean start

### future steps and options

1. get clean text files
- option 1. select and pick the "cleaner" pdfs
    - pro: 
        - files are readily available
    - con: 
        - only a very small fraction of the file will be used. 
        - picking out "cleaner" files may take unknown time, and may need manual labor.
- option 2. rescrape all available links and save into html.
    - pro: 
        - a clean start
        - more files
        - timeline is more controlable
    - con: 
        - some links might be broken

2. cleaning 
   - if using only the "cleaner" pdfs, docling should do the job, if these pdfs are guaranteed to be "cleaner". 
   - if taking the total rescraping route, select correct elements from the htmls.

3. fine-tuning

**use shreyas pipeline, show reference**

