if(interactive()){
  
  
  ### 
  ### Full example for a process workflow
  ###
   path <- tempdir()
   path <- system.file(package='MagellanNTK')
   
  ll.process <- list(
    fullname = 'PipeA_ProcessX',
    mode = "process",
    steps = c("Step 1", "Step 2"),
    mandatory = c(TRUE, FALSE)
    )


files <- createModuleTemplate(ll.process, path = path)
source(file.path(path, files), local=TRUE)
data("data_na")
run_workflow("PipeA_ProcessX", 
             dataIn = data_na, 
             path = path)
unlink(file)
  
  

### 
### Full example for a pipeline workflow
###
path <- tempdir()
path <- system.file(package='MagellanNTK')

ll.proc1 <- list(
  fullname = 'PipeA_Proc1',
  mode = "process",
  steps = c("Step 1", "Step 2"),
  mandatory = c(TRUE, FALSE)
)

ll.proc2 <- list(
  fullname = 'PipeA_Proc2',
  mode = "process",
  steps = c("Step 1", "Step 2"),
  mandatory = c(TRUE, FALSE)
)

ll.proc3 <- list(
  fullname = 'PipeA_Proc3',
  mode = "process",
  steps = c("Step 1", "Step 2"),
  mandatory = c(TRUE, FALSE)
)


files.proc1 <- createModuleTemplate(ll.proc1, path = path)
files.proc2 <- createModuleTemplate(ll.proc2, path = path)
files.proc3 <- createModuleTemplate(ll.proc3, path = path)


lapply(files.proc1[which(grepl('.R', files.proc1))], 
       function(f) 
         source(file.path(path, f), local=FALSE))

lapply(files.proc2[which(grepl('.R', files.proc2))], 
       function(f) 
         source(file.path(path, f), local=FALSE))
lapply(files.proc3[which(grepl('.R', files.proc3))], 
       function(f) 
         source(file.path(path, f), local=FALSE))


# Create the pipeline workflow
ll.pipe <- list(
  mode = "pipeline",
  fullname = 'PipeA',
  steps = c("Proc 1", "Proc 2", "Proc 3"),
  mandatory = c(TRUE, FALSE, TRUE)
  )
files.pipeA <- createModuleTemplate(ll.pipe, path = path)

R.files <- files.pipeA[which(grepl('.R', files.pipeA))]
lapply(R.files, 
       function(f) 
         source(file.path(path, f), local=FALSE)
       )

data(data_na)
run_workflow("PipeA", dataIn = data_na, path = path)

unlink(c(files.proc1, files.proc2, files.proc3, files.pipeA))


}