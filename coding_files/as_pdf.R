as_pdf <- function(x){
  require(tools)
  
  fname <- tempfile(pattern = "texview-", tmpdir = tempdir(),
                    fileext = ".tex")
  
  header <- "\\documentclass{article}
  \\usepackage[margin=10pt,font=small,labelformat=empty,
  labelsep=none]{caption}
  \\usepackage{dcolumn}
  \\thispagestyle{empty}
  \\begin{document}"
  footer <- "\\end{document}"
  
  cat(header, file=fname, sep='\n')
  cat(x, file=fname, append=TRUE)
  cat(footer, file=fname, append=TRUE, sep='\n')
  
  newfile <- paste0(file_path_sans_ext(fname), ".pdf")
  cropfile <- paste0(file_path_sans_ext(fname), "-crop.pdf")
  
  origdir <- getwd()
  tryCatch({
    setwd(tempdir()) ## next to the tex file
    texi2pdf(fname, clean=TRUE)
    system2("pdfcrop", args=c("--margins", "10", newfile, cropfile),
            stdout=FALSE)
  }, finally=setwd(origdir))
  
  system2("open", args=c(cropfile))
  ## shell.exec(file_path_as_absolute(cropfile)) ## for Windows, probably
}