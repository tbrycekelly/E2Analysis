
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(openxlsx)

shinyServer(function(input, output) {
    
    raw = reactive({
        infile = input$raw
        
        if (is.null(infile)) {
            return(NULL)
        }
        if (input$meta) {
            r = read.xlsx(infile$datapath)[-c(1,2),]
        } else {
            r = read.xlsx(infile$datapath)
        }
        colnames(r) = r[1,]
        r = r[-1,]
        r
    })
    
    checkStandards = reactive({
        r = raw()
        
        if (is.null(r)) {
            return(NULL)
        }
        
        checkTable = r[1,]
        
        l = which(r$Sample == '5.0 ppb ME Check Std')
        
        for (i in 1:length(l)) {
            checkTable[i,] = r[ l[i], ]
        }
        
        checkTable
    })
    
    finalData = reactive({
        ch = checkStandards()
        r = raw()
        
        if (is.null(ch)) {
            return(NULL)
        }
        
        f = r
        l = which(r$Sample == '5.0 ppb ME Check Std')
        
        for (i in 3:(nrow(r))-1) {
            orig = as.numeric(f[i, c(7:ncol(r))])
            index1 = max(l[ l<i ])
            index2 = min(l[ l>i ])
            
            if(index2 > nrow(r)) {
                index2 = index1
            }
            
            ch1 = as.numeric(f[index1, c(7:ncol(r))])
            ch2 = as.numeric(f[index2, c(7:ncol(r))])
            
            m = (ch1-ch2) / ch1 / (index1-index2) # relative change in check standard
            
            f[i, c(7:ncol(f))] = orig * (1 - m * (i-index1))
        }
        f[nrow(r),] = f[l[1],]
        f
    })
    
    normData = reactive({
        f = finalData()
        r = raw()
            
        if (is.null(f)) {
            return(NULL)
        }
        
        normalized = f
        l = which(r$Sample == '5.0 ppb ME Check Std')
        ch1 = as.numeric(f[l[1],c(7:ncol(f))])
        
        for (i in l[2]:nrow(f)) {
            normalized[i,c(7:ncol(f))] = as.numeric(f[i,c(7:ncol(f))]) * ch1 /
                as.numeric(f[max(l[l<=i]), c(7:ncol(f))])
        }
        normalized
    })
    
    
    output$preview = renderDataTable({
        raw()
        })
    
    output$checks = renderDataTable({
        checkStandards()
    })
    
    output$final = renderDataTable({
        finalData()
    })
    
    output$norm = renderDataTable({
        normData()
    })
    
    output$dow = downloadHandler(
        filename = function() {
            infile = input$raw
            
            if (is.null(infile)) {
                return(NULL)
            }
            return(paste0(infile$name, "-Normalized.xlsx"))
        },
        content = function(file) {
            write.xlsx(normData(), file) 
        }
    )
    
    output$previewPlot = renderPlot({
        if (is.null(normData())) {
            return(NULL)
        }
        r = raw()
        l = which(r$Sample == '5.0 ppb ME Check Std')
        i = as.numeric(input$whichCheck) + 6
        name = colnames(r)[i]
        r = as.numeric(r[,i])
        f = finalData()
        f = as.numeric(f[,i])
        n = normData()
        n = as.numeric(n[,i])
        
        
        x = c(1:length(l))
        
        plot(x, r[l], xlab="Check Standard", ylim=c(0.8*min(r[l]),1.2*max(r[l])), ylab=paste0(name," CPS"))
        points(x, f[l], pch=2, col='#af0000')
        points(x, n[l], pch=16, col='#ff0000')
        lines(x, n[l], pch=16, col='#ff000070')
        legend(1, 0.5*max(r[l]) + 0.5*min(r[l]), c("Raw","Drifted", "Normalized"), col=c("black", '#af0000', '#ff0000'), pch=c(1,2,16))
        
    })
})
