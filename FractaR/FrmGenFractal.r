##################################################################
#                                                                #
#    Camada visual para o gerador de fractais                    #
#                                                                #
#    Author:      Misael C. Homem                                #
#    Year:        2015/1                                         #
#                                                                #
#    Descreption: Configura uma interface gráfica para           #
#    final.                                                      #
#                                                                #
##################################################################

# Carrega biblioteca previamente instalada no ambiente R
library("tcltk", lib.loc="C:/Program Files/R/R-3.1.3/library")
# Carrega um script do diretorio de trabalho
source("Fractal.r")

# Funcao  para carregar a UI do programa
load <- function() {
    
    # Gera um formulario
    form <- tktoplevel();
    
    # Variaveis iniciando valores dos campos.
    x1           <- tclVar("-1.7")
    y1           <- tclVar("1.7")
    x2           <- tclVar("1.7")
    y2           <- tclVar("-1.7")
    maxFramesGig <- tclVar("50")
    interval     <- tclVar("30")
    size         <- tclVar("700")
    rbValue      <- tclVar("original")
    rbColor      <- tclVar("random")
    filePath     <- tclVar(paste(getwd(),"/", "fractal.gif", sep = "" ) )

    
    # Configura os atributos e caracteristicas dos controles(tamanho, posicao, etc)
    initializeForm <- function() {
        
        tktitle(form) <- "Fractal Generator"
        
        ######################################################################################
        # Configuracao do frame principal (container)
        frameMain     <- tkframe(form)
        # Adiciona o frameMain no form
        tkgrid(frameMain)
        ######################################################################################
        
        
        
        ######################################################################################
        # Configuracao do frame para os controles de area, adicionado ao frameMain
        # + a configuracao do frame para os controles de configuracao do gif
        frameArea     <- tkframe(frameMain, relief = "groove", borderwidth = 3)
        
        tkgrid(tklabel(frameArea, text = "Area"), columnspan = 4)
        tkgrid(tklabel(frameArea, text = " ")) # linha em branco para efeito de espaçamento
        
        lblX1         <- tklabel(frameArea, text = "X1: ")
        txtX1         <- tkentry(frameArea, width = "10", textvariable = x1)
        tkgrid(tklabel(frameArea, text = " "), lblX1, txtX1, tklabel(frameArea, text = "    "))
        
        lblY1         <- tklabel(frameArea, text = "Y1: ")
        txtY1         <- tkentry(frameArea, width = "10", textvariable = y1)
        tkgrid(tklabel(frameArea, text = " "), lblY1, txtY1, tklabel(frameArea, text = "    "))
        
        lblX2         <- tklabel(frameArea, text = "X2: ")
        txtX2         <- tkentry(frameArea, width = "10", textvariable = x2)
        tkgrid(tklabel(frameArea, text = " "), lblX2, txtX2, tklabel(frameArea, text = "    "))
        
        lblY2         <- tklabel(frameArea, text = "Y2: ")
        txtY2         <- tkentry(frameArea, width = "10", textvariable = y2)
        tkgrid(tklabel(frameArea, text = " "), lblY2, txtY2, tklabel(frameArea, text = "    "))
        
        ######################################################################################
        # Configuracao do frame para os controles de configuracao do gif, adicionado ao frameMain
        frameConfigGif     <- tkframe(frameMain, relief = "groove", borderwidth = 3)
        # Adiciona os dois frames na mesma linha.
        tkgrid(frameArea, frameConfigGif)
        
        tkgrid(tklabel(frameConfigGif, text = "Atributos"), columnspan = 4)
        tkgrid(tklabel(frameConfigGif, text = " "))
        
        lblmaxFramesGig <- tklabel(frameConfigGif, text = "Max Frames: ")
        txtmaxFramesGig <- tkentry(frameConfigGif, width = "20", textvariable = maxFramesGig)
        tkgrid(tklabel(frameConfigGif, text = " "), lblmaxFramesGig, txtmaxFramesGig, tklabel(frameConfigGif, text = " "))
        
        lblInterval     <- tklabel(frameConfigGif, text = "Interval (seconds): ")
        txtInterval     <- tkentry(frameConfigGif, width = "20", textvariable = interval)
        tkgrid(tklabel(frameConfigGif, text = " "), lblInterval, txtInterval, tklabel(frameConfigGif, text = " "))
        
        lblSize         <- tklabel(frameConfigGif, text = "Size (pixel): ")
        txtSize         <- tkentry(frameConfigGif, width = "20", textvariable = size)
        tkgrid(tklabel(frameConfigGif, text = " "), lblSize, txtSize, tklabel(frameConfigGif, text = " "))
        
        tkgrid(tklabel(frameConfigGif, text = " "))
        ######################################################################################
        
        
        
        ######################################################################################
        # Configuracao dos controles para selecao do conjunto de Mandelbrot e variacoes
        frameSets     <- tkframe(frameMain, relief = "groove", borderwidth = 3)
        
        lblSetsMandelbrot <- tklabel(frameSets, text = "Conjunto de Mandelbrot")
        tkgrid(lblSetsMandelbrot)
        
        lblSetOriginal <- tklabel(frameSets, text = "Original")
        rbOriginal     <- tkradiobutton(frameSets)
        tkconfigure(rbOriginal, variable = rbValue, value = "original")
        tkgrid(lblSetOriginal, rbOriginal)
        
        lblVar1        <- tklabel(frameSets, text = "Var 1")
        rbVar1         <- tkradiobutton(frameSets)
        tkconfigure(rbVar1, variable = rbValue, value = "var_1")
        tkgrid(lblVar1, rbVar1)
        
        lblVar2        <- tklabel(frameSets, text = "Var 2")
        rbVar2         <- tkradiobutton(frameSets)
        tkconfigure(rbVar2, variable = rbValue, value = "var_2")
        tkgrid(lblVar2, rbVar2)
        
        lblVar3        <- tklabel(frameSets, text = "Var 3")
        rbVar3         <- tkradiobutton(frameSets)
        tkconfigure(rbVar3, variable = rbValue, value = "var_3")
        tkgrid(lblVar3, rbVar3)
        
        lblVar4        <- tklabel(frameSets, text = "Var 4")
        rbVar4         <- tkradiobutton(frameSets)
        tkconfigure(rbVar4, variable = rbValue, value = "var_4")
        tkgrid(lblVar4, rbVar4)
        
        lblVar5        <- tklabel(frameSets, text = "Var 5")
        rbVar5         <- tkradiobutton(frameSets)
        tkconfigure(rbVar5, variable = rbValue, value = "var_5")
        tkgrid(lblVar5, rbVar5)
        
        lblVar6        <- tklabel(frameSets, text = "Var 6")
        rbVar6         <- tkradiobutton(frameSets)
        tkconfigure(rbVar6, variable = rbValue, value = "var_6")
        tkgrid(lblVar6, rbVar6)
        
        lblVar7        <- tklabel(frameSets, text = "Var 7")
        rbVar7         <- tkradiobutton(frameSets)
        tkconfigure(rbVar7, variable = rbValue, value = "var_7")
        tkgrid(lblVar7, rbVar7)
        
        lblVar8        <- tklabel(frameSets, text = "Var 8")
        rbVar8         <- tkradiobutton(frameSets)
        tkconfigure(rbVar8, variable = rbValue, value = "var_8")
        tkgrid(lblVar8, rbVar8)
        
        lblVar9        <- tklabel(frameSets, text = "Var 9")
        rbVar9         <- tkradiobutton(frameSets)
        tkconfigure(rbVar9, variable = rbValue, value = "var_9")
        tkgrid(lblVar9, rbVar9)
        
        ######################################################################################
        # Configuracao dos controles para configuracao de cores
        frameColors    <- tkframe(frameMain, relief = "groove", borderwidth = 3)
        tkgrid(frameSets, frameColors)
        
        lblColors      <- tklabel(frameColors, text = "Cores")
        tkgrid(lblColors)
        
        lblRandom      <- tklabel(frameColors, text = "Ramdom")
        rbRandom       <- tkradiobutton(frameColors)
        tkconfigure(rbRandom, variable = rbColor, value = "random")
        tkgrid(lblRandom, rbRandom)
        
        lblFire        <- tklabel(frameColors, text = "Fire")
        rbFire         <- tkradiobutton(frameColors)
        tkconfigure(rbFire, variable = rbColor, value = "fire")
        tkgrid(lblFire, rbFire)
        
        lblWater        <- tklabel(frameColors, text = "Water")
        rbWater         <- tkradiobutton(frameColors)
        tkconfigure(rbWater, variable = rbColor, value = "water")
        tkgrid(lblWater, rbWater)
        
        lblBlackWrite  <- tklabel(frameColors, text = "Black & Write")
        rbBlackWrite   <- tkradiobutton(frameColors)
        tkconfigure(rbBlackWrite, variable = rbColor, value = "black_write")
        tkgrid(lblBlackWrite, rbBlackWrite)
        
        lblCosmos  <- tklabel(frameColors, text = "Cosmos")
        rbCosmos   <- tkradiobutton(frameColors)
        tkconfigure(rbCosmos, variable = rbColor, value = "cosmos")
        tkgrid(lblCosmos, rbCosmos)
        
        ######################################################################################
        
        
        
        ######################################################################################
        # Configuracao dos controles para gravar o arquivo .gif.
        frameFilePath    <- tkframe(frameMain, relief = "groove", borderwidth = 3)
        tkgrid(frameFilePath, sticky = "w", columnspan = 2)
        
        lblFilePath      <- tklabel(frameFilePath, text = "Caminho: ")
        txtFilePath      <- tkentry(frameFilePath, width = "50", textvariable = filePath)
        btnSaveFile      <- tkbutton(frameFilePath, text = "...", command = btnSaveFile_Click)
        tkgrid(tklabel(frameFilePath, text = " "), lblFilePath, txtFilePath, btnSaveFile, tklabel(frameFilePath, text = " "))
        tkbind(txtFilePath, "<Return>", btnSaveFile_Click)
        ######################################################################################
        
        
        
        ######################################################################################
        # Configuracao do botao para chamar a funcao que ira gerar o fractal.
        bntGen           <- tkbutton(form, text = "  Gerar  ", command = bntGen_Click)
        tkgrid(bntGen, sticky ="s")
        ######################################################################################
        
        
        tkgrid(tklabel(form, text = " "))
        
        tkfocus(form)
            
    }

    # Funcao para chamar a geracao de fractais
    bntGen_Click <- function() {
        
        lX1           <- tclvalue(x1)
        lY1           <- tclvalue(y1)
        lX2           <- tclvalue(x2)
        lY2           <- tclvalue(y2)
        lMaxFramesGig <- tclvalue(maxFramesGig)
        lInterval     <- tclvalue(interval)
        lSize         <- tclvalue(size)
        lrbVal        <- as.character(tclvalue(rbValue))
        lrbColor      <- as.character(tclvalue(rbColor))
        
        
        # TODO revisar o if não esta funcionando corretamente
        # Implementar um tryCatch aqui sera melhor
        if(!is.numeric(as.numeric(lX1))
           | !is.numeric(as.numeric(lY1))
           | !is.numeric(as.numeric(lX2))
           | !is.numeric(as.numeric(lY2))
           | !is.numeric(as.numeric(lMaxFramesGig))
           | !is.numeric(as.numeric(lInterval))
           | !is.numeric(as.numeric(lSize)) ) {
            
            tkmessageBox(message = "Informe somente dados numericos!", icon = "warning")
            
        } else {
            
            # Chama a funcao para gerar fractal.
             genf(lX1, lY1, lX2, lY2, lMaxFramesGig, lInterval, lSize, lrbVal, lrbColor)
            # Exibe uma mensagem.
            tkmessageBox(message = "Fractal gerada com sucesso." )
            # Formata o caminho.
            fileOpen <- paste("file://", getwd(), "/", "fractal.gif", sep = "")
            # Abre o gif via navegador.
            browseURL(fileOpen, "C:/Program Files (x86)/Mozilla Firefox/firefox.exe")
            
        }
        
    }
    
    btnSaveFile_Click <- function() {
        
        extensions <- "{{GIF Files} {.gif}} {{All files} *}"
        
        filePath <- tclvalue(tkgetSaveFile(initialfile = "fractal.gif", filetypes = extensions))
        
        if (!nchar(filePath)) {
            tkmessageBox(message = "Selecione um arquivo para salvar a fractal.", icon = "warning")
        } else {
            tkmessageBox(message = paste("Arquivo selecionado", filePath))
        }
        
        print(filePath)
        return(filePath)
        
    }
    
    # Chama a funcao registrando tudo no ambiente do R.
    initializeForm()
    
}
