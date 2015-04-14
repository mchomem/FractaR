##################################################################
#                                                                #
#    Gerador de fractais baseado nos conjuntos de Mandelbrot.    #
#                                                                #
#    Autor:      Misael C. Homem                                 #
#    Ano:        2015/1                                          #
#                                                                #
#    Descrição: Este script r gera iteracoes de fractais         #
#    baseado na equacao de Mandelbrot e as imprimi em frames     #
#    coloridos de um gif.                                        #
#                                                                #
##################################################################

# External package providing write.gif function
library(caTools)

# Funcao para gerar a fractal e gravar no disco.
genf <- function(x1, y1, x2, y2, max_frames_gif = 30, interval = 10, size = 600, equation, colors) {
    
    x1             <- as.numeric(x1)
    y1             <- as.numeric(y1)
    x2             <- as.numeric(x2)
    y2             <- as.numeric(y2)
    max_frames_gif <- as.numeric(max_frames_gif)
    interval       <- as.numeric(interval)
    size           <- as.numeric(size)
    
    # checkValues(x1, y1, x2, y2, max_frames_gif, interval, size)
    
    # Exibe mensagem no console informando o início do processo.    
    print( paste("[", as.character(Sys.time()), "]", "Gerando", max_frames_gif, "frames de fractal, aguarde...", sep = " ") )
    
    # Modo randomico com 1 palheta de 9 cores para cores.
    if(colors == "random") {
        
        ar <- c()
        
        for(n in 1:9) {
            
            num <- rnorm(n = 1, mean = 10000000, sd = 10000000)
            num <- as.integer(num)
            
            # Se valor negativo...
            if(num < 0) {
                # Muda o sinal
                num <- abs(num)
            }
            
            # Valor maximo para um inteiro que representa o
            # maximo valor para um hexa no caso #FFFFFF.
            if(num > 16777215) {
                num <- 16777215
            }
            
            # Valor minimo para um inteiro que representa o
            # minimo valor de um hexa no caso #000000.
            if(num < 1048575) {
                num <- 0
            }
            
            # Transforma o inteiro e hexa
            hex <- as.hexmode(x =  num)
            
            # Se o valor igual a zero...
            if(hex == "0") {
                # Atribui o caracter hexa com seis digitos no formato aceito pela funcao colorRampPalette
                ar[n] <- "#000000"
            } else {
                # Formata o valor hexa para o padrao hexa da funcao colorRampPalette
                ar[n] <- paste("#", hex, sep = "")
            }
            
        }
        
        # Atribui a paleta de cores randomizadas.
        jet.colors <- colorRampPalette( c(ar[1], ar[2], ar[3], ar[4], ar[5], ar[6], ar[7], ar[8], ar[9]) )
        
    }
    
    # Modo fire para cores
    if(colors == "fire") {
        jet.colors <- colorRampPalette( c("#000000", "#CC3300", "#FF9966", "#FFFF66", "#FFFFFF") )
    }
    
    # Modo water para cores
    if(colors == "water") {
        jet.colors <- colorRampPalette( c("#FFFFFF", "#000099", "#0066FF", "#99CCFF", "#FFFFFF") )
    }
    
    # Modo Preto no branco para cores
    if(colors == "black_write") {
        jet.colors <- colorRampPalette( c("#FFFFFF", "#000000") )
    }
    
    # Modo Cosmos para cores
    if(colors == "cosmos") {
        jet.colors <- colorRampPalette(c("#000000", "black", "#007FFF", "cyan", "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))
    }
    
    # Obtem um arranjo de numeros complexos
    C <- complex(real = rep(seq(x1, y1, length.out = size), each = size ), imag = rep(seq(y2, x2, length.out = size), size))
    
    # Dimensiona uma matriz de quadrados de numeros complexos
    C <- matrix(C, size, size)
    
    # Inicializa z
    Z <- 0
    
    # Inicializa a saida 3d do array
    X <- array(0, c(size, size, max_frames_gif))    
    
    # Equacao oringal de Mandelbrot
    if(equation == "original") {
        
        for (k in 1:max_frames_gif) {
            
            Z <- Z ^ 2 + C
            X[,,k] <- exp(-abs(Z))
            
        }
        
    }
    
    if(equation == "var_1") {
        
        for (k in 1:max_frames_gif) {
            
            Z <- Z ^ 3 + C
            X[,,k] <- exp(-abs(Z))
            
        }
        
    }
    
    if(equation == "var_2") {
        
        a <- 4
        
        for (k in 1:max_frames_gif) {
            
            Z <- Z ^ a + C
            X[,,k] <- exp(-abs(Z))
            
        }
        
    }
    
    if(equation == "var_3") {
        
        for (k in 1:max_frames_gif) {
        
            Z <- exp(Z) + C
            X[,,k] <- exp(-abs(Z))
                
        }
        
    }
    
    if(equation == "var_4") {
        
        for (k in 1:max_frames_gif) {
            
            Z <- sinh(Z) + C
            X[,,k] <- exp(-abs(Z))
            
        }
        
    }
    
    if(equation == "var_5") {
        
        for (k in 1:max_frames_gif) {
            
            Z <- cosh(Z) + C
            X[,,k] <- exp(-abs(Z))
            
        }
        
    }
    
    if(equation == "var_6") {
        
        for (k in 1:max_frames_gif) {
            
            Z <- tanh(Z) + C
            X[,,k] <- exp(-abs(Z))
            
        }
        
    }
    
    if(equation == "var_7") {
        
        for (k in 1:max_frames_gif) {
            
            Z <- sin(Z) + C
            X[,,k] <- exp(-abs(Z))
            
        }
        
    }
    
    if(equation == "var_8") {
        
        for (k in 1:max_frames_gif) {
            
            Z <- cos(Z) + C
            X[,,k] <- exp(-abs(Z))
            
        }
        
    }
    
    if(equation == "var_9") {
        
        for (k in 1:max_frames_gif) {
            
            Z <- tan(Z) + C
            X[,,k] <- exp(-abs(Z))
            
        }
        
    }
        
    fileGif <- paste("fractal", ".gif", sep = "")
    
    # Escreve o arquivo gif no disco, adicionando cada frame num intervalo determinado.
    write.gif(X, fileGif, col = jet.colors, delay = interval)
    
    # Imprimi no console a finalizacao do processo de geracao do fractal no disco.
    print( paste("[", as.character(Sys.time()), "] Arquivo gif gerado no caminho do projeto." ) )
    
}

# Funcao para fazer a consitencia de valores informados (sem uso)
checkValues <- function(vMax_frames_gif, vInterval, vSize) {
    
    if(vMax_frames_gif < 0 | vMax_frames_gif > 255 ) {
        vMax_frames_gif <- 24
    }
    
    if(vInterval < 0 | vInterval > 10) {
        vInterval <- 10
    }
    
    if(vSize < 100 | vSize > 1200) {
        vSize <- 600
    }
    
}
