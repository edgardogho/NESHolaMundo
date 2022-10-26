.linecont       +               ; Permitir continuar lineas
.feature        c_comments      /* Soportar comentarios tipo C */

;Definir el segmento HEADER para que FCEUX reconozca el archivo .nes 
;como una imagen valida de un cartucho de NES

.segment "HEADER"

; Configurar con Mapper NROM 0 con bancos fijos
  .byte 'N', 'E', 'S', $1A    ; Firma de NES para el emulador
  .byte $02                   ; PRG tiene 16k 
  .byte $01                   ; CHR tiene 8k (archivo chr)
  .byte %00000000             ;NROM Mapper 0
  .byte $0, $0, $0, $0, $0, $0
; Fin del header 
;; Este Header se encuentra definido en el archivo link.x el cual
;; define donde va a quedar en el archivo .NES final


;Incluir el binario con las imagenes de la rom de caracteres
.segment "IMG"
.incbin "letras.chr"


;Declaracion de variables en la pagina 0
;; Esto es RWM (RAM), por ende se "reservan" bytes
;; para luego ser usados como variables.
.segment "ZEROPAGE"
	posX: .res 1
	posY: .res 1

;;Segmento de codigo guardado en la ROM
.segment "CODE"

;Rutina de interrupcion (IRQ)
;No es utilizada por ahora
irq:
	rti
	
;;Rutina de interrupcion (Reset)
;;; Esta rutina se dispara cuando el nintendo se enciende 
;;; o se aprieta el boton de reset. Se encarga de inicializar
;;; el hardware
reset:
  SEI          ; desactivar IRQs
  CLD          ; desactivar modo decimal
  
;;Durante el encendido del Nintendo hay que respetar unos tiempos 
  ;;hasta que el PPU se encuentra listo para ser utilizado.
  ;;A continuacion se siguen los pasos sugeridos en:
  ;; https://wiki.nesdev.com/w/index.php/Init_code
  LDX #$40
  STX $4017    ; disable APU frame IRQ
  LDX #$FF
  TXS          ; Set up stack
  INX          ; now X = 0
  STX $2000    ; disable NMI
  STX $2001    ; disable rendering
  STX $4010    ; disable DMC IRQs
  
vblankwait1:       ; First wait for vblank to make sure PPU is ready
  BIT $2002
  BPL vblankwait1

clrmem:
  LDA #$00
  STA $0000, x
  STA $0100, x
  STA $0300, x
  STA $0400, x
  STA $0500, x
  STA $0600, x
  STA $0700, x
  LDA #$FE
  STA $0200, x
  INX
  BNE clrmem
   
vblankwait2:      ; Second wait for vblank, PPU is ready after this
  BIT $2002
  BPL vblankwait2
  
  
  ;;Cargamos la paleta de colores.
  JSR SUBPaleta

  

  ;; Ahora que las paletas estan cargadas, podemos dibujar el fondo
  JSR SUBDibujaFondo

  ;; Encendemos el PPU 
  ;; y el barrido vertical 
  ;; y apuntamos el PPU a 
  ;; la tabla 0 de sprites 
  ;; y 1 para fondos
  
  LDA #%10000000 
  STA $2000
  
  ;; Encendemos Sprites, Background y sin clipping en lado izquierdo
  LDA #%00011110   
  STA $2001
  
  ;;Apagamos el scroll del background (fondo)
  LDA #$00       
  STA $2005
  STA $2005
  
  ;;Habilitamos las interrupciones
  CLI

fin:

  jmp fin
;Rutina de interrupcion (NMI)
;Esta rutina se dispara cuando la pantalla
;se dibujo por completo, y el barrido vertical 
;esta volviendo al inicio. Deberia poder utilizarse
;solo por 2250 ciclos aprox. Deberia dispararse 25 veces
;por segundo o 50 con interlaceado
nmi:
  ;;Guardamos en el stack el estado del CPU (flags y acumulador)
  PHA
  PHP
  
  ;;Ahora recuperamos el estado del CPU (flags y acumulador) y listo
  PLP
  PLA
  RTI 
  




;; Esta rutina dibuja el fondo (Background Tiles).
SUBDibujaFondo:
  LDA $2002             ; Hay que leer el PPU para resetear la posicion
  LDA #$20
  STA $2006             ; Se carga la parte alta de $2000
  LDA #$00
  STA $2006             ; Se carga la parte baja de $2000
  LDX #$00        
        
  ;;Una pantalla entera tiene 32x30 tiles, o sea 960 bytes.
  ;;Pero el registro X es de 8 bits, por ende tenemos que cargar 
  ;;de 256 bytes a la vez o usar modo indirecto indexado pero hay que
  ;;contar con 16 bits, asi que es mas facil cargarlo por partes.
   
  ;;Cargamos los primeros 256 bytes
LoadBackgroundLoop:
  LDA MapaHolaMundo, x     
  STA $2007              ;Grabamos el valor a PPU, que se autoincrementa
  INX                    ; incrementamos X para el loop.
  BNE LoadBackgroundLoop ; Si X pego la vuelta, se cargaron 256 bytes.
  

                        
LoadBackgroundLoop256:
  LDA MapaHolaMundo+256, x     
  STA $2007             
  INX                             
  BNE LoadBackgroundLoop256  
                        
LoadBackgroundLoop512:
  LDA MapaHolaMundo+512, x     
  STA $2007             
  INX                            
  BNE LoadBackgroundLoop512 
  
  ;;Este ultimo loop tiene que ir desde 768 hasta 960 solamente

LoadBackgroundLoop192:
  LDA MapaHolaMundo+512+256, x    
  STA $2007            
  INX                   
  CPX #192             
  BNE LoadBackgroundLoop192 
                       
 ;;Este loop carga los registros de atributos que estan a continuacion de los tiles.
 ;;Comienza apuntando el PPU a la memoria de atributos correspondiente

LoadAttribute:
  LDA $2002             
  LDA #$23
  STA $2006             
  LDA #$C0
  STA $2006             
  LDX #$00           
  
  ;;Ahora barremos los atributos y los vamos pasando al PPU   
LoadAttributeLoop:
  LDA MapaHolaMundo+960, x      
  STA $2007            
  INX                  
  CPX #64            
  BNE LoadAttributeLoop
  
  ;;Todo listo, podemos volver
  RTS
 
SUBPaleta:  
	lda #$3f
	sta $2006
	lda #$00
	sta $2006
	ldx #$21
	stx $2007
	lda #$14
	sta $2007
	lda #$10
	sta $2007
	lda #$30
	sta $2007
	stx $2007
	lda #$01
	sta $2007
	lda #$21
	sta $2007
	lda #$31
	sta $2007
	stx $2007
	lda #$06
	sta $2007
	lda #$16
	sta $2007
	lda #$26
	sta $2007
	stx $2007
	lda #$09
	sta $2007
	lda #$19
	sta $2007
	lda #$29
	sta $2007
	RTS
  
  
MapaHolaMundo:
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$28,$00,$2f,$00,$2c,$00,$21,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$01,$01,$01,$2d,$35,$2e,$24,$2f,$01,$01
	.byte $01,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$04,$09,$07,$01,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00





    
; Direcciones para las ISR
.segment "VECTORS" 
.word nmi
.word reset
.word irq
