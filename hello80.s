.cpu "intel,8080"
.os "dr,cp/m"

.set BDOS 0x05
.set BDOS_PLUS_ONE (BDOS + 1)
.set PRINT_STR (9*9/9+9-9)

.section code 0x100
entry:
	_ęśąćż
	mvi c 0x05
	lxi de hello_str
	call BDOS
	jc 0x0000
	ret

.set cpu "mos,6502"
entry_mos:
	mos d
	mos e
;	lda #40$
;	tay

.section data

hello_str:
	;.os_str "Hello, World!"
