.set cpu "intel,8080"
.set os "dr,cp/m"

.set BDOS 0x05
.set PRINT_STR 0x09

.section code
entry:
	mvi c 0x5
	lxi de hello_str
	call BDOS
	jc 0
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
