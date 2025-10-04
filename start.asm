.286
.model small
.stack 100h

.data
    fileName    db 'words.txt', 0
    handle      dw ?
    buffer1     db 100 dup(?)
    newline     db 13, 10, '$'
    choice_prompt db 'Enter 1 for Dictionary, 2 for File Input: $'
    word_prompt db 'Enter word (press Enter to save, space to exit): $'
    choice      db ?

.code
org 100h
start:
    mov ax, @data
    mov ds, ax
    mov ah, 9
    lea dx, choice_prompt
    int 21h
    call GetChoice
    cmp choice, '1'
    je run_dict_program
    call CheckSecondChoice
    jmp exit_program

run_dict_program:
    lea dx, newline
    int 21h
    xor dx, dx
    jmp install
    
    mess1   db "Slovar",13,10
            db "Press Shift to activate",13,10,"$"
    mess2   db "Slovar already installed",13,10
            db "Press Shift to activate",13,10,"$"
    active  db 0        ; flag - Program is active
    savx    db 0        ; saved cursor position
    savy    db 0
    curx    db 0        ; current cursor position
    cury    db 0
    attr = 95
    buflen = 10
    wordlen = 10        ; word length in bytes
    dictlen = 6         ; dictionary length in lines
    iword   db wordlen dup (32)
    trans   db wordlen dup (32),0
    nfnd    db "not  found",0

    buf     db buflen+1 ; input string buffer
    bufcnt  db 0
            db buflen+1 dup (0)

    line1   db "------------",0
    line2   db "|           |",0

    scrbuf  dw 5*(buflen+2) dup (0) ; screen save buffer

    POP_KEY = 42        ; Shift - activation key
    old9    label dword ; original interrupt int 9
    old9ip  dw 0
    old9cs  dw 0
    buffer  db 1000 dup(?) ; Array for storing data from file

; our interrupt int 9 taken from Dan Rollings Tech Help
new9:
    push ax
    in al, 60H          ; read the key
    cmp al, POP_KEY     ; is this the hot key?
    je do_pop           ; yes, trigger the popup
                        ; no, drop through to original driver
    pop ax
    jmp cs:old9         ; just hop out to original int handler

; activation key pressed
do_pop:
    ; ------ the following housekeeping is needed to satisfy the hdwr int
    in al, 61H          ; get value of keyboard control lines
    mov ah, al          ; save it
    or al, 80h          ; set the "enable kbd" bit
    out 61H, al         ; and write it out the control port
    xchg ah, al         ; fetch the original control port value
    out 61H, al         ; and write it back
    mov al, 20H         ; send End-Of-Interrupt signal
    out 20H, al         ; to the 8259 Interrupt Controller
    
    cmp cs:active, 0    ; program already active - do nothing
    jnz not_pop
    mov cs:active, 1    ; start program
    push ds             ; save all registers
    push es
    pusha
    sti                 ; enable interrupts
    call beg            ; actually call the program
    popa                ; restore all registers
    pop es
    pop ds
    mov cs:active, 0
not_pop:
    pop ax              ; restore AX since it was saved
    iret                ; return from interrupt

; save screen before displaying window
savescr:
    push ds
    mov ax, 0B800h
    mov ds, ax
    mov si, 0
    mov di, offset scrbuf
    mov cx, 5
sav0:
    push cx
    push si
    mov cx, buflen+2
    rep movsw
    pop si
    add si, 160
    pop cx
    loop sav0
    pop ds
    ret

; restore screen after displaying window
resscr:
    push es
    mov ax, 0B800h
    mov es, ax
    mov di, 0
    mov si, offset scrbuf
    mov cx, 5
res0:
    push cx
    push di
    mov cx, buflen+2
    rep movsw
    pop di
    add di, 160
    pop cx
    loop res0
    pop es
    ret

blank:
    mov ax, 0003h
    int 10h
    ret

; move cursor when BS is pressed
bkspace:
    dec curx
setcur:
    mov bh, 0
    mov dl, curx
    mov dh, cury
    mov ah, 2
    int 10h
    ret

; output character al and shift cursor
putc:
    mov bh, 0
    mov dl, curx
    mov dh, cury
    mov ah, 2
    int 10h
    mov cx, 1
    mov bl, attr
    mov ah, 9
    int 10h
    inc curx
    mov dl, curx
    mov dh, cury
    mov ah, 2
    int 10h
    ret

putc1:
    mov bh, 0
    mov dl, curx
    mov dh, cury
    mov ah, 2
    int 10h
    mov cx, 1
    mov bl, 13
    mov ah, 9
    int 10h
    inc curx
    mov dl, curx
    mov dh, cury
    mov ah, 2
    int 10h
    ret

putc2:
    mov bh, 0
    mov dl, curx
    mov dh, cury
    mov ah, 2
    int 10h
    mov cx, 1
    mov bl, 9
    mov ah, 9
    int 10h
    inc curx
    mov dl, curx
    mov dh, cury
    mov ah, 2
    int 10h
    ret

; input string similar to function 10 int 21h, but only through BIOS
input:
    mov cl, buflen
    mov ch, 0
    mov di, offset buf+2
    mov al, 0
    rep stosb
    mov bufcnt, 0
    mov di, offset buf+2
nextinp:
    call setcur
    mov ah, 2
    int 16h
    test al, 2
    jnz inputesc        ; left shift pressed
    mov ah, 0
    int 16h
    cmp al, 8
    jz inputbsp
    cmp al, 27
    jz inputesc
    cmp al, 13
    jz inputex
    cmp al, 32
    jc nextinp
    mov cl, bufcnt
    inc cl
    cmp cl, buf
    jnc nextinp
    stosb
    inc bufcnt
    call putc
    jmp nextinp
inputex:
    mov al, 13
    stosb
    clc
    ret

inputesc:
    mov al, 13
    stosb
    stc
    ret

inputbsp:
    mov al, bufcnt
    cmp al, 0
    jz nextinp
    dec di
    mov al, 0
    mov [di], al
    dec bufcnt
    call bkspace
    mov al, 32
    call putc
    call bkspace
    jmp nextinp

; print Z-string with address SI
print:
    lodsb
    cmp al, 0
    jz print1
    call putc
    jmp print
print1:
    ret

print_1:
    lodsb
    cmp al, 0
    jz print1
    call putc1
    jmp print_1

print_2:
    lodsb
    cmp al, 0
    jz print1
    call putc2
    jmp print_2

; main program - dictionary
beg:
    push cs
    pop ds
    push cs
    pop es

    mov bh, 0
    mov ah, 3
    int 10h
    mov savy, dh
    mov savx, dl

    call savescr
    ; call blank
mainloop:
    mov curx, 0         ; display window
    mov cury, 0
    mov si, offset line1
    call print_1
    mov curx, 0
    mov cury, 1
    mov si, offset line2
    call print_2
    mov curx, 0
    mov cury, 2
    mov si, offset line1
    call print_1
    mov curx, 0
    mov cury, 3
    mov si, offset line2
    call print_2
    mov curx, 0
    mov cury, 4
    mov si, offset line1
    call print_1

    mov curx, 1
    mov cury, 1
    call input          ; enter text string
    jc exdos            ; ESC was pressed - exit

    mov cl, bufcnt      ; empty string entered - exit
    mov ch, 0
    cmp cl, 0
    jz exdos

    push cx
    mov di, offset iword
    mov cx, wordlen
    mov al, 32          ; space
    rep stosb
    pop cx
    mov di, offset iword
    mov si, offset buf+2
    rep movsb

    call translate      ; actually perform translation
    jnc m10
    mov si, offset nfnd ; word not found
m10:
    mov di, offset trans ; copy translation to output string
    mov cx, wordlen
    rep movsb

    mov curx, 1
    mov cury, 3
    mov si, offset trans ; output translation
    call print
    mov ah, 0           ; wait for any key press
    int 16h
    jmp mainloop
exdos:
    call resscr         ; erase window and restore cursor position
    mov al, savx
    mov curx, al
    mov al, savy
    mov cury, al
    call setcur
    ret

; resident installer program
install:
    push cs
    pop ds

    ; save old interrupt vector
    mov ax, 3509h
    int 21h
    mov word ptr old9ip, bx
    mov word ptr old9cs, es
    cmp bx, offset new9
    jz already
    
    ; set new interrupt vector
    mov ax, 2509h
    mov dx, offset New9
    int 21h

    ; Open file for reading
    mov ah, 3Dh         ; Function 3Dh - open file
    mov al, 0           ; Open mode (0 - for reading)
    lea dx, fileName    ; Pointer to file name
    int 21h
    mov handle, ax      ; Save file handle

    ; Read data from file into array
    mov ah, 3Fh         ; Function 3Fh - read from file
    mov bx, handle
    mov cx, 1000        ; Maximum number of bytes to read
    lea dx, buffer      ; Pointer to buffer
    int 21h

    mov ah, 3Eh         ; Function 3Eh - close file
    mov bx, handle      ; Pass file handle
    int 21h

    mov dx, offset mess1
    mov ah, 9
    int 21h
    mov dx, (offset erdos) + 100*(wordlen+2) ; resident length address
    mov cl, 4
    shr dx, cl          ; dx=dx/16+1 ; resident size in paragraphs
    inc dx
    mov ax, 3100h       ; terminate and stay resident
    int 21h
already:
    mov dx, offset mess2 ; if program already loaded, then exit
    mov ah, 9
    int 21h
    mov ax, 4C01h
    int 21h

; dictionary search
translate:
    mov si, offset buffer
    mov cx, 0
m0:
    push cx
    push si
    mov di, offset iword
    mov cx, wordlen
    repe cmpsb
    pop si
    pop cx
    je tfound
    add si, wordlen+2
    inc cx
    cmp cx, dictlen
    jc m0
    stc
    ret
tfound:
    test cx, 1
    jnz rus2eng
    add si, wordlen+2
    clc
    ret
rus2eng:
    sub si, wordlen+2
    clc
    ret

; the dictionary itself should be attached here!
erdos label byte

CheckSecondChoice proc
    cmp choice, '2'
    je run_file_program
    ret
CheckSecondChoice endp

run_file_program:
    lea dx, newline
    int 21h
input_loop:
    ; Display prompt
    mov ah, 9
    lea dx, word_prompt
    int 21h

    ; Read input
    call read_input
    cmp si, 0           ; If empty input (just Enter)
    je exit_program

    ; Open file in append mode
    mov ah, 3Dh
    mov al, 1           ; Write-only with append
    lea dx, filename
    int 21h
    jc create_file
    mov handle, ax
    jmp write_to_file

create_file:
    mov ah, 3Ch
    mov cx, 0
    lea dx, fileName
    int 21h
    mov handle, ax

write_to_file:
    ; Move to end of file
    mov ah, 42h
    mov bx, handle
    mov al, 2
    xor cx, cx
    xor dx, dx
    int 21h

    ; Write the word
    mov ah, 40h
    mov bx, handle
    mov cx, si
    lea dx, buffer1
    int 21h

    ; Write newline
    mov ah, 40h
    mov bx, handle
    mov cx, 2
    lea dx, newline
    int 21h

    ; Close file
    mov ah, 3Eh
    mov bx, handle
    int 21h

    jmp input_loop

read_input proc
    mov si, 0
    lea di, buffer1
read_char:
    mov ah, 1
    int 21h
    cmp al, 13          ; Enter
    je end_input_read
    cmp al, 32          ; Space
    je exit_program
    mov [di], al
    inc di
    inc si
    jmp read_char
end_input_read:
    ret
read_input endp

GetChoice proc
    mov ah, 1
    int 21h
    mov choice, al
    ret
GetChoice endp

exit_program:
    mov ah, 4Ch
    int 21h

end start
