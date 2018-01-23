dane1 segment

	args	db 256 dup('$')
    arg_start db 128 dup(0)          ;n-ty bajt to indeks poczatku n-tego arg. 
    arg_length db 128 dup(0)          ;dlugosc n-tego arg.
    arg_num db 0

	xmin dq ?
	xmax dq ?
	ymin dq ?
	ymax dq ?
	cr dq ?
	ci dq ?

	four dq 4.0
	ten dq 10.0
	tenth dq 0.1
	minus dq -1.0

	NMAX dq 320.0
	MMAX dq 200.0
	x		DQ ?  ;23.5
	y		DQ ?  ;17.2
	digit dw ?
	tmp dq ?
    
dane1 ends
    
code1 segment
xx dw 60
yy dw 80
kol dw 15
is_negative db 0
temp dw 0
 
start1:
	.386
	.387




	mov	sp, offset wstosu
	mov	ax, seg wstosu
	mov	ss, ax

	xor bx,bx
	mov	bl,byte ptr ds:[080h]
	dec bl  
	mov cx,bx

	call eat_all
	
	finit
	call args_to_double
	
	call graph_mode
	call draw
	call wait_for_key
	call text_mode

exit:
	mov	ah,4ch 
	int	021h

	
	
	
	
;===============================================================================
                 
                 ;PARSER
                 
;===================================================================================	
eat_all: 
; brak parametrow
    
    push ax
    push bx
    push es
    push si
    push bp
    
    mov ax,seg arg_start
    mov es,ax
    mov si,offset arg_start				;wspolrzedne poczatkow slow
    mov byte ptr es:[si],0             ;pierwszy arg. zaczyna sie w 0
    
	mov	ax,seg args                    
	mov	es,ax                          ;tu beda argumenty oddzielone spacjami
	mov	si,offset args
    
	mov bp,0                           ;indeks aktualnie wczytywanego znaku
	
	eat_all_l:                         ;WCZYTAJ
		call eat_spaces                ;zjedz spacje
		cmp bx,bp                      ;czy to koniec?
		jle eat_all_f
		
		call eat_arg                   ;zjedz kolejny argument
		cmp bx,bp                      ;czy to koniec?
		jle eat_all_f
		
		jmp eat_all_l                  ;petla
	eat_all_f:
	
	pop bp
	pop si
	pop es
	pop bx
	pop ax
	ret

eat_arg:
; kopiuje do stringa argument do bialego znaku
;PARAMETRY:
;       bp - indeks wczytywanego znaku
;       ds - segment programu (parametry) 
;       si - indeks docelowy w stringu args
;       es - segment args  
;       cx - ile znakow zostalo do wczytania
;ZWRACA:
;       cx, bp, si - aktualne
    
	eat_arg_l:
	    cmp byte ptr ds:[082h+bp]," "       ;bialy znak - koniec argumentu
		jz eat_arg_f  
		cmp byte ptr ds:[082h+bp],09h       ;tab
		jz eat_arg_f
		
		mov	al, byte ptr ds:[082h+bp]	  ;aktualny znak do al  
		mov	byte ptr es:[si],al           ;zapisz znak w stringu
		inc	bp
		inc	si                            ;indeksy do przodu
		
		loop eat_arg_l
	eat_arg_f:
	mov byte ptr es:[si]," "              ; dodaj separator " "
	inc si                                ; si ma indeks pierwszego znaku nast. arg.
	call inc_arg_num
	
	ret

eat_spaces:
; pomin biale znaki
;PARAMETRY:
;       bp - dany znak parametru
;       ds - segment programu (parametry)
;       cx - ile znakow zostalo do wczytania 
;ZWRACA:
;       bp, cx - aktualne

	eat_s_l:
		cmp byte ptr ds:[082h+bp]," "         ;czy to bialy znak?
		jz eat_s_continue
		cmp byte ptr ds:[082h+bp],09h         ;tab
		jz eat_s_continue
		jmp eat_s_f                           ;nie, to nie bialy znak
		
		eat_s_continue:
		inc	bp                                ;pomin ten znak
		loop eat_s_l
	eat_s_f:
	ret     

inc_arg_num:
; zwieksza arg_num po zjedzeniu arg., zapisuje jego dlugosc  
;PARAMETRY:
;       zmienna arg_num - dotychczasowa l. arg.
              
    push ax
    push bx
    push si
    push es
    
    xor ax,ax                                                  
                                                      
    mov bx,seg arg_num                     ;zwieksz liczbe arg.
    mov es,bx
    mov bx,offset arg_num
    mov al,byte ptr es:[bx]
    inc al
    mov byte ptr es:[bx],al         ;al - liczba wczytanych dotad arg.
    
    mov bx,seg arg_start          
    mov es,bx
    mov bx,offset arg_start
    add bx,ax                     ;nakierowanie na al-ty el. tablicy
    
    push ax                       ;bo numer tego arg. sie jeszcze przyda
    
    mov ax,si                     ;si - indeks pierwszego znaku arg. w stringu
    mov byte ptr es:[bx],al       ;zapisz poczatek arg.
									;spokojnie, nie przekroczy wart. 127 (ah=0)                    
    
    mov al,byte ptr es:[bx-1]     ;ax ma poczatek poprzedniego arg.
    sub si,ax                     ;si = roznica-1 = dlugosc
    dec si                        ;si zawiera dlugosc danego arg.
    
    pop ax                        ;ax znowu ma numer arg.
    
    mov bx,seg arg_length
    mov es,bx
    mov bx,offset arg_length      ; es:[bx] - tablica dlugosci
    add bx,ax                     ; ax-ty el.
    dec bx                        ; -1, zeby sie zgadzalo
    mov ax,si                     ; si to dlugosc arg.
    mov byte ptr es:[bx],al       ; al bo 8bit
    
    pop es
    pop si
    pop bx
    pop ax
    ret

;============================================================================


get_arg_start:
;PARAMETRY: ax - indeks arg., od 0
;ZWRACA: es - seg args
;		 si - offset tego arg.

	push ax
	push bx

	mov bx,seg arg_start
    mov es,bx
    mov si,offset arg_start         ;znajdz poczatek tego arg. 
    xor bx,bx
    add si,ax                       ;przesun na ax-ty element arg_start
    mov bl,byte ptr es:[si]         ; bl - ind. pierwszego znaku tego arg.
    
    mov ax,seg args
    mov es,ax
    mov si,offset args
    add si,bx                       ;ustaw si na pierwszym znaku tego arg.

	pop bx
	pop ax
	ret	


get_arg_len:
;PARAMETRY: ax - indeks arg., od 0
;ZWRACA: bx - dlugosc arg.
	push ax
	push es
	push si

	mov bx,seg arg_length
    mov es,bx
    mov si,offset arg_length         ;znajdz poczatek tego arg. 
    
    add si,ax                       ;przesun na ax-ty element arg_length
    xor bh,bh
	mov bl,byte ptr es:[si]
    
	pop si
	pop es
	pop ax
	ret		
	
;==========================================================================
;
;							ARGS TO DOUBLE	
;	
;==========================================================================
	
	
args_to_double:
; wczytuje argumenty do pamieci
	push ax
	push es
	push si
	
	xor ax,ax
	call arg_to_double
	
	push ax
		mov ax,seg xmin
		mov es,ax
		mov si, offset xmin
		fstp qword ptr es:[si]
	pop ax
	
	call arg_to_double
	
	push ax
		mov ax,seg xmax
		mov es,ax
		mov si, offset xmax
		fstp qword ptr es:[si]
	pop ax
	
	call arg_to_double
	
	push ax
		mov ax,seg ymin
		mov es,ax
		mov si, offset ymin
		fstp qword ptr es:[si]
	pop ax
	
	call arg_to_double
	
	push ax
		mov ax,seg ymax
		mov es,ax
		mov si, offset ymax
		fstp qword ptr es:[si]
	pop ax
	
	call arg_to_double
	
	push ax
		mov ax,seg cr
		mov es,ax
		mov si, offset cr
		fstp qword ptr es:[si]
	pop ax
	
	call arg_to_double
	
	push ax
		mov ax,seg ci
		mov es,ax
		mov si, offset ci
		fstp qword ptr es:[si]
	pop ax
	
	pop si
	pop es
	pop ax
	ret
	
arg_to_double:
;PARAMETRY: ax - indeks arg. (inkrementowany przy kazdym obiegu)
;ZWRACA: wczytana liczba na gorze stosu FPU

	push ax
	push bx
	push es
	push si
	
	call get_arg_start
	call get_arg_len
	;es - seg, si - offset poczatku arg.
	;bx - dlugosc arg.
	
	call parse_double
	
	pop si
	pop es
	pop bx
	pop ax
	inc ax
	ret
	
	




;========================================================================
;
;						PARSE TO DOUBLE
;
;========================================================================


	
	
	
	
parse_double:
;PARAMETRY: es:si - adres poczatku danego parametru wywolania
;			bx - dlugosc tego arg.

;1) ALL ?
;2) double | ?
	
	call parse_minus			;sprawdz pierwszy znak
	
	push si
		call parsef1			;przed kropka
		fstp st(1)				;na st1 bylo 10
	pop si
	call parsef2			;po kropce

	fstp st(1)				;na st1 bylo 0.1
	faddp st(1), st			;dodajemy czesc calk. do ulamkowej
	
	call add_minus			;jesli flaga ustawiona, pomnoz przez -1

	ret

parse_minus:
;ustawia flage na 1 jesli jest minus 
 
	push ax
	push es
	mov byte ptr cs:[is_negative],0
	
	cmp byte ptr es:[si],"-"
	jne parse_minus_f
 
	mov byte ptr cs:[is_negative],1
	inc si								;na pierwsza cyfre
	
	parse_minus_f:
	pop es
	pop ax
	ret
	
add_minus:
	push ax
	push es
	push si
	
	cmp byte ptr cs:[is_negative],1
	jne add_minus_f
	
	mov ax,seg minus
	mov es,ax
	mov si, offset minus
	fld qword ptr es:[si]
	fmulp st(1),st
	
	add_minus_f:
	pop si
	pop es
	pop ax
	ret
	
parsef1:
;1) ALL ?
;2) czesc calkowita | 10 | ? 
	push es
	push si
 
	mov ax,seg ten
	mov es,ax
	mov si, offset ten
	fld qword ptr es:[si]
	fldz
 
	pop si
	pop es

	parse_digit_l1:

		xor ch,ch
		mov cl,byte ptr es:[si]
		cmp cl,"."
		jz parse_digit_f1
	
		fmul st, st(1)
	
		sub cx, "0"
		mov cs:[temp],cx
		fild word ptr cs:[temp]
		faddp st(1), st
	
		inc si							;nie mozna zrobic osobnej procedury, bo tu inc a tam dec!
										;i tu mnozymy przed, a tam po
	
	jmp parse_digit_l1
	parse_digit_f1:
	ret	
	
parsef2: 
;1) czesc calkowita | ?
;2) czesc ulamkowa | 0.1 | czesc calk. | ?
	push es
	push si
	
	cmp byte ptr cs:[is_negative],1
	jne parsef2_moving_f
	dec bx								;jesli byl minus, popraw dlugosc arg.
	
	parsef2_moving_f:

	mov ax,seg tenth
	mov es,ax
	mov si, offset tenth
	fld qword ptr es:[si]
	fldz
 
	pop si
	pop es
	
	dec bx
	add si,bx

	parse_digit_l2:

		xor ch,ch
		mov cl,byte ptr es:[si]
		cmp cl,"."
		jz parse_digit_f2
	
		sub cx, "0"
		mov cs:[temp],cx
		fild word ptr cs:[temp]		
		faddp st(1), st
		
		fmul st, st(1)
	
		dec si
	
	jmp parse_digit_l2
	parse_digit_f2:
	ret



;============================================================================
	
	;						MAGIA
	
;============================================================================




calc:
	push ax
	push cx
	push es
	push si
	
	mov ax,seg ci
	mov es,ax
	mov si,offset ci
	fld qword ptr es:[si]				; ci
	
	mov ax,seg cr
	mov es,ax
	mov si,offset cr
	fld qword ptr es:[si]				; cr | ci

	call calc_zi						; y=zi | cr | ci
	call calc_zr						; x=zr | y=zi | cr | ci
	
	fld st								; x | x | y | cr | ci
	fld st								; x | x | x | y | cr | ci
	fmulp st(1), st						; x2 | x | y | cr | ci
	
	fld st(2)							; y | x2 | x | y | cr | ci
	fld st								; y | y | x2 | x | y | cr | ci
	fmulp st(1), st						; y2 | x2 | x | y | cr | ci
		
	mov cx,1000d
	calc_l:
		
		fsubp st(1), st					; x2-y2 | x | y | cr | ci
		
		fld st(3)						; cr | x2-y2 | x | y | cr | ci
		faddp st(1), st					; tmp = x2-y2+cr | x | y | cr | ci
	
		fxch st(2)						; y | x | tmp | cr | ci
		fld st							; y | y | x | tmp | cr | ci
		faddp st(1), st					; 2y | x | tmp | cr | ci
		fmulp st(1), st					; 2xy | x=tmp | cr | ci
		
		fld st(3)						; ci | 2xy | x=tmp | cr | ci
		faddp st(1), st					; y=2xy+ci | x | cr | ci
		
		fxch st(1)						; x | y | cr | ci
		fld st							; x | x | y | cr | ci
		fmul st, st(1)					; x2 | x | y | cr | ci
		
		fld st							; x2 | x2 | x | y | cr | ci
		fld st(3)						; y | x2 | x2 | x | y | cr | ci
		fld st							; y | y | x2 | x2 | x | y | cr | ci
		
		fmulp st(1), st					; y2 | x2 | x2 | x | y | cr | ci
		fxch st(1)						; x2 | y2 | x2 | x | y | cr | ci
		
		fadd st, st(1)					; x2+y2 | y2 | x2 | x | y | cr | ci
		
		mov ax,seg four
		mov es,ax
		mov si,offset four
		fild qword ptr es:[si]			; 4 | x2+y2 | y2 | x2 | x | y | cr | ci
		
		fcomp							; x2+y2 | y2 | x2 | x | y | cr | ci
		fstsw ax
		sahf
		jb calc_break
		
		fstp st							; y2 | x2 | x | y | cr | ci

	loop calc_l
	
	calc_ok:
	mov	byte ptr cs:[kol],0h
	jmp calc_done
	
	calc_break:
	
	sub cl,50h
	mov	byte ptr cs:[kol],cl
	
	jmp calc_done
	
	calc_done:
	
	pop si
	pop es
	pop cx
	pop ax
	ret

	


	
	
	
calc_zr:
	push ax
	push es
	push si
	
	mov ax,seg xmax
	mov es,ax
	mov si,offset xmax
	fld qword ptr es:[si]
	
	mov ax,seg xmin
	mov es,ax
	mov si,offset xmin
	fld qword ptr es:[si]
	
	fsubp st(1),st
	
	mov ax,seg NMAX
	mov es,ax
	mov si,offset NMAX
	fld qword ptr es:[si]
	
	fdivp st(1),st
	
	fild word ptr cs:[xx]
	
	fmulp st(1),st
	
	mov ax,seg xmin
	mov es,ax
	mov si,offset xmin
	fld qword ptr es:[si]
	
	faddp st(1),st
	
	pop si
	pop es
	pop ax
	
	ret
	
calc_zi:
	push ax
	push es
	push si
	
	mov ax,seg ymax
	mov es,ax
	mov si,offset ymax
	fld qword ptr es:[si]
	
	mov ax,seg ymin
	mov es,ax
	mov si,offset ymin
	fld qword ptr es:[si]
	
	fsubp st(1),st
	
	mov ax,seg MMAX
	mov es,ax
	mov si,offset MMAX
	fld qword ptr es:[si]
	
	fdivp st(1),st
	
	fild word ptr cs:[yy]
	
	fmulp st(1),st
	
	mov ax,seg ymin
	mov es,ax
	mov si,offset ymin
	fld qword ptr es:[si]
	
	faddp st(1),st
	
	pop si
	pop es
	pop ax
	ret

	



draw:
	mov	word ptr cs:[yy],0
	mov cx,200
	rows_l:
		push cx
		
		mov	word ptr cs:[xx],0
		
		mov cx,320
		cols_l:
			call calc
			call zapal_punkt
			inc word ptr cs:[xx]
		loop cols_l
		
		pop cx
		inc word ptr cs:[yy]
	loop rows_l
	ret

zapal_punkt:
	mov	ax,0a000h  ;adres segm pam. obrazu
	mov	es,ax
	mov	ax,word ptr cs:[yy]
	mov	bx,320
	mul	bx	; dx:ax = ax * bx
	add	ax,word ptr cs:[xx]   ;ax = 320*y +x
	mov	bx,ax
	mov	al,byte ptr cs:[kol]
	mov	byte ptr es:[bx],al
	ret

graph_mode:
	mov	al,13h  ;tryb graficzny 320x200 punktow 256 kolorow
	mov	ah,0
	int	10h
	ret
	
text_mode:
	mov	al,3h  ;tryb tekstowy
	xor ah,ah
	int	10h
	ret
	
wait_for_key:
	xor	ax,ax
	int	16h  ;czekaj na dow. klawisz
	ret
	
	
	
	
;============================================================================



;=============================================================================
	

	
code1 ends
 
 
 
stos1 segment stack
	dw 400 dup(?)
wstosu	dw ?
stos1 ends
 
 
end start1