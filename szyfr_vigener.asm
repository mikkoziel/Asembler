DATA1 segment

Key db 200 dup(?)	;klucz po przetworzeniu 
Code db 100 dup (?)  ;kod szyfrowania
Version db 0		;szyfrowanie/deszyfrowanie
InputFileName db 13 dup(?)   ;nazwa pliku wejscia
OutputFileName db 13 dup(?)  ;nazwa pliku wyjsciowego
Buffer db 10000 dup(?)        ;bufor odczytu			
CodeBuffer db 10000 dup(?)		;bufor zapisu
InputHandler	dw	?		;wskaŸnik do pliku wejœciowego
OutputHandler	dw	?		;wskaŸnik do pliku wyjœciowego
InputBuffPointer dw 0  ;wskaznik pozycji w buferze wejsciowym
OutputBuffPointer dw 0  ;wskaznik pozycji w buferze wyjsciowym
BufferSize	dw	0			;rozmiar pobranego bufora
Leng	db 4 dup (?)		;dlugosci argumentów
Counter db 0				;licznik pomocniczy do sprawdzenia z dlugosci kodu
CodeSize db 0				; dlugosc klucza (kodu)
KeyError 	db	"Blad w linii polecen! ", 10, 13, '$'
InputError	db	"Blad otwierania pliku wejsciowego", 10, 13, '$'
OutputError	db	"Blad otwierania pliku wyjsciowego", 10, 13, '$'



DATA1 ends

CODE1 segment
assume ss:STOS1, ds:DATA1 


start:
	
	mov ax, DATA1
	mov ds, ax
	mov ax,seg top1
	mov	ss,ax				;segment stosu do SS
	mov	sp,offset top1 

	
main:
		CALL argLoad
		CALL reWriteName
		CALL getHandler
		CALL vigenereCipher
		CALL exit
	
	
	
errorKey :
	mov dx, offset KeyError
	CALL errors


inputErr:
	mov dx, offset InputError
	CALL errors


outputErr:
	mov dx, offset OutputError
	CALL errors


errors :
	CALL printLine
	CALL exit




argLoad proc
	push si
	push di
	push ax
	push bx
	push cx
	mov bx, 0
	
	mov si, 81h					;od teraz si wskazuje na poczatek podanego argumentu
	mov di, offset Key
	CALL whiteSign			
	cmp al, 0
	JNE errorKey			;b³¹d jeœli s¹ same bia³e znaki
	argLoadLoop:			; pomijamy znaki i przepisujemy argumenty
		CALL Arg				;procedura przepisujaca do tablicy Key i liczaca dlugosc argumentow
		CALL whiteSign
		inc bx					;bx - ilosc argumentow
		cmp al, 0			;jeœli nie jest koniec lini dalej sklejaj argumenty
		JE	argLoadLoop
	CALL checkLength			;procedura sprawdza ilosc argumentow oraz ich dlugosc
argLoadEnd:
	
	
	pop cx
	pop bx
	pop ax
	pop di
	pop si
RET	

argLoad endp

checkLength proc				;procedura sprawdzajaca dlugosc argumetow i ich ilosc
	push si
	mov si, offset Leng
	cmp bx, 3				
	JE lEncryptMode				; jesli 3 argumenty to kodowanie
	cmp bx, 4
	JE lDecryptMode				; jesli 4 dekodowanie
	CALL errorKey

lDecryptMode:
	cmp byte ptr ds:[si], 2		
	JNE errorKey
	CALL checkVersion
	inc si
	
lEncryptMode: 
	cmp byte ptr ds:[si], 12
	JA errorKey
	inc si
	cmp byte ptr ds:[si], 12
	JA errorKey
	
	pop si
RET
checkLength endp
	


whiteSign:		;procedura pomijaj¹ca bia³e znaki. koñczy siê gdy natrafi na inny znak ni¿ spacja i tabulator 
	mov al,0	;flaga konca lini jeœli same bia³e znaki
	
	whiteSignStart:
		mov ah, byte ptr es:[si]		; w rejestrze es przechowujemy pobrane argumenty
		cmp ah, ' ' 
		JE whiteSignSkip
		cmp ah, 9d			;9d kod tabulatora
		JE whiteSignSkip
		cmp ah, 13			;znak konca linii
		JE et
	JMP whiteSignEnd
	whiteSignSkip:			
		inc si				;przesuwa wskaznik ojedno miejsce dalej
		JMP whiteSignStart
	et:
		mov al,1			;ustawia flage na 1 jeœli koniec linii
	whiteSignEnd:
RET


Arg:				; procedura liczaca dlugosc znakow i przepisujaca do Key bez spacji miedzy argumentami
	
	mov al, 0		;flaga koñca lini
	mov cl, 0		;licznik d³ugoœci argumentu



	ArgStart:	
		mov ah, byte ptr es:[si]		
		cmp ah, ' '			;sprawdzanie poprawnoœci argumentów 
		JE ArgEnd
		cmp ah, 9d			
		JE ArgEnd
		cmp ah, 13
		JE Arg1
	
		mov byte ptr ds:[di], ah	;przepisywanie do tablicy sklejonych argumentów 
		inc si				
		inc di				
		inc cl						;zwiekszamy dlugosc argumentu
		JMP ArgStart
	Arg1:
		mov 	al,1			;koniec lini ustawiamy al=1

	ArgEnd:
		push di
		mov di, offset Leng;
		mov byte ptr ds:[di+bx], cl
		pop di

RET	

checkVersion proc				;procedura sprawdzajaca czy pierwszy argumaent dla deszyfrowania to "-d"
	push si
	mov si, offset Key
	cmp byte ptr ds:[si], '-'
	JNE errorKey
	inc si
	cmp byte ptr ds:[si], 'd'
	JNE errorKey
	mov ds:[Version], 1
	pop si
RET
checkVersion endp

reWriteName proc					;procedura przepisujaca nazwe wyjscia i wejscia oraz klucza
	push si
	push di
	push bx
	mov si, offset Key
	
	cmp ds:[Version], 1
	JNE reWriteNameNorm
	add si, 2						;jesli jest deszyforwaniem to omijamy pierwsze 2 znaki
	
reWriteNameNorm:
	mov di, offset InputFileName		;przepisujemy nazwe wejscia
	CALL writeName
	mov bx, 1							;bx zmienia wskaznik w zmiennej Leng
	
	mov di, offset OutputFileName		;przepisujemy nazwe wyjscia
	CALL writeName
	
	inc bx
	
	mov di, offset Code					;przepisujemy kod
	CALL writeName
	
	pop bx
	pop di
	pop si

RET

reWriteName endp

writeName proc							;przepisuje z Klucza do InputFileName oraz OutputFileName i kod
	push ax
	push cx								;pushujemy cx bo uzywamy go w procedurze checkName
	
	CALL checkName						
	writeInLoop:
		mov al, byte ptr ds:[si]		
		mov byte ptr ds:[di], al
		inc di
		inc si
		dec cl							;w cl dlugosc argumentu
		cmp cl, 0
		JE checkNameEnd
	JMP writeInLoop
checkNameEnd:
	pop cx
	pop ax
RET

writeName endp

checkName proc				;pobiera kolejne dlugosci argumentow
	push di
	
	
	mov di, offset Leng
	
	cmp ds:[Version], 1
	JNE checkNameNorm
	inc di
	
checkNameNorm:
	mov cl, ds:[di+bx]
	;mov bx, cx
	;sub bx, 3						;ostatnie 3 znaki s¹ rozszerzeniem
	;cmp byte ptr ds:[di+bx], '.'
	;JNE errorKey
	
	
	pop di
	
RET
checkName endp

printLine: 
	push ax
	mov	ah, 9			
	int	21h
	pop	ax
RET



getHandler proc
	push ax
	push dx
	push cx
	
    mov al, 0						; tylko do odczytu
	mov	ah, 3dh				
	mov	dx, offset InputFileName
	int	21h				
	JC	InputErr
	mov word ptr ds:[InputHandler], ax 				;uchwyt do pliku wejsciowego
	

	mov	cx, 0					;bez atrybutow
	mov ah, 3ch
	mov	dx, offset OutputFileName
	int	21h
	JC	OutputErr
	
	mov word ptr ds:[OutputHandler], ax 			;uchwyt do pliku wyjsciowego	
	
	pop cx
	pop dx
	pop ax
RET

getHandler endp	


vigenereCipher proc

	push ax
	push bx
	push cx
	;xor bx, bx
	mov bl, byte ptr ds:[Version]			
	add bl, 2
	mov al, byte ptr ds:[Leng+bx]
	mov di, offset CodeSize				;pobieramy dlugosc kodu (klucza)
	mov byte ptr ds:[di], al

	
vigenereCipherLoop:
	CALL readToBuffer						;procedura wczytujaca do buffera z pliku
	mov	bx, word ptr ds:[InputBuffPointer]			;bx wskaznik pozycji w bufferze
	vigenereCipherSign:
	mov	al, byte ptr ds:[Buffer + bx]			;pobieramy z buffera do al
	CALL encryption							; dzialamy na al, procedura szyfrujace (deszyfrujaca)
	mov byte ptr ds:[CodeBuffer+bx], al		;przepisujemy do 2 buffera wynikowego
	inc bx								
	cmp	bx, ds:[BufferSize]		;czy w buforze jest jeszcze cos do wczytania?
	JB	vigenereCipherSign
	CALL writeFromBuffer		;procedura przepisujaca z CodeBuffer do pliku
	cmp	ds:[BufferSize], 10000	;(jeœli w buforze jest <10000 znaków, to ostatnie wczytanie wyczerpa³o plik)
	JE	vigenereCipherLoop	
	
	pop cx
	pop bx
	pop ax
RET
	
vigenereCipher endp

readToBuffer proc

	push ax
	push bx
	push cx
	push dx
	
	
	mov	bx, word ptr ds:[InputHandler]	;wskaŸnik pliku wejœciowego do bx
	mov	cx, 10000				;¿¹danie kolejnych 10kb z pliku
	mov	ah, 3Fh					;przerwanie wczytania z pliku
	mov	dx, offset Buffer			;dx - pocz¹tek miejsca dla wczytanych danych
	int 21h
	mov	word ptr ds:[BufferSize], ax	; aktualna liczba bajtów w buforze = ax = (z przerwania)liczba rzeczywiœcie wczytanych bajtów 
	mov	word ptr ds:[InputBuffPointer], 0;	;pozycja w buforze = 0
	
	pop dx
	pop cx
	pop	bx
	pop ax
	
	RET
	
readToBuffer endp


encryption proc
	
	push bx
	push cx
	
	xor	ah, ah						
	xor	dh, dh
	mov bl, byte ptr ds:[Counter]		;zapetlenie hasla
	cmp bl, byte ptr ds:[CodeSize]
	JNA ety								; jesli jest przeszlismy cale haslo zaczynamy od poczatku
	xor bl, bl
	mov byte ptr ds:[Counter],  bl			
ety:	
	xor bh, bh
	mov dl, ds:[Code+bx]				;w dl litera z klucza
	cmp ds:[Version], 1
	JE decryptMode
	
encryptMode:
	add al, dl							; dodajemy rejestry sa 8 bitowe wiec sam zmoduje
	JMP nat
decryptMode:
	sub al, dl							; odejmujemy rejestry sa 8 bitowe wiec sam zmoduje
nat:	
	inc byte ptr ds:[Counter]
	
	pop cx
	pop bx

	RET
	
encryption endp	
	
	
writeFromBuffer proc

	push	ax
	push	bx
	push	cx
	push	dx
	pushf
	
	
	mov	cx, word ptr ds:[BufferSize]			;aktualna pozycja w buforze wyjœcia do CX (tyle znaków wypisze przerwanie)
	mov	dx, offset CodeBuffer					;DS:DX - pocz¹tek buforu wyjœcia (wymaganie przerwania)
	mov	bx, ds:[OutputHandler]				;wskaŸnik do pliku wyjœciowego do BX (wymagania przerwania)
	mov	ah, 40h							;wypisanie bufora do pliku
	int	21h	
	mov	word ptr ds:[OutputBuffPointer], 0
	
	popf
	pop	dx
	pop	cx
	pop	bx
	pop	ax	
	
	RET
	
writeFromBuffer endp

printSign:
	push ax
	mov	ah, 2h			
	int	21h
	pop	ax

RET

exit proc

	push ax					;zamykamy pliki
	push bx
    mov	bx, word ptr ds:[InputHandler]
	mov	ah, 3eh
	int	21h		
	
	
	mov	bx, word ptr ds:[OutputHandler]
	mov	ah, 3eh
	int	21h	

pop bx
pop ax
	mov ax,04c00h			
	int 21h
exit endp
	
CODE1	ends 

stos1	segment STACK
	dw	1000 dup(?)			
top1	dw	?
stos1	ends


end start