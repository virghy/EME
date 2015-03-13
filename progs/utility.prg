#INCLUDE INCLUDE\TASTRADE.H
EXTERNAL ARRAY lmmatriz
EXTERNAL ARRAY tafiles
ENDPROC


FUNCTION IsTag
LPARAMETERS tctagname, tcalias
LOCAL llistag, lctagfound
IF PARAMETERS() < 2
     tcalias = ALIAS()
ENDIF
IF EMPTY(tcalias)
     RETURN .F.
ENDIF
llistag = .F.
tctagname = UPPER(ALLTRIM(tctagname))
lntagnum = 1
lctagfound = TAG(lntagnum,  ;
             tcalias)
DO WHILE  .NOT. EMPTY(lctagfound)
     IF UPPER(ALLTRIM(lctagfound)) ==  ;
        tctagname
          llistag = .T.
          EXIT
     ENDIF
     lntagnum = lntagnum + 1
     lctagfound = TAG(lntagnum,  ;
                  tcalias)
ENDDO
RETURN llistag
ENDFUNC
*
PROCEDURE NotYet
= MESSAGEBOX("En construcción",  ;
  64, "Futura Software")
RETURN
ENDPROC
*
FUNCTION FileSize
LPARAMETERS tcfilename
LOCAL lcsetcompatible, lnfilesize
lcsetcompatible = SET('COMPATIBLE')
SET COMPATIBLE ON
lnfilesize = FSIZE(tcfilename)
SET COMPATIBLE &lcSetCompatible
RETURN lnfilesize
ENDFUNC
*
FUNCTION FormIsObject
*SET COVERAGE TO futura.log ADDITIVE 

*RETURN (TYPE("_screen.activeform") ==  "O" .AND.  UPPER(_SCREEN.activeform.baseclass) =  "FORM")
RETURN (TYPE("_screen.activeform") ==  "O" .AND.  INLIST(UPPER(_SCREEN.activeform.class),'TSBASEFORM','TSMOVFORM','TSMAINTFORM'))
*SET COVERAGE TO 
ENDFUNC

*
FUNCTION ToolBarEnabled
PARAMETER oobject
LOCAL otoolobj
otoolobj = "oApp.oToolBar." +  oobject + ".enabled"
IF TYPE(otoolobj) <> "L"
     RETURN .F.
ELSE
     RETURN EVALUATE(otoolobj)
ENDIF
ENDFUNC
*
FUNCTION OnShutdown
LOCAL lcrespuesta
lcrespuesta = MESSAGEBOX( ;
              "Realmente desea salir del sistema ?",  ;
              0292,  ;
              "Futura Software")
IF lcrespuesta = 6
     CLEAR EVENTS
ELSE
     RETURN .F.
ENDIF
ENDFUNC
*
PROCEDURE MAINHWND
*
** ReFox - no body found for this  ;
   procedure **
*
ENDPROC
*
PROCEDURE _WHTOHWND
*
** ReFox - no body found for this  ;
   procedure **
*
ENDPROC
*
PROCEDURE _WONTOP
*
** ReFox - no body found for this  ;
   procedure **
*
ENDPROC
*
FUNCTION ValHora
LPARAMETERS lchora
RETURN (BETWEEN(VAL(SUBSTR(lchora,  ;
       1, 2)), 0, 23) .AND.  ;
       BETWEEN(VAL(SUBSTR(lchora,  ;
       4, 2)), 0, 59))
ENDFUNC
*
FUNCTION Descuento
PARAMETER gvalor, gdescuento
LOCAL lnvalor
IF gdescuento = 0
     lnvalor = gvalor
ELSE
     lnvalor = 0
     lnvalor = ROUND(gvalor -  ;
               (gvalor *  ;
               (gdescuento /  ;
               100)), 2)
ENDIF
RETURN lnvalor
ENDFUNC
*
FUNCTION Numeral
LPARAMETERS valornumero
LOCAL lcValor
IF valorNumero >999999999
	lcvalor = word(VAL(LEFT(STR(valornumero,12),3))*1000)+numeral1(valornumero-1000000000)
ELSE
	lcValor = word(valornumero)
ENDIF
&&VG 25/02/09
&&Corregimos en valor VEINTE Y UN MILLONES, ETC.
IF RIGHT(RTRIM(lcValor),2)='UN'
	lcValor=RTRIM(lcValor) + 'O'
ENDIF
RETURN lcValor 


ENDFUNC 

FUNCTION NUMERAL1
LPARAMETERS valornumero

DIMENSION unidad[10]
unidad[1] = " "
unidad[2] = "un"
unidad[3] = "dos"
unidad[4] = "tres"
unidad[5] = "cuatro"
unidad[6] = "cinco"
unidad[7] = "seis"
unidad[8] = "siete"
unidad[9] = "ocho"
unidad[10] = "nueve"
DIMENSION decena[10]
decena[1] = " "
decena[2] = "diez"
decena[3] = "veinte"
decena[4] = "treinta"
decena[5] = "cuarenta"
decena[6] = "cincuenta"
decena[7] = "sesenta"
decena[8] = "setenta"
decena[9] = "ochenta"
decena[10] = "noventa"
DIMENSION centena[10]
centena[1] = " "
centena[2] = "ciento"
centena[3] = "doscientos"
centena[4] = "trescientos"
centena[5] = "cuatrocientos"
centena[6] = "quinientos"
centena[7] = "seiscientos"
centena[8] = "setecientos"
centena[9] = "ochocientos"
centena[10] = "novecientos"
ndig = 0
aux = valornumero
DO WHILE aux >= 1
     aux = aux / 10
     ndig = ndig + 1
ENDDO
DIMENSION tab1[10]
tab1 = " "
var = valornumero
a = 1
DO WHILE var >= 1
     var = var / 10
     tab1[a] = STR((var -  ;
         INT(var)) * 10, 1, 0)
     var = INT(var)
     a = a + 1
ENDDO

DIMENSION letra[20]
letra = " "
pp = 1
DO WHILE pp <= ndig
     letra[pp] =  ;
          ALLTRIM(unidad(VAL(tab1(pp)) +  ;
          1)) + " "
     pp = pp + 1
     IF pp <= ndig
          letra[pp] =  ;
               ALLTRIM(decena(VAL(tab1(pp)) +  ;
               1)) + " "
     ENDIF
     pp = pp + 1
     IF pp <= ndig
          letra[pp] =  ;
               ALLTRIM(centena(VAL(tab1(pp)) +  ;
               1)) + " "
     ENDIF
     pp = pp + 1
ENDDO
DIMENSION otro[20]
otro = " "
FOR xx = 1 TO ndig
     otro[xx] = letra(xx)
ENDFOR
letra[1] = otro(1)
letra[2] = "Y "
letra[3] = otro(2)
letra[4] = otro(3)
letra[5] = " "
letra[6] = "MIL "
letra[7] = otro(4)
letra[8] = "Y "
letra[9] = otro(5)
letra[10] = otro(6)
letra[11] = " "
letra[12] = "MILLONES "
letra[13] = otro(7)
letra[14] = "Y "
letra[15] = otro(8)
letra[16] = otro(9)



IF tab1(2) = "1"
     IF tab1(1) = "1"
          otro[1] = "ONCE "
          otro[2] = " "
     ELSE
          IF tab1(1) = "2"
               otro[1] = "DOCE "
               otro[2] = " "
          ELSE
               IF tab1(1) = "3"
                    otro[1] =  "TRECE "
                    otro[2] = " "
               ELSE
                    IF tab1(1) =  "4"
                         otro[1] =  "CATORCE "
                         otro[2] =  " "
                    ELSE
                         IF tab1(1) =  "5"
                              otro[1] =  "QUINCE "
                              otro[2] =  " "
                         ENDIF
                    ENDIF
               ENDIF
          ENDIF
     ENDIF
ENDIF

IF tab1(5) = "1"
     IF tab1(4) = "1"
          otro[4] = "ONCE "
          otro[5] = " "
     ELSE
          IF tab1(4) = "2"
               otro[4] = "DOCE "
               otro[5] = " "
          ELSE
               IF tab1(4) = "3"
                    otro[4] = "TRECE "
                    otro[5] = " "
               ELSE
                    IF tab1(4) = "4"
                         otro[4] =  "CATORCE "
                         otro[5] = " "
                    ELSE
                         IF tab1(4) =  "5"
                              otro[4] =  "QUINCE "
                              otro[5] =  " "
                         ENDIF
                    ENDIF
               ENDIF
          ENDIF
     ENDIF
ENDIF
IF tab1(8) = "1"
     IF tab1(7) = "1"
          otro[7] = "ONCE "
          otro[8] = " "
     ELSE
          IF tab1(7) = "2"
               otro[7] = "DOCE "
               otro[8] = " "
          ELSE
               IF tab1(7) = "3"
                    otro[7] =  ;
                        "TRECE "
                    otro[8] = " "
               ELSE
                    IF tab1(7) =  ;
                       "4"
                         otro[7] =  ;
                             "CATORCE "
                         otro[8] =  ;
                             " "
                    ELSE
                         IF tab1(7) =  ;
                            "5"
                              otro[7] =  ;
                               "QUINCE "
                              otro[8] =  ;
                               " "
                         ENDIF
                    ENDIF
               ENDIF
          ENDIF
     ENDIF
ENDIF
SET STEP ON
IF otro(1) = " " .OR. otro(2) =  " "
     letra[2] = " "
ENDIF
IF otro(4) = " " .OR. otro(5) =  " "
     letra[8] = " "
ENDIF
IF otro(7) = " " .OR. otro(8) =  " "
     letra[14] = " "
ENDIF
IF otro(4) = " " .AND. otro(5) =  " " .AND. otro(6) = " "
     letra[6] = " "
ENDIF
IF otro(1) + otro(2) + otro(3) =  " "
     letra[5] = " "
ENDIF
IF otro(4) + otro(5) + otro(6) =  " "
     letra[5] = " "
ENDIF
IF otro(4) + otro(5) + otro(6) =  " "
     letra[11] = " "
ENDIF
IF otro(7) + otro(8) + otro(9) =  " "
     letra[11] = " "
ENDIF
IF otro(7) = " " .AND. otro(8) =  " " .AND. otro(9) = " "
     letra[12] = " "
ENDIF
IF tab1(3) = "1" .AND. otro(1) +  otro(2) = " "
     otro[3] = "CIEN "
ENDIF
IF tab1(6) = "1" .AND. otro(5) +  otro(4) = " "
     otro[6] = "CIEN "
ENDIF
IF tab1(9) = "1" .AND. otro(8) +  otro(7) = " "
     otro[9] = "CIEN "
ENDIF
IF tab1(4) = "1" .AND. otro(5) +  otro(6) + otro(7) = " "
     otro[4] = " "
ENDIF
IF tab1(7) = "1" .AND.  EMPTY(tab1(8) + tab1(9) +  tab1(10))
     otro[7] = "UN "
     letra[12] = "MILLON "
ENDIF

SET STEP ON


letra[1] = otro(1)
letra[3] = otro(2)
letra[4] = otro(3)
letra[7] = otro(4)
letra[9] = otro(5)
letra[10] = otro(6)
letra[13] = otro(7)
letra[15] = otro(8)
letra[16] = otro(9)
letra[18] = otro(10)

aaa = SPACE(80)
zzz = LTRIM(letra(16)) +  ;
      LTRIM(letra(15)) +  ;
      LTRIM(letra(14)) +  ;
      LTRIM(letra(13)) +  ;
      LTRIM(letra(12)) +  ;
      LTRIM(letra(11)) +  ;
      LTRIM(letra(10)) +  ;
      LTRIM(letra(9)) +  ;
      LTRIM(letra(8)) +  ;
      LTRIM(letra(7)) +  ;
      LTRIM(letra(6)) +  ;
      LTRIM(letra(5)) +  ;
      LTRIM(letra(4)) +  ;
      LTRIM(letra(3)) +  ;
      LTRIM(letra(2)) +  ;
      LTRIM(letra(1))
aaa = zzz
RETURN UPPER(aaa)
ENDFUNC
*

FUNCTION WORD()
 LPARAMETERS _NUM
 IF TYPE('_NUM')#'N'
*  RETURN 'Solo traduce Cadenas Numericas'
 ENDIF
 STORE ' ' TO _LMILLON,_LMILES,_LET
 _L1='UN DOS TRES CUATRO CINCO SEIS SIETE OCHO NUEVE DIEZ ONCE DOCE TRECE CATORCE QUINCE'
 _G1=' 1  2  3 4  5  6  7  8  9  10 11 12 13 14 15'
 _X1=' 1  4  8 13 20 26 31 37 42 48 53 58 63 69 77'
 _Y1=' 2  3  4 6  5  4  5  4  5  4  4  4  5  7  6'
 _L2='VEINTE TREINTA CUARENTA CINCUENTA SESENTA SETENTA OCHENTA NOVENTA'
 _G2='20 30 40 50 60 70 80 90'
 _X2=' 1  8 16 25 35 43 51 59'
 _Y2=' 6  7 8  9  7  7  7  7'
 _L3='CIENTO DOSC TRESC CUATROC QUIN SEISC SETEC OCHOC NOVEC'
 _G3=' 1  2 3  4  5  6  7  8  9'
 _X3=' 1  8 13 19 27 32 38 44 50'
 _Y3=' 6  4 5  7  4  5  5  5  5'
 IF _NUM=0
  RETURN 'CERO'
 ENDIF
 IF _NUM>999999999
  RETURN 'Solo traduce Numeros inferiores a Mil Millones......'
 ENDIF
 _NOM=_NUM
 _NAM=INT(_NUM)
 _MILLON=VAL(SUBSTR(STR(_NAM,9),1,3))
 _MILES=VAL(SUBSTR(STR(_NAM,9),4,3))
 _CIENTOS=VAL(SUBSTR(STR(_NAM,9),7,3))
 IF _CIENTOS>0
  _X=1
 ENDIF
 IF _MILES>0
  _X=2
 ENDIF
 IF _MILLON>0
  _X=3
 ENDIF
 DO WHILE _X>0
  IF _X=3
   _NUM=_MILLON
  ELSE
   IF _X=2
    _NUM=_MILES
   ELSE
    IF _X=1
     _NUM=_CIENTOS
    ENDIF
   ENDIF
  ENDIF
  _NUM1=_NUM
  _CAD=STR(_NUM,3)
  _C1=SUBSTR(_CAD,2,2)
  IF _NUM>0
   IF _NUM>100
    _NUM2=VAL(_C1)
    _NUM=_NUM2
    _C3=SUBSTR(_CAD,1,1)
    _N3=AT(_C3,_G3)
    _INI3=VAL(SUBSTR(_X3,_N3,2))
    _LON3=VAL(SUBSTR(_Y3,_N3,2))
    _LET3=SUBSTR(_L3,_INI3,_LON3)+IIF(_C3='1',' ','IENTOS ')
   ENDIF
   IF _NUM<10
    _CAD=STR(_NUM,1)
    _C1=SUBSTR(_CAD,1,1)
   ELSE
    IF _NUM<100
     _CAD=STR(_NUM,2)
    ENDIF
   ENDIF
   IF _NUM<16
    _N1=AT(_C1,_G1)
    _INI=VAL(SUBSTR(_X1,_N1,2))
    _LON=VAL(SUBSTR(_Y1,_N1,2))
    _LET=SUBSTR(_L1,_INI,_LON)
   ELSE
    IF _NUM<20
     _C1=SUBSTR(_CAD,2,1)
     _N1=AT(_C1,_G1)
     _INI=VAL(SUBSTR(_X1,_N1,2))
     _LON=VAL(SUBSTR(_Y1,_N1,2))
     _LET='DIECI'+SUBSTR(_L1,_INI,_LON)
    ELSE
     _C2=VAL(SUBSTR(_CAD,2,1))
     _NUM=_NUM-_C2
     _C1=STR(_NUM,2)
     _N1=AT(_C1,_G2)
     _INI=VAL(SUBSTR(_X2,_N1,2))
     _LON=VAL(SUBSTR(_Y2,_N1,2))
     _LET=SUBSTR(_L2,_INI,_LON)
     IF SUBSTR(_CAD,2,1)#'0'
      _C2=SUBSTR(_CAD,2,1)
      _N2=AT(_C2,_G1)
      _INI2=VAL(SUBSTR(_X1,_N2,2))
      _LON2=VAL(SUBSTR(_Y1,_N2,2))
      _LET=_LET+' Y '+SUBSTR(_L1,_INI2,_LON2)
     ENDIF
    ENDIF
   ENDIF
   IF _NUM1>99
    IF _NUM1=100
     _LET='CIEN '
    ELSE
     _LET=_LET3+_LET
    ENDIF
   ENDIF
   IF _X=3
    _LMILLON=_LET+' MILLONES'
    IF SUBSTR(_LET,1,2)='UN'
     _LMILLON=_LET+' MILLON'
    ENDIF
   ENDIF
   IF _X=2
    _LMILES=_LET+' MIL'
    IF SUBSTR(_LET,1,2)='UN'
     _LMILES='MIL'
    ENDIF
   ENDIF
  ENDIF
  _X=_X-1
 ENDDO
 IF _MILLON>0
  IF _MILES=0
   IF _cientos=0
    _BB=_LMILLON
   ELSE
    _BB=_LMILLON+' '+_LET
   ENDIF
  ELSE
   IF _cientos=0
    _BB=_lmillon+' '+_lmiles
   ELSE
    _BB=_LMILLON+' '+_LMILES+' '+_LET
   ENDIF
  ENDIF
 ELSE
  IF _MILES>0
   IF _cientos=0
    _BB=_lmiles
   ELSE
    _BB=_LMILES+' '+_LET
   ENDIF
  ELSE
   _BB=_LET
  ENDIF
 ENDIF
 *_BB=_BB+' CON '+SUBSTR(STR(_NOM,13,2),12,2)+'/100'
 RETURN _BB
ENDFUNC

PROCEDURE LogCambio
lctabla = ALIAS()
lnrecno = RECNO()
IF TYPE('OAPP') = 'O'
     lcusuario = oapp.getemployeeid()
ELSE
     lcusuario = "Desconocido"
ENDIF
ctabla = ALIAS()
nrocampos = FCOUNT(ctabla)
DIMENSION campos[nrocampos, 3]
campos = ''
FOR nc = 1 TO nrocampos
     campos[nc, 1] = FIELD(nc)
     IF GETFLDSTATE(campos(nc,1),  ;
        ctabla) = 2
          campos[nc,2]=&campos[nc,1]
          campos[nc, 3] = 'OK'
     ENDIF
ENDFOR
FOR nc = 1 TO nrocampos
     IF campos(nc,3) = 'OK'
          DO CASE
               CASE TYPE( ;
                    'campos[nc,2]' ;
                    ) = 'N'
                    campos[nc, 2] =  ;
                          ALLTRIM(STR(campos(nc, ;
                          2)))
               CASE TYPE( ;
                    'campos[nc,2]' ;
                    ) = 'D'
                    campos[nc, 2] =  ;
                          DTOC(campos(nc, ;
                          2))
               CASE TYPE( ;
                    'campos[nc,2]' ;
                    ) = 'T'
                    campos[nc, 2] =  ;
                          DTOC(campos(nc, ;
                          2))
               CASE TYPE( ;
                    'campos[nc,2]' ;
                    ) = 'L'
                    campos[nc, 2] =  ;
                          IIF(campos(nc, ;
                          2),  ;
                          'Verdadero',  ;
                          'Falso')
          ENDCASE
          STRTOFILE(campos(nc,1) +  ;
                   ' = ' +  ;
                   ALLTRIM(campos(nc, ;
                   2)) + CHR(13),  ;
                   'auditoria.log',  ;
                   .T.)
     ENDIF
ENDFOR
INSERT INTO Datos!AuditoriaLog  ;
       (operacion, usuario, tabla,  ;
       registro) VALUES  ;
       ("Edición", lcusuario,  ;
       lctabla, lnrecno)
SELECT auditorialog
APPEND MEMO detalle FROM  ;
       Auditoria.LOG
DELETE FILE Auditoria.LOG
USE IN auditorialog
ENDPROC
*
PROCEDURE LogBorrar
lctabla = ALIAS()
lnrecno = RECNO()
IF TYPE('OAPP') = 'O'
     lcusuario = oapp.getemployeeid()
ELSE
     lcusuario = "Desconocido"
ENDIF
INSERT INTO Datos!AuditoriaLog  ;
       (operacion, usuario, tabla,  ;
       registro) VALUES  ;
       ("Eliminación", lcusuario,  ;
       lctabla, lnrecno)
USE IN auditorialog
ENDPROC
*
PROCEDURE LogInsertar
lnrecno = RECCOUNT() + 1
IF TYPE('OAPP') = 'O'
     lcusuario = oapp.getemployeeid()
ELSE
     lcusuario = "Desconocido"
ENDIF
ctabla = ALIAS()
nrocampos = FCOUNT(ctabla)
DIMENSION campos[nrocampos, 3]
campos = ''
FOR nc = 1 TO nrocampos
     campos[nc, 1] = FIELD(nc)
     IF GETFLDSTATE(campos(nc,1),  ;
        ctabla) = 3 .OR.  ;
        GETFLDSTATE(campos(nc,1),  ;
        ctabla) = 4
          campos[nc,2]=&campos[nc,1]
          IF GETFLDSTATE(campos(nc, ;
             1), ctabla) = 4
               campos[nc, 3] =  ;
                     'OK'
          ENDIF
     ENDIF
ENDFOR
FOR nc = 1 TO nrocampos
     IF campos(nc,3) = 'OK'
          DO CASE
               CASE TYPE( ;
                    'campos[nc,2]' ;
                    ) = 'N'
                    campos[nc, 2] =  ;
                          ALLTRIM(STR(campos(nc, ;
                          2)))
               CASE TYPE( ;
                    'campos[nc,2]' ;
                    ) = 'D'
                    campos[nc, 2] =  ;
                          DTOC(campos(nc, ;
                          2))
               CASE TYPE( ;
                    'campos[nc,2]' ;
                    ) = 'T'
                    campos[nc, 2] =  ;
                          DTOC(campos(nc, ;
                          2))
               CASE TYPE( ;
                    'campos[nc,2]' ;
                    ) = 'L'
                    campos[nc, 2] =  ;
                          IIF(campos(nc, ;
                          2),  ;
                          'Verdadero',  ;
                          'Falso')
          ENDCASE
          STRTOFILE(campos(nc,1) +  ;
                   ' = ' +  ;
                   ALLTRIM(campos(nc, ;
                   2)) + CHR(13),  ;
                   'auditoria.log',  ;
                   .T.)
     ENDIF
ENDFOR
INSERT INTO Datos!AuditoriaLog  ;
       (operacion, usuario, tabla,  ;
       registro) VALUES  ;
       ("Agregar", lcusuario,  ;
       ctabla, lnrecno)
SELECT auditorialog
APPEND MEMO detalle FROM  ;
       Auditoria.LOG
DELETE FILE Auditoria.LOG
USE IN auditorialog
ENDPROC
*
FUNCTION VERIFICAR
RETURN .F.
ENDFUNC
*
FUNCTION LeerIni
LPARAMETERS tcentry, tcgrupo
LOCAL lcbuffer, lcolderror, lntop,  ;
      lnleft, llerror, lncommapos,  ;
      lcentry
IF PCOUNT() = 0
     lcentry = thisform.caption
ELSE
     lcentry = tcentry
ENDIF
lcbuffer = SPACE(200) + CHR(0)
lcolderror = ON('ERROR')
IF getprivstr(tcgrupo,lcentry,"", ;
   @lcbuffer,LEN(lcbuffer), ;
   CURDIR() + "FUTURA.INI") > 0
     ON ERROR LLERROR = .T.
     ON ERROR &lcOldError
     IF  .NOT. llerror
          lcbuffer = ALLTRIM(STRTRAN(lcbuffer,  ;
                     CHR(0),  ;
                     ' '))
          RETURN lcbuffer
     ENDIF
ENDIF
RETURN ''
ENDFUNC
*
PROCEDURE EscribirIni
LPARAMETERS tcvalor, tcentry,  ;
            tcgrupo
LOCAL lcvalue, lcentry
IF PCOUNT() = 0
     lcentry = thisform.caption
ELSE
     lcentry = tcentry
ENDIF
lcvalue = tcvalor
= writeprivstr(tcgrupo,lcentry, ;
  lcvalue,CURDIR() +  ;
  "FUTURA.INI")
ENDPROC
*
PROCEDURE Generar_turno
LPARAMETERS lmmatriz, lcestado,  ;
            lndiasestado,  ;
            lndiastrabajados,  ;
            lndiaslibres,  ;
            lcturnoanterior,  ;
            lnrotativo,  ;
            lcturno_a_rotar,  ;
            lndias_a_generar
LOCAL lncontinua, lnposicion,  ;
      lcturno, x
lnposicion = 1
IF lcestado = 'F'
     lncontinua = lndiaslibres -  ;
                  lndiasestado
ELSE
     lncontinua = lndiastrabajados -  ;
                  lndiasestado
ENDIF
IF lncontinua <> 0
     IF lcestado <> 'F'
          lcturno = lcturnoanterior
     ELSE
          lcturno = 'F'
     ENDIF
     FOR x = 1 TO lncontinua
          lmmatriz(lnposicion) =  ;
                  lcturno
          lnposicion = lnposicion +  ;
                       1
     ENDFOR
ENDIF
IF lcestado <> 'F'
     FOR x = 1 TO lndiaslibres
          lmmatriz(lnposicion) =  ;
                  'F'
          lnposicion = lnposicion +  ;
                       1
     ENDFOR
ENDIF
FOR x = 1 TO lndias_a_generar
     IF lnposicion >  ;
        lndias_a_generar
          EXIT
     ENDIF
     IF lnrotativo <> 0
          lcturno = ALLTRIM(STR(lnrotativo))
     ELSE
          lcturno = SUBSTR(lcturno_a_rotar,  ;
                    AT(lcturnoanterior,  ;
                    lcturno_a_rotar) +  ;
                    1, 1)
          IF EMPTY(lcturno)
               lcturno = SUBSTR(lcturno_a_rotar,  ;
                         1, 1)
          ENDIF
          lcturnoanterior = lcturno
     ENDIF
     FOR x = 1 TO  ;
         lndiastrabajados
          IF lnposicion >  ;
             lndias_a_generar
               EXIT
          ENDIF
          lmmatriz(lnposicion) =  ;
                  lcturno
          lnposicion = lnposicion +  ;
                       1
     ENDFOR
     FOR x = 1 TO lndiaslibres
          IF lnposicion >  ;
             lndias_a_generar
               EXIT
          ENDIF
          lmmatriz(lnposicion) =  ;
                  'F'
          lnposicion = lnposicion +  ;
                       1
     ENDFOR
ENDFOR
RETURN
ENDPROC
*
PROCEDURE seteo
SET TALK OFF
SET CONSOLE OFF
SET SYSFORMATS ON
SET CENTURY ON
SET DATE short
SET POINT TO ','
SET SEPARATOR TO '.'
SET DATABASE TO (oapp.cdatabase)
SET DELETED ON
SET NULLDISPLAY TO ' '
SET MULTILOCKS ON
SET SAFETY OFF 
_REPORTOUTPUT = "ReportOutput.app"
RETURN
ENDPROC
*
FUNCTION SetInforme
LPARAMETERS informe
LOCAL lcorientacion, lcmargen, lcmargentop, obj

lcmargen = leerini('Margen', 'System')
lcmargentop = leerini('MargenTOP', 'System')

IF EMPTY(lcmargen) .OR.  EMPTY(lcmargentop)
     obj = CREATEOBJECT('OpcionesImpresion')
     obj.show()
     lcmargen = leerini('Margen', 'System')
ENDIF

SELECT * FROM (informe) INTO  TABLE lcRepofile.frx
lcorientacion = MLINE(expr,  ATCLINE('ORIENTATION',  expr))

IF RIGHT(lcorientacion, 1) = '0'
     lcpapel = MLINE(expr,  ATCLINE('papersize',  expr))
     lcnuevopapel = 'PAPERSIZE=' +  ALLTRIM(STR(PRTINFO(2)))
     REPLACE expr WITH  STRTRAN(expr,  lcpapel,  lcnuevopapel) IN  lcrepofile
ENDIF

IF  .NOT. EMPTY(lcmargen) .AND.  RIGHT(lcorientacion, 1) =  '1'
     REPLACE hpos WITH  ROUND(VAL(lcmargen) *  393.7 , 0) IN  lcrepofile
ENDIF

IF  .NOT. EMPTY(lcmargentop)
     REPLACE vpos WITH vpos +  ROUND(VAL(lcmargentop) *  393.7 , 0) ALL FOR  objtype = 8 IN  lcrepofile
ENDIF

*	USE IN lcrepofile
	USE IN (informe)
*	RETURN 'lcRepofile.frx'
	
	use in lcRepofile
	informe= 'lcRepofile.frx'
	return informe
		
ENDFUNC

*
FUNCTION nulltostr
LPARAMETERS lcvalor
RETURN IIF(ISNULL(lcvalor), '',  ;
       lcvalor)
ENDFUNC
*
FUNCTION SQL2
LPARAMETERS cmdsql, lccursor,  ;
            lnconn
LOCAL _hndconn, _lnresultado,  ;
      _lcalias
_lcalias = ALIAS()
IF TYPE('lnConn') = 'N'
     _hndconn = lnconn
ELSE
     lnconn = 0

    
	     IF TYPE('oApp') <> 'O'
	          sistema = 'ND'
	          _hndconn = SQLCONNECT('datos_Sql')
	     ELSE
		     IF TYPE('oApp.hndConn') <> 'N'	     
		        sistema = oapp.sistema
				oApp.hndConn = SQLCONNECT(oApp.Sistema,.T.)
		     	= SQLSETPROP(oApp.hndConn, 'BatchMode', .T.)
				= SQLSETPROP(oApp.hndConn, "Transactions", 1)
		     ENDIF			
			_hndconn=oApp.hndConn
	     ENDIF
		

     
              	
     
     
ENDIF
IF _hndconn <= 0
     MESSAGEBOX( ;
               'No se pudo conectar al Servidor' +  ;
               CHR(13) +  ;
               'Sistema: ' +  ;
               sistema + CHR(13) +  ;
               'BD: ' + DBC())
     RETURN -1
ENDIF
DO WHILE SQLGETPROP(_hndconn,  ;
   'ConnectBusy')
     WAIT WINDOW NOCLEAR NOWAIT  ;
          'Conexion Ocupada...'
ENDDO

IF TYPE('lcCursor') = 'C'
     _lnresultado = SQLEXEC(_hndconn,  ;
                    cmdsql,  ;
                    lccursor)
ELSE
     _lnresultado = SQLEXEC(_hndconn,  ;
                    cmdsql)
ENDIF
IF _lnresultado < 0
     lnelemento = AERROR(laerror)
     lcerror = ''
     FOR i = 1 TO lnelemento
          lcerror = lcerror +  ;
                    TRANSFORM(i) +  ;
                    ') ' +  ;
                    laerror(i,3) +  ;
                    CHR(13)
     ENDFOR
     MESSAGEBOX(lcerror)
ENDIF
IF lnconn = 0
     SQLCANCEL(_hndconn)
*     SQLDISCONNECT(_hndconn)
ENDIF
IF  .NOT. EMPTY(_lcalias)
     SELECT (_lcalias)
ENDIF
RETURN _lnresultado
ENDFUNC

FUNCTION SQL
LPARAMETERS lcmdsql, lccursor,  ;
            lnconn
LOCAL _hndconn, _lnresultado,  ;
      _lcalias
_lcalias = ALIAS()
IF TYPE('lnConn') = 'N'
     _hndconn = lnconn
ELSE
     lnconn = 0
     IF TYPE('oApp') <> 'O'
          sistema = 'ND'
          _hndconn = SQLCONNECT('datos_Sql')
     ELSE
          sistema = oapp.sistema
          _hndconn = SQLCONNECT(oapp.sistema)
     ENDIF
     = SQLSETPROP(_hndconn, "Transactions", 1)
     
ENDIF
IF _hndconn <= 0
     MESSAGEBOX( ;
               'No se pudo conectar al Servidor' +  ;
               CHR(13) +  ;
               'Sistema: ' +  ;
               sistema + CHR(13) +  ;
               'BD: ' + DBC())
     RETURN -1
ENDIF
*!*	DO WHILE SQLGETPROP(_hndconn,'ConnectBusy')
*!*	     WAIT WINDOW NOCLEAR NOWAIT  'Conexion Ocupada...'
*!*	ENDDO
= SQLSETPROP(_hndconn, 'BatchMode', .T.)
*= SQLSETPROP(_hndconn, "Transactions", 1)
IF TYPE('lcCursor') = 'C'
     _lnresultado = SQLEXEC(_hndconn,  ;
                    lcmdsql,  ;
                    lccursor)
ELSE
     _lnresultado = SQLEXEC(_hndconn,  ;
                    lcmdsql)
ENDIF
IF _lnresultado < 0
     lnelemento = AERROR(laerror)
     lcerror = ''
     FOR i = 1 TO lnelemento
          lcerror = lcerror +  ;
                    TRANSFORM(i) +  ;
                    ') ' +  ;
                    laerror(i,3) +  ;
                    CHR(13)
     ENDFOR
     =RegistrarError(lcError)
       MESSAGEBOX(lcerror,16,TASTRADE_LOC)
ENDIF

IF lnconn = 0
     SQLCANCEL(_hndconn)
     SQLDISCONNECT(_hndconn)
ENDIF
IF  .NOT. EMPTY(_lcalias)
     SELECT (_lcalias)
ENDIF
RETURN _lnresultado
ENDFUNC

*
FUNCTION SQLDB2
LPARAMETERS cmdsql, lccursor,  ;
            lnconn
LOCAL _hndconn, _lnresultado,  ;
      _lcalias
_lcalias = ALIAS()
IF TYPE('lnConn') = 'N'
     _hndconn = lnconn
ELSE
     lnconn = 0
     WAIT WINDOW NOCLEAR NOWAIT  ;
          'Conectando...'
     IF TYPE('oApp') <> 'O'
          sistema = 'ND'
          _hndconn = SQLCONNECT('CED')
     ELSE
          sistema = oapp.sistema
          _hndconn = SQLCONNECT("CED")
     ENDIF
ENDIF
IF _hndconn <= 0
     MESSAGEBOX( ;
               'No se pudo conectar al Servidor' +  ;
               CHR(13) +  ;
               'Sistema: ' +  ;
               sistema + CHR(13) +  ;
               'BD: ' + DBC())
     RETURN -1
ENDIF
DO WHILE SQLGETPROP(_hndconn,  ;
   'ConnectBusy')
     WAIT WINDOW NOCLEAR NOWAIT  ;
          'Conexion Ocupada...'
ENDDO
WAIT WINDOW NOCLEAR NOWAIT  ;
     'Procesando...'
= SQLSETPROP(_hndconn,  ;
  'BatchMode', .T.)
= SQLSETPROP(_hndconn,  ;
  "Transactions", 1)
IF TYPE('lcCursor') = 'C'
     _lnresultado = SQLEXEC(_hndconn,  ;
                    cmdsql,  ;
                    lccursor)
ELSE
     _lnresultado = SQLEXEC(_hndconn,  ;
                    cmdsql)
ENDIF
WAIT CLEAR
IF _lnresultado < 0
     lnelemento = AERROR(laerror)
     lcerror = ''
     FOR i = 1 TO lnelemento
          lcerror = lcerror +  ;
                    TRANSFORM(i) +  ;
                    ') ' +  ;
                    laerror(i,3) +  ;
                    CHR(13)
     ENDFOR
     MESSAGEBOX(lcerror)
ENDIF
IF lnconn = 0
     SQLCANCEL(_hndconn)
     SQLDISCONNECT(_hndconn)
ENDIF
IF  .NOT. EMPTY(_lcalias)
     SELECT (_lcalias)
ENDIF
RETURN _lnresultado
ENDFUNC
*
FUNCTION Encriptar
LPARAMETERS lccadena
LOCAL lnlongitud, i, lcencriptado,  ;
      lccaracter
lnlongitud = LEN(ALLTRIM(lccadena))
lcencriptado = ''
FOR i = lnlongitud TO 1 STEP -1
     lccaracter = ASC(SUBSTR(lccadena,  ;
                  i, 1)) - (i *  ;
                  2)
     lcencriptado = lcencriptado +  ;
                    CHR(lccaracter)
ENDFOR
RETURN lcencriptado
ENDFUNC
*
FUNCTION CambiarHoja
LPARAMETERS lcselerepo
LOCAL lcpapel, lcnuevopapel
IF  .NOT. FILE(lcselerepo)
     RETURN lcselerepo
ENDIF
SELECT * FROM (lcselerepo) INTO  ;
         TABLE lcRepofile.frx
lcpapel = MLINE(expr,  ;
          ATCLINE('papersize',  ;
          expr))
lcnuevopapel = 'PAPERSIZE=' +  ;
               ALLTRIM(STR(PRTINFO(2)))
REPLACE expr WITH STRTRAN(expr,  ;
        lcpapel, lcnuevopapel)
USE IN lcrepofile
RETURN 'lcRepofile.frx'
ENDFUNC
*

FUNCTION LetraMes
LPARAMETERS lnMes
LOCAL lcMes as String
DO CASE 
	CASE lnMes = 1
		lcMes = "Enero"
	CASE lnMes = 2
		lcMes = "Febrero"
	CASE lnMes = 3
		lcMes = "Marzo"
	CASE lnMes = 4
		lcMes = "Abril"
	CASE lnMes = 5
		lcMes = "Mayo"
	CASE lnMes = 6
		lcMes = "Junio"
	CASE lnMes = 7
		lcMes = "Julio"
	CASE lnMes = 8
		lcMes = "Agosto"
	CASE lnMes = 9
		lcMes = "Setiembre"
	CASE lnMes = 10
		lcMes = "Octubre"
	CASE lnMes = 11
		lcMes = "Noviembre"
	CASE lnMes = 12
		lcMes = "Diciembre"
ENDCASE 
RETURN lcMEs
ENDFUNC 
		



FUNCTION LeerParam
LPARAMETERS lcdevuelve, lctabla,  ;
            lccondicion, lcodbc
LOCAL cadenasql, lcalias,  ;
      lxresultado
lcalias = ALIAS()
cadenasql = 'Select ' +  ;
            lcdevuelve +  ;
            ' as Devuelve From ' +  ;
            lctabla + ' where ' +  ;
            lccondicion
IF PCOUNT() < 4
     lcodbc = 'datos_sql'
ENDIF
IF sql(cadenasql,'xxParam') > 0
     SELECT (lcalias)
     lcresultado = xxparam.devuelve
     USE IN xxparam
     RETURN lcresultado
ENDIF
ENDFUNC
*
FUNCTION Cotizacion
PARAMETER lcmoneda, lctipo,  ;
          ldfecha
LOCAL lncotizacion, lcalias AS  ;
      CHARACTER
lcalias = ALIAS()
IF PCOUNT() < 3
     ldfecha = DATE()
ENDIF
TEXT TO cmdsql NOSHOW

	SELECT TOP 1 compra, venta,  fecha 
	FROM  bs_cotizacion 
	WHERE  bs_cotizacion.idmoneda =  ?lcmoneda AND fecha <=  ?ldfecha 
	ORDER BY fecha  DESC 

ENDTEXT
sql(cmdsql,'xxCotiza')
IF RECCOUNT('xxCotiza') > 0
     lncotizacion = IIF(lctipo =  ;
                    'V',  ;
                    xxcotiza.venta,  ;
                    xxcotiza.compra)
ELSE
     lncotizacion = 1
ENDIF
USE IN xxcotiza
IF  .NOT. EMPTY(lcalias)
     SELECT (lcalias)
ENDIF
RETURN lncotizacion
ENDFUNC
*
FUNCTION ret_tip_recibo
LPARAMETERS lccondicion
LOCAL vtip_reci, lcalias
strsql = "select tip_reci from vt_tipo_doc " +  ;
         "where idcondicion = '" +  ;
         ALLTRIM(lccondicion) +  ;
         "'"
lcalias = ALIAS()
IF sql(strsql,'cu_condi') > 0
     IF  .NOT. EOF('cu_condi')
          vtip_reci = cu_condi.tip_reci
     ELSE
          vtip_reci = ''
     ENDIF
     IF USED('cu_condi')
          USE IN 'cu_condi'
     ENDIF
ELSE
     vtip_reci = ''
ENDIF
SELECT (lcalias)
RETURN vtip_reci
ENDFUNC
*
PROCEDURE EXPORTAR
gcdelimname = ALIAS() + '.xls'
lcdefa = SYS(5) + SYS(2003)
gcdelimfile = PUTFILE('Tabla:',  ;
              gcdelimname,  ;
              'xls')
IF  .NOT. EMPTY(gcdelimfile)
     COPY TO (gcdelimfile) TYPE  ;
          XL5
     MESSAGEBOX( ;
               'El archivo ha sido copiado satisfactoriamente a ' +  ;
               gcdelimfile, 064,  ;
               tastrade_loc)
ENDIF
ENDPROC
*
FUNCTION SendViaMAPI
LPARAMETERS tcfrom, tcto,  ;
            tcsubject, tcbody,  ;
            tafiles
ON ERROR RETURN(.F.)
LOCAL losession, lomessages
losession = CREATEOBJECT("MSMAPI.MAPISession")
losession.signon()
IF (losession.sessionid > 0)
     lomessages = CREATEOBJECT("MSMAPI.MAPIMessages")
     lomessages.sessionid = losession.sessionid
ENDIF
WITH lomessages
     .compose()
     .recipdisplayname = tcto
     .reciptype = 1
     .resolvename()
     .msgsubject = tcsubject
     .msgnotetext = tcbody
     IF PCOUNT() > 4
          FOR lncountattachments =  ;
              1 TO ALEN(tafiles)
               .attachmentindex =  ;
                .attachmentcount
               .attachmentname = JUSTFNAME(tafiles(lncountattachments))
               .attachmentposition =  ;
                .attachmentindex
               .attachmentpathname =  ;
                tafiles(lncountattachments)
          ENDFOR
     ENDIF
     .send(.F.)
ENDWITH
losession.signoff()
STORE .NULL. TO losession,  ;
      lomessages
RELEASE losession, lomessages
RETURN .T.
ENDFUNC
*
DEFINE CLASS txtnumeric AS  ;
       textbox
alignment = 3
value = 0
width = 100
commas = .T.
decimals = 3
name = "txtnumeric"
currency = .F.
PROTECTED coldinputmask
PROTECTED coriginputmask
PROTECTED cvalue
PROTECTED oldcontrolsource
PROTECTED oldtype
*
PROTECTED FUNCTION setinputmask
     LOCAL lcmask, liintcount,  ;
           lnvalue, lix
     WITH this
          IF  .NOT.  ;
              EMPTY(.coriginputmask)
               .inputmask = .coldinputmask
               RETURN .T.
          ENDIF
          IF  .NOT. (.commas .OR.  ;
              .currency .OR.  ;
              (.decimals > 0))
               RETURN .T.
          ENDIF
          DO CASE
               CASE VARTYPE(.value) =  ;
                    'N'
                    lnvalue = .value
               CASE VARTYPE(.value) =  ;
                    'C'
                    lnvalue = VAL(.value)
               OTHERWISE
                    = MESSAGEBOX( ;
                      "Unhandled Value Type",  ;
                      0,  ;
                      "Error")
                    .enabled = .F.
                    RETURN .F.
          ENDCASE
          lcmask = ""
          liintcount = LEN(ALLTRIM(STR(lnvalue,  ;
                       20)))
          IF INT(lnvalue) = 0
               liintcount = 0
          ENDIF
          FOR lix = 1 TO  ;
              liintcount
               lcmask = '9' +  ;
                        lcmask
               IF MOD(lix, 3) = 0  ;
                  .AND. lix <  ;
                  liintcount  ;
                  .AND. .commas
                    lcmask = ',' +  ;
                             lcmask
               ENDIF
          ENDFOR
          IF .decimals > 0
               lcmask = lcmask +  ;
                        "."
               FOR lix = 1 TO  ;
                   .decimals
                    lcmask = lcmask +  ;
                             "9"
               ENDFOR
               IF LEFT(lcmask, 1) =  ;
                  '.'
                    lcmask = "9" +  ;
                             lcmask
               ENDIF
          ENDIF
          IF .currency
               lcmask = '$' +  ;
                        lcmask
          ENDIF
          this.inputmask = lcmask
     ENDWITH
ENDFUNC
*
PROCEDURE Refresh
     this.setinputmask()
ENDPROC
*
FUNCTION Init
     ASSERT VARTYPE(this.value) ==  ;
            'N' MESSAGE  ;
            "Value must be numeric"
     ASSERT VARTYPE(this.decimals) ==  ;
            'N' MESSAGE  ;
            "Decimals must be numeric"
     ASSERT VARTYPE(this.commas) ==  ;
            'L' MESSAGE  ;
            "Commas must be Logical"
     ASSERT VARTYPE(this.currency) ==  ;
            'L' MESSAGE  ;
            "Currencey must be Logical"
     IF VARTYPE(this.value) +  ;
        VARTYPE(this.decimals) +  ;
        VARTYPE(this.commas) +  ;
        VARTYPE(this.currency) <>  ;
        "NNLL"
          RETURN .F.
     ENDIF
     this.coriginputmask = this.inputmask
     this.coldinputmask = this.inputmask
     this.setinputmask()
ENDFUNC
*
PROCEDURE LostFocus
     LOCAL lcmask, lcolddecimals
     lcolddecimals = SET("DECIMALS")
     SET DECIMALS TO this.decimals
     WITH this
          IF VARTYPE(.oldtype) <>  ;
             'C'
               .oldtype = VARTYPE(.value)
          ENDIF
          .setinputmask()
          DO CASE
               CASE .oldtype =  ;
                    'N' .AND.  ;
                    VARTYPE(.value) ==  ;
                    'C'
                    .value = VAL(.value)
               CASE .oldtype =  ;
                    'N' .AND.  ;
                    VARTYPE(.value) ==  ;
                    'N'
               CASE .oldtype =  ;
                    'C' .AND.  ;
                    VARTYPE(.value) ==  ;
                    'C'
               CASE .oldtype =  ;
                    'C' .AND.  ;
                    VARTYPE(.value) ==  ;
                    'N'
                    .value = STR(.value)
               OTHERWISE
          ENDCASE
          lvalue = .value
          .controlsource = .oldcontrolsource
          .value = lvalue
     ENDWITH
     SET DECIMALS TO lcolddecimals
ENDPROC
*
FUNCTION GotFocus
     WITH this
          .oldcontrolsource = .controlsource
          .controlsource = ""
          .oldtype = VARTYPE(.value)
          DO CASE
               CASE .oldtype =  ;
                    'N'
                    IF INT(.value) <>  ;
                       .value
                         .value =  ;
                          ALLTRIM(STR(.value,  ;
                          20,  ;
                          .decimals))
                    ELSE
                         .value =  ;
                          ALLTRIM(STR(.value,  ;
                          20))
                    ENDIF
               CASE .oldtype =  ;
                    'C'
               OTHERWISE
                    MESSAGEBOX( ;
                              "Unhandled Type",  ;
                              0,  ;
                              "Unhandled Value Type" ;
                              )
                    .enabled = .F.
                    RETURN .F.
          ENDCASE
          .coldinputmask = .inputmask
          .inputmask = ""
          textbox::gotfocus()
          .selstart = 0
          .sellength = LEN(.value)
          NODEFAULT
     ENDWITH
ENDFUNC
*
PROCEDURE Key
     LPARAMETERS nkeycode,  ;
                 nshiftaltctrl
     LOCAL liloc
     WITH this
          DO CASE
               CASE nkeycode =  ;
                    ASC('.')
                    liloc = AT('.',  ;
                            .value)
                    IF (liloc <>  ;
                       0) .AND.   ;
                       .NOT.  ;
                       (BETWEEN(liloc -  ;
                       1,  ;
                       .selstart,  ;
                       .selstart +  ;
                       .sellength -  ;
                       1))
                         NODEFAULT
                    ENDIF
               CASE nkeycode =  ;
                    ASC('-')
                    liloc = AT( ;
                            '-',  ;
                            .value)
                    IF .selstart <>  ;
                       0 .OR.  ;
                       (liloc <>  ;
                       0 .AND.  ;
                       .sellength >  ;
                       0)
                         NODEFAULT
                    ENDIF
               CASE BETWEEN(nkeycode,  ;
                    ASC('0'),  ;
                    ASC('9'))
               CASE BETWEEN(nkeycode,  ;
                    32, 122)
                    NODEFAULT
               OTHERWISE
          ENDCASE
     ENDWITH
ENDPROC
*
ENDDEFINE
*

FUNCTION NombreMes
PARAMETERS nMes
LOCAL NombreMes

DO CASE 
	CASE nMes = 1
		NombreMes="Enero"
	CASE nMes = 2
		NombreMes="Febrero"
	CASE nMes = 3
		NombreMes="Marzo"
	CASE nMes = 4
		NombreMes="Abril"
	CASE nMes = 5
		NombreMes="Mayo"
	CASE nMes = 6
		NombreMes="Junio"
	CASE nMes = 7
		NombreMes="Julio"
	CASE nMes = 8
		NombreMes="Agosto"
	CASE nMes = 9
		NombreMes="Setiembre"
	CASE nMes = 10
		NombreMes="Octubre"
	CASE nMes = 11
		NombreMes="Noviembre"
	CASE nMes = 12
		NombreMes="Diciembre"
	ENDCASE 

RETURN 	NombreMes

ENDFUNC 

PROCEDURE Imprimir
LPARAMETERS Fila,Columna,Valor,Mascara
IF PCOUNT()=3
	Mascara="@X"
ENDIF

@fila,columna Say Valor picture Mascara
ENDPROC 
	
FUNCTION RPVersion
LPARAMETERS lcReportName
LOCAL ldFecha
ldFecha = FDATE(lcReportName)
ldFecha = STR(YEAR(ldFecha),4)+'.'+ STR(month(ldFecha),2)+'.'+ STR(day(ldFecha),2)
RETURN JUSTFNAME(lcReportName)+'.V:'+ldFecha 


FUNCTION Exportar
gcTable=GETFILE('XLS', 'Seleccione el archivo destino:','Browse', 0, 'Browse')
IF !EMPTY(gcTable)
	COPY TO (gcTable) TYPE xl5
ENDIF
ENDFUNC
	   
PROCEDURE  SETNull
LPARAMETERS Valor
	IF EMPTY(Valor)
		valor= null
	ENDIF
ENDPROC


PROCEDURE Recorrer(nColumna,Valor)
EXTERNAL ARRAY Matriz,Vector
LOCAL i as Integer
	FOR i=1 TO ALEN(matriz,1)
		*Usamos el indice de recursividad para guardar el valor
		vector(PROGRAM(-1)) = matriz(i,nColumna)
		IF ALEN(matriz,2)>nColumna
			Recorrer(nColumna+1,matriz(i,nColumna))
		ENDIF
		Valor=""
		*Solo en el ultimo nivel de recursividad recorremos el vector final
		IF PROGRAM(-1)=ALEN(vector)
		FOR x=1 TO ALEN(vector)
			Valor = Valor +  vector(x) + " "
		ENDFOR 	
		? Valor
		ENDIF
		
	ENDFOR

ENDPROC


PROCEDURE CapturarPantalla
DECLARE LONG keybd_event IN "user32" INTEGER bVk,   INTEGER bScan,   LONG dwFlags,  LONG dwExtraInfo
=keybd_event (44, 1, 0, 0)
ENDPROC

PROCEDURE AcercaDe
	
DECLARE LONG ShellAbout IN Shell32 ;
 LONG nHwnd,;
 STRING cTitulo,;
 STRING cCaption,;
 LONG nIcon
nHwnd = 0
cTitulo = "Titulo de la ventana"
cCaption = "Aqui se pone el autor del programa"
nIcon = 0
ShellAbout(@nHwnd,@cTitulo,@cCaption, @nIcon)
ENDPROC

PROCEDURE ImpresionFactura
	LPARAMETERS idformato
	m.codigo= idformato
	=Sql('select descripcionsql,comandoImpresion,Destino from sys_impresion where codigo=?m.codigo', 'csql')
	=Sql("select * from sys_impresiondet where codigo =?m.codigo order by ordenimpresion, fila, columna", 'detsql')
	*=sql("select * from sys_impresiondet where codigo =?idformato and tipo= 'P' order by fila, columna", 'psql')
	=Sql("select * from sys_impresiondet where codigo =?m.codigo and tipo= 'D' order by ordenimpresion, fila, columna", 'dsql')
	=Sql (csql.descripcionsql, 'cfactura')



IF RECCOUNT('cSQL')=0
	MESSAGEBOX("No se encuentra el formato indicado: " + STR(idformato),16,"Futura Software")
	RETURN
ENDIF
		
*SET STEP ON

	Create Cursor CampoMemo(Contenido m)
	Append Blank In CampoMemo



	Set Device To File (csql.Destino)
	*fpos=6
	*imprimir(0,0,CHR(27)+CHR(48))
	pos=0
	Select detsql
	Scan For tipo = 'C'
		Select cfactura
		If Empty(Nvl(detsql.condicion,'')) Or Evaluate(ALLTRIM(detsql.condicion))
			If (detsql.calculo)
				ejecutar=ALLTRIM(detsql.campo)
				&ejecutar
			Else
				imprimir(detsql.fila,detsql.columna,Evaluate(detsql.campo))
			Endif
		Endif

		Select detsql
	Endscan

	Select cfactura
	lpos=1
	Scan

		Select dsql

*SET STEP ON

		pos=0 &&prueba
		SCAN 	&& Para las lineas simples
			**pos=0 prueba
			
			Select cfactura
			If Empty(Nvl(dsql.condicion,'')) Or Evaluate(dsql.condicion)
				If (dsql.calculo)
					ejecutar=dsql.campo
					&ejecutar
				Else
					If Not Nvl(dsql.Estirar,.F.)
						If Not Empty(Nvl(dsql.LongitudMax,''))
							imprimir(dsql.fila+lpos,dsql.columna,Left(Evaluate(dsql.campo),dsql.LongitudMax))
						Else
							imprimir(dsql.fila+lpos,dsql.columna,Evaluate(dsql.campo))
						Endif

					Else
						m.Contenido = Evaluate(dsql.campo)
						Set Memowidth To Nvl(dsql.LongitudMax,40)
						Replace CampoMemo.Contenido With Alltrim(m.Contenido) In CampoMemo
						imprimir(dsql.fila+lpos,dsql.columna,Mline(CampoMemo.Contenido, 1) )
					ENDIF
					pos=1

				Endif


			Endif

			Select dsql
		Endscan

		SCAN FOR Nvl(dsql.Estirar,.F.) 	&& Para las multilineas

			pos=1
			
			Select cfactura
						m.Contenido = Evaluate(dsql.campo)
						Set Memowidth To Nvl(dsql.LongitudMax,40)
						Replace CampoMemo.Contenido With Alltrim(m.Contenido) In CampoMemo
	*	STORE 0 TO _MLINE             && Reset _MLINE to zero
						For i=2 To Memlines(CampoMemo.Contenido)
							imprimir(dsql.fila+lpos+pos,dsql.columna,Mline(CampoMemo.Contenido, i) )
							pos = pos +1
						ENDFOR
						
			Select dsql
		Endscan


		lpos=lpos + pos
		Select cfactura
	Endscan

	Goto Top In cfactura
	Select detsql
	Scan For tipo = 'P'
		Select cfactura
		If Empty(Nvl(detsql.condicion,'')) Or Evaluate(detsql.condicion)
			If (detsql.calculo)
				ejecutar=detsql.campo
				&ejecutar
			Else
				IF (detsql.fila=0) 
					lfila = PROW()
				ELSE  	
					IF (detsql.fila= -1)
						lfila = PROW()+1
					ELSE
						lfila = detsql.fila
					ENDIF
				endif	
					
				imprimir(lfila,detsql.columna,Evaluate(detsql.campo))
			Endif
		Endif


		Select detsql
	Endscan

	Set Device To Screen
	ejecutar=Nvl(csql.ComandoImpresion,'')
	**SET STEP ON
	
	If !Empty(ejecutar)
		EXECSCRIPT(ejecutar)
	ENDIF
ENDPROC

PROCEDURE RegistrarError(lcmensaje as String)
LOCAL lcAlias
lcAlias=ALIAS()
	
	IF  !USED("_Errores")
		USE datos!Errores ALIAS _Errores IN 0 
	ENDIF
	IF TYPE("oApp")="O"
	 
		INSERT INTO  _Errores(IdEmpresa,Usuario,Detalle) vALUES(oApp.Empresa,oapp.getemployeeid(),lcmensaje)
	ELSE
		INSERT INTO  _Errores(IdEmpresa,Usuario,Detalle) vALUES("","",lcmensaje)
	ENDIF
	
	USE IN _Errores
	IF !EMPTY(lcAlias)
		SELECT (lcAlias)
	ENDIF
		
ENDPROC 
	

PROCEDURE SendMail(desde,A,Asunto,Mensaje,archivoAdjunto)
x=CREATEOBJECT("NET4COM.Network")
IF SQL("Select convert(varchar(200),dbo.LeerConstante(?oApp.Empresa,'SYS_SMTPSERVER')) as SMTP", 'cConfig')>0

p1=AT(',',cConfig.SMTP)
p2=AT(',',cConfig.SMTP,2)

lcServer = ALLTRIM(SUBSTR(cConfig.SMTP,1,p1-1))
lcUser = ALLTRIM(SUBSTR(cConfig.SMTP,p1+1,p2-p1-1))
lcPwd = ALLTRIM(SUBSTR(cConfig.SMTP,p2+1))

	
IF EMPTY(archivoAdjunto)
	x.SendEmail(desde,A,Asunto,Mensaje,lcServer,lcUser,lcPwd)
ELSE
	x.SendEmailWithAttachments(desde,A,Asunto,Mensaje,lcServer,lcUser,lcPwd,archivoAdjunto)	

	*x.SendEmailWithAttachments("vgonzalez@futura.com.py",A,Asunto,Mensaje,"mail.futura.com.py","Soporte","AppFutura2010",archivoAdjunto)
ENDIF

ENDIF

x=null

ENDPROC 

PROCEDURE CalculoInteres

Capital = 10000000
CantidadCuotas = 24
TasaAnual = 24  && Porcentaje
TasaMensual = TasaAnual/12/100


*-- Listado
? "    Nro.Cuota        Cuota             Cuota Capitalizada Interes"
? REPLICATE("-",80)
FOR NroCuota = 1 TO CantidadCuotas
Cuota = Capital * (TasaMensual/(1-(1+TasaMensual)^-CantidadCuotas))
CuotaCapitalizada = Cuota * (1-TasaMensual*(1-(1+TasaMensual)^-(CantidadCuotas-NroCuota+1))/TasaMensual)
Interes = TasaMensual * Cuota * (1-(1+TasaMensual)^-(CantidadCuotas-NroCuota+1))/TasaMensual
? NroCuota, ROUND(Cuota,2), ROUND(CuotaCapitalizada,2), ROUND(Interes,2)
ENDFOR

ENDPROC 



PROCEDURE ImprimirFactura
LPARAMETERS lnIdFactura

*!*	=THIS.RUNSQL("Select convert(int,dbo.LeerConstante(?oApp.Empresa,'VT_IDFORMATOTPV')) as IdFormato", 'cConfig')
*!*	IF RECCOUNT('cConfig')=0
*!*	 MESSAGEBOX("No se encuentra la constante VT_IDFORMATOTPV")
*!*		RETURN
*!*	ENDIF

*!*	M.IDFACTURA = lnIdFactura
*!*	m.IdFormato=cConfig.IdFormato
*!*	=ImpresionFactura(cConfig.IdFormato)


****

 M.IDFACTURA = lnIdFactura
*idFormato = 5
*IF CMONEDA.DEC=0
	M.DEC = 0
*ELSE
*	M.DEC = 2
*ENDIF

*this.impresion1.imprimir()

*Verificamos la forma de imprimir la factura
*Si tiene nombre formato, se usa el report
*Si no tiene, se usa ASCII
=SQL("Select convert(CHAR(50),dbo.LeerConstante(?oApp.Empresa,'VT_NOMBREFORMATO_FACT')) as NombreFormato", 'cTipo')
IF RECCOUNT('cTipo')=0
 MESSAGEBOX("No se encuentra la constante VT_NOMBREFORMATO_FACT")
	RETURN
ENDIF

IF !EMPTY(NVL(cTipo.NombreFormato,''))
	cTipoImpresion='R'		&&Report
ELSE
	cTipoImpresion='A'		&&Ascii
ENDIF


*VG 05/12/2010
*No se aplica nota de credito
*F xVenta.Tipo='D'

	IF cTipoImpresion='A'
		=SQL("Select convert(int,dbo.LeerConstante(?oApp.Empresa,'VT_IDFORMATOTPV')) as IdFormato", 'cConfig')
		IF RECCOUNT('cConfig')=0
		 MESSAGEBOX("No se encuentra la constante VT_IDFORMATOTPV")
			RETURN
		ENDIF
			=ImpresionFactura(cConfig.Idformato)
	ELSE
*		REPORT FORM (cTipo.NombreFormato)  TO PRINTER NOPAGEEJECT 
*		REPORT FORM (cTipo.NombreFormato)  TO PRINTER NOPAGEEJECT 
		REPORT FORM (cTipo.NombreFormato)  PREVIEW
	ENDIF
							
*ELSE
*	DO ('notacredito'+oApp.Empresa)
	*REPORT FORM vt_NotaCredito ASCII TO FILE c:\factura.txt
*ENDIF
ENDPROC 


PROCEDURE PrintShell(FileName, FilePath)

hndwin= 0 
cAction= "print" 
cFileName= FileName  
cParams= "" 
cDir= FilePath
nShowWin= 1

ShellExecute(hndWin,cAction,cFileName,cParams,cDir,nShowWin)

ENDPROC 	



*--------------------------------------------------------------------------
* FUNCTION _StrTo39(tcString)
*--------------------------------------------------------------------------
* Convierte un string para ser impreso con
* fuente True Type Barcode 3 of 9
* USO: _StrTo39("Codigo 39")
* RETORNA: Caracter
* AUTOR: Luis María Guayán
*--------------------------------------------------------------------------
FUNCTION _StrTo39(tcString)
   lcRet = "*"+tcString+"*"
   RETURN lcRet
ENDFUNC

*--------------------------------------------------------------------------
* FUNCTION _StrTo128A(tcString)
*--------------------------------------------------------------------------
* Convierte un string para ser impreso con
* fuente True Type Barcode 128 A
* Caracteres numéricos y alfabéticos (solo mayúsculas)
* Si un caracter es no válido lo reemplaza por espacio
* USO: _StrTo128A("CODIGO 128")
* RETORNA: Caracter
* AUTOR: Luis María Guayán
*--------------------------------------------------------------------------
FUNCTION _StrTo128A(tcString)

   LOCAL lcStart, lcStop, lcRet, lcCheck, ;
      lnLong, lnI, lnCheckSum, lnAsc

   lcStart = CHR(103 + 32)
   lcStop = CHR(106 + 32)
   lnCheckSum = ASC(lcStart) - 32

   lcRet = tcString
   lnLong = LEN(lcRet)
   FOR lnI = 1 TO lnLong
      lnAsc = ASC(SUBS(lcRet,lnI,1)) - 32
      IF NOT BETWEEN(lnAsc, 0, 64)
         lcRet = STUFF(lcRet,lnI,1,CHR(32))
         lnAsc = ASC(SUBS(lcRet,lnI,1)) - 32
      ENDIF
      lnCheckSum = lnCheckSum + (lnAsc * lnI)
   ENDFOR
   lcCheck = CHR(MOD(lnCheckSum,103) + 32)
   lcRet = lcStart + lcRet + lcCheck + lcStop
   *--- Esto es para cambiar los espacios y caracteres invalidos
   lcRet = STRTRAN(lcRet, CHR(32), CHR(232))
   lcRet = STRTRAN(lcRet, CHR(127), CHR(192))
   *---
   RETURN lcRet
ENDFUNC

*--------------------------------------------------------------------------
* FUNCTION _StrTo128B(tcString)
*--------------------------------------------------------------------------
* Convierte un string para ser impreso con
* fuente True Type Barcode 128 B
* Caracteres numéricos y alfabéticos (mayúsculas y minúsculas)
* Si un caracter es no válido lo reemplaza por espacio
* USO: _StrTo128B("Codigo 128")
* RETORNA: Caracter
* AUTOR: Luis María Guayán
*--------------------------------------------------------------------------
FUNCTION _StrTo128B(tcString)

   LOCAL lcStart, lcStop, lcRet, lcCheck, ;
      lnLong, lnI, lnCheckSum, lnAsc

   lcStart = CHR(104 + 32)
   lcStop = CHR(106 + 32)
   lnCheckSum = ASC(lcStart) - 32

   lcRet = tcString
   lnLong = LEN(lcRet)
   FOR lnI = 1 TO lnLong
      lnAsc = ASC(SUBS(lcRet,lnI,1)) - 32
      IF NOT BETWEEN(lnAsc, 0, 99)
         lcRet = STUFF(lcRet,lnI,1,CHR(32))
         lnAsc = ASC(SUBS(lcRet,lnI,1)) - 32
      ENDIF
      lnCheckSum = lnCheckSum + (lnAsc * lnI)
   ENDFOR
   lcCheck = CHR(MOD(lnCheckSum,103) + 32)
   lcRet = lcStart + lcRet + lcCheck + lcStop
   *--- Esto es para cambiar los espacios y caracteres invalidos
   lcRet = STRTRAN(lcRet, CHR(32), CHR(232))
   lcRet = STRTRAN(lcRet, CHR(127), CHR(192))
   *---
   RETURN lcRet
ENDFUNC

*--------------------------------------------------------------------------
* FUNCTION _StrTo128C(tcString)
*--------------------------------------------------------------------------
* Convierte un string para ser impreso con
* fuente True Type Barcode 128 C
* Solo caracteres numéricos
* USO: _StrTo128C("01234567")
* RETORNA: Caracter
* AUTOR: Luis María Guayán
*--------------------------------------------------------------------------
FUNCTION _StrTo128C(tcString)

   LOCAL lcStart, lcStop, lcRet, lcCheck, lcCar;
      lnLong, lnI, lnCheckSum, lnAsc

   lcStart = CHR(105 + 32)
   lcStop = CHR(106 + 32)
   lnCheckSum = ASC(lcStart) - 32

   lcRet = ALLTRIM(tcString)
   lnLong = LEN(lcRet)
   *--- La longitud debe ser par
   IF MOD(lnLong,2) # 0
      lcRet = "0" + lcRet
      lnLong = LEN(lcRet)
   ENDIF

   *--- Convierto los pares a caracteres
   lcCar = ""
   FOR lnI = 1 TO lnLong STEP 2
      lcCar = lcCar + CHR(VAL(SUBS(lcRet,lnI,2)) + 32)
   ENDFOR
   lcRet = lcCar
   lnLong = LEN(lcRet)

   FOR lnI = 1 TO lnLong
      lnAsc = ASC(SUBS(lcRet,lnI,1)) - 32
      lnCheckSum = lnCheckSum + (lnAsc * lnI)
   ENDFOR
   lcCheck = CHR(MOD(lnCheckSum,103) + 32)
   lcRet = lcStart + lcRet + lcCheck + lcStop
   *--- Esto es para cambiar los espacios y caracteres invalidos
   lcRet = STRTRAN(lcRet, CHR(32), CHR(232))
   lcRet = STRTRAN(lcRet, CHR(127), CHR(192))
   *---
   RETURN lcRet
ENDFUNC

*--------------------------------------------------------------------------
* FUNCTION _StrToEan13(tcString, .T.)
*--------------------------------------------------------------------------
* Convierte un string para ser impreso con
* fuente True Type EAN-13
* PARAMETROS:
*   tcString: Caracter de 12 dígitos (0..9)
*   tlCheckD: .T. Solo genera el dígito de control
*             .F. Genera dígito y caracteres a imprimir
* USO: _StrToEan13("123456789012")
* RETORNA: Caracter
* AUTOR: Luis María Guayán
*--------------------------------------------------------------------------
FUNCTION _StrToEan13(tcString, tlCheckD)

   LOCAL lcLat, lcMed, lcRet, lcJuego, ;
      lcIni, lcResto, lcCod, ;
      lnI, lnCheckSum, lnAux, laJuego(10), lnPri

   lcRet=ALLTRIM(tcString)

   IF LEN(lcRet) # 12
      *--- Error en parámetro
      *--- debe tener un len = 12
      RETURN ""
   ENDIF

   *--- Genero dígito de control
   lnCheckSum=0
   FOR lnI = 1 TO 12
      IF MOD(lnI,2) = 0
         lnCheckSum = lnCheckSum + VAL(SUBS(lcRet,lnI,1)) * 3
      ELSE
         lnCheckSum = lnCheckSum + VAL(SUBS(lcRet,lnI,1)) * 1
      ENDIF
   ENDFOR
   lnAux = MOD(lnCheckSum,10)
   lcRet = lcRet + ALLTRIM(STR(IIF(lnAux = 0, 0, 10-lnAux)))

   IF tlCheckD
      *--- Si solo genero dígito de control
      RETURN lcRet
   ENDIF

   *--- Para imprimir con fuente True Type EAN13
   *--- 1er. dígito (lnPri)
   lnPri = VAL(LEFT(lcRet, 1))
   *--- Tabla de Juegos de Caracteres
   *--- según "lnPri" (¡NO CAMBIAR!)
   laJuego(1) = "AAAAAACCCCCC"   && 0
   laJuego(2) = "AABABBCCCCCC"   && 1
   laJuego(3) = "AABBABCCCCCC"   && 2
   laJuego(4) = "AABBBACCCCCC"   && 3
   laJuego(5) = "ABAABBCCCCCC"   && 4
   laJuego(6) = "ABBAABCCCCCC"   && 5
   laJuego(7) = "ABBBAACCCCCC"   && 6
   laJuego(8) = "ABABABCCCCCC"   && 7
   laJuego(9) = "ABABBACCCCCC"   && 8
   laJuego(10) = "ABBABACCCCCC"   && 9

   *--- Caracter inicial (fuera del código)
   lcIni = CHR(lnPri + 35)
   *--- Caracteres lateral y central
   lcLat = CHR(33)
   lcMed = CHR(45)

   *--- Resto de los caracteres
   lcResto = SUBS(lcRet, 2, 12)
   FOR lnI = 1 TO 12
      lcJuego = SUBS(laJuego(lnPri + 1), lnI, 1)
      DO CASE
         CASE lcJuego = "A"
            lcResto = STUFF(lcResto, lnI, 1, CHR(VAL(SUBS(lcResto, lnI, 1))+48))
         CASE lcJuego = "B"
            lcResto = STUFF(lcResto, lnI, 1, CHR(VAL(SUBS(lcResto, lnI, 1))+65))
         CASE lcJuego = "C"
            lcResto = STUFF(lcResto, lnI, 1, CHR(VAL(SUBS(lcResto, lnI, 1))+97))
      ENDCASE
   ENDFOR

   *--- Armo código
   lcCod = lcIni + lcLat + SUBS(lcResto,1,6) + lcMed + SUBS(lcResto,7,6) + lcLat
   RETURN lcCod
ENDFUNC


*--------------------------------------------------------------------------
* FUNCTION _StrToEan8(tcString, .T.)
*--------------------------------------------------------------------------
* Convierte un string para ser impreso con
* fuente True Type EAN-8
* PARAMETROS:
*   tcString: Caracter de 7 dígitos (0..9)
*   tlCheckD: .T. Solo genera el dígito de control
*             .F. Genera dígito y caracteres a imprimir
* USO: _StrToEan8("1234567")
* RETORNA: Caracter
* AUTOR: Luis María Guayán
*--------------------------------------------------------------------------
FUNCTION _StrToEan8(tcString, tlCheckD)

   LOCAL lcLat, lcMed, lcRet, ;
      lcIni, lcCod, ;
      lnI, lnCheckSum, lnAux

   lcRet=ALLTRIM(tcString)

   IF LEN(lcRet) # 7
      *--- Error en parámetro
      *--- debe tener un len = 7
      RETURN ""
   ENDIF

   *--- Genero dígito de control
   lnCheckSum=0
   FOR lnI = 1 TO 7
      IF MOD(lnI,2) = 0
         lnCheckSum = lnCheckSum + VAL(SUBS(lcRet,lnI,1)) * 1
      ELSE
         lnCheckSum = lnCheckSum + VAL(SUBS(lcRet,lnI,1)) * 3
      ENDIF
   ENDFOR
   lnAux = MOD(lnCheckSum,10)
   lcRet = lcRet + ALLTRIM(STR(IIF(lnAux = 0, 0, 10-lnAux)))

   IF tlCheckD
      *--- Si solo genero dígito de control
      RETURN lcRet
   ENDIF

   *--- Para imprimir con fuente True Type EAN8
   *--- Caracteres lateral y central
   lcLat = CHR(33)
   lcMed = CHR(45)

   *--- Caracteres
   FOR lnI = 1 TO 8
      IF lnI <= 4
         lcRet = STUFF(lcRet, lnI, 1, CHR(VAL(SUBS(lcRet, lnI, 1))+48))
      ELSE
         lcRet = STUFF(lcRet, lnI, 1, CHR(VAL(SUBS(lcRet, lnI, 1))+97))
      ENDIF
   ENDFOR

   *--- Armo código
   lcCod = lcLat + SUBS(lcRet,1,4) + lcMed + SUBS(lcRet,5,4) + lcLat
   RETURN lcCod
ENDFUNC

*--------------------------------------------------------------------------

FUNCTION CadenaConsulta(lcCadena,lcCampo,lcValor,lcCondicion)

lcCondicion = UPPER(lcCondicion)
IF lcCondicion='LIKE'
	DO WHILE AT(' ',lcValor)>0
		
		lcCadena=CadenaConsulta(lcCadena,lcCampo,SUBSTR(lcValor,1,AT(' ',lcValor)-1),lcCondicion)
		lcValor = SUBSTR(lcValor,AT(' ',lcValor)+1)
	ENDDO 
ENDIF 
	

IF !EMPTY(lcValor)
	IF !EMPTY(lcCadena)
		lcCadena = lcCadena + " AND "
	ENDIF
	
	lcCadena = lcCadena + lcCampo + " "+ lcCondicion + " "
	IF ALLTRIM(lcCondicion) ='LIKE' 
		lcCadena = lcCadena + "'%"+lcValor+"%'"
	ELSE
		lcCadena = lcCadena + "'"+ TRANSFORM(lcValor)+"'"
	ENDIF
ENDIF
RETURN lcCadena
ENDFUNC 


function TIMECALC

   PARAMETERS bt,et,op
   hr1=VAL(LEFT(bt,2))
   hr2=VAL(LEFT(et,2))
   mn1=VAL(SUBSTR(bt,4,2))
   mn2=VAL(SUBSTR(et,4,2))
   sc1=VAL(RIGHT(bt,2))
   sc2=VAL(RIGHT(et,2))
   tot1=(hr1*3600)+(mn1*60)+sc1
   tot2=(hr2*3600)+(mn2*60)+sc2
   IF op=1
     tt=tot1+tot2
   ELSE
   		IF tot2>tot1
     		tt=tot2-tot1
     	ELSE
     		tt= 86400-tot1+tot2
     	ENDIF
     	
   ENDIF
   thr=ALLTRIM(STR(INT(tt/3600)))
   tmn=ALLTRIM(STR(INT((tt%3600)/60)))
   tsc=ALLTRIM(STR((tt%3600)%60))
   tdc=RIGHT(STR(INT((VAL(tmn)/60)*10)/10,5,1),1)
   RETURN RIGHT('00'+thr,2)+':'+ ;
          RIGHT('00'+tmn,2)+':'+ ;
          RIGHT('00'+tsc,2)
          **;
          **chr(13)+chr(13)+ '  Decimal equivalent: '+ ;
          **thr+'.'+tdc+' hour'+iif(val(tdc)<>1,'s','')+'.'+chr(13)
  ENDFUNC         
  
FUNCTION ACOLSCAN
LPARAMETER aSearch,sExpr,nColumn,lRetRow
 * This function does an ASCAN for a specific row
* aSearch - array to scan
* sExpr - expression to scan
* nColumn - column to scan
* lRetRow - return row (T) or array element (F)

LOCAL npos
IF VARTYPE(m.nColumn)#'N'
	nColumn = 1
ENDIF
IF VARTYPE(m.lRetRow)#'L'
	lRetRow = .F.
ENDIF

npos = 1
DO WHILE .T.
	npos = ASCAN(aSearch,m.sExpr,m.npos)
	DO CASE
	CASE m.npos=0	&&did not find match
		EXIT
	CASE ASUBSCRIPT(aSearch,m.npos,2)=m.nColumn
		EXIT
	OTHERWISE
		m.npos=m.npos+1
	ENDCASE
ENDDO
IF m.lRetRow
	RETURN ASUBSCRIPT(aSearch,m.npos,1)
ELSE
	RETURN m.npos
ENDIF

ENDFUNC 
 
PROCEDURE  CrearFacturaCursor
LPARAMETERS lcEmpresa, lnIdFactura, lcCpbte, lndNro, lnhNro, lnIdEmision
*DO seteo

CREATE CURSOR params (Empresa C(3), Cpbte c(3), dNro n(13), hNro n(13), IdFactura I, IdEmision I)
INSERT INTO Params values(lcEmpresa,lcCpbte, lndNro,lnhNro,lnIdFactura,lnIdEmision)    


TEXT TO cmdSQL noshow
	SELECT     v.IdComprobante, v.Numero, v.Sucursal, v.IdCliente, V.RazonSocial AS Cliente, v.IdCondicion, cond.Descripcion AS Condicion, v.Fecha, v.IdVendedor, 
	                      RTRIM(p.Nombre) + ' ' + p.Apellido AS Vendedor, v.IdNegocio, det.IdProducto, det.Cantidad, det.Real as Precio, det.Ult_Costo AS CostoUnitario, 
	                      det.GravadaIncluido, det.RegimenTurismo, det.Imprime, ISNULL(cn.DireccionServicio, c.Direccion) as Direccion, c.Telefono, v.Ruc, cond.Plazo, isnull(rtrim(det.Descripcion), ' ') AS Producto, det.Iva, 
	                      det.[real], TotalFactura,v.IdMoneda,Vence=dateadd(dd,cond.Inicial,v.Fecha),cond.Inicial,
	                      Remision = dbo.Vt_RemisionDescripcion(v.IdFactura),v.IdFActura,Det.Descuento,v.IdPedido,v.Audit_Usuario,v.NroContrato,v.IdCiclo,
	                      SPACE(30) Obs1, space(30) obs2, SPACE(240) Letras 
	FROM         dbo.vt_clientes c RIGHT OUTER JOIN
	                      dbo.vt_factura v left JOIN
	                      dbo.st_movimiento_Det det ON v.IdFactura = det.IdFactura and det.Imprime=1 ON c.IdEmpresa = v.IdEmpresa AND c.IdCliente = v.IdCliente INNER JOIN 
	                      st_producto AS pr ON det.idproducto = pr.idproducto and det.idempresa=pr.idempresa LEFT OUTER JOIN
	                      dbo.vt_Condicion cond ON v.IdEmpresa = cond.IdEmpresa AND v.IdCondicion = cond.IdCondicion LEFT OUTER JOIN
	                      dbo.BS_Personas p INNER JOIN
	                      dbo.vt_Vendedores vend ON p.IdPersona = vend.idpersona ON v.IdEmpresa = vend.IdEmpresa AND v.IdVendedor = vend.IdVendedor
	                      left join sas_Contrato cn on v.IdEmpresa = cn.IdEmpresa and v.NroContrato = cn.NroContrato
	                      where v.IdEmpresa = ?oApp.Empresa
	                      and (v.IdFactura = ?Params.IdFactura or ?Params.IdFactura = 0)
	                      and ((v.IdComprobante = ?Params.Cpbte and v.numero between ?Params.dNro and ?Params.hNro) or (?Params.dNro=0 and ?Params.hNro =0))
	                      and (v.IdEmision = ?Params.IdEmision or ?Params.IdEmision=0)
ENDTEXT


sql(cmdSQL,'_cFactura')

m.IdActual = _cFactura.IdFactura

SELECT * FROM _cFactura INTO CURSOR _cFact READWRITE WHERE 1=2


SELECT distinct IdFactura FROM _cFactura INTO CURSOR xFac 


SELECT xFac
SCAN 
	m.IdActual = xFac.IdFactura
	SELECT * FROM _cFactura c INTO CURSOR cFactura READWRITE WHERE c.IdFactura=m.IdActual
	SELECT cFactura 



	m.Total = TotalFactura
	m.moneda = IdMoneda
	m.nro=0
	*m.nro = RECCOUNT()

	TEXT TO cmdSQL NOSHOW 
		SELECT top(8)  a.IdAfiliado,a.CI,a.Nombre
		from sas_Contrato c inner join sas_AfiliadoContrato ac on c.IdContrato = ac.IdContrato
		inner join sas_Afiliados a on ac.IdAfiliado= a.IdAfiliado
		where c.IDEmpresa=?oApp.Empresa
			and c.NroContrato = ?cFactura.NroContrato
			and ac.IdEstado ='A'
		order by a.IdAfiliado
	ENDTEXT

	sql(cmdSQL,'cA')

	DIMENSION a1(8)
	DIMENSION a2(8)

	a1=''
	a2=''
	SELECT cA
	m.i=1
	SCAN
		a1(m.i) = Nombre
		a2(m.i) = Nombre
		m.i = m.i + 1
	ENDSCAN


	SELECT cFactura

	SCAN
		m.nro = m.nro + memlines(producto)
	ENDSCAN

	GO top
	m.Ciclo = IdCiclo
	m.Contrato = NroContrato

	m.Factura = IdFActura
	m.IdCliente = cFactura.IdCliente
	m.Cliente = cFactura.Cliente
	m.Direccion = cFactura.Direccion
	m.Fecha = cFactura.Fecha
	m.IdFactura = cFactura.IdFactura
	COUNT ALL FOR NVL(Descuento,0)<>0 TO m.NroDesc

	m.Nro = m.Nro + m.NroDesc

	DO WHILE m.nro<10
		APPEND BLANK
		replace IdProducto WITH "", TotalFactura WITH m.Total,IdMoneda WITH m.Moneda, IdCiclo WITH m.Ciclo, NroContrato WITH m.Contrato,;
		IdCliente WITH m.IDCliente,Cliente WITH m.Cliente,direccion WITH m.Direccion, Fecha WITH m.Fecha, IdFactura WITH m.IdFactura
		m.nro= m.nro + 1
	ENDDO 
		
	*PUBLIC mLetras


	mLetras= ALLTRIM(numeral(int(m.total)))

	m.decimales=IIF(m.moneda='GS',0,2)

	IF m.decimales > 0
		mletras = "DOLARES " + mletras 
		mLetras = mLetras + ' CON ' + SUBSTR(STR(m.total - INT(m.total),3,2),2) + '/100'
	ELSE 
		mletras = "GUARANIES " + mletras 
	ENDIF
	GO BOTTOM 
	replace Letras WITH mLetras
	FOR m.i=4 TO 1 STEP -1
		replace obs1 WITH a1(m.i), obs2 WITH a2(m.i+4) 
		SKIP -1
	ENDFOR

INSERT INTO _cFact ;
SELECT * FROM cFactura

SELECT xFac
ENDSCAN 
IF USED('xFac')
	USE IN xFac
ENDIF
IF USED('cFactura')	
	USE in cFactura
ENDIF
	
SELECT _cFact
*BROWSE


ENDPROC 

