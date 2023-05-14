' TIPOS
TYPE tipoCliente
        nombre AS STRING * 40
        dni AS STRING * 9' 8 nฃmeros y un letra
        dire AS STRING * 37
        Cuenta AS INTEGER
        pass AS INTEGER
        Saldo AS LONG
        estado AS STRING * 1 ' "1" = est  de alta, "0"= Esta de baja
END TYPE

TYPE TipoOpera
   Cuenta AS INTEGER
   Opera AS INTEGER '1 = Ingreso,  2 =  Extraer
   Fecha AS STRING * 10
   Hora AS STRING * 8
   Saldo AS LONG
   Cantidad AS LONG
END TYPE
                                

' VARIABLES
DIM SHARED posi AS INTEGER 'Escoge posicion del MenuCliente y del MenuBanco
DIM SHARED modo AS INTEGER '1= modo Banco, 2= modo Usuario
DIM SHARED password AS STRING ' PASSORD para entrar en modo Banco
DIM SHARED acertado AS INTEGER' 0=Fallado, 1= Acertado
DIM SHARED PassUsuari AS INTEGER
DIM SHARED RegUsuari AS INTEGER
DIM SHARED AuxCliente AS tipoCliente
DIM SHARED operacion AS TipoOpera





' FUNCIONES Y PROCEDIMIENTOS
DECLARE SUB Fondo ()
DECLARE SUB MenuBanco ()
DECLARE SUB MenuPrincipal ()
DECLARE SUB pidepass ()
DECLARE SUB altas ()
DECLARE SUB bajas ()
DECLARE SUB Modificacion ()
DECLARE SUB listado ()
DECLARE SUB MuestraFicha (reg%)
DECLARE SUB Consulta ()
DECLARE SUB Ingreso ()
DECLARE SUB Extraer ()
DECLARE SUB MuestraOpera ()
DECLARE SUB CambiaPass ()
DECLARE SUB NoPass ()

DECLARE FUNCTION size ()
DECLARE FUNCTION PideCadena$ (x, y, max AS INTEGER)
DECLARE FUNCTION PideDni$ (x, y, max AS INTEGER)
DECLARE FUNCTION PidePassword% (x, y, max AS INTEGER)
DECLARE FUNCTION PideNum% (x, y, max AS INTEGER)
DECLARE FUNCTION PideLong& (x, y, max AS INTEGER)
DECLARE FUNCTION PideDire$ (x, y, max AS INTEGER)
DECLARE FUNCTION PideClave$ (x, y, max AS INTEGER)
DECLARE FUNCTION CreaPass% ()
DECLARE FUNCTION CreaCuenta% ()
DECLARE FUNCTION BuscaFicha% (cliente2 AS tipoCliente)
DECLARE FUNCTION BuscaDni% (cliente2 AS tipoCliente)
DECLARE FUNCTION BuscaPass% (cliente2 AS tipoCliente)
DECLARE FUNCTION BuscaCuenta% (cliente2 AS tipoCliente)
DECLARE FUNCTION MenuConsulta% ()
DECLARE FUNCTION MenuUsuario% ()
DECLARE FUNCTION Reorganiza% ()


























 password = "LSUPERO"

 DO
  MenuPrincipal
  SELECT CASE modo
   CASE 1
    pidepass
    IF acertado = 1 THEN
    DO
     MenuBanco
     SELECT CASE posi
      CASE 1
       altas
      CASE 2
       bajas
      CASE 3
       Modificacion
      CASE 4
       listado
      CASE 5
       Consulta
      CASE 6
       DO
        i% = Reorganiza
       LOOP UNTIL i% = 0
     END SELECT
     CLOSE
    LOOP UNTIL posi = 0
    END IF
    CASE 2
     CLOSE
     COLOR 0, 9
     CLS
     COLOR , 0
     LOCATE 14, 21: PRINT "                                       "
     FOR i% = 12 TO 14
      LOCATE i%, 59: PRINT " "
     NEXT i%
     COLOR 0, 15
     OPEN "Banco.dat" FOR RANDOM AS #1 LEN = LEN(AuxCliente)
     LOCATE 11, 20: PRINT "ษอออออออออออออออออออออออออออออออออออออป"
     LOCATE 12, 20: PRINT "บ Introduce tu PASSWORD:              บ"
     LOCATE 13, 20: PRINT "ศอออออออออออออออออออออออออออออออออออออผ"
     PassUsuari = PidePassword%(12, 45, 3)
     AuxCliente.pass = PassUsuari
     RegUsuari = BuscaPass%(AuxCliente)
     IF RegUsuari = 0 THEN
      NoPass
     ELSE
      DO
       GET #1, RegUsuari, AuxCliente
       posi = MenuUsuario%
       SELECT CASE posi
        CASE 1
         Ingreso
        CASE 2
         Extraer
        CASE 3
         MuestraOpera
        CASE 4
         MuestraFicha (RegUsuari)
         LOCATE 1, 1: PRINT STRING$(80, " ")
         LOCATE 1, 30: PRINT "CONSULTA DE SALDO"
         op$ = INPUT$(1)
        CASE 5
         CambiaPass
       END SELECT
      LOOP UNTIL (posi = 0)
     END IF
   END SELECT
  LOOP UNTIL (modo = 0)

SUB altas

 DIM Nreg AS LONG
 DIM repe AS INTEGER
 CLS
 Fondo
 
 OPEN "Banco.dat" FOR RANDOM AS #1 LEN = LEN(AuxCliente)
 Nreg = LOF(1) / LEN(AuxCliente)
 COLOR 15, 9:
 LOCATE 1, 1: PRINT STRING$(80, " ")
 LOCATE 1, 35: PRINT "ALTAS"
 COLOR 10: LOCATE 7, 17: PRINT "Nombre: "
 COLOR 15: AuxCliente.nombre = PideCadena$(7, 25, 40)
 LOCATE 7, 17: PRINT "Nombre: "
 repe = BuscaFicha%(AuxCliente)
 IF repe > 0 THEN
  COLOR 15
  LOCATE 4, 30: PRINT "Nombre est  repetido"
  SOUND 200, 5
  op$ = INPUT$(1)
  EXIT SUB
 END IF
 COLOR 10: LOCATE 9, 17: PRINT "D.N.I: "
 COLOR 15: AuxCliente.dni = PideDni$(9, 24, 9)
 COLOR 15: LOCATE 9, 17: PRINT "D.N.I: "
 repe = BuscaDni%(AuxCliente)
 IF repe > 0 THEN
  COLOR 15
  LOCATE 4, 30: PRINT " D.N.I est  repetido"
  SOUND 200, 5
  op$ = INPUT$(1)
  EXIT SUB
 END IF


 COLOR 10: LOCATE 11, 17: PRINT "Direcciขn: "
 COLOR 15: AuxCliente.dire = PideDire$(11, 28, 37)
 COLOR 15: LOCATE 11, 17: PRINT "Direcciขn: "
 AuxCliente.Cuenta = CreaCuenta%
 LOCATE 13, 25: PRINT AuxCliente.Cuenta
 AuxCliente.pass = CreaPass%
 LOCATE 15, 26: PRINT AuxCliente.pass
 AuxCliente.Saldo = 0
 LOCATE 17, 23: PRINT AuxCliente.Saldo
 LOCATE 19, 24: PRINT " ALTA"
 AuxCliente.estado = "1"
 PUT #1, Nreg + 1, AuxCliente
 CLOSE #1
  op$ = INPUT$(1)
END SUB

SUB bajas

DIM RegActual AS INTEGER
DIM TotalReg AS INTEGER
DIM AuxNum AS INTEGER
DIM AuxCliente3 AS tipoCliente

 OPEN "Banco.dat" FOR RANDOM AS #1 LEN = LEN(AuxCliente)
 TotalReg = LOF(1) / LEN(AuxCliente)
 IF TotalReg = 0 THEN
  COLOR , 9
  CLS
  COLOR 0, 15
  LOCATE 11, 10: PRINT "ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออป"
  LOCATE 12, 10: PRINT "บ จQu vas a dar de baja si no hay nada para dar de baja? บ"
  LOCATE 13, 10: PRINT "ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ"
  COLOR , 0
  LOCATE 14, 10: PRINT "                                                           "
  FOR i% = 12 TO 14
   LOCATE i%, 69
   PRINT "  "
  NEXT i%
  SOUND 250, 2
  op$ = UCASE$(INPUT$(1))
  CLOSE 1
  EXIT SUB
 END IF
 CLS
 Fondo
 COLOR 15, 9: LOCATE 1, 1: PRINT STRING$(80, " ");
 LOCATE 1, 33: PRINT "BAJAS LOGICAS"
 DO
  COLOR 10: LOCATE 13, 17: PRINT "Cuenta:         "
  COLOR 15
  AuxNum = PideNum%(13, 25, 4)
 LOOP UNTIL AuxNum > 0
 AuxCliente3.Cuenta = AuxNum
 RegActual = BuscaCuenta%(AuxCliente3)
 IF RegActual = 0 THEN
  CLS
  COLOR 0, 15
  LOCATE 11, 30: PRINT "ษอออออออออออออออป"
  LOCATE 12, 30: PRINT "บFicha no existeบ"
  LOCATE 13, 30: PRINT "ศอออออออออออออออผ"
  COLOR , 0
  LOCATE 14, 31: PRINT "                 "
  FOR i% = 12 TO 14
   LOCATE i%, 47: PRINT " "
  NEXT i%
  SOUND 200, 3
  op$ = UCASE$(INPUT$(1))
 ELSE
  MuestraFicha (RegActual)
  COLOR 15, 9: LOCATE 1, 1: PRINT STRING$(80, " ");
  LOCATE 1, 33: PRINT "BAJAS LOGICAS"
  COLOR 14: LOCATE 12, 23: PRINT "จ DESEA DAR DE BAJA ESTA FICHA ? S/N "
  DO
   op$ = UCASE$(INPUT$(1))
  LOOP UNTIL (op$ = "S" OR op$ = "N")
  IF op$ = "S" THEN
   GET #1, RegActual, AuxCliente
   AuxCliente.estado = "0"
   PUT #1, RegActual, AuxCliente
   CLOSE #1
  END IF
 END IF
END SUB

FUNCTION BuscaCuenta% (cliente2 AS tipoCliente)

DIM AuxNumReg AS INTEGER
DIM NumReg AS INTEGER
DIM Cliente AS tipoCliente


 NumReg = 0
 AuxNumReg = 0
 DO
  AuxNumReg = AuxNumReg + 1
  GET #1, AuxNumReg, Cliente
  IF (Cliente.Cuenta = cliente2.Cuenta) THEN
   NumReg = AuxNumReg
  END IF
 LOOP UNTIL EOF(1)
 BuscaCuenta% = NumReg

END FUNCTION

FUNCTION BuscaDni% (cliente2 AS tipoCliente)

DIM AuxNumReg AS INTEGER
DIM NumActual AS INTEGER
DIM Cliente AS tipoCliente


 NumReg = 0
 AuxNumReg = 0
 DO
  AuxNumReg = AuxNumReg + 1
  GET #1, AuxNumReg, Cliente
  IF (Cliente.dni = cliente2.dni) THEN
   NumReg = AuxNumReg
  END IF
 LOOP UNTIL EOF(1)
 BuscaDni% = NumReg
END FUNCTION

FUNCTION BuscaFicha% (cliente2 AS tipoCliente)

DIM AuxNumReg AS INTEGER
DIM NumActual AS INTEGER
DIM Cliente AS tipoCliente


 NumReg = 0
 AuxNumReg = 0
 DO
  AuxNumReg = AuxNumReg + 1
  GET #1, AuxNumReg, Cliente
  IF (Cliente.nombre = cliente2.nombre) THEN
   NumReg = AuxNumReg
  END IF
 LOOP UNTIL EOF(1)
 BuscaFicha% = NumReg
END FUNCTION

FUNCTION BuscaPass% (cliente2 AS tipoCliente)

DIM AuxNumReg AS INTEGER
DIM NumReg AS INTEGER
DIM Cliente AS tipoCliente


 NumReg = 0
 AuxNumReg = 0
 DO
  AuxNumReg = AuxNumReg + 1
  GET #1, AuxNumReg, Cliente
  IF (Cliente.pass = cliente2.pass) THEN
   NumReg = AuxNumReg
  END IF
 LOOP UNTIL EOF(1)
 BuscaPass% = NumReg
END FUNCTION

SUB CambiaPass

 DIM AuxCliente3 AS tipoCliente

 MuestraFicha (RegUsuari)
 LOCATE 1, 1: PRINT STRING$(80, " ")
 LOCATE 1, 30: PRINT "CAMBIO DE PASSORD"
 COLOR 14, 0: LOCATE 3, 28: PRINT " El PASSORD actual es"; AuxCliente.pass
 COLOR 30
 LOCATE 4, 28: PRINT " จ Desea cambiarlo ?(S/N) "
 DO
  op$ = UCASE$(INPUT$(1))
 LOOP UNTIL (op$ = "S" OR op$ = "N")
 COLOR 15, 0: LOCATE 4, 1: PRINT STRING$(80, "ฒ")
 IF op$ = "N" THEN EXIT SUB
 DO
  COLOR 13, 9: LOCATE 15, 17: PRINT "Password: "
  LOCATE 15, 27: PRINT STRING$(38, " ")
  COLOR 15: AuxCliente3.pass = PidePassword(15, 27, 3)
  acertado = BuscaPass(AuxCliente3)
  IF AuxCliente.pass = AuxCliente3.pass THEN acertado = 0
  IF (acertado > 0) THEN
   COLOR 26, 0: LOCATE 4, 34: PRINT "PassWord repetido"
   SOUND 205, 2
   op$ = UCASE$(INPUT$(1))
   COLOR 9: LOCATE 4, 30: PRINT STRING$(20, " ")
  END IF
 LOOP UNTIL acertado = 0
 AuxCliente.pass = AuxCliente3.pass
 PUT #1, RegUsuari, AuxCliente
END SUB

SUB Consulta
 
 DIM op AS INTEGER
 DIM RegTotal AS INTEGER
 DIM ch AS STRING * 1
 DIM AuxCliente3 AS tipoCliente
 DIM AuxNombre AS STRING * 50


 CLS
 OPEN "Banco.dat" FOR RANDOM AS #1 LEN = LEN(AuxCliente)
 RegTotal = LOF(1) / LEN(AuxCliente)
 IF RegTotal = 0 THEN
  COLOR , 9
  CLS
  COLOR 0, 15
  LOCATE 11, 10: PRINT "ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออป"
  LOCATE 12, 10: PRINT "บ  จ Qu vas a consultar si no hay nada para consultar ?  บ"
  LOCATE 13, 10: PRINT "ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ"
  COLOR , 0
  LOCATE 14, 10: PRINT "                                                           "
  FOR i% = 12 TO 14
   LOCATE i%, 69
   PRINT "  "
  NEXT i%
  SOUND 250, 2
  ch = UCASE$(INPUT$(1))
  CLOSE
  EXIT SUB
 END IF
 op = MenuConsulta
 CLS
 Fondo
 COLOR 15, 9: LOCATE 1, 1: PRINT STRING$(80, " ");
 LOCATE 1, 33: PRINT "CONSULTAS"
 SELECT CASE op
  CASE 1
   COLOR 10: LOCATE 7, 17: PRINT "Nombre: "
   COLOR 15
   AuxNombre = PideCadena$(7, 25, 40)
   AuxCliente3.nombre = AuxNombre
   RegActual = BuscaFicha(AuxCliente3)
   IF RegActual = 0 THEN
    COLOR 9, 0
    LOCATE 4, 30: PRINT "Ficha no existe"
    ch = UCASE$(INPUT$(1))
   ELSE
    MuestraFicha (RegActual)
    COLOR 15, 9: LOCATE 1, 1: PRINT STRING$(80, " ");
    LOCATE 1, 33: PRINT "CONSULTA"
    DO
     ch = UCASE$(INPUT$(1))
    LOOP UNTIL ch = CHR$(27)
   END IF
 
  CASE 2
   COLOR 10: LOCATE 9, 17: PRINT "D.N.I: "
   COLOR 15
   AuxNombre = PideDni$(9, 24, 9)
   AuxCliente3.dni = AuxNombre
   RegActual = BuscaDni(AuxCliente3)
   IF RegActual = 0 THEN
    COLOR 9, 0
    LOCATE 4, 30: PRINT "Ficha no existe"
    ch = UCASE$(INPUT$(1))
   ELSE
    MuestraFicha (RegActual)
    COLOR 15, 9: LOCATE 1, 1: PRINT STRING$(80, " ");
    LOCATE 1, 33: PRINT "CONSULTA"
    DO
     ch = UCASE$(INPUT$(1))
    LOOP UNTIL ch = CHR$(27)
   END IF

  CASE 3
   DO
    COLOR 10: LOCATE 13, 17: PRINT "Cuenta:         "
    COLOR 15
    AuxNum% = PideNum%(13, 28, 4)
   LOOP UNTIL AuxNum% > 0
   AuxCliente3.Cuenta = AuxNum%
   RegActual = BuscaCuenta%(AuxCliente3)
   IF RegActual = 0 THEN
    COLOR 9, 0
    LOCATE 4, 30: PRINT "Ficha no existe"
    ch = UCASE$(INPUT$(1))
   ELSE
    MuestraFicha (RegActual)
    COLOR 15, 9: LOCATE 1, 1: PRINT STRING$(80, " ");
    LOCATE 1, 33: PRINT "CONSULTA"
    DO
     ch = UCASE$(INPUT$(1))
    LOOP UNTIL ch = CHR$(27)
   END IF
  
   
    
 END SELECT
 CLOSE
END SUB

FUNCTION CreaCuenta%

DIM RegTotal AS INTEGER
DIM Cliente AS tipoCliente
DIM valido AS INTEGER
DIM Cuenta AS INTEGER

 RegTotal = LOF(1) / LEN(AuxCliente)
 OPEN "Banco.dat" FOR RANDOM AS #2 LEN = LEN(AuxCliente)
 IF RegTotal = 0 THEN
  Cuenta = 1
 ELSE
  GET #2, RegTotal, Cliente
  Cuenta = Cliente.Cuenta + 1
 END IF
 CreaCuenta% = Cuenta
 CLOSE #2
END FUNCTION

FUNCTION CreaPass%

DIM RegTotal AS INTEGER
DIM RegActual AS INTEGER
DIM pass AS INTEGER
DIM Cliente AS tipoCliente
DIM valido AS INTEGER

 RegTotal = LOF(1) / LEN(AuxCliente)
 OPEN "Banco.dat" FOR RANDOM AS #2 LEN = LEN(AuxCliente)
 RANDOMIZE TIMER
 DO
  RegActual = 0
  valido = 1
  pass = INT(RND * 999)
  DO
   RegActual = RegActual + 1
   GET 2, RegActual, Cliente
   IF Cliente.pass = pass THEN valido = 0
  LOOP UNTIL EOF(2)
 LOOP UNTIL (valido = 1 AND pass > 99)
CLOSE #2
CreaPass% = pass
END FUNCTION

SUB Extraer

 COLOR , 9
 CLS
 GET #1, RegUsuari, AuxCliente
 COLOR 0, 15
 LOCATE 9, 8: PRINT "ษออออออออออออออออออออออออออออออออออออออออออออออออออป"
 LOCATE 10, 8: PRINT "บ                                                  บ"
 LOCATE 11, 8: PRINT "บ                                                  บ"
 LOCATE 12, 8: PRINT "บ                                                  บ"
 LOCATE 13, 8: PRINT "ศออออออออออออออออออออออออออออออออออออออออออออออออออผ"
 COLOR , 0
 LOCATE 14, 9: PRINT "                                                    "
 FOR i% = 10 TO 14
  LOCATE i%, 60: PRINT " "
 NEXT i%
 COLOR 0, 15
 LOCATE 10, 9: PRINT " Tiene "; AuxCliente.Saldo; " Euro(s) disponibles."
 LOCATE 12, 9: PRINT " Introduce los Euros que va a extraer: "
 Cantidad = PideLong&(12, 48, 6)
 IF AuxCliente.Saldo < Cantidad THEN
  CLS
  LOCATE 12, 10: PRINT "No hay suficiente saldo"
  LOCATE 13, 10: PRINT "El saldo disponible es de:"; AuxCliente.Saldo; " Euro(s)"
  op$ = INPUT$(1)
 ELSE
  AuxCliente.Saldo = AuxCliente.Saldo - Cantidad
  PUT #1, RegUsuari, AuxCliente
  operacion.Cuenta = AuxCliente.Cuenta
  operacion.Opera = 2 'Extraccion
  operacion.Fecha = DATE$
  operacion.Hora = TIME$
  operacion.Saldo = AuxCliente.Saldo
  operacion.Cantidad = Cantidad
  OPEN "Opera.dat" FOR APPEND AS #2
  PRINT #2, operacion.Cuenta
  PRINT #2, operacion.Opera
  PRINT #2, operacion.Fecha
  PRINT #2, operacion.Hora
  PRINT #2, operacion.Saldo
  PRINT #2, operacion.Cantidad
  CLOSE #2
 END IF
END SUB

SUB Fondo

DIM i AS INTEGER

 COLOR 15, 0
 CLS
 FOR i = 1 TO 24
  LOCATE i, 1: PRINT STRING$(80, "ฒ")
 NEXT i
 COLOR 15, 9
 LOCATE 6, 15: PRINT "ษอออออออออออออออออออออออออออออออออออออออออออออออออป"
 LOCATE 7, 15: PRINT "บ                                                 บ"
 LOCATE 8, 15: PRINT "บ                                                 บ"
 LOCATE 9, 15: PRINT "บ                                                 บ"
 LOCATE 10, 15: PRINT "บ                                                 บ"
 LOCATE 11, 15: PRINT "บ                                                 บ"
 LOCATE 12, 15: PRINT "บ                                                 บ"
 LOCATE 13, 15: PRINT "บ                                                 บ"
 LOCATE 14, 15: PRINT "บ                                                 บ"
 LOCATE 15, 15: PRINT "บ                                                 บ"
 LOCATE 16, 15: PRINT "บ                                                 บ"
 LOCATE 17, 15: PRINT "บ                                                 บ"
 LOCATE 18, 15: PRINT "บ                                                 บ"
 LOCATE 19, 15: PRINT "บ                                                 บ"
 LOCATE 20, 15: PRINT "ศอออออออออออออออออออออออออออออออออออออออออออออออออผ"
 COLOR , 0
 LOCATE 21, 16: PRINT "ฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐ"
 FOR i% = 7 TO 21
  LOCATE i%, 66
  PRINT "ฐฐ"
 NEXT i%
 COLOR 15, 7
 LOCATE 1, 1
 PRINT STRING$(80, " ")
 COLOR 15, 1
 LOCATE 7, 17: PRINT "Nombre:"
 LOCATE 9, 17: PRINT "D.N.I:"
 LOCATE 11, 17: PRINT "Direcciขn:"
 LOCATE 13, 17: PRINT "Cuenta:"
 LOCATE 15, 17: PRINT "Password:"
 LOCATE 17, 17: PRINT "Saldo:"
 LOCATE 19, 17: PRINT "Estado:"
END SUB

SUB Ingreso

 DIM Cantidad AS LONG

 COLOR , 9
 CLS
 COLOR 0, 15
 GET #1, RegUsuari, AuxCliente
 LOCATE 9, 8: PRINT "ษอออออออออออออออออออออออออออออออออออออออออออออออออออป"
 LOCATE 10, 8: PRINT "บ                                                   บ"
 LOCATE 11, 8: PRINT "บ                                                   บ"
 LOCATE 12, 8: PRINT "บ                                                   บ"
 LOCATE 13, 8: PRINT "ศอออออออออออออออออออออออออออออออออออออออออออออออออออผ"
 LOCATE 10, 9: PRINT " Tiene "; AuxCliente.Saldo; " Euro(s)a disponibles."
 LOCATE 12, 9: PRINT " Introduce los Euros que va a ingresar: "
 COLOR , 0
 LOCATE 14, 9: PRINT "                                                     "
 FOR i% = 10 TO 14
  LOCATE i%, 61: PRINT " "
 NEXT i%
 COLOR 0, 15
 Cantidad = PideLong&(12, 48, 6)
 AuxCliente.Saldo = AuxCliente.Saldo + Cantidad
 PUT #1, RegUsuari, AuxCliente
 OPEN "Opera.dat" FOR APPEND AS #2
 operacion.Cuenta = AuxCliente.Cuenta
 operacion.Opera = 1 'Ingreso
 operacion.Fecha = DATE$
 operacion.Hora = TIME$
 operacion.Saldo = AuxCliente.Saldo
 operacion.Cantidad = Cantidad
 PRINT #2, operacion.Cuenta
 PRINT #2, operacion.Opera
 PRINT #2, operacion.Fecha
 PRINT #2, operacion.Hora
 PRINT #2, operacion.Saldo
 PRINT #2, operacion.Cantidad
 CLOSE #2
END SUB

SUB listado

 DIM Nreg AS INTEGER
 DIM RegTotal AS INTEGER

 OPEN "Banco.dat" FOR RANDOM AS #1 LEN = LEN(AuxCliente)
 RegTotal = LOF(1) / LEN(AuxCliente)
 IF RegTotal = 0 THEN
  COLOR , 9
  CLS
  COLOR 0, 15
  LOCATE 11, 21: PRINT "ษอออออออออออออออออออออออออออออออออออออออออออออป"
  LOCATE 12, 21: PRINT "บ จQu vas a listar si no hay nada que listar?บ"
  LOCATE 13, 21: PRINT "ศอออออออออออออออออออออออออออออออออออออออออออออผ"
  COLOR , 0
  LOCATE 14, 22: PRINT "                                              "
  FOR i% = 12 TO 14
   LOCATE i%, 68
   PRINT "  "
  NEXT i%
  SOUND 250, 2
  op$ = UCASE$(INPUT$(1))
  EXIT SUB
 END IF
 Nreg = 0
 LOCATE 1, 1
 PRINT STRING$(80, " ")
 LOCATE 1, 38: PRINT "LISTADO"
 LOCATE 23, 1: PRINT STRING$(80, " ")
 LOCATE 23, 10: PRINT "[+] = Sgte ficha         [-] = Ant ficha             ESC = SALIR"

 op$ = "A"
 Nreg = 1
 DO
  IF op$ = "+" THEN
   IF Nreg < RegTotal THEN
    Nreg = Nreg + 1
   ELSE
    SOUND 250, 2
   END IF
  END IF
  IF op$ = "-" THEN
   IF Nreg > 1 THEN
    Nreg = Nreg - 1
   ELSE
    SOUND 250, 2
   END IF
  END IF
  LOCATE 5, 16: PRINT Nreg; "/"; RegTotal
  MuestraFicha (Nreg)
  COLOR , 0
  LOCATE 5, 26: PRINT "ฐ"
  COLOR 15, 9
  LOCATE 4, 15: PRINT "ฺฤฤฤฤฤฤฤฤฤฟ"
  LOCATE 5, 15: PRINT "ณ         ณ"
  LOCATE 6, 15: PRINT "ฦอออออออออฯ"
  LOCATE 5, 16: PRINT Nreg; "/"; RegTotal
  LOCATE 23, 1: PRINT "  [ESC] -> SALIR    -    [+] -> Siguiente Ficha    -    [-] -> Anterior ficha   "
  COLOR 0, 15: LOCATE 1, 35: PRINT "LISTADO"
  DO
   op$ = UCASE$(INPUT$(1))
  LOOP UNTIL (op$ = "+" OR op$ = "-" OR op$ = CHR$(27))
 LOOP UNTIL (op$ = CHR$(27))
END SUB

SUB MenuBanco

 COLOR , 9
 CLOSE #1
 CLS
 LOCATE 1, 1
 COLOR 4, 15
 PRINT STRING$(80, " ");
 LOCATE 1, 33: PRINT "MODO BANCO"
 COLOR 0, 15
 LOCATE 7, 28: PRINT "ษออออออออออออออออออออออป"
 LOCATE 8, 28: PRINT "บ 1.- Altas de cuentas บ"
 LOCATE 9, 28: PRINT "บ                      บ"
 LOCATE 10, 28: PRINT "บ 2.- Bajas de cuentas บ"
 LOCATE 11, 28: PRINT "บ                      บ"
 LOCATE 12, 28: PRINT "บ 3.- Modificaciones   บ"
 LOCATE 13, 28: PRINT "บ                      บ"
 LOCATE 14, 28: PRINT "บ 4.- Listado          บ"
 LOCATE 15, 28: PRINT "บ                      บ"
 LOCATE 16, 28: PRINT "บ 5.- Consulta         บ"
 LOCATE 17, 28: PRINT "บ                      บ"
 LOCATE 18, 28: PRINT "บ 6.- Reorganizar      บ"
 LOCATE 19, 28: PRINT "บ                      บ"
 LOCATE 20, 28: PRINT "บ 0.- SALIR            บ"
 LOCATE 21, 28: PRINT "ศออออออออออออออออออออออผ"
 COLOR , 0
 LOCATE 22, 29: PRINT "                        "
 FOR i% = 8 TO 22
  LOCATE i%, 52: PRINT " "
 NEXT i%

 DO
  op$ = UCASE$(INPUT$(1))
 LOOP UNTIL (op$ = "1" OR op$ = "2" OR op$ = "3" OR op$ = "4" OR op$ = "5" OR op$ = "6" OR op$ = "0")
 posi% = VAL(op$)
END SUB

FUNCTION MenuConsulta%

 DIM opci AS INTEGER

 COLOR , 9
 CLS
 COLOR 0, 15
 LOCATE 8, 20: PRINT "ษออออออออออออออออออออออออออออออป"
 LOCATE 9, 20: PRINT "บ                              บ"
 LOCATE 10, 20: PRINT "บ   1.- Consulta por NOMBRE    บ"
 LOCATE 11, 20: PRINT "บ                              บ"
 LOCATE 12, 20: PRINT "บ   2.- Consulta por D.N.I     บ"
 LOCATE 13, 20: PRINT "บ                              บ"
 LOCATE 14, 20: PRINT "บ   3.- Consulta por CUENTA    บ"
 LOCATE 15, 20: PRINT "บ                              บ"
 LOCATE 16, 20: PRINT "บ   0.- Volver al menฃ         บ"
 LOCATE 17, 20: PRINT "บ                              บ"
 LOCATE 18, 20: PRINT "ศออออออออออออออออออออออออออออออผ"
 COLOR , 0
 LOCATE 19, 21: PRINT "                                 "
 FOR i% = 9 TO 18
  LOCATE i%, 52: PRINT "  "
 NEXT i%
 DO
  op$ = INPUT$(1)
 LOOP UNTIL (op$ = "1" OR op$ = "2" OR op$ = "3" OR op$ = "0")
 opci = VAL(op$)
 MenuConsulta% = opci


END FUNCTION

SUB MenuPrincipal

 COLOR , 9
 CLS
 COLOR 4, 15
 LOCATE 1, 1
 PRINT STRING$(80, " "): COLOR 4: LOCATE 1, 32: PRINT "Menฃ Principal"
 COLOR 0, 15
 LOCATE 9, 28: PRINT "ษออออออออออออออออออออออออป"
 LOCATE 10, 28: PRINT "บ                        บ"
 LOCATE 11, 28: PRINT "วฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤถ"
 LOCATE 12, 28: PRINT "บ                        บ"
 LOCATE 13, 28: PRINT "วฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤถ"
 LOCATE 14, 28: PRINT "บ                        บ"
 LOCATE 15, 28: PRINT "ศออออออออออออออออออออออออผ"
 LOCATE 10, 35: PRINT "1.- BANCO"
 LOCATE 12, 35: PRINT "2.- USUARIO"
 LOCATE 14, 35: PRINT "0.- SALIR"
 COLOR , 0
 LOCATE 16, 29: PRINT "                           "
 FOR i% = 10 TO 16
  LOCATE i%, 54: PRINT "  "
 NEXT i%

 DO
  op$ = UCASE$(INPUT$(1))
 LOOP UNTIL (op$ = "1" OR op$ = "2" OR op$ = "0")
 modo = VAL(op$)





END SUB

FUNCTION MenuUsuario%

 COLOR 0, 9
 CLS
 COLOR 4, 15
 LOCATE 1, 1: PRINT STRING$(80, " ")
 LOCATE 1, 35: PRINT "MODO USUARIO"
 LOCATE 23, 1: PRINT STRING$(80, " ")
 LOCATE 23, 3: PRINT "USUARIO: "; AuxCliente.nombre
 COLOR , 0
 LOCATE 19, 26: PRINT "                                     "
 FOR i% = 7 TO 19
  LOCATE i%, 62: PRINT "  "
 NEXT i%
 COLOR 0, 15
 LOCATE 6, 25: PRINT "ษอออออออออออออออออออออออออออออออออออป"
 LOCATE 7, 25: PRINT "บ      1- INGRESAR DINERO           บ"
 LOCATE 8, 25: PRINT "บ                                   บ"
 LOCATE 9, 25: PRINT "บ      2- EXTRAER DINERO            บ"
 LOCATE 10, 25: PRINT "บ                                   บ"
 LOCATE 11, 25: PRINT "บ      3- CONSULTA DE OPERACIONES   บ"
 LOCATE 12, 25: PRINT "บ                                   บ"
 LOCATE 13, 25: PRINT "บ      4- CONSULTA SALDO            บ"
 LOCATE 14, 25: PRINT "บ                                   บ"
 LOCATE 15, 25: PRINT "บ      5- CAMBIO DE CONTRASEฅA      บ"
 LOCATE 16, 25: PRINT "บ                                   บ"
 LOCATE 17, 25: PRINT "บ      0- SALIR                     บ"
 LOCATE 18, 25: PRINT "ศอออออออออออออออออออออออออออออออออออผ"
 DO
  op$ = UCASE$(INPUT$(1))
 LOOP UNTIL (op$ = "1" OR op$ = "2" OR op$ = "3" OR op$ = "4" OR op$ = "5" OR op$ = "0")
 posi% = VAL(op$)
 MenuUsuario% = posi%

END FUNCTION

SUB Modificacion

 DIM AuxCliente2 AS tipoCliente
 DIM RegActual AS INTEGER
 DIM AuxReg AS INTEGER
 DIM cadena AS STRING
 DIM repe AS INTEGER


 OPEN "BANCO.DAT" FOR RANDOM AS #1 LEN = LEN(AuxCliente)
 RegActual = LOF(1) / LEN(AuxCliente)
 IF RegActual = 0 THEN
  COLOR , 9
  CLS
  COLOR 0, 15
  LOCATE 11, 10: PRINT "ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออป"
  LOCATE 12, 10: PRINT "บ   จ Qu vas a modificar si no hay nada que modificar ?  บ"
  LOCATE 13, 10: PRINT "ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ"
  COLOR , 0
  LOCATE 14, 10: PRINT "                                                           "
  FOR i% = 12 TO 14
   LOCATE i%, 69
   PRINT "  "
  NEXT i%
  SOUND 250, 2
  op$ = UCASE$(INPUT$(1))
  EXIT SUB
 END IF
 CLS
 Fondo
 LOCATE 1, 1: PRINT STRING$(80, " "): LOCATE 1, 33: PRINT "MODIFICACIONES"
 COLOR 10: LOCATE 7, 17: PRINT "Nombre: "
 COLOR 15: AuxCliente.nombre = PideCadena$(7, 25, 40)
 RegActual = BuscaFicha(AuxCliente)
 IF RegActual = 0 THEN
  CLS
  COLOR 0, 15
  LOCATE 11, 30: PRINT "ษอออออออออออออออป"
  LOCATE 12, 30: PRINT "บFicha no existeบ"
  LOCATE 13, 30: PRINT "ศอออออออออออออออผ"
  COLOR , 0
  LOCATE 14, 31: PRINT "                 "
  FOR i% = 12 TO 14
   LOCATE i%, 47: PRINT " "
  NEXT i%
  SOUND 200, 3
  op$ = UCASE$(INPUT$(1))
 END IF
 IF RegActual > 0 THEN
  DO
   GET #1, RegActual, AuxCliente
   MuestraFicha (RegActual)
   LOCATE 6, 15: PRINT RegActual
   LOCATE 1, 1: PRINT STRING$(80, " "): LOCATE 1, 33: PRINT "MODIFICACIONES"
   LOCATE 20, 31: PRINT "[ ESC -> SALIR ]"
   COLOR 13
   LOCATE 7, 17: PRINT "N"
   LOCATE 11, 17: PRINT "D"
   LOCATE 15, 17: PRINT "P"
   DO
    op$ = UCASE$(INPUT$(1))
   LOOP UNTIL (op$ = "N" OR op$ = "D" OR op$ = "P" OR op$ = CHR$(27))
   SELECT CASE op$
   CASE "N"
    LOCATE 7, 17: PRINT "Nombre: "
    LOCATE 7, 25: PRINT STRING$(40, " ")
    COLOR 15: cadena = PideCadena$(7, 25, 40)
    AuxCliente2.nombre = cadena
    repe = BuscaFicha%(AuxCliente2)
    IF repe = 0 THEN AuxCliente.nombre = cadena
    IF repe = RegActual THEN
     AuxCliente.nombre = cadena
    ELSE
     IF repe > 0 THEN
      COLOR 15: LOCATE 4, 30: PRINT "Nombre Repetido"
      SOUND 250, 3
      op$ = INPUT$(1)
      EXIT SUB
     END IF
    END IF
   CASE "D"
    LOCATE 11, 17: PRINT "Direcciขn: "
    LOCATE 11, 28:  PRINT STRING$(37, " ")
    COLOR 15: AuxCliente.dire = PideDire$(11, 28, 37)
   CASE "P"
    OPEN "Banco.dat" FOR RANDOM AS #2 LEN = LEN(AuxCliente)
    DO
     COLOR 13, 9: LOCATE 15, 17: PRINT "Password: "
     LOCATE 15, 27: PRINT STRING$(38, " ")
     COLOR 15: AuxCliente.pass = PidePassword(15, 27, 3)
     AuxReg = 0
     repe = 0
     DO
      AuxReg = AuxReg + 1
      GET #2, AuxReg, AuxCliente2
      IF AuxCliente.pass = AuxCliente2.pass THEN repe = 1
     LOOP UNTIL EOF(2)
     IF repe = 1 THEN
      COLOR 26, 7: LOCATE 4, 30: PRINT "PassWord repetido"
      SOUND 205, 2
      op$ = UCASE$(INPUT$(1))
      COLOR 9: LOCATE 4, 30: PRINT STRING$(20, " ")
     END IF
    LOOP UNTIL repe = 0
   CLOSE #2
   END SELECT
   PUT #1, RegActual, AuxCliente
  LOOP UNTIL (op$ = CHR$(27))
 END IF


END SUB

SUB MuestraFicha (reg%)

 DIM Cliente AS tipoCliente

 CLS
 GET #1, reg%, Cliente
 Fondo
 LOCATE 7, 25: PRINT Cliente.nombre
 LOCATE 9, 24: PRINT Cliente.dni
 LOCATE 11, 28: PRINT Cliente.dire
 LOCATE 13, 24: PRINT Cliente.Cuenta
 LOCATE 15, 26: PRINT Cliente.pass
 LOCATE 17, 23: PRINT (STRING$(30, " "))
 LOCATE 17, 23: PRINT Cliente.Saldo; "Euro(s)"
 LOCATE 19, 24
 IF Cliente.estado = "1" THEN
  PRINT " ALTA"
 ELSE
  PRINT " BAJA"
 END IF
END SUB

SUB MuestraOpera

 DIM Nreg AS INTEGER

 COLOR 15, 9
 CLS
 COLOR 0, 15: LOCATE 1, 1: PRINT STRING$(80, " ")
 LOCATE 1, 28: PRINT "OPERACIONES REALIZADAS"
 COLOR 15, 9
 LOCATE 2, 2: PRINT "CUENTA: "; AuxCliente.Cuenta; "|| USUARIO: "; AuxCliente.nombre
 LOCATE 3, 1: PRINT STRING$(80, "ฤ")
 LOCATE 4, 1: PRINT "ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤยฤฤฤฤฤฤฤฤฤฤฤฤฤยฤฤฤฤฤฤฤฤฤฤฤฤยฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤยฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ"
 LOCATE 5, 1: PRINT "    OPERACION    ณ    FECHA    ณ    HORA    ณ    CANTIDAD    ณ    SALDO ACTUAL"
 LOCATE 6, 1: PRINT "ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤมฤฤฤฤฤฤฤฤฤฤฤฤฤมฤฤฤฤฤฤฤฤฤฤฤฤมฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤมฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ"
 OPEN "opera.dat" FOR APPEND AS #2
 CLOSE #2
 OPEN "I", #2, "Opera.dat"
 Nreg = LOF(2) / LEN(operacion)
 IF Nreg = 0 THEN
  CLOSE #2
  COLOR , 9
  CLS
  COLOR 0, 15
  LOCATE 10, 10: PRINT "ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออป"
  LOCATE 11, 10: PRINT "บ      Ningฃn cliente ha realizado alguna operacion       บ"
  LOCATE 12, 10: PRINT "ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ"
  COLOR , 0
  LOCATE 13, 11: PRINT "                                                           "
  FOR i% = 11 TO 13
   LOCATE i%, 69: PRINT " "
  NEXT i%
  SOUND 250, 2
  op$ = INPUT$(1)
  EXIT SUB
 END IF
 i% = 7
 DO
  INPUT #2, operacion.Cuenta
  INPUT #2, operacion.Opera
  INPUT #2, operacion.Fecha
  INPUT #2, operacion.Hora
  INPUT #2, operacion.Saldo
  INPUT #2, operacion.Cantidad
  LOCATE i%, 5
  IF operacion.Cuenta = AuxCliente.Cuenta THEN
   IF operacion.Opera = 1 THEN
    COLOR 14: PRINT "INGRESO"
   ELSE
    COLOR 12: PRINT "EXTRACCION"
   END IF
   LOCATE i%, 21: PRINT operacion.Fecha
   LOCATE i%, 36: PRINT operacion.Hora
   LOCATE i%, 50: PRINT operacion.Cantidad
   LOCATE i%, 65: PRINT operacion.Saldo
   i% = i% + 1
   IF i% > 22 THEN
    COLOR 14, 0
    LOCATE 23, 15: PRINT "Pulse una tecla para continuar con el listado"
    op$ = INPUT$(1)
    COLOR , 9
    FOR i% = 7 TO 23
     LOCATE i%, 1: PRINT STRING$(80, " ")
    NEXT i%
    i% = 7
   END IF
  END IF
  LOOP UNTIL EOF(2)





 op$ = INPUT$(1)

 CLOSE #2
END SUB

SUB NoPass

 COLOR , 9
 CLS
 COLOR 0, 15
 LOCATE 11, 15: PRINT "ษอออออออออออออออออออออออออออออออออออออออออออออป"
 LOCATE 12, 15: PRINT "บ   Password no v lido, PROGRAM ABORTED :-)   บ"
 LOCATE 13, 15: PRINT "ศอออออออออออออออออออออออออออออออออออออออออออออผ"
 COLOR , 0
 LOCATE 14, 16: PRINT "                                               "
 FOR i% = 12 TO 14
 LOCATE i%, 62: PRINT " "
 NEXT i%
 SOUND 250, 2: SOUND 100, 2: SOUND 300, 2: SOUND 500, 2
 SOUND 500, 2: SOUND 300, 2: SOUND 100, 2: SOUND 250, 2
 op$ = INPUT$(1)
END SUB

FUNCTION PideCadena$ (x, y, max AS INTEGER)

DIM char AS STRING * 1
DIM conta AS INTEGER
DIM cadena AS STRING
 
 conta = 0
 DO
  char = UCASE$(INPUT$(1))
 
  IF (char = "ค") THEN char = "ฅ"
  LOCATE x, y
  SELECT CASE char
        CASE "A" TO "Z", "ฅ", ".", ",", "-", "(", ")", "_", "?", "จ", "!", "ญ"
        IF (conta < max) THEN
                conta = conta + 1
                cadena = cadena + char
                PRINT cadena
        ELSE
                SOUND 350, 2
        END IF
        CASE " "
                IF (conta > 0) THEN
                        conta = conta + 1
                        cadena = cadena + char
                        PRINT cadena
                ELSE
                        SOUND 350, 2
                END IF
        CASE CHR$(8)
                IF conta > 0 THEN
                        conta = conta - 1
                        cadena = LEFT$(cadena, conta)
                        PRINT cadena; (STRING$(max - conta, " "))
                ELSE
                        SOUND 350, 2
                END IF
  END SELECT
 LOOP UNTIL ((char = CHR$(13)) AND (conta > 0))
 PideCadena$ = cadena$

END FUNCTION

FUNCTION PideClave$ (x, y, max AS INTEGER)
DIM char AS STRING * 1
DIM conta AS INTEGER
DIM cadena AS STRING

 conta = 0
 DO
  char = UCASE$(INPUT$(1))

  IF (char = "ค") THEN char = "ฅ"
  LOCATE x, y
  SELECT CASE char
        CASE "A" TO "Z", "ฅ", ".", ",", "-", "(", ")", "_", "?", "จ", "!", "ญ"
        IF (conta < max) THEN
                conta = conta + 1
                cadena = cadena + char
                PRINT STRING$(conta, "*")
        ELSE
                SOUND 350, 2
        END IF
        CASE " "
                IF (conta > 0) THEN
                        conta = conta + 1
                        cadena = cadena + char
                        PRINT STRING$(conta, "*")
                ELSE
                        SOUND 350, 2
                END IF
        CASE CHR$(8)
                IF conta > 0 THEN
                        conta = conta - 1
                        cadena = LEFT$(cadena, conta)
                        PRINT STRING$(conta + 1, " ")
                        LOCATE x, y
                        PRINT STRING$(conta, "*")
                ELSE
                        SOUND 350, 2
                END IF
  END SELECT
 LOOP UNTIL ((char = CHR$(13)) AND (conta > 0))
 PideClave$ = cadena$
END FUNCTION

FUNCTION PideDire$ (x, y, max AS INTEGER)

DIM char AS STRING * 1
DIM conta AS INTEGER
DIM cadena AS STRING

 conta = 0
 DO
  char = UCASE$(INPUT$(1))

  IF (char = "ค") THEN char = "ฅ"
  LOCATE x, y
  SELECT CASE char
        CASE "A" TO "Z", "ฅ", "'", "ง", "ฆ", "0" TO "9", "/"
        IF (conta < max) THEN
                conta = conta + 1
                cadena = cadena + char
                PRINT cadena
        ELSE
                SOUND 350, 2
        END IF
        CASE " "
                IF (conta > 0) THEN
                        conta = conta + 1
                        cadena = cadena + char
                        PRINT cadena
                ELSE
                        SOUND 350, 2
                END IF
        CASE CHR$(8)
                IF conta > 0 THEN
                        conta = conta - 1
                        cadena = LEFT$(cadena, conta)
                        PRINT cadena; (STRING$(max - conta, " "))
                ELSE
                        SOUND 350, 2
                END IF
  END SELECT
 LOOP UNTIL ((char = CHR$(13)) AND (conta > 0))
 PideDire$ = cadena$

END FUNCTION

FUNCTION PideDni$ (x, y, max AS INTEGER)

DIM char AS STRING * 1
DIM conta AS INTEGER
DIM cadena AS STRING

 conta = 0
 DO
  char = UCASE$(INPUT$(1))

 IF (char = "ค") THEN char = "ฅ"
  LOCATE x, y
  SELECT CASE char
        CASE "0" TO "9"
        IF (conta < 8) THEN
                conta = conta + 1
                cadena = cadena + char
                PRINT cadena
        ELSE
                SOUND 350, 2
        END IF
        CASE "A" TO "Z", "ฅ"
                IF (conta = 8) THEN
                        conta = conta + 1
                        cadena = cadena + char
                        PRINT cadena
                ELSE
                        SOUND 350, 2
                END IF
        CASE CHR$(8)
                IF conta > 0 THEN
                        conta = conta - 1
                        cadena = LEFT$(cadena, conta)
                        PRINT cadena; (STRING$(max - conta, " "))
                ELSE
                        SOUND 350, 2
                END IF
  END SELECT
 LOOP UNTIL ((char = CHR$(13)) AND (conta > 8))
 PideDni$ = cadena$
END FUNCTION

FUNCTION PideLong& (x, y, max AS INTEGER)

DIM char AS STRING * 1
DIM conta AS INTEGER
DIM cadena AS STRING
DIM Num AS LONG

 conta = 0
 DO
  char = INPUT$(1)

  LOCATE x, y
  SELECT CASE char
        CASE "0" TO "9"
        IF (conta < max) THEN
                conta = conta + 1
                cadena = cadena + char
                PRINT cadena
        ELSE
                SOUND 350, 2
        END IF
        CASE CHR$(8)
                IF conta > 0 THEN
                        conta = conta - 1
                        cadena = LEFT$(cadena, conta)
                        PRINT cadena; (STRING$(max - conta, " "))
                ELSE
                        SOUND 350, 2
                END IF
  END SELECT
 LOOP UNTIL ((char = CHR$(13)) AND (conta <= max))
 Num = VAL(cadena$)
 PideLong& = Num

END FUNCTION

FUNCTION PideNum% (x, y, max AS INTEGER)

DIM char AS STRING * 1
DIM conta AS INTEGER
DIM cadena AS STRING
DIM Num AS INTEGER

 conta = 0
 DO
  char = INPUT$(1)

  LOCATE x, y
  SELECT CASE char
        CASE "0" TO "9"
        IF (conta < max) THEN
                conta = conta + 1
                cadena = cadena + char
                PRINT cadena
        ELSE
                SOUND 350, 2
        END IF
        CASE CHR$(8)
                IF conta > 0 THEN
                        conta = conta - 1
                        cadena = LEFT$(cadena, conta)
                        PRINT cadena; (STRING$(max - conta, " "))
                ELSE
                        SOUND 350, 2
                END IF
  END SELECT
 LOOP UNTIL ((char = CHR$(13)) AND (conta <= max))
 Num = VAL(cadena$)
 PideNum% = Num
END FUNCTION

SUB pidepass

 COLOR , 9
 CLS
 COLOR , 0
 LOCATE 12, 2: PRINT "                                                                             "
 FOR i% = 10 TO 12
  LOCATE i%, 78: PRINT "  "
 NEXT i%

 COLOR 0, 15

 LOCATE 9, 1: PRINT "ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป"
 LOCATE 10, 1: PRINT "บ Introduce el PASSWORD del Supervisor:                                     บ"
 LOCATE 11, 1: PRINT "ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ"
 PasswordAux$ = PideClave$(10, 41, 35)
 IF PasswordAux$ = password THEN
  acertado = 1
 ELSE
  NoPass
  acertado = 0
 END IF
END SUB

FUNCTION PidePassword% (x, y, max AS INTEGER)

DIM char AS STRING * 1
DIM conta AS INTEGER
DIM cadena AS STRING
DIM Num AS INTEGER

 DO
 conta = 0
 cadena = ""
 LOCATE x, y: PRINT "   "
 DO
   char = UCASE$(INPUT$(1))

   IF (char = "ค") THEN char = "ฅ"
   LOCATE x, y
   SELECT CASE char
    CASE "0" TO "9"
     IF (conta < max) THEN
      conta = conta + 1
      cadena = cadena + char
      PRINT cadena
     ELSE
      SOUND 350, 2
     END IF
    CASE CHR$(8)
     IF conta > 0 THEN
      conta = conta - 1
      cadena = LEFT$(cadena, conta)
      PRINT cadena; (STRING$(max - conta, " "))
     ELSE
      SOUND 350, 2
     END IF
   END SELECT
  LOOP UNTIL ((char = CHR$(13)) AND (conta > 0))
  Num = VAL(cadena$)
 LOOP UNTIL (Num > 99)
 PidePassword% = Num
END FUNCTION

FUNCTION Reorganiza%
 DIM RegTotal AS INTEGER
 DIM RegActual AS INTEGER
 DIM AuxCliente2 AS tipoCliente
 DIM band AS INTEGER '0 No habia bajas, 1 habia alguna baja
 DIM RegQuitados AS INTEGER

 OPEN "BANCO.DAT" FOR RANDOM AS #1 LEN = LEN(AuxCliente)
 RegTotal = LOF(1) / LEN(AuxCliente)
 IF RegTotal < 1 THEN
  COLOR , 9
  CLS
  COLOR 0, 15
  LOCATE 10, 10: PRINT "ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออป"
  LOCATE 11, 10: PRINT "บ      Ahora mismo no queda ningฃn fichero de Alta        บ"
  LOCATE 12, 10: PRINT "ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ"
  COLOR , 0
  LOCATE 13, 11: PRINT "                                                           "
  FOR i% = 11 TO 13
   LOCATE i%, 69: PRINT " "
  NEXT i%
  SOUND 250, 2
  op$ = INPUT$(1)
  EXIT FUNCTION
 END IF
 band = 0
 RegQuitados = 0
 FOR RegActual = 1 TO RegTotal
  GET #1, RegActual, AuxCliente
  IF AuxCliente.estado = "0" THEN
   band = 1
   RegQuitados = RegQuitados + 1
   FOR i% = RegActual TO RegTotal
    GET #1, i% + 1, AuxCliente2
    PUT #1, i%, AuxCliente2
   NEXT i%
  END IF
 NEXT RegActual
 IF band = 1 THEN
  OPEN "REORGA.DAT" FOR RANDOM AS #2 LEN = LEN(AuxCliente)
  FOR i% = 1 TO RegTotal - RegQuitados
   GET #1, i%, AuxCliente
   PUT #2, i%, AuxCliente
  NEXT i%
  CLOSE 1
  KILL "Banco.dat"
  OPEN "Banco.dat" FOR RANDOM AS #1 LEN = LEN(AuxCliente)
  
  RegTotal = LOF(2) / LEN(AuxCliente)
  FOR i% = 1 TO RegTotal
   GET #2, i%, AuxCliente
   PUT #1, i%, AuxCliente
  NEXT i%
  CLOSE
  KILL "Reorga.dat"

 END IF
 CLOSE
 Reorganiza% = band

END FUNCTION

