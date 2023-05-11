* @ValidationCode : MjoxODc3Mzg0MzcxOlVURi04OjE2ODM2MTYwODkyNTI6SVRTUzotMTotMTo1OTE6MTpmYWxzZTpOL0E6UjIxX0FNUi4wOi0xOi0x
* @ValidationInfo : Timestamp         : 09 May 2023 12:38:09
* @ValidationInfo : Encoding          : UTF-8
* @ValidationInfo : User Name         : ITSS
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : 591
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : true
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : R21_AMR.0
* @ValidationInfo : Copyright Temenos Headquarters SA 1993-2021. All rights reserved.
$PACKAGE APAP.REDOFCFI
SUBROUTINE REDO.FC.S.COLL.FS
*=============================================================================
* Subroutine Type : ROUTINE
* Attached to     : TEMPLATE REDO.CREATE.ARRANGEMENT
* Attached as     : ROUTINE
* Primary Purpose : VALIDATION TO COLLATERAL FIRMAS SOLIDARIAS
*
* Incoming:
* ---------
*
* Outgoing:
* ---------
*
*=============================================================================
* Modification History:
*
* Development for : Asociacion Popular de Ahorros y Prestamos
* Development by  : Bryan Torres (btorresalbornoz@temenos.com) - TAM Latin America
* Date            : Jun 17, 2011
*
* Modified by     : Luis Fernando Pazmino - TAM Latin America
* Date            : Dec 08, 2011
* Notes           : Delete input param P.MESSAGE - not used
*                   Minor fixes in F.READ statements
* Modified by     : Jorge Valarezo - TAM Latin America
* Date            : 11.05.2012
* Notes           : Rebuild it's a dependency of PAC00169926
* Modified by     : mgudino - TAM Latin America
* Date            : 22.01.2013
* Modified BY     : Jorge Valarezo - TAM Latin America
* Date            : 20.02.2013   PACS00249518
* Modification History:
* Date                 Who                              Reference                            DESCRIPTION
*04-04-2023            CONVERSION TOOL                AUTO R22 CODE CONVERSION           VM TO @VM ,FM TO @FM SM TO @SM and I++ to I=+1
*04-04-2023          jayasurya H                       MANUAL R22 CODE CONVERSION           call routine format modified
 
*=============================================================================

******************************************************************************
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.COLLATERAL
    $INSERT I_F.COLLATERAL.CODE
    $INSERT I_F.CUSTOMER
    $INSERT I_F.USER

    $INSERT I_F.REDO.CREATE.ARRANGEMENT
    $INSERT I_F.REDO.COLLATERAL.REA
******************************************************************************

    GOSUB INITIALISE
    GOSUB OPEN.FILES
    GOSUB PROCESS

RETURN

* ======
PROCESS:
* ======
* GOSUB TIPO.GARANTIA ;*Opcion deshabilitada basado en el pacs PACS00249518


    Y.I = 1
    Y.COUNT = ""
    Y.COUNT = DCOUNT(Y.COLL.TYPE,@VM)
    LOOP
    WHILE Y.I LE Y.COUNT
        IF Y.COLL.TYPE<1,Y.I> THEN
            GOSUB CLASE.GARANTIA
            GOSUB NOMINAL.VALUE
            GOSUB ID.GARANTE
            GOSUB TIPO.GARANTE.SOLIDARIO
            GOSUB VAL.REA.COLL
            GOSUB MONEDA

        END
        IF Y.SEC.NO<1,Y.I> THEN
            Y.SEC.TYPE = Y.COLL.TYPE<1,Y.I>
            CALL APAP.REDOFCFI.RedoFcSMapping(Y.SEC.TYPE , Y.SEC.NO<1,Y.I>, Y.I);*MANUAL R22 CODE CONVERSION
            
            GOSUB VALIDA.CUST
        END ELSE
            GOSUB FECHA.CREA.GARANTIA.USUA
        END
        Y.I += 1
    REPEAT
RETURN

* ============
VALIDA.CUST:
* ============
    Y.CUST = FIELD(Y.SEC.NO<1,Y.I>, '.', 1)
    IF Y.CUSTOMER NE Y.CUST THEN
        TEXT = 'EB.FC.CUST.DIF'
        M.CONT = DCOUNT(R.NEW(REDO.FC.OVERRIDE),@VM) + 1
        CALL STORE.OVERRIDE(M.CONT)
    END
RETURN

* ============
TIPO.GARANTIA:
* ============
    R.NEW(REDO.FC.TYPE.RATE.REV)= "PERIODICO"
RETURN

* =============
CLASE.GARANTIA:
* =============

    CALL F.READ(FN.COLLATERAL.CODE,Y.COLL.TYPE<1,Y.I>,R.COLLATERAL.CODE,F.COLLATERAL.CODE,ERR.MSJ)
    IF ERR.MSJ THEN
        AF = REDO.FC.SEC.CLASSIFY.FS
        AV= Y.I
        ETEXT = "EB-FC-DONT-COLL-ASO"
        CALL STORE.END.ERROR
    END ELSE
        Y.TYPE.CODES= R.COLLATERAL.CODE<COLL.CODE.COLLATERAL.TYPE>
        Y.NEW.TYPE.CODE=R.NEW(REDO.FC.SEC.CLASSIFY.FS)
        LOCATE Y.NEW.TYPE.CODE IN Y.TYPE.CODES<1,1> SETTING YPOS THEN
        END
    END
RETURN

* =========
PROCESS.CUS.LEGAL.ID:
* =========
    IF NOT(Y.CURRENT.L.ID) THEN
        IF R.CUSTOMER<EB.CUS.LOCAL.REF,WPOSLCUCI> THEN
            Y.CURRENT.L.ID = R.CUSTOMER<EB.CUS.LOCAL.REF,WPOSLCUCI>
            RETURN
        END
        IF R.CUSTOMER<EB.CUS.LOCAL.REF,WPOSLCUPASS> THEN
            Y.CURRENT.L.ID  = R.CUSTOMER<EB.CUS.LOCAL.REF,WPOSLCUPASS>
        END
        RETURN
    END

RETURN
* =======================
FECHA.CREA.GARANTIA.USUA:
* =======================

*Get the values for the  fields (Value Date)
    Y.FEC1 = R.NEW(REDO.FC.SEC.CREATE.DATE.FS)
*######### Get the current user and validate date #########
    Y.USR.ID = OPERATOR
    CALL CACHE.READ(FN.USR, Y.USR.ID, R.USR, Y.USR.ERROR);* R22 Auto conversion

*GET USER LOG ON
    IF Y.USR.ERROR THEN
        ETEXT = "EB-FC-READ.ERROR" : @FM : FN.USR
        CALL STORE.END.ERROR
    END ELSE
        VAR.BAN.DATE = R.USR<EB.USE.LOCAL.REF,WPOSUSER>
    END

*VALIDATE THE PARAMETER IS ASIGNED
    IF (VAR.BAN.DATE EQ '') THEN
        AF =REDO.FC.SEC.CREATE.DATE.FS
        TEXT = 'EB-FC-USER-ALOW-VALID'
        CALL  STORE.END.ERROR
    END

*VALIDATE THAT THE USER HAVE ACCESS TO MODIFY THE DATE
    IF (VAR.BAN.DATE EQ 2) AND (Y.FEC1<1,Y.I> NE TODAY) THEN
        AF = REDO.FC.SEC.CREATE.DATE.FS
        AV = Y.I
        ETEXT = 'EB-FC-NO.ALLOW-TO-USER'
        CALL  STORE.END.ERROR
    END

*TEST THE DATE
    IF Y.FEC1<1,Y.I> GT TODAY THEN
        AF = REDO.FC.SEC.CREATE.DATE.FS
        AV = Y.I
        ETEXT = 'EB-FC-DONT-AFTER-DATE'
        CALL STORE.END.ERROR

    END

*VALIDATE THE DATE IS LESS THAN ACTUAL DATE
    IF (Y.FEC1<1,Y.I> LT TODAY) THEN
        TEXT = 'EB.FC.BEFORE.DATE'
        M.CONT = DCOUNT(R.NEW(REDO.FC.OVERRIDE),@VM) + 1
        CALL STORE.OVERRIDE(M.CONT)
    END

RETURN

* =========
ID.GARANTE:
* =========

    R.NEW(REDO.FC.LOAN.DEBTOR.ID.FS)<1,Y.I> = R.NEW(REDO.FC.CUSTOMER)
    R.NEW(REDO.FC.GUARAN.ID.FS)<1,Y.I> = R.NEW(REDO.FC.SEC.HLD.IDEN.FS)<1,Y.I>

    GUAR.CUS.ID = R.NEW(REDO.FC.GUARAN.ID.FS)<1,Y.I>
    LOAN.DEBTORS.ID = R.NEW(REDO.FC.LOAN.DEBTOR.ID.FS)<1,Y.I>

    CALL F.READ(FN.CUSTOMER,LOAN.DEBTORS.ID,R.CUSTOMER,F.CUSTOMER,Y.CUSTOMER.ERR.MSJ)
    IF Y.CUSTOMER.ERR.MSJ THEN
        ETEXT = "EB-FC-READ.ERROR" : @FM : FN.CUSTOMER
        CALL STORE.END.ERROR
    END ELSE
        VAL.CUS.NAME.LOAN = R.CUSTOMER<EB.CUS.NAME.1>
        VAL.LEGAL.ID.LOAN = R.CUSTOMER<EB.CUS.LEGAL.ID>
        R.NEW(REDO.FC.LOAN.DEB.NAME.FS)<1,Y.I>=VAL.CUS.NAME.LOAN
        Y.CURRENT.L.ID = VAL.LEGAL.ID.LOAN
        GOSUB PROCESS.CUS.LEGAL.ID
        R.NEW(REDO.FC.LOAN.DEB.LEG.ID.FS)<1,Y.I> = Y.CURRENT.L.ID
        Y.CURRENT.L.ID = ''
    END


    IF GUAR.CUS.ID THEN
        CALL F.READ(FN.CUSTOMER,GUAR.CUS.ID,R.CUSTOMER,F.CUSTOMER,Y.CUS.GUAR.ERR.MSJ)
        IF Y.CUS.GUAR.ERR.MSJ THEN
            ETEXT = "EB-FC-READ.ERROR" : @FM : FN.CUSTOMER
            CALL STORE.END.ERROR
        END ELSE
            VAL.CUS.NAME = R.CUSTOMER<EB.CUS.NAME.1>
            VAL.LEGAL.ID = R.CUSTOMER<EB.CUS.LEGAL.ID>
            Y.CURRENT.L.ID = VAL.LEGAL.ID
            GOSUB PROCESS.CUS.LEGAL.ID
            R.NEW(REDO.FC.GUARAN.NAME.FS)<1,Y.I>= VAL.CUS.NAME
            R.NEW(REDO.FC.GUARAN.LEGAL.ID.FS)<1,Y.I>=Y.CURRENT.L.ID
            Y.CURRENT.L.ID = ''
        END
    END

    IF GUAR.CUS.ID EQ LOAN.DEBTORS.ID THEN
        AF = REDO.FC.GUARAN.ID.FS
        AV=Y.I
        ETEXT = 'EB-FC-GUAR-SAME-DEU'
        CALL STORE.END.ERROR
    END

RETURN

* =====================
TIPO.GARANTE.SOLIDARIO:
* =====================
    IF GUAR.CUS.ID<1,Y.I> THEN
        CALL F.READ(FN.CUSTOMER,GUAR.CUS.ID<1,Y.I>,R.CUSTOMER,F.CUSTOMER,Y.GUR.CUS.ERR.MSJ)
        IF Y.GUR.CUS.ERR.MSJ THEN
            ETEXT = "EB-FC-READ.ERROR" : @FM : FN.CUSTOMER
            CALL STORE.END.ERROR
        END ELSE
            VAL.CUS.NA= R.CUSTOMER<EB.CUS.NATIONALITY>
            VAL.CUS.TYPE= R.CUSTOMER<EB.CUS.LOCAL.REF,WPOSLCU>
            VAL.CUS.LEGA.ID= R.CUSTOMER<EB.CUS.LEGAL.ID>
            BEGIN CASE
                CASE VAL.CUS.TYPE  EQ "PERSONA FISICA" AND VAL.CUS.NA EQ "DO" AND NOT(R.NEW(REDO.FC.GUARAN.LEGAL.ID.FS))
                    R.NEW(REDO.FC.TYPE.JOINT.GUAR.FS)<1,AV>= "P3"
                CASE VAL.CUS.TYPE  EQ "PERSONA FISICA" AND VAL.CUS.NA EQ "DO"
                    R.NEW(REDO.FC.TYPE.JOINT.GUAR.FS)<1,AV>= "P1"
                CASE VAL.CUS.TYPE  EQ "PERSONA FISICA" AND VAL.CUS.NA NE "DO"
                    R.NEW(REDO.FC.TYPE.JOINT.GUAR.FS)<1,AV>= "P2"
                CASE VAL.CUS.TYPE  EQ "PERSONA JURIDICA"
                    R.NEW(REDO.FC.TYPE.JOINT.GUAR.FS)<1,AV>= "E3"
            END CASE
        END
    END
RETURN

* ===========
VAL.REA.COLL:
* ===========
    CALL F.READ(FN.REDO.COLLATERAL.REA, Y.REDO.COLLATERAL.REA.ID<1,Y.I>, R.REDO.COLLATERAL.REA, F.REDO.COLLATERAL.REA, Y.ERR.REDO.COLLATERAL.REA)
    IF R.REDO.COLLATERAL.REA THEN
        PERCENTAGE = R.REDO.COLLATERAL.REA<R.COL.REA.PERCENTAGE>
        Y.CENTRAL.BANK.VALUE = Y.NOMINAL.VALUE * PERCENTAGE / 100
        Y.CENTRAL.BANK.VALUE = DROUND(Y.CENTRAL.BANK.VALUE, 2)
        R.NEW(REDO.FC.CENT.BANK.VAL.FS)<1,Y.I> = Y.CENTRAL.BANK.VALUE
    END
RETURN

* =====
MONEDA:
* =====
    R.NEW(REDO.FC.COLL.CURRENCY.FS)<1,Y.I> = R.NEW(REDO.FC.LOAN.CURRENCY)
RETURN

* ============
NOMINAL.VALUE:
* ============
    R.NEW(REDO.FC.SEC.VALUE.FS)<1,Y.I> = R.NEW(REDO.FC.AMOUNT)
    Y.NOMINAL.VALUE = R.NEW(REDO.FC.SEC.VALUE.FS)<1,Y.I>      ;*** VALOR NOMINAL
    R.NEW(REDO.FC.SEC.EXE.VAL.FS)<1,Y.I> = Y.NOMINAL.VALUE    ;*** VALOR DE EJECUCION
    R.NEW(REDO.FC.GEN.LEDGER.VAL.FS)<1,Y.I> = Y.NOMINAL.VALUE ;*** VALOR LIBRO MAYOR

RETURN

* =========
OPEN.FILES:
* =========
    CALL OPF(FN.USR,F.USR)
    CALL OPF(FN.REDO.COLLATERAL.REA, F.REDO.COLLATERAL.REA)

RETURN

* =========
INITIALISE:
* =========

    WCAMPOU = "VAL.MOD.DATE"    ;*Set the field validate user date
    WCAMP = "L.CU.TIPO.CL"
    WCAMP<2> = "L.CU.CIDENT"
    WCAMP<3> = "L.CU.PASS.NAT"
    WCAMPO  = "L.COL.SEC.HOLD"
    WCAMPO<2> = "L.COL.GT.DATE"
    WCAMPO<3> = "L.COL.EXE.DATE"
    WCAMPO<4> = "L.COL.VAL.AVA"
    WCAMPO<5> = "L.COL.DEBTOR.ID"
    WCAMPO<6> = "L.COL.DEBTOR.NA"
    WCAMPO<7> = "L.COL.DBR.LEGID"
    WCAMPO<8> = "L.COL.GUAR.ID"
    WCAMPO<9> = "L.COL.GUAR.NAME"
    WCAMPO<10> = "L.COL.GUR.LEGID"
    WCAMPO<11> = "L.COL.GUAR.TYPE"
    WCAMPO = CHANGE(WCAMPO,@FM,@VM)
    WCAMP = CHANGE(WCAMP,@FM,@VM)
    WCAMPO = WCAMPO : @FM : WCAMPOU : @FM : WCAMP
    ZPOS = ''
    Y.APP = "COLLATERAL" : @FM : "USER" : @FM : "CUSTOMER"
    CALL MULTI.GET.LOC.REF(Y.APP,WCAMPO,ZPOS)
    WPOSSECH = ZPOS<1,1>
    WPOSGTDATA = ZPOS<1,2>
    WPOSEXEDATE = ZPOS<1,3>
    WPOSAVAIL = ZPOS<1,4>
    WPOSDEBTORID = ZPOS<1,5>
    WPOSDEBTORNAME = ZPOS<1,6>
    WPOSDEBTORLEGID = ZPOS<1,7>
    WPOSGUARID = ZPOS<1,8>
    WPOSGUARNAME = ZPOS<1,9>
    WPOSGUARLEGID = ZPOS<1,10>
    WPOSGUARTYPE = ZPOS<1,11>
    WPOSUSER     = ZPOS<2,1>
    WPOSLCU      = ZPOS<3,1>
    WPOSLCUCI    = ZPOS<3,2>
    WPOSLCUPASS    = ZPOS<3,3>

    Y.SEC.NO = R.NEW(REDO.FC.SEC.NO.STATE.TP)
*    GUAR.CUS.ID = R.NEW(COLL.LOCAL.REF)<1,WPOSGUARID>
*    LOAN.DEBTORS.ID = R.NEW(COLL.LOCAL.REF)<1,WPOSDEBTORID>

    FN.COLLATERAL= "F.COLLATERAL"
    F.COLLATERAL=""

    FN.COLLATERAL.CODE= "F.COLLATERAL.CODE"
    F.COLLATERAL.CODE=""

    FN.CUSTOMER= "F.CUSTOMER"
    F.CUSTOMER=""

    FN.USR  = 'F.USER'
    F.USR   = ''
    R.USR   = ''


    GUAR.CUS.ID = R.NEW(REDO.FC.GUARAN.ID.FS)

    FN.CUSTOMER= "F.CUSTOMER"
    F.CUSTOMER=""

    Y.COLLATERAL.TYPE = R.NEW(REDO.FC.SEC.CLASSIFY.FS)
    Y.VALUE.DATE = R.NEW(REDO.FC.SEC.CREATE.DATE.FS)
    Y.NOMINAL.VALUE = R.NEW(REDO.FC.SEC.VALUE.FS)
* Y.BLOCK.NO = R.NEW(REDO.FC.FABR.YEAR.VS)
    Y.CENTRAL.BANK.VALUE = ''

    FN.REDO.COLLATERAL.REA = 'F.REDO.COLLATERAL.REA'
    F.REDO.COLLATERAL.REA = ''
    R.REDO.COLLATERAL.REA = ''
    Y.REDO.COLLATERAL.REA.ID = Y.COLLATERAL.TYPE
    Y.ERR.REDO.COLLATERAL.REA = ''
    Y.CUSTOMER           = R.NEW(REDO.FC.CUSTOMER)

    Y.COLL.TYPE = R.NEW(REDO.FC.TYPE.OF.SEC.FS)

RETURN

END
