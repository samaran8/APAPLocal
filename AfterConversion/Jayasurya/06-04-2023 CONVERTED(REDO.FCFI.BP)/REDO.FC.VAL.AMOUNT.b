* @ValidationCode : MjoyMTMzOTMxMDQxOkNwMTI1MjoxNjgwNjg3MTc4MjgzOklUU1NCTkc6LTE6LTE6MDowOmZhbHNlOk4vQTpERVZfMjAyMTA4LjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 05 Apr 2023 15:02:58
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : ITSSBNG
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : DEV_202108.0
$PACKAGE APAP.REDOFCFI
SUBROUTINE REDO.FC.VAL.AMOUNT
*
* ====================================================================================
*
*
*
* ====================================================================================
*
* Subroutine Type :ROUTINE
* Attached to     :AMOUNT HOLD.FIELD IN REDO.CREATE.ARRANGEMENT TEMPLATE
* Attached as     :HOLD.FIELD in AMOUNT field
* Primary Purpose :VALIDATE MAX AND MIN AMOUNT OF HEADER TO REDO.CREATE.ARRANGEMENT TEMPLATE
*
*
* Incoming:
* ---------
*
*
*
* Outgoing:

* ---------
*
*
*-----------------------------------------------------------------------------------
* Modification History:
*
* Development for : Asociacion Popular de Ahorros y Prestamos
* Development by  : Bryan Torres (btorresalbornoz@temenos.com) - TAM Latin America
* Date            : Agosto 25 2011
* Modification History:
* Date                 Who                              Reference                            DESCRIPTION
*04-04-2023            CONVERSION TOOL                AUTO R22 CODE CONVERSION           VM TO @VM ,> TO GT
*04-04-2023          jayasurya H                       MANUAL R22 CODE CONVERSION            NO CHANGES
*=======================================================================

    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.AA.PRODUCT.DESIGNER
    $INSERT I_F.REDO.CREATE.ARRANGEMENT
    $INSERT I_F.USER
    $INSERT I_F.AA.TERM.AMOUNT
    $INSERT I_GTS.COMMON
*
*************************************************************************
*

* -------------------------------------------------------------------------------
* PA20071025 Se debe ejecutar solo cuando es invocado desde el campo HOT.FIELD de
*CAMPO.ACTUAL = OFS$HOT.FIELD
*NOMBRE.CAMPO = "...AMOUNT..."
*IF CAMPO.ACTUAL MATCH NOMBRE.CAMPO ELSE
*   RETURN
*END
* Fin PA20071025
*-------------------


    GOSUB INITIALISE
    GOSUB OPEN.FILES
    GOSUB CHECK.PRELIM.CONDITIONS
    IF PROCESS.GOAHEAD THEN
        GOSUB PROCESS
    END

*
RETURN


*
* ======
PROCESS:
* ======

    GOSUB MONTO


RETURN




* =========
MONTO:
* =========
*
    Y.AMOUNT = COMI

    Y.ID.ARR.PRD.CAT.TERM.AMOUNT = Y.ARR.PRODUCT:'-COMMITMENT':'-':Y.ARR.CURRENCY:'...'
    SELECT.STATEMENT = 'SELECT ':FN.AA.PRD.CAT.TERM.AMOUNT:' ':'WITH @ID LIKE ':Y.ID.ARR.PRD.CAT.TERM.AMOUNT:' BY-DSND ID.COMP.5'
    AA.PRD.CAT.TERM.AMOUNT.LIST = ''
    LIST.NAME = ''
    SELECTED = ''
    SYSTEM.RETURN.CODE = ''
    Y.ID.AA.PRD = ''
    CALL EB.READLIST(SELECT.STATEMENT,AA.PRD.CAT.TERM.AMOUNT.LIST,LIST.NAME,SELECTED,SYSTEM.RETURN.CODE)

    REMOVE Y.ID.AA.PRD FROM AA.PRD.CAT.TERM.AMOUNT.LIST SETTING POS

    CALL CACHE.READ(FN.AA.PRD.CAT.TERM.AMOUNT, Y.ID.AA.PRD, R.ARR.PRD.CAT.TERM.AMOUNT, Y.ERR)
    Y.ATTRIBUTE = R.ARR.PRD.CAT.TERM.AMOUNT<AA.AMT.NR.ATTRIBUTE>
    Y.NR.TYPE  = R.ARR.PRD.CAT.TERM.AMOUNT<AA.AMT.NR.TYPE>
    Y.NR.VALUE = R.ARR.PRD.CAT.TERM.AMOUNT<AA.AMT.NR.VALUE>

    LOCATE "AMOUNT" IN Y.ATTRIBUTE<1,1> SETTING APOS THEN

        LOCATE Y.MAXIMUN IN Y.NR.TYPE<1,APOS,1> SETTING YPOS THEN
            Y.NR.VALUE.MAX = R.ARR.PRD.CAT.TERM.AMOUNT<AA.AMT.NR.VALUE,APOS,YPOS>

            IF Y.AMOUNT GT Y.NR.VALUE.MAX THEN ;* AUTO R22 CODE CONVERSION
                AF = REDO.FC.AMOUNT
                ETEXT = 'EB-FC-AMOUNT-NO-PRODUCT'
                CALL STORE.END.ERROR
            END
        END
        LOCATE Y.MINIMUN IN Y.NR.TYPE<1,APOS,1> SETTING ZPOS THEN
            Y.NR.VALUE.MIN = R.ARR.PRD.CAT.TERM.AMOUNT<AA.AMT.NR.VALUE,APOS,ZPOS>

            IF Y.AMOUNT LT Y.NR.VALUE.MIN THEN
                AF = REDO.FC.AMOUNT
                TEXT = 'OEB-FC-AMOUNT-NO-PRODUCT'
                M.CONT = DCOUNT(R.NEW(REDO.FC.OVERRIDE),@VM) + 1
                CALL STORE.OVERRIDE(M.CONT)
            END
        END


    END





*
* =========
OPEN.FILES:
* =========
*

    CALL OPF(FN.AA.PRD.CAT.TERM.AMOUNT, F.AA.PRD.CAT.TERM.AMOUNT)

RETURN
*
* =========
INITIALISE:
* =========
*
    LOOP.CNT        = 1
    MAX.LOOPS       = 1
    PROCESS.GOAHEAD = 1




    FN.AA.PRD.CAT.TERM.AMOUNT = 'FBNK.AA.PRD.CAT.TERM.AMOUNT'
    F.AA.PRD.CAT.TERM.AMOUNT = ''
    Y.ARR.PRODUCT =  R.NEW(REDO.FC.PRODUCT)
    Y.ARR.CURRENCY = R.NEW(REDO.FC.LOAN.CURRENCY)

    Y.MAXIMUN="MAXIMUM"
    Y.MINIMUN="MINIMUM"







RETURN

* ======================
CHECK.PRELIM.CONDITIONS:
* ======================
*
    LOOP
    WHILE LOOP.CNT LE MAX.LOOPS AND PROCESS.GOAHEAD DO
        BEGIN CASE

            CASE LOOP.CNT EQ 1

        END CASE

        LOOP.CNT +=1
    REPEAT
*
RETURN
*
