* @ValidationCode : Mjo3NDk4NTEzMjI6Q3AxMjUyOjE2ODExMzUxNjM4NzY6SVRTUzotMTotMTo1NjoxOmZhbHNlOk4vQTpERVZfMjAyMTA4LjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 10 Apr 2023 19:29:23
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : ITSS
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : 56
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : true
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : DEV_202108.0
* @ValidationInfo : Copyright Temenos Headquarters SA 1993-2021. All rights reserved.
$PACKAGE APAP.REDOFCFI
SUBROUTINE REDO.FC.VAL.CURRENCY
*
* ====================================================================================
*
*
* ====================================================================================
*
* Subroutine Type :ROUTINE
* Attached to     :CURRENCY HOLD.FIELD IN REDO.CREATE.ARRANGEMENT TEMPLATE
* Attached as     :HOLD.FIELD in CURRENCY field OF REDO.CREATE.ARRANGEMENT TEMPLATE
* Primary Purpose :VALIDATE WHAT KIND OF CURRENCY IS POSIBLE TO INPUT OF HEADER TO REDO.CREATE.ARRANGEMENT TEMPLATE
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
* Date            : Agosto 23 2011
* Modification History:
* Date                 Who                              Reference                            DESCRIPTION
*04-04-2023           CONVERSION TOOL                AUTO R22 CODE CONVERSION                 NO CHANGES
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
*NOMBRE.CAMPO = "...LOAN.CURRENCY..."
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
    GOSUB CURRENCY


RETURN

* ======
CURRENCY:
* ======


    SEL.CMD  = 'SELECT ' :FN.AA.PRODUCT.DESIGNER
    SEL.CMD := '  LIKE ' :Y.PRODUCT: '-... BY-DSND @ID'
    SEL.LIST = ''
    NO.REC   = ''
    SEL.ERR  = ''
    CALL EB.READLIST(SEL.CMD, SEL.LIST, '', NO.REC, SEL.ERR)
    IF SEL.LIST THEN
        REMOVE ID.PRODUCT FROM SEL.LIST SETTING POS
        CALL CACHE.READ(FN.AA.PRODUCT.DESIGNER, ID.PRODUCT, R.AA.PRODUCT.DESIGNER, Y.ERR)
        Y.CURRENCY =  R.AA.PRODUCT.DESIGNER<AA.PRD.CURRENCY>

        LOCATE  Y.ARR.CURRENCY IN Y.CURRENCY<1,1> SETTING YPOS ELSE

            AF = REDO.FC.LOAN.CURRENCY
            ETEXT = 'EB-FC-CURRENCY-NOTIN-PRODUCT'
            CALL STORE.END.ERROR

        END
    END


RETURN




*
* =========
OPEN.FILES:
* =========
*
    CALL OPF(FN.AA.PRODUCT.DESIGNER,F.AA.PRODUCT.DESIGNER)

RETURN
*
* =========
INITIALISE:
* =========
*
    LOOP.CNT        = 1
    MAX.LOOPS       = 1
    PROCESS.GOAHEAD = 1

    Y.PRODUCT = R.NEW(REDO.FC.PRODUCT)
    FN.AA.PRODUCT.DESIGNER= "F.AA.PRODUCT.DESIGNER"
    F.AA.PRODUCT.DESIGNER=""
    Y.ARR.CURRENCY = COMI
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

END
