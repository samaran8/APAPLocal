* @ValidationCode : MjotMjMyNjcyNjQ5OkNwMTI1MjoxNjgxODgxMjcwMzU3OklUU1NCTkc6LTE6LTE6MDowOmZhbHNlOk4vQTpERVZfMjAyMTA4LjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 19 Apr 2023 10:44:30
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : ITSSBNG
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : DEV_202108.0
$PACKAGE APAP.REDORETAIL
SUBROUTINE REDO.DS.ST.BUYERS(Y.RET)
***********************************************************************
* COMPANY NAME: ASOCIACION POPULAR DE AHORROS Y PRESTAMOS
* DEVELOPED BY: Arulprakasam P
* PROGRAM NAME: REDO.DS.ST.BUYERS
* ODR NO      : ODR-2010-07-0082
*----------------------------------------------------------------------
*DESCRIPTION: This routine is attched in DEAL.SLIP.FORMAT 'REDO.BUS.SELL'
* to get the details of the Product selected for LETTER

*IN PARAMETER:  NA
*OUT PARAMETER: NA
*LINKED WITH:
*----------------------------------------------------------------------
* Modification History:
* Date                 Who                              Reference                            DESCRIPTION
*13-04-2023            CONVERSION TOOL                AUTO R22 CODE CONVERSION           VM TO @VM ,FM TO @FM , I++ TO I=+1
*13-04-2023          jayasurya H                       MANUAL R22 CODE CONVERSION            NO CHANGES

*----------------------------------------------------------------------

    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.SEC.TRADE
    $INSERT I_F.CUSTOMER

    GOSUB INIT
    GOSUB OPENFILES
    GOSUB PROCESS
RETURN

INIT:
*****
    FN.CUSTOMER = 'F.CUSTOMER'
    F.CUSTOMER = ''

RETURN

OPENFILES:
**********
    CALL OPF(FN.CUSTOMER,F.CUSTOMER)
RETURN

PROCESS:
********

    NAME.1 = ''
    CUST.TRANS.CODE = R.NEW(SC.SBS.CUST.TRANS.CODE)
    CHANGE @VM TO '*' IN CUST.TRANS.CODE
    Y.COUNT.TRANS = DCOUNT(CUST.TRANS.CODE,'*')
    INIT = 1
*LOOP
*WHILE INIT LE Y.COUNT.TRANS
    Y.FIRST.TRANS = FIELD(CUST.TRANS.CODE,'*',INIT)
    IF Y.FIRST.TRANS EQ 'SEL' THEN
        CUSTOMER.NO = R.NEW(SC.SBS.BROKER.NO)<1,INIT>
        CALL F.READ(FN.CUSTOMER,CUSTOMER.NO,R.CUSTOMER,F.CUSTOMER,CUSTOMER.ERR)
        SHORT.NAME = R.CUSTOMER<EB.CUS.SHORT.NAME>
        Y.INT.CNT=DCOUNT(SHORT.NAME,@VM)

        IF Y.INT.CNT GT 1 THEN
            SHORT.NAME = R.CUSTOMER<EB.CUS.SHORT.NAME,LNGG>
        END
        IF  SHORT.NAME EQ '' THEN
            SHORT.NAME = R.CUSTOMER<EB.CUS.SHORT.NAME,2>
        END

        NAME.1<-1> := SHORT.NAME
    END
    IF Y.FIRST.TRANS EQ 'BUY' THEN
        NAME.1 = ''
        NAME.1 = "APAP"
    END
    INIT += 1
*REPEAT
    CHANGE @FM TO ',' IN NAME.1
    Y.RET = NAME.1
RETURN
END
