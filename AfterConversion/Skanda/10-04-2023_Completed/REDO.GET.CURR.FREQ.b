* @ValidationCode : MjoxMzM0MTIwNjIxOkNwMTI1MjoxNjgxMTA3MTE2MDk5OklUU1M6LTE6LTE6MDoxOmZhbHNlOk4vQTpSMjFfQU1SLjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 10 Apr 2023 11:41:56
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : ITSS
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : true
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : R21_AMR.0
* @ValidationInfo : Copyright Temenos Headquarters SA 1993-2021. All rights reserved.
$PACKAGE APAP.TAM
* COMPANY NAME: ASOCIACION POPULAR DE AHORROS Y PRESTAMOS
* DEVELOPED BY: PRABHU N
* PROGRAM NAME: REDO.GET.CURR.FREQ
* ODR NO      :PACS00125978
** 10-04-2023 R22 Auto Conversion 
** 10-04-2023 Skanda R22 Manual Conversion line no 38
*----------------------------------------------------------------------
SUBROUTINE REDO.GET.CURR.FREQ
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.STANDING.ORDER


    GOSUB INIT
    GOSUB PROCESS
RETURN

*****
INIT:
****
    LOC.APP='STANDING.ORDER'
    APP.FIELD='L.STO.START.DTE'
    STO.FREQ.POS = ''

    CALL APAP.TAM.MULTI.GET.LOC.REF(LOC.APP,APP.FIELD,STO.FREQ.POS) ;* R22 Manual Conversion

    ST.DATE=R.NEW(STO.LOCAL.REF)<1,STO.FREQ.POS>
    START.DATE = ST.DATE
    START.DAY = ST.DATE[7,2]
    END.DATE=R.NEW(STO.CURRENT.END.DATE)
    FREQ.MTH='M01'
    Y.OLD.COMI = COMI
    COMI = START.DATE:FREQ.MTH:START.DAY
    CALL CFQ
    END.CFQ.DATE = COMI[1,8]
    COMI = Y.OLD.COMI

RETURN
********
PROCESS:
********

*    IF START.DATE LE TODAY THEN ;* R22 Auto conversion
*       AF = STO.LOCAL.REF ;* R22 Auto conversion
*       AV=STO.FREQ.POS ;* R22 Auto conversion
*       ETEXT = 'EB-DATE.G..TODAY' ;* R22 Auto conversion
*       CALL STORE.END.ERROR ;* R22 Auto conversion

*   END ;* R22 Auto conversion


    IF END.DATE LE START.DATE THEN
        AF = STO.CURRENT.END.DATE
        ETEXT = 'EB-DATE.GT.THAN.TODAY'
        CALL STORE.END.ERROR

    END

    IF END.DATE LT END.CFQ.DATE THEN
        AF = STO.CURRENT.END.DATE
        ETEXT = 'EB-1ST.MONTH'
        CALL STORE.END.ERROR

    END

RETURN
END
