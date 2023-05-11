$PACKAGE APAP.TAM
SUBROUTINE REDO.GET.TOTAL.INTEREST(ARR.ID,PROPERTY,INT.AMT)

*--------------------------------------------------------------------------------
* Company Name : ASOCIACION POPULAR DE AHORROS Y PRESTAMOS
* Program Name : REDO.GET.TOTAL.INTEREST
*--------------------------------------------------------------------------------
* Description: This is a generic routine to get the total property amount
* for the arrangement ID from schedule
*
* Input Arg   : ARR.ID , PROPERTY
* INT.AMT     : Amount
*
*-----------------------------------------------------------------------------
* Modification History :
*-----------------------------------------------------------------------------
*
*  DATE             WHO         REFERENCE         DESCRIPTION
* 19-May-2011   H GANESH      PACS00055012 - B.16 Initial creation
*** 10-04-2023 R22 Auto Conversion - FM TO @FM, VM to @VM, SM to @SM
** 10-04-2023 Skanda R22 Manual Conversion - No changes
*----------------------------------------------------------------------------

    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_AA.APP.COMMON
    $INSERT I_F.REDO.TEMP.STORE.COMMON


    GOSUB PROCESS
RETURN
*---------------------------------------------------------------------------------
PROCESS:
*---------------------------------------------------------------------------------

    INT.AMT=''
    CALL REDO.TEMP.STORE.COMMON('STORE')
    NO.RESET='1'
    DATE.RANGE=''
    SIMULATION.REF=''
    CALL AA.SCHEDULE.PROJECTOR(ARR.ID, SIMULATION.REF, NO.RESET, DATE.RANGE, TOT.PAYMENT, PAYMENT.DATES, DUE.DEFER.DATES, PAYMENT.TYPES, DUE.METHODS,DUE.TYPE.AMTS, PAYMENT.PROPERTIES, PAYMENT.PROPERTIES.AMT, DUE.OUTS)

    CALL REDO.TEMP.STORE.COMMON('RESTORE')
    GOSUB INTEREST.CALC

RETURN
*---------------------------------------------------------------------------------
INTEREST.CALC:
*---------------------------------------------------------------------------------
* This part locates the principal interest property from schedule infomation and sums the amount

    NO.OF.DATES = DCOUNT(PAYMENT.DATES,@FM)
    Y.VAR1=1
    LOOP
    WHILE Y.VAR1 LE NO.OF.DATES
        TOT.TYPES = DCOUNT(PAYMENT.TYPES<Y.VAR1>,@VM)
        GOSUB GET.INTEREST
        Y.VAR1 += 1 ;* R22 Auto conversion
    REPEAT

RETURN

*---------------------------------------------------------------------------
GET.INTEREST:
*---------------------------------------------------------------------------
* Here we gets the principal interest part
    Y.VAR2=1
    LOOP
    WHILE Y.VAR2 LE TOT.TYPES
        PROP.LIST = PAYMENT.PROPERTIES<Y.VAR1,Y.VAR2> ;* This is the list of properties due for the current date
        PROP.AMT = PAYMENT.PROPERTIES.AMT<Y.VAR1,Y.VAR2>        ;* This is the list of property amt for each property
        LOCATE PROPERTY IN PROP.LIST<1,1,1> SETTING INT.PROP.POS THEN
            INT.AMT += PROP.AMT<1,1,INT.PROP.POS>
        END
        Y.VAR2 += 1 ;* R22 Auto conversion
    REPEAT
RETURN
END
