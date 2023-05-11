* @ValidationCode : MjotMTAxNDQzMDY5NzpDcDEyNTI6MTY4MzEwNjAxODM1ODpoYWk6LTE6LTE6MDowOmZhbHNlOk4vQTpSMjFfQU1SLjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 03 May 2023 14:56:58
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : hai
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : R21_AMR.0
$PACKAGE APAP.AA;*MANUAL R22 CODE CONVERSION
SUBROUTINE REDO.GET.TOTAL.INTEREST(ARR.ID,PROPERTY,INT.AMT)
    
*-----------------------------------------------------------------------------------

* Modification History:
*DATE              WHO                REFERENCE                        DESCRIPTION
*29-03-2023     CONVERSION TOOL         AUTO R22 CODE CONVERSION          ++ TO +=1, VM TO @VM,FM TO @FM
*29-03-2023      MOHANRAJ R        MANUAL R22 CODE CONVERSION         Package name added APAP.AA


*-----------------------------------------------------------------------------------


    

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
*
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
    CALL APAP.AA.redoTempStoreCommon('STORE')
    NO.RESET='1'
    DATE.RANGE=''
    SIMULATION.REF=''
    CALL AA.SCHEDULE.PROJECTOR(ARR.ID, SIMULATION.REF, NO.RESET, DATE.RANGE, TOT.PAYMENT, PAYMENT.DATES, DUE.DEFER.DATES, PAYMENT.TYPES, DUE.METHODS,DUE.TYPE.AMTS, PAYMENT.PROPERTIES, PAYMENT.PROPERTIES.AMT, DUE.OUTS)

    CALL APAP.AA.redoTempStoreCommon('RESTORE')
    GOSUB INTEREST.CALC

RETURN
*---------------------------------------------------------------------------------
INTEREST.CALC:
*---------------------------------------------------------------------------------
* This part locates the principal interest property from schedule infomation and sums the amount

    NO.OF.DATES = DCOUNT(PAYMENT.DATES,@FM) ;*AUTO R22 CODE CONVERSION
    Y.VAR1=1
    LOOP
    WHILE Y.VAR1 LE NO.OF.DATES
        TOT.TYPES = DCOUNT(PAYMENT.TYPES<Y.VAR1>,@VM) ;*AUTO R22 CODE CONVERSION
        GOSUB GET.INTEREST
        Y.VAR1 += 1 ;*AUTO R22 CODE CONVERSION
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
        Y.VAR2 += 1 ;*AUTO R22 CODE CONVERSION
    REPEAT
RETURN
END
