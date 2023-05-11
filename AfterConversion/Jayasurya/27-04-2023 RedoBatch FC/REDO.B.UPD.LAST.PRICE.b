* @ValidationCode : MjotNDUzOTU5NDE4OkNwMTI1MjoxNjgyNTkwMDk1NjcwOklUU1NCTkc6LTE6LTE6MDowOmZhbHNlOk4vQTpERVZfMjAyMTA4LjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 27 Apr 2023 15:38:15
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : ITSSBNG
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : DEV_202108.0
$PACKAGE APAP.REDOBATCH
SUBROUTINE REDO.B.UPD.LAST.PRICE(Y.SM.ID)
*-----------------------------------------------------------------------------------------------
* Company Name    : ASOCIACION POPULAR DE AHORROS Y PRESTAMOS
* Developed by    : Temenos Application Management
* Program Name    : REDO.B.UPD.LAST.PRICE
* Program Type    : BATCH JOB (Multithreaded routine)
*-----------------------------------------------------------------------------------------------
* Description   : This routine will calculate the APAP price using the formula and updates in the
*                 LAST.PRICE field
*
* In  Parameter : Y.SM.ID - holds the SECURITY.MASTER record id returned from the SELECT routine
* Out Parameter : --na--
* ODR Number    : ODR-2010-07-0083
*--------------------------------------------------------------------------------------------------
* Modification History :
*-----------------------
*
* 18.11.2010      Krishna Murthy T.S     SC006         INITIAL CREATION
* Date                   who                   Reference
* 13-04-2023         CONVERSTION TOOL     R22 AUTO CONVERSTION - VM TO @VM
* 13-04-2023          ANIL KUMAR B        R22 MANUAL CONVERSTION -CALL RTN METHOD ADDED
*
*-------------------------------------------------------------------------------------------------
    $INSERT I_EQUATE
    $INSERT I_COMMON
    $INSERT I_F.SECURITY.MASTER
    $INSERT I_F.PRICE.TYPE
    $INSERT I_F.DATES
    $INSERT I_REDO.B.UPD.LAST.PRICE.COMMON
    $USING APAP.TAM

    Y.REDEM = 100
    Y.TODAY = R.DATES(EB.DAT.NEXT.WORKING.DAY)
    Y.ROUNDING = ''
    CALL F.READ(FN.SECURITY.MASTER,Y.SM.ID,R.SECURITY.MASTER,F.SECURITY.MASTER,Y.SM.ERR)
    Y.MAT.DATE = R.SECURITY.MASTER<SC.SCM.MATURITY.DATE>
    Y.INT.RATE.LIST = R.SECURITY.MASTER<SC.SCM.INTEREST.RATE>
    Y.SM.LOC.REF.LIST = R.SECURITY.MASTER<SC.SCM.LOCAL.REF>
    Y.FREQ = R.SECURITY.MASTER<SC.SCM.NO.OF.PAYMENTS>
    Y.INT.BASIS = R.SECURITY.MASTER<SC.SCM.INTEREST.DAY.BASIS>
    Y.PRICE.TYPE = R.SECURITY.MASTER<SC.SCM.PRICE.TYPE>
    Y.ACCR.ST.DATE = R.SECURITY.MASTER<SC.SCM.ACCRUAL.START.DATE>
    Y.YLD = Y.SM.LOC.REF.LIST<1,Y.TRN.LOC.POS>
    Y.INT.CNT = DCOUNT(Y.INT.RATE.LIST,@VM)
    Y.INT.RATE = Y.INT.RATE.LIST<1,Y.INT.CNT>

    IF Y.MAT.DATE AND Y.INT.RATE AND Y.YLD AND Y.FREQ AND Y.INT.BASIS AND Y.ACCR.ST.DATE AND Y.MAT.DATE LE Y.TODAY THEN
        CALL F.READ(FN.PRICE.TYPE,Y.PRICE.TYPE,R.PRICE.TYPE,F.PRICE.TYPE,Y.PT.ERR)
        Y.CALC.METHOD = R.PRICE.TYPE<SC.PRT.CALCULATION.METHOD>

        BEGIN CASE
            CASE Y.CALC.METHOD EQ 'PRICE'
*CALL APAP.TAM.REDO.SC.CALCULATE.PRICE(Y.TODAY,Y.MAT.DATE,Y.INT.RATE,Y.YLD,Y.REDEM,Y.FREQ,Y.INT.BASIS,Y.PRICE,Y.ROUNDING,Y.ACCR.ST.DATE,RESERVED.1) ;*MANUAL R22 CODE CONVERSION
                CALL APAP.TAM.redoScCalculatePrice(Y.TODAY,Y.MAT.DATE,Y.INT.RATE,Y.YLD,Y.REDEM,Y.FREQ,Y.INT.BASIS,Y.PRICE,Y.ROUNDING,Y.ACCR.ST.DATE,RESERVED.1) ;*MANUAL R22 CODE CONVERSION
            CASE Y.CALC.METHOD EQ 'DPRICE'
*CALL APAP.TAM.REDO.SC.CALCULATE.DPRICE(Y.TODAY,Y.MAT.DATE,Y.YLD,Y.INT.BASIS,Y.PRICE,RESERVED.2,RESERVED.1) ;*MANUAL R22 CODE CONVERSION
                CALL APAP.TAM.redoScCalculatePrice(settlement, maturityDate, rate, yld, redemption, freq, yearDaysBasis, price, rounding, accrStDate, yIssueDate)
        END CASE

*Posting OFS message to update the LAST.PRICE in SECURITY.MASTER table
        IF Y.PRICE NE '' THEN
            OFS.SOURCE.ID = 'TAM.OFS.SRC'
            MSG.KEY = ''
            Y.NO.AUTH = 0
            Y.OFS.HDR = ''
            Y.OFS.BODY = ''

            Y.OFS.HDR = 'SECURITY.MASTER,/I/PROCESS//':Y.NO.AUTH:',//':ID.COMPANY:',':Y.SM.ID:','
            Y.OFS.BODY = 'LAST.PRICE::=':Y.PRICE
            R.OFS.MSG = Y.OFS.HDR:Y.OFS.BODY
            CALL OFS.POST.MESSAGE(R.OFS.MSG,MSG.KEY,OFS.SOURCE.ID,'')
        END
    END
RETURN
END
