* @ValidationCode : MjotNTU1MDY2MzMwOkNwMTI1MjoxNjgyNTAyNjYzMTc2OklUU1M6LTE6LTE6NTY3OjE6ZmFsc2U6Ti9BOlIyMV9BTVIuMDotMTotMQ==
* @ValidationInfo : Timestamp         : 26 Apr 2023 15:21:03
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : ITSS
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : 567
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : true
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : R21_AMR.0
* @ValidationInfo : Copyright Temenos Headquarters SA 1993-2021. All rights reserved.
$PACKAGE APAP.REDOAPAP
*MODIFICATION HISTORY:
*---------------------------------------------------------------------------------------
*DATE               WHO                       REFERENCE                 DESCRIPTION
*17-04-2023       CONVERSION TOOLS            AUTO R22 CODE CONVERSION  FM to @FM ,
*17-04-2023       AJITHKUMAR                  MANUAL R22 CODE CONVERSION NO CHANGE
*----------------------------------------------------------------------------------------




SUBROUTINE REDO.APAP.NOFILE.FX.PRINT.DEAL.SLIP(Y.OUT.ARRAY)
*---------------------------------------------------------------------------------
* Company Name  : ASOCIACION POPULAR DE AHORROS Y PRESTAMOS
* Developed By  : GANESH
* Program Name  : REDO.APAP.NOFILE.FX.PRINT.DEAL.SLIP
* ODR NUMBER    : ODR-2010-07-0074
*----------------------------------------------------------------------------------
* Description   : This ia nofile routine used for the enquiry REDO.APAP.NOF.REPRINT.DEAL.TICKET
* In parameter  : None
* out parameter : None
*----------------------------------------------------------------------------------
*
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_ENQUIRY.COMMON
    $INSERT I_F.FOREX
    $INSERT I_F.FX.DEAL.ID.LOG
*
    FN.FOREX = 'F.FOREX'
    F.FOREX  = ''
    CALL OPF(FN.FOREX, F.FOREX)
*
    FN.FOREX.HIS = 'F.FOREX$HIS'
    F.FOREX.HIS  = ''
    CALL OPF(FN.FOREX.HIS, F.FOREX.HIS)
*
    FN.FX.DEAL.ID.LOG = 'F.FX.DEAL.ID.LOG'
    F.FX.DEAL.ID.LOG  = ''
    CALL OPF(FN.FX.DEAL.ID.LOG, F.FX.DEAL.ID.LOG)
*
    Y.ID = ''
    LOCATE "TRADER.REFERENCE" IN D.FIELDS<1> SETTING FX.POS THEN
        Y.ID = D.RANGE.AND.VALUE<FX.POS>
    END
*
    Y.DEAL.SLIP.ID = ''

    IF Y.ID EQ '' THEN
*
        SEL.CMD.LIVE = "SELECT ":FN.FOREX
        CALL EB.READLIST(SEL.CMD.LIVE, SEL.LIST.LIVE, '', NO.OF.REC.LIVE, SEL.ERR.LIVE)
*
        SEL.CMD.HIS = "SELECT ":FN.FOREX.HIS
        CALL EB.READLIST(SEL.CMD.HIS, SEL.LIST.HIS, '', NO.OF.REC.HIS, SEL.ERR.HIS)
*
        SEL.LIST.FINAL<-1> = SEL.LIST.LIVE:@FM:SEL.LIST.HIS
*
        Y.DEAL.SLIP.ID = ''
        Y.ID = ''
        LOOP
            REMOVE Y.ID FROM SEL.LIST.FINAL SETTING Y.POS
        WHILE Y.ID:Y.POS
            GOSUB OPEN.RECORD
            Y.OUT.ARRAY<-1> = Y.ID:"*":Y.FX.CTRPARTY:"*":Y.FX.CCY.BOUGHT:"*":Y.FX.AMT.BOUGHT:"*":Y.FX.CCY.SOLD:"*":Y.FX.AMT.SOLD:"*":Y.FX.VAL.DATE.BUY:"*":Y.FX.VAL.DT.SELL:"*":Y.FX.SPOT.RATE:"*":Y.DEAL.SLIP.ID
        REPEAT
*
    END ELSE
        SEL.HIS.REC = "SELECT ":FN.FOREX.HIS:" WITH @ID LIKE ":Y.ID:"..."
        CALL EB.READLIST(SEL.HIS.REC, SEL.HIS.REC.LIST, '', NOR.HIS, SEL.ERR)
        IF NOT(FIELD(Y.ID,';',2)) THEN
            SEL.HIS.REC.LIST.FINAL<-1> = Y.ID:@FM:SEL.HIS.REC.LIST
        END ELSE
            SEL.HIS.REC.LIST.FINAL<-1> = SEL.HIS.REC.LIST
        END

        LOOP
            REMOVE Y.ID FROM SEL.HIS.REC.LIST.FINAL SETTING HIS.POS
        WHILE Y.ID:HIS.POS
            GOSUB OPEN.RECORD
            Y.OUT.ARRAY<-1> = Y.ID:"*":Y.FX.CTRPARTY:"*":Y.FX.CCY.BOUGHT:"*":Y.FX.AMT.BOUGHT:"*":Y.FX.CCY.SOLD:"*":Y.FX.AMT.SOLD:"*":Y.FX.VAL.DATE.BUY:"*":Y.FX.VAL.DT.SELL:"*":Y.FX.SPOT.RATE:"*":Y.DEAL.SLIP.ID
        REPEAT
    END

RETURN
*
************
OPEN.RECORD:
************
*
    R.REC.FOREX = ''
    R.REC.FX.DEAL.ID.LOG = ''
    Y.TEMP.ID = FIELD(Y.ID,";",1)
*
    CALL F.READ(FN.FOREX, Y.ID, R.REC.FOREX, F.FOREX, Y.ERR.FOREX)
*
    IF R.REC.FOREX EQ '' THEN
        CALL F.READ(FN.FOREX.HIS, Y.ID, R.REC.FOREX, F.FOREX.HIS, Y.ERR.FOREX.HIS)
        IF R.REC.FOREX NE '' THEN
            GOSUB GET.VALUES
        END
    END ELSE
        GOSUB GET.VALUES
    END
*
    CALL F.READ(FN.FX.DEAL.ID.LOG, Y.TEMP.ID, R.REC.FX.DEAL.ID.LOG, F.FX.DEAL.ID.LOG, Y.ERR.FX.DEAL.ID.LOG)
    Y.DEAL.SLIP.ID = ''

    IF NOT(R.REC.FX.DEAL.ID.LOG) THEN
        RETURN
    END

    LOCATE R.REC.FOREX<FX.CURR.NO> IN R.REC.FX.DEAL.ID.LOG<DEAL.ID.FX.CURR.NO,1> SETTING Y.CURR.POS THEN
        Y.DEAL.SLIP.ID = R.REC.FX.DEAL.ID.LOG<DEAL.ID.DEAL.SLIP.ID,Y.CURR.POS>
    END

*
RETURN
*
***********
GET.VALUES:
***********
*
    Y.FX.CTRPARTY      = R.REC.FOREX<FX.COUNTERPARTY>
    Y.FX.CCY.BOUGHT    = R.REC.FOREX<FX.CURRENCY.BOUGHT>
    Y.FX.AMT.BOUGHT    = R.REC.FOREX<FX.AMOUNT.BOUGHT>
    Y.FX.CCY.SOLD      = R.REC.FOREX<FX.CURRENCY.SOLD>
    Y.FX.AMT.SOLD      = R.REC.FOREX<FX.AMOUNT.SOLD>
    Y.FX.VAL.DATE.BUY  = R.REC.FOREX<FX.VALUE.DATE.BUY>
    Y.FX.VAL.DT.SELL   = R.REC.FOREX<FX.VALUE.DATE.SELL>
    Y.FX.SPOT.RATE     = R.REC.FOREX<FX.SPOT.RATE>
*
RETURN
*
END
