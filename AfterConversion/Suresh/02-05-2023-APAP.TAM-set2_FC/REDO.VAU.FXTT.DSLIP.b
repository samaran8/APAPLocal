* @ValidationCode : MjoxMjk2MTg1NjY0OkNwMTI1MjoxNjgxODkxODQzNDIxOnNhbWFyOi0xOi0xOjA6MDpmYWxzZTpOL0E6REVWXzIwMjEwOC4wOi0xOi0x
* @ValidationInfo : Timestamp         : 19 Apr 2023 13:40:43
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : samar
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : DEV_202108.0
* @ValidationInfo : Copyright Temenos Headquarters SA 1993-2021. All rights reserved.
$PACKAGE APAP.TAM
SUBROUTINE REDO.VAU.FXTT.DSLIP
****************************************************************
*-------------------------------------------------------------------------
* Company Name  : ASOCIACION POPULAR DE AHORROS Y PRESTAMOS
* Developed By  : VICTOR NAVA
* Program Name  : REDO.VAU.FXTT.DSLIP
*-------------------------------------------------------------------------

* Description   : This After Unau routine is for BUY and SELL deal slip generation triggered by conditional
*                 TELLER transaction type
* In parameter  : None
* out parameter : None
*--------------------------------------------------------------------------------
*
* MODIFICATION HISTORY:
* DATE            WHO              REFERENCE       DESCRIPTION
*
* 05-07-2012    Victor Nava        PACS00172912    Initial creation
* 04-06-2013    Vignesh Kumaar R   PACS00280705    DEALSLIP shouldn't be generated during AUTH stage
*----------------------------------------------------------------------------------
*Modification History
*DATE                       WHO                         REFERENCE                                   DESCRIPTION
*19-04-2023            Conversion Tool             R22 Auto Code conversion                      FM TO @FM, TNO TO C$T24.SESSION.NO
*19-04-2023              Samaran T                R22 Manual Code conversion                         No Changes
*------------------------------------------------------------------------------------------------------------------------------
*
    $INSERT I_COMMON
    $INSERT I_EQUATE
*
    $INSERT I_RC.COMMON
    $INSERT I_DEAL.SLIP.COMMON
    $INSERT I_GTS.COMMON
    $INSERT I_System
*
    $INSERT I_F.USER
    $INSERT I_F.TELLER
    $INSERT I_F.FOREX
    $INSERT I_F.FUNDS.TRANSFER
    $INSERT I_F.DEAL.SLIP.FORMAT
    $INSERT I_F.REDO.FOREX.SEQ.NUM
    $INSERT I_F.REDO.FOREX.SELL.SEQ.NUM

*
    GOSUB INITIALISE
    GOSUB OPEN.FILES
    GOSUB PRE.PROCESS
    GOSUB PROCESS
    GOSUB UPDATE.FXN.TABLE
*
    GOSUB CLEAR.WVARS
*
RETURN
* ======
PROCESS:
* ======

* Fix for PACS00280705 [DEALSLIP shouldn't be generated during AUTH stage]

    GET.VERSION.NAME = PGM.VERSION
    GET.FUNCTION = V$FUNCTION
*   2013JUL05 - VNL - S
* For FT buy/sell version FT.FXSN.PSLIP dealslip not to be triggered
    IF GET.VERSION.NAME EQ ',REDO.FCY.CASHIN' AND GET.FUNCTION EQ 'A' OR APPLICATION EQ 'TELLER' OR GET.VERSION.NAME EQ ',REDO.SELL.ACTR' OR GET.VERSION.NAME EQ ',REDO.BUY.ACTR' THEN
*   2013JUL05 - VNL - E
        RETURN
    END

    OFS$DEAL.SLIP.PRINTING = 1
    PRT.ADVICED.PRODUCED   = ""
    CALL PRODUCE.DEAL.SLIP(DEAL.SLIP.ID)

* End of Fix

RETURN
*
* =========
TT.PROCESS:
* =========
*
    IF R.NEW(TT.TE.DR.CR.MARKER) EQ 'CREDIT' THEN
        IF R.NEW(TT.TE.CURRENCY.1) NE LCCY THEN
            FX.TXN.TYPE = '2'       ;* SELL type transaction
        END ELSE
            FX.TXN.TYPE = '1'       ;* BUY type transaction
        END
    END
*
    IF R.NEW(TT.TE.DR.CR.MARKER) EQ 'DEBIT' THEN
        IF R.NEW(TT.TE.CURRENCY.1) NE LCCY THEN
            FX.TXN.TYPE = '1'       ;* BUY type transaction
        END ELSE
            FX.TXN.TYPE = '2'       ;* SELL type transaction
        END
    END
*
    IF FX.TXN.TYPE EQ '2' THEN  ;* SELL type transaction
        DEAL.SLIP.ID = 'TT.FXSN.SLIP2'
    END
*
    IF FX.TXN.TYPE EQ '1' THEN  ;* BUY type transaction
        DEAL.SLIP.ID = 'TT.FXSN.SLIP'
    END
*
RETURN
*
* ===============
UPDATE.FXN.TABLE:
* ===============
*
    FXSN.HC.ID = C$LAST.HOLD.ID
    CHANGE ',' TO @FM IN FXSN.HC.ID
    IF LEN(FXSN.HC.ID<1>) EQ '17' THEN
        Y.CNT.H = ''
        Y.CNT.H = DCOUNT(FXSN.HC.ID,@FM)
        FXSN.HC.ID = FXSN.HC.ID<Y.CNT.H>
    END ELSE
        FXSN.HC.ID = FXSN.HC.ID<2>
    END

    R.REDO.FOREX.SEQ.NUM   = ''
    REDO.FOREX.SEQ.NUM.ERR = ''
*
    IF FX.TXN.TYPE EQ '1' THEN
        CALL F.READ(FN.REDO.FOREX.SEQ.NUM,FXSN.ID,R.REDO.FOREX.SEQ.NUM,F.REDO.FOREX.SEQ.NUM,REDO.FOREX.SEQ.NUM.ERR)
        GOSUB FORM.BUY.SEQ.NUM
    END ELSE
        R.REDO.FOREX.SELL.SEQ.NUM = ''
        REDO.FOREX.SELL.SEQ.NUM.ERR = ''
        CALL F.READ(FN.REDO.FOREX.SELL.SEQ.NUM,FXSN.ID,R.REDO.FOREX.SELL.SEQ.NUM,F.REDO.FOREX.SELL.SEQ.NUM,REDO.FOREX.SELL.SEQ.NUM.ERR)
        GOSUB FORM.SELL.SEQ.NUM
    END
*
RETURN
*
*-----------------
FORM.BUY.SEQ.NUM:
*-----------------
*
    IF R.REDO.FOREX.SEQ.NUM THEN
*
        GOSUB GET.TIME.NOW
*
        R.REDO.FOREX.SEQ.NUM<REDO.FXSN.HOLD.CONTROL.ID> = FXSN.HC.ID
        R.REDO.FOREX.SEQ.NUM<REDO.FXSN.CURR.NO>        += 1
        R.REDO.FOREX.SEQ.NUM<REDO.FXSN.INPUTTER>        = C$T24.SESSION.NO:"_":OPERATOR      ;*R22 AUTO CODE CONVERSION
        R.REDO.FOREX.SEQ.NUM<REDO.FXSN.DATE.TIME>       = SYS.TIME.NOW
        R.REDO.FOREX.SEQ.NUM<REDO.FXSN.AUTHORISER>      = C$T24.SESSION.NO:"_":OPERATOR        ;*R22 AUTO CODE CONVERSION
        R.REDO.FOREX.SEQ.NUM<REDO.FXSN.CO.CODE>         = ID.COMPANY
        R.REDO.FOREX.SEQ.NUM<REDO.FXSN.DEPT.CODE>       = R.USER<EB.USE.DEPARTMENT.CODE>
*
        CALL F.WRITE(FN.REDO.FOREX.SEQ.NUM,FXSN.ID,R.REDO.FOREX.SEQ.NUM)
*
    END
*
RETURN
*
*-----------------
FORM.SELL.SEQ.NUM:
*------------------
*
    IF R.REDO.FOREX.SELL.SEQ.NUM THEN
*
        GOSUB GET.TIME.NOW
*
        R.REDO.FOREX.SELL.SEQ.NUM<REDO.FXSN.SELL.HOLD.CONTROL.ID> = FXSN.HC.ID
        R.REDO.FOREX.SELL.SEQ.NUM<REDO.FXSN.SELL.CURR.NO>        += 1
        R.REDO.FOREX.SELL.SEQ.NUM<REDO.FXSN.SELL.INPUTTER>        = C$T24.SESSION.NO:"_":OPERATOR         ;*R22 AUTO CODE CONVERSION
        R.REDO.FOREX.SELL.SEQ.NUM<REDO.FXSN.SELL.DATE.TIME>       = SYS.TIME.NOW
        R.REDO.FOREX.SELL.SEQ.NUM<REDO.FXSN.SELL.AUTHORISER>      = C$T24.SESSION.NO:"_":OPERATOR       ;*R22 AUTO CODE CONVERSION
        R.REDO.FOREX.SELL.SEQ.NUM<REDO.FXSN.SELL.CO.CODE>         = ID.COMPANY
        R.REDO.FOREX.SELL.SEQ.NUM<REDO.FXSN.SELL.DEPT.CODE>       = R.USER<EB.USE.DEPARTMENT.CODE>
*
        CALL F.WRITE(FN.REDO.FOREX.SELL.SEQ.NUM,FXSN.ID,R.REDO.FOREX.SELL.SEQ.NUM)
*
    END
*
RETURN
*
* ===========
GET.TIME.NOW:
* ===========
*
    SYS.TIME.NOW  = OCONV(DATE(),"D-")
    SYS.TIME.NOW  = SYS.TIME.NOW[9,2]:SYS.TIME.NOW[1,2]:SYS.TIME.NOW[4,2]
    SYS.TIME.NOW := TIMEDATE()[1,2]:TIMEDATE()[4,2]
*
RETURN
*
* ==========
CLEAR.WVARS:
* ==========
*
    CALL System.setVariable("CURRENT.VAR.DETAILS","")
*
    CALL System.setVariable("CURRENT.CLIENTE.APAP","")
*
RETURN
*
* =========
INITIALISE:
* =========
*
    PROCESS.GOAHEAD        = 1
    DEAL.SLIP.ID           = ''
    FX.TXN.REF             = ''
    FXSN.ID                = ''
*
    APPL.ARR = "FUNDS.TRANSFER":@FM:"TELLER":@FM:"FOREX"
*
    FT.LOC.FIELDS = "L.FT.FXSN.NUM"
    TT.LOC.FIELDS = "L.TT.FXSN.NUM"
    FX.LOC.FIELDS = "L.FX.FXSN.NUM"
*
    FIELDS.NAME.ARR = FT.LOC.FIELDS:@FM:TT.LOC.FIELDS:@FM:FX.LOC.FIELDS
    FIELD.POS.ARR   = ''
*
    CALL MULTI.GET.LOC.REF(APPL.ARR,FIELDS.NAME.ARR,FIELD.POS.ARR)
*
    L.FT.FXSN.NUM.POS   = FIELD.POS.ARR<1,1>
*
    L.TT.FXSN.NUM.POS   = FIELD.POS.ARR<2,1>
*
    L.FX.FXSN.NUM.POS   = FIELD.POS.ARR<3,1>
*
RETURN
*
* =========
OPEN.FILES:
* =========
*
    FN.REDO.FOREX.SEQ.NUM = 'F.REDO.FOREX.SEQ.NUM'
    F.REDO.FOREX.SEQ.NUM  = ''
*
    FN.REDO.FOREX.SELL.SEQ.NUM = 'F.REDO.FOREX.SELL.SEQ.NUM'
    F.REDO.FOREX.SELL.SEQ.NUM  = ''
*
    CALL OPF(FN.REDO.FOREX.SEQ.NUM,F.REDO.FOREX.SEQ.NUM)
*
    CALL OPF(FN.REDO.FOREX.SELL.SEQ.NUM,F.REDO.FOREX.SELL.SEQ.NUM)
*
RETURN
*
* ==========
PRE.PROCESS:
* ==========
*
    BEGIN CASE
        CASE APPLICATION EQ 'FOREX'
*
            FX.TXN.REF = R.NEW(FX.LOCAL.REF)<1,L.FX.FXSN.NUM.POS>
            DEAL.SLIP.ID='FX.FXSN.PSLIP'
            IF FX.TXN.REF EQ "" THEN
                RETURN
            END
*
            FXSN.ID = FIELD(FX.TXN.REF,"-",2)
*
        CASE APPLICATION EQ 'FUNDS.TRANSFER'
*
            FT.TXN.REF = R.NEW(FT.LOCAL.REF)<1,L.FT.FXSN.NUM.POS>
            DEAL.SLIP.ID='FT.FXSN.PSLIP'
            IF FT.TXN.REF EQ "" THEN
                RETURN
            END
*
            FXSN.ID = FIELD(FT.TXN.REF,"-",2)
*
        CASE APPLICATION EQ 'TELLER'
*
            FX.TXN.TYPE     = ''
            TT.TXN.REF = R.NEW(TT.TE.LOCAL.REF)<1,L.TT.FXSN.NUM.POS>
            IF TT.TXN.REF EQ "" THEN
                RETURN
            END
*
            FXSN.ID = FIELD(TT.TXN.REF,"-",2)
            GOSUB TT.PROCESS
*
    END CASE
*
RETURN
*
END
