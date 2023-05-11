* @ValidationCode : MjotMTQyNTI4MzMzMjpDcDEyNTI6MTY4MjY1ODcwMzU0MjpJVFNTOi0xOi0xOjA6MDpmYWxzZTpOL0E6UjIxX0FNUi4wOi0xOi0x
* @ValidationInfo : Timestamp         : 28 Apr 2023 10:41:43
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : ITSS
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : R21_AMR.0
* @ValidationInfo : Copyright Temenos Headquarters SA 1993-2021. All rights reserved.
$PACKAGE APAP.TAM
*-----------------------------------------------------------------------------
* <Rating>-63</Rating>
*-----------------------------------------------------------------------------
SUBROUTINE REDO.V.VAL.TFS.TP
*-----------------------------------------------------------------------------
*DESCRIPTION:
*------------
* This is a input routine which check calls the funtion JEE
*
* Input/Output:
*--------------
* IN : -NA-
* OUT : -NA-
*
* Dependencies:
*---------------
* CALLS : @ID
* CALLED BY :
*
* Revision History:
*------------------------------------------------------------------------------------------
*   Date               who              Reference            Description
* 20-Jan-2011         Prabhu N       ODR-2009-10-0318        Initial Draft
** 18-04-2023 R22 Auto Conversion FM, VM, SM TO @FM, @VM, @SM
** 18-04-2023 Skanda R22 Manual Conversion - No changes
*------------------------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.T24.FUND.SERVICES
    $INSERT I_F.REDO.THIRDPRTY.PARAMETER
    $INSERT I_F.REDO.TRANS.CODE.PARAM
    $INSERT JBC.h

    IF VAL.TEXT NE "" THEN
        RETURN
    END

    GOSUB INIT
    GOSUB OPEN.FILES
    GOSUB PROCESS
RETURN
*---*
INIT:
*---*
    LOC.REF.APPLICATION = APPLICATION
    LOC.REF.FIELDS      = 'L.TT.CMPNY.ID'
    LOC.REF.POS         = ''
    Y.INTERFACE.REQ     = ""
RETURN
*---------*
OPEN.FILES:
*---------*
    FN.RTP                    = 'F.REDO.THIRDPRTY.PARAMETER'
    FN.REDO.TRANS.CODE.PARAM  = "F.REDO.TRANS.CODE.PARAM"
RETURN
*--------*
PROCESS:
*--------*
    CALL MULTI.GET.LOC.REF(LOC.REF.APPLICATION,LOC.REF.FIELDS,LOC.REF.POS)
    Y.FT.CMPNY.ID.POS   = LOC.REF.POS<1,1>
    Y.ID                = R.NEW(TFS.LOCAL.REF)<1,Y.FT.CMPNY.ID.POS>

    R.REDO.TRANS.CODE.PARAM = ''
    REDO.TRANS.CODE.PARAM.ERR = ''
    CALL CACHE.READ(FN.REDO.TRANS.CODE.PARAM,"SYSTEM",R.REDO.TRANS.CODE.PARAM,REDO.TRANS.CODE.PARAM.ERR)
    ACTIVATION.KEY = R.REDO.TRANS.CODE.PARAM<REDO.TS.ACTIVATION.KEY>
    Y.DELIMITER    = R.REDO.TRANS.CODE.PARAM<REDO.TS.DELIMITER>
    CALL CACHE.READ(FN.RTP,Y.ID,R.RTP,REDO.ERR)
    Y.COMP.ID       = R.RTP<REDO.TP.COMP.NAME>
    Y.INTERFACE.REQ = R.RTP<REDO.TP.INTERFACE.REQ>
    Y.METHOD.LIST   = R.RTP<REDO.TP.METHOD.DESC>
    Y.METHOD.NAMES  = R.RTP<REDO.TP.METHOD.NAME>
    Y.FT.BILL.NUM.VALUE=COMI
    CHANGE @VM TO @FM IN Y.METHOD.LIST
    CHANGE @VM TO @FM IN Y.METHOD.NAMES
    IF Y.INTERFACE.REQ EQ "Y" THEN
        LOCATE "getBalanceByNumber" IN Y.METHOD.LIST SETTING METHOD.POS THEN
            GOSUB PROCESS.EXEC
        END
    END
RETURN
*------------
PROCESS.EXEC:
*------------

    Y.METHOD=Y.METHOD.NAMES<METHOD.POS>
    BEGIN CASE
*--------------------------------------------------------------------------------------------------
*This case is to get the due from the orange company
*--------------------------------------------------------------------------------------------------
        CASE Y.METHOD EQ "getBalanceByNumber"
            EJB_ARGUMENT             = Y.METHOD:Y.DELIMITER:Y.FT.BILL.NUM.VALUE
            Y.RESPONSE               = CALLJEE(ACTIVATION.KEY,EJB_ARGUMENT)
            GOSUB HANDLE.PROCESS
*----------------------------------------------------------------------------------------------------
*This case is to get the due from CODETEL company
*----------------------------------------------------------------------------------------------------
        CASE Y.METHOD EQ  "ENQUIRY_CLIENT_POST_PAYMENT"
            Y.STATUS              = R.RTP<REDO.TP.PHONE.STATUS>
            Y.INTERFACE.USER.NAME = R.RTP<REDO.TP.INTERFACE.USER>
            Y.INTERFACE.PASS      = R.RTP<REDO.TP.INTERFACE.PASS>
            EJB_ARGUMENT=Y.METHOD:Y.DELIMITER:Y.STATUS:Y.DELIMITER:Y.FT.BILL.NUM.VALUE:Y.DELIMITER:Y.INTERFACE.USER.NAME:Y.DELIMITER:Y.INTERFACE.PASS
            Y.RESPONSE  = CALLJEE(ACTIVATION.KEY,EJB_ARGUMENT)
            GOSUB HANDLE.PROCESS
*---------------------------------------------------------------------------------------------------
*This is to get the due from generic interface
*--------------------------------------------------------------------------------------------------
        CASE Y.METHOD EQ "GetBalance"
            EJB_ARGUMENT=Y.METHOD:Y.DELIMITER:Y.COMP.ID:Y.DELIMITER:Y.FT.BILL.NUM.VALUE
            Y.RESPONSE         = CALLJEE(ACTIVATION.KEY,EJB_ARGUMENT)
            GOSUB HANDLE.PROCESS
    END CASE
RETURN
*--------------
HANDLE.PROCESS:
*--------------
    CHANGE Y.DELIMITER TO @FM IN EJB_ARGUMENT
    IF EJB_ARGUMENT<1> EQ 'SUCCESS' THEN
        R.NEW(TFS.AMOUNT.LCY)=EJB_ARGUMENT<3>
    END
    ELSE
        GOSUB HANDLE.FAIL
    END
RETURN
*------------
HANDLE.FAIL:
*------------
    IF EJB_ARGUMENT<1> EQ 'FAIL' THEN
        ETEXT=EJB_ARGUMENT<2>
        CALL STORE.END.ERROR
    END
    ELSE
        Y.RESPONSE.MSG = EJB_ARGUMENT[1,4]
        IF Y.RESPONSE.MSG EQ 'FAIL' THEN
            Y.MESSAGE=EJB_ARGUMENT<1>
            Y.RESP.ERR =FIELDS(Y.MESSAGE,':',2)
            INT.CODE = 'TPI001'
            INT.TYPE = 'ONLINE'
            BAT.NO = ''
            BAT.TOT = ''
            INFO.OR = ''
            INFO.DE = ''
            ID.PROC = ID.NEW
            MON.TP = '03'
            DESC = Y.RESP.ERR
            REC.CON = ''
            EX.USER = ''
            EX.PC = ''
            CALL REDO.INTERFACE.REC.ACT(INT.CODE,INT.TYPE,BAT.NO,BAT.TOT,INFO.OR,INFO.DE,ID.PROC,MON.TP,DESC,REC.CON,EX.USER,EX.PC)
            ETEXT = Y.RESP.ERR
            CALL STORE.END.ERROR
        END
    END
RETURN
END
