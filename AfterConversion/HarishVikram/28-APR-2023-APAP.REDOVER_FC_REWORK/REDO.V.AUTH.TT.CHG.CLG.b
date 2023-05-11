* @ValidationCode : MjozMDAwMDczODY6Q3AxMjUyOjE2ODI0MTIzNDAzNTg6SGFyaXNodmlrcmFtQzotMTotMTowOjE6ZmFsc2U6Ti9BOkRFVl8yMDIxMDguMDotMTotMQ==
* @ValidationInfo : Timestamp         : 25 Apr 2023 14:15:40
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : HarishvikramC
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : true
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : DEV_202108.0
* @ValidationInfo : Copyright Temenos Headquarters SA 1993-2021. All rights reserved.
$PACKAGE APAP.REDOVER
SUBROUTINE REDO.V.AUTH.TT.CHG.CLG
*----------------------------------------------------------------------------
* Description:
***************
*  This routine is an authorisation routine attached to VERISION
*  of teller TELLER,REDO.CHQ.CLG updates AC.LOCKED.EVENTS file and
*  REDO.H.CLEARING.OUTWARD file
*
*------------------------------------------------------------------------------------------
* * Input / Output
*
* --------------
* IN     : -NA-
* OUT    : -NA-
*------------------------------------------------------------------------------------------
* COMPANY NAME : APAP
* DEVELOPED BY : NATCHIMUTHU.P
* PROGRAM NAME : REDO.V.AUTH.TT.CHG.CLG
*------------------------------------------------------------------------------------------
* Modification History :
*-----------------------
* DATE             WHO                     REFERENCE                     DESCRIPTION
* 08.07.2010      NATCHIMUTHU.P        ODR-2010-02-0001                INITIAL CREATION
*10-04-2023       Conversion Tool      R22 Auto Code conversion          No Changes
*10-04-2023       Samaran T             R22 Manual Code Conversion       No Changes
 
* -----------------------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.TELLER
    $INSERT I_F.USER

    $INSERT I_F.AC.LOCKED.EVENTS
    $INSERT I_F.REDO.H.CLEARING.OUTWARD

    GOSUB INIT
    GOSUB OPENFILES
    GOSUB PROCESS
    GOSUB TEMPLATE

RETURN

INIT:
*******
* DEBUG
    R.AC.LOCKED.EVENTS=''
    Y.APPL = "AC.LOCKED.EVENTS"
    Y.FIELDS = "L.AC.CLR.REF"
    Y.POSS = ""
    CALL MULTI.GET.LOC.REF(Y.APPL,Y.FIELDS,Y.POSS)
    Y.AC.CLR.REF.POS = Y.POSS<1,1>

    R.REDO.H.CLEARING.OUTWARD=''
    Y.APPL = "TELLER"
    Y.FIELDS = "L.TT.NO.OF.CHQ"
    Y.POSI = ""
    CALL MULTI.GET.LOC.REF(Y.APPL,Y.FIELDS,Y.POSI)
    Y.L.TT.NO.OF.CHQ.POS = Y.POSI<1,1>

RETURN
*----------------------------------------------------------------------------------------
OPENFILES:
**********
    FN.TELLER='F.TELLER'
    F.TELLER=''
    CALL OPF(FN.TELLER,F.TELLER)

    FN.AC.LOCKED.EVENTS='F.AC.LOCKED.EVENTS'
    F.AC.LOCKED.EVENTS=''
    CALL OPF(FN.AC.LOCKED.EVENTS,F.AC.LOCKED.EVENTS)

    FN.REDO.H.CLEARING.OUTWARD = 'F.REDO.H.CLEARING.OUTWARD'
    F.REDO.H.CLEARING.OUTWARD =''
    CALL OPF(FN.REDO.H.CLEARING.OUTWARD,F.REDO.H.CLEARING.OUTWARD)

RETURN
*-----------------------------------------------------------------------------------------
PROCESS:
*********

    Y.AMOUNT=R.NEW(TT.TE.AMOUNT.LOCAL.2)
    R.AC.LOCKED.EVENTS<AC.LCK.ACCOUNT.NUMBER>=R.NEW(TT.TE.ACCOUNT.2)
    R.AC.LOCKED.EVENTS<AC.LCK.FROM.DATE>=R.NEW(TT.TE.VALUE.DATE.2)
    R.AC.LOCKED.EVENTS<AC.LCK.LOCKED.AMOUNT>=R.NEW(TT.TE.AMOUNT.LOCAL.1)
    R.AC.LOCKED.EVENTS<AC.LCK.LOCAL.REF,Y.AC.CLR.REF.POS>=ID.NEW

    OFS.SOURCE.ID = 'OFSUPDATE'

    APPLICATION.NAME = 'AC.LOCKED.EVENTS'
    TRANS.FUNC.VAL = 'I'
    TRANS.OPER.VAL = 'PROCESS'
    APPLICATION.NAME.VERSION = 'AC.LOCKED.EVENTS,TEST'
    NO.AUT = '0'
    OFS.MSG.ID = ''
    APPLICATION.ID =''
    OFS.POST.MSG = ''

    CALL OFS.BUILD.RECORD(APPLICATION.NAME,TRANS.FUNC.VAL,TRANS.OPER.VAL,APPLICATION.NAME.VERSION,"",NO.AUT,APPLICATION.ID,R.AC.LOCKED.EVENTS,OFS.REQ.MSG)
    CALL OFS.POST.MESSAGE(OFS.REQ.MSG,OFS.MSG.ID,OFS.SOURCE.ID,OFS.ERR)

RETURN
*-------------------------------------------------------------------------------------------
TEMPLATE:
*********

    Y.TRANS.REF=ID.NEW

    CALL F.READ(FN.REDO.H.CLEARING.OUTWARD,Y.TRANS.REF,R.REDO.H.CLEARING.OUTWARD,F.REDO.H.CLEARING.OUTWARD,Y.ERR)
    IF Y.ERR THEN
        R.REDO.H.CLEARING.OUTWARD<REDO.CLR.OUT.AMOUNT>=R.NEW(TT.TE.AMOUNT.LOCAL.1)
        R.REDO.H.CLEARING.OUTWARD<REDO.CLR.OUT.TRANS.REFERENCE>= ID.NEW
        R.REDO.H.CLEARING.OUTWARD<REDO.CLR.OUT.ACCOUNT.NUMBER>=R.NEW(TT.TE.ACCOUNT.2)
        R.REDO.H.CLEARING.OUTWARD<REDO.CLR.OUT.CURRENCY>=R.NEW(TT.TE.CURRENCY.1)
        R.REDO.H.CLEARING.OUTWARD<REDO.CLR.OUT.NO.OF.CHEQUE>=R.NEW(TT.TE.LOCAL.REF)<1,Y.L.TT.NO.OF.CHQ.POS>
        R.REDO.H.CLEARING.OUTWARD<REDO.CLR.OUT.TELLER.ID>=R.NEW(TT.TE.TELLER.ID.1)
        R.REDO.H.CLEARING.OUTWARD<REDO.CLR.OUT.USER.ID>=OPERATOR
        R.REDO.H.CLEARING.OUTWARD<REDO.CLR.OUT.CO.CODE>=ID.COMPANY
        R.REDO.H.CLEARING.OUTWARD<REDO.CLR.OUT.DEPT.CODE>=R.USER<EB.USE.DEPARTMENT.CODE>
        R.REDO.H.CLEARING.OUTWARD<REDO.CLR.OUT.TRANSFER>=''
    END
    CALL F.WRITE(FN.REDO.H.CLEARING.OUTWARD,Y.TRANS.REF,R.REDO.H.CLEARING.OUTWARD)

    IF V$FUNCTION EQ 'A' THEN
        OFS$DEAL.SLIP.PRINTING = 1
        Y.DEAL.ID = 'TT.CHQ.PSLIP'
        CALL PRODUCE.DEAL.SLIP(Y.DEAL.ID)
    END

RETURN
END
*-----------------------------------------------------------------------------------------------
