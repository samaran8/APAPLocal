* @ValidationCode : MjotMTc1MTI4MjUwMTpDcDEyNTI6MTY4MjQxMjMzMzQxOTpIYXJpc2h2aWtyYW1DOi0xOi0xOjA6MTpmYWxzZTpOL0E6REVWXzIwMjEwOC4wOi0xOi0x
* @ValidationInfo : Timestamp         : 25 Apr 2023 14:15:33
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
SUBROUTINE REDO.V.AUT.CHQ.BR.LOG.UPDATE
*-----------------------------------------------------------------------------
*----------------------------------------------------------------------------------------------------
* DESCRIPTION : This Auth routine is used to Update the Log CR.CONTACT.LOG table
*
*-----------------------------------------------------------------------------------------------------
*-----------------------------------------------------------------------------------------------------
* * Input / Output
* --------------
* IN     : -NA-
* OUT    : -NA-
*-----------------------------------------------------------------------------------------------------
* COMPANY NAME : APAP
* DEVELOPED BY : RENUGADEVI B
* PROGRAM NAME : REDO.V.AUT.CHQ.BR.LOG.UPDATE
*-----------------------------------------------------------------------------------------------------
* Modification History :
*-----------------------
* DATE              WHO                            REFERENCE                DESCRIPTION
* 25-AUG-2010       RENUGADEVI B             ODR-2009-12-0283            INITIAL CREATION
*06-04-2023         Conversion Tool        R22 Auto Code conversion          No Changes
*06-04-2023         Samaran T               R22 Manual Code Conversion       No Changes
* ----------------------------------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.CR.CONTACT.LOG
    $INSERT I_F.ACCOUNT
    $INSERT I_F.REDO.H.SOLICITUD.CK

    GOSUB INIT
    GOSUB PROCESS
RETURN
*****
INIT:
*****
    R.CR.CONTACT.LOG             = ''
    FN.CR.CONTACT.LOG            = 'F.CR.CONTACT.LOG'
    F.CR.CONTACT.LOG             = ''
    CALL OPF(FN.CR.CONTACT.LOG, F.CR.CONTACT.LOG)

    FN.REDO.H.SOLICITUD.CK       = 'F.REDO.H.SOLICITUD.CK'
    F.REDO.H.SOLICITUD.CK        = ''
    CALL OPF(FN.REDO.H.SOLICITUD.CK,F.REDO.H.SOLICITUD.CK)

    FN.ACCOUNT                   = 'F.ACCOUNT'
    F.ACCOUNT                    = ''
    CALL OPF(FN.ACCOUNT, F.ACCOUNT)

    LREF.APPL                    = 'CR.CONTACT.LOG'
    LREF.FIELDS                  = 'L.CR.SER.REQ'
    LREF.POS                     = ''
    CALL MULTI.GET.LOC.REF(LREF.APPL, LREF.FIELDS, LREF.POS)
    L.CR.SER.REQ.POS             = LREF.POS<1,1>

RETURN
********
PROCESS:
********

    Y.ID                  = ID.NEW
    Y.SAM.TIME            = TIMEDATE()
    Y.TIME                = Y.SAM.TIME[1,5]
    Y.ACCT.ID             = R.NEW(REDO.H.SOL.ACCOUNT)
    CALL F.READ(FN.ACCOUNT, Y.ACCT.ID, R.ACCOUNT, F.ACCOUNT, ACC.ERR)
    IF R.ACCOUNT THEN
        Y.CUST.ID             = R.ACCOUNT<AC.CUSTOMER>
    END

    Y.INPUTTER            = R.NEW(REDO.H.SOL.INPUTTER)
    Y.USER                = FIELD(Y.INPUTTER,'-',2)
    GOSUB UPDATE.LOG
RETURN

***********
UPDATE.LOG:
***********

    CALL F.READ(FN.CR.CONTACT.LOG,Y.LOG.ID,R.CR.CONTACT.LOG,F.CR.CONTACT.LOG,CR.ERR)

    R.CR.CONTACT.LOG<CR.CONT.LOG.CONTACT.CLIENT>                 = Y.CUST.ID
    R.CR.CONTACT.LOG<CR.CONT.LOG.CONTACT.STATUS>                 = "NEW"
    R.CR.CONTACT.LOG<CR.CONT.LOG.CONTACT.TYPE>                   = "AUTOMATICO"
    R.CR.CONTACT.LOG<CR.CONT.LOG.CONTACT.DESC>                   = "CHECKBOOK"
    R.CR.CONTACT.LOG<CR.CONT.LOG.CONTACT.CHANNEL>                = "BRANCH"
    R.CR.CONTACT.LOG<CR.CONT.LOG.CONTACT.DATE>                   = TODAY
    R.CR.CONTACT.LOG<CR.CONT.LOG.CONTACT.TIME>                   = Y.TIME
    R.CR.CONTACT.LOG<CR.CONT.LOG.CONTRACT.ID>                    = Y.ID
    R.CR.CONTACT.LOG<CR.CONT.LOG.CONTACT.STAFF>                  = Y.USER
    R.CR.CONTACT.LOG<CR.CONT.LOG.LOCAL.REF,L.CR.SER.REQ.POS>    = "CHECKBOOK"

    OFS.SOURCE.ID            = 'OFS.LOG.UPDATE'
    APPLICATION.NAME         = 'CR.CONTACT.LOG'
    TRANS.FUNC.VAL           = 'I'
    TRANS.OPER.VAL           = 'PROCESS'
    APPLICATION.NAME.VERSION = 'CR.CONTACT.LOG,LOG.INTERACT'
    NO.AUT                   = '0'
    OFS.MSG.ID               = ''
    APPLICATION.ID           = ''
    OFS.POST.MSG             = ''

    CALL OFS.BUILD.RECORD(APPLICATION.NAME,TRANS.FUNC.VAL,TRANS.OPER.VAL,APPLICATION.NAME.VERSION,"",NO.AUT,APPLICATION.ID,R.CR.CONTACT.LOG,OFS.REQ.MSG)
    CALL OFS.POST.MESSAGE(OFS.REQ.MSG,OFS.MSG.ID,OFS.SOURCE.ID,OFS.ERR)

RETURN
END
