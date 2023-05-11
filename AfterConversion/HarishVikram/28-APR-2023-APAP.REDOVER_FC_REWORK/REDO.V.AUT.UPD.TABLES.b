* @ValidationCode : MjoxNTk4MDkwOTI5OkNwMTI1MjoxNjgyNDEyMzM4MzAwOkhhcmlzaHZpa3JhbUM6LTE6LTE6MDoxOmZhbHNlOk4vQTpSMjFfQU1SLjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 25 Apr 2023 14:15:38
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : HarishvikramC
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : true
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : R21_AMR.0
* @ValidationInfo : Copyright Temenos Headquarters SA 1993-2021. All rights reserved.
$PACKAGE APAP.REDOVER
SUBROUTINE REDO.V.AUT.UPD.TABLES
*---------------------------------------------------------------------------
* COMPANY NAME: ASOCIACION POPULAR DE AHORROS Y PRESTAMOS
* DEVELOPED BY: H GANESH
* PROGRAM NAME: REDO.V.AUT.UPD.TABLES
* ODR NO      : ODR-2009-12-0285
*----------------------------------------------------------------------
* DESCRIPTION: This routine is Auth routine attached to VERSION.CONTROL
*
*IN PARAMETER: NA
*OUT PARAMETER: NA
*LINKED WITH: TELLER & FT
*----------------------------------------------------------------------
* Modification History :
*-----------------------
*   DATE           WHO               REFERENCE            DESCRIPTION
*   19.02.2010     H GANESH          ODR-2009-12-0285     INITIAL CREATION
*   11-04-2011     Bharath G         PACS00032271         Update user and concept in REDO.PRINT.CHQ.LIST table
*   09-05-2011     Bharath G         PACS00023918         The BENEFICIARY  field should be multivalue field
*   12-05-2011     Bharath G         PACS00023955         Account numbers swaped in TT
*   01-07-2011     H GANESH          PACS00072695         Added logic for FUNDS.TRANSFER,CHQ.OTHERS.DEPOSIT
*   26-JAN-2012    J.COSTA.C.        PACS00163682         Take out HARDCODE
*   12-09-2013     Vignesh Kumaar R  PACS00312873         CHEQUE.INTERNAL ACCOUNT CHANGES
*Modification history
*Date                Who               Reference                  Description
*12-04-2023      conversion tool     R22 Auto code conversion    TNO:'_':OPERATOR TO C$T24.SESSION.NO:'_':OPERATOR,VM TO @VM,SM TO @SM,FM TO @FM
*12-04-2023      Mohanraj R          R22 Manual code conversion   No changes
*----------------------------------------------------------------------
*
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.TELLER
    $INSERT I_F.FUNDS.TRANSFER
    $INSERT I_F.USER
    $INSERT I_F.AZ.ACCOUNT
*
    $INSERT I_F.REDO.ADMIN.CHQ.DETAILS
    $INSERT I_F.REDO.H.ADMIN.CHEQUES
    $INSERT I_F.REDO.L.ADMIN.DEP.DETS
    $INSERT I_System
*
    GOSUB INIT
    GOSUB OPENFILES
    GOSUB PROCESS
*
RETURN
*
*----------------------------------------------------------------------
INIT:
*----------------------------------------------------------------------
*
    W.STATUS = ""
*
    FN.REDO.ADMIN.CHQ.DETAILS = 'F.REDO.ADMIN.CHQ.DETAILS'
    F.REDO.ADMIN.CHQ.DETAILS  = ''
    FN.REDO.H.ADMIN.CHEQUES   = 'F.REDO.H.ADMIN.CHEQUES'
    F.REDO.H.ADMIN.CHEQUES    = ''


    FN.AZ.ACCOUNT = 'F.AZ.ACCOUNT'
    F.AZ.ACCOUNT  = ''

    FN.AZ.ACCOUNT.HIST = 'F.AZ.ACCOUNT$HIS'
    F.AZ.ACCOUNT.HIST  = ''

    FN.REDO.L.ADMIN.DEP.DETS = 'F.REDO.L.ADMIN.DEP.DETS'
    F.REDO.L.ADMIN.DEP.DETS = ''
    CALL OPF(FN.REDO.L.ADMIN.DEP.DETS, F.REDO.L.ADMIN.DEP.DETS)

*
    LOC.REF.FIELDS      = 'L.TT.BENEFICIAR' : @VM : 'L.TT.BASE.AMT' : @FM : "BENEFIC.NAME" : @VM : "TRANSACTION.REF"
    LOC.REF.POS         = ''
    LOC.REF.APPLICATION = "TELLER" : @FM : "FUNDS.TRANSFER"

    CALL MULTI.GET.LOC.REF(LOC.REF.APPLICATION,LOC.REF.FIELDS,LOC.REF.POS)

    POS.L.TT.BENEFICIAR = LOC.REF.POS<1,1>
    POS.L.TT.BASE.AMT   = LOC.REF.POS<1,2>
    POS.BENEFIC.NAME    = LOC.REF.POS<2,1>
    TR.REF.POS          = LOC.REF.POS<2,2>

RETURN
*
*----------------------------------------------------------------------
OPENFILES:
*----------------------------------------------------------------------
*
    CALL OPF(FN.REDO.ADMIN.CHQ.DETAILS,F.REDO.ADMIN.CHQ.DETAILS)
    CALL OPF(FN.REDO.H.ADMIN.CHEQUES,F.REDO.H.ADMIN.CHEQUES)
    CALL OPF(FN.AZ.ACCOUNT,F.AZ.ACCOUNT)
    CALL OPF(FN.AZ.ACCOUNT.HIST,F.AZ.ACCOUNT.HIST)
*
RETURN
*
*----------------------------------------------------------------------
PROCESS:
*----------------------------------------------------------------------
*
    IF APPLICATION EQ 'TELLER' THEN
        GOSUB TELLER
    END
    IF APPLICATION EQ 'FUNDS.TRANSFER' THEN
        GOSUB FT
    END

    GOSUB UPDATE.DEP.ADMIN.CHECK

*
RETURN
*
*----------------------------------------------------------------------
UPDATE.DEP.ADMIN.CHECK:
*----------------------

    Y.DEP.ADM.FILE.ID = ID.NEW


    E.BACK = E
    Y.CURRENT.DEPNO = System.getVariable("CURRENT.DEP.ACC")
    IF E EQ "EB-UNKNOWN.VARIABLE" THEN ;*R22 Auto code conversion-START
        Y.CURRENT.DEPNO = ""
    END ;*R22 Auto code conversion-END


    IF Y.CURRENT.DEPNO EQ "CURRENT.DEP.ACC" THEN
        LOCATE 'EB-UNKNOWN.VARIABLE' IN E<1,1> SETTING POS THEN
            E = E.BACK
        END
        Y.CURRENT.DEPNO = ''
    END


    IF Y.CURRENT.DEPNO THEN     ;*If it is a deposit capitalisation then this curr variable contains Deposit No.
        R.ADMIN.DEP.REC<RE.ADM.DEP.AZ.ID> = Y.CURRENT.DEPNO
        CALL F.READ(FN.AZ.ACCOUNT,Y.CURRENT.DEPNO,R.AZ.ACCOUNT,F.AZ.ACCOUNT,AZ.ACCOUNT.ERR)
        IF R.AZ.ACCOUNT THEN
            R.ADMIN.DEP.REC<RE.ADM.DEP.CATEGORY> = R.AZ.ACCOUNT<AZ.CATEGORY>
        END ELSE
            CALL EB.READ.HISTORY.REC(F.AZ.ACCOUNT.HIST,Y.CURRENT.DEPNO,R.AZ.ACCOUNT.HIST,AZ.HIST.ERR)
            R.ADMIN.DEP.REC<RE.ADM.DEP.CATEGORY> = R.AZ.ACCOUNT.HIST<AZ.CATEGORY>
        END
        R.ADMIN.DEP.REC<RE.ADM.DEP.CHQ.ISS.DATE> = TODAY
        R.ADMIN.DEP.REC<RE.ADM.DEP.CHQ.NNUMBER> = R.NEW(FT.CREDIT.THEIR.REF)
        R.ADMIN.DEP.REC<RE.ADM.DEP.ISSUER> = R.NEW(FT.DEBIT.AMOUNT)
        R.ADMIN.DEP.REC<RE.ADM.DEP.ISSUER> = FIELD(R.NEW(FT.INPUTTER), '_', 2,1)
        R.ADMIN.DEP.REC<RE.ADM.DEP.AUTHORISER> = FIELD(R.NEW(FT.AUTHORISER), '_', 2,1)
        R.ADMIN.DEP.REC<RE.ADM.DEP.CHQ.AMOUNT> = R.NEW(FT.DEBIT.AMOUNT)
        R.ADMIN.DEP.REC<RE.ADM.DEP.CURRENCY> = R.NEW(FT.DEBIT.CURRENCY)
        R.ADMIN.DEP.REC<RE.ADM.DEP.CO.CODE> = R.NEW(FT.CO.CODE)
        R.ADMIN.DEP.REC<RE.ADM.DEP.BENEF.NAME> = R.NEW(FT.LOCAL.REF)<1, POS.BENEFIC.NAME>

        CALL F.WRITE(FN.REDO.L.ADMIN.DEP.DETS, ID.NEW, R.ADMIN.DEP.REC)
    END

RETURN
*----------------------------------------------------------------------
TELLER:
*----------------------------------------------------------------------
*

    Y.REDO.H.ADMIN.CHQ.ID       = R.NEW(TT.TE.NARRATIVE.1)<1,1>
    Y.REDO.ADMIN.CHQ.DETAILS.ID = R.NEW(TT.TE.CHEQUE.NUMBER)

* PACS00023955 - S
    R.REDO.ADMIN.CHQ.DETAILS<ADMIN.CHQ.DET.CHEQUE.INT.ACCT> = R.NEW(TT.TE.ACCOUNT.1)
*    R.REDO.ADMIN.CHQ.DETAILS<ADMIN.CHQ.DET.CHEQUE.INT.ACCT> = R.NEW(TT.TE.ACCOUNT.2)
* PACS00023955 - E

*    R.REDO.ADMIN.CHQ.DETAILS<ADMIN.CHQ.DET.STATUS>='ISSUED'
* PACS00072695 -> Added logic for version FUNDS.TRANSFER,CHQ.OTHERS.DEPOSIT
    IF R.NEW(TT.TE.AMOUNT.LOCAL.1) NE '' THEN
        R.REDO.ADMIN.CHQ.DETAILS<ADMIN.CHQ.DET.AMOUNT>  = R.NEW(TT.TE.AMOUNT.LOCAL.1)<1,1>
    END
    IF R.NEW(TT.TE.AMOUNT.FCY.1) NE '' THEN
        R.REDO.ADMIN.CHQ.DETAILS<ADMIN.CHQ.DET.AMOUNT>  = R.NEW(TT.TE.AMOUNT.FCY.1)<1,1>
    END
* PACS00023955 - S
    R.REDO.ADMIN.CHQ.DETAILS<ADMIN.CHQ.DET.ISSUE.ACCOUNT> = R.NEW(TT.TE.ACCOUNT.2)
*    R.REDO.ADMIN.CHQ.DETAILS<ADMIN.CHQ.DET.ISSUE.ACCOUNT>   = R.NEW(TT.TE.ACCOUNT.1)
* PACS00023955 - E
    R.REDO.ADMIN.CHQ.DETAILS<ADMIN.CHQ.DET.ISSUE.DATE>      = TODAY
    R.REDO.ADMIN.CHQ.DETAILS<ADMIN.CHQ.DET.COMPANY.CODE>    = ID.COMPANY
    Y.BENEFI = R.NEW(TT.TE.LOCAL.REF)<1,POS.L.TT.BENEFICIAR>  ;* PACS00023918
    CHANGE @SM TO @VM IN Y.BENEFI ;* PACS00023918
    R.REDO.ADMIN.CHQ.DETAILS<ADMIN.CHQ.DET.BENEFICIARY>     = Y.BENEFI  ;* PACS00023918
    R.REDO.ADMIN.CHQ.DETAILS<ADMIN.CHQ.DET.TRANS.REFERENCE> = ID.NEW
    R.REDO.ADMIN.CHQ.DETAILS<ADMIN.CHQ.DET.USER>            = OPERATOR
*
    IF R.NEW(TT.TE.RECORD.STATUS)[1,1] EQ "R" THEN
        CALL F.READ(FN.REDO.ADMIN.CHQ.DETAILS,Y.REDO.ADMIN.CHQ.DETAILS.ID,R.REDO.ADMIN.CHQ.DETAILS,F.REDO.ADMIN.CHQ.DETAILS,ERR.READ)
        R.REDO.ADMIN.CHQ.DETAILS<ADMIN.CHQ.DET.STATUS>  = 'CANCELLED'
        W.STATUS = "CANCELLED"
    END ELSE
        R.REDO.ADMIN.CHQ.DETAILS<ADMIN.CHQ.DET.STATUS> = 'ISSUED'
        W.STATUS = "ISSUED"
    END

* added on 29/feb/2012
    R.REDO.ADMIN.CHQ.DETAILS<ADMIN.CHQ.DET.CHQ.SEQ.NUM> = Y.REDO.H.ADMIN.CHQ.ID
*
    GOSUB RECORD.WRITE
*
RETURN
*
*----------------------------------------------------------------------
FT:
*----------------------------------------------------------------------
* IF Application equals FUNDS.TRANSFER

    Y.BENEF = R.NEW(FT.LOCAL.REF)<1,POS.BENEFIC.NAME>
    CHANGE @SM TO @VM IN Y.BENEF  ;* PACS00023918
*
    Y.REDO.ADMIN.CHQ.DETAILS.ID                             = R.NEW(FT.CREDIT.THEIR.REF)<1,1>
    Y.REDO.H.ADMIN.CHQ.ID                                   = R.NEW(FT.LOCAL.REF)<1,TR.REF.POS>
    R.REDO.ADMIN.CHQ.DETAILS<ADMIN.CHQ.DET.CHEQUE.INT.ACCT> = R.NEW(FT.CREDIT.ACCT.NO)

    R.REDO.ADMIN.CHQ.DETAILS<ADMIN.CHQ.DET.AMOUNT>          = R.NEW(FT.DEBIT.AMOUNT)
    R.REDO.ADMIN.CHQ.DETAILS<ADMIN.CHQ.DET.ISSUE.ACCOUNT>   = R.NEW(FT.DEBIT.ACCT.NO)


    R.REDO.ADMIN.CHQ.DETAILS<ADMIN.CHQ.DET.ISSUE.DATE>      = TODAY
    R.REDO.ADMIN.CHQ.DETAILS<ADMIN.CHQ.DET.COMPANY.CODE>    = ID.COMPANY
    R.REDO.ADMIN.CHQ.DETAILS<ADMIN.CHQ.DET.BENEFICIARY>     = Y.BENEF   ;* PACS00023918
    R.REDO.ADMIN.CHQ.DETAILS<ADMIN.CHQ.DET.TRANS.REFERENCE> = ID.NEW
    R.REDO.ADMIN.CHQ.DETAILS<ADMIN.CHQ.DET.USER>            = OPERATOR
*
*PACS00293633 - S
*IF R.NEW(FT.RECORD.STATUS)[1,1] EQ "R" THEN
    IF R.NEW(FT.TRANSACTION.TYPE) EQ "AC95" THEN
        CALL F.READ(FN.REDO.ADMIN.CHQ.DETAILS,Y.REDO.ADMIN.CHQ.DETAILS.ID,R.REDO.ADMIN.CHQ.DETAILS,F.REDO.ADMIN.CHQ.DETAILS,ERR.ADMIN.CHQ)
        IF PGM.VERSION NE ",REDO.REINSTATE" THEN
            R.REDO.ADMIN.CHQ.DETAILS<ADMIN.CHQ.DET.STATUS>  = 'CANCELLED'
            W.STATUS = "CANCELLED"
        END ELSE
            R.REDO.ADMIN.CHQ.DETAILS<ADMIN.CHQ.DET.STATUS>  = 'REINSTATED'
            W.STATUS = "PAID"
        END
    END ELSE
        R.REDO.ADMIN.CHQ.DETAILS<ADMIN.CHQ.DET.STATUS> = 'ISSUED'
        W.STATUS = "ISSUED"
    END
*PACS00293633 - E
*
    R.REDO.ADMIN.CHQ.DETAILS<ADMIN.CHQ.DET.CHQ.SEQ.NUM> = Y.REDO.H.ADMIN.CHQ.ID
    GOSUB RECORD.WRITE
*
RETURN
*
*----------------------------------------------------------------------
RECORD.WRITE:
*----------------------------------------------------------------------
* To write the REDO.MANAGER.CHQ.DETAILS using F.WRITE

    CON.DATE = OCONV(DATE(),"D-")
    Y.DATE.TIME = CON.DATE[9,2]:CON.DATE[1,2]:CON.DATE[4,2]:TIME.STAMP[1,2]:TIME.STAMP[4,2]

    R.REDO.ADMIN.CHQ.DETAILS<ADMIN.CHQ.DET.CURR.NO>    = R.REDO.ADMIN.CHQ.DETAILS<ADMIN.CHQ.DET.CURR.NO> + 1
    R.REDO.ADMIN.CHQ.DETAILS<ADMIN.CHQ.DET.INPUTTER>   = C$T24.SESSION.NO:'_':OPERATOR ;*R22 Auto Code conversion
    R.REDO.ADMIN.CHQ.DETAILS<ADMIN.CHQ.DET.AUTHORISER> = C$T24.SESSION.NO:'_':OPERATOR ;*R22 Auto Code conversion
    R.REDO.ADMIN.CHQ.DETAILS<ADMIN.CHQ.DET.DATE.TIME>  = Y.DATE.TIME
    R.REDO.ADMIN.CHQ.DETAILS<ADMIN.CHQ.DET.CO.CODE>    = ID.COMPANY
    R.REDO.ADMIN.CHQ.DETAILS<ADMIN.CHQ.DET.DEPT.CODE>  = R.USER<EB.USE.DEPARTMENT.CODE>
    R.REDO.ADMIN.CHQ.DETAILS<ADMIN.CHQ.DET.ADDITIONAL.INFO,-1> = R.REDO.ADMIN.CHQ.DETAILS<ADMIN.CHQ.DET.STATUS> : "-" : ID.NEW : "-" : TODAY

    CALL F.WRITE(FN.REDO.ADMIN.CHQ.DETAILS,Y.REDO.ADMIN.CHQ.DETAILS.ID,R.REDO.ADMIN.CHQ.DETAILS)

    GOSUB UPDATE.ADMIN.CHEQUE
*
RETURN
*
*----------------------------------------------------------------------
UPDATE.ADMIN.CHEQUE:
*----------------------------------------------------------------------

    CALL F.READ(FN.REDO.H.ADMIN.CHEQUES,Y.REDO.H.ADMIN.CHQ.ID,R.REDO.H.ADMIN.CHEQUES,F.REDO.H.ADMIN.CHEQUES,REDO.H.ADMIN.CHEQUES.ERR)
    IF R.REDO.H.ADMIN.CHEQUES NE '' THEN
        CON.DATE = OCONV(DATE(),"D-")
        Y.DATE.TIME = CON.DATE[9,2]:CON.DATE[1,2]:CON.DATE[4,2]:TIME.STAMP[1,2]:TIME.STAMP[4,2]
*
        R.REDO.H.ADMIN.CHEQUES<REDO.ADMIN.STATUS>       = W.STATUS
        R.REDO.H.ADMIN.CHEQUES<REDO.ADMIN.BENEFICIARY>  = R.REDO.ADMIN.CHQ.DETAILS<ADMIN.CHQ.DET.BENEFICIARY>
        R.REDO.H.ADMIN.CHEQUES<REDO.ADMIN.DATE.UPDATED> = TODAY
        R.REDO.H.ADMIN.CHEQUES<REDO.ADMIN.USER>         = OPERATOR
        R.REDO.H.ADMIN.CHEQUES<REDO.ADMIN.CURR.NO>      = R.REDO.H.ADMIN.CHEQUES<REDO.ADMIN.CURR.NO> + 1
        R.REDO.H.ADMIN.CHEQUES<REDO.ADMIN.INPUTTER>     = C$T24.SESSION.NO:'_':OPERATOR ;*R22 Auto Code conversion
        R.REDO.H.ADMIN.CHEQUES<REDO.ADMIN.AUTHORISER>   = C$T24.SESSION.NO:'_':OPERATOR ;*R22 Auto Code conversion
        R.REDO.H.ADMIN.CHEQUES<REDO.ADMIN.DATE.TIME>    = Y.DATE.TIME
        R.REDO.H.ADMIN.CHEQUES<REDO.ADMIN.CO.CODE>      = ID.COMPANY
        R.REDO.H.ADMIN.CHEQUES<REDO.ADMIN.DEPT.CODE>    = R.USER<EB.USE.DEPARTMENT.CODE>
        R.REDO.H.ADMIN.CHEQUES<REDO.ADMIN.TRANS.REF>    = ID.NEW
*
        CALL F.WRITE(FN.REDO.H.ADMIN.CHEQUES,Y.REDO.H.ADMIN.CHQ.ID,R.REDO.H.ADMIN.CHEQUES)

    END
*
RETURN
END
