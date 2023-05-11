* @ValidationCode : MjoxOTY3MDQ3MDY6Q3AxMjUyOjE2ODExMTAxNTE1NzA6c2FtYXI6LTE6LTE6MDowOmZhbHNlOk4vQTpERVZfMjAyMTA4LjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 10 Apr 2023 12:32:31
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
$PACKAGE APAP.REDOVER
SUBROUTINE REDO.V.AUT.UPD.MGRTABLES
*---------------------------------------------------------------------------
* COMPANY NAME: ASOCIACION POPULAR DE AHORROS Y PRESTAMOS
* DEVELOPED BY: H GANESH
* PROGRAM NAME: REDO.V.AUT.UPD.MGRTABLES
* ODR NO      : ODR-2009-12-0285
*----------------------------------------------------------------------
*DESCRIPTION: This routine is Auth routine attached to below list of versions,
* FUNDS.TRANSFER,MGR.CHQ.TAX
* FUNDS.TRANSFER,MGRUSD.CHQ.TAX
* FUNDS.TRANSFER,MGR.CHQ.NO.TAX
* FUNDS.TRANSFER,MGRUSD.CHQ.NO.TAX
* TELLER,MGR.CHQ.NOTAX
* TELLER,MGR.CHQ.TAX


*IN PARAMETER: NA
*OUT PARAMETER: NA
*LINKED WITH: TELLER & FT
*----------------------------------------------------------------------
* Modification History :
*-----------------------
*DATE            WHO               REFERENCE            DESCRIPTION
* 19.02.2010      H GANESH          ODR-2009-12-0285     INITIAL CREATION
* 11-04-2011     Bharath G         PACS00032271         Update user and concept in REDO.PRINT.CHQ.LIST table.
* 09-05-2011     Bharath G         PACS00023918         The BENEFICIARY  field should be multivalue field
* 28-03-2012     Nava V.           PACS00172913         Adding CHEQUE STATUS updation when transaction is REVERSED.
*----------------------------------------------------------------------
*Modification History
*DATE                       WHO                         REFERENCE                                   DESCRIPTION
*10-04-2023            Conversion Tool             R22 Auto Code conversion                      FM TO @FM VM TO @VM,SM TO @SM, TNO TO C$T24.SESSION.NO
*10-04-2023              Samaran T                R22 Manual Code conversion                         No Changes
*-------------------------------------------------------------------------------------------------------------------------------


    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.TELLER
    $INSERT I_F.USER
    $INSERT I_F.FUNDS.TRANSFER
    $INSERT I_F.REDO.MANAGER.CHQ.DETAILS
    $INSERT I_F.REDO.MANAGER.CHQ.PARAM
    $INSERT I_F.REDO.H.BANK.DRAFTS    ;* VNL - 20120328 - S/E

    GOSUB INIT
    GOSUB OPENFILES
    GOSUB PROCESS
RETURN
*----------------------------------------------------------------------
INIT:
*----------------------------------------------------------------------
    FN.REDO.MANAGER.CHQ.PARAM='F.REDO.MANAGER.CHQ.PARAM'
    F.REDO.MANAGER.CHQ.PARAM=''
    FN.REDO.MANAGER.CHQ.DETAILS='F.REDO.MANAGER.CHQ.DETAILS'
    F.REDO.MANAGER.CHQ.DETAILS=''
    R.REDO.MANAGER.CHQ.DETAILS=''
    FN.REDO.H.BANK.DRAFTS='F.REDO.H.BANK.DRAFTS'    ;* VNL - 20120328 - S
    F.REDO.H.BANK.DRAFTS=''
    R.REDO.H.BANK.DRAFTS=''
*
    Y.NOSTRO.ACCT = ""
    W.STATUS      = ""          ;* VNL - 20120328 - E
*
RETURN
*----------------------------------------------------------------------
OPENFILES:
*----------------------------------------------------------------------
    CALL OPF(FN.REDO.MANAGER.CHQ.PARAM,F.REDO.MANAGER.CHQ.PARAM)
    CALL OPF(FN.REDO.MANAGER.CHQ.DETAILS,F.REDO.MANAGER.CHQ.DETAILS)
    CALL OPF(FN.REDO.H.BANK.DRAFTS,F.REDO.H.BANK.DRAFTS)      ;* VNL - 20120328 - S/E
*
RETURN
*----------------------------------------------------------------------
PROCESS:
*----------------------------------------------------------------------
    GOSUB LOCAL.REF
    IF APPLICATION EQ 'TELLER' THEN

        GOSUB TELLER
    END
    IF APPLICATION EQ 'FUNDS.TRANSFER' THEN

        GOSUB FT
    END

RETURN
*----------------------------------------------------------------------
TELLER:
*----------------------------------------------------------------------
* IF Application equals TELLER
*
    Y.REDO.H.BDRAFT.CHQ.ID=R.NEW(TT.TE.NARRATIVE.1)<1,1>      ;* VNL - 20120328 - S
    R.REDO.MANAGER.CHQ.DETAILS<MAN.CHQ.DET.CHQ.SEQ.NUM> = Y.REDO.H.BDRAFT.CHQ.ID
    Y.REDO.MANAGER.CHQ.DETAILS.ID=R.NEW(TT.TE.CHEQUE.NUMBER)
*
    R.REDO.MANAGER.CHQ.DETAILS<MAN.CHQ.DET.CHEQUE.INT.ACCT> = R.NEW(TT.TE.ACCOUNT.1)
*      R.REDO.MANAGER.CHQ.DETAILS<MAN.CHQ.DET.STATUS>='ISSUED'
    IF R.NEW(TT.TE.RECORD.STATUS)[1,1] EQ "R" THEN
        CALL F.READ(FN.REDO.MANAGER.CHQ.DETAILS,Y.REDO.MANAGER.CHQ.DETAILS.ID,R.REDO.MANAGER.CHQ.DETAILS,F.REDO.MANAGER.CHQ.DETAILS,ERR.READ)
        R.REDO.MANAGER.CHQ.DETAILS<MAN.CHQ.DET.STATUS>  = 'CANCELLED'
        W.STATUS = "CANCELLED"
    END ELSE
        R.REDO.MANAGER.CHQ.DETAILS<MAN.CHQ.DET.STATUS> = 'ISSUED'
        W.STATUS = "ISSUED"
    END
*                                                            ;* VNL - 20120328 - E
    R.REDO.MANAGER.CHQ.DETAILS<MAN.CHQ.DET.AMOUNT>=R.NEW(TT.TE.AMOUNT.LOCAL.1)
    R.REDO.MANAGER.CHQ.DETAILS<MAN.CHQ.DET.ISSUE.ACCOUNT> = R.NEW(TT.TE.ACCOUNT.2)
    R.REDO.MANAGER.CHQ.DETAILS<MAN.CHQ.DET.ISSUE.DATE>=TODAY
    R.REDO.MANAGER.CHQ.DETAILS<MAN.CHQ.DET.COMPANY.CODE>=ID.COMPANY
    Y.BENEFI = R.NEW(TT.TE.LOCAL.REF)<1,POS.L.TT.BENEFICIAR>  ;* PACS00023918
    CHANGE @SM TO @VM IN Y.BENEFI ;* PACS00023918
    R.REDO.MANAGER.CHQ.DETAILS<MAN.CHQ.DET.BENEFICIARY>=Y.BENEFI        ;* PACS00023918
    R.REDO.MANAGER.CHQ.DETAILS<MAN.CHQ.DET.TRANS.REFERENCE>=ID.NEW
    GOSUB CALC.TAX
    R.REDO.MANAGER.CHQ.DETAILS<MAN.CHQ.DET.TAX.AMT>=Y.TAX.AMOUNT
    R.REDO.MANAGER.CHQ.DETAILS<MAN.CHQ.DET.USER>=OPERATOR
    GOSUB RECORD.WRITE
RETURN
*----------------------------------------------------------------------
FT:
*----------------------------------------------------------------------
* IF Application equals FUNDS.TRANSFER
*
    Y.REDO.H.BDRAFT.CHQ.ID = R.NEW(FT.LOCAL.REF)<1,TR.REF.POS>          ;* VNL - 20120328 - S/E
    Y.REDO.MANAGER.CHQ.DETAILS.ID = R.NEW(FT.CREDIT.THEIR.REF)<1,1>     ;* MULTIVALUE
    R.REDO.MANAGER.CHQ.DETAILS<MAN.CHQ.DET.CHEQUE.INT.ACCT> = R.NEW(FT.CREDIT.ACCT.NO)
    R.REDO.MANAGER.CHQ.DETAILS<MAN.CHQ.DET.CHQ.SEQ.NUM> = Y.REDO.H.BDRAFT.CHQ.ID
    R.REDO.MANAGER.CHQ.DETAILS<MAN.CHQ.DET.AMOUNT> = R.NEW(FT.DEBIT.AMOUNT)
    R.REDO.MANAGER.CHQ.DETAILS<MAN.CHQ.DET.ISSUE.ACCOUNT> = R.NEW(FT.DEBIT.ACCT.NO)
    R.REDO.MANAGER.CHQ.DETAILS<MAN.CHQ.DET.ISSUE.DATE> = TODAY
    R.REDO.MANAGER.CHQ.DETAILS<MAN.CHQ.DET.COMPANY.CODE> = ID.COMPANY
    Y.BENEF = R.NEW(FT.LOCAL.REF)<1,POS.BENEFIC.NAME>         ;* PACS00023918
    CHANGE @SM TO @VM IN Y.BENEF  ;* PACS00023918
    R.REDO.MANAGER.CHQ.DETAILS<MAN.CHQ.DET.BENEFICIARY> = Y.BENEF       ;* PACS00023918
    R.REDO.MANAGER.CHQ.DETAILS<MAN.CHQ.DET.TRANS.REFERENCE> = ID.NEW
    R.REDO.MANAGER.CHQ.DETAILS<MAN.CHQ.DET.TAX.AMT>=R.NEW(FT.TAX.AMT)   ;* MULTIVALUE
    R.REDO.MANAGER.CHQ.DETAILS<MAN.CHQ.DET.USER>=OPERATOR
*PACS00293633 - S
*    IF R.NEW(FT.RECORD.STATUS)[1,1] EQ "R" THEN   ;* VNL - 20120328 - S

    IF R.NEW(FT.TRANSACTION.TYPE) EQ "AC95" THEN
        CALL F.READ(FN.REDO.MANAGER.CHQ.DETAILS,Y.REDO.MANAGER.CHQ.DETAILS.ID,R.REDO.MANAGER.CHQ.DETAILS,F.REDO.MANAGER.CHQ.DETAILS,ERR.MGR.CHQ)
        IF PGM.VERSION NE ",REDO.REINSTATE" THEN
            R.REDO.MANAGER.CHQ.DETAILS<MAN.CHQ.DET.STATUS>  = 'CANCELLED'
            W.STATUS = "CANCELLED"
        END ELSE
            R.REDO.MANAGER.CHQ.DETAILS<MAN.CHQ.DET.STATUS>  = 'REINSTATED'
            W.STATUS = "PAID"
        END
    END ELSE
        R.REDO.MANAGER.CHQ.DETAILS<MAN.CHQ.DET.STATUS> = 'ISSUED'
        W.STATUS = "ISSUED"
    END
*PACS00293633 - E
*

    GOSUB RECORD.WRITE
RETURN
*----------------------------------------------------------------------
LOCAL.REF:
*----------------------------------------------------------------------
*
    LOC.REF.APPLICATION="TELLER":@FM:"FUNDS.TRANSFER"
    LOC.REF.FIELDS='L.TT.BENEFICIAR':@VM:'L.FT.NOSTRO.ACC':@FM:'L.FT.CONCEPT':@VM:'BENEFIC.NAME':@VM:'TRANSACTION.REF'
    LOC.REF.POS=''
*CALL GET.LOC.REF(LOC.REF.APPLICATION,LOC.REF.FIELDS,LOC.REF.POS)          ;* VNL - 20120328 - S
    CALL MULTI.GET.LOC.REF(LOC.REF.APPLICATION,LOC.REF.FIELDS,LOC.REF.POS)        ;* VNL - 20120328 - E
    POS.L.TT.BENEFICIAR = LOC.REF.POS<1,1>
    POS.L.TT.NOSTROACCT = LOC.REF.POS<1,2>
    POS.L.FT.CONCEPT    = LOC.REF.POS<2,1>
    POS.BENEFIC.NAME    = LOC.REF.POS<2,2>
    TR.REF.POS          = LOC.REF.POS<2,3>
RETURN
*----------------------------------------------------------------------
CALC.TAX:
*----------------------------------------------------------------------
* Calculates the TAX
    Y.NOSTRO.ACCT=R.NEW(TT.TE.LOCAL.REF)<1,POS.L.TT.NOSTROACCT>         ;* VNL - 20120328 - S
    CALL CACHE.READ(FN.REDO.MANAGER.CHQ.PARAM,Y.REDO.MANAGER.CHQ.PARAM.ID,R.REDO.MANAGER.CHQ.PARAM,PARA.ERR)
    LOCATE Y.NOSTRO.ACCT IN R.REDO.MANAGER.CHQ.PARAM<MAN.CHQ.PRM.ACCOUNT,1> SETTING POS1 THEN         ;* VNL - 20120328 - E
        Y.TAX.KEY=R.REDO.MANAGER.CHQ.PARAM<MAN.CHQ.PRM.TAX.KEY,POS1>
    END
    Y.CHARGE.CODE=R.NEW(TT.TE.CHARGE.CODE)
    LOCATE Y.TAX.KEY IN Y.CHARGE.CODE<1,1> SETTING POS2 THEN
        IF R.NEW(TT.TE.CURRENCY.1) EQ LCCY THEN
            Y.TAX.AMOUNT=R.NEW(TT.TE.CHRG.AMT.LOCAL)<1,POS2>      ;* MULTIVALUE
        END ELSE
            Y.TAX.AMOUNT=R.NEW(TT.TE.CHRG.AMT.FCCY)<1,POS2>
        END
    END
RETURN
*
*----------------------------------------------------------------------
UPDATE.DRAFTS.CHEQUE:
*----------------------------------------------------------------------
*
    CALL F.READ(FN.REDO.H.BANK.DRAFTS,Y.REDO.H.BDRAFT.CHQ.ID,R.REDO.H.BANK.DRAFTS,F.REDO.H.BANK.DRAFTS,REDO.H.BANK.DRAFTS.ERR)
    IF R.REDO.H.BANK.DRAFTS NE '' THEN
        CON.DATE = OCONV(DATE(),"D-")
        Y.DATE.TIME = CON.DATE[9,2]:CON.DATE[1,2]:CON.DATE[4,2]:TIME.STAMP[1,2]:TIME.STAMP[4,2]
*
        R.REDO.H.BANK.DRAFTS<REDO.BANK.STATUS>     = W.STATUS
        R.REDO.H.BANK.DRAFTS<REDO.BANK.CURR.NO>    = R.REDO.H.BANK.DRAFTS<REDO.BANK.CURR.NO> + 1
        R.REDO.H.BANK.DRAFTS<REDO.BANK.INPUTTER>   = C$T24.SESSION.NO:'_':OPERATOR  ;*R22 AUTO CODE CONVERSION
        R.REDO.H.BANK.DRAFTS<REDO.BANK.AUTHORISER> = C$T24.SESSION.NO:'_':OPERATOR   ;*R22 AUTO CODE CONVERSION
        R.REDO.H.BANK.DRAFTS<REDO.BANK.DATE.TIME>  = Y.DATE.TIME
        R.REDO.H.BANK.DRAFTS<REDO.BANK.CO.CODE>    = ID.COMPANY
        R.REDO.H.BANK.DRAFTS<REDO.BANK.DEPT.CODE>  = R.USER<EB.USE.DEPARTMENT.CODE>
        R.REDO.H.BANK.DRAFTS<REDO.BANK.TRANS.REF>  = ID.NEW
*
        CALL F.WRITE(FN.REDO.H.BANK.DRAFTS,Y.REDO.H.BDRAFT.CHQ.ID,R.REDO.H.BANK.DRAFTS)
*
    END
*
RETURN
*
*----------------------------------------------------------------------
RECORD.WRITE:
*----------------------------------------------------------------------
* To write the REDO.MANAGER.CHQ.DETAILS using F.WRITE
    TEMPTIME = OCONV(TIME(),"MTS")
    TEMPTIME = TEMPTIME[1,5]
    R.REDO.MANAGER.CHQ.DETAILS<MAN.CHQ.DET.CURR.NO>=1
    R.REDO.MANAGER.CHQ.DETAILS<MAN.CHQ.DET.INPUTTER>=C$T24.SESSION.NO:'_':OPERATOR   ;*R22 AUTO CODE CONVERSION
    R.REDO.MANAGER.CHQ.DETAILS<MAN.CHQ.DET.AUTHORISER>=C$T24.SESSION.NO:'_':OPERATOR  ;*R22 AUTO CODE CONVERSION
    R.REDO.MANAGER.CHQ.DETAILS<MAN.CHQ.DET.DATE.TIME>=OCONV(DATE(),"D2"):' ':TEMPTIME
    R.REDO.MANAGER.CHQ.DETAILS<MAN.CHQ.DET.CO.CODE>=ID.COMPANY
    R.REDO.MANAGER.CHQ.DETAILS<MAN.CHQ.DET.DEPT.CODE>=R.USER<EB.USE.DEPARTMENT.CODE>
    R.REDO.MANAGER.CHQ.DETAILS<MAN.CHQ.DET.ADDITIONAL.INFO,-1> = R.REDO.MANAGER.CHQ.DETAILS<MAN.CHQ.DET.STATUS> : "-" : ID.NEW : "-" : TODAY  ;* VNL - 20120328 - S/E
    CALL F.WRITE(FN.REDO.MANAGER.CHQ.DETAILS,Y.REDO.MANAGER.CHQ.DETAILS.ID,R.REDO.MANAGER.CHQ.DETAILS)
*
    GOSUB UPDATE.DRAFTS.CHEQUE  ;* VNL - 20120328 - S/E
*
RETURN
END
