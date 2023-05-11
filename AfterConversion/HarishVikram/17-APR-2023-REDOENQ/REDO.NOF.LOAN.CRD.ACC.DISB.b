* @ValidationCode : MjotMjEwNzA1OTAwMDpDcDEyNTI6MTY4MTcyNTk5OTkxNjpIYXJpc2h2aWtyYW1DOi0xOi0xOjA6MDpmYWxzZTpOL0E6UjIxX0FNUi4wOi0xOi0x
* @ValidationInfo : Timestamp         : 17 Apr 2023 15:36:39
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : HarishvikramC
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : R21_AMR.0
* @ValidationInfo : Copyright Temenos Headquarters SA 1993-2021. All rights reserved.
$PACKAGE APAP.REDOENQ
SUBROUTINE REDO.NOF.LOAN.CRD.ACC.DISB(Y.FIN.ARRR)
*-----------------------------------------------------------------------------
*Company   Name    :Asociacion Popular de Ahorros y Prestamos
*Developed By      :Bharath G
*Program   Name    :REDO.NOF.LOAN.CRD.ACC.DISB
*----------------------------------------------------------------------------------

*DESCRIPTION       : This is no-file routine used to show the list of account nos for a field in version
*
*
*LINKED WITH       :VERSION>FUNDS.TRANSFER,REDO.AA.LTCC
*---------------------------------------------------------------------------------
*Modification Details:
*=====================
*   Date               who                    Reference                   Description
* 02-sep-2010          Marimuthu S     PACS00080544& PACS000112734     Initial Creation
*
* 17-APR-2023     Conversion tool   R22 Auto conversion   		++ to +=
* 17-APR-2023      Harishvikram C   Manual R22 conversion      No changes
*---------------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.ACCOUNT
    $INSERT I_F.FUNDS.TRANSFER
    $INSERT I_F.AZ.PRODUCT.PARAMETER
    $INSERT I_F.CUSTOMER
    $INSERT I_F.AA.ARRANGEMENT

MAIN:

    GOSUB OPENFILES
    GOSUB PROCESS
    GOSUB PGM.END

OPENFILES:

    FN.ACCOUNT = 'F.ACCOUNT'
    F.ACCOUNT = ''
    CALL OPF(FN.ACCOUNT,F.ACCOUNT)

    FN.AZ.PRODUCT.PARAMETER = 'F.AZ.PRODUCT.PARAMETER'
    F.AZ.PRODUCT.PARAMETER = ''
    CALL OPF(FN.AZ.PRODUCT.PARAMETER,F.AZ.PRODUCT.PARAMETER)

    FN.CUSTOMER = 'F.CUSTOMER'
    F.CUSTOMER = ''
    CALL OPF(FN.CUSTOMER,F.CUSTOMER)

    FN.AA.ARRANGEMENT = 'F.AA.ARRANGEMENT'
    F.AA.ARRANGEMENT = ''
    CALL OPF(FN.AA.ARRANGEMENT,F.AA.ARRANGEMENT)

    L.APPLS = 'AZ.PRODUCT.PARAMETER'
    L.FIELDS = 'L.AZ.RE.INV.CAT'
    CALL MULTI.GET.LOC.REF(L.APPLS,L.FIELDS,L.POS)
    Y.POS.RE.INV.CAT = L.POS<1,1>

RETURN

PROCESS:


    Y.ACC = R.NEW(FT.DEBIT.ACCT.NO)
    IF Y.ACC[1,2] EQ 'AA' THEN
        CALL F.READ(FN.AA.ARRANGEMENT,Y.ACC,R.AA.ARR,F.AA.ARRANGEMENT,AA.AC.ERR)
        Y.ACC = R.AA.ARR<AA.ARR.LINKED.APPL.ID>
    END
    CALL F.READ(FN.ACCOUNT,Y.ACC,R.ACCOUNT,F.ACCOUNT,ACC.ERR)
    Y.CUS = R.ACCOUNT<AC.CUSTOMER>

    SEL.CMD = 'SELECT ':FN.AZ.PRODUCT.PARAMETER:' WITH L.AZ.RE.INV.CAT NE ""'
    CALL EB.READLIST(SEL.CMD,SEL.LIST,'',NOR.APP,SEL.ERR)
    APP.CNT = 1
    LOOP
    WHILE APP.CNT LE NOR.APP
        APP.ID = SEL.LIST<APP.CNT>
        CALL CACHE.READ(FN.AZ.PRODUCT.PARAMETER,APP.ID,R.APP,AP.ER)
        RE.INV.CATEG = R.APP<AZ.APP.LOCAL.REF,Y.POS.RE.INV.CAT>
        IF RE.INV.CATEG THEN
            VAR.RE.INV.CATEG<-1> = R.APP<AZ.APP.LOCAL.REF,Y.POS.RE.INV.CAT>
        END
        APP.CNT += 1
    REPEAT

    GOSUB SUB.PROCESS

RETURN

SUB.PROCESS:


    SEL.CMD.ACC = 'SELECT ':FN.ACCOUNT:' WITH CUSTOMER EQ ':Y.CUS:' AND ((CATEGORY GE 1000 AND CATEGORY LE 1999) OR (CATEGORY GE 6000 AND CATEGORY LE 6599))'
    CALL EB.READLIST(SEL.CMD.ACC,SEL.LIST.ACC,'',NOF.RECS,SEL.AC.ERR)
    LOOP
        REMOVE Y.AC.ID FROM SEL.LIST.ACC SETTING POS.L
    WHILE Y.AC.ID:POS.L
        CALL F.READ(FN.ACCOUNT,Y.AC.ID,R.ACCOUNT,F.ACCOUNT,ACC.ERRR)
        Y.CATEG = R.ACCOUNT<AC.CATEGORY>
        LOCATE Y.CATEG IN VAR.RE.INV.CATEG SETTING POS.LOC ELSE
            Y.CUS.ID = R.ACCOUNT<AC.CUSTOMER>
            CALL F.READ(FN.CUSTOMER,Y.CUS.ID,R.CUSTOMER,F.CUSTOMER,CUS.ERR)
            Y.SHORT.TIT = R.CUSTOMER<EB.CUS.SHORT.NAME>
            Y.FIN.ARRR<-1> = Y.AC.ID:'*':Y.CUS.ID:'*':Y.SHORT.TIT
        END

    REPEAT

RETURN

PGM.END:

END
