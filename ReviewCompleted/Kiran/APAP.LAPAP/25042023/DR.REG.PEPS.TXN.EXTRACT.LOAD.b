* @ValidationCode : MjotMTA2NzQzMzE4NjpDcDEyNTI6MTY4MjMyNDcwNzY4Nzphaml0aDotMTotMTowOjA6ZmFsc2U6Ti9BOlIyMV9BTVIuMDotMTotMQ==
* @ValidationInfo : Timestamp         : 24 Apr 2023 13:55:07
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : ajith
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : R21_AMR.0
* @ValidationInfo : Copyright Temenos Headquarters SA 1993-2021. All rights reserved.
$PACKAGE APAP.LAPAP
SUBROUTINE DR.REG.PEPS.TXN.EXTRACT.LOAD
*-----------------------------------------------------------------------------
* Company Name   : APAP
* Developed By   :
* Program Name   : DR.REG.PEPS.TXN.EXTRACT
*-----------------------------------------------------------------------------
* Description:
*------------
* This multi-thread job is meant for to extact the tranactions made over 10000 by individual Customer daily.
*-----------------------------------------------------------------------------
*
* Modification History :
* ----------------------
*   Date       Author              Modification Description
* 15-Aug-2014  V.P.Ashokkumar       PACS00396224 - Initial Release
*---------------------------------------------------------------------------------------
*DATE               WHO                       REFERENCE                 DESCRIPTION
*24-04-2023       CONVERSION TOOLS            AUTO R22 CODE CONVERSION  TAM.BP,LAPAP.BP,T24.BP is removed ,$INCLUDE to$INSERT ,SM to@SM ,VMto@VM
*24-04-2023       AJITHKUMAR                  MANUAL R22 CODE CONVERSION NO CHANGE
*----------------------------------------------------------------------------------------
*-----------------------------------------------------------------------------

    $INSERT I_COMMON ;*R22 AUYO CODE CONVERSION
    $INSERT I_EQUATE
    $INSERT I_BATCH.FILES
    $INSERT I_F.DATES
    $INSERT I_F.COMPANY
    $INSERT I_DR.REG.PEPS.TXN.EXTRACT.COMMON ;*R22 AUYO CODE CONVERSION
    $INSERT I_F.REDO.H.REPORTS.PARAM ;*R22 AUYO CODE CONVERSION

    GOSUB OPEN.FILES
    GOSUB INIT.PARA

RETURN
*----------------------------------------------------------
OPEN.FILES:
***********

    FN.CUSTOMER = 'F.CUSTOMER'
    F.CUSTOMER = ''
    CALL OPF(FN.CUSTOMER, F.CUSTOMER)

    FN.ACCOUNT = 'F.ACCOUNT'
    F.ACCOUNT = ''
    CALL OPF(FN.ACCOUNT, F.ACCOUNT)

    FN.STMT.ENTRY = 'F.STMT.ENTRY'
    F.STMT.ENTRY = ''
    CALL OPF(FN.STMT.ENTRY, F.STMT.ENTRY)

    FN.FUNDS.TRANSFER = 'F.FUNDS.TRANSFER'
    F.FUNDS.TRANSFER = ''
    CALL OPF(FN.FUNDS.TRANSFER,F.FUNDS.TRANSFER)

    FN.TELLER = 'F.TELLER'
    F.TELLER = ''
    CALL OPF(FN.TELLER,F.TELLER)

    FN.FT.HIS = 'F.FUNDS.TRANSFER$HIS'
    F.FT.HIS = ''
    CALL OPF(FN.FT.HIS, F.FT.HIS)

    FN.TT.HIS = 'F.TELLER$HIS'
    F.TT.HIS = ''
    CALL OPF(FN.TT.HIS, F.TT.HIS)

    FN.DR.REG.213IF01.CONCAT = 'F.DR.REG.213IF01.CONCAT'
    F.DR.REG.213IF01.CONCAT = ''
    CALL OPF(FN.DR.REG.213IF01.CONCAT,F.DR.REG.213IF01.CONCAT)

    FN.DR.REG.PEPS.WORKFILE = 'F.DR.REG.PEPS.WORKFILE'
    F.DR.REG.PEPS.WORKFILE = ''
    CALL OPF(FN.DR.REG.PEPS.WORKFILE,F.DR.REG.PEPS.WORKFILE)

    FN.REDO.H.REPORTS.PARAM = "F.REDO.H.REPORTS.PARAM"
    F.REDO.H.REPORTS.PARAM  = ""
    CALL OPF(FN.REDO.H.REPORTS.PARAM,F.REDO.H.REPORTS.PARAM)

    FN.RELATION.CUSTOMER = 'F.RELATION.CUSTOMER'
    F.RELATION.CUSTOMER = ''
    CALL OPF(FN.RELATION.CUSTOMER,F.RELATION.CUSTOMER)
RETURN
*-------------------------------------------------------------------
INIT.PARA:
**********
    Y.REPORT.PARAM.ID = "REDO.PEPS"
    CALL CACHE.READ(FN.REDO.H.REPORTS.PARAM,Y.REPORT.PARAM.ID,R.REDO.H.REPORTS.PARAM,PARAM.ERR)
    IF R.REDO.H.REPORTS.PARAM THEN
        Y.OUT.FILE.NAME = R.REDO.H.REPORTS.PARAM<REDO.REP.PARAM.OUT.FILE.NAME>
        Y.FIELD.NME.ARR = R.REDO.H.REPORTS.PARAM<REDO.REP.PARAM.FIELD.NAME>
        Y.FIELD.VAL.ARR = R.REDO.H.REPORTS.PARAM<REDO.REP.PARAM.FIELD.VALUE>
        Y.DISP.TEXT.ARR = R.REDO.H.REPORTS.PARAM<REDO.REP.PARAM.DISPLAY.TEXT>
    END

    LOCATE "RELATION.CODE" IN Y.FIELD.NME.ARR<1,1> SETTING NME.POS THEN
        Y.REL.VAL.ARR = Y.FIELD.VAL.ARR<1,NME.POS>
    END
    Y.REL.VAL.ARR1 = CHANGE(Y.REL.VAL.ARR,@SM,@VM)
    Y.REL.VAL.ARR = CHANGE(Y.REL.VAL.ARR,@SM,' ')

    LOCATE "TRANSACTION.CODE" IN Y.FIELD.NME.ARR<1,1> SETTING TXNCE.POS THEN
        Y.TXNCDE.VAL.ARR = Y.FIELD.VAL.ARR<1,TXNCE.POS>
        Y.TXNCDE.DIS.ARR = Y.DISP.TEXT.ARR<1,TXNCE.POS>
    END
    Y.TXNCDE.VAL.ARR = CHANGE(Y.TXNCDE.VAL.ARR,@SM,@VM)
    Y.TXNCDE.DIS.ARR = CHANGE(Y.TXNCDE.DIS.ARR,@SM,@VM)

    LOCATE "TRANSACTION.TYPE" IN Y.FIELD.NME.ARR<1,1> SETTING TXNTE.POS THEN
        Y.TXNTYE.VAL.ARR = Y.FIELD.VAL.ARR<1,TXNTE.POS>
        Y.TXNTYE.DIS.ARR = Y.DISP.TEXT.ARR<1,TXNTE.POS>
    END
    Y.TXNTYE.VAL.ARR = CHANGE(Y.TXNTYE.VAL.ARR,@SM,@VM)
    Y.TXNTYE.DIS.ARR = CHANGE(Y.TXNTYE.DIS.ARR,@SM,@VM)

    LOCATE "CATEGORY" IN Y.FIELD.NME.ARR<1,1> SETTING CAT.POS THEN
        Y.CAT.VAL1 = Y.FIELD.VAL.ARR<1,CAT.POS,1>
        Y.CAT.VAL2 = Y.FIELD.VAL.ARR<1,CAT.POS,2>
    END


    LAST.WRK.DATE = R.DATES(EB.DAT.LAST.WORKING.DAY)
    YLST.YEAR.ED = R.COMPANY(EB.COM.LAST.YEAR.END)
    YLST.MTH = LAST.WRK.DATE

    BEGIN CASE
        CASE LAST.WRK.DATE[5,2] EQ '06'
            YST.DAT = YLST.MTH[1,4]:'0101'
            YED.DAT = YLST.MTH[1,4]:'0630'
            YST.DATE = LAST.WRK.DATE[1,6]:'01'
            YED.DATE = LAST.WRK.DATE
        CASE LAST.WRK.DATE[5,2] EQ '12'
            YST.DAT = YLST.MTH[1,4]:'0701'
            YED.DAT = YLST.MTH[1,4]:'1231'
            YST.DATE = LAST.WRK.DATE[1,6]:'01'
            YED.DATE = LAST.WRK.DATE
        CASE 1
            YST.DAT = LAST.WRK.DATE[1,6]:'01'
            YED.DAT = LAST.WRK.DATE
    END CASE

    LOC.APP = "CUSTOMER"
    CUS.FLDS = "L.CU.TIPO.CL":@VM:"L.CU.PEPS":@VM:"L.CU.CIDENT":@VM:"L.CU.POS.COMP":@VM:"L.APAP.INDUSTRY":@VM:"L.CU.RNC":@VM:"L.CU.NOUNICO":@VM:"L.CU.ACTANAC"
    LOC.FLD = CUS.FLDS
    LOC.POS = ""
    CALL MULTI.GET.LOC.REF(LOC.APP, LOC.FLD, LOC.POS)
    L.CU.TIPO.CL.POS = LOC.POS<1,1>
    L.CU.PEPS.POS = LOC.POS<1,2>
    L.CU.CIDENT.POS = LOC.POS<1,3>
    L.CU.POS.COMP.POS = LOC.POS<1,4>
    L.APAP.INDUSTRY.POS = LOC.POS<1,5>
    L.CU.RNC.POS = LOC.POS<1,6>
    L.CU.NOUNICO.POS = LOC.POS<1,7>
    L.CU.ACTANAC.POS = LOC.POS<1,8>
RETURN
*----------------------------------------------------------------
END
