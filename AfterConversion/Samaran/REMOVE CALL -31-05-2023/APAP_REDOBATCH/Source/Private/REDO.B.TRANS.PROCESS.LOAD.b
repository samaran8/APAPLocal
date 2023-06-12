* @ValidationCode : MjotMTg2OTY5MDM3MDpDcDEyNTI6MTY4NDg1NDM5OTUwMzpJVFNTOi0xOi0xOjE0NzQ6MTpmYWxzZTpOL0E6UjIxX0FNUi4wOi0xOi0x
* @ValidationInfo : Timestamp         : 23 May 2023 20:36:39
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : ITSS
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : 1474
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : true
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : R21_AMR.0
* @ValidationInfo : Copyright Temenos Headquarters SA 1993-2021. All rights reserved.
$PACKAGE APAP.REDOBATCH
SUBROUTINE REDO.B.TRANS.PROCESS.LOAD
*-----------------------------------------------------------------------------
* Company Name  : APAP
* Developed By  : SAKTHI
* Program Name  : REDO.B.TRANS.PROCESS.LOAD
* ODR           : ODR-2010-08-0031
*-----------------------------------------------------------------------------
* Description: This routine is a load routine used to load the variable
*-----------------------------------------------------------------------------
* MODIFICATION HISTORY
*-----------------------------------------------------------------------------
*   DATE         WHO                    ODR                   DESCRIPTION
*============    ==============         ================      ================
*19-10-2010      Sakthi Sellappillai    ODR-2010-08-0031      Initial Creation
* Date                   who                   Reference              
* 13-04-2023         CONVERSTION TOOL     R22 AUTO CONVERSTION - FM TO @FM AND VM TO @VM AND F.READ TO CACHE.READ AND REMOVED F.FT.COMMISSION.TYPE AND F.FT.TXN.TYPE.CONDITION AND COMMENTED I_F.FUNDS.TRANSFER
* 13-04-2023          ANIL KUMAR B        R22 MANUAL CONVERSTION -NO CHANGES
*-----------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.FUNDS.TRANSFER
    $INSERT I_BATCH.FILES
    $INSERT I_REDO.B.TRANS.PROCESS.COMMON
    $INSERT I_F.REDO.SUPPLIER.PAYMENT
    $INSERT I_F.REDO.FILE.DATE.PROCESS
    $INSERT I_F.REDO.SUPPLIER.PAY.DATE
    $INSERT I_F.AI.REDO.ARCIB.PARAMETER
    $INSERT I_F.FT.COMMISSION.TYPE
    $INSERT I_F.FT.CHARGE.TYPE
    $INSERT I_F.FT.TXN.TYPE.CONDITION
    $INSERT I_F.REDO.NCF.ISSUED
*   $INSERT I_F.FUNDS.TRANSFER  ;*R22 AUTO CONVERSTION COMMENTED I_F.FUNDS.TRANSFER
    $INSERT I_F.REDO.L.NCF.UNMAPPED
    $INSERT I_F.REDO.L.NCF.STATUS
    $INSERT I_F.REDO.L.NCF.CANCELLED
    GOSUB PROCESS
RETURN
*-----------------------------------------------------------------------------
PROCESS:
*-----------------------------------------------------------------------------
    FN.REDO.SUPPLIER.PAYMENT = 'F.REDO.SUPPLIER.PAYMENT'
    F.REDO.SUPPLIER.PAYMENT = ''
    CALL OPF(FN.REDO.SUPPLIER.PAYMENT,F.REDO.SUPPLIER.PAYMENT)
    R.REDO.SUPPLIER.PAYMENT = ''
    FN.REDO.FILE.DATE.PROCESS = 'F.REDO.FILE.DATE.PROCESS'
    F.REDO.FILE.DATE.PROCESS  = ''
    CALL OPF(FN.REDO.FILE.DATE.PROCESS,F.REDO.FILE.DATE.PROCESS)
    FN.REDO.SUPPLIER.PAY.DATE = 'F.REDO.SUPPLIER.PAY.DATE'
    F.REDO.SUPPLIER.PAY.DATE = ''
    CALL OPF(FN.REDO.SUPPLIER.PAY.DATE,F.REDO.SUPPLIER.PAY.DATE)
    R.REDO.SUPPLIER.PAY.DATE = ''
    Y.REDO.SUPP.ERR = ''
    FN.FUNDS.TRANSFER = 'F.FUNDS.TRANSFER'
    F.FUNDS.TRANSFER = ''
    CALL OPF(FN.FUNDS.TRANSFER,F.FUNDS.TRANSFER)

    FN.REDO.NCF.ISSUED = 'F.REDO.NCF.ISSUED'
    F.REDO.NCF.ISSUED  = ''
    CALL OPF(FN.REDO.NCF.ISSUED,F.REDO.NCF.ISSUED)

    FN.REDO.L.NCF.UNMAPPED = 'F.REDO.L.NCF.UNMAPPED'
    F.REDO.L.NCF.UNMAPPED  = ''
    CALL OPF(FN.REDO.L.NCF.UNMAPPED,F.REDO.L.NCF.UNMAPPED)

    FN.REDO.L.NCF.STATUS = 'F.REDO.L.NCF.STATUS'
    F.REDO.L.NCF.STATUS  = ''
    CALL OPF(FN.REDO.L.NCF.STATUS,F.REDO.L.NCF.STATUS)

    FN.REDO.L.NCF.CANCELLED = 'F.REDO.L.NCF.CANCELLED'
    F.REDO.L.NCF.CANCELLED  = ''
    CALL OPF(FN.REDO.L.NCF.CANCELLED,F.REDO.L.NCF.CANCELLED)

    FN.FUNDS.TRANSFER = 'F.FUNDS.TRANSFER'
    F.FUNDS.TRANSFER  = ''
    CALL OPF(FN.FUNDS.TRANSFER,F.FUNDS.TRANSFER)


    FN.ACCOUNT = 'F.ACCOUNT'
    F.ACCOUNT = ''
    CALL OPF(FN.ACCOUNT,F.ACCOUNT)
    R.ACCOUNT = ''

    R.FUNDS.TRANSFER.REC = ''
    FN.OFS.RESPONSE.QUEUE = 'F.OFS.RESPONSE.QUEUE'
    F.OFS.RESPONSE.QUEUE = ''
    CALL OPF(FN.OFS.RESPONSE.QUEUE,F.OFS.RESPONSE.QUEUE)

    FN.AI.REDO.ARCIB.PARAMETER = 'F.AI.REDO.ARCIB.PARAMETER'
    F.AI.REDO.ARCIB.PARAMETER  = ''
    CALL OPF(FN.AI.REDO.ARCIB.PARAMETER,F.AI.REDO.ARCIB.PARAMETER)

    FN.FT.TXN.TYPE.CONDITION = 'F.FT.TXN.TYPE.CONDITION'
    F.FT.TXN.TYPE.CONDITION = ''
    R.FT.TXN.TYPE.CONDITION = ''
    CALL OPF(FN.FT.TXN.TYPE.CONDITION,F.FT.TXN.TYPE.CONDITION)

    FN.FT.COMMISSION.TYPE = 'F.FT.COMMISSION.TYPE'
    F.FT.COMMISSION.TYPE  = ''
    R.FT.COMMISSION.TYPE  = ''
    CALL OPF(FN.FT.COMMISSION.TYPE,F.FT.COMMISSION.TYPE)

    FN.FT.CHARGE.TYPE = 'F.FT.CHARGE.TYPE'
    F.FT.CHARGE.TYPE  = ''
    R.FT.CHARGE.TYPE  = ''
    CALL OPF(FN.FT.CHARGE.TYPE,F.FT.CHARGE.TYPE)

    CALL CACHE.READ(FN.AI.REDO.ARCIB.PARAMETER,'SYSTEM',R.AI.REDO.ARCIB.PARAMETER,AI.REDO.ARCIB.PARAMETER.ERR)
    IF NOT(AI.REDO.ARCIB.PARAMETER.ERR) THEN
        Y.PAYROLL.TXN.CODE  = R.AI.REDO.ARCIB.PARAMETER<AI.PARAM.PAYROLL.TXN.CODE>
        Y.SUPPLIER.TXN.CODE = R.AI.REDO.ARCIB.PARAMETER<AI.PARAM.SUPPLIER.TXN.CODE>
        Y.SUPPLIER.INT.ACCT = R.AI.REDO.ARCIB.PARAMETER<AI.PARAM.SUPPLIER.INT.ACCT>
        Y.PAYROLL.INT.ACCT = R.AI.REDO.ARCIB.PARAMETER<AI.PARAM.PAYROLL.INT.ACCT>
        Y.ARC.TRANSACTION.TYPE  = R.AI.REDO.ARCIB.PARAMETER<AI.PARAM.TRANSACTION.TYPE>
        Y.RET.TXN.CODE =  R.AI.REDO.ARCIB.PARAMETER<AI.PARAM.RET.TXN.CODE>
        Y.RET.TAX.CODE =  R.AI.REDO.ARCIB.PARAMETER<AI.PARAM.RET.TAX.CODE>
        CHANGE @VM TO @FM IN Y.ARC.TRANSACTION.TYPE
        LOCATE 'UPLOADPAYMENT' IN Y.ARC.TRANSACTION.TYPE SETTING UPLOAD.POS THEN
            Y.SUP.CR.ACCT.NO = R.AI.REDO.ARCIB.PARAMETER<AI.PARAM.ACCOUNT.NO,UPLOAD.POS>
        END
    END

    GOSUB PAYROLL.COMMISSION.PROCESS
    GOSUB SUPPLIER.COMMISSION.PROCESS
    R.OFS.RESPONSE.QUEUE = ''
    Y.OFS.RES.ERR = ''
    Y.LOC.FT.IDEN.TYPE.POS = ''
    Y.LOC.FT.INV.NO.POS = ''
    Y.LOC.FT.NCF.NUM.POS = ''
    APPL.ARRAY = "FUNDS.TRANSFER":@FM:"ACCOUNT"
    FIELD.ARRAY = "L.FT.IDEN.TYPE":@VM:"L.FT.INV.NO":@VM:"L.NCF.NUMBER":@VM:"L.FTST.ACH.PART":@VM:"L.FT.ACH.B.ACC":@VM:"L.FT.ACH.B.NAM":@VM:"L.ACH.PART.ID":@VM:"L.COMMENTS":@VM:"L.TT.TAX.AMT":@VM:"L.NCF.TAX.NUM":@FM:"L.AC.AV.BAL"
    FIELD.POS = ''
    CALL MULTI.GET.LOC.REF(APPL.ARRAY,FIELD.ARRAY,FIELD.POS)
    Y.LOC.FT.IDEN.TYPE.POS = FIELD.POS<1,1>
    Y.LOC.FT.INV.NO.POS = FIELD.POS<1,2>
    Y.LOC.FT.NCF.NUM.POS = FIELD.POS<1,3>
    Y.LOC.FT.ACH.PART.POS = FIELD.POS<1,4>
    Y.LOC.FT.ACH.B.ACC.POS = FIELD.POS<1,5>
    Y.LOC.FT.ACH.B.NAM.POS = FIELD.POS<1,6>
    Y.LOC.FT.ACH.PART.ID.POS  = FIELD.POS<1,7>
    Y.LOC.L.AC.AV.BAL.POS = FIELD.POS<2,1>
    Y.LOC.COMMENTS.POS = FIELD.POS<1,8>
    Y.L.TT.TAX.AMT.POS = FIELD.POS<1,9>
    POS.L.NCF.TAX.NUM  = FIELD.POS<1,10>
    Y.OFS.MSG.ID.VAL = ''
    Y.SUPPLIER.PAY.LIST = ''
    Y.THIRD.DAY.VAL = ''
RETURN

*-------------------------------------------------------------------------------------
PAYROLL.COMMISSION.PROCESS:
*-------------------------------------------------------------------------------------

    CALL CACHE.READ(FN.FT.TXN.TYPE.CONDITION, Y.PAYROLL.TXN.CODE, R.FT.TXN.TYPE.CONDITION, FT.TXN.TYPE.CONDITION.ERR)  ;*R22 AUTO CONVERSTION F.READ TO CACHE.READ AND REMOVED F.FT.TXN.TYPE.CONDITION
    Y.PAYROLL.COMM.TYPE       = R.FT.TXN.TYPE.CONDITION<FT6.COMM.TYPES>
    Y.PAYROLL.CHG.TYPE  = R.FT.TXN.TYPE.CONDITION<FT6.CHARGE.TYPES>
    IF Y.PAYROLL.CHG.TYPE THEN
        Y.PAYROLL.CHG.TYPE<2,-1> = 'CHG'
    END

    IF Y.PAYROLL.COMM.TYPE THEN
        CALL CACHE.READ(FN.FT.COMMISSION.TYPE, Y.PAYROLL.COMM.TYPE, R.FT.COMMISSION.TYPE, FT.COMMISSION.TYPE.ERR)  ;*R22 AUTO CONVERSTION F.READ TO CACHE.READ AND REMOVED F.FT.COMMISSION.TYPE
        Y.PAY.COM.FLAT.AMT = R.FT.COMMISSION.TYPE<FT4.FLAT.AMT,1>
        Y.PAY.COM.PERCENT = R.FT.COMMISSION.TYPE<FT4.PERCENTAGE,1,1>
        Y.PAY.CATEG.ACCOUNT = R.FT.COMMISSION.TYPE<FT4.CATEGORY.ACCOUNT>
    END


RETURN
*-------------------------------------------------------------------------------------
SUPPLIER.COMMISSION.PROCESS:
*-------------------------------------------------------------------------------------
    CALL CACHE.READ(FN.FT.TXN.TYPE.CONDITION, Y.SUPPLIER.TXN.CODE, R.FT.TXN.TYPE.CONDITION, FT.TXN.TYPE.CONDITION.ERR) ;*R22 AUTO CONVERSTION F.READ TO CACHE.READ AND REMOVED F.FT.TXN.TYPE.CONDITION
    Y.SUPPLIER.COMM.TYPE = R.FT.TXN.TYPE.CONDITION<FT6.COMM.TYPES>
    Y.PAYROLL.CHG.TYPE  = R.FT.TXN.TYPE.CONDITION<FT6.CHARGE.TYPES>
    IF Y.PAYROLL.CHG.TYPE THEN
        Y.PAYROLL.CHG.TYPE<2,-1> = 'CHG'
    END

    IF Y.SUPPLIER.COMM.TYPE THEN
        CALL CACHE.READ(FN.FT.COMMISSION.TYPE, Y.SUPPLIER.COMM.TYPE, R.FT.COMMISSION.TYPE, FT.COMMISSION.TYPE.ERR)  ;*R22 AUTO CONVERSTION F.READ TO CACHE.READ AND REMOVED F.FT.COMMISSION.TYPE
        Y.SUP.COM.FLAT.AMT = R.FT.COMMISSION.TYPE<FT4.FLAT.AMT,1>
        Y.SUP.COM.PERCENT  = R.FT.COMMISSION.TYPE<FT4.PERCENTAGE,1,1>
        Y.SUP.CATEG.ACCOUNT = R.FT.COMMISSION.TYPE<FT4.CATEGORY.ACCOUNT>
    END

RETURN
END
*-----------------------------------------------------------------------------