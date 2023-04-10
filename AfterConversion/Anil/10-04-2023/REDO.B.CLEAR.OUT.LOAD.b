* @ValidationCode : MjozOTA0MjU3NDI6Q3AxMjUyOjE2ODExMDY2NDg3NTI6SVRTUzotMTotMTowOjA6ZmFsc2U6Ti9BOlIyMV9BTVIuMDotMTotMQ==
* @ValidationInfo : Timestamp         : 10 Apr 2023 11:34:08
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
$PACKAGE APAP.REDOBATCH
SUBROUTINE REDO.B.CLEAR.OUT.LOAD

****************************************************************
*-------------------------------------------------------------------------
* Company Name  : ASOCIACION POPULAR DE AHORROS Y PRESTAMOS
* Developed By  : Arulprakasam P
* Program Name  : REDO.B.CLEAR.OUT.LOAD
****-------------------------------------------------------------------------
* Description: This routine is a load routine used to load the variables
*
***-----------------------------------------------------------------------------
* Linked with:
* In parameter :
* out parameter : None
*------------------------------------------------------------------------------
* MODIFICATION HISTORY
*------------------------------------------------------------------------------
*   DATE                ODR                             DESCRIPTION
* 23-11-2010      ODR-2010-09-0251                  Initial Creation
* 16-04-2012        PACS00188869                   changed settlement Account number
* Date                   who                   Reference              
* 10-04-2023         CONVERSTION TOOL     R22 AUTO CONVERSTION VM TO @VM AND FM TO @FM
* 10-04-2023          ANIL KUMAR B        R22 MANUAL CONVERSTION -NO CHANGES
*--------------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.ACCOUNT
    $INSERT I_F.AZ.ACCOUNT
    $INSERT I_F.AZ.PRODUCT.PARAMETER
    $INSERT I_F.REDO.APAP.CLEARING.INWARD
    $INSERT I_F.REDO.MAPPING.TABLE
    $INSERT I_F.REDO.CLEARING.PROCESS
    $INSERT I_REDO.B.CLEAR.OUT.COMMON
    $INSERT I_F.REDO.APAP.CLEAR.PARAM
    $INSERT I_F.REDO.COLLECT.PARAM
    $INSERT I_F.REDO.TFS.PROCESS
    $INSERT I_F.AC.LOCKED.EVENTS
    $INSERT I_F.LOCKING
    $INSERT I_F.REDO.H.ROUTING.HEADER

    GOSUB INIT
    GOSUB READ.FILE

RETURN

*-----------------------------------------------------------------------------------------------------------
*****
INIT:
*****

    CLEARING.PROCESS.ID = 'B132.PROCESS'

    FN.REDO.BATCH.FILE = 'F.REDO.BATCH.FILE'
    F.REDO.BATCH.FILE = ''
    CALL OPF(FN.REDO.BATCH.FILE,F.REDO.BATCH.FILE)

    FN.REDO.CLEARING.PROCESS = 'F.REDO.CLEARING.PROCESS'
    F.REDO.CLEARING.PROCESS  = ''
    CALL OPF(FN.REDO.CLEARING.PROCESS,F.REDO.CLEARING.PROCESS)

    FN.MTS = 'F.MULTI.TRANSACTION.SERVICE'
    F.MTS = ''
    CALL OPF(FN.MTS,F.MTS)

    FN.REDO.MAPPING.TABLE = 'F.REDO.MAPPING.TABLE'
    F.REDO.MAPPING.TABLE = ''
    CALL OPF(FN.REDO.MAPPING.TABLE,F.REDO.MAPPING.TABLE)

    FN.REDO.CLEARING.PROCESS = 'F.REDO.CLEARING.PROCESS'
    F.REDO.CLEARING.PROCESS  = ''
    CALL OPF(FN.REDO.CLEARING.PROCESS,F.REDO.CLEARING.PROCESS)

    FN.AC.LOCKED.EVENTS = 'F.AC.LOCKED.EVENTS'
    F.AC.LOCKED.EVENTS = ''
    CALL OPF(FN.AC.LOCKED.EVENTS,F.AC.LOCKED.EVENTS)

    FN.ACCOUNT = 'F.ACCOUNT'
    F.ACCOUNT = ''
    CALL OPF(FN.ACCOUNT,F.ACCOUNT)

    FN.REDO.MAPPING.TABLE = 'F.REDO.MAPPING.TABLE'
    F.REDO.MAPPING.TABLE = ''
    CALL OPF(FN.REDO.MAPPING.TABLE,F.REDO.MAPPING.TABLE)

    FN.CUSTOMER = 'F.CUSTOMER'
    F.CUSTOMER = ''
    CALL OPF(FN.CUSTOMER,F.CUSTOMER)

    FN.TFS = 'F.T24.FUND.SERVICES'
    F.TFS = ''
    CALL OPF(FN.TFS,F.TFS)

    FN.TELLER.USER = 'F.TELLER.USER'
    F.TELLER.USER = ''
    CALL OPF(FN.TELLER.USER,F.TELLER.USER)

    FN.REDO.H.ROUTING.NUMBER = 'F.REDO.H.ROUTING.NUMBER'
    F.REDO.H.ROUTING.NUMBER = ''
    CALL OPF(FN.REDO.H.ROUTING.NUMBER,F.REDO.H.ROUTING.NUMBER)


    FN.REDO.CLEARING.OUTWARD = 'F.REDO.CLEARING.OUTWARD'
    F.REDO.CLEARING.OUTWARD = ''
    CALL OPF(FN.REDO.CLEARING.OUTWARD,F.REDO.CLEARING.OUTWARD)

    FN.REDO.APAP.CLEAR.PARAM = 'F.REDO.APAP.CLEAR.PARAM'
    F.REDO.APAP.CLEAR.PARAM = ''
    CALL OPF(FN.REDO.APAP.CLEAR.PARAM,F.REDO.APAP.CLEAR.PARAM)

    FN.REDO.H.ROUTING.HEADER = 'F.REDO.H.ROUTING.HEADER'
    F.REDO.H.ROUTING.HEADER  = ''
    CALL OPF(FN.REDO.H.ROUTING.HEADER,F.REDO.H.ROUTING.HEADER)

    FN.CUSTOMER = 'F.CUSTOMER'
    F.CUSTOMER = ''
    CALL OPF(FN.CUSTOMER,F.CUSTOMER)

    FN.LOCKING = 'F.LOCKING'
    F.LOCKING.VAR = ''
    CALL OPF(FN.LOCKING,F.LOCKING.VAR)

    FN.COLLECT.PARAM = 'F.REDO.COLLECT.PARAM'
    F.COLLECT.PRAMA = ''
    CALL OPF(FN.COLLECT.PARAM,F.COLLECT.PRAMA)

    FN.OUT.CLEAR.FILE = 'F.REDO.OUT.CLEAR.FILE'
    F.OUT.CLEAR.FILE = ''
    CALL OPF(FN.OUT.CLEAR.FILE,F.OUT.CLEAR.FILE)

    FN.REDO.TFS.ALE='F.REDO.TFS.ALE'
    F.REDO.TFS.ALE=''
    CALL OPF(FN.REDO.TFS.ALE,F.REDO.TFS.ALE)

    FN.REDO.TFS.PROCESS = 'F.REDO.TFS.PROCESS'
    F.REDO.TFS.PROCESS = ''
    CALL OPF(FN.REDO.TFS.PROCESS,F.REDO.TFS.PROCESS)

    FN.AZ.ACCOUNT = 'F.AZ.ACCOUNT'
    F.AZ.ACCOUNT = ''
    CALL OPF(FN.AZ.ACCOUNT,F.AZ.ACCOUNT)

    FN.APP = 'F.AZ.PRODUCT.PARAMETER'
    F.APP = ''
    CALL OPF(FN.APP,F.APP)

    FN.REDO.OTH.BANK.NAME = 'F.REDO.OTH.BANK.NAME'
    F.REDO.OTH.BANK.NAME  = ''
    CALL OPF(FN.REDO.OTH.BANK.NAME,F.REDO.OTH.BANK.NAME)

    INT.CODE = ''
    ID.PROC = ''
    BAT.NO =''
    BAT.TOT =''
    INFO.OR =''
    INFO.DE =''
    ID.PROC = ''
    MON.TP =''
    DESC = ''
    REC.CON = ''
    EX.USER = ''
    EX.PC = ''

    FN.REDO.INTRANSIT.CHQ = 'F.REDO.INTRANSIT.CHQ'
    F.REDO.INTRANSIT.CHQ = ''
    CALL OPF(FN.REDO.INTRANSIT.CHQ,F.REDO.INTRANSIT.CHQ)

    FN.REDO.CONCAT.CHEQUE.NOS = 'F.REDO.CONCAT.CHEQUE.NOS'
    F.REDO.CONCAT.CHEQUE.NOS = ''
    CALL OPF(FN.REDO.CONCAT.CHEQUE.NOS,F.REDO.CONCAT.CHEQUE.NOS)

    FN.REDO.TRANSACTION.CHAIN = 'F.REDO.TRANSACTION.CHAIN'
    F.REDO.TRANSACTION.CHAIN = ''
    CALL OPF(FN.REDO.TRANSACTION.CHAIN,F.REDO.TRANSACTION.CHAIN)

    FN.REDO.CONCAT.CHQ.TXN = 'F.REDO.CONCAT.CHQ.TXN'
    F.REDO.CONCAT.CHQ.TXN = ''
    CALL OPF(FN.REDO.CONCAT.CHQ.TXN,F.REDO.CONCAT.CHQ.TXN)

RETURN

*-----------------------------------------------------------------------------------------------------------
**********
READ.FILE:
**********

    RCB.ERR = ''
    CALL CACHE.READ(FN.REDO.CLEARING.PROCESS,CLEARING.PROCESS.ID,R.REDO.CLEARING.PROCESS,RCB.ERR)

    VAR.FILE.NAME = R.REDO.CLEARING.PROCESS<PRE.PROCESS.OUT.PROCESS.NAME>
    VAR.FILE.PATH = R.REDO.CLEARING.PROCESS<PRE.PROCESS.OUT.PROCESS.PATH>

    FN.APERTA = VAR.FILE.PATH
    F.APERTA = ''
    CALL OPF(FN.APERTA,F.APERTA)
    RMT.ERR = ''
    CALL CACHE.READ(FN.REDO.MAPPING.TABLE,'INW.PROCESS',R.REDO.MAPPING.TABLE,RMT.ERR)
    RAC.ERR = ''
    CALL CACHE.READ(FN.REDO.APAP.CLEAR.PARAM,"SYSTEM",R.REDO.APAP.CLEAR.PARAM,RAC.ERR)
    CALL CACHE.READ(FN.COLLECT.PARAM,"SYSTEM",R.COLLECT.PARAM,CP.ERR)
* PACS00188869 - S
*   OUT.SETTLEMENT = R.REDO.APAP.CLEAR.PARAM<CLEAR.PARAM.OUT.SETTLEMENT>
    OUT.SETTLEMENT = R.REDO.APAP.CLEAR.PARAM<CLEAR.PARAM.UNIV.CLEAR.ACCT>
* PACS00188869 - E
    CUTOFF.TIME = R.REDO.APAP.CLEAR.PARAM<CLEAR.PARAM.CUTOFF>
    ADJUST.CR.CODE = R.REDO.APAP.CLEAR.PARAM<CLEAR.PARAM.ADJUST.CR.CODE>
    ADJUST.DR.CODE = R.REDO.APAP.CLEAR.PARAM<CLEAR.PARAM.ADJUST.DR.CODE>
* PACS00188869 - S
*   CCY.OUT.SETTLEMENT = R.COLLECT.PARAM<COLLECT.PARAM.SETTLE.CCY>
    CCY.OUT.SETTLEMENT = R.COLLECT.PARAM<COLLECT.PARAM.UNIV.CLEAR.CCY>
*   ACCT.SETTLEMENT    = R.COLLECT.PARAM<COLLECT.PARAM.SETTLE.ACCOUNT>
    ACCT.SETTLEMENT    = R.COLLECT.PARAM<COLLECT.PARAM.UNIV.CLEAR.ACCT>
* PACS00188869 - E
    FORWARD.DAYS = R.REDO.APAP.CLEAR.PARAM<CLEAR.PARAM.FORWARD.DAYS>
    BANK.CATEGORY = R.REDO.APAP.CLEAR.PARAM<CLEAR.PARAM.BANK.CATEGORY>
    OPERAND = R.REDO.APAP.CLEAR.PARAM<CLEAR.PARAM.OPERAND>
    PARAM.AMOUNT = R.REDO.APAP.CLEAR.PARAM<CLEAR.PARAM.AMOUNT>
    CP.ERR = ''
    LIST.CURRENCY = R.COLLECT.PARAM<COLLECT.PARAM.CURRENCY>
    CUST.TYPE = R.COLLECT.PARAM<COLLECT.PARAM.CUST.TYPE>
    CUTOFF.TIME.FCY = R.COLLECT.PARAM<COLLECT.PARAM.CUTOFF>
    IN.TRANS.DAYS = R.COLLECT.PARAM<COLLECT.PARAM.IN.TRANS.DAYS>
    CALL CACHE.READ(FN.REDO.H.ROUTING.HEADER,"SYSTEM",R.REDO.H.ROUTING.HEADER,ROUTE.ERR)

    Y.INW.CCY.TYPE =     R.REDO.H.ROUTING.HEADER<REDO.HEAD.INW.CCY.TYPE>
    Y.INW.DOC.TYPE =     R.REDO.H.ROUTING.HEADER<REDO.HEAD.INW.DOC.TYPE>
    Y.INW.CAT.VAL  =     R.REDO.H.ROUTING.HEADER<REDO.HEAD.INW.CAT.VAL>
    Y.OUT.CCY.TYPE =     R.REDO.H.ROUTING.HEADER<REDO.HEAD.OUT.CCY.TYPE>
    Y.OUT.DOC.TYPE =     R.REDO.H.ROUTING.HEADER<REDO.HEAD.OUT.DOC.TYPE>
    Y.OUT.CAT.VAL  =     R.REDO.H.ROUTING.HEADER<REDO.HEAD.OUT.CAT.VAL>

    CHANGE @VM TO @FM IN Y.INW.CCY.TYPE
    CHANGE @VM TO @FM IN Y.INW.DOC.TYPE
    CHANGE @VM TO @FM IN Y.INW.CAT.VAL
    CHANGE @VM TO @FM IN Y.OUT.CCY.TYPE
    CHANGE @VM TO @FM IN Y.OUT.DOC.TYPE
    CHANGE @VM TO @FM IN Y.OUT.CAT.VAL

    Y.LR.POS = ''
    LOC.REF.APP = 'CUSTOMER':@FM:'T24.FUND.SERVICES':@FM:'AZ.PRODUCT.PARAMETER'
    LOC.REF.FIELD = 'L.CU.TIPO.CL':@FM:'L.TT.PROCESS':@VM:'L.FT.ADD.INFO':@FM:'L.AZP.TRAN.DAYS'
    LOC.REF.POS = ''
    CALL MULTI.GET.LOC.REF(LOC.REF.APP,LOC.REF.FIELD,LOC.REF.POS)

    Y.LR.POS = LOC.REF.POS<1,1>
    Y.TT.PROCESS.POS = LOC.REF.POS<2,1>
    Y.AZ.REF.TFS  = LOC.REF.POS<2,2>
    Y.AZP.TRAN.DAYS.POS = LOC.REF.POS<3,1>

RETURN

*----------------------------------------------------------------------------------------------------------------
END
