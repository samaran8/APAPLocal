*-----------------------------------------------------------------------------
* <Rating>-20</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE DR.BLD.OPER.DIVISAS.LOAD
*-----------------------------------------------------------------------------
* Modification History :
* ----------------------
*   Date        Author             Modification Description
* 12-Sep-2014   V.P.Ashokkumar     PACS00318671 - Rewritten to create 2 reports
* 24-Jun-2015   Ashokkumar.V.P     PACS00466000 - Mapping changes - Fetch customer details to avoid blank.
*-----------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.DATES
    $INCLUDE LAPAP.BP I_DR.BLD.OPER.DIVISAS
    $INCLUDE TAM.BP I_F.REDO.H.REPORTS.PARAM

    GOSUB OPEN.FILES
    GOSUB INIT.PARA
    RETURN

OPEN.FILES:
***********
    FN.FOREX = 'F.FOREX'; F.FOREX = ''
    CALL OPF(FN.FOREX,F.FOREX)

    FN.FOREX.HST = 'F.FOREX$HIS'; F.FOREX.HST = ''
    CALL OPF(FN.FOREX.HST,F.FOREX.HST)

    FN.FUNDS.TRANSFER = 'F.FUNDS.TRANSFER'; F.FUNDS.TRANSFER = ''
    CALL OPF(FN.FUNDS.TRANSFER,F.FUNDS.TRANSFER)

    FN.FUNDS.TRANSFER.HST = 'F.FUNDS.TRANSFER$HIS';  F.FUNDS.TRANSFER.HST = ''
    CALL OPF(FN.FUNDS.TRANSFER.HST,F.FUNDS.TRANSFER.HST)

    FN.TELLER = 'F.TELLER'; F.TELLER = ''
    CALL OPF(FN.TELLER,F.TELLER)

    FN.TELLER.HST = 'F.TELLER$HIS'; F.TELLER.HST = ''
    CALL OPF(FN.TELLER.HST,F.TELLER.HST)

    FN.CUSTOMER = 'F.CUSTOMER'; F.CUSTOMER = ''
    CALL OPF(FN.CUSTOMER,F.CUSTOMER)

    FN.EB.LOOKUP = 'F.EB.LOOKUP'; F.EB.LOOKUP = ''
    CALL OPF(FN.EB.LOOKUP,F.EB.LOOKUP)

    FN.POS.MVMT.HST = 'F.POS.MVMT.HIST'; F.POS.MVMT.HST = ''
    CALL OPF(FN.POS.MVMT.HST,F.POS.MVMT.HST)

    FN.DR.OPER.DIVISAS.FILE = 'F.DR.OPER.DIVISAS.FILE'; F.DR.OPER.DIVISAS.FILE = ''
    CALL OPF(FN.DR.OPER.DIVISAS.FILE,F.DR.OPER.DIVISAS.FILE)

    FN.REDO.H.REPORTS.PARAM = "F.REDO.H.REPORTS.PARAM"; F.REDO.H.REPORTS.PARAM  = ""
    CALL OPF(FN.REDO.H.REPORTS.PARAM,F.REDO.H.REPORTS.PARAM)

    FN.CUSTOMER.L.CU.PASS.NAT = 'F.CUSTOMER.L.CU.PASS.NAT'; F.CUSTOMER.L.CU.PASS.NAT = ''
    CALL OPF(FN.CUSTOMER.L.CU.PASS.NAT,F.CUSTOMER.L.CU.PASS.NAT)

    FN.CUSTOMER.L.CU.RNC = 'F.CUSTOMER.L.CU.RNC'; F.CUSTOMER.L.CU.RNC = ''
    CALL OPF(FN.CUSTOMER.L.CU.RNC,F.CUSTOMER.L.CU.RNC)

    FN.CUSTOMER.L.CU.CIDENT = 'F.CUSTOMER.L.CU.CIDENT'; F.CUSTOMER.L.CU.CIDENT = ''
    CALL OPF(FN.CUSTOMER.L.CU.CIDENT,F.CUSTOMER.L.CU.CIDENT)

    FN.FT.TXN.TYPE.CONDITION = 'F.FT.TXN.TYPE.CONDITION'; F.FT.TXN.TYPE.CONDITION = ''
    CALL OPF(FN.FT.TXN.TYPE.CONDITION,F.FT.TXN.TYPE.CONDITION)

    FN.DR.REG.FD01.CONCAT = 'F.DR.REG.FD01.CONCAT'; F.DR.REG.FD01.CONCAT = ''
    CALL OPF(FN.DR.REG.FD01.CONCAT,F.DR.REG.FD01.CONCAT)

    Y.LAST.WRK.DAY = R.DATES(EB.DAT.LAST.WORKING.DAY)
    Y2.LWORK.DAY = Y.LAST.WRK.DAY
    CALL CDT('',Y2.LWORK.DAY,'-1W')
    RETURN

INIT.PARA:
**********
    FLD.POS = ''; FLD.APPL = ''; FLD.NAME = ''
    FLD.APPL = 'FOREX':FM:'FUNDS.TRANSFER':FM:'TELLER':FM:'CUSTOMER':FM:'FT.TXN.TYPE.CONDITION'
    FLD.NAME = 'L.FX.LEGAL.ID':VM:'L.FX.FX.BUY.SRC':VM:'L.FX.FX.SEL.DST':VM:'L.FX.FXSN.NUM':FM:'L.FT.LEGAL.ID':VM:'L.FT.FX.BUY.SRC':VM:'L.FT.FX.SEL.DST':VM:'L.FT.FXSN.NUM':VM:'L.FT.CHANNELS':FM:'L.TT.LEGAL.ID':VM:'L.TT.FX.BUY.SRC':VM:'L.TT.FX.SEL.DST':VM:'L.TT.FXSN.NUM':VM:'L.TT.DOC.NUM':VM:'L.TT.DOC.DESC':VM:'L.TT.CLIENT.COD':VM:'L.TT.CLIENT.NME':FM:'L.CU.TIPO.CL':FM:'L.FTTC.CHANNELS'
    CALL MULTI.GET.LOC.REF(FLD.APPL,FLD.NAME,FLD.POS)
    L.FX.LEGAL.ID.POS = FLD.POS<1,1>
    L.FX.FX.BUY.SRC.POS = FLD.POS<1,2>
    L.FX.FX.SEL.DST.POS = FLD.POS<1,3>
    L.FX.FXSN.NUM.POS = FLD.POS<1,4>
    L.FT.LEGAL.ID.POS = FLD.POS<2,1>
    L.FT.FX.BUY.SRC.POS = FLD.POS<2,2>
    L.FT.FX.SEL.DST.POS = FLD.POS<2,3>
    L.FT.FXSN.NUM.POS = FLD.POS<2,4>
    L.FT.CHANNELS.POS = FLD.POS<2,5>
    L.TT.LEGAL.ID.POS = FLD.POS<3,1>
    L.TT.FX.BUY.SRC.POS = FLD.POS<3,2>
    L.TT.FX.SEL.DST.POS = FLD.POS<3,3>
    L.TT.FXSN.NUM.POS = FLD.POS<3,4>
    L.TT.DOC.NUM.POS = FLD.POS<3,5>
    L.TT.DOC.DESC.POS = FLD.POS<3,6>
    L.TT.CLIENT.COD.POS = FLD.POS<3,7>
    L.TT.CLIENT.NME.POS = FLD.POS<3,8>
    L.CU.TIPO.CL.POS = FLD.POS<4,1>
    L.FTTC.CHANNELS.POS = FLD.POS<5,1>
    RETURN

END
