SUBROUTINE DR.REG.FD03.UPDATE.CONCAT.LOAD
*-----------------------------------------------------------------------------
* Company Name   : APAP
* Developed By   :
* Program Name   : DR.REG.FD03.TXN.EXTRACT
* Date           : 10-June-2013
*-----------------------------------------------------------------------------
* Description:
*------------
* This multi-thread job is meant for to extact the tranactions made over 1000 by individual Customer daily.
*-----------------------------------------------------------------------------
*
* Modification History :
* ----------------------
*   Date       Author              Modification Description
* 02-Aug-2014  Ashokkumar.V.P      PACS00316981 - Updated new parameter fields.
*-----------------------------------------------------------------------------

    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_BATCH.FILES
    $INSERT I_F.DATES

    $INSERT I_DR.REG.FD03.UPDATE.CONCAT.COMMON
    $INSERT I_F.DR.REG.FD03.PARAM

    GOSUB OPEN.FILES
    GOSUB INIT.PARA
RETURN
*----------------------------------------------------------
OPEN.FILES:
***********

    FN.STMT.ENTRY = 'F.STMT.ENTRY'  ; F.STMT.ENTRY = ''
    CALL OPF(FN.STMT.ENTRY, F.STMT.ENTRY)

    FN.FT.HIS = 'F.FUNDS.TRANSFER$HIS'  ; F.FT.HIS = ''
    CALL OPF(FN.FT.HIS, F.FT.HIS)

    FN.FT.TXN.TYPE.CONDITION = 'F.FT.TXN.TYPE.CONDITION'
    F.FT.TXN.TYPE.CONDITION = ''
    CALL OPF(FN.FT.TXN.TYPE.CONDITION,F.FT.TXN.TYPE.CONDITION)

    FN.ACCT.ENT.LWORK.DAY = 'F.ACCT.ENT.LWORK.DAY'  ; F.ACCT.ENT.LWORK.DAY = ""
    CALL OPF(FN.ACCT.ENT.LWORK.DAY, F.ACCT.ENT.LWORK.DAY)

    FN.DR.REG.FD03.PARAM = 'F.DR.REG.FD03.PARAM'   ; F.DR.REG.FD03.PARAM = ''
    CALL OPF(FN.DR.REG.FD03.PARAM, F.DR.REG.FD03.PARAM)

    FN.DR.REG.FD03.CONCAT = 'F.DR.REG.FD03.CONCAT'; F.DR.REG.FD03.CONCAT = ''
    CALL OPF(FN.DR.REG.FD03.CONCAT,F.DR.REG.FD03.CONCAT)

RETURN
*-------------------------------------------------------------------
INIT.PARA:
**********
    R.DR.REG.FD03.PARAM = ''; DR.REG.FD03.PARAM.ERR = ''; YTHRESHOLD.FAMT = ''
    FTTC.PAY.TYPES = ''; L.FTTC.PAY.TYPE.POS = ''
    CALL CACHE.READ(FN.DR.REG.FD03.PARAM, "SYSTEM", R.DR.REG.FD03.PARAM, DR.REG.FD03.PARAM.ERR)
    FTTC.PAY.TYPES = R.DR.REG.FD03.PARAM<DR.FD03.PARAM.FTTC.PAY.TYPES>
    CHANGE @VM TO @FM IN FTTC.PAY.TYPES
    YTHRESHOLD.FAMT = R.DR.REG.FD03.PARAM<DR.FD03.PARAM.THRESHOLD.FAMT>
    LAST.WRK.DATE = R.DATES(EB.DAT.LAST.WORKING.DAY)
    CALL GET.LOC.REF('FT.TXN.TYPE.CONDITION','L.FTTC.PAY.TYPE',L.FTTC.PAY.TYPE.POS)
*
RETURN
*----------------------------------------------------------------
END
