*-----------------------------------------------------------------------------
* <Rating>-20</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE DR.REG.FD03.EXTRACT.LOAD
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
* 02-Aug-2014  Ashokkumar.V.P      PACS00316981 - Corrected the fields based on mapping.
*-----------------------------------------------------------------------------

    $INSERT T24.BP I_COMMON
    $INSERT T24.BP I_EQUATE
    $INSERT T24.BP I_BATCH.FILES
    $INSERT T24.BP I_F.DATES

    $INCLUDE LAPAP.BP I_DR.REG.FD03.EXTRACT.COMMON
    $INCLUDE REGREP.BP I_F.DR.REG.FD03.PARAM

    GOSUB OPEN.FILES
    GOSUB INIT.PARA

    RETURN
*----------------------------------------------------------
OPEN.FILES:
***********

    FN.DR.REG.FD03.WORKFILE = 'F.DR.REG.FD03.WORKFILE'
    F.DR.REG.FD03.WORKFILE = ''
    CALL OPF(FN.DR.REG.FD03.WORKFILE,F.DR.REG.FD03.WORKFILE)

    FN.CUSTOMER = 'F.CUSTOMER'
    F.CUSTOMER = ''
    CALL OPF(FN.CUSTOMER,F.CUSTOMER)

    FN.ACCOUNT = 'F.ACCOUNT'
    F.ACCOUNT = ''
    CALL OPF(FN.ACCOUNT,F.ACCOUNT)

    FN.STMT.ENTRY = 'F.STMT.ENTRY'  ; F.STMT.ENTRY = ''
    CALL OPF(FN.STMT.ENTRY, F.STMT.ENTRY)

    FN.FT.HIS = 'F.FUNDS.TRANSFER$HIS'  ; F.FT.HIS = ''
    CALL OPF(FN.FT.HIS, F.FT.HIS)

    FN.FT.TXN.TYPE.CONDITION = 'F.FT.TXN.TYPE.CONDITION'
    F.FT.TXN.TYPE.CONDITION = ''
    CALL OPF(FN.FT.TXN.TYPE.CONDITION,F.FT.TXN.TYPE.CONDITION)

    FN.DR.REG.FD03.PARAM = 'F.DR.REG.FD03.PARAM'   ; F.DR.REG.FD03.PARAM = ''
    CALL OPF(FN.DR.REG.FD03.PARAM, F.DR.REG.FD03.PARAM)

    FN.DR.REG.FD03.CONCAT = 'F.DR.REG.FD03.CONCAT'; F.DR.REG.FD03.CONCAT = ''
    CALL OPF(FN.DR.REG.FD03.CONCAT,F.DR.REG.FD03.CONCAT)

    FN.RELATION = 'F.RELATION' ; F.RELATION = ''
    CALL OPF(FN.RELATION,F.RELATION)
    RETURN

INIT.PARA:
**********
    DR.REG.FD03.PARAM.ERR = ''; R.DR.REG.FD03.PARAM = ''; FTTC.PAY.TYPES = ''
    REP.STRT.DATE = ''; REP.END.DATE = ''; YTHRESHOLD.FAMT = ''
    CALL CACHE.READ(FN.DR.REG.FD03.PARAM, "SYSTEM", R.DR.REG.FD03.PARAM, DR.REG.FD03.PARAM.ERR)
    FTTC.PAY.TYPES = R.DR.REG.FD03.PARAM<DR.FD03.PARAM.FTTC.PAY.TYPES>
    CHANGE VM TO FM IN FTTC.PAY.TYPES
    REP.STRT.DATE = R.DR.REG.FD03.PARAM<DR.FD03.PARAM.REP.START.DATE>
    REP.END.DATE = R.DR.REG.FD03.PARAM<DR.FD03.PARAM.REP.END.DATE>
    YTHRESHOLD.FAMT = R.DR.REG.FD03.PARAM<DR.FD03.PARAM.THRESHOLD.FAMT>
    LRUN.WDATE = R.DR.REG.FD03.PARAM<DR.FD03.PARAM.LAST.RUN.DATE>
    CALL CDT('',LRUN.WDATE,'+1C')
    LAST.WDAY = R.DATES(EB.DAT.LAST.WORKING.DAY)
    IF NOT(REP.STRT.DATE) AND NOT(REP.END.DATE) THEN
        REP.STRT.DATE = LRUN.WDATE
        REP.END.DATE = LAST.WDAY
    END

    FLD.POS = ''; L.FLD = ''
    L.APP = 'FT.TXN.TYPE.CONDITION':FM:'FUNDS.TRANSFER':FM:'CUSTOMER'
    FT.TXN.FLD = 'L.FTTC.PAY.TYPE'
    FT.FLDS = 'L.ID.PERS.ORD':VM:'L.FT.ACH.B.ACC':VM:'L.PAIS':VM:'L.ESTADO':VM:'L.FT.CLNT.TYPE':VM:'L.ID.PERS.BENEF':VM:'L.PROPOSITO':VM:'L.FT.PARTY.NAME':VM:'L.FT.CR.CARD.NO':VM:'BENEFIC.NAME':VM:'L.CR.FACILITY'
    CUS.FLD = 'L.CU.TIPO.CL':VM:'L.LOCALIDAD'
    L.FLD = FT.TXN.FLD:FM:FT.FLDS:FM:CUS.FLD
    CALL MULTI.GET.LOC.REF(L.APP,L.FLD,FLD.POS)
    L.FTTC.PAY.TYPE.POS = FLD.POS<1,1>
    L.ID.PERS.ORD.POS = FLD.POS<2,1>
    L.FT.ACH.B.ACC.POS = FLD.POS<2,2>
    L.PAIS.POS = FLD.POS<2,3>
    L.ESTADO.POS = FLD.POS<2,4>
    L.FT.CLNT.TYPE.POS = FLD.POS<2,5>
    L.ID.PERS.BENEF.POS = FLD.POS<2,6>
    L.PROPOSITO.POS = FLD.POS<2,7>
    L.FT.PARTY.NAME.POS = FLD.POS<2,8>
    L.FT.CR.CARD.NO.POS = FLD.POS<2,9>
    BENEFIC.NAME.POS = FLD.POS<2,10>
    L.CR.FACILITY.POS = FLD.POS<2,11>
    L.CU.TIPO.CL.POS = FLD.POS<3,1>
    L.LOCALIDAD.POS = FLD.POS<3,2>
    RETURN

END
