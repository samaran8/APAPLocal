*-----------------------------------------------------------------------------
* <Rating>-11</Rating>
*-----------------------------------------------------------------------------
SUBROUTINE DR.REG.RIEN5.AA.ACC.EXT.LOAD
*-----------------------------------------------------------------------------
* Company Name : APAP
* Developed By : gangadhar@temenos.com
* Program Name : DR.REG.RIEN5.AA.ACC.EXT
* Date : 3-Jun-2013
*-----------------------------------------------------------------------------
* Description:
*------------
* This multi-thread job is meant for to extact the AZ.ACCOUNT Details for PRODUCT Wise.
*-----------------------------------------------------------------------------
*
* Modification History :
* ----------------------
* Date Author Modification Description
* 05/10/2014 Ashokkumar.V.P PACS00309203 - Added credit lines loan
* 26/02/2015 Ashokkumar.V.P PACS00309203 - Just for compilation
* 18/03/2015 Ashokkumar.V.P PACS00309203 - Removed the NAB child account OPF.
* 27/03/2015 Ashokkumar.V.P PACS00309203 - Performance change
*-----------------------------------------------------------------------------

$INSERT I_COMMON
$INSERT I_EQUATE
$INSERT I_BATCH.FILES
$INSERT I_F.DATES
$INSERT I_DR.REG.RIEN5.AA.ACC.EXT.COMMON
$INSERT I_F.DR.REG.RIEN5.PARAM

GOSUB INIT.PROCESS

RETURN

*-----------------------------------------------------------------------------
INIT.PROCESS:
*-----------*
* PACS00309203 - Indeterminado field
FN.EB.CONTRACT.BALANCES = 'F.EB.CONTRACT.BALANCES'; F.EB.CONTRACT.BALANCES = ''
CALL OPF(FN.EB.CONTRACT.BALANCES,F.EB.CONTRACT.BALANCES)
* PACS00309203

FN.DR.REG.RIEN5.PARAM = 'F.DR.REG.RIEN5.PARAM'; F.DR.REG.RIEN5.PARAM = ''
CALL OPF(FN.DR.REG.RIEN5.PARAM,F.DR.REG.RIEN5.PARAM)

FN.AA.ARR.ACCOUNT = 'F.AA.ARR.ACCOUNT'; F.AA.ARR.ACCOUNT = ''
CALL OPF(FN.AA.ARR.ACCOUNT,F.AA.ARR.ACCOUNT)

FN.ACCOUNT = 'F.ACCOUNT'; F.ACCOUNT = ''
CALL OPF(FN.ACCOUNT,F.ACCOUNT)

FN.AA.ARRANGEMENT = 'F.AA.ARRANGEMENT'; F.AA.ARRANGEMENT = ''
CALL OPF(FN.AA.ARRANGEMENT,F.AA.ARRANGEMENT)

FN.DR.REG.RIEN5.REP1 = 'F.DR.REG.RIEN5.REP1'; F.DR.REG.RIEN5.REP1 = ''
CALL OPF(FN.DR.REG.RIEN5.REP1,F.DR.REG.RIEN5.REP1)

FN.DR.REG.RIEN5.REP2 = 'F.DR.REG.RIEN5.REP2'; F.DR.REG.RIEN5.REP2 = ''
CALL OPF(FN.DR.REG.RIEN5.REP2,F.DR.REG.RIEN5.REP2)

FN.DR.REG.RIEN5.REP3 = 'F.DR.REG.RIEN5.REP3'; F.DR.REG.RIEN5.REP3 = ''
CALL OPF(FN.DR.REG.RIEN5.REP3,F.DR.REG.RIEN5.REP3)

FN.REDO.AA.SCHEDULE = 'F.REDO.AA.SCHEDULE'; F.REDO.AA.SCHEDULE = ''
CALL OPF(FN.REDO.AA.SCHEDULE,F.REDO.AA.SCHEDULE)

R.DR.REG.RIEN5.PARAM = ''; DR.REG.RIEN5.PARAM.ERR = ''; PROD.GRP = ''
CALL CACHE.READ(FN.DR.REG.RIEN5.PARAM,'SYSTEM',R.DR.REG.RIEN5.PARAM,DR.REG.RIEN5.PARAM.ERR)
PROD.GRP = R.DR.REG.RIEN5.PARAM<RIEN5.PARAM.AA.PRODUCT>
CHANGE VM TO ' ' IN PROD.GRP
L.LOAN.STATUS.POS = ''
CALL GET.LOC.REF('AA.PRD.DES.OVERDUE','L.LOAN.STATUS.1',L.LOAN.STATUS.POS)

Y.TODAY = R.DATES(EB.DAT.TODAY)
LAST.WORK.DAY = R.DATES(EB.DAT.LAST.WORKING.DAY)
RETURN
*-----------------------------------------------------------------------------
END
