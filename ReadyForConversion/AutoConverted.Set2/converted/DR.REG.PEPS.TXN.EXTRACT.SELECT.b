SUBROUTINE DR.REG.PEPS.TXN.EXTRACT.SELECT
*-----------------------------------------------------------------------------
* Company Name   : APAP
* Developed By   :
* Program Name   : DR.REG.PEPS.TXN.EXTRACT
*-----------------------------------------------------------------------------
* Description:
*------------
* This multi-thread job is meant for to extact the transactions over 10000 USD made by individual customer
*-----------------------------------------------------------------------------
*
* Modification History :
* ----------------------
*   Date          Author              Modification Description
*
* 15-Aug-2014     V.P.Ashokkumar       PACS00396224 - Initial Release
*-----------------------------------------------------------------------------

    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_BATCH.FILES
    $INSERT I_F.DATES
    $INSERT I_DR.REG.PEPS.TXN.EXTRACT.COMMON
    $INSERT I_F.REDO.H.REPORTS.PARAM


    GOSUB SEL.PROCESS
RETURN

SEL.PROCESS:
************
*
* Clear workfile before build for this run.
    CALL EB.CLEAR.FILE(FN.DR.REG.PEPS.WORKFILE,F.DR.REG.PEPS.WORKFILE)
*
    SEL.CMD = "SELECT ":FN.DR.REG.213IF01.CONCAT:" WITH CUST.RELATION EQ 'PEP' AND WITH CR.DATE GE ":YST.DAT:" AND CR.DATE LE ":YED.DAT:" BY @ID"
    CALL EB.READLIST(SEL.CMD,BUILD.LIST,'',Y.SEL.CNT,Y.ERR)
    CALL BATCH.BUILD.LIST('',BUILD.LIST)
RETURN

END
