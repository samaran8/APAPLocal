*-----------------------------------------------------------------------------
* <Rating>-11</Rating>
*-----------------------------------------------------------------------------
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

    $INSERT T24.BP I_COMMON
    $INSERT T24.BP I_EQUATE
    $INSERT T24.BP I_BATCH.FILES
    $INSERT T24.BP I_F.DATES
    $INCLUDE LAPAP.BP I_DR.REG.PEPS.TXN.EXTRACT.COMMON
    $INCLUDE TAM.BP I_F.REDO.H.REPORTS.PARAM


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
