*-----------------------------------------------------------------------------
* <Rating>-10</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE DR.REG.TASAS.ACTIVAS.CONCAT.SELECT
*-----------------------------------------------------------------------------
* Company Name   : APAP
* Developed By   : gangadhar@temenos.com
* Program Name   : DR.REG.TASAS.ACTIVAS.CONCAT
* Date           : 27-May-2013
*-----------------------------------------------------------------------------
* Description:
*------------
* This multi-thread job is meant for to extact the Active rates
*-----------------------------------------------------------------------------
*
* Modification History :
* ----------------------
*   Date          Author              Modification Description
*  ======        ========            ==========================
* 09-Oct-2014  Ashokkumar.V.P      PACS00305233:- Changed the parameter values
*-----------------------------------------------------------------------------

    $INSERT T24.BP I_COMMON
    $INSERT T24.BP I_EQUATE
    $INSERT T24.BP I_BATCH.FILES
    $INCLUDE LAPAP.BP I_DR.REG.TASAS.ACTIVAS.CONCAT.COMMON


    GOSUB SEL.PROCESS
    RETURN

SEL.PROCESS:
************
    CALL EB.CLEAR.FILE(FN.DR.REG.ACTIVAS.GROUP, F.DR.REG.ACTIVAS.GROUP)
    LIST.PARAMETER = ""
    LIST.PARAMETER<2> = "F.AA.ARRANGEMENT"
    LIST.PARAMETER<3> = "ARR.STATUS EQ 'CURRENT' AND START.DATE EQ ":LAST.WORK.DAY
    CALL BATCH.BUILD.LIST(LIST.PARAMETER, "")
    RETURN

END
