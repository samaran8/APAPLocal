*-----------------------------------------------------------------------------
* <Rating>-10</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE DR.REG.FD01.LASTD.EXTRACT.SELECT
*-----------------------------------------------------------------------------
* Company Name   : APAP
* Program Name   : DR.REG.FD01.LASTD.EXTRACT
*-----------------------------------------------------------------------------
* Description:
*------------
* This multi-thread job is meant for to extact the Buying and selling currencies
*-----------------------------------------------------------------------------
*
* Modification History :
* ----------------------
*   Date          Author              Modification Description
* 14-May-2015  Ashokkumar.V.P          PACS00309078 - Initial Version.
* 24-Jun-2015   Ashokkumar.V.P     PACS00466000 - Mapping changes - Fetch customer details to avoid blank.
*-----------------------------------------------------------------------------

    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_BATCH.FILES
    $INSERT I_F.DATES
    $INCLUDE LAPAP.BP I_DR.REG.FD01.LASTD.EXTRACT.COMMON
    $INCLUDE REGREP.BP I_F.DR.REG.FD01.PARAM

    GOSUB SEL.PROCESS
    RETURN

SEL.PROCESS:
************
    YLCCY = LCCY
    CALL EB.CLEAR.FILE(FN.DR.REG.FD01.WORKFILE, F.DR.REG.FD01.WORKFILE)
    R.DR.REG.FD01.CONCAT = ''; ERR.DR.REG.FD01.CONCAT = ''
    CALL F.READ(FN.DR.REG.FD01.CONCAT,Y.LAST.DATE.TIME,R.DR.REG.FD01.CONCAT,F.DR.REG.FD01.CONCAT,ERR.DR.REG.FD01.CONCAT)
    CALL BATCH.BUILD.LIST('',R.DR.REG.FD01.CONCAT)
    RETURN

END
