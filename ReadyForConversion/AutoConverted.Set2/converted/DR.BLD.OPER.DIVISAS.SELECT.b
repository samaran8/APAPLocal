SUBROUTINE DR.BLD.OPER.DIVISAS.SELECT
*-----------------------------------------------------------------------------
* Modification History :
* ----------------------
*   Date        Author             Modification Description
* 12-Sep-2014   V.P.Ashokkumar     PACS00318671 - Rewritten to create 2 reports.
* 24-Jun-2015   Ashokkumar.V.P     PACS00466000 - Mapping changes - Fetch customer details to avoid blank
*-----------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_BATCH.FILES
    $INSERT I_DR.BLD.OPER.DIVISAS

    GOSUB SEL.PROCESS
RETURN

SEL.PROCESS:
************
    YLCCY = LCCY
    CALL EB.CLEAR.FILE(FN.DR.OPER.DIVISAS.FILE,F.DR.OPER.DIVISAS.FILE)
    R.DR.REG.FD01.CONCAT = ''; ERR.DR.REG.FD01.CONCAT = ''
    CALL F.READ(FN.DR.REG.FD01.CONCAT,Y.LAST.WRK.DAY,R.DR.REG.FD01.CONCAT,F.DR.REG.FD01.CONCAT,ERR.DR.REG.FD01.CONCAT)
    CALL BATCH.BUILD.LIST('',R.DR.REG.FD01.CONCAT)
RETURN
END
