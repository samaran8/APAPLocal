*
*-----------------------------------------------------------------------------
*-----------------------------------------------------------------------------
SUBROUTINE DR.REG.RIEN11.EXTRACT.SELECT
*-----------------------------------------------------------------------------
* Company Name   : APAP
* Developed By   :
* Program Name   : DR.REG.RIEN11.EXTRACT
* Date           : 10-June-2013
*-----------------------------------------------------------------------------
* Description:
*------------
* This multi-thread job is meant for to extact the transactions over 1000 USD made by individual customer
*-----------------------------------------------------------------------------
*
* Modification History :
* ----------------------
*   Date          Author              Modification Description
*
*-----------------------------------------------------------------------------

    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_BATCH.FILES

    $INSERT I_DR.REG.RIEN11.EXTRACT.COMMON
    $INSERT I_F.DR.REG.RIEN11.PARAM


    GOSUB BUILD.CONTROL.LIST
    GOSUB SEL.PROCESS
RETURN

BUILD.CONTROL.LIST:
*******************
    CALL EB.CLEAR.FILE(FN.DR.REG.RIEN11.WORKFILE, FV.DR.REG.RIEN11.WORKFILE)    ;* Clear the WORK file before building for Today
    CALL EB.CLEAR.FILE(FN.DR.REG.RIEN11.WORKFILE.FCY, FV.DR.REG.RIEN11.WORKFILE.FCY)      ;* Clear the WORK file before building for Today
RETURN
*-----------------------------------------------------------------------------
SEL.PROCESS:
************

    LIST.PARAMETER = ""
    LIST.PARAMETER<2> = "F.ACCT.ENT.LWORK.DAY"
    LIST.PARAMETER<7> = "FILTER"        ;* Call Fillter Routine to filer out the Internal Accounts from process
    CALL BATCH.BUILD.LIST(LIST.PARAMETER, "")
RETURN

END
