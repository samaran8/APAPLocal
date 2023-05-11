*========================================================================
SUBROUTINE LAPAP.DELETE.CUST.PRD.LIST.LOAD
*========================================================================
* Technical report:
* =================
* Company Name   : APAP
* Program Name   : LAPAP.DELETE.CUST.PRD.LIST.LOAD
* Date           : 2018-01-31
* Item ID        : CN00
*========================================================================
* Brief description :
* -------------------
* This a multi-threading program for inject data in monitor interface
* without use any version.
*========================================================================
* Modification History :
* ======================
* Date           Author            Modification Description
* -------------  -----------       ---------------------------
* 2018-01-31     Richard HC        Initial Development
*========================================================================
* Content summary :
* =================
* Table name     :
* Auto Increment : N/A
* Views/versions : N/A
* EB record      : LAPAP.DELETE.CUST.PRD.LIST.LOAD
* Routine        : LAPAP.DELETE.CUST.PRD.LIST.LOAD
*========================================================================


    $INSERT I_COMMON
    $INSERT I_EQUATE

    $INSERT I_F.REDO.CUST.PRD.LIST

    FN.PRD.LIST = "F.REDO.CUST.PRD.LIST"
    F.PRD.LIST = ""
    CALL OPF(FN.PRD.LIST,F.PRD.LIST)

RETURN

END
