*========================================================================
*-----------------------------------------------------------------------------
*-----------------------------------------------------------------------------
SUBROUTINE LAPAP.BUILD.MONITOR.LOAD(ARR)
*========================================================================
* Technical report:
* =================
* Company Name   : APAP
* Program Name   : LAPAP.BUILD.MONITOR.LOAD
* Date           : 2018-06-04
* Item ID        : --------------
*========================================================================
* Brief description :
* -------------------
* This program receive as parameter ARR array from any routine in order
* to segment each value inside and assign at the same time corresponding
* values that will be sent to LAPAP.SEND.MONITOR.LOAD program.
*========================================================================
* Modification History :
* ======================
* Date           Author            Modification Description
* -------------  -----------       ---------------------------
* 2018-06-04     Richard HC         Initial Development
*========================================================================
* Content summary :
* =================
* Table name     :CUSTOMER
* Auto Increment :N/A
* Views/versions :ALL VERSION TO REQUIRED IT
* EB record      :LAPAP.BUILD.MONITOR.LOAD
* Routine        :LAPAP.BUILD.MONITOR.LOAD
*========================================================================

****    D O  N O T  M O D I F Y  T H I S  R O U T I N E    ****

* A lot of requeriments could be depending to this program if you unknown
* all of those previous soluctions, take as sugerence doesn't edit any
* fragment of code content here. In case that you need solve particular
* cases, please kindly create a new soluction independent to this one.
*------------------------------------------------------------------------


    $INSERT I_COMMON
    $INSERT I_EQUATE


    FN.ARR = EREPLACE(ARR,",","@vm")

    ID = FN.ARR<1>
    SQL.FIELDS = FN.ARR<2>
    DATA.TYPES = FN.ARR<3>
    MON.FIELDS = FN.ARR<4>
    SQL.TABLE = FN.ARR<5>
*DEBUG
    CALL LAPAP.SEND.MONITOR.LOAD(ID,SQL.FIELDS,DATA.TYPES,MON.FIELDS,SQL.TABLE)
RETURN

END
