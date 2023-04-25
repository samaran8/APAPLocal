*========================================================================
    SUBROUTINE LAPAP.REGEXs(string,code)
*========================================================================
* Technical report:
* =================
* Company Name   : APAP
* Program Name   : LAPAP.REGEXs
* Date           : 2018-11-29
* Item ID        : -----------
*========================================================================
* Brief description :
* -------------------
* This program allow verify string chains, returning as value 1 in
* case that found any special charecter.
*========================================================================
*========================================================================
* Modification History :
* ======================
* Date           Author            Modification Description
* -------------  -----------       ---------------------------
* 2018-11-29     Richard HC        Initial Development
*========================================================================
* Content summary :
* =================
* Table name     :N/A
* Auto Increment :N/A
* Views/versions :N/A
* EB record      :LAPAP.REGEXs
* Routine        :LAPAP.REGEXs
*========================================================================

****    D O  N O T  M O D I F Y  T H I S  R O U T I N E    ****

* A lot of requeriments could be depending to this program if you unknown
* all of those previous soluctions, take as sugerence doesn't edit any
* fragment of code content here. In case that you need solve particular
* cases, please kindly create a new soluction independent to this one.
*------------------------------------------------------------------------


    $INCLUDE T24.BP I_COMMON
    $INCLUDE T24.BP I_EQUATE


    return.code = 0
    REGEX = "0123456789'~`!@#$%^&*()_+|}{:?><,./;\[]+-*ÁÉÍÓÚÑáéíóúñ"

    SIZE = LEN(string)
    FOR A = SIZE TO 1 STEP -1

        ARG = string[A,1]
        FINDSTR ARG IN REGEX SETTING POS THEN
            return.code = 1
        END

    NEXT A

    code = return.code

    RETURN

END
