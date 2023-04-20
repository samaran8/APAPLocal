*========================================================================
*-----------------------------------------------------------------------------
*-----------------------------------------------------------------------------
SUBROUTINE LAPAP.VERIFY.CATEGORY(ACC,CTG)
*========================================================================
* Technical report:
* =================
* Company Name   : APAP
* Program Name   : LAPAP.VERIFY.CATEGORY
* Date           : 2018-05-04
* Item ID        : -----------
*========================================================================
* Brief description :
* -------------------
* This a routine verify what kind of category have any account and return
* some value like 0 or 1 to indicated the expesific categories.
*========================================================================
* Modification History :
* ======================
* Date           Author            Modification Description
* -------------  -----------       ---------------------------
* 2018-05-04     Richard HC                Initial Development
*========================================================================
* Content summary :
* =================
* Table name     : ACCOUNT
* Auto Increment : N/A
* Views/versions : ALL VERSION REQUIRED
* EB record      : LAPAP.VERIFY.CATEGORY
* Routine        : LAPAP.VERIFY.CATEGORY
*========================================================================

****    D O  N O T  M O D I F Y  T H I S  R O U T I N E    ****

* A lot of requeriments could be depending to this program if you unknown
* all of those previous soluctions, take as sugerence doesn't edit any
* fragment of code content here. In case that you need solve particular
* cases, please kindly create a new soluction independent to this one.
*------------------------------------------------------------------------



    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.ACCOUNT

    GOSUB INIT
    GOSUB PROCESS

INIT:
*----
    FN.ACC = "F.ACCOUNT"
    F.ACC = ""
    CALL OPF(FN.ACC,F.ACC)
RETURN

PROCESS:
*-------
    CALL F.READ(FN.ACC,ACC,R.ACC,F.ACC,ACC.ERR)
    CATEGORY = R.ACC<AC.CATEGORY>
    IF CATEGORY GE 6000 AND CATEGORY LE 6599 THEN
        CTG = 1
    END
    IF CATEGORY GE 6600 AND CATEGORY LE 6699 THEN
        CTG = 2
    END ELSE
        CTG = 0
    END

RETURN

END
