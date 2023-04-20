SUBROUTINE L.APAP.VALID.DEPOSIT.NUMBER
*-------------------------------------------------------------------------------------
* Technical report:
* -----------------
* Company Name   : APAP
* Program Name   : L.APAP.VALID.DEPOSIT.NUMBER
* Date           : 2017-11-09
* Author         : RichardHC
* Item ID        : CN007204
*-------------------------------------------------------------------------------------
* Description :
* ------------
* This program allow close financial certificated with debit account previously closed
*-------------------------------------------------------------------------------------
* Modification History :
* ----------------------
* Date           Author            Modification Description
* -------------  -----------       ---------------------------
* 2017/11/09     RichardHC                      Initial development
*-------------------------------------------------------------------------------------
* Content summary :
* -----------------
* Table name     : NONE
* Auto Increment : NONE
* Views/versions : AZ.ACCOUNT,NOR.PRECLOSURE
* EB record      : L.APAP.VALID.DEPOSIT.NUMBER
* Routines       : L.APAP.VALID.DEPOSIT.NUMBER
*-------------------------------------------------------------------------------------


*Importing the neccessary libraries and tables.
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.AZ.ACCOUNT
    $INSERT I_F.ACCOUNT

*Defining the table and path
    FN.AZ.ACCOUNT = "F.AZ.ACCOUNT"
    F.AZ.ACCOUNT = ""

    FN.ACCOUNT = "F.ACCOUNT"
    F.ACCOUNT = ""

*Declaring variable and asign the browser data value
    ACC.ID = COMI

*Openig corresponding table
****
    CALL OPF(FN.ACCOUNT,F.ACCOUNT)
    CALL F.READ(FN.ACCOUNT,ACC.ID,R.ACCOUNT,F.ACCOUNT,ERR.ACCOUNT)
    AC = R.ACCOUNT<AC.CUSTOMER>

    IF AC EQ '' THEN

*Conditional validation

        L.AZ.DEBIT = ''

        COMI = L.AZ.DEBIT
        RETURN
    END

END
