* @ValidationCode : Mjo5ODE3MTgyNzU6Q3AxMjUyOjE2ODIwNzIxMTExMzE6SVRTU0JORzotMTotMTowOjA6ZmFsc2U6Ti9BOkRFVl8yMDIxMDguMDotMTotMQ==
* @ValidationInfo : Timestamp         : 21 Apr 2023 15:45:11
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : ITSSBNG
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : DEV_202108.0
$PACKAGE APAP.LAPAP
* Modification History:
* Date                 Who                              Reference                            DESCRIPTION
*21-04-2023           CONVERSION TOOL                AUTO R22 CODE CONVERSION                BP REMOVED
*21-04-2023          jayasurya H                       MANUAL R22 CODE CONVERSION            NO CHANGES
*-------------------------------------------------------------------------------------------------------
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
    $INSERT I_COMMON ;* AUTO R22 CODE CONVERSION START
    $INSERT I_EQUATE
    $INSERT I_F.AZ.ACCOUNT
    $INSERT I_F.ACCOUNT ;* AUTO R22 CODE CONVERSION END

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
