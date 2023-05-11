* @ValidationCode : MjozMTEzMTQ0NjpDcDEyNTI6MTY4MTM3MjIyNTkyNDpJVFNTQk5HOi0xOi0xOjA6MDpmYWxzZTpOL0E6REVWXzIwMjEwOC4wOi0xOi0x
* @ValidationInfo : Timestamp         : 13 Apr 2023 13:20:25
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : ITSSBNG
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : DEV_202108.0
$PACKAGE APAP.REDORETAIL
* Modification History:
* Date                 Who                              Reference                            DESCRIPTION
*12-04-2023           CONVERSION TOOL                AUTO R22 CODE CONVERSION                 NO CHANGES
*12-04-2023          jayasurya H                       MANUAL R22 CODE CONVERSION            NO CHANGES
SUBROUTINE REDO.CUS.OTH.FININS.THREE(CUST.ID,OTH.FIN3)
*-----------------------------------------------------------------------------
*DESCRIPTION:
*------------
* This development is for ODR Reference ODR-2010-04-0425
* This is field format routine for the deal slip to return third multivalue field
* of OTH.FIN.INST
* Input/Output:
*--------------
* IN :CUSTOMER.ID
* OUT : OTH.FIN
*
* Dependencies:
*---------------
* CALLS : -NA-
* CALLED BY : -NA-
*
* Revision History:
*------------------------------------------------------------------------------------------
*   Date               who           Reference             Description
* 28-DEC-2009          B.Renugadevi  ODR-2010-04-0425      Initial Creation
*------------------------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.CUSTOMER
    GOSUB INIT
    GOSUB PROCESS
RETURN
******
INIT:
******
    CUS.ID = CUST.ID

    FN.CUSTOMER = 'F.CUSTOMER'
    F.CUSTOMER = ''

    CALL OPF(FN.CUSTOMER,F.CUSTOMER)

RETURN
****************
PROCESS:
***************

    CALL F.READ(FN.CUSTOMER,CUS.ID,R.CUSTOMER,F.CUSTOMER,CUS.ERR)

    IF R.CUSTOMER THEN

        IF R.CUSTOMER<EB.CUS.OTHER.FIN.INST,3> NE '' THEN

            OTH.FIN3=R.CUSTOMER<EB.CUS.OTHER.FIN.INST,3>

        END ELSE
            OTH.FIN3=''
        END
    END

RETURN
END
