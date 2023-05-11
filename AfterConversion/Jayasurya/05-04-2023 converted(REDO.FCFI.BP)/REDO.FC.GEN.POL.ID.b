* @ValidationCode : MjotNTI1MjM4MTAxOkNwMTI1MjoxNjgwNjA5NzU3Mzc1OklUU1NCTkc6LTE6LTE6MDowOmZhbHNlOk4vQTpERVZfMjAyMTA4LjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 04 Apr 2023 17:32:37
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : ITSSBNG
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : DEV_202108.0
$PACKAGE APAP.REDOFCFI
SUBROUTINE REDO.FC.GEN.POL.ID(P.ID)
*-----------------------------------------------------------------------------
* Developer    : mgudino@temenos.com
* Date         : 2011-06-13
* Description  : This routine its on charge TO ASSING THE CORRECT POLICY TO THE CORRECT COLLATERAL
*                BASED ON RULES PARAMETRISED IN REDO.FC.PROD.COLL.POLICY
* Modification History:
* Date                 Who                              Reference                            DESCRIPTION
*04-04-2023           CONVERSION TOOL                AUTO R22 CODE CONVERSION                 NO CHANGES
*04-04-2023          jayasurya H                       MANUAL R22 CODE CONVERSION            NO CHANGES
*-----------------------------------------------------------------------------
* Input/Output:
* -------------
* In  :  P.ID
* Out :
*-----------------------------------------------------------------------------

    $INSERT I_COMMON
    $INSERT I_EQUATE

    GOSUB PROCESS

RETURN
*===========
PROCESS:
*===========
    UNIQUE.TIME = ''
    CALL ALLOCATE.UNIQUE.TIME(UNIQUE.TIME)
    YSYSDATE = OCONV(DATE(),"D-")
    YSYSDATE = YSYSDATE[7,4]:YSYSDATE[1,2]:YSYSDATE[4,2]
    IDPOL ="POL":YSYSDATE:UNIQUE.TIME
    CHANGE "." TO "0" IN IDPOL
    P.ID = IDPOL

RETURN


END
