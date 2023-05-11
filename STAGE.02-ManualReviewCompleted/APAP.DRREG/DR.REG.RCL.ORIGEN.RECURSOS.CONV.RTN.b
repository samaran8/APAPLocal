* @ValidationCode : MjoxNzgzODM0MjcwOkNwMTI1MjoxNjgwNjkwNDAwMDU2OmFqaXRoOi0xOi0xOjA6MDpmYWxzZTpOL0E6UjIxX0FNUi4wOi0xOi0x
* @ValidationInfo : Timestamp         : 05 Apr 2023 15:56:40
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : ajith
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : R21_AMR.0
* @ValidationInfo : Copyright Temenos Headquarters SA 1993-2021. All rights reserved.
$PACKAGE APAP.DRREG
*
*-----------------------------------------------------------------------------
*MODIFICATION HISTORY:
*---------------------------------------------------------------------------------------
*DATE               WHO                       REFERENCE                 DESCRIPTION
*05-04-2023       CONVERSION TOOLS            AUTO R22 CODE CONVERSION   NO CHANGE
*05-04-2023       AJITHKUMAR                  MANUAL R22 CODE CONVERSION NO CHANGE
*----------------------------------------------------------------------------------------




*-----------------------------------------------------------------------------
SUBROUTINE DR.REG.RCL.ORIGEN.RECURSOS.CONV.RTN
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.AA.ACCOUNT
    $INSERT I_DR.REG.COMM.LOAN.SECTOR.EXT.COMMON
    $INSERT I_DR.REG.COMM.LOAN.SECTOR.COMMON

    R.AA.ARRANGEMENT = RCL$COMM.LOAN(1)
** Get SECTOR
** IF SEC.VAL EQ '03.01.99' THEN
    ArrangementID = COMI
    effectiveDate = ''
    idPropertyClass = 'ACCOUNT'
    idProperty = ''
    returnIds = ''
    returnConditions = ''
    returnError = ''
    CALL AA.GET.ARRANGEMENT.CONDITIONS(ArrangementID, idPropertyClass, idProperty, effectiveDate, returnIds, returnConditions, returnError)
    R.AA.ARR.ACCOUNT = RAISE(returnConditions)
*    IF R.AA.ARR.ACCOUNT<1> EQ 'LENDING-TAKEOVER-ARRANGEMENT' OR R.AA.ARR.ACCOUNT<1> EQ 'LENDING-NEW-ARRANGEMENT' THEN
    SOURCE.OF.FUNDS = R.AA.ARR.ACCOUNT<AA.AC.LOCAL.REF,ORIGEN.RECURSOS.POS>
    IF SOURCE.OF.FUNDS EQ '01' THEN
        SOURCE.OF.FUNDS.VAL = '01'
    END ELSE
        SOURCE.OF.FUNDS.VAL = '02'
    END
*    END
*END ELSE
* SOURCE.OF.FUNDS.VAL = ''
*END
*
    COMI = SOURCE.OF.FUNDS.VAL
*
RETURN
END
