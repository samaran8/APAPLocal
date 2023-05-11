* @ValidationCode : MjotNzEyNTM2NjE4OkNwMTI1MjoxNjgxMzcyMDc0NTcwOklUU1NCTkc6LTE6LTE6MDowOmZhbHNlOk4vQTpERVZfMjAyMTA4LjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 13 Apr 2023 13:17:54
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
SUBROUTINE REDO.CUS.ID
*-----------------------------------------------------------------------------
*Date Name Ref.ID Description
*15-07-2011 Sudharsanan S PACS00087225 Initial Creation
*-------------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.CUSTOMER
    $INSERT I_GTS.COMMON
    $INSERT I_REDO.V.VAL.CED.IDENT.COMMON
    IF V$FUNCTION EQ 'I' THEN
        GOSUB OPENFILES
        GOSUB PROCESS
    END
RETURN
*------------------------------------------------------------------------
OPENFILES:
*------------------------------------------------------------------------
    FN.CUSTOMER='F.CUSTOMER'
    F.CUSTOMER=''
    CALL OPF(FN.CUSTOMER,F.CUSTOMER)
    FN.CUSTOMER$NAU='F.CUSTOMER$NAU'
    F.CUSTOMER$NAU=''
    CALL OPF(FN.CUSTOMER$NAU,F.CUSTOMER$NAU)
RETURN
*-------------------------------------------------------------------------
PROCESS:
*-------------------------------------------------------------------------
    CALL F.READ(FN.CUSTOMER$NAU,COMI,R.CUS.LOC,F.CUSTOMER$NAU,ERR)
    IF R.CUS.LOC EQ '' THEN
        CALL F.READ(FN.CUSTOMER,COMI,R.CUS.LOC,F.CUSTOMER,ERR)
    END
    IF GTSACTIVE THEN
        IF OFS$OPERATION EQ 'BUILD' THEN
            LOC.REF.POS = ''
            CALL GET.LOC.REF('CUSTOMER','L.CU.CIDENT',LOC.REF.POS)
            VAR.INTERFACE.RNC=''
            Y.LOC.SPARE=R.CUS.LOC<EB.CUS.LOCAL.REF><1,LOC.REF.POS>
            LOC.INTERFACE=''
        END
    END
RETURN
*---------------------------------------------------------------------------
END
