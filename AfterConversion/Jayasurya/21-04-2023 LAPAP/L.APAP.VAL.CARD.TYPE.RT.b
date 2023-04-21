* @ValidationCode : MjoyMDU1MTM2ODY1OkNwMTI1MjoxNjgyMDcwNzUyOTU5OklUU1NCTkc6LTE6LTE6MDowOmZhbHNlOk4vQTpERVZfMjAyMTA4LjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 21 Apr 2023 15:22:32
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
*21-04-2023           CONVERSION TOOL                AUTO R22 CODE CONVERSION                 NO CHANGES
*21-04-2023          jayasurya H                       MANUAL R22 CODE CONVERSION            NO CHANGES
*-----------------------------------------------------------------------------------------------------------------------
SUBROUTINE L.APAP.VAL.CARD.TYPE.RT
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.CARD.TYPE

    FN.CT = "F.CARD.TYPE"
    F.CT = ""
    R.CT = ""
    CT.ERR = ""
    CALL OPF(FN.CT,F.CT)

    Y.ID = ''
    Y.ID = COMI

    CALL F.READ(FN.CT,Y.ID,R.CT, F.CT, CT.ERR)
    IF R.CT EQ '' THEN
        MESSAGE = "REGISTRO NO EXISTE EN CARD.TYPE."
        E = MESSAGE
        ETEXT = E
        CALL ERR
    END

END
