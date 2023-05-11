* @ValidationCode : MjotMjA3NzE4ODYwOTpDcDEyNTI6MTY4MjA3MzQyMTA0NDpJVFNTQk5HOi0xOi0xOjA6MDpmYWxzZTpOL0E6REVWXzIwMjEwOC4wOi0xOi0x
* @ValidationInfo : Timestamp         : 21 Apr 2023 16:07:01
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
*21-04-2023           CONVERSION TOOL                AUTO R22 CODE CONVERSION                 VM TO @VM, BP REMOVED
*21-04-2023          jayasurya H                       MANUAL R22 CODE CONVERSION            CALL RTN METHOD ADDED
*--------------------------------------------------------------------------------------------------------------------------
SUBROUTINE L.APAP.VAL.LBEN.CEDULA.RT
    $INSERT I_COMMON ;* AUTO R22 CODE CONVERSION START
    $INSERT I_EQUATE
    $INSERT I_F.BENEFICIARY ;* AUTO R22 CODE CONVERSION END

    GOSUB INITIAL
    GOSUB PROCESS

INITIAL:
    Y.APPLICATION = 'BENEFICIARY'
    Y.FIELDS = 'L.BEN.DOC.ARCIB':@VM:'L.BEN.CEDULA':@VM:'L.BEN.COUNTRY':@VM:'L.BEN.GENDER'
    CALL MULTI.GET.LOC.REF(Y.APPLICATION,Y.FIELDS,Y.FIELD.POS)
    L.BEN.DOC.ARCIB.POS = Y.FIELD.POS<1,1>
    L.BEN.CEDULA.POS = Y.FIELD.POS<1,2>
    L.BEN.COUNTRY.POS = Y.FIELD.POS<1,3>
    L.BEN.GENDER.POS = Y.FIELD.POS<1,4>

    Y.IDENTIFICACION = COMI
    Y.TIPO.IDENTIFICACION = R.NEW(ARC.BEN.LOCAL.REF)<1,L.BEN.DOC.ARCIB.POS>
RETURN

PROCESS:
    IF Y.TIPO.IDENTIFICACION EQ 'CEDULA' OR Y.TIPO.IDENTIFICACION EQ 'Cedula' THEN
        OUT.ARR = '';
        CALL APAP.LAPAP.L.APAP.VERIF.CEDULA.RT(Y.IDENTIFICACION,OUT.ARR) ;* MANUAL R22 CODE CONVERSION
        V1 = OUT.ARR<1>
        V2 = OUT.ARR<2>
        V3 = OUT.ARR<3>
        IF V2 EQ '-1' THEN
            MESSAGE = V3
            E = MESSAGE
            ETEXT = E
            CALL ERR
        END
    END
RETURN

END
