* @ValidationCode : Mjo2MjU3ODE1MDg6Q3AxMjUyOjE2ODIwNzIyMjUwNzY6SVRTU0JORzotMTotMTowOjA6ZmFsc2U6Ti9BOkRFVl8yMDIxMDguMDotMTotMQ==
* @ValidationInfo : Timestamp         : 21 Apr 2023 15:47:05
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
*21-04-2023           CONVERSION TOOL                AUTO R22 CODE CONVERSION                BP REMOVED, = TO EQ, > TO GT
*21-04-2023          jayasurya H                       MANUAL R22 CODE CONVERSION            NO CHANGES
*--------------------------------------------------------------------------------------------------------------------------
SUBROUTINE  L.APAP.VM.TO.JSON.ARRAY(VM.IN.FIELDS, JSON.ARRAY.OUT)
    $INSERT I_COMMON ;* AUTO R22 CODE CONVERSION START
    $INSERT I_EQUATE ;* AUTO R22 CODE CONVERSION END

*Subroutine Convert a @VM to Json Array.
*----------------------------------------------------------------------------------------------------------------------------------------------------
*DEBUG
    Y.VM.FIELD = DCOUNT(VM.IN.FIELDS,@VM)
    JSON.ARRAY.OUT  = ''

    IF Y.VM.FIELD GT 0 THEN ;* AUTO R22 CODE CONVERSION > TO GT
        FOR Y.I = 1 TO Y.VM.FIELD
            IF Y.I EQ 1 THEN ;* AUTO R22 CODE CONVERSION = TO EQ
                JSON.ARRAY.OUT =  QUOTE(VM.IN.FIELDS<1,Y.I>)
            END
            ELSE
                JSON.ARRAY.OUT := ',' : QUOTE(VM.IN.FIELDS<1,Y.I>)
            END
        NEXT Y.I
    END
    JSON.ARRAY.OUT  =  '[' : JSON.ARRAY.OUT : ']'

RETURN
END
