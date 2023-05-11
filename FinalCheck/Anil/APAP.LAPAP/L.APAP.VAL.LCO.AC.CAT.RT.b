* @ValidationCode : Mjo0NDM5MjQ0OTk6Q3AxMjUyOjE2ODIzMzU5NDUxNzc6SVRTUzotMTotMTo2NzE6MTpmYWxzZTpOL0E6REVWXzIwMjEwOC4wOi0xOi0x
* @ValidationInfo : Timestamp         : 24 Apr 2023 17:02:25
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : ITSS
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : 671
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : true
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : DEV_202108.0
* @ValidationInfo : Copyright Temenos Headquarters SA 1993-2021. All rights reserved.
$PACKAGE APAP.LAPAP
* Modification History:
* Date                 Who                              Reference                            DESCRIPTION
*21-04-2023           CONVERSION TOOL                AUTO R22 CODE CONVERSION                BP REMOVED, F.READ TO CACHE.READ, = TO EQ
*21-04-2023          jayasurya H                       MANUAL R22 CODE CONVERSION            NO CHANGES
*-----------------------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE L.APAP.VAL.LCO.AC.CAT.RT
    $INSERT I_COMMON ;* AUTO R22 CODE CONVERSION
    $INSERT I_EQUATE
    $INSERT I_F.ACCOUNT
    $INSERT I_F.LATAM.CARD.ORDER
    $INSERT I_F.LATAM.CARD.CUSTOMER ;* AUTO R22 CODE CONVERSION
*$INSERT BP I_F.ST.L.APAP.TD.CAT.RES

    GOSUB INI
    GOSUB PROCESS
INI:
    Y.ACCOUNT.NUMBER = COMI   ;*R.NEW(CARD.IS.ACCOUNT) SI ES INPUT ROUTINE

    FN.ACC = "FBNK.ACCOUNT"
    F.ACC = ""
    CALL OPF(FN.ACC,F.ACC)

    FN.LCC = "F.LATAM.CARD.CUSTOMER"
    F.LCC = ""
    CALL OPF(FN.LCC,F.LCC)

    FN.LCO = "F.LATAM.CARD.ORDER"
    F.LCO = ""
    CALL OPF(FN.LCO,F.LCO)

    FN.CAT.RES = "FBNK.ST.L.APAP.TD.CAT.RES"
    F.CAT.RES = ""
    CALL OPF(FN.CAT.RES,F.CAT.RES)

RETURN

PROCESS:
    GOSUB READ.CAT.RESTRICT
    GOSUB READ.ACCOUNT
    GOSUB CONSULTA.CUENTA

    Y.ACC.CATEGORY = R.ACC<AC.CATEGORY>
    Y.RES.CATEGORY = R.CAT.RES<1>       ;*ST.TDCR.ACC.CATEGORY
    FINDSTR Y.ACC.CATEGORY IN Y.RES.CATEGORY SETTING Ap, Vp THEN
        MESSAGE = "T.D. NO PUEDE SER RELACIONADA A CUENTA DE LA CATEGORIA ":Y.ACC.CATEGORY
        E = MESSAGE
        ETEXT = E
        CALL ERR
    END
RETURN

READ.ACCOUNT:
    R.ACC = ''; ACC.ERROR = '';
    CALL F.READ(FN.ACC,Y.ACCOUNT.NUMBER,R.ACC,F.ACC,ACC.ERROR)
    Y.COD.CLIENTE  = R.ACC<AC.CUSTOMER>
RETURN

READ.CAT.RESTRICT:
    R.CAT.RES = ''; CAT.RES.ERROR = '';
    CALL CACHE.READ(FN.CAT.RES, 'SYSTEM', R.CAT.RES, CAT.RES.ERROR) ;* AUTO R22 CODE CONVERSION F.READ TO CACHE.READ
RETURN

CONSULTA.CUENTA:
    Y.TIENE.TARJETA = 'false'

    CALL F.READ(FN.LCC,Y.COD.CLIENTE,R.LATAM.CUSTOMER,F.LCC,ERROR.LATAM.CUSTOMER)
    Y.ACCS.CUSTOMER = R.LATAM.CUSTOMER<APAP.DC.ACCOUNT.NO>
    Y.CARDS = R.LATAM.CUSTOMER<APAP.DC.CARD.NO>
    Y.CANT.ACC = DCOUNT(Y.ACCS.CUSTOMER,@VM)

    FOR CONT=1 TO Y.CANT.ACC STEP 1

        IF (Y.ACCS.CUSTOMER<1,CONT> EQ Y.ACCOUNT.NUMBER) THEN ;* AUTO R22 CODE CONVERSION = TO EQ
            LATAM.NO = Y.CARDS<1,CONT>

            CALL F.READ(FN.LCO,LATAM.NO,R.LATAM.ORDER,F.LCO,ERROR.LATAM)
            Y.CARD.STATUS = R.LATAM.ORDER<CARD.IS.CARD.STATUS>

            IF (Y.CARD.STATUS EQ 90 OR Y.CARD.STATUS EQ 94 OR Y.CARD.STATUS EQ 75 OR Y.CARD.STATUS EQ 74) THEN
                Y.TIENE.TARJETA = 'true'
            END
        END

    NEXT CONT

    IF (Y.COD.CLIENTE NE "" AND Y.TIENE.TARJETA EQ 'true' ) THEN
        R.NEW(CARD.IS.TYPE.OF.CARD)  = "ADICIONAL"
        R.NEW(CARD.IS.PROSPECT.ID) = Y.COD.CLIENTE
    END

RETURN

END
