*-----------------------------------------------------------------------------
* <Rating>-1</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE L.APAP.V.CUS.ME.TIP.CLI.RT
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.CUSTOMER
    $INSERT I_F.DATES


    Y.L.TIP.CLI = COMI        ;*L.TIP.CLI = 521 => Recibe Ingresos Familiares.
    Y.CUST.DATE.OF.BIRTH = R.NEW(EB.CUS.DATE.OF.BIRTH)
    Y.TODAY = R.DATES(EB.DAT.TODAY)
    C.DAYS = "C"

    IF Y.CUST.DATE.OF.BIRTH NE '' THEN
        CALL CDD("",Y.CUST.DATE.OF.BIRTH,Y.TODAY,C.DAYS)

        IF (C.DAYS LT 16 AND Y.L.TIP.CLI NE '521') THEN
            MESSAGE = "EDAD CLIENTE NO PERMITE SECTOR COMERCIAL DIFERENTE A <Recibe Ingresos Familiares>"
            E = MESSAGE
            ETEXT = E
            CALL ERR
        END

    END

    RETURN

END
