*-----------------------------------------------------------------------------
* <Rating>-9</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE LAPAP.1.V.CITY.DIR.RT
    $INSERT T24.BP I_COMMON
    $INSERT T24.BP I_EQUATE
    $INSERT BP I_F.ST.LAPAP.MOD.DIRECCIONES
    $INSERT BP I_F.ST.LAPAP.RD.CITIES
    $INSERT BP I_F.ST.LAPAP.RD.PROVINCES

*
    P.RESIDENCE = ""
    P.TOWN.COUNTRY = ""
    P.L.CU.URB.ENS.RE = ""
    P.L.CU.RES.SECTOR = ""


    FN.CITIES = "F.ST.LAPAP.RD.CITIES"
    F.CITIES = ""
    CALL OPF(FN.CITIES,F.CITIES)

    FN.PROVINCE = "F.ST.LAPAP.RD.PROVINCES"
    F.PROVINCE = ""
    CALL OPF(FN.PROVINCE,F.PROVINCE)
*

*
    P.RESIDENCE = R.NEW(ST.MDIR.PAIS)
    P.COUNTRY = R.NEW(ST.MDIR.PROVINCIA)
    P.TOWN.COUNTRY = R.NEW(ST.MDIR.CIUDAD)
    P.L.CU.URB.ENS.RE = R.NEW(ST.MDIR.URBANIZACION)
    P.L.CU.RES.SECTOR = R.NEW(ST.MDIR.SECTOR)
*
    P.COUNTRY = EREPLACE(P.COUNTRY," ",".")
    P.TOWN.COUNTRY = COMI     ;*EREPLACE(P.TOWN.COUNTRY," ",".")
    Y.ID.TOWN.COUNTRY = EREPLACE(P.TOWN.COUNTRY," ",".")

*CALL F.READ(FN.PROVINCE,Y.ID.TOWN.COUNTRY,R.PROVINCES,F.PROVINCE,ERR.PROVINCES)
    CALL F.READ(FN.CITIES,Y.ID.TOWN.COUNTRY,R.CITIES,F.CITIES,ERR.CITIES)

    IF P.RESIDENCE EQ "DO" THEN

*IF ERR.CITIES THEN
*    ETEXT = "INDIQUE UNO DE LOS POSIBLE VALORES DE LA LISTA PARA EL CAMPO CIUDAD"
*    CALL STORE.END.ERROR
*END

*IF ERR.PROVINCES THEN
*    ETEXT = "INDIQUE UNO DE LOS POSIBLE VALORES DE LA LISTA PARA EL CAMPO PROVINCIA"
*    CALL STORE.END.ERROR
*END

        IF P.TOWN.COUNTRY EQ '' THEN
            ETEXT = "INDIQUE UNO DE LOS POSIBLE VALORES DE LA LISTA PARA EL CAMPO CIUDAD"
            CALL STORE.END.ERROR
        END

        IF ERR.CITIES NE '' THEN
            ETEXT = "INDIQUE UNO DE LOS POSIBLE VALORES DE LA LISTA PARA EL CAMPO CIUDAD"
            CALL STORE.END.ERROR
        END

*IF P.COUNTRY EQ '' THEN
*    ETEXT = "INDIQUE UNO DE LOS POSIBLE VALORES DE LA LISTA PARA EL CAMPO PROVINCIA"
*    CALL STORE.END.ERROR
*END

    END

    RETURN

END
