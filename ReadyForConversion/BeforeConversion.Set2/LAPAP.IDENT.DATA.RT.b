*-----------------------------------------------------------------------------
* <Rating>-12</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE LAPAP.IDENT.DATA.RT
    $INSERT T24.BP I_COMMON
    $INSERT T24.BP I_EQUATE
    $INSERT T24.BP I_F.CUSTOMER
    $INSERT BP I_F.ST.LAPAP.MOD.DIRECCIONES
    $INSERT T24.BP I_F.EB.LOOKUP

    IF V$FUNCTION EQ 'S' THEN
        RETURN
    END

*Y.DUMMY = R.NEW(ST.MDIR.CLIENTE)
    WVAR.NAMES = "CURRENT.CUSTOMER"
    WVAR.VAL = ""
    WPOS.X = 0

    CALL System.getUserVariables(U.VARNAMES,U.VARVALS)

    LOOP
        REMOVE WWVAR FROM WVAR.NAMES SETTING WVAR.POS
    WHILE WWVAR : WVAR.POS DO
        WPOS.X += 1
        LOCATE WWVAR IN U.VARNAMES SETTING YPOS.VAR THEN
            WVAR.VAL<WPOS.X> = U.VARVALS<YPOS.VAR>
        END

    REPEAT

    Y.DUMMY = ''
    Y.DUMMY = WVAR.VAL<1>

    IF Y.DUMMY EQ '' THEN
        TEXT =  "Ha ocurrido un problema, no se obtuvo el codigo de cliente."
        CALL REM
        RETURN
    END


    FN.CUS = "FBNK.CUSTOMER"
    FV.CUS = ""

    Y.CUS.NAME = ''
    Y.CUS.LASTNAME = ''

    CALL OPF(FN.CUS,FV.CUS)

    CALL GET.LOC.REF(FN.CUS,"L.CU.RES.SECTOR",Y.SECTOR.POS)
    CALL GET.LOC.REF(FN.CUS,"L.CU.URB.ENS.RE",Y.URB.POS)
    Y.SECTOR.POS = 11
    Y.URB.POS = 14
    Y.TEL.TYPE.POS = 21
    Y.TEL.AREA.POS = 22
    Y.TEL.NO.POS = 23
    Y.TEL.EXT.POS = 24
    Y.TEL.PER.POS = 25
    Y.LOCALIDAD.POS = 40
    Y.L.CU.CIDENT.POS = 7
    Y.L.CU.PASS.NAT.POS = 31
    Y.L.CU.NOUNICO.POS = 8
*Y.IDENTIFICACION = R.NEW(ST.MDIR.IDENTIFICACION)        ;*COMI
    Y.CODIGO.CLIENTE = ''
*Y.SEL.CMD = "SELECT FBNK.CUSTOMER WITH L.CU.CIDENT EQ ":Y.IDENTIFICACION :" OR L.CU.PASS.NAT EQ ":Y.IDENTIFICACION :" OR L.CU.RNC EQ ":Y.IDENTIFICACION : " SAMPLE 1"

*CALL EB.READLIST(Y.SEL.CMD,SEL.LIST,'',NO.OF.RECS,SEL.ERR)

*LOOP REMOVE CUSTOMER.CODE FROM SEL.LIST SETTING ID.POS
*WHILE CUSTOMER.CODE DO
    CUSTOMER.CODE = Y.DUMMY
    Y.CODIGO.CLIENTE = CUSTOMER.CODE
    CALL F.READ(FN.CUS,CUSTOMER.CODE,R.CUS,FV.CUS,ERR.CUS)


    Y.CUS.NAME    = R.CUS<EB.CUS.GIVEN.NAMES>
    Y.CUS.LASTNAME   = R.CUS<EB.CUS.FAMILY.NAME>
    Y.RESIDENCE.TYPE  = R.CUS<EB.CUS.RESIDENCE.TYPE>        ;*TIPO RESIDENCIA
    Y.RESIDENCE   = R.CUS<EB.CUS.RESIDENCE>       ;*PAIS
    Y.TOWN.COUNTRY   = R.CUS<EB.CUS.TOWN.COUNTRY> ;*PROVINCIA
    Y.COUNTRY    = R.CUS<EB.CUS.COUNTRY>          ;*CIUDAD
    Y.SECTOR    = R.CUS<EB.CUS.LOCAL.REF,Y.SECTOR.POS>      ;*L.CU.RES.SECTOR
    Y.URB     = R.CUS<EB.CUS.LOCAL.REF,Y.URB.POS> ;*L.CU.URB.ENS.RE
    Y.CALLE    = R.CUS<EB.CUS.STREET>   ;*EB.CUS.STREET
    Y.LOCALIDAD   = R.CUS<EB.CUS.LOCAL.REF,Y.LOCALIDAD.POS>
    Y.NO.APTO    = R.CUS<EB.CUS.ADDRESS>          ;*NO APARTAMENTO
    Y.CODIGO.POSTAL  = R.CUS<EB.CUS.POST.CODE>    ;*CODIGO POSTAL
    Y.EMAIL    = R.CUS<EB.CUS.EMAIL.1>  ;* EMAIL
    Y.CEDULA = R.CUS<EB.CUS.LOCAL.REF,Y.L.CU.CIDENT.POS>
    Y.PASSNAT = R.CUS<EB.CUS.LOCAL.REF,Y.L.CU.PASS.NAT.POS>
    Y.NOUNICO = R.CUS<EB.CUS.LOCAL.REF,Y.L.CU.NOUNICO.POS>

    R.NEW(ST.MDIR.CLIENTE) = Y.CODIGO.CLIENTE
    R.NEW(ST.MDIR.NOMBRE.1) = Y.CUS.NAME
    CALL System.setVariable('CURRENT.CUST.NAME1',Y.CUS.NAME)
    R.NEW(ST.MDIR.NOMBRE.2) = Y.CUS.LASTNAME
    CALL System.setVariable('CURRENT.CUST.NAME2',Y.CUS.LASTNAME)
    IF Y.PASSNAT NE '' THEN
        R.NEW(ST.MDIR.IDENTIFICACION) = Y.PASSNAT
        CALL System.setVariable('CURRENT.IDENTIFICACION',Y.PASSNAT)
    END
    IF Y.NOUNICO NE '' THEN
        R.NEW(ST.MDIR.IDENTIFICACION) = Y.NOUNICO
        CALL System.setVariable('CURRENT.IDENTIFICACION',Y.NOUNICO)
    END
    IF Y.CEDULA NE '' THEN
        R.NEW(ST.MDIR.IDENTIFICACION) = Y.CEDULA
        CALL System.setVariable('CURRENT.IDENTIFICACION',Y.CEDULA)
    END
    Y.QNT.R.T = DCOUNT(Y.RESIDENCE.TYPE, @VM)
*DEBUG
    Y.STR.RESIDENCE.TYPE = ''
    Y.STR.PAIS = ''
    Y.STR.PROVINCIA = ''
    Y.STR.CIUDAD = ''
    Y.STR.SECTOR = ''
    Y.STR.URBANIZACION = ''
    Y.STR.CALLE := ''
    Y.STR.NUMERO := ''
    Y.STR.LOCALIDAD := ''

    FOR A = 1 TO Y.QNT.R.T STEP 1
        R.NEW(ST.MDIR.RESIDENCE.TYPE)<-1> = Y.RESIDENCE.TYPE<1,A>

*R.NEW(ST.MDIR.RESIDENCE.TYPE)<-1> = Y.RESIDENCE.TYPE<1,A>
*R.NEW(ST.MDIR.PAIS)<-1> = Y.RESIDENCE<1,A>
*R.NEW(ST.MDIR.PROVINCIA)<-1> = Y.TOWN.COUNTRY<1,A>
*R.NEW(ST.MDIR.CIUDAD)<-1> = Y.COUNTRY<1,A>
*R.NEW(ST.MDIR.SECTOR)<-1> = Y.SECTOR<1,A>
*R.NEW(ST.MDIR.URBANIZACION)<-1> = Y.URB<1,A>
        Y.STR.RESIDENCE.TYPE := Y.RESIDENCE.TYPE<1,A>
        Y.STR.PAIS     := Y.RESIDENCE<1,A>
        Y.STR.PROVINCIA   := Y.TOWN.COUNTRY<1,A>
        Y.STR.CIUDAD    := Y.COUNTRY<1,A>
        Y.STR.SECTOR    := Y.SECTOR<1,A>
        Y.STR.URBANIZACION   := Y.URB<1,A>
        Y.STR.CALLE    := Y.CALLE<1,A>
        Y.STR.NUMERO    := Y.NO.APTO<1,A>
        Y.STR.LOCALIDAD   := Y.LOCALIDAD<1,A>
        IF A NE Y.QNT.R.T THEN
            Y.STR.RESIDENCE.TYPE := @VM
            Y.STR.PAIS     := @VM
            Y.STR.PROVINCIA   := @VM
            Y.STR.CIUDAD    := @VM
            Y.STR.SECTOR    := @VM
            Y.STR.URBANIZACION   := @VM
            Y.STR.CALLE    := @VM
            Y.STR.NUMERO    := @VM
            Y.STR.LOCALIDAD   := @VM
        END
    NEXT A
    R.NEW(ST.MDIR.RESIDENCE.TYPE)  = Y.STR.RESIDENCE.TYPE
    CALL System.setVariable('CURRENT.RESIDENCE.TYPE',Y.STR.RESIDENCE.TYPE)
    R.NEW(ST.MDIR.PAIS)    = Y.STR.PAIS
    CALL System.setVariable('CURRENT.PAIS',Y.STR.PAIS)
    R.NEW(ST.MDIR.PROVINCIA)   = Y.STR.PROVINCIA
    CALL System.setVariable('CURRENT.PROVINCIA',Y.STR.PROVINCIA)
    R.NEW(ST.MDIR.CIUDAD)    = Y.STR.CIUDAD
    CALL System.setVariable('CURRENT.CIUDAD',Y.STR.CIUDAD)
    R.NEW(ST.MDIR.SECTOR)    = Y.STR.SECTOR
    CALL System.setVariable('CURRENT.SECTOR',Y.STR.SECTOR)
    R.NEW(ST.MDIR.URBANIZACION)  = Y.STR.URBANIZACION
    CALL System.setVariable('CURRENT.URBANIZACION',Y.STR.URBANIZACION)
    R.NEW(ST.MDIR.CALLE)    = Y.STR.CALLE
    CALL System.setVariable('CURRENT.CALLE',Y.STR.CALLE)
    R.NEW(ST.MDIR.NUMERO)    = Y.STR.NUMERO
    CALL System.setVariable('CURRENT.NUMERO',Y.STR.NUMERO)
    R.NEW(ST.MDIR.LOCALIDAD)   = Y.STR.LOCALIDAD
    CALL System.setVariable('CURRENT.LOCALIDAD',Y.STR.LOCALIDAD)

    Y.TEL.TYPE  = R.CUS<EB.CUS.LOCAL.REF,Y.TEL.TYPE.POS>
    Y.TEL.AREA  = R.CUS<EB.CUS.LOCAL.REF,Y.TEL.AREA.POS>
    Y.TEL.NO  = R.CUS<EB.CUS.LOCAL.REF,Y.TEL.NO.POS>
    Y.TEL.EXT  = R.CUS<EB.CUS.LOCAL.REF,Y.TEL.EXT.POS>
    Y.TEL.PER  = R.CUS<EB.CUS.LOCAL.REF,Y.TEL.PER.POS>

    Y.TEL.QNT = DCOUNT(Y.TEL.TYPE, @SM)

*DEBUG
    Y.STR.TIPO.TEL = ''
    Y.STR.TEL.ARA = ''
    Y.STR.TEL.NO = ''
    Y.STR.TEL.EXT = ''
    Y.STR.TEL.PER = ''
    FOR B = 1 TO Y.TEL.QNT STEP 1


        Y.STR.TIPO.TEL := Y.TEL.TYPE<1,1,B>
        Y.STR.TEL.ARA := Y.TEL.AREA<1,1,B>
        Y.STR.TEL.NO := Y.TEL.NO<1,1,B>
        Y.STR.TEL.EXT := Y.TEL.EXT<1,1,B>

        Y.STR.TEL.PER := Y.TEL.PER<1,1,B>
        IF B NE Y.TEL.QNT THEN
            Y.STR.TIPO.TEL := @VM
            Y.STR.TEL.ARA := @VM
            Y.STR.TEL.NO := @VM
            Y.STR.TEL.EXT := @VM
            Y.STR.TEL.PER := @VM
        END
    NEXT B
    R.NEW(ST.MDIR.TIPO.TELEFONO) = Y.STR.TIPO.TEL
    CALL System.setVariable('CURRENT.TIPO.TEL',Y.STR.TIPO.TEL)
    R.NEW(ST.MDIR.CODIGO.AREA)  = Y.STR.TEL.ARA
    CALL System.setVariable('CURRENT.TEL.ARA',Y.STR.TEL.ARA)
    R.NEW(ST.MDIR.NUMERO.TELEFONO) = Y.STR.TEL.NO
    CALL System.setVariable('CURRENT.TEL.NO',Y.STR.TEL.NO)
    R.NEW(ST.MDIR.EXTENSION)  = Y.STR.TEL.EXT
    CALL System.setVariable('CURRENT.TEL.EXT',Y.STR.TEL.EXT)
    R.NEW(ST.MDIR.PERSONA.CONTACT) = Y.STR.TEL.PER
    CALL System.setVariable('CURRENT.TEL.PER',Y.STR.TEL.PER)

    R.NEW(ST.MDIR.EMAIL) = Y.EMAIL
    CALL System.setVariable('CURRENT.EMAIL',Y.EMAIL)

    FN.EB.LOOKUP = 'F.EB.LOOKUP'
    F.EB.LOOKUP = ''
    CALL OPF(FN.EB.LOOKUP,F.EB.LOOKUP)

    CALL F.READ(FN.EB.LOOKUP,"PA.REVIEW.CUST*DATE.CC",R.EB.LOOKUP,F.EB.LOOKUP,ERR.MSG)
    Y.PARAM = R.EB.LOOKUP<EB.LU.DATA.VALUE,1>
    PROCESS.DATE = TODAY
    DAY.COUNT = Y.PARAM
    CALL CDT('', PROCESS.DATE, DAY.COUNT)
    R.NEW(ST.MDIR.FECHA.P.APLICAR) = PROCESS.DATE
    CALL System.setVariable('CURRENT.PROCESS.DATE',PROCESS.DATE)

*REPEAT
    CALL REBUILD.SCREEN

    RETURN
END
