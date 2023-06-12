*-----------------------------------------------------------------------------
* <Rating>690</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE L.APAP.B.TASAS.ACTIV.PASIV.LOAD
*
* Client Name   : APAP
* Develop By    : Ashokkumar
* Description   : The routine to generate the Activasa and Pasivas report AR010.
*
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.ACCOUNT
    $INSERT I_F.AZ.ACCOUNT
    $INSERT I_F.CUSTOMER
    $INSERT I_F.AA.ARRANGEMENT
    $INSERT I_F.AZ.PRODUCT.PARAMETER
    $INSERT I_F.COLLATERAL
    $INSERT I_F.DATES
    $INCLUDE REGREP.BP I_F.DR.REG.PASIVAS.PARAM
    $INSERT TAM.BP I_F.REDO.H.REPORTS.PARAM
    $INSERT I_L.APAP.B.TASAS.ACTIV.PASIV.COMMON



    GOSUB INIT
    GOSUB GET.LOC.REF
    RETURN

INIT:
*****
    FN.ACCOUNT = 'F.ACCOUNT'; F.ACCOUNT = ''
    CALL OPF(FN.ACCOUNT,F.ACCOUNT)
    FN.AZ.ACCOUNT = 'F.AZ.ACCOUNT'; F.AZ.ACCOUNT = ''
    CALL OPF(FN.AZ.ACCOUNT,F.AZ.ACCOUNT)
    FN.AA.ARRANGEMENT = 'F.AA.ARRANGEMENT'; F.AA.ARRANGEMENT = ''
    CALL OPF(FN.AA.ARRANGEMENT,F.AA.ARRANGEMENT)
    FN.CUSTOMER = 'F.CUSTOMER'; F.CUSTOMER = ''
    CALL OPF(FN.CUSTOMER,F.CUSTOMER)
    FN.BASIC.INTEREST = 'F.BASIC.INTEREST' ; F.BASIC.INTEREST = ''
    CALL OPF(FN.BASIC.INTEREST,F.BASIC.INTEREST)
    FN.GROUP.DATE = 'F.GROUP.DATE' ; F.GROUP.DATE = ''
    CALL OPF(FN.GROUP.DATE,F.GROUP.DATE)
    FN.PERIODIC.INTEREST = 'F.PERIODIC.INTEREST'  ; F.PERIODIC.INTEREST = ''
    CALL OPF(FN.PERIODIC.INTEREST,F.PERIODIC.INTEREST)
    FN.AZ.PRODUCT.PARAMETER = 'F.AZ.PRODUCT.PARAMETER' ; F.AZ.PRODUCT.PARAMETER = ''
    CALL OPF(FN.AZ.PRODUCT.PARAMETER,F.AZ.PRODUCT.PARAMETER)
    FN.DR.REG.PASIVAS.PARAM = 'F.DR.REG.PASIVAS.PARAM' ; F.DR.REG.PASIVAS.PARAM = ''
    CALL OPF(FN.DR.REG.PASIVAS.PARAM,F.DR.REG.PASIVAS.PARAM)
    FN.COLLATERAL = 'F.COLLATERAL'; F.COLLATERAL = ''
    CALL OPF(FN.COLLATERAL,F.COLLATERAL)
    FN.AA.ARR.TERM.AMOUNT = 'F.AA.ARR.TERM.AMOUNT'; F.AA.ARR.TERM.AMOUNT = ''
    CALL OPF(FN.AA.ARR.TERM.AMOUNT,F.AA.ARR.TERM.AMOUNT)
    FN.DR.REG.PASIVAS.ACTIV = 'F.DR.REG.PASIVAS.ACTIV'; F.DR.REG.PASIVAS.ACTIV = ''
    CALL OPF(FN.DR.REG.PASIVAS.ACTIV,F.DR.REG.PASIVAS.ACTIV)
    FN.BASIC.INTEREST = 'F.BASIC.INTEREST'; F.BASIC.INTEREST =''
    CALL OPF(FN.BASIC.INTEREST,F.BASIC.INTEREST)
    FN.PERIODIC.INTEREST = 'F.PERIODIC.INTEREST'; F.PERIODIC.INTEREST =''
    CALL OPF(FN.PERIODIC.INTEREST,F.PERIODIC.INTEREST)
    FN.GROUP.DATE = 'F.GROUP.DATE'; F.GROUP.DATE = ''
    CALL OPF(FN.GROUP.DATE,F.GROUP.DATE)
    FN.REDO.H.REPORTS.PARAM = 'F.REDO.H.REPORTS.PARAM'; F.REDO.H.REPORTS.PARAM = ''
    CALL OPF(FN.REDO.H.REPORTS.PARAM,F.REDO.H.REPORTS.PARAM)
    FN.REDO.APAP.INSTIT.FINANC.PARAM = 'F.REDO.APAP.INSTIT.FINANC.PARAM'; F.REDO.APAP.INSTIT.FINANC.PARAM = ''
    CALL OPF(FN.REDO.APAP.INSTIT.FINANC.PARAM,F.REDO.APAP.INSTIT.FINANC.PARAM)

    LAST.WORK.DAY = R.DATES(EB.DAT.LAST.WORKING.DAY)
    CALL CACHE.READ(FN.DR.REG.PASIVAS.PARAM,'SYSTEM',R.DR.REG.PASIVAS.PARAM,DR.REG.PASIVAS.PARAM.ERR)
    CAT.LIST3 = R.DR.REG.PASIVAS.PARAM<DR.PASIVAS.PARAM.GRP16.CATEGORY>
    CHANGE VM TO FM IN CAT.LIST3

    REDO.H.REPORTS.PARAM.ID = 'REDO.TASAS.PASIV'
    ERR.REDO.H.REPORTS.PARAM = ''; R.REDO.H.REPORTS.PARAM = ''
    CALL F.READ(FN.REDO.H.REPORTS.PARAM,REDO.H.REPORTS.PARAM.ID,R.REDO.H.REPORTS.PARAM,F.REDO.H.REPORTS.PARAM,ERR.REDO.H.REPORTS.PARAM)
    Y.FIELD.NME.ARR = R.REDO.H.REPORTS.PARAM<REDO.REP.PARAM.FIELD.NAME>
    Y.FIELD.VAL.ARR = R.REDO.H.REPORTS.PARAM<REDO.REP.PARAM.FIELD.VALUE>
    Y.DISP.TEXT.ARR = R.REDO.H.REPORTS.PARAM<REDO.REP.PARAM.DISPLAY.TEXT>

    LOCATE "CODIGO.PROVINCIA" IN Y.FIELD.NME.ARR<1,1> SETTING COMP.POS THEN
        Y.COMP.VAL.ARR = Y.FIELD.VAL.ARR<1,COMP.POS>
        Y.COMP.DIS.ARR = Y.DISP.TEXT.ARR<1,COMP.POS>
    END
    Y.COMP.VAL.ARR = CHANGE(Y.COMP.VAL.ARR,SM,VM)
    Y.COMP.DIS.ARR = CHANGE(Y.COMP.DIS.ARR,SM,VM)

    LOCATE "GRUPO.CONTRAPARTE" IN Y.FIELD.NME.ARR<1,1> SETTING SECT.POS THEN
        Y.SECT.VAL.ARR = Y.FIELD.VAL.ARR<1,SECT.POS>
        Y.SECT.DIS.ARR = Y.DISP.TEXT.ARR<1,SECT.POS>
    END
    Y.SECT.VAL.ARR = CHANGE(Y.SECT.VAL.ARR,SM,VM)
    Y.SECT.DIS.ARR = CHANGE(Y.SECT.DIS.ARR,SM,VM)

    LOCATE "TIPO.DE.EDIFICATION" IN Y.FIELD.NME.ARR<1,1> SETTING TIPO.POS THEN
        Y.TIPO.VAL.ARR = Y.FIELD.VAL.ARR<1,TIPO.POS>
        Y.TIPO.DIS.ARR = Y.DISP.TEXT.ARR<1,TIPO.POS>
    END
    Y.TIPO.VAL.ARR = CHANGE(Y.TIPO.VAL.ARR,SM,VM)
    Y.TIPO.DIS.ARR = CHANGE(Y.TIPO.DIS.ARR,SM,VM)

****************************************
    LOCATE "ACTIV.CONDICION.OPERACION.AC" IN Y.FIELD.NME.ARR<1,1> SETTING CONDAC.POS THEN
        Y.CONDAC.DIS.ARR = Y.DISP.TEXT.ARR<1,CONDAC.POS>
    END
    LOCATE "ACTIV.TIPO.OPERACION.AC" IN Y.FIELD.NME.ARR<1,1> SETTING TIPOAC.POS THEN
        Y.TIPOAC.DIS.ARR = Y.DISP.TEXT.ARR<1,TIPOAC.POS>
    END
    LOCATE "ACTIV.TIPO.OPERACION.AZ" IN Y.FIELD.NME.ARR<1,1> SETTING TIPOAZ.POS THEN
        Y.TIPOAZ.DIS.ARR = Y.DISP.TEXT.ARR<1,TIPOAZ.POS>
    END
    LOCATE "PASIV.TIPO.OPERACION.AA" IN Y.FIELD.NME.ARR<1,1> SETTING TIPOAA.POS THEN
        Y.TIPOAA.DIS.ARR = Y.DISP.TEXT.ARR<1,TIPOAA.POS>
    END
    LOCATE "GRP.ACTIVO.SECTOR.AC" IN Y.FIELD.NME.ARR<1,1> SETTING GRPSECTAC.POS THEN
        Y.GRPSECTAC.DIS.ARR = Y.DISP.TEXT.ARR<1,GRPSECTAC.POS>
    END
    LOCATE "GRP.ACTIVO.SECTOR.AZ" IN Y.FIELD.NME.ARR<1,1> SETTING GRPSECTAZ.POS THEN
        Y.GRPSECTAZ.VAL.ARR = Y.FIELD.VAL.ARR<1,GRPSECTAZ.POS>
        Y.GRPSECTAZ.DIS.ARR = Y.DISP.TEXT.ARR<1,GRPSECTAZ.POS>
    END
    Y.GRPSECTAZ.VAL.ARR = CHANGE(Y.GRPSECTAZ.VAL.ARR,SM,VM)
    Y.GRPSECTAZ.DIS.ARR = CHANGE(Y.GRPSECTAZ.DIS.ARR,SM,VM)
    LOCATE "GRP.PASIVO.SECTOR.AA" IN Y.FIELD.NME.ARR<1,1> SETTING GRPSECTAA.POS THEN
        Y.GRPSECTAA.VAL.ARR = Y.FIELD.VAL.ARR<1,GRPSECTAA.POS>
        Y.GRPSECTAA.DIS.ARR = Y.DISP.TEXT.ARR<1,GRPSECTAA.POS>
    END
    Y.GRPSECTAA.VAL.ARR = CHANGE(Y.GRPSECTAA.VAL.ARR,SM,VM)
    Y.GRPSECTAA.DIS.ARR = CHANGE(Y.GRPSECTAA.DIS.ARR,SM,VM)

    LOCATE "ACTIV.SUBSECTOR.AC" IN Y.FIELD.NME.ARR<1,1> SETTING SUBSAC.POS THEN
        Y.SUBSAC.DIS.ARR = Y.DISP.TEXT.ARR<1,SUBSAC.POS>
    END
    LOCATE "ACTIV.SUBSECTOR.AZ" IN Y.FIELD.NME.ARR<1,1> SETTING SUBSAZ.POS THEN
        Y.SUBSAZ.DIS.ARR = Y.DISP.TEXT.ARR<1,SUBSAZ.POS>
    END
    LOCATE "ACTIV.GRUPO.CONTRAPARTE.AC" IN Y.FIELD.NME.ARR<1,1> SETTING GRPCONTAC.POS THEN
        Y.GRPCONTAC.DIS.ARR = Y.DISP.TEXT.ARR<1,GRPCONTAC.POS>
    END
    LOCATE "ACTIV.CONTRAPARTE.AC" IN Y.FIELD.NME.ARR<1,1> SETTING CONTRAAC.POS THEN
        Y.CONTRAAC.DIS.ARR = Y.DISP.TEXT.ARR<1,CONTRAAC.POS>
    END
    LOCATE "ACTIV.IDENTIFIC.CONTRAPARTE.AC" IN Y.FIELD.NME.ARR<1,1> SETTING IDENCONTAC.POS THEN
        Y.IDENCONTAC.DIS.ARR = Y.DISP.TEXT.ARR<1,IDENCONTAC.POS>
    END
    LOCATE "ACTIV.TIPO.IDENTIFICATION.AC" IN Y.FIELD.NME.ARR<1,1> SETTING TIPOIDENAC.POS THEN
        Y.TIPOIDENAC.DIS.ARR = Y.DISP.TEXT.ARR<1,TIPOIDENAC.POS>
    END
    LOCATE "ACTIV.PLAZA.EXACTO.AC" IN Y.FIELD.NME.ARR<1,1> SETTING PLAZAEXTAC.POS THEN
        Y.PLAZAEXTAC.DIS.ARR = Y.DISP.TEXT.ARR<1,PLAZAEXTAC.POS>
    END
    LOCATE "ACTIV.CODIGO.PLAZO.AC" IN Y.FIELD.NME.ARR<1,1> SETTING CONDIPLAZAC.POS THEN
        Y.CONDIPLAZAC.DIS.ARR = Y.DISP.TEXT.ARR<1,CONDIPLAZAC.POS>
    END
    LOCATE "ACTIV.TASA.INTERES.PREFERENCIAL.AC" IN Y.FIELD.NME.ARR<1,1> SETTING TASAINT.PREFAC.POS THEN
        Y.TASAINT.PREFAC.DIS.ARR = Y.DISP.TEXT.ARR<1,TASAINT.PREFAC.POS>
    END
    LOCATE "ACTIV.TASA.INTERES.PREFERENCIAL.AZ" IN Y.FIELD.NME.ARR<1,1> SETTING TASAINT.PREFAZ.POS THEN
        Y.TASAINT.PREFAZ.VAL.ARR = Y.FIELD.VAL.ARR<1,TASAINT.PREFAZ.POS>
        Y.TASAINT.PREFAZ.DIS.ARR = Y.DISP.TEXT.ARR<1,TASAINT.PREFAZ.POS>
    END
    Y.TASAINT.PREFAZ.VAL.ARR = CHANGE(Y.TASAINT.PREFAZ.VAL.ARR,SM,VM)
    Y.TASAINT.PREFAZ.DIS.ARR = CHANGE(Y.TASAINT.PREFAZ.DIS.ARR,SM,VM)

    LOCATE "ACTIV.TASAFIJA.VARIABLE.AC" IN Y.FIELD.NME.ARR<1,1> SETTING TASAFIJAAC.POS THEN
        Y.TASAFIJAAC.DIS.ARR = Y.DISP.TEXT.ARR<1,TASAFIJAAC.POS>
    END
    LOCATE "ACTIV.TASAFIJA.VARIABLE.AZ" IN Y.FIELD.NME.ARR<1,1> SETTING TASAFIJAAZ.POS THEN
        Y.TASAFIJAAZ.DIS.ARR = Y.DISP.TEXT.ARR<1,TASAFIJAAZ.POS>
    END
    LOCATE "PASIV.TASAFIJA.VARIABLE.AA" IN Y.FIELD.NME.ARR<1,1> SETTING TASAFIJAAA.POS THEN
        Y.TASAFIJAAA.DIS.ARR = Y.DISP.TEXT.ARR<1,TASAFIJAAA.POS>
    END
    LOCATE "ACTIV.TIPO.TASA.REFERENCIA.AC" IN Y.FIELD.NME.ARR<1,1> SETTING TASAREFAC.POS THEN
        Y.TASAREFAC.DIS.ARR = Y.DISP.TEXT.ARR<1,TASAREFAC.POS>
    END
    LOCATE "ACTIV.TIPO.TASA.REFERENCIA.AZ" IN Y.FIELD.NME.ARR<1,1> SETTING TASAREFAZ.POS THEN
        Y.TASAREFAZ.DIS.ARR = Y.DISP.TEXT.ARR<1,TASAREFAZ.POS>
    END
    LOCATE "PASIV.TIPO.TASA.REFERENCIA.AA" IN Y.FIELD.NME.ARR<1,1> SETTING TASAREFAA.POS THEN
        Y.TASAREFAA.DIS.ARR = Y.DISP.TEXT.ARR<1,TASAREFAA.POS>
    END
    LOCATE "ACTIV.PERIODO.TASA.REFERENCIA.AC" IN Y.FIELD.NME.ARR<1,1> SETTING PERIODTASAAC.POS THEN
        Y.PERIODTASAAC.DIS.ARR = Y.DISP.TEXT.ARR<1,PERIODTASAAC.POS>
    END
    LOCATE "ACTIV.PERIODO.TASA.REFERENCIA.AZ" IN Y.FIELD.NME.ARR<1,1> SETTING PERIODTASAAZ.POS THEN
        Y.PERIODTASAAZ.DIS.ARR = Y.DISP.TEXT.ARR<1,PERIODTASAAZ.POS>
    END
    LOCATE "PASIV.PERIODO.TASA.REFERENCIA.AA" IN Y.FIELD.NME.ARR<1,1> SETTING PERIODTASAAA.POS THEN
        Y.PERIODTASAAA.DIS.ARR = Y.DISP.TEXT.ARR<1,PERIODTASAAA.POS>
    END
    LOCATE "ACTIV.COSTO.FINANCIERO.AC" IN Y.FIELD.NME.ARR<1,1> SETTING COSTOAC.POS THEN
        Y.COSTOAC.DIS.ARR = Y.DISP.TEXT.ARR<1,COSTOAC.POS>
    END
    LOCATE "ACTIV.CODIGO.PROVINCIA.AC" IN Y.FIELD.NME.ARR<1,1> SETTING CODIGOPROCAC.POS THEN
        Y.CODIGOPROCAC.DIS.ARR = Y.DISP.TEXT.ARR<1,CODIGOPROCAC.POS>
    END
    LOCATE "ACTIV.CODIGO.MUNICIPIO.AC" IN Y.FIELD.NME.ARR<1,1> SETTING CODIGOMUNAC.POS THEN
        Y.CODIGOMUNAC.DIS.ARR = Y.DISP.TEXT.ARR<1,CODIGOMUNAC.POS>
    END
    LOCATE "ACTIV.UBICACION.VIVIENDA.AC" IN Y.FIELD.NME.ARR<1,1> SETTING UBICACIONAC.POS THEN
        Y.UBICACIONAC.DIS.ARR = Y.DISP.TEXT.ARR<1,UBICACIONAC.POS>
    END
    LOCATE "ACTIV.UBICACION.VIVIENDA.AZ" IN Y.FIELD.NME.ARR<1,1> SETTING UBICACIONAZ.POS THEN
        Y.UBICACIONAZ.DIS.ARR = Y.DISP.TEXT.ARR<1,UBICACIONAZ.POS>
    END
    LOCATE "ACTIV.METRO.CUADRADO.VIVIENDA.AC" IN Y.FIELD.NME.ARR<1,1> SETTING METROVIVIENAC.POS THEN
        Y.METROVIVIENAC.DIS.ARR = Y.DISP.TEXT.ARR<1,METROVIVIENAC.POS>
    END
    LOCATE "ACTIV.METRO.CUADRADO.VIVIENDA.AZ" IN Y.FIELD.NME.ARR<1,1> SETTING METROVIVIENAZ.POS THEN
        Y.METROVIVIENAZ.DIS.ARR = Y.DISP.TEXT.ARR<1,METROVIVIENAZ.POS>
    END
    LOCATE "ACTIV.VALOR.VIVIENDA.AC" IN Y.FIELD.NME.ARR<1,1> SETTING VALORVIVIENAC.POS THEN
        Y.VALORVIVIENAC.DIS.ARR = Y.DISP.TEXT.ARR<1,VALORVIVIENAC.POS>
    END
    LOCATE "ACTIV.VALOR.VIVIENDA.AZ" IN Y.FIELD.NME.ARR<1,1> SETTING VALORVIVIENAZ.POS THEN
        Y.VALORVIVIENAZ.DIS.ARR = Y.DISP.TEXT.ARR<1,VALORVIVIENAZ.POS>
    END
    LOCATE "ACTIV.ESTADO.VIVIENDA.AC" IN Y.FIELD.NME.ARR<1,1> SETTING ESTADOVIVIENAC.POS THEN
        Y.ESTADOVIVIENAC.DIS.ARR = Y.DISP.TEXT.ARR<1,ESTADOVIVIENAC.POS>
    END
    LOCATE "ACTIV.ESTADO.VIVIENDA.AZ" IN Y.FIELD.NME.ARR<1,1> SETTING ESTADOVIVIENAZ.POS THEN
        Y.ESTADOVIVIENAZ.DIS.ARR = Y.DISP.TEXT.ARR<1,ESTADOVIVIENAZ.POS>
    END
    LOCATE "ACTIV.TIPO.EDIFICACION.AC" IN Y.FIELD.NME.ARR<1,1> SETTING EDIFICACIONAC.POS THEN
        Y.EDIFICACIONAC.DIS.ARR = Y.DISP.TEXT.ARR<1,EDIFICACIONAC.POS>
    END
    LOCATE "ACTIV.TIPO.EDIFICACION.AZ" IN Y.FIELD.NME.ARR<1,1> SETTING EDIFICACIONAZ.POS THEN
        Y.EDIFICACIONAZ.DIS.ARR = Y.DISP.TEXT.ARR<1,EDIFICACIONAZ.POS>
    END
    LOCATE "ACTIV.CANTIDAOPERACIONES.AC" IN Y.FIELD.NME.ARR<1,1> SETTING CANTIDAOPERACIONESAC.POS THEN
        Y.CANTIDAOPERACIONESAC.DIS.ARR = Y.DISP.TEXT.ARR<1,CANTIDAOPERACIONESAC.POS>
    END
    LOCATE "ACTIV.CANTIDAOPERACIONES.AZ" IN Y.FIELD.NME.ARR<1,1> SETTING CANTIDAOPERACIONESAZ.POS THEN
        Y.CANTIDAOPERACIONESAZ.DIS.ARR = Y.DISP.TEXT.ARR<1,CANTIDAOPERACIONESAZ.POS>
    END
    LOCATE "PASIV.CANTIDAOPERACIONES.AA" IN Y.FIELD.NME.ARR<1,1> SETTING CANTIDAOPERACIONESAA.POS THEN
        Y.CANTIDAOPERACIONESAA.DIS.ARR = Y.DISP.TEXT.ARR<1,CANTIDAOPERACIONESAA.POS>
    END
    LOCATE "ACTIV.CODIGO.INSTITUTION.AC" IN Y.FIELD.NME.ARR<1,1> SETTING CONDIGINSTIAC.POS THEN
        Y.CONDIGINSTIAC.DIS.ARR = Y.DISP.TEXT.ARR<1,CONDIGINSTIAC.POS>
    END
    LOCATE "ACTIV.CODIGO.INSTITUTION.AZ" IN Y.FIELD.NME.ARR<1,1> SETTING CONDIGINSTIAZ.POS THEN
        Y.CONDIGINSTIAZ.DIS.ARR = Y.DISP.TEXT.ARR<1,CONDIGINSTIAZ.POS>
    END
    LOCATE "PASIV.CODIGO.INSTITUTION.AA" IN Y.FIELD.NME.ARR<1,1> SETTING CONDIGINSTIAA.POS THEN
        Y.CONDIGINSTIAA.DIS.ARR = Y.DISP.TEXT.ARR<1,CONDIGINSTIAA.POS>
    END
    LOCATE "ACTIV.CODIGO.NUMERO.INSTITUTION.AC" IN Y.FIELD.NME.ARR<1,1> SETTING CODNUMINSTAC.POS THEN
        Y.CODNUMINSTAC.DIS.ARR = Y.DISP.TEXT.ARR<1,CODNUMINSTAC.POS>
    END
    LOCATE "ACTIV.CODIGO.NUMERO.INSTITUTION.AZ" IN Y.FIELD.NME.ARR<1,1> SETTING CODNUMINSTAZ.POS THEN
        Y.CODNUMINSTAZ.DIS.ARR = Y.DISP.TEXT.ARR<1,CODNUMINSTAZ.POS>
    END
    LOCATE "PASIV.CODIGO.NUMERO.INSTITUTION.AA" IN Y.FIELD.NME.ARR<1,1> SETTING CODNUMINSTAA.POS THEN
        Y.CODNUMINSTAA.DIS.ARR = Y.DISP.TEXT.ARR<1,CODNUMINSTAA.POS>
    END

    Y.TODAY = R.DATES(EB.DAT.TODAY)
    Y.LAST.DAY = R.DATES(EB.DAT.LAST.WORKING.DAY)
    CALL CDT('',Y.TODAY,'-1C')
    IF Y.LAST.DAY[5,2] NE Y.TODAY[5,2] THEN
        COMI = Y.LAST.DAY[1,6]:'01'
        CALL LAST.DAY.OF.THIS.MONTH
        Y.TODAY = COMI
    END

    RETURN

GET.LOC.REF:
************
    YTA.FILENAME = 'AZ.PRODUCT.PARAMETER':FM:'CUSTOMER':FM:'AZ.ACCOUNT':FM:'AA.PRD.DES.INTEREST':FM:'AA.PRD.DES.TERM.AMOUNT':FM:'COLLATERAL':FM:'AA.PRD.DES.OVERDUE':FM:'AA.PRD.DES.ACCOUNT':FM:'AA.PRD.DES.CUSTOMER'
    YTA.FIELDNAME = 'L.AP.ABB.DEPO':FM:'L.CU.TIPO.CL':VM:'L.CU.CIDENT':VM:'L.CU.RNC':VM:'L.CU.PASS.NAT':VM:'L.CU.NOUNICO':VM:'L.CU.ACTANAC':VM:'L.LOCALIDAD':VM:'L.APAP.INDUSTRY':FM:'L.AZ.REIVSD.INT':FM:'L.AA.RT.RV.FREQ':FM:'L.AA.COL'
    YTA.FIELDNAME := FM:'L.COL.TOT.VALUA':VM:'L.COL.BLD.AREA':VM:'L.COL.DEP.VALUE':FM:'L.LOAN.STATUS.1': FM : 'L.LOAN.COND' :FM:'L.AA.LOAN.DSN':FM:'L.AA.CAMP.TY'
    YTA.FIELDPOSN = ''
    CALL MULTI.GET.LOC.REF(YTA.FILENAME,YTA.FIELDNAME,YTA.FIELDPOSN)
    L.AP.ABB.DEPO.POSN = YTA.FIELDPOSN<1,1>
    L.CU.TIPO.CL.POSN = YTA.FIELDPOSN<2,1>
    L.CU.CIDENT.POSN = YTA.FIELDPOSN<2,2>
    L.CU.RNC.POSN = YTA.FIELDPOSN<2,3>
    L.CU.PASS.NAT.POSN = YTA.FIELDPOSN<2,4>
    L.CU.NOUNICO.POSN = YTA.FIELDPOSN<2,5>
    L.CU.ACTANAC.POSN = YTA.FIELDPOSN<2,6>
    L.LOCALIDAD.POSN = YTA.FIELDPOSN<2,7>
    L.APAP.INDUSTRY.POSN = YTA.FIELDPOSN<2,8>
    L.AZ.REIVSD.INT.POSN = YTA.FIELDPOSN<3,1>
    L.AA.RT.RV.FREQ.POSN = YTA.FIELDPOSN<4,1>
    L.AA.COL.POSN = YTA.FIELDPOSN<5,1>
    L.COL.TOT.VALUA.POSN = YTA.FIELDPOSN<6,1>
    L.COL.BLD.AREA.POSN = YTA.FIELDPOSN<6,2>
    L.COL.DEP.VALUE.POSN = YTA.FIELDPOSN<6,3>
    OD.L.LOAN.STATUS1.POS = YTA.FIELDPOSN<7,1>
    Y.L.LOAN.COND.POS = YTA.FIELDPOSN<7,2>
    L.AA.LOAN.DSN.POS = YTA.FIELDPOSN<8,1>
    L.AA.CAMP.TY.POS = YTA.FIELDPOSN<9,1>

    CALL GET.LOC.REF('AA.ARR.OVERDUE','L.LOAN.COND',Y.L.LOAN.COND.POS)

    RETURN

END