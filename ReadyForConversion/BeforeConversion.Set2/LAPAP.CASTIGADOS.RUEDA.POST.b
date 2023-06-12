*-----------------------------------------------------------------------------
* <Rating>-42</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE LAPAP.CASTIGADOS.RUEDA.POST

    $INSERT T24.BP I_COMMON
    $INSERT T24.BP I_EQUATE
    $INSERT T24.BP I_BATCH.FILES
    $INSERT T24.BP I_TSA.COMMON
    $INSERT T24.BP I_F.AA.BILL.DETAILS
    $INSERT T24.BP I_F.AA.ARRANGEMENT
    $INSERT T24.BP I_F.DATES

    GOSUB MAIN.PROCESS

    RETURN

MAIN.PROCESS:
*************

    GOSUB OPEN.FILES
    GOSUB PROCESS

    RETURN

OPEN.FILES:
**********

    Y.GRUPO1 = "";
    Y.EXCLUIDOS = "";

    FN.LAPAP.CASTIGADOS.RUEDA = "F.LAPAP.CASTIGADOS.RUEDA";
    F.LAPAP.CASTIGADOS.RUEDA = "";
    CALL OPF (FN.LAPAP.CASTIGADOS.RUEDA,F.LAPAP.CASTIGADOS.RUEDA)

***directorio de salidad
    FN.CHK.DIR1 = "DMFILES";
    F.CHK.DIR1 = "";
    CALL OPF(FN.CHK.DIR1,F.CHK.DIR1)

    RETURN

PROCESS:
********

    SEL.CMD  = ''; NO.OF.RECS = ''; F.CHK.DIR = "" ; SEL.LIST= '';
    SEL.CMD = " SELECT " :FN.LAPAP.CASTIGADOS.RUEDA;
    CALL EB.READLIST(SEL.CMD, SEL.LIST, '',NO.OF.RECS,SE L.ERR)

    LOOP
        REMOVE Y.ID.RECORD FROM SEL.LIST SETTING REGISTRO.POS
    WHILE Y.ID.RECORD  DO
        CALL F.READ(FN.LAPAP.CASTIGADOS.RUEDA,Y.ID.RECORD,R.LAPAP.CASTIGADOS.RUEDA,F.LAPAP.CASTIGADOS.RUEDA,ERROR.MIGR)
        Y.ID = Y.ID.RECORD
        Y.ID = CHANGE(Y.ID,'*',FM)
        BEGIN CASE
        CASE Y.ID<2> EQ 'GRUPO1'
            Y.GRUPO1<-1> = R.LAPAP.CASTIGADOS.RUEDA
        CASE Y.ID<2> EQ 'EXCLUIDO'
            Y.EXCLUIDOS<-1> = R.LAPAP.CASTIGADOS.RUEDA
        CASE 1
            Y.VALOR = "NO REGISTRO"
        END CASE
    REPEAT

    IF Y.EXCLUIDOS NE '' THEN
        Y.FILE.NAME = 'CARGA0.CASTIGADO.EXCLUIDOS.txt';
        Y.ARREGLO = Y.EXCLUIDOS
        GOSUB CHECK.ARCHIVO.FILES
    END

    Y.FILE.NAME = 'CARGA1.AJUS.CASTIGADOS.RTC.txt';
    Y.ARREGLO = Y.GRUPO1
    GOSUB CHECK.ARCHIVO.FILES

    RETURN

CHECK.ARCHIVO.FILES:
*******************

    R.FIL = ''; READ.FIL.ERR = '';
    CALL F.READ(FN.CHK.DIR1,Y.FILE.NAME,R.FIL,F.CHK.DIR1,READ.FIL.ERR)
    IF R.FIL THEN
        DELETE F.CHK.DIR1,Y.FILE.NAME
    END

    WRITE Y.ARREGLO ON F.CHK.DIR1, Y.FILE.NAME ON ERROR
        CALL OCOMO("Error en la escritura del archivo en el directorio":F.CHK.DIR1)
    END

    RETURN

END