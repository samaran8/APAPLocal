*-----------------------------------------------------------------------------
* <Rating>-26</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE PRO.RPT.MILLAS.LIBERADA.MORA(Y.ARR.ID, Y.FT.ID, Y.RETORNO)
    $INSERT T24.BP I_COMMON
    $INSERT T24.BP I_EQUATE
    $INSERT T24.BP I_F.AA.ARRANGEMENT.ACTIVITY
    $INSERT T24.BP I_F.AA.BILL.DETAILS

************************************************************
*APERTURA DE ARCHIVOS A UTILIZAR
************************************************************
    FN.AAB = "F.AA.BILL.DETAILS"
    FV.AAB = ""
    CALL OPF(FN.AAB,FV.AAB)
    FN.AAA = 'F.AA.ARRANGEMENT.ACTIVITY'
    FV.AAA = ''
    CALL OPF (FN.AAA,FV.AAA)

************************************************************
*COMANDO DE SELECION - PRINCIPAL
************************************************************
    SEL.CMD = "" ; SEL.LIST = "" ; NO.OF.RECS = "" ; SEL.ERR = ""
    SEL.CMD = "SELECT FBNK.AA.ARRANGEMENT.ACTIVITY WITH ARRANGEMENT EQ " : Y.ARR.ID :  " AND TXN.CONTRACT.ID EQ " : Y.FT.ID
    CALL EB.READLIST(SEL.CMD, SEL.LIST, '',NO.OF.RECS,SEL.ERR)
************************************************************
*INICIALIZACION DE VARIABLES
************************************************************
    Y.RETORNO = "NO"
************************************************************
*LOOP DE PROCESAMIENTO DE DATOS
************************************************************
    LOOP
        REMOVE Y.AAT.ID FROM SEL.LIST SETTING FI.POS
    WHILE Y.AAT.ID DO
        IF Y.AAT.ID NE "" THEN
            GOSUB READ.AAA.ACTIVITY
            GOSUB LBL.AABILLDETAILS
        END
    REPEAT
    RETURN

LBL.AABILLDETAILS:
************************************************************
*COMANDO DE SELECION - SECUNDARIO
************************************************************
    SEL.CMD.AAB = ""
    SEL.LIST.AAB = ""
    NO.OF.RECS.AAB = ""
    SEL.ERR.AAB = ""
    Y.REPAY.REF = ''; Y.REPAY.REF = Y.AAT.ID:"-":Y.EFFECTIVE.DATE
    SEL.CMD.AAB = "SELECT ":FN.AAB:" WITH ARRANGEMENT.ID EQ ":Y.ARR.ID
    SEL.CMD.AAB := " AND REPAY.REF EQ ":Y.REPAY.REF:" AND PROPERTY LIKE ...MORA..."
    CALL EB.READLIST(SEL.CMD.AAB, SEL.LIST.AAB, '',NO.OF.RECS.AAB, SEL.ERR.AAB)
    IF SEL.LIST.AAB EQ '' THEN
        SEL.CMD.AAB = "SELECT FBNK.AA.BILL.DETAILS.HIST WITH ARRANGEMENT.ID EQ ":Y.ARR.ID
        SEL.CMD.AAB := " AND REPAY.REF EQ ":Y.REPAY.REF:" AND PROPERTY LIKE ...MORA..."
        CALL EB.READLIST(SEL.CMD.AAB, SEL.LIST.AAB, '',NO.OF.RECS.AAB, SEL.ERR.AAB)
    END
    LOOP
        REMOVE Y.AAB.ID FROM SEL.LIST.AAB SETTING FI.POS
    WHILE Y.AAB.ID DO
        IF Y.AAB.ID NE "" THEN
            Y.RETORNO = "SI"
        END
    REPEAT
    RETURN
READ.AAA.ACTIVITY:
    CALL F.READ(FN.AAA,Y.AAT.ID,R.AAA,FV.AAA,ERROR.AA)
    Y.EFFECTIVE.DATE = R.AAA<AA.ARR.ACT.EFFECTIVE.DATE>
    RETURN
END
