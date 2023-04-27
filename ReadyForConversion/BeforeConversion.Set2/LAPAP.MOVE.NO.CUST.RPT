*-----------------------------------------------------------------------------
* <Rating>-23</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE LAPAP.MOVE.NO.CUST.RPT
    $INSERT T24.BP I_COMMON
    $INSERT T24.BP I_EQUATE
    $INCLUDE T24.BP I_F.DATES
    $INSERT T24.BP I_F.HOLD.CONTROL

    GOSUB LOAD.FILE
    GOSUB PROCESS

LOAD.FILE:
    Y.TODAY = TODAY
    Y.NAME.FINAL = "Divisas_No_Clientes_":Y.TODAY:".txt"

    FN.HOLD.CONTROL = 'F.HOLD.CONTROL'
    F.HOLD.CONTROL = ''
    CALL OPF(FN.HOLD.CONTROL,F.HOLD.CONTROL)

    FN.DIR.ORIGEN   ='../bnk.data/eb/&HOLD&'
    F.DIR.ORIGEN    =''
    CALL OPF(FN.DIR.ORIGEN,F.DIR.ORIGEN)

    FN.DIR.DESTINO   ='../bnk.data/eb/&HOLD&'
    F.DIR.DESTINO   =''
    CALL OPF(FN.DIR.DESTINO,F.DIR.DESTINO)


    DIR.ORIGEN          = '../bnk.data/eb/&HOLD&'
    OPEN DIR.ORIGEN TO DIR.ORIGEN ELSE
    END
    DIR.DESTINO         = '../bnk.interface/REG.REPORTS'
    OPEN DIR.DESTINO TO DIR.DESTINO ELSE
    END

    RETURN

PROCESS:

    NO.OF.REC = ''; SEL.ERR = ''; Y.COUNT.HOLD = ''; HOLD.POS = '';
    SEL.CMD = "SELECT ":FN.HOLD.CONTROL:" WITH REPORT.NAME EQ 'LAPAP.ENQ.NO.CUS' AND BANK.DATE EQ " :Y.TODAY"";

    CALL EB.READLIST(SEL.CMD, SEL.LIST, "", NO.OF.REC, SEL.ERR);
    Y.COUNT.CUST = DCOUNT(SEL.LIST,FM);
*IF Y.COUNT.CUST GT 0 THEN
*    Y.RPT.ID = SEL.LIST;
*END

    LOOP
        REMOVE Y.RPT.ID FROM SEL.LIST SETTING CUS.POS
    WHILE Y.RPT.ID DO
        EXECUTE 'COPY FROM &HOLD& TO ../bnk.interface/REG.REPORTS ':Y.RPT.ID:",":Y.NAME.FINAL
    REPEAT
    RETURN
END
