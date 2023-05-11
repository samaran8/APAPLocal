SUBROUTINE LAPAP.TDVV.REORDER.POINT.RT
**************************************************************
*-------------------------------------------------------------
* Descripcion: Automatizacion de la generacion de tarjetas vituales.
* Autor: Juan Garcia
* Fecha: 09/05/2022
* Ticket: https://apap-software.atlassian.net/browse/DIP-280
*-------------------------------------------------------------
**************************************************************

    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.CUSTOMER
    $INSERT I_TSA.COMMON
    $INSERT I_F.COMPANY
    $INSERT I_F.REDO.CARD.REQUEST

    GOSUB LOAD.TABLE
    GOSUB SELECT
    GOSUB MAIN

***********
LOAD.TABLE:
***********
    Y.STATUS = "STATUS"
    Y.ACEPTE.QTY = "REGOFF.ACCEPTQTY"
    FN.CARD.REQUEST = 'F.REDO.CARD.REQUEST'
    F.REDO.CARD.REQUEST = ''
RETURN

*******
SELECT:
*******
    NO.OF.REC = ''; SEL.ERR = ''; Y.COUNT.CARD = ''; CARD.POS = '';
    SEL.CMD = "SELECT ":FN.CARD.REQUEST:" WITH CARD.TYPE EQ 'TDVV' AND STATUS LT '6'";
    CALL EB.READLIST(SEL.CMD, SEL.LIST, "", NO.OF.REC, SEL.ERR);
    Y.COUNT.CARD = DCOUNT(SEL.LIST,@FM);
RETURN

*****
MAIN:
*****
    LOOP
        REMOVE Y.CARD.REQUEST.ID FROM SEL.LIST SETTING CARD.POS
    WHILE Y.CARD.REQUEST.ID DO

        R.CARD.REQUEST = ''; CARD.REQUEST.ERR = ''
        CALL F.READ(FN.CARD.REQUEST,Y.CARD.REQUEST.ID,R.CARD.REQUEST,F.REDO.CARD.REQUEST,CARD.REQUEST.ERR)

        Y.CARD.TYPE                         = R.CARD.REQUEST<REDO.CARD.REQ.CARD.TYPE>
        Y.CARD.STATUS                       = R.CARD.REQUEST<REDO.CARD.REQ.STATUS>
        Y.COMPANY                           = R.CARD.REQUEST<REDO.CARD.REQ.CO.CODE>
        Y.BRANCH.ORDERQTY                   = R.CARD.REQUEST<REDO.CARD.REQ.BRANCH.ORDERQTY>
        Y.REGOFF.ACCEPTQTY                  = R.CARD.REQUEST<REDO.CARD.REQ.REGOFF.ACCEPTQTY>

        IF Y.CARD.TYPE EQ 'TDVV' AND Y.CARD.STATUS NE '6' THEN

            Y.NEW.STATUS               = "6"
            Y.CARD.REQUEST             = "REDO.CARD.REQUEST,TDVV/I/PROCESS///,//":Y.COMPANY:",":Y.CARD.REQUEST.ID:",":Y.STATUS:"=":Y.NEW.STATUS:",":Y.ACEPTE.QTY:"=":Y.BRANCH.ORDERQTY
            options = 'DM.OFS.SRC.VAL'; RESP = '';
            CALL OFS.POST.MESSAGE(Y.CARD.REQUEST,RESP,options,COMM)
            GOSUB CARD.GENERATION
        END
    REPEAT
RETURN

****************
CARD.GENERATION:
****************
    Y.CARD.GENERATION         = "REDO.CARD.GENERATION,TDVV/I/PROCESS///,//":Y.COMPANY:",":Y.CARD.REQUEST.ID
    options = 'DM.OFS.SRC.VAL'; RESP = '';
    CALL OFS.POST.MESSAGE(Y.CARD.GENERATION,RESP,options,COMM)
RETURN
END
