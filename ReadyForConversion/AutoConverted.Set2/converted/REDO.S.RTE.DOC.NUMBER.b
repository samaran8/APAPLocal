SUBROUTINE REDO.S.RTE.DOC.NUMBER(Y.OUT)
*--------------------------------------------------------------------------------
*Company   Name    :Asociacion Popular de Ahorros y Prestamos
*Developed By      :APAP
*Program   Name    :REDO.S.RTE.DOC.NUMBER
*---------------------------------------------------------------------------------
*DESCRIPTION       : This program is used to get the Document Number value
* ----------------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_REDO.DEAL.SLIP.COMMON
    $INSERT I_F.TELLER

    GOSUB PROCESS
RETURN
*********
PROCESS:
*********

    LRF.APP = "TELLER"
    LRF.FIELD = "L.TT.LEGAL.ID"
    LRF.POS = ''
    CALL MULTI.GET.LOC.REF(LRF.APP,LRF.FIELD,LRF.POS)

    TT.LEGAL.POS = LRF.POS<1,1>

    BEGIN CASE

        CASE VAR.PASSPORT NE ''
            Y.OUT = VAR.PASSPORT
        CASE VAR.RNC NE ''
            Y.OUT = VAR.RNC
        CASE VAR.CEDULA NE ''
            Y.OUT = VAR.CEDULA
        CASE OTHERWISE
            IF ID.NEW[1,2] EQ 'TT' THEN
                Y.TT.LEGAL.ID = R.NEW(TT.TE.LOCAL.REF)<1,TT.LEGAL.POS>
                IF Y.TT.LEGAL.ID NE '' THEN
                    Y.OUT = FIELD(Y.TT.LEGAL.ID,'.',2)
                END ELSE
                    Y.OUT = ''
                END
            END ELSE
                Y.OUT = ''
            END
    END CASE

RETURN
END
