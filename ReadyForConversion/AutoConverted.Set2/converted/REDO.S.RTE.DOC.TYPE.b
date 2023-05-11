SUBROUTINE REDO.S.RTE.DOC.TYPE(Y.OUT)
*--------------------------------------------------------------------------------
*Company   Name    :Asociacion Popular de Ahorros y Prestamos
*Developed By      :APAP
*Program   Name    :REDO.S.RTE.DOC.TYPE
*---------------------------------------------------------------------------------
*DESCRIPTION       : This program is used to get the Document Type value
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
            Y.OUT = 'Pasaporte'
        CASE VAR.RNC NE ''
            Y.OUT = 'RNC'
        CASE VAR.CEDULA NE ''
            Y.OUT = 'Cedula'
        CASE OTHERWISE
            IF ID.NEW[1,2] EQ 'TT' THEN
                Y.TT.LEGAL.ID = R.NEW(TT.TE.LOCAL.REF)<1,TT.LEGAL.POS>
                IF Y.TT.LEGAL.ID NE '' THEN
                    Y.OUT = FIELD(Y.TT.LEGAL.ID,'.',1)
                END ELSE
                    Y.OUT = ''
                END
            END ELSE
                Y.OUT = ''
            END
    END CASE

RETURN
END
