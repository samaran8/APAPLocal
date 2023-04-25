*-----------------------------------------------------------------------------
* <Rating>-22</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE LAPAP.TRG.OFS.STRING(Y.LIST.NAME)

*Routine to process OFS STRING. OFS STRING are stored in savedlists STRING.IDS

    $INSERT T24.BP I_COMMON
    $INSERT T24.BP I_EQUATE

    GOSUB INITIALISE
    GOSUB PROCESS
    RETURN
********************************************************************************
INITIALISE:
    OPEN '','&SAVEDLISTS&' TO SAVEDLISTS ELSE
        ERR.OPEN ='EB.RTN.CANT.OPEN.&SAVEDLISTS'
    END
    options = ''; OFS.REQ = ''; theResponse = ''
    options<1> = "AA.COB"

    RETURN
********************************************************************************
PROCESS:

*READ OFS.LIST FROM SAVEDLISTS,'STRING.IDS' ELSE
*PRINT 'CANNOT READ SAVEDLIST'
*END
    Y.LIST.NAME = 'STRING.IDS'
    READ OFS.LIST FROM SAVEDLISTS,Y.LIST.NAME ELSE
        PRINT 'CANNOT READ SAVEDLIST'
    END
    COUNT.I = 0
    LOOP
        REMOVE STR.ID FROM OFS.LIST SETTING STR.POS
    WHILE STR.ID:STR.POS
        COUNT.I = COUNT.I + 1
*CRT "Processing string count = ":COUNT.I
*CRT "Processing string = ":STR.ID
        OFS.REQ = STR.ID
        CALL OFS.CALL.BULK.MANAGER(options,OFS.REQ,theResponse,'')
*CRT "Response ":theResponse
        theResponse = ''
    REPEAT
    RETURN
END
