*-----------------------------------------------------------------------------
* <Rating>-2</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE L.APAP.DEL.AZ.NAU

    $INSERT T24.BP I_COMMON

    $INSERT T24.BP I_EQUATE

    $INSERT T24.BP I_F.AZ.ACCOUNT

    FN.AZ.ACCOUNT = 'F.AZ.ACCOUNT$NAU'
    F.AZ.ACCOUNT = ''
    CALL OPF(FN.AZ.ACCOUNT,F.AZ.ACCOUNT)

    SELECT.STATEMENT = 'SELECT ':FN.AZ.ACCOUNT : ' WITH CURR.NO EQ 1'
    ACCOUNT.LIST = ''
    LIST.NAME = ''
    SELECTED = ''
    SYSTEM.RETURN.CODE = ''
    CALL EB.READLIST(SELECT.STATEMENT,ACCOUNT.LIST,LIST.NAME,SELECTED,SYSTEM.RETURN.CODE)

    LOOP
        REMOVE ACCOUNT.ID FROM ACCOUNT.LIST SETTING ACCOUNT.MARK
    WHILE ACCOUNT.ID : ACCOUNT.MARK

        R.ACCOUNT = ''
        YERR = ''
        CALL F.DELETE(FN.AZ.ACCOUNT,ACCOUNT.ID)

        CALL OCOMO('SE BORRO EL ID ' : ACCOUNT.ID)

    REPEAT


    SELECT.STATEMENT2 = 'SELECT ':FN.AZ.ACCOUNT
    ACCOUNT.LIST2 = ''
    LIST.NAME2 = ''
    SELECTED2 = ''
    SYSTEM.RETURN.CODE2 = ''
    CALL EB.READLIST(SELECT.STATEMENT2,ACCOUNT.LIST2,LIST.NAME2,SELECTED2,SYSTEM.RETURN.CODE2)

    LOOP
        REMOVE ACCOUNT.ID FROM ACCOUNT.LIST2 SETTING ACCOUNT.MARK2
    WHILE ACCOUNT.ID : ACCOUNT.MARK2

        R.ACCOUNT = ''
        YERR = ''

        Y.ID = ACCOUNT.ID

        Y.APP.NAME = "AZ.ACCOUNT"

        Y.VER.NAME = Y.APP.NAME :",MB.DM.LOAD"

        Y.FUNC = "D"

        Y.PRO.VAL = "PROCESS"

        Y.GTS.CONTROL = ""

        Y.NO.OF.AUTH = ""

        FINAL.OFS = ""

        OPTIONS = ""

        R.ACC = ""

        CALL OFS.BUILD.RECORD(Y.APP.NAME,Y.FUNC,Y.PRO.VAL,Y.VER.NAME,Y.GTS.CONTROL,Y.NO.OF.AUTH,Y.ID,R.ACC,FINAL.OFS)

        CALL OCOMO('STRING OFS : ' : FINAL.OFS )

        CALL OFS.POST.MESSAGE(FINAL.OFS,"","DM.OFS.SRC.VAL","")

        CALL OCOMO('SE REVERSO EL ID ' : ACCOUNT.ID)

    REPEAT

END
